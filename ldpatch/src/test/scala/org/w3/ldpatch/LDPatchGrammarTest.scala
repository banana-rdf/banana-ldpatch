package org.w3.banana
package ldpatch

import utest._
import java.io._
import scala.util.{ Try, Success, Failure }
import org.w3.banana.ldpatch.model._

// TODO
import org.parboiled2._
import org.w3.banana.io._

class LDPatchGrammarTest[Rdf <: RDF](implicit
  ops: RDFOps[Rdf],
  reader: RDFReader[Rdf, Try, Turtle],
  writer: RDFWriter[Rdf, Try, Turtle]
) extends TestSuite {

  import ops._

  /** transforms triples with vars into a regular RDF Graph (variables
    * are transformed into bnode)
    */
  def makeGraph(triples: Vector[model.Triple[Rdf]]): Rdf#Graph = {
    def makeNode(node: VarOrConcrete[Rdf]): Rdf#Node = node match {
      case Concrete(node) => node
      case Var(_) => BNode()
    }
    Graph(
      triples.map { case model.Triple(s, p, o) => Triple(makeNode(s), p, makeNode(o)) }
    )
  }


  val g = new Grammar[Rdf] { implicit val ops = LDPatchGrammarTest.this.ops }

  def newParser(input: String) =
    new g.grammar.PEGPatchParser(input, baseURI = URI("http://example.com/foo"), prefixes = Map("foaf" -> URI("http://xmlns.com/foaf/"), "schema" -> URI("http://schema.org/")))

  def newFreshParser(input: String) =
    new g.grammar.PEGPatchParser(input, baseURI = URI("http://example.com/foo"), prefixes = Map.empty)

  def tests = TestSuite {

    "parse IRIREF" - {
      assert(newParser("""<http://example.com/foo#\u2665>""").IRIREF.run() == Success(URI("http://example.com/foo#♥")))
      assert(newParser("""<#\u2665>""").IRIREF.run() == Success(URI("http://example.com/foo#♥")))
    }
  
    "parse iri" - {
      assert(newParser("""<http://example.com/foo#\u2665>""").iri.run() == Success(URI("http://example.com/foo#♥")))
      assert(newParser("""foaf:name""").iri.run() == Success(URI("http://xmlns.com/foaf/name")))
    }
  
    "parse prefixID" - {
      assert(newParser("""@prefix example:<http://example.com/foo#> .""").prefixID.run() == Success("example" -> URI("http://example.com/foo#")))
      assert(newParser("""@prefix   example: <http://example.com/foo#> .""").prefixID.run() == Success("example" -> URI("http://example.com/foo#")))
      assert(newParser("""@prefix : <http://example.com/foo#> .""").prefixID.run() == Success("" -> URI("http://example.com/foo#")))
    }
  
    "parse BlankNode" - {
      // the only way to know about the expected bnode is to look into the map built by the parser
      val parser = newParser("""_:foo""")
      val parsedValue = parser.BlankNode.run()
      assert(parsedValue == Success(parser.bnodeMap("foo")))
      assertMatch(newParser("""[]""").BlankNode.run()){ case Success(_) => }
      assertMatch(newParser("""[ ]""").BlankNode.run()){ case Success(_) => }
    }
  
    "parse RDFLiteral" - {
      assert(newParser(""""foo"""").RDFLiteral.run() == Success(Literal("foo")))
      assert(newParser(""""foo"@en""").RDFLiteral.run() == Success(Literal.tagged("foo", Lang("en"))))
      assert(newParser(""""foo"^^foaf:name""").RDFLiteral.run() == Success(Literal("foo", URI("http://xmlns.com/foaf/name"))))
      assert(newParser(""""foo"^^<http://xmlns.com/foaf/name>""").RDFLiteral.run() == Success(Literal("foo", URI("http://xmlns.com/foaf/name"))))
    }
  
    "parse NumericLiteral" - {
      assert(newParser("""42""").NumericLiteral.run() == Success(Literal("42", xsd.integer)))
      assert(newParser("""-42""").NumericLiteral.run() == Success(Literal("-42", xsd.integer)))
      assert(newParser("""3.14""").NumericLiteral.run() == Success(Literal("3.14", xsd.decimal)))
      assert(newParser("""-3.14""").NumericLiteral.run() == Success(Literal("-3.14", xsd.decimal)))
      assert(newParser("""42e-10""").NumericLiteral.run() == Success(Literal("42e-10", xsd.double)))
      assert(newParser("""-3.14E10""").NumericLiteral.run() == Success(Literal("-3.14E10", xsd.double))    )
    }
  
    "parse BooleanLiteral" - {
      assert(newParser("""true""").BooleanLiteral.run() == Success(xsd.`true`))
      assert(newParser("""false""").BooleanLiteral.run() == Success(xsd.`false`))
    }
  
    "parse literal" - {
      assert(newParser(""""foo"^^foaf:name""").literal.run() == Success(Literal("foo", URI("http://xmlns.com/foaf/name"))))
      assert(newParser("""-3.14""").literal.run() == Success(Literal("-3.14", xsd.decimal)))
      assert(newParser("""true""").literal.run() == Success(xsd.`true`))
    }
  
    "parse Var" - {
      assert(newParser("""?name""").VAR1.run() == Success(Var("name")))
    }
  
    "parse triples" - {
      val parser = newParser("""_:betehess foaf:name "Alexandre Bertails"""")
  //    parser.triples.run() match {
  //      case Failure(error: ParseError) =>
  //        println(parser.formatError(error))
  //    }
      val parsedTriples = parser.triples.run()
      assert(parsedTriples == Success(
        Vector(model.Triple(
          Concrete(parser.bnodeMap("betehess")),
          URI("http://xmlns.com/foaf/name"),
          Concrete(Literal("Alexandre Bertails"))
        ))
      ))
    }
  
    "parse Add" - {
      val parser = newParser("""Add { _:betehess foaf:name "Alexandre Bertails" } .""")
      val parsedAdd = parser.add.run()
      assert(parsedAdd == Success(
        Add(Vector(model.Triple(
          Concrete(parser.bnodeMap("betehess")),
          URI("http://xmlns.com/foaf/name"),
          Concrete(Literal("Alexandre Bertails"))
        )))
      ))
    }
  
  
    "parse Complex Add" - {
      val parser = newParser("""Add {
    ?ted schema:location [
      schema:name "Long Beach, California" ;
      schema:geo [
        schema:latitude "33.7817" ;
        schema:longitude "-118.2054"
      ]
    ]
  } .""")
  
      val parsedAdd = parser.add.run().get
  
      val equivalentGraph = reader.read(new StringReader("""
  @prefix schema: <http://schema.org/> .
  [] schema:location [
    schema:name "Long Beach, California" ;
    schema:geo [
      schema:latitude "33.7817" ;
      schema:longitude "-118.2054"
    ]
  ]
  """), "http://example.com/timbl").get
  
      val graph = makeGraph(parsedAdd.triples)
  
      assert(equivalentGraph isIsomorphicWith graph)
  
    }
  
    "parse Add List" - {
      val parser = newParser("""Add { _:betehess foaf:name "Alexandre Bertails", "Betehess" } .""")
      val parsedAdd = parser.add.run()
      assert(parsedAdd == Success(
        Add(Vector(
          model.Triple(
            Concrete(parser.bnodeMap("betehess")),
            URI("http://xmlns.com/foaf/name"),
            Concrete(Literal("Alexandre Bertails"))
          ),
          model.Triple(
            Concrete(parser.bnodeMap("betehess")),
            URI("http://xmlns.com/foaf/name"),
            Concrete(Literal("Betehess"))
          )
        ))
      ))
    }
  
    "parse Delete" - {
      assert(newParser("""Delete { ?betehess foaf:name "Alexandre Bertails" } .""").delete.run() == Success(
        Delete(Vector(model.Triple(
          Var("betehess"),
          URI("http://xmlns.com/foaf/name"),
          Concrete(Literal("Alexandre Bertails"))
        )))
      ))
    }
  
    "parse Cut" - {
      assert(newParser("""Cut ?betehess .""").cut.run() == Success(Cut(Var("betehess"))))
  // assert(TODO should not compile    newParser("""Cut <http://example.com/foo> .""").cut.run() == Success(Cut(Concrete(URI("http://example.com/foo")))))
    }
  
    "parse Path" - {
      assert(newParser("""/foaf:name/^foaf:name/<http://example.com/foo>""").path.run() == Success(
        Path(Seq(
          StepForward(URI("http://xmlns.com/foaf/name")),
          StepBackward(URI("http://xmlns.com/foaf/name")),
          StepForward(URI("http://example.com/foo"))
        ))
      ))
  
      assert(newParser("""[/<http://example.com/foo>/foaf:name="Alexandre Bertails"]""").path.run() == Success(
        Path(Seq(
          Filter(
            Path(Seq(
              StepForward(URI("http://example.com/foo")),
              StepForward(URI("http://xmlns.com/foaf/name"))
            )),
            Some(Concrete(Literal("Alexandre Bertails")))
          )
        ))
      ))
  
      assert(newParser("""[/<http://example.com/foo>/^foaf:name]/foaf:friend""").path.run() == Success(
        Path(Seq(
          Filter(
            Path(Seq(
              StepForward(URI("http://example.com/foo")),
              StepBackward(URI("http://xmlns.com/foaf/name"))
            )),
            None
          ),
          StepForward(URI("http://xmlns.com/foaf/friend"))
        ))
      ))
  
      assert(newParser("""/foaf:name!/42""").path.run() == Success(
        Path(Seq(
          StepForward(URI("http://xmlns.com/foaf/name")),
          UnicityConstraint,
          StepAt(42)
        ))
      ))
  
      assert(newParser("""/foaf:knows [ /foaf:holdsAccount /foaf:accountName = "bertails" ]""").path.run() == Success(
        Path(Seq(
          StepForward(URI("http://xmlns.com/foaf/knows")),
          Filter(
            Path(Seq(
              StepForward(URI("http://xmlns.com/foaf/holdsAccount")),
              StepForward(URI("http://xmlns.com/foaf/accountName"))
            )),
            Some(Concrete(Literal("bertails")))
          )
        ))
      ))
  
    }
  
  
    "parse Bind" - {
  
      assert(newParser("""Bind ?foo <http://example.com/blah> .""").bind.run() == Success(
        Bind(
          Var("foo"),
          Concrete(URI("http://example.com/blah")),
          Path(Seq())
        )
      ))
  
      assert(newParser("""Bind ?foo <http://example.com/blah> /foaf:name/^foaf:name/<http://example.com/foo> .""").bind.run() == Success(
        Bind(
          Var("foo"),
          Concrete(URI("http://example.com/blah")),
          Path(Seq(
            StepForward(URI("http://xmlns.com/foaf/name")),
            StepBackward(URI("http://xmlns.com/foaf/name")),
            StepForward(URI("http://example.com/foo"))
          ))
        )
      ))
  
      val bind = """Bind ?alex
       <http://champin.net/#pa>/foaf:knows
          [/foaf:holdsAccount/foaf:accountName="bertails"] ."""
      assert(newParser(bind).bind.run() == Success(
        Bind(
          Var("alex"),
          Concrete(URI("http://champin.net/#pa")),
          Path(Seq(
            StepForward(URI("http://xmlns.com/foaf/knows")),
            Filter(
              Path(Seq(
                StepForward(URI("http://xmlns.com/foaf/holdsAccount")),
                StepForward(URI("http://xmlns.com/foaf/accountName"))
              )),
              Some(Concrete(Literal("bertails")))
            )
          ))
        )
      ))
  
  
    }
  
    "parse Slice" - {
      assert(newParser("""42..2868""").slice.run() == Success(Range(42, 2868)))
      assert(newParser("""42..""").slice.run() == Success(EverythingAfter(42)))
      assert(newParser("""..2868""").slice.run() == Success(Range(0, 2868)))
      assert(newParser("""..""").slice.run() == Success(End))
    }
  
    "parse UpdateList" - {
      assertMatch(newParser("""UpdateList ?alex foaf:prefLang 0.. ( "fr" "en" ) .""").updateList.run()) { case Success(_) => }
  
    }
  
    "parse Prologue" - {

      assert(newFreshParser("""@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix v: <http://example.org/vocab#> .""").prologue.run() == Success(
        Map("rdf" -> URI("http://www.w3.org/1999/02/22-rdf-syntax-ns#"), "foaf" -> URI("http://xmlns.com/foaf/0.1/"), "v" -> URI("http://example.org/vocab#"))
      ))

    }
  
    "parse LDPatch" - {
      val patch = """
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix v: <http://example.org/vocab#> .
  
  Bind ?alex
       <http://champin.net/#pa> /foaf:knows
          [/foaf:holdsAccount/foaf:accountName="bertails"] .
  
  UpdateList ?alex v:prefLang 0.. ( "fr" "en" ) .
  """
      assertMatch(newFreshParser(patch).ldpatch.run()){ case Success(_) => }
      
    }
  
    "parse Example 2 from spec" - {
      val patch = """
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  @prefix schema: <http://schema.org> .
  @prefix profile: <http://ogp.me/ns/profile#> .
  @prefix ex: <http://example.org/vocab#> .
  
  Delete { <#> profile:first_name "Tim" } .
  Add    { <#> profile:first_name "Timothy" } .
  
  UpdateList <#> ex:preferredLanguages 1..2 ( "fr-CH" ) .
  
  Bind ?event <#> /schema:attendee[/schema:url = <http://conferences.ted.com/TED2009/>]  .
  Add { ?event rdf:type schema:Event } .
  
  Bind ?ted <http://conferences.ted.com/TED2009/> /^schema:url! .
  Delete { ?ted schema:startDate "2009-02-04" } .
  Add { ?ted schema:location _:loc } .
  Add { _:loc schema:name "Long Beach, California" } .
  Add { _:loc schema:geo _:geo } .
  Add { _:geo schema:latitude "33.7817" } .
  Add { _:geo schema:longitude "-118.2054" } .
  """
  //    val parser = newFreshParser(patch)
  //    parser.ldpatch.run() match {
  //      case Failure(error: ParseError) =>
  //        println(parser.formatError(error))
  //    }
  
      assertMatch(newFreshParser(patch).ldpatch.run()){ case Success(_) => }
    }
  
    "parse Example 2 from spec - v2" - {
      val patch = """
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  @prefix schema: <http://schema.org> .
  @prefix profile: <http://ogp.me/ns/profile#> .
  @prefix ex: <http://example.org/vocab#> .
  
  Delete { <#> profile:first_name "Tim" } .
  Add    { <#> profile:first_name "Timothy" } .
  
  UpdateList <#> ex:preferredLanguages 1..2 ( "fr-CH" ) .
  
  Bind ?event <#> /schema:attendee[/schema:url = <http://conferences.ted.com/TED2009/>]  .
  Add { ?event rdf:type schema:Event } .
  
  Bind ?ted <http://conferences.ted.com/TED2009/> /^schema:url! .
  Delete { ?ted schema:startDate "2009-02-04" } .
  Add {
    ?ted schema:location [
      schema:name "Long Beach, California" ;
      schema:geo [
        schema:latitude "33.7817" ;
        schema:longitude "-118.2054"
      ]
    ]
  } .
  """
      assertMatch(newFreshParser(patch).ldpatch.run()){ case Success(_) => }
    }

  }

}
