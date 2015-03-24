package org.w3.banana
package ldpatch

import org.w3.banana.io._
import utest._
import java.io._
import scala.util.{ Try, Success, Failure }
import org.w3.banana.ldpatch.model._

class LDPatchSemanticsTest[Rdf <: RDF](implicit
  ops: RDFOps[Rdf],
  reader: RDFReader[Rdf, Try, Turtle],
  writer: RDFWriter[Rdf, Try, Turtle]
) extends TestSuite {

  import ops._

  val foaf = FOAFPrefix[Rdf]

  val ex: Prefix[Rdf] = new PrefixBuilder[Rdf]("ex", "http://example.org/vocab#")

  val g = new Grammar[Rdf] { implicit val ops = LDPatchSemanticsTest.this.ops }

  val s = new Semantics[Rdf] { implicit val ops = LDPatchSemanticsTest.this.ops }

  implicit class ReaderW(val reader: RDFReader[Rdf, Try, Turtle]) {
    def read(input: String, base: String): Try[Rdf#Graph] =
      reader.read(new StringReader(input), base)
  }

  def newParser(input: String) =
    new g.grammar.PEGPatchParser(
      input,
      baseURI = URI("http://example.com/timbl"),
      prefixes = Map(
        "foaf" -> URI("http://xmlns.com/foaf/"),
        "rdf" -> URI("http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
        "schema" -> URI("http://schema.org/"),
        "profile" -> URI("http://ogp.me/ns/profile#"),
        "ex" -> URI("http://example.org/vocab#")
      )
    )

  def newFreshParser(input: String) =
    new g.grammar.PEGPatchParser(input, baseURI = URI("http://example.com/timbl"), prefixes = Map.empty)

  val graph = reader.read("""
@prefix schema: <http://schema.org/> .
@prefix profile: <http://ogp.me/ns/profile#> .
@prefix ex: <http://example.org/vocab#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
<http://example.com/timbl#> a schema:Person ;
  schema:alternateName "TimBL" ;
  profile:first_name "Tim" ;
  profile:last_name "Berners-Lee" ;
  schema:workLocation [ schema:name "W3C/MIT" ] ;
  schema:attendee _:b1, _:b2 ;
  ex:preferredLanguages ( "en" "fr" ).

_:b1 schema:name "F2F5 - Linked Data Platform" ;
  schema:url <https://www.w3.org/2012/ldp/wiki/F2F5> .

_:b2 a schema:Event ;
  schema:name "TED 2009" ;
  schema:startDate "2009-02-04" ;
  schema:url <http://conferences.ted.com/TED2009/> .
""", "http://example.com/timbl").get

  def tests = TestSuite {

    "Path semantics" - {
  
      val path = newParser("""/schema:attendee[/schema:url=<http://conferences.ted.com/TED2009/>]/schema:name""").path.run().get
  
      val nodes = s.semantics.Path(path, URI("http://example.com/timbl#"), s.semantics.State(graph))
  
      assert(nodes == Set(Literal("TED 2009")))
  
    }
  
  
    "Path semantics (exception)" - {
  
      val path = newParser("""/schema:attendee!""").path.run().get
  
      intercept[RuntimeException] {
  
        s.semantics.Path(path, URI("http://example.com/timbl#"), s.semantics.State(graph))
  
      }
  
    }
  
    "UpdateList semantics" - {
  
      val ul = newParser("""UpdateList <#> ex:preferredLanguages 1..2 ( "fr-CH" ) .""").updateList.run().get
  
      import org.w3.banana.diesel._
  
      val newGraph = s.semantics.UpdateList(ul, s.semantics.State(graph)).graph
  
      val l = (PointedGraph(URI("http://example.com/timbl#"), newGraph) / ex("preferredLanguages")).as[List[String]]
  
      assert(l == Success(List("en", "fr-CH")))
  
    }
  
    def testUpdate(initialList: List[Int], update: String, expectedList: List[Int]) = {
  
      import org.w3.banana.diesel._
  
      val graph: Rdf#Graph = (URI("http://example.com/#") -- ex("numbers") ->- initialList).graph
  
      val ul = newParser(update).updateList.run().get
  
      val newGraph = s.semantics.UpdateList(ul, s.semantics.State(graph)).graph
  
      val list = (PointedGraph(URI("http://example.com/#"), newGraph) / ex("numbers")).as[List[Int]]
  
      assert(list == Success(expectedList))
  
    }
  
    "UpdateList semantics2" - {
  
      // substitution
  
      testUpdate(List(1, 2), """UpdateList <http://example.com/#> ex:numbers 1..2 ( 42 ) .""", List(1, 42))
  
      testUpdate(List(1, 2), """UpdateList <http://example.com/#> ex:numbers 1..2 ( 42 2868 ) .""", List(1, 42, 2868))
  
      testUpdate(List(1, 2, 3), """UpdateList <http://example.com/#> ex:numbers 1..2 ( 42 2868 ) .""", List(1, 42, 2868, 3))
  
      testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 1..3 ( 42 2868 ) .""", List(1, 42, 2868, 4))
  
      testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 0..4 ( ) .""", List())
  
      testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 4..5 ( 42 ) .""", List(1, 2, 3, 4, 42))
  
      // append
  
      testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers .. ( 42 2868 ) .""", List(1, 2, 3, 4, 42, 2868))
  
      testUpdate(List(), """UpdateList <http://example.com/#> ex:numbers .. ( 42 2868 ) .""", List(42, 2868))
  
      testUpdate(List(), """UpdateList <http://example.com/#> ex:numbers .. ( ) .""", List())
  
      testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers .. ( ) .""", List(1, 2, 3, 4))
  
      // insert at
  
      testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 2..2 ( 42 2868 ) .""", List(1, 2, 42, 2868, 3, 4))
  
      testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 0..0 ( 42 2868 ) .""", List(42, 2868, 1, 2, 3, 4))
  
      testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 2..2 ( ) .""", List(1, 2, 3, 4))
  
      // replace everything after
  
      testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 2.. ( 42 2868 ) .""", List(1, 2, 42, 2868))
  
      testUpdate(List(), """UpdateList <http://example.com/#> ex:numbers 0.. ( 42 2868 ) .""", List(42, 2868))
  
      testUpdate(List(), """UpdateList <http://example.com/#> ex:numbers 2.. ( 42 2868 ) .""", List(42, 2868))
  
      testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 4.. ( 42 2868 ) .""", List(1, 2, 3, 4, 42, 2868))
  
      // weird cases
  
      testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 2..1 ( 42 2868 ) .""", List(1, 2, 42, 2868, 3, 4))
  
    }
  
  
  
    /* rdf:List manipulation examples */
  
    val listGraph = reader.read("""
  <#> <http://example.org/vocab#preferredLanguages> ( "lorem" "ipsum" "dolor" "sit" "amet" ).
  """, "http://example.com/timbl").get
  
  
    "rdf:List manipulation examples - replace one element" - {
  
      val ldpatch = newFreshParser("""
  UpdateList <#> <http://example.org/vocab#preferredLanguages> 1..2 ( "fr" ) .
  """).ldpatch.run().get
  
      val expectedGraph = reader.read("""
  <#> <http://example.org/vocab#preferredLanguages> ( "lorem" "fr" "dolor" "sit" "amet" ).
  """, "http://example.com/timbl").get
  
      val newGraph = s.semantics.LDPatch(ldpatch, listGraph)
  
      assert(expectedGraph isIsomorphicWith newGraph)
  
    }
  
    "rdf:List manipulation examples - insert new elements" - {
  
      val ldpatch = newFreshParser("""
  UpdateList <#> <http://example.org/vocab#preferredLanguages> 2..2 ( "en" "fr" ) .
  """).ldpatch.run().get
  
      val expectedGraph = reader.read("""
  <#> <http://example.org/vocab#preferredLanguages> ( "lorem" "ipsum" "en" "fr" "dolor" "sit" "amet" ).
  """, "http://example.com/timbl").get
  
      val newGraph = s.semantics.LDPatch(ldpatch, listGraph)
  
      assert(expectedGraph isIsomorphicWith newGraph)
  
    }
  
    "rdf:List manipulation examples - append elements" - {
  
      val ldpatch = newFreshParser("""
  UpdateList <#> <http://example.org/vocab#preferredLanguages> .. ( "en" "fr" ) .
  """).ldpatch.run().get
  
      val expectedGraph = reader.read("""
  <#> <http://example.org/vocab#preferredLanguages> ( "lorem" "ipsum" "dolor" "sit" "amet" "en" "fr" ).
  """, "http://example.com/timbl").get
  
      val newGraph = s.semantics.LDPatch(ldpatch, listGraph)
  
      assert(expectedGraph isIsomorphicWith newGraph)
  
    }
  
    "rdf:List manipulation examples - replace all the elements" - {
  
      val ldpatch = newFreshParser("""
  UpdateList <#> <http://example.org/vocab#preferredLanguages> 2.. ( "en" "fr" ) .
  """).ldpatch.run().get
  
      val expectedGraph = reader.read("""
  <#> <http://example.org/vocab#preferredLanguages> ( "lorem" "ipsum" "en" "fr" ).
  """, "http://example.com/timbl").get
  
      val newGraph = s.semantics.LDPatch(ldpatch, listGraph)
  
      assert(expectedGraph isIsomorphicWith newGraph)
  
    }
  
    "rdf:List manipulation examples - replace last n elements" - {
  
      val ldpatch = newFreshParser("""
  UpdateList <#> <http://example.org/vocab#preferredLanguages> -3.. ( "en" "fr" ) .
  """).ldpatch.run().get
  
      val expectedGraph = reader.read("""
  <#> <http://example.org/vocab#preferredLanguages> ( "lorem" "ipsum" "en" "fr" ).
  """, "http://example.com/timbl").get
  
      val newGraph = s.semantics.LDPatch(ldpatch, listGraph)
  
      assert(expectedGraph isIsomorphicWith newGraph)
  
    }
  
    "rdf:List manipulation examples - remove elements" - {
  
      val ldpatch = newFreshParser("""
  UpdateList <#> <http://example.org/vocab#preferredLanguages> 1..3 ( ) .
  """).ldpatch.run().get
  
      val expectedGraph = reader.read("""
  <#> <http://example.org/vocab#preferredLanguages> ( "lorem" "sit" "amet" ).
  """, "http://example.com/timbl").get
  
      val newGraph = s.semantics.LDPatch(ldpatch, listGraph)
  
      assert(expectedGraph isIsomorphicWith newGraph)
  
    }
  
    "rdf:List manipulation examples - empty a collection" - {
  
      val ldpatch = newFreshParser("""
  UpdateList <#> <http://example.org/vocab#preferredLanguages> 0.. ( ) .
  """).ldpatch.run().get
  
      val expectedGraph = reader.read("""
  <#> <http://example.org/vocab#preferredLanguages> ( ).
  """, "http://example.com/timbl").get
  
      val newGraph = s.semantics.LDPatch(ldpatch, listGraph)
  
      assert(expectedGraph isIsomorphicWith newGraph)
  
    }
  
  
  
  
  
  
    "cut TED event" - {
  
      val patch = """
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  @prefix schema: <http://schema.org/> .
  @prefix profile: <http://ogp.me/ns/profile#> .
  @prefix ex: <http://example.org/vocab#> .
  
  Bind ?ted <http://conferences.ted.com/TED2009/> /^schema:url! .
  Cut ?ted .
  """
  
      val expectedGraph = reader.read("""
  @prefix schema: <http://schema.org/> .
  @prefix profile: <http://ogp.me/ns/profile#> .
  @prefix ex: <http://example.org/vocab#> .
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  
  @prefix schema: <http://schema.org/> .
  @prefix profile: <http://ogp.me/ns/profile#> .
  @prefix ex: <http://example.org/vocab#> .
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  <http://example.com/timbl#> a schema:Person ;
    schema:alternateName "TimBL" ;
    profile:first_name "Tim" ;
    profile:last_name "Berners-Lee" ;
    schema:workLocation [ schema:name "W3C/MIT" ] ;
    schema:attendee _:b1 ;
    ex:preferredLanguages ( "en" "fr" ).
  
  _:b1 schema:name "F2F5 - Linked Data Platform" ;
    schema:url <https://www.w3.org/2012/ldp/wiki/F2F5> .
  """, "http://example.com/timbl").get
  
      val ldpatch = newFreshParser(patch).ldpatch.run().get
  
      val newGraph = s.semantics.LDPatch(ldpatch, graph)
  
      assert(expectedGraph isIsomorphicWith newGraph)
    }
  
  
    "full test" - {
  
      val patch = """
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  @prefix schema: <http://schema.org/> .
  @prefix profile: <http://ogp.me/ns/profile#> .
  @prefix ex: <http://example.org/vocab#> .
  
  Delete { <#> profile:first_name "Tim" } .
  Add {
    <#> profile:first_name "Timothy" ;
      profile:image <https://example.org/timbl.jpg> .
  } .
  
  Bind ?workLocation <#> /schema:workLocation .
  Cut ?workLocation .
  
  UpdateList <#> ex:preferredLanguages 1..2 ( "fr-CH" ) .
  
  Bind ?event <#> /schema:attendee[/schema:url = <https://www.w3.org/2012/ldp/wiki/F2F5>]  .
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
  
      val expectedGraph = reader.read("""
  @prefix schema: <http://schema.org/> .
  @prefix profile: <http://ogp.me/ns/profile#> .
  @prefix ex: <http://example.org/vocab#> .
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  <http://example.com/timbl#> a schema:Person ;
    schema:alternateName "TimBL" ;
    profile:first_name "Timothy" ;
    profile:last_name "Berners-Lee" ;
    profile:image <https://example.org/timbl.jpg> ;
    schema:attendee _:b1, _:b2 ;
    ex:preferredLanguages ( "en" "fr-CH" ).
  
  _:b1 a schema:Event ;
    schema:name "F2F5 - Linked Data Platform" ;
    schema:url <https://www.w3.org/2012/ldp/wiki/F2F5> .
  
  _:b2 a schema:Event ;
    schema:name "TED 2009" ;
    schema:url <http://conferences.ted.com/TED2009/> ;
    schema:location [
      schema:name "Long Beach, California";
      schema:geo [ schema:latitude "33.7817" ; schema:longitude "-118.2054" ]
    ] .
  """, "http://example.com/timbl").get
  
      val ldpatch = newFreshParser(patch).ldpatch.run().get
  
      val newGraph = s.semantics.LDPatch(ldpatch, graph)
  
      assert(expectedGraph isIsomorphicWith newGraph)
  
    }

  }

}
