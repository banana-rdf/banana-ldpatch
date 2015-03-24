package org.w3.banana
package ldpatch

import scala.util.{ Try, Success, Failure }
import org.w3.banana.ldpatch.{ model => m }
import shapeless._

trait Grammar[Rdf <: RDF] {

  implicit val ops: RDFOps[Rdf]

  import ops._

  object grammar {

    import org.parboiled2._
    import CharPredicate._

    def between(low: Char, high: Char): CharPredicate = CharPredicate.from(c => c >= low && c <= high)

    def parseLDPatch(input: ParserInput, baseURI: Rdf#URI): Try[m.LDPatch[Rdf]] = {
      val parser = new PEGPatchParser(input, baseURI, prefixes = Map.empty)
      parser.ldpatch.run() match {
	case success @ Success(_) => success
	case Failure(error: ParseError) => Failure(new RuntimeException(parser.formatError(error)))
	case failure @ Failure(_) => failure
      }
    }

    class PEGPatchParser(
      val input: ParserInput,
      baseURI: Rdf#URI,
      var prefixes: Map[String, Rdf#URI] = Map.empty,
      var bnodeMap: Map[String, Rdf#BNode] = Map.empty,
      var triplesAcc: Vector[m.Triple[Rdf]] = Vector.empty
    ) extends Parser with StringBuilding {


      /** use this method to maintain the map from bnodes label to actual
        * unique labels
        */
      def makeBNode(label: String): Rdf#BNode = {
        this.bnodeMap.getOrElse(label, {
          val bnode = BNode()
          this.bnodeMap += (label -> bnode)
          bnode
        })
      }

      def emptyTriplesAcc: Rule0 = rule (
        run(this.triplesAcc = Vector.empty)
      )

      // token separators

      def WS0: Rule0 = rule { zeroOrMore(WS) }

      def WS1: Rule0 = rule { oneOrMore(WS) }

      // ldpatch ::= prologue statement*
      def ldpatch: Rule1[m.LDPatch[Rdf]] = rule {
        WS0 ~ prologue ~> (prefixes => this.prefixes = prefixes) ~ WS0 ~ zeroOrMore(statement).separatedBy(WS1) ~ WS0 ~ EOI ~> {
          (statements: Seq[m.Statement[Rdf]]) => m.LDPatch(statements)
        }
      }

      // prologue ::= prefixID*
      def prologue: Rule1[Map[String, Rdf#URI]] = rule { zeroOrMore(prefixID).separatedBy(WS1) ~> ((prefixes: Seq[(String, Rdf#URI)]) => push(this.prefixes ++ prefixes)) }

      // statement ::= bind | add | addNew | delete | deleteExisting | cut | updateList
      def statement: Rule1[m.Statement[Rdf]] = rule (
        bind | add | addNew | delete | deleteExisting | cut | updateList
      )

      // bind ::= ("Bind" | "B") Var value path? "."
      def bind: Rule1[m.Bind[Rdf]] = rule (
        ("Bind" | 'B') ~ WS1 ~ VAR1 ~ WS1 ~ value ~ optional(WS0 ~ path) ~ WS0 ~ '.' ~> ((varr: m.Var, value: m.VarOrConcrete[Rdf], pathOpt: Option[m.Path[Rdf]]) => m.Bind(varr, value, pathOpt.getOrElse(m.Path(Seq.empty))))
      )

      // add ::= ("Add" | "A") "{" graph "}" "."
      def add: Rule1[m.Add[Rdf]] = rule (
        ("Add" | 'A') ~ WS1 ~ '{' ~ WS0 ~ graph ~ WS0 ~ '}' ~ WS0 ~ '.' ~> { (graph: Vector[m.Triple[Rdf]]) => m.Add(m.Lax, graph) }
      )

      // addNew ::= ("AddNew" | "AN") "{" graph "}" "."
      def addNew: Rule1[m.Add[Rdf]] = rule (
        ("AddNew" | "AN") ~ WS1 ~ '{' ~ WS0 ~ graph ~ WS0 ~ '}' ~ WS0 ~ '.' ~> { (graph: Vector[m.Triple[Rdf]]) => m.Add(m.Strict, graph) }
      )

      // delete ::= ("Delete" | "D") "{" graph "}" "."
      def delete: Rule1[m.Delete[Rdf]] = rule (
        ("Delete" | 'D') ~ WS1 ~ '{' ~ WS0 ~ graph ~ WS0 ~ '}' ~ WS0 ~ '.' ~> { (graph: Vector[m.Triple[Rdf]]) => m.Delete(m.Lax, graph) }
      )

      // deleteExisting ::= ("DeleteExisting" | "DE") "{" graph "}" "."
      def deleteExisting: Rule1[m.Delete[Rdf]] = rule (
        ("DeleteExisting" | "DE") ~ WS1 ~ '{' ~ WS0 ~ graph ~ WS0 ~ '}' ~ WS0 ~ '.' ~> { (graph: Vector[m.Triple[Rdf]]) => m.Delete(m.Strict, graph) }
      )

      // cut ::= ("Cut" | "C") VAR1 "."
      def cut: Rule1[m.Cut] = rule (
        ("Cut" | "C") ~ WS1 ~ VAR1 ~ WS0 ~ '.' ~> { (varr: m.Var) => m.Cut(varr) }
      )

      // updateList ::= ("UpdateList" | "UL") varOrIRI predicate slice collection "."
      def updateList: Rule1[m.UpdateList[Rdf]] = rule (
        ("UpdateList" | "UL") ~ emptyTriplesAcc ~ WS1 ~ varOrIRI ~ WS1 ~ predicate ~ WS1 ~ slice ~ WS1 ~ collection ~ WS0 ~ '.' ~> {
          (s: m.VarOrConcrete[Rdf], p: Rdf#URI, slice: m.Slice, node: Rdf#Node) => m.UpdateList(s, p, slice, node, triplesAcc)
        }
      )

      // varOrIRI ::= iri | VAR1
      def varOrIRI: Rule1[m.VarOrConcrete[Rdf]] = rule (
          iri ~> (m.Concrete(_))
        | VAR1
      )

      // value ::= iri | literal | VAR1
      def value: Rule1[m.VarOrConcrete[Rdf]] = rule (
          iri ~> (m.Concrete(_))
        | literal ~> (m.Concrete(_))
        | VAR1
      )

      // path ::= ( '/' step | constraint )*
      def path: Rule1[m.Path[Rdf]] = rule (
        zeroOrMore(WS0 ~ ('/' ~ WS0 ~ step | constraint)) ~> {
          (pathElems: Seq[m.PathElement[Rdf]]) => m.Path(pathElems)
        }
      )

      // step ::= '^' iri | iri | INDEX
      def step: Rule1[m.Step[Rdf]] = rule (
          '^' ~ iri ~> ((uri: Rdf#URI) => m.StepBackward(uri))
        | iri ~> ((uri: Rdf#URI) => m.StepForward(uri))
        | INDEX ~> (m.StepAt(_: Int))
      )

      // constraint ::= '[' path ( '=' value )? ']' | '!'
      def constraint: Rule1[m.Constraint[Rdf]] = rule (
          '[' ~ WS0 ~ path ~ optional(WS0 ~ '=' ~ WS0 ~ value) ~ WS0 ~ ']' ~> ((path: m.Path[Rdf], valueOpt: Option[m.VarOrConcrete[Rdf]]) => m.Filter(path, valueOpt))
        | '!' ~ push(m.UnicityConstraint)
      )

      // slice ::= INDEX? '..' INDEX?
      def slice: Rule1[m.Slice] = rule (
        optional(INDEX) ~ ".." ~ optional(INDEX) ~> ((leftOpt: Option[Int], rightOpt: Option[Int]) => (leftOpt, rightOpt) match {
          case (Some(left), Some(right)) => m.Range(left, right)
          case (Some(index), None)       => m.EverythingAfter(index)
          case (None, Some(index))       => m.Range(0, index)
          case (None, None)              => m.End
        })
      )

      // INDEX ::= '-'? [0-9]+
      def INDEX: Rule1[Int] = rule (
        capture(optional('-') ~ oneOrMore(Digit)) ~> { (s: String) =>
          if (s.charAt(0) == '-')
            - s.substring(1).toInt
          else
            s.toInt
        }
      )

      // copied from SPARQL

      // [143s] VAR1 ::= '?' VARNAME
      def VAR1: Rule1[m.Var] = rule (
        '?' ~ VARNAME
      )

      // [166s] VARNAME ::= ( PN_CHARS_U | [0-9] ) ( PN_CHARS_U | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040] )*
      def VARNAME: Rule1[m.Var] = rule (
        clearSB() ~ (PN_CHARS_U | CharPredicate.Digit ~ appendSB()) ~ zeroOrMore(
            PN_CHARS_U
          | (CharPredicate.Digit ++ CharPredicate('\u00B7') ++ between('\u0300', '\u036F') ++ between('\u203F', '\u2040')) ~ appendSB()
        ) ~ push(m.Var(sb.toString))
      )

      // copied from Turtle

      // [4t] prefixID ::= "@prefix" PNAME_NS IRIREF "."
      def prefixID: Rule1[(String, Rdf#URI)] = rule {
        "@prefix" ~ WS1 ~ PNAME_NS ~ WS0 ~ IRIREF ~ WS0 ~ '.' ~> ((qname: String, iri: Rdf#URI) => (qname, iri))
      }

      // graph ::= triples ( '.' triples )* '.'?
      def graph: Rule1[Vector[m.Triple[Rdf]]] = rule (
        zeroOrMore(triples).separatedBy('.') ~ optional('.') ~> { (_: Seq[Vector[m.Triple[Rdf]]]).flatten.toVector /* TODO optimize */ }
      )

      // TODO simplify the second part of the 
      // [6t] triples ::= subject predicateObjectList | blankNodePropertyList predicateObjectList?
      def triples: Rule1[Vector[m.Triple[Rdf]]] = rule (
          emptyTriplesAcc ~ (
              subject ~ WS1 ~ predicateObjectList
            | blankNodePropertyList ~> (m.Concrete(_)) ~ WS0 ~ optional(run((n: m.VarOrConcrete[Rdf]) => n :: n :: HNil) ~ predicateObjectList) ~> ((n: m.VarOrConcrete[Rdf]) => ())
//            | blankNodePropertyList ~> (m.Concrete(_)) ~ WS0 ~ optional(run((n: m.VarOrConcrete[Rdf]) => push(n) ~ predicateObjectList ~ push(n)))
          ) ~> (() => push(triplesAcc))
      )

      // [7t] predicateObjectList ::= verb objectList (';' (verb objectList)?)*
      def predicateObjectList: Rule[m.VarOrConcrete[Rdf] :: HNil, HNil] = rule (
        verbObjectList ~ WS0 ~ zeroOrMore(';' ~ WS0 ~ optional(verbObjectList)) ~> ((subject: m.VarOrConcrete[Rdf]) => ())
      )

      // HELPER: contraction of [[ verb objectList ]]
      def verbObjectList: Rule[m.VarOrConcrete[Rdf] :: HNil, m.VarOrConcrete[Rdf] :: HNil] = rule (
        verb ~ WS1 ~ objectList ~> { (p: Rdf#URI) => () }
      )

      // [8t] objectList ::= object (',' object)*
      def objectList: Rule[m.VarOrConcrete[Rdf] :: Rdf#URI :: HNil, m.VarOrConcrete[Rdf] :: Rdf#URI :: HNil] = rule {
        objectt ~ makeTriple ~ WS0 ~ zeroOrMore(',' ~ WS0 ~ objectt ~ WS0 ~ makeTriple)
      }

      // HELPER: assumes that the current subject/predicate state is
      // set, then consumes the latest object on the stack, and
      // produces a Triple as a side-effect
      def makeTriple: Rule[m.VarOrConcrete[Rdf] :: Rdf#URI :: m.VarOrConcrete[Rdf] :: HNil, m.VarOrConcrete[Rdf] :: Rdf#URI :: HNil] = rule (
        run { (s: m.VarOrConcrete[Rdf], p: Rdf#URI, o: m.VarOrConcrete[Rdf]) =>
          triplesAcc = triplesAcc :+ m.Triple(s, p, o)
          push(s) ~ push(p)
        }
      )

      // [9t] verb ::= predicate | 'a'
      def verb: Rule1[Rdf#URI] = rule (
          predicate
        | 'a' ~ push(rdf.`type`)
      )

      // [10t*] subject ::= iri | BlankNode | collection | VAR1
      def subject: Rule1[m.VarOrConcrete[Rdf]] = rule (
          iri ~> (m.Concrete(_))
        | BlankNode ~> (m.Concrete(_))
        | collection ~> (m.Concrete(_))
        | VAR1
      )

      // [11t] predicate ::= iri
      def predicate: Rule1[Rdf#URI] = rule (
        iri
      )

      // [12t*] object ::= iri | BlankNode | collection | blankNodePropertyList | literal | VAR1
      def objectt: Rule1[m.VarOrConcrete[Rdf]] = rule (
          iri ~> (m.Concrete(_))
        | BlankNode ~> (m.Concrete(_))
        | collection ~> (m.Concrete(_))
        | blankNodePropertyList ~> (m.Concrete(_))
        | literal ~> (m.Concrete(_))
        | VAR1
      )

      // [13t] literal ::= RDFLiteral | NumericLiteral | BooleanLiteral
      def literal: Rule1[Rdf#Literal] = rule (
        RDFLiteral | NumericLiteral | BooleanLiteral
      )

      // [14t] blankNodePropertyList ::= '[' predicateObjectList ']'
      def blankNodePropertyList: Rule1[Rdf#BNode] = rule (
        '[' ~ WS0 ~ push{ val bnode = BNode() ; bnode :: m.Concrete(bnode) :: HNil } ~ predicateObjectList ~ WS0 ~ ']'
      )

      // [15t] collection ::= '(' object* ')'
      def collection: Rule1[Rdf#Node] = rule (
        '('  ~ WS0 ~ zeroOrMore(objectt ~ WS0) ~ ')' ~> { os: Seq[m.VarOrConcrete[Rdf]] =>
          if (os.isEmpty) {
            rdf.nil
          } else {
            val bnodes: Seq[Rdf#BNode] = os.map { _ => BNode() }
            val bnodesVar: Seq[m.VarOrConcrete[Rdf]] = bnodes.map(m.Concrete(_))
            val firsts: Seq[m.Triple[Rdf]] = bnodesVar.zip(os).map{ case (bnode, o) => m.Triple(bnode, rdf.first, o) }
            val rests: Seq[m.Triple[Rdf]] = bnodesVar.zip(bnodesVar.tail :+ m.Concrete(rdf.nil)).map { case (b1, b2) => m.Triple(b1, rdf.rest, b2) }
            this.triplesAcc ++= firsts
            this.triplesAcc ++= rests
            bnodes.head
          }
        }
      )

      // [16t] NumericLiteral ::= INTEGER | DECIMAL | DOUBLE
      def NumericLiteral: Rule1[Rdf#Literal] = rule (
          DOUBLE  ~> ((lexicalForm: String) => Literal(lexicalForm, xsd.double))
        | DECIMAL ~> ((lexicalForm: String) => Literal(lexicalForm, xsd.decimal))
        | INTEGER ~> ((lexicalForm: String) => Literal(lexicalForm, xsd.integer))
      )

      // [128s] RDFLiteral ::= String (LANGTAG | '^^' iri)?
      def RDFLiteral: Rule1[Rdf#Literal] = rule (
        String ~ optional(LangOrIRI) ~> ((lexicalForm: String, opt: Option[Either[Rdf#Lang, Rdf#URI]]) => opt match {
          case None                  => Literal(lexicalForm)
          case Some(Left(langtag))   => Literal.tagged(lexicalForm, langtag)
          case Some(Right(datatype)) => Literal(lexicalForm, datatype)
        })
      )

      // just the (LANGTAG | '^^' iri) part
      def LangOrIRI: Rule1[Either[Rdf#Lang, Rdf#URI]] = rule (
          LANGTAG ~> ((lang: Rdf#Lang) => Left(lang))
        | "^^" ~ iri ~> ((datatype: Rdf#URI) => Right(datatype))
      )

      // [133s] BooleanLiteral ::= 'true' | 'false'
      def BooleanLiteral: Rule1[Rdf#Literal] = rule (
          "true" ~ push(xsd.`true`)
        | "false" ~ push(xsd.`false`)
      )

      // [17] String ::= STRING_LITERAL_QUOTE | STRING_LITERAL_SINGLE_QUOTE | STRING_LITERAL_LONG_SINGLE_QUOTE | STRING_LITERAL_LONG_QUOTE
      def String: Rule1[String] = rule (
        STRING_LITERAL_QUOTE | STRING_LITERAL_SINGLE_QUOTE | STRING_LITERAL_LONG_SINGLE_QUOTE | STRING_LITERAL_LONG_QUOTE
      )

      // [135s] iri ::= IRIREF | PrefixedName
      def iri: Rule1[Rdf#URI] = rule (
        IRIREF | PrefixedName
      )

      // [136s] PrefixedName ::= PNAME_LN | PNAME_NS
      def PrefixedName: Rule1[Rdf#URI] = rule (
          PNAME_LN ~> ((prefix, localName) => URI(prefixes(prefix).getString + localName))
        | PNAME_NS ~> (prefix => URI(prefix))
      )

      // [137s] BlankNode ::= BLANK_NODE_LABEL | ANON
      def BlankNode: Rule1[Rdf#BNode] = rule (
        (BLANK_NODE_LABEL | ANON)
      )

      // [18] IRIREF ::= '<' ([^#x00-#x20<>"{}|^`\] | UCHAR)* '>' /* #x00=NULL #01-#x1F=control codes #x20=space */
      def IRIREF: Rule1[Rdf#URI] = rule {
        '<' ~ clearSB() ~ zeroOrMore(IRIREF_CHAR) ~ '>' ~ push(baseURI.resolve(URI(sb.toString())))
      }
  
      // matches a Char in [^#x00-#x20<>"{}|^`\] or /uxxxx or /Uxxxxxxxx, and pushes it on the StringBuffer
      def IRIREF_CHAR: Rule0 = rule (
          UCHAR
        | (CharPredicate('\u0000' to '\u0020') ++ CharPredicate("<>\"{}|^`\\")).negated ~ appendSB()
      )

      // [139s] PNAME_NS ::= PN_PREFIX? ':'
      def PNAME_NS: Rule1[String] = rule {
        optional(PN_PREFIX) ~ ':' ~> ((prefixOpt: Option[String]) => push(prefixOpt.getOrElse("")))
      }

      // [140s] PNAME_LN ::= PNAME_NS PN_LOCAL
      def PNAME_LN: Rule2[String, String] = rule (
        PNAME_NS ~ PN_LOCAL
      )

      // [141s] BLANK_NODE_LABEL ::= '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
      def BLANK_NODE_LABEL: Rule1[Rdf#BNode] = rule (
        "_:" ~ clearSB() ~ BLANK_NODE_LABEL1 ~ optional(BLANK_NODE_LABEL2) ~ push(makeBNode(sb.toString()))
      )

      // PN_CHARS_U | [0-9]
      def BLANK_NODE_LABEL1: Rule0 = rule (
          PN_CHARS_U
        | Digit ~ appendSB()
      )

      // (PN_CHARS | '.')* PN_CHARS
      def BLANK_NODE_LABEL2: Rule0 = rule (
        oneOrMore(PN_CHARS) ~ test(lastChar != '.')
      )

      // [144s] LANGTAG ::= '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
      def LANGTAG: Rule1[Rdf#Lang] = rule (
        '@' ~ capture(oneOrMore(CharPredicate.Alpha) ~ zeroOrMore('-' ~ oneOrMore(CharPredicate.AlphaNum))) ~> ((langString: String) => Lang(langString))
      )

      // [19] INTEGER ::= [+-]? [0-9]+
      def INTEGER: Rule1[String] = rule (
        capture(optional(anyOf("+-")) ~ oneOrMore(CharPredicate.Digit))
      )

      // [20] DECIMAL ::= [+-]? [0-9]* '.' [0-9]+
      def DECIMAL: Rule1[String] = rule (
        capture(optional(anyOf("+-")) ~ zeroOrMore(CharPredicate.Digit) ~ '.' ~ zeroOrMore(CharPredicate.Digit))
      )

      // [21] DOUBLE ::= [+-]? ([0-9]+ '.' [0-9]* EXPONENT | '.' [0-9]+ EXPONENT | [0-9]+ EXPONENT)
      def DOUBLE: Rule1[String] = rule (
        capture(optional(anyOf("+-")) ~ (
            oneOrMore(CharPredicate.Digit) ~ '.' ~ zeroOrMore(CharPredicate.Digit) ~ EXPONENT
          | '.' ~ oneOrMore(CharPredicate.Digit) ~ EXPONENT
          | oneOrMore(CharPredicate.Digit) ~ EXPONENT
        ))
      )

      // [154s] EXPONENT ::= [eE] [+-]? [0-9]+
      def EXPONENT: Rule0 = rule (
        anyOf("eE") ~ optional(anyOf("+-")) ~ oneOrMore(CharPredicate.Digit)
      )

      // [22] STRING_LITERAL_QUOTE ::= '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'      /* #x22=" #x5C=\ #xA=new line #xD=carriage return */
      def STRING_LITERAL_QUOTE: Rule1[String] = rule (
        '"' ~ clearSB() ~ zeroOrMore(ECHAR | UCHAR | noneOf("\"\\\n\r") ~ appendSB()) ~ '"' ~ push(sb.toString())
      )

      // [23] STRING_LITERAL_SINGLE_QUOTE ::= "'" ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)* "'"      /* #x27=' #x5C=\ #xA=new line #xD=carriage return */
      def STRING_LITERAL_SINGLE_QUOTE: Rule1[String] = rule (
        '\'' ~ clearSB() ~ zeroOrMore(ECHAR | UCHAR | noneOf("\"\\\n\r") ~ appendSB()) ~ '\'' ~ push(sb.toString())
      )

      // [24] STRING_LITERAL_LONG_SINGLE_QUOTE ::= "'''" (("'" | "''")? ([^'\] | ECHAR | UCHAR))* "'''"
      def STRING_LITERAL_LONG_SINGLE_QUOTE: Rule1[String] = rule (
        "'''" ~ clearSB() ~ zeroOrMore(optional('\'' ~ appendSB() ~ optional('\'' ~ appendSB())) ~ (ECHAR | UCHAR | noneOf("'\\") ~ appendSB())) ~ "'''" ~ push(sb.toString())
      )

      // [25] STRING_LITERAL_LONG_QUOTE ::= '"""' (('"' | '""')? ([^"\] | ECHAR | UCHAR))* '"""'
      def STRING_LITERAL_LONG_QUOTE: Rule1[String] = rule (
        "\"\"\"" ~ clearSB() ~ zeroOrMore(optional('"' ~ appendSB() ~ optional('"' ~ appendSB())) ~ (ECHAR | UCHAR | noneOf("\"\\") ~ appendSB())) ~ "\"\"\"" ~ push(sb.toString())
      )

      // [26] UCHAR ::= '\\u' HEX HEX HEX HEX | '\\U' HEX HEX HEX HEX HEX HEX HEX HEX
      def UCHAR: Rule0 = rule {
        "\\u" ~ capture(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> ((code: String) => appendSB(java.lang.Integer.parseInt(code, 16).asInstanceOf[Char]))
      }

      // [159s] ECHAR ::= '\' [tbnrf"'\]
      def ECHAR: Rule0 = rule (
        '\\' ~ (
            't'  ~ appendSB('\t')
          | 'b'  ~ appendSB('\b')
          | 'n'  ~ appendSB('\n')
          | 'r'  ~ appendSB('\r')
          | 'f'  ~ appendSB('\f')
          | '\'' ~ appendSB('\"')
          | '\'' ~ appendSB('\'')
          | '\\' ~ appendSB('\\')
        )
      )

      // [161s] WS ::= #x20 | #x9 | #xD | #xA
      def WS: Rule0 = rule { anyOf(" \t\r\n") }

      // [162s] ANON ::= '[' WS* ']'
      def ANON: Rule1[Rdf#BNode] = rule (
        '[' ~ WS0 ~ ']' ~ push(BNode())
      )

      // [163s] PN_CHARS_BASE ::= [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
      def PN_CHARS_BASE: Rule0 = rule (
          CharPredicate.Alpha ~ appendSB()
        | (between('\u00C0', '\u00D6') ++ between('\u00D8', '\u00F6') ++ between('\u00F8', '\u02FF') ++ between('\u0370', '\u037D') ++ between('\u037F', '\u1FFF') ++ between('\u200C', '\u200D') ++ between('\u2070', '\u218F') ++ between('\u2C00', '\u2FEF') ++ between('\u3001', '\uD7FF') ++ between('\uF900', '\uFDCF') ++ between('\uFDF0', '\uFFFD')) ~ appendSB()
      )

      // [164s] PN_CHARS_U ::= PN_CHARS_BASE | '_'
      def PN_CHARS_U: Rule0 = rule (
          PN_CHARS_BASE
        | '_' ~ appendSB()
      )

      // [166s] PN_CHARS ::= PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
      def PN_CHARS: Rule0 = rule (
          PN_CHARS_U
        | (CharPredicate('-') ++ CharPredicate.Digit ++ CharPredicate('\u00B7') ++ between('\u0300', '\u036F') ++ between('\u203F', '\u2040')) ~ appendSB()
      )

      // [167s] PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?
      def PN_PREFIX: Rule1[String] = rule {
        clearSB() ~ PN_CHARS_BASE ~ optional(PN_PREFIX2) ~ push(sb.toString())
      }
  
      // (PN_CHARS | '.')* PN_CHARS
      // so basically PN_CHARS+ with no trailing '.'
      def PN_PREFIX2: Rule0 = rule {
  //      capture(oneOrMore(PN_CHARS)) ~> ((s: String) => test(s.charAt(s.length-1) != '.') ~ appendSB(s))
        oneOrMore(PN_CHARS) ~ test(lastChar != '.')
      }

      // [168s] PN_LOCAL ::= (PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
      def PN_LOCAL: Rule1[String] = rule (
        clearSB() ~ (PN_CHARS_U | (CharPredicate(':') ++ CharPredicate.Digit) ~ appendSB() | PLX) ~ optional(PN_LOCAL2) ~ push(sb.toString())
      )

      // (PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX)
      // so basically (PN_CHARS | '.' | ':' | PLX)+ with no trailing '.'
      def PN_LOCAL2: Rule0 = rule (
        oneOrMore(PN_CHARS | anyOf(".:") ~ appendSB() | PLX) ~ test(lastChar != '.')
      )

      // [169s] PLX ::= PERCENT | PN_LOCAL_ESC
      def PLX: Rule0 = rule (
        PERCENT | PN_LOCAL_ESC
      )

      // [170s] PERCENT ::= '%' HEX HEX
      // [171s] HEX ::= [0-9] | [A-F] | [a-f]
      def PERCENT: Rule0 = rule (
        '%' ~ appendSB() ~ CharPredicate.HexDigit ~ appendSB() ~ CharPredicate.HexDigit ~ appendSB()
      )

      // [172s] PN_LOCAL_ESC ::= '\' ('_' | '~' | '.' | '-' | '!' | '$' | '&' | "'" | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/' | '?' | '#' | '@' | '%')
      def PN_LOCAL_ESC: Rule0 = rule (
        '\\' ~ appendSB() ~ anyOf("_~.-!$&'()*+,;=/?#@%") ~ appendSB()
      )
  
    }

  }

}
