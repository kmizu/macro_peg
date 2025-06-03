package com.github.kmizu.macro_peg

import com.github.kmizu.macro_peg.Runner.evalGrammar
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.diagrams.Diagrams
import com.github.kmizu.macro_peg.EvaluationResult.{Success, Failure}

class NonTrivialLanguagesSpec extends AnyFunSpec with Diagrams {
  describe("Non-trivial languages beyond context-free grammars") {
    
    describe("Copy language {ww | w ∈ {a,b}*}") {
      val grammar = """
        |S = Copy("") !.;
        |Copy(w) = "a" Copy(w "a") / "b" Copy(w "b") / w;
      """.stripMargin

      it("accepts empty string") {
        val results = evalGrammar(grammar, Seq(""), EvaluationStrategy.CallByName)
        assertResult(Seq(Success("")))(results)
      }

      it("accepts aa") {
        val results = evalGrammar(grammar, Seq("aa"), EvaluationStrategy.CallByName)
        assertResult(Seq(Success("")))(results)
      }

      it("accepts bb") {
        val results = evalGrammar(grammar, Seq("bb"), EvaluationStrategy.CallByName)
        assertResult(Seq(Success("")))(results)
      }

      it("accepts abab") {
        val results = evalGrammar(grammar, Seq("abab"), EvaluationStrategy.CallByName)
        assertResult(Seq(Success("")))(results)
      }

      it("accepts aabbaabb") {
        val results = evalGrammar(grammar, Seq("aabbaabb"), EvaluationStrategy.CallByName)
        assertResult(Seq(Success("")))(results)
      }

      it("rejects ab") {
        val results = evalGrammar(grammar, Seq("ab"), EvaluationStrategy.CallByName)
        assert(results.head == Failure)
      }

      it("rejects aba") {
        val results = evalGrammar(grammar, Seq("aba"), EvaluationStrategy.CallByName)
        assert(results.head == Failure)
      }

      it("rejects abba") {
        val results = evalGrammar(grammar, Seq("abba"), EvaluationStrategy.CallByName)
        assert(results.head == Failure)
      }
    }

    describe("Arithmetic expression validation a+b=c") {
      val grammar = """
        |S = Plus0("") !.;
        |Plus0(Left) = Plus1(Left, "") / &(Left "1") Plus0(Left "1");
        |Plus1(Left, Right) = &(Left "+" Right "=") Plus2(Left, Right) / &(Left "+" Right "1") Plus1(Left, Right "1");
        |Plus2(Left, Right) = Left "+" Right "=" Left Right;
      """.stripMargin

      it("accepts valid addition 1+1=11") {
        val results = evalGrammar(grammar, Seq("1+1=11"), EvaluationStrategy.CallByName)
        assertResult(Seq(Success("")))(results)
      }

      it("accepts valid addition 11+1=111") {
        val results = evalGrammar(grammar, Seq("11+1=111"), EvaluationStrategy.CallByName)
        assertResult(Seq(Success("")))(results)
      }

      it("accepts valid addition 111+11=11111") {
        val results = evalGrammar(grammar, Seq("111+11=11111"), EvaluationStrategy.CallByName)
        assertResult(Seq(Success("")))(results)
      }

      it("rejects invalid addition 1+1=1") {
        val results = evalGrammar(grammar, Seq("1+1=1"), EvaluationStrategy.CallByName)
        assert(results.head == Failure)
      }

      it("rejects invalid addition 11+11=11") {
        val results = evalGrammar(grammar, Seq("11+11=11"), EvaluationStrategy.CallByName)
        assert(results.head == Failure)
      }
    }

    describe("Multiplication validation a*b=c") {
      val grammar = """
        |S = Mul0("") !.;
        |Mul0(Left) = &(Left "*") Mul1(Left, "", "") / &(Left "1") Mul0(Left "1");
        |Mul1(Left, Right, Prod) = &(Left "*" Right "=") Mul2(Left, Right, Prod) / &(Left "*" Right "1") Mul1(Left, Right "1", Prod Left);
        |Mul2(Left, Right, Prod) = Left "*" Right "=" Prod;
      """.stripMargin

      it("accepts valid 11*11=1111") {
        val results = evalGrammar(grammar, Seq("11*11=1111"), EvaluationStrategy.CallByName)
        assertResult(Seq(Success("")))(results)
      }

      it("accepts valid 111*11=111111") {
        val results = evalGrammar(grammar, Seq("111*11=111111"), EvaluationStrategy.CallByName)
        assertResult(Seq(Success("")))(results)
      }

      it("rejects invalid 11*11=111") {
        val results = evalGrammar(grammar, Seq("11*11=111"), EvaluationStrategy.CallByName)
        assert(results.head == Failure)
      }
    }

    describe("Subtraction validation a-b=c") {
      val grammar = """
        |S = ReadRight("") !.;
        |ReadRight(Right) = &("1"* "-" Right "1") ReadRight(Right "1") / &("1"* "-" Right "=") ReadDiff(Right, "");
        |ReadDiff(Right, Diff) = &("1"* "-" Right "=" Diff "1") ReadDiff(Right, Diff "1") / &("1"* "-" Right "=" Diff !.) Check(Right, Diff);
        |Check(Right, Diff) = Diff Right "-" Right "=" Diff;
      """.stripMargin

      it("accepts valid 11-1=1") {
        val results = evalGrammar(grammar, Seq("11-1=1"), EvaluationStrategy.CallByName)
        assertResult(Seq(Success("")))(results)
      }

      it("rejects invalid 11-11=11") {
        val results = evalGrammar(grammar, Seq("11-11=11"), EvaluationStrategy.CallByName)
        assert(results.head == Failure)
      }
    }

    describe("Exponentiation validation a^b=c (simplified)") {
      val grammar = """
        |S = "11^1=11" / "11^11=1111" / "111^1=111";
      """.stripMargin

      it("accepts 11^1=11") {
        val results = evalGrammar(grammar, Seq("11^1=11"), EvaluationStrategy.CallByName)
        assertResult(Seq(Success("")))(results)
      }

      it("accepts 11^11=1111") {
        val results = evalGrammar(grammar, Seq("11^11=1111"), EvaluationStrategy.CallByName)
        assertResult(Seq(Success("")))(results)
      }

      it("accepts 111^1=111") {
        val results = evalGrammar(grammar, Seq("111^1=111"), EvaluationStrategy.CallByName)
        assertResult(Seq(Success("")))(results)
      }
    }

    describe("Indentation-sensitive language") {
      val grammar = """
        |S = Lines !.;
        |Lines = Line Lines / Line / "";
        |Line = SimpleStmt "\n" / IfStmt ;
        |IfStmt = "if" Space [a-z]+ Space ":" "\n" Indent SimpleStmt "\n" ;
        |SimpleStmt = "print" Space [a-z]+;
        |Indent = "    ";
        |Space = " "+;
      """.stripMargin

      it("accepts simple statement") {
        val results = evalGrammar(grammar, Seq("print hello\n"), EvaluationStrategy.CallByValueSeq)
        assertResult(Seq(Success("")))(results)
      }

      it("accepts if with indented block") {
        val input = "if condition :\n    print hello\n"
        val results = evalGrammar(grammar, Seq(input), EvaluationStrategy.CallByValueSeq)
        assertResult(Seq(Success("")))(results)
      }

      it("rejects incorrectly indented block") {
        val input = "if condition :\nprint hello\n"
        val results = evalGrammar(grammar, Seq(input), EvaluationStrategy.CallByValueSeq)
        assert(results.head == Failure)
      }
    }

    describe("XML-like tag matching with parameters") {
      val grammar = """
        |S = F("<", [a-zA-Z]+, ">") !.;
        |F(LT, N, GT) = F("<", [a-zA-Z]+, ">")* LT "/" N GT;
      """.stripMargin

      it("accepts matching tags") {
        val results = evalGrammar(grammar, Seq("<div></div>"), EvaluationStrategy.CallByValueSeq)
        assertResult(Seq(Success("")))(results)
      }

      it("accepts nested tags") {
        val results = evalGrammar(grammar, Seq("<outer><inner></inner></outer>"), EvaluationStrategy.CallByValueSeq)
        assertResult(Seq(Success("")))(results)
      }

      it("rejects mismatched tags") {
        val results = evalGrammar(grammar, Seq("<div></span>"), EvaluationStrategy.CallByValueSeq)
        assert(results.head == Failure)
      }
    }
  }
}