package com.github.kmizu.macro_peg

import com.github.kmizu.macro_peg.EvaluationResult.{Success, Failure}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.diagrams.Diagrams

class HigherOrderAdvancedSpec extends AnyFunSpec with Diagrams {
  describe("Advanced higher-order macro evaluation") {
    it("passes functions between rules") {
      val grammar = Parser.parse(
        """S = ApplyTwice(Double, "a") !.;
          |Double(s: ?) = s s;
          |ApplyTwice(f: ?, s: ?) = f(f(s));
        """.stripMargin)
      val expanded = MacroExpander.expandGrammar(grammar)
      val evaluator = Evaluator(expanded)
      val result = evaluator.evaluate("aaaa", Symbol("S"))
      assert(result == Success(""))
    }

    it("returns functions from rules") {
      // This test shows that functions can be constructed and immediately applied
      val grammar = Parser.parse(
        """S = MakeDoubler("a") !.;
          |MakeDoubler(x: ?) = Apply((s -> s s), x);
          |Apply(f: ?, x: ?) = f(x);
        """.stripMargin)
      val expanded = MacroExpander.expandGrammar(grammar)
      val evaluator = Evaluator(expanded)
      val result = evaluator.evaluate("aa", Symbol("S"))
      assert(result == Success(""))
    }

    it("composes functions") {
      val grammar = Parser.parse(
        """S = Compose((s -> s s), (s -> s "x"), "a") !.;
          |Compose(f: ?, g: ?, x: ?) = f(g(x));
        """.stripMargin)
      val expanded = MacroExpander.expandGrammar(grammar)
      val evaluator = Evaluator(expanded)
      val result = evaluator.evaluate("axax", Symbol("S"))
      assert(result == Success(""))
    }

    it("uses higher-order functions with multiple parameters") {
      val grammar = Parser.parse(
        """S = Map2((x, y -> x y x), "a", "b") !.;
          |Map2(f: ?, x: ?, y: ?) = f(x, y);
        """.stripMargin)
      val expanded = MacroExpander.expandGrammar(grammar)
      val evaluator = Evaluator(expanded)
      val result = evaluator.evaluate("aba", Symbol("S"))
      assert(result == Success(""))
    }

    it("creates and uses curried functions") {
      val grammar = Parser.parse(
        """S = Curry((x, y -> x y))("a")("b") !.;
          |Curry(f: ?) = (x -> (y -> f(x, y)));
        """.stripMargin)
      val expanded = MacroExpander.expandGrammar(grammar)
      val evaluator = Evaluator(expanded)
      val result = evaluator.evaluate("ab", Symbol("S"))
      assert(result == Success(""))
    }
  }
}