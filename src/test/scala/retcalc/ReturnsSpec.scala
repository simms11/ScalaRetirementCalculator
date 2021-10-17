package retcalc

import org.scalactic.{Equality, TolerantNumerics, TypeCheckedTripleEquals}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ReturnsSpec  extends AnyWordSpec with Matchers with TypeCheckedTripleEquals{
  implicit val doubleEquality:Equality[Double] =
    TolerantNumerics.tolerantDoubleEquality(0.001)

  "Returns.monthlyRate" should{
    "return a fixed rate for FixedReturn" in {
      Returns.monthlyRate(FixedReturns(0.04), 0) should === (0.04/12)
      Returns.monthlyRate(FixedReturns(0.04),10 )should === (0.04 /12)
    }
  }

  val variableReturns = VariableReturns(Vector(
    VariableReturn("2000.01", 0.1),
    VariableReturn("2000.02", 0.2)))

    "return the nth rate for VariableReturn" in {
      Returns.monthlyRate(variableReturns, 0) should === (0.1)
      Returns.monthlyRate(variableReturns, 1) should === (0.2)
    }

  "roll over from the first rate if n > length" in {
    Returns.monthlyRate(variableReturns, 2) should === (0.1)
    Returns.monthlyRate(variableReturns,3) should === (0.2)
    Returns.monthlyRate(variableReturns, 4) should === (0.1)

  }

  "return the n+offset th rate for OffsetReturn" in {
    val returns = OffsetReturns(variableReturns, 1)
    Returns.monthlyRate(returns, 0) should ===(0.2)
    Returns.monthlyRate(returns, 1) should ===(0.1)
  }

  "Returns.fromEquityAndInflationData" should {
    "compute real total returns from equity and inflation data" in {
      val equities = Vector(
        EquityData("2117.01", 100.0, 10.0),
        EquityData("2117.02", 101.0, 12.0),
        EquityData("2117.03", 102.0, 12.0))

      val inflation = Vector(
        InflationData("2117.01", 100.0),
        InflationData("2117.02", 102.0),
        InflationData("2117.03", 102.0))

      val returns = Returns.fromEquityAndInflationData(equities,
        inflation)
      returns should ===(VariableReturns(Vector(
        VariableReturn("2117.02", (101.0 + 12.0 / 12) / 100.0 - 102.0 /
          100.0),
        VariableReturn("2117.03", (102.0 + 12.0 / 12) / 101.0 - 102.0 /
          102.0))))
    }
  }

  "Returns.annualizedTotalReturn" should {
    val returns = VariableReturns(Vector.tabulate(12)(i => VariableReturn(i.toString, i.toDouble / 100 / 12)))
    val avg = Returns.annualizedTotalReturn(returns)
    "compute a geometric mean of the returns" in {
      avg should ===(0.0549505735)
    }

    "compute an average that can be used to calculate a futureCapital instead of using variable returns" in {
      // calculation only works if the capital doesn't change over time
      // otherwise, the capital fluctuates as well as the interest rates, and we cannot use the mean
      val futCapVar = RetCalc.futureCapital(returns, 12, 0, 0, 500000)
      val futCapFix = RetCalc.futureCapital(FixedReturns(avg), 12, 0, 0, 500000)
      futCapVar should ===(futCapFix)
    }
  }
}
