package retcalc

sealed trait Returns

case class VariableReturn(monthId: String, monthlyRate: Double)

case class FixedReturns(annualRate: Double) extends Returns

case class OffsetReturns(orig: Returns, offset: Int) extends Returns

case class VariableReturns(returns: Vector[VariableReturn]) extends Returns {
  def fromUntil(monthIdFrom: String, monthIdUntil: String): VariableReturns =
    VariableReturns(
      returns
        .dropWhile(_.monthId != monthIdFrom)
        .takeWhile(_.monthId != monthIdUntil))
}

object Returns {
  def monthlyRate(returns: Returns, month: Int): Double = returns match {
    case FixedReturns(r) => r / 12
    case VariableReturns(rs) => rs(month % rs.length).monthlyRate
    case OffsetReturns(rs, offset) => monthlyRate(rs, month + offset)
  }

  def annualizedTotalReturn(returns: Returns): Double = returns match {
    case FixedReturns(r) => r
    case VariableReturns(rs) =>
      val product = rs.foldLeft(1.0) { case (acc, ret) =>
        acc * (1 + ret.monthlyRate)
      }
      (scala.math.pow(product, 1.0 / rs.size) - 1) * 12
    case OffsetReturns(rs, _) => annualizedTotalReturn(rs)
  }

  def fromEquityAndInflationData(equities: Vector[EquityData],
                                 inflation: Vector[InflationData]):
  VariableReturns = {
    VariableReturns(equities.zip(inflation).sliding(2).collect {
      case (prevEquity, prevInflation) +: (equity, inflation) +: Vector() =>
        val inflationRate = inflation.value / prevInflation.value
        val totalReturn = (equity.value + equity.monthlyDividend) / prevEquity.value
        val realTotalReturn = totalReturn - inflationRate
        VariableReturn(equity.monthId, realTotalReturn)
    }.toVector)
  }
}