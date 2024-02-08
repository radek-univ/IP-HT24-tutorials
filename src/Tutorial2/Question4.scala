package Tutorial2

object Question4 {
  // - Can we make this loop run a different number of steps than { numSteps, numSteps + 1, ∞ }?
  // - How to write a more reliable loop?
  def loopUsingDoubles(): Unit = {
    val timeEnd: Double = 1.0
    val numSteps: Int = Int.MaxValue
    var step: Int = 0
    // val timeStep: Double = timeEnd / numSteps
    var time = 0.0
    while (step < numSteps) {
      // Inv: 0 <= time <= timeEnd and time=k*timeStep for some k∈N
      time = timeEnd * step / numSteps
      step += 1
    }
    println(s"requested: $numSteps, actual: ")
  }

  def fixedLoop(): Unit = {
    val timeEnd: Double = 1.0
    val numSteps: Int = Int.MaxValue
    var time = 0.0
    var i = 0
    while (i < numSteps) {
      time = timeEnd * i / numSteps
      i += 1
    }
  }

  def main(args: Array[String]): Unit = {
    loopUsingDoubles()
  }
}
