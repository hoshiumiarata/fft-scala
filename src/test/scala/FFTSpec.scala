import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.junit.Assert._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FFTSpec extends FlatSpec {
  it should "be true" in {
    val discretization_rate = 32.0
    val count = 32
    val frequency = 4
    val magnitude = 5
    val time = count / discretization_rate
    val x = new Array[Double](count)

    for (i <- 0 until x.length) {
      x(i) = Math.cos(frequency * 2 * Math.PI * i.toDouble / discretization_rate) * magnitude
    }

    val X = FFT.rfft(x)
    for (k <- 0 until X.length) {
      if (frequency === k / time) {
        assertEquals(X(k).magnitude / count * 2.0, magnitude, 1e-5)
        assertEquals(X(k).phase, 0.0, 1e-5)
      } else {
        assertEquals(X(k).magnitude / count * 2.0, 0.0, 1e-5)
      }
    }
  }
}
