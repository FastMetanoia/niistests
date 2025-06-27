package v2

object ErlangGen {

  import scala.util.Random

  object ErlangGenerator {
    private val rnd = new Random()

    /**
     * Генерирует случайную величину с эрланговским распределением.
     *
     * @param k      порядок распределения (целое число ≥ 1)
     * @param lambda параметр интенсивности (> 0)
     * @return случайное значение X ~ Erlang(k, lambda)
     */
    def gamma(k: Int, lambda: Double): Double = {
      require(k >= 1, "порядок k должен быть ≥ 1")
      require(lambda > 0, "параметр lambda должен быть > 0")

      // Сумма k независимых экспоненциальных величин
      var sum = 0.0
      for (_ <- 1 to k) {
        val u = rnd.nextDouble()
        sum += -math.log(u) / lambda
      }
      sum
    }
    def erlang(k: Int, lambda: Double):Int =
      math.ceil(gamma(k,lambda)).toInt
  }

}
