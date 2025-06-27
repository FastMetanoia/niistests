package v2

@main def testErlang() =
  val d = 1
  val k = 1
  val lambda = .407
  val size = 100000
  // Генерируем 10 случайных чисел
  val samples = Seq.fill(size)(ErlangGen.ErlangGenerator.erlang(k, lambda))
  val grouped = samples.groupBy(x=>x).map((n,xs)=>(n,xs.length)).toSeq.sortBy(_._1)
  grouped.foreach((n,m) => println(s"$n:" + "#"*math.ceil((m +.0)/ size * 100).toInt + f"${(m +.0)/ size * 100}%.4f"+"%"))
  samples.take(10).tapEach(println)
  val mean = (samples.sum.toDouble / samples.size)
  println(mean)