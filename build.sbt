val scala3Version = "3.5.2"

  resolvers += "Akka library repository".at("https://repo.akka.io/maven")

  lazy val akkaVersion = "2.9.7"

  // Run in a separate JVM, to make sure sbt waits until all threads have
  // finished before returning.
  // If you want to keep the application running while executing other
  // sbt tasks, consider https://github.com/spray/sbt-revolver/
  fork := true

lazy val root = project
  .in(file("."))
  .settings(
    name := "niistests",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.4" % Test,
      "org.scala-graph" %% "graph-core" % "2.0.2",
      "org.scala-graph" %% "graph-dot" % "2.0.0",
      "org.scala-lang" %% "toolkit" % "0.7.0",
      // "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
      // "ch.qos.logback" % "logback-classic" % "1.2.13",
      // "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
      "org.scalatest" %% "scalatest" % "3.2.15" % Test
      )
  )

