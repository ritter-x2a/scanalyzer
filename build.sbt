lazy val root = (project in file(".")).
  settings(
    // basic settings
    name := "scanalyzer",
    version := "0.1",

    // ScalaTest dependencies
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
  )
