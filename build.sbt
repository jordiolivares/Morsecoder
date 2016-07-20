lazy val root = (project in file(".")).
  settings(
    scalaVersion := "2.11.8",
    libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0",
    resolvers += Resolver.sonatypeRepo("public")
  )
