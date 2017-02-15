name := "eff-multiple-tests"

scalaVersion := "2.11.8"

libraryDependencies += "org.atnos" %% "eff" % "3.0.0"

// to write types like Reader[String, ?]
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.0")

// to get types like Reader[String, ?] (with more than one type parameter) correctly inferred
addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0")
