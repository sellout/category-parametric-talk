inThisBuild(Seq(
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.12.4-bin-typelevel-4",
  scalacOptions := Seq(
    "-language:higherKinds",
    "-Ykind-polymorphism"),
  libraryDependencies := Seq(
    "org.typelevel" %% "cats-core" % "1.3.1",
    "org.typelevel" %% "spire"     % "0.16.0")))

addCompilerPlugin(
  "org.spire-math" %% "kind-projector" % "0.9.8")
