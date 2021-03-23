scalaVersion := "2.13.3"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Ymacro-annotations"
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.2.0"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.6" % Test,
  "org.scalatestplus" %% "scalacheck-1-15" % "3.2.6.0" % Test
)
