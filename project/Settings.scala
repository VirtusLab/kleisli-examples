import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import sbt.Keys._
import sbt.Resolver

object Settings {
  private val compilerOptions = Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-Xlint",
    "-Xfatal-warnings",
    "-target:jvm-1.8"
  )

  private val scalariformSettings = SbtScalariform.scalariformSettings ++ Seq(
    ScalariformKeys.preferences := formattingPreferences
  )

  private def formattingPreferences = {
    import scalariform.formatter.preferences._
    FormattingPreferences().
    setPreference(PreserveSpaceBeforeArguments, true).
    setPreference(PreserveDanglingCloseParenthesis, true).
    setPreference(AlignParameters, true).
    setPreference(AlignSingleLineCaseStatements, true)
  }

  lazy val commonSettings = Seq(
    organization := "org.virtuslab",
    organizationName := "VirtusLab",
    version := "1.0",
    scalaVersion := "2.11.7",
    scalacOptions ++= compilerOptions,
    resolvers += Resolver.sonatypeRepo("releases")
  ) ++ scalariformSettings
}
