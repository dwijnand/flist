import SbtKitPre._

lazy val asyncseq = project in file(".")

organization := "com.dwijnand"
        name := "asyncseq"
     version := "0.1.0-SNAPSHOT"

      scalaVersion := "2.11.7"
crossScalaVersions := Seq(scalaVersion.value)

maxErrors := 5
triggeredMessage := Watched.clearWhenTriggered

scalacOptions ++= Seq("-encoding", "utf8")
scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")
scalacOptions  += "-language:higherKinds"
scalacOptions  += "-language:implicitConversions"
scalacOptions  += "-language:postfixOps"
scalacOptions  += "-Xfuture"
scalacOptions  += "-Yinline-warnings"
scalacOptions  += "-Yno-adapted-args"
scalacOptions  += "-Ywarn-dead-code"
scalacOptions  += "-Ywarn-numeric-widen"
scalacOptions  += "-Ywarn-unused-import"
scalacOptions  += "-Ywarn-value-discard"

scalacOptions in (Compile, console) -= "-Ywarn-unused-import"
scalacOptions in (Test,    console) -= "-Ywarn-unused-import"

wartremoverWarnings ++= Warts.unsafe
wartremoverWarnings  -= Wart.Any                    // bans f-interpolator #158
wartremoverWarnings  -= Wart.DefaultArguments
wartremoverWarnings  += Wart.FinalCaseClass
wartremoverWarnings  += Wart.JavaConversions
wartremoverWarnings  += Wart.MutableDataStructures
wartremoverWarnings  -= Wart.NonUnitStatements      // bans this.type #118
wartremoverWarnings  -= Wart.Product
wartremoverWarnings  -= Wart.Serializable
wartremoverWarnings  -= Wart.Throw

initialCommands in console += "\nimport com.dwijnand.asyncseq._"
initialCommands in console += "\nimport scala.concurrent._"
initialCommands in console += "\nimport scala.concurrent.duration._"
initialCommands in console += "\nimport scala.concurrent.ExecutionContext.Implicits._"

parallelExecution in Test := true
fork in Test := false

fork in run := true
cancelable in Global := true

watchSources ++= (baseDirectory.value * "*.sbt").get
watchSources ++= (baseDirectory.value / "project" * "*.scala").get
