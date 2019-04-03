
name := "medred"
organization := "ch.hevs"
version := "0.0.1"
scalaVersion := "2.12.3"

//enablePlugins(JavaAppPackaging)
  
libraryDependencies ++= Seq(
  "org.apache.jena" % "apache-jena-libs" % "3.1.0",
  "com.github.jpcik" % "rdf-tools" % "v0.1.2",
  "com.github.jpcik.rdf-tools" %% "rdf-tools-jena" % "v0.1.2",
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-csv" % "2.7.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.codehaus.groovy" % "groovy-all" % "2.4.13",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "junit" % "junit" % "4.12" % "test"
)
                                 
resolvers ++= Seq(
  "typesafe" at "http://repo.typesafe.com/typesafe/releases/",
  Resolver.sonatypeRepo("public"),
  "jitpack" at "https://jitpack.io"
)

//scriptClasspath := Seq("*")

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

scalacOptions ++= Seq("-feature","-deprecation")