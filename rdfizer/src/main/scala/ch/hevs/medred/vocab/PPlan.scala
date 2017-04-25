package ch.hevs.medred.vocab

import ch.hevs.rdftools.rdf.Vocab
import ch.hevs.rdftools.rdf._
import ch.hevs.rdftools.rdf.RdfTools._

object PPlan extends Vocab {
  override val iri: Iri = "http://purl.org/net/p-plan#"
  val hasOutputVar = prop("hasOutputVar")
  val isPrecededBy=prop("isPrecededBy")
  val Variable =clazz("Variable")
}