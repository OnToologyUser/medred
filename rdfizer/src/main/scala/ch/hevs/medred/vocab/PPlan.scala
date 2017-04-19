package ch.hevs.medred.vocab

import ch.hevs.rdftools.rdf.Vocab
import ch.hevs.rdftools.rdf._
import ch.hevs.rdftools.rdf.RdfTools._

object PPlan extends Vocab {
  override val iri: Iri = "http://purl.org/net/p-plan#"
  val hasOutputVar = PPlan("hasOutputVar")
  val isPrecededBy=PPlan("isPrecededBy")
  val Variable =PPlan("Variable")
}