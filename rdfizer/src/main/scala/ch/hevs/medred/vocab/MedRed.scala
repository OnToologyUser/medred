package ch.hevs.medred.vocab

import ch.hevs.rdftools.rdf.Vocab
import ch.hevs.rdftools.rdf._
import ch.hevs.rdftools.rdf.RdfTools._

object MedRed extends Vocab {
  override val iri: Iri = "http://aislab.hevs.ch/ontology/medred#"
  val varName = MedRed("varName")
  val hasValue=MedRed("hasValue")
  val items = MedRed("items")
  val dataType = MedRed("dataType")
  val choices=MedRed("choices")
  val calculation=MedRed("calculation")
  val Instrument=MedRed("Instrument")
  val Item=MedRed("Item")
  val Section=MedRed("Section")
  val Field=MedRed("Field")
  val Question=MedRed("Question")
  val Operation=MedRed("Operation")
  val Questionnaire=MedRed("Questionnaire")
  val Information=MedRed("Information")
  val Choice=MedRed("Choice")
}