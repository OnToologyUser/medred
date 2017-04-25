package ch.hevs.medred.vocab

import ch.hevs.rdftools.rdf.Vocab
import ch.hevs.rdftools.rdf._
import ch.hevs.rdftools.rdf.RdfTools._

object MedRed extends Vocab {
  override val iri: Iri = "http://aislab.hevs.ch/ontology/medred#"
  val varName = prop("varName")
  val hasValue=prop("hasValue")
  val ofInstrument=prop("ofInstrument")
  val ofStudy=prop("ofStudy")
  val ofSection=prop("ofSection")
  val items = prop("items")
  val dataType = prop("dataType")
  val choices=prop("choices")
  val calculation=prop("calculation")
  val Instrument=clazz("Instrument")
  val Item=clazz("Item")
  val Section=clazz("Section")
  val Question=clazz("Question")
  val Operation=clazz("Operation")
  val Questionnaire=clazz("Questionnaire")
  val Information=clazz("Information")
  val Choice=clazz("Choice")
  val ChoiceList=clazz("ChoiceList")
  val ItemList=clazz("ItemList")
}