package ch.hevs.medred.vocab

import rdftools.rdf.Vocab

import rdftools.rdf._

import rdftools.rdf.RdfTools._

object MedRed extends Vocab {
  override val iri: Iri = "http://w3id.org/medred/medred#"
  val Questionnaire = clazz("Questionnaire")
  val Information = clazz("Information")
  val Question = clazz("Question")
  val Instrument = clazz("Instrument")
  val Item = clazz("Item")
  val CaseReportForm = clazz("CaseReportForm")
  val Study = clazz("Study")
  val ItemList = clazz("ItemList")
  val Section = clazz("Section")
  val ChoiceList = clazz("ChoiceList")
  val Choice = clazz("Choice")
  val Operation = clazz("Operation")
  val choices = prop("choices")
  val instrument = prop("instrument")
  val ofInstrument = prop("ofInstrument")
  val dataType = prop("dataType")
  val ofSection = prop("ofSection")
  val items = prop("items")
  val studyName = prop("studyName")
  val varName = prop("varName")
  val calculation = prop("calculation")
  val hasValue=prop("hasValue")
  val validationShape=prop("validationShape")
  val dataValue=prop("dataValue")
  
  // added manually, consider adding in MedRed ontology
  val Record = clazz("Record")
  val valueList = prop("valueList")
  val varNameList = prop("varNameList")
}