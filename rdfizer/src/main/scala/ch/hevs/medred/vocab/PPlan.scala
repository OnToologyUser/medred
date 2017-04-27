package ch.hevs.medred.vocab

import rdftools.rdf.Vocab

import rdftools.rdf._

import rdftools.rdf.RdfTools._

object PPlan extends Vocab {
  override val iri: Iri = "http://purl.org/net/p-plan#"
  val MultiStep = clazz("MultiStep")
  val Activity = clazz("Activity")
  val Step = clazz("Step")
  val Entity = clazz("Entity")
  val Bundle = clazz("Bundle")
  val Plan = clazz("Plan")
  val Variable = clazz("Variable")
  val correspondsToStep = prop("correspondsToStep")
  val isStepOfPlan = prop("isStepOfPlan")
  val isSubPlanOfPlan = prop("isSubPlanOfPlan")
  val isInputVarOf = prop("isInputVarOf")
  val isVariableOfPlan = prop("isVariableOfPlan")
  val isOutputVarOf = prop("isOutputVarOf")
  val isDecomposedAsPlan = prop("isDecomposedAsPlan")
  val hasInputVar = prop("hasInputVar")
  val correspondsToVariable = prop("correspondsToVariable")
  val isPrecededBy = prop("isPrecededBy")
  val hasOutputVar = prop("hasOutputVar")
}