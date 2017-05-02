package ch.hevs.medred

import org.apache.jena.datatypes.xsd.XSDDatatype
import org.joda.time.DateTime

case class Study(id:String,name:String,notes:String,description:String,protocolName:String,
    version:String,created:DateTime,modified:DateTime,instruments:Seq[Instrument]) {
  def copy(newInstruments:Seq[Instrument])=Study(id,name,notes,description,protocolName,
      version,created,modified,newInstruments)
}

case class Instrument(name:String,items:Seq[Item])

trait Item{
  def name:String
  def label:String 
  def note:String 
}

case class Choice(value:Any,label:String)


case class Section(name:String,label:String,note:String,items:Seq[Item],matrix:Boolean=false) 
  extends Item
  
case class Question(name:String,label:String,note:String,field:Field,choices:Seq[Choice]) 
  extends Item

case class Operation(name:String,label:String,note:String,field:Field)
  extends Item

case class Field(fieldName:String,control:Control,variable:Variable,
    validation:Option[ValidationShape],computation:Option[String]) 

case class Note(name:String,label:String,note:String="") 
  extends Item
  
case class Variable(varName:String,varType:XSDDatatype)

trait PropertyShape

case class ValidationShape(propShapes:Seq[PropertyShape])

case class MaxInclShape(maxVal:Double) extends PropertyShape
case class MaxExclShape(maxVal:Double) extends PropertyShape
case class MinInclShape(minVal:Double) extends PropertyShape
case class MinExclShape(minVal:Double) extends PropertyShape


