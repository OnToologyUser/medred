package ch.hevs.medred

import scala.io.Source
import org.apache.jena.datatypes.xsd.XSDDatatype

case class Study(id:String)

case class Instrument(name:String,items:Seq[Item])

class Item(name:String,label:String) {
  
}

case class Section(sectionName:String,label:String) extends Item(sectionName,label)

case class Field(fieldName:String,label:String,variable:Variable) extends Item(fieldName,label)

case class Variable(varName:String,varType:XSDDatatype)

object Rdfizer {
  
  //"Variable / Field Name","Form Name","Section Header","Field Type",
  //"Field Label","Choices, Calculations, OR Slider Labels","Field Note",
  //"Text Validation Type OR Show Slider Number","Text Validation Min","Text Validation Max",
  //Identifier?,"Branching Logic (Show field only if...)","Required Field?","Custom Alignment",
  //"Question Number (surveys only)","Matrix Group Name","Matrix Ranking?","Field Annotation"  
  
  def loadRedCap(file:String)={
    Source.fromFile(file).getLines
    
  }
  
  
  def main (args:Array[String])={
    
  }
}