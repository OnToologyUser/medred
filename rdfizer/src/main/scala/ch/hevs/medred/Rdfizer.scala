package ch.hevs.medred

import scala.io.Source
import org.apache.jena.datatypes.xsd.XSDDatatype
import scala.collection.mutable.ArrayBuffer
import org.apache.jena.rdf.model.Model
import org.apache.jena.rdf.model.ModelFactory
import rsp.data.Iri
import rsp.data.Triple

case class Study(id:String)

case class Instrument(name:String,items:Seq[Item])

class Item(val name:String,label:String,note:String) {
  
}

case class FieldType(id:String)
object TextType extends FieldType("text")
object OptionType extends FieldType("option")
object FileType extends FieldType("file")
object NoType extends FieldType("none")

object FieldType {
  def parse(str:String)=str match {
    case "text"=>TextType
    case "dropdown"=>OptionType
    case "file"=>FileType
  //  case _ =>NoType
  }
}


case class Section(sectionName:String,label:String,note:String,items:Seq[Item]) extends Item(sectionName,label,note)

case class Field(fieldName:String,label:String,note:String,fieldType:FieldType,variable:Variable) extends Item(fieldName,label,note)

case class Note(noteName:String,label:String) extends Item(noteName,label,"")

case class Variable(varName:String,varType:XSDDatatype)

object Rdfizer {
  
  //"Variable / Field Name","Form Name","Section Header","Field Type",
  //"Field Label","Choices, Calculations, OR Slider Labels","Field Note",
  //"Text Validation Type OR Show Slider Number","Text Validation Min","Text Validation Max",
  //Identifier?,"Branching Logic (Show field only if...)","Required Field?","Custom Alignment",
  //"Question Number (surveys only)","Matrix Group Name","Matrix Ranking?","Field Annotation"  
 
  def loadRedCap(file:String)={
    var instrumentName=""
    var section:Option[Section]=None
    var nr=1
    var secNr=0
    val items=ArrayBuffer[Item]()
    Source.fromFile(file).getLines.drop(1).foreach { line =>
      val r=""",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)"""
      val data=  line.split(r,-1)
      println(line)
      println(data.size)
      val fieldName=data(0)
      if (nr==1) 
        instrumentName=data(1)
      
      
      
      val fieldLabel=data(4)
      val fieldNote=data(6)

      val field=if (data(3)=="descriptive")
          Note(fieldName,fieldLabel)
        else {
          val fieldType=FieldType.parse(data(3))
          Field(fieldName,fieldLabel,fieldNote,fieldType,null)  
        }
        

      val sectionName=data(2)
      if (!sectionName.isEmpty){
        secNr+=1
        section=Some(Section("section"+secNr,sectionName,"",ArrayBuffer(field)))
        items+=section.get
      }
      else
        section.get.items.asInstanceOf[ArrayBuffer[Item]]+=field
      if (!section.isDefined)
        items+=field
      nr+=1 
    }
    Instrument(instrumentName,items)    
  }
  
  def toRdf(instr:Instrument)={
    import rsp.util.JenaTools._
    import rsp.data.Literal._
    val m= ModelFactory.createDefaultModel
    val pfx="http://example.org/"
    val t=Triple(Iri(pfx), Iri(""), lit(""))
    //m.add(toJenaTriple(t))
    
  }
  
  
  def main (args:Array[String])={
    val instr=loadRedCap("src/main/resources/instruments/childs_sleep_habits_questionnaire_cshq.csv")
    instr.items.foreach {d=>
      d match {
        case s:Section=>println(s.sectionName)
          println(s.items.map { i => i.name }.mkString(","))
        
      }
    }
  }
}