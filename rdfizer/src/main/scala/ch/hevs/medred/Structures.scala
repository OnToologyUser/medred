package ch.hevs.medred

import org.apache.jena.datatypes.xsd.XSDDatatype

case class Study(id:String)

case class Instrument(name:String,items:Seq[Item])

trait Item{
  def name:String
  def label:String 
  def note:String 
}

case class Choice(value:Any,label:String)

case class FieldType(id:String){
 /* def toXsd=this match {
    case TextType=>XSDDatatype.XSDstring
    case LongTextType=>XSDDatatype.XSDstring
    case FileType=>XSDDatatype.XSDbase64Binary
    case OptionType=>XSDDatatype.XSDstring
    case OneOptionType=>XSDDatatype.XSDstring
    case CalculationType=>XSDDatatype.XSDdouble
  }*/
}
object TextType extends FieldType("text")
object OptionType extends FieldType("option")
object OneOptionType extends FieldType("oneoption")
object FileType extends FieldType("file")
object NoType extends FieldType("none")
object LongTextType extends FieldType("longtext")
object CalculationType extends FieldType("calc")


object FieldType {
  def parse(str:String)=str match {
    case "text"=>TextType
    case "dropdown"=>OptionType
    case "radio"=>OneOptionType
    case "file"=>FileType
    case "notes"=>LongTextType
    case "calc"=>CalculationType
    
  //  case _ =>NoType
  }
}


case class Section(name:String,label:String,note:String,items:Seq[Item],matrix:Boolean=false) 
  extends Item//(sectionName,label,note)
  
case class Question(name:String,label:String,note:String,field:Field,choices:Seq[Choice]) 
  extends Item//(questionName,label,note)

case class Operation(name:String,label:String,note:String,field:Field)
  extends Item//(operationName,label,note)

case class Field(fieldName:String,fieldType:FieldType,variable:Variable,computation:Option[String]) 

case class Note(name:String,label:String,note:String="") 
  extends Item//(noteName,label,"")

case class Variable(varName:String,varType:XSDDatatype)
