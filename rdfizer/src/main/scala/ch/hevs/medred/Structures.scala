package ch.hevs.medred

import org.apache.jena.datatypes.xsd.XSDDatatype
import org.joda.time.DateTime

/** Represents a study, including its metadata, and its instruments
 *  @param id study identifier
 *  @param name study name
 *  @param notes additional notes about the study
 *  @param description study description
 *  @param protocolName name of the protocol that governs the study
 *  @param version study version number/code
 *  @param created date of creation
 *  @param modified date of last modification
 *  @param instruments list of instruments
 */
case class Study(id:String,name:String,
    notes:String,description:String,
    protocolName:String,version:String,
    created:DateTime,modified:DateTime,
    instruments:Seq[Instrument]) {
  
  /** creates a copy of the study, overwriting the list of instruments
   *  @param newInsturments list of instruments for the study copy
   */
  def copy(newInstruments:Seq[Instrument])=
    Study(id,name,notes,description,protocolName,
      version,created,modified,newInstruments)
}

/** Represents an data acquisition instrument (e.g., questionnaire)
 *  @param name instrument name
 *  @param items list of instrument items (e.g., sections, questions)
 */
case class Instrument(name:String,items:Seq[Item])

/** Represents an item on a data acquisition instrument (e.g., section, questions, etc.)
 *  
 */
trait Item{
  /** name of the item */
  def name:String
  
  /** label to be displayed for the item */
  def label:String 
  
  /** additional information or description of the item */
  def note:String 
}

/** Represents choices for a data acquisition input Item
 *  @param value choice value
 *  @param label label of the choice value, usually for display purposes
 */
case class Choice(value:Any,label:String)

/** Represents a section of a data acquisition instrument
 *  @param name section name
 *  @param label section label
 *  @param note section description
 *  @param items list of items inside the section
 *  @param matrix true, if the section is a data input matrix. @default false
 */
case class Section(name:String,label:String,
    note:String,items:Seq[Item],matrix:Boolean=false) 
  extends Item
  
/** Represents a question item
 *  @param name question name
 *  @param label question label or text representation
 *  @param note question additional description
 *  @param field field associated to the question, specifying variables, types, etc.
 *  @param choices list of possible choices to answer this question  
 */
case class Question(name:String,label:String,
    note:String,field:Field,choices:Seq[Choice]) 
  extends Item

/** Represents an operation item on an instrument, typically for computing
 *  fields
 *  @param name operation name
 *  @param label operation label
 *  @param note operation description
 *  @param field Field associated to the operation
 */
case class Operation(name:String,label:String,note:String,field:Field)
  extends Item

/** Represents a field for an instrument, including a variable 
 *  and linked to a specific input control
 *  
 *  @constructor create an operation item
 *  @param fieldName name of the field
 *  @param control input control associated with the filed
 *  @param variable variable associated to the field
 *  @param validation shape used to validate the field
 *  @param computation computation expression used to fill the content of the variable of this field
 */
case class Field(fieldName:String,control:Control,
    variable:Variable,
    validation:Option[ValidationShape],
    computation:Option[String]) 

/** Represents a note item, usually used to provide information on the instrument
  * @param name note name
  * @param label text of the note
  * @param note description of the note
  */
case class Note(name:String,label:String,note:String="") 
  extends Item
  
/** Represents a variable
 *  @param varName variable name
 *  @param varType variable data type  
 */
case class Variable(varName:String,varType:XSDDatatype)

/** Shape for validating a property as a SHACL rule
 *  
 */
trait PropertyShape

/** Validation shape comprising a set of `PropertyShape` items
 *  @param propShapes collection of property shapes
 */
case class ValidationShape(propShapes:Seq[PropertyShape])

case class MaxInclShape(maxVal:Double) extends PropertyShape
case class MaxExclShape(maxVal:Double) extends PropertyShape
case class MinInclShape(minVal:Double) extends PropertyShape
case class MinExclShape(minVal:Double) extends PropertyShape

/** Represents a record on a data acquisition activity
 *  @param recordId id of the record
 *  @param varNames collection of variable names for the record
 *  @param fields collection of field values corresponding to 
 *  			 each variable name in `varNames`
 */
case class Record(recordId:String,varNames:Seq[String],fields:Seq[Any])
