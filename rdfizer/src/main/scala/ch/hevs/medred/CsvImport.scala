package ch.hevs.medred

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import org.apache.jena.datatypes.xsd.XSDDatatype
import scala.util.Try
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.DateTimeFormat

object CsvImport {
  
  
  
  def stripQuotes(str:String)=str.replaceAll("^\"|\"$", "")

  def parseChoice(str:String)={
    val split=str.split(", ")
    //println(str)
    Choice(split(0),split(1))
  }
 
  def getXsd(fieldType:Control,validation:String,options:Seq[String])={
    fieldType match {
    case t:TextControl => validation match {
      case "integer" => XSDDatatype.XSDinteger
      case "number" => XSDDatatype.XSDdouble
      case "date_ymd"=> XSDDatatype.XSDdate
      case "date_mdy"=> XSDDatatype.XSDdate
      case "date_dmy"=> XSDDatatype.XSDdate
      case "datetime_ymd"=> XSDDatatype.XSDdateTime
      case "email" => XSDDatatype.XSDstring
      case "ssn" => XSDDatatype.XSDstring
      case "phone" => XSDDatatype.XSDstring
      case "time" => XSDDatatype.XSDtime
      case "" => XSDDatatype.XSDstring
    }
    case UploadControl=>XSDDatatype.XSDbase64Binary
    case o:OptionControl=>
      options.map{op=>
        Try(op.toInt).getOrElse(Try(op.toDouble).getOrElse(op))
      }.head match {
        case i:Int=>XSDDatatype.XSDinteger
        case d:Double=>XSDDatatype.XSDdouble
        case s:String=>XSDDatatype.XSDstring
      }
    case BooleanControl=>XSDDatatype.XSDboolean  
    case CalculationControl=>XSDDatatype.XSDdouble
    case SliderControl=>XSDDatatype.XSDdouble
    }
  }
  
  val dateStringFormat = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")

  def splitLine(line:String)={
    val splitr=""",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)"""
    println(line)
    line.split(splitr,-1)
    
  }

  //StudyId,StudyName,Description,ProtocolName,Notes,
  //Version,Created,Modified,Creator,Source,SourceVersion
  def loadStudies(file:String)={
    val studies=new ArrayBuffer[Study]
    Source.fromFile(file).getLines.drop(1).foreach { line => 
      val data=splitLine(line) 
      val created = dateStringFormat.parseDateTime(data(6))
      val modified = dateStringFormat.parseDateTime(data(7))
      val study=Study(stripQuotes(data(0)),stripQuotes(data(1)),stripQuotes(data(4)),stripQuotes(data(2)),
          data(3),data(5),created, modified,Seq())
      studies+=study
    }
    studies
  }
  
  //"Variable / Field Name","Form Name","Section Header","Field Type",
  //"Field Label","Choices, Calculations, OR Slider Labels","Field Note",
  //"Text Validation Type OR Show Slider Number","Text Validation Min","Text Validation Max",
  //Identifier?,"Branching Logic (Show field only if...)","Required Field?","Custom Alignment",
  //"Question Number (surveys only)","Matrix Group Name","Matrix Ranking?","Field Annotation"  
  
  val studies=loadStudies("src/main/resources/studies/studies.csv")
  def getStudy(id:String)=studies.filter(s => s.id==id).head
  
  def loadStudy(file:String)={
    val study=getStudy(file.split("/").last.replace(".csv", ""))
    val instruments=new ArrayBuffer[Instrument]
    var currentInstrument=""
    var section:Option[Section]=None
    var secNr=0
    val items=ArrayBuffer[Item]()
    Source.fromFile(file).getLines.drop(1).foreach { line =>
      val data=  splitLine(line)
      val fieldName=data(0)
      val instrumentName=data(1)
      if (!currentInstrument.isEmpty && instrumentName!=currentInstrument){
        instruments+=Instrument(currentInstrument,items.toArray.toSeq)
        items.clear
        secNr=0
        section=None
      }
      currentInstrument=instrumentName
      val fieldLabel=stripQuotes(data(4))
      val fieldNote=stripQuotes(data(6))
      val fieldValidation=data(7)
      val validMin=Try(data(8).toDouble).toOption
      val validMax=Try(data(9).toDouble).toOption
      
      val validation=
        if (validMin.isEmpty && validMax.isEmpty) None
        else {
          val shapes=Array(validMin.map(MinInclShape(_)),
                           validMax.map(MaxInclShape(_)))
                     .filter(_.isDefined).map(_.get)
          Some(ValidationShape(shapes))
        }
      
      val field=
        if (data(3)=="descriptive")
          Note(fieldName,fieldLabel)
        else {
          val fieldType=Control.parse(data(3))
          val computation=
            if (fieldType!=CalculationControl) None
            else Some(stripQuotes(data(5)))
          val choices:Array[Choice]=fieldType match {
              case o:OptionControl=>stripQuotes(data(5)).split("\\s\\|\\s").map(parseChoice)
              case _ => Array()
            }
          val xsddatatype=getXsd(fieldType,fieldValidation,choices.map(_.value.toString))
          val f=Field(fieldName,fieldType,Variable(fieldName,xsddatatype),validation,computation)
          
          if (fieldType==CalculationControl)
            Operation(fieldName,fieldLabel,fieldNote,f)
          else
            Question(fieldName,fieldLabel,fieldNote,f,choices)
        }        

      val matrixName=data(15)
      val sectionName=stripQuotes(data(2))
      if (!sectionName.isEmpty){
        val sectionId= 
          if (matrixName.isEmpty){
            secNr+=1
            "section"+secNr
          }
          else matrixName
        section=Some(Section(currentInstrument+"/"+sectionId,sectionName,"",ArrayBuffer(field),!matrixName.isEmpty))
        items+=section.get
      }
      else if (!section.isDefined || (section.get.matrix && matrixName.isEmpty))
        items+=field
      else
        section.get.items.asInstanceOf[ArrayBuffer[Item]]+=field      
   
    }
    instruments+=Instrument(currentInstrument,items)    
    study.copy(instruments)
  }

  def loadRecords(file:String)={

    //val records=Itera new [Record]
    val labels= splitLine(Source.fromFile(file).getLines().next)
    

    val records = Source.fromFile(file).getLines.drop(1).map { line => 
      val data=splitLine(line) 
      //val created = dateStringFormat.parseDateTime(data(6))
      //val modified = dateStringFormat.parseDateTime(data(7))
      val recordId = data(0)
      
      Record(recordId,labels.tail,data.tail)
    }
    records
  }
  
}