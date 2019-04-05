package ch.hevs.medred

import java.io.File

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

import org.apache.jena.datatypes.xsd.XSDDatatype
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.slf4j.LoggerFactory

import com.fasterxml.jackson.dataformat.csv.CsvMapper
import com.fasterxml.jackson.dataformat.csv.CsvParser

/** methods to import MedRed objects from Csv files
 *  
 */
object CsvImport {
  
  /** logger */
  val log=LoggerFactory.getLogger(CsvImport.getClass)
  
  def stripQuotes(str:String)=str.replaceAll("^\"|\"$", "")

  /** Parse a string pair into a Choice */
  def parseChoice(str:String)={
    val split=str.split(", ")
    Choice(split(0),split(1))
  }
  
  /** parse a string list of choices */
  def parseChoices(control:Control,choicesStr:String):Array[Choice]=
    control match {
      case o:OptionControl=>stripQuotes(choicesStr).split("\\s\\|\\s").map(parseChoice)
      case _ => Array()
    }
  
  def buildValidation(min:String,max:String)={
    val validMin=Try(min.toDouble).toOption
    val validMax=Try(max.toDouble).toOption
    if (validMin.isEmpty && validMax.isEmpty) 
      None
    else {
      val shapes=Array(validMin.map(MinInclShape(_)),
                       validMax.map(MaxInclShape(_)))
                   .filter(_.isDefined).map(_.get)
      Some(ValidationShape(shapes))
    }
  }
  
  def buildItem(itemType:String,itemName:String,itemLabel:String,itemNote:String,
      computeOrChoices:String,validationType:String,maxMin:Option[ValidationShape]):Item={
    
    if (itemType=="descriptive")
      Note(itemName,itemLabel)
    else {
      val control=Control.parse(itemType)
      val computation=
        if (control!=CalculationControl) None
        else Some(stripQuotes(computeOrChoices))
      val choices=parseChoices(control, computeOrChoices)
      val xsddatatype=getXsd(control,validationType,choices.map(_.value.toString))
      val f=Field(itemName,control,Variable(itemName,xsddatatype),maxMin,computation)
          
      if (control==CalculationControl)
        Operation(itemName,itemLabel,itemNote,f)
      else
        Question(itemName,itemLabel,itemNote,f,choices)
    }
  }
  
  def getXsd(fieldType:Control,
      validation:String,options:Seq[String])=fieldType match {
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
    case _ => throw new IllegalArgumentException("Invalid fieldType.")
  }
  
  val dateStringFormat = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")

  /*
  def splitLine(line:String)={
    val splitr=""",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)"""
    println(line)
    line.split(splitr,-1)
    
  }*/

  //StudyId,StudyName,Description,ProtocolName,Notes,
  //Version,Created,Modified,Creator,Source,SourceVersion
  def loadStudies(file:String)={
    val studies=new ArrayBuffer[Study]
    readCsv(file).records.foreach { data => 
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
  def getStudy(id:String)= {
    studies.filter(s => s.id==id).headOption.getOrElse{
      log.info("Study metadata is not on studies.csv")
      defaultStudy(id)
    }
  }
  

  def defaultStudy(studyId:String)=
    Study(studyId,studyId,"","",studyId,null,DateTime.now,DateTime.now,Seq())

  /** Load study and instruments from a csv file 
   *  @param file String of the location of the csv file
   *  @return Study object loaded form the csv file
   *  */
  def loadStudy(file:String)={
    val study=getStudy(file.split("/").last.replace(".csv", ""))
    val instruments=new ArrayBuffer[Instrument]
    var currentInstrument=""
    var section:Option[Section]=None
    var secNr=0
    val items=ArrayBuffer[Item]()
    val csv=readCsv(file)
    csv.records.foreach{ data=>
      val fieldName=data(0)
      val instrumentName=data(1)
      if (!currentInstrument.isEmpty && instrumentName!=currentInstrument){
        instruments+=Instrument(currentInstrument,items.toArray.toSeq)
        items.clear
        secNr=0
        section=None
      }
      currentInstrument=instrumentName
      val itemLabel=stripQuotes(data(4))
      val itemNote=stripQuotes(data(6))
      val validationType=data(7)
      val validMin=data(8)
      val validMax=data(9)
      val validation=buildValidation(validMin,validMax)
      
      val itemType=data(3)
      val item=buildItem(itemType,fieldName,itemLabel,itemNote,
          data(5),validationType,validation)
        
      val matrixName=data(15)
      val sectionName=stripQuotes(data(2))
      if (!sectionName.isEmpty){
        val sectionId= 
          if (matrixName.isEmpty){
            secNr+=1
            "section"+secNr
          }
          else matrixName
        section=Some(Section(currentInstrument+"/"+sectionId,sectionName,"",ArrayBuffer(item),!matrixName.isEmpty))
        items+=section.get
      }
      else if (!section.isDefined || (section.get.matrix && matrixName.isEmpty))
        items+=item
      else
        section.get.items.asInstanceOf[ArrayBuffer[Item]]+=item      
   
    }
    instruments+=Instrument(currentInstrument,items)    
    study.copy(instruments)
  }

  def loadRecords(file:String)={
    val csv=readCsv(file)
    val records=csv.records.map {r=>
      val recordId=r.head
      Record(recordId,csv.labels.tail,r.tail)
    }
    records
  }
  
  case class Csv(labels:Array[String],records:Iterator[Array[String]])
  
  def readCsv(file:String)={
    val csvMapper= new CsvMapper
    csvMapper.enable(CsvParser.Feature.WRAP_AS_ARRAY)
    val reader=csvMapper.readerFor(classOf[Array[String]])
    val it= reader.readValues[Array[String]](new File(file))
    val labels=it.next()

    val records=it.asScala
    Csv(labels,records)
  }
  
  
}