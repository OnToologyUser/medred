package ch.hevs.medred

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import org.apache.jena.rdf.model.Model
import org.apache.jena.rdf.model.ModelFactory

import org.apache.jena.rdf.model.RDFNode
import org.apache.jena.rdf.model.ResourceFactory
import org.apache.jena.rdf.model.Literal
import ch.hevs.medred.vocab.MedRed
import ch.hevs.rdftools.rdf.vocab.Rdfs
import ch.hevs.rdftools.rdf.vocab.Rdf
import ch.hevs.rdftools.rdf.vocab.Dcterms
import ch.hevs.rdftools.rdf.RdfTerm
import ch.hevs.rdftools.rdf.Iri
import org.apache.jena.riot.RDFFormat
import org.apache.jena.riot.RDFDataMgr
import org.apache.jena.rdf.model.RDFList

  import ch.hevs.rdftools.rdf.api.JenaTools._
  import ch.hevs.rdftools.rdf.Literal._
  import org.apache.jena.rdf.model.Container
  import org.apache.jena.vocabulary.XSD
  import ch.hevs.medred.vocab.PPlan
  
class Rdfizer(prefix:Iri) {
  
  //"Variable / Field Name","Form Name","Section Header","Field Type",
  //"Field Label","Choices, Calculations, OR Slider Labels","Field Note",
  //"Text Validation Type OR Show Slider Number","Text Validation Min","Text Validation Max",
  //Identifier?,"Branching Logic (Show field only if...)","Required Field?","Custom Alignment",
  //"Question Number (surveys only)","Matrix Group Name","Matrix Ranking?","Field Annotation"  
 
  import Rdfizer._
  import Iri._
  
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
      
      val fieldLabel=stripQuotes(data(4))
      val fieldNote=stripQuotes(data(6))

      val field=
        if (data(3)=="descriptive")
          Note(fieldName,fieldLabel)
        else {
          val fieldType=FieldType.parse(data(3))
          val computation=
            if (fieldType!=CalculationType) None
            else Some(stripQuotes(data(5)))
          val f=Field(fieldName,fieldType,Variable(fieldName,fieldType.toXsd),computation)
          val choices:Array[Choice]=
            if (fieldType!=OptionType && fieldType!=OneOptionType) Array()
            else stripQuotes(data(5)).split("\\s\\|\\s").map(parseChoice)
          if (fieldType==CalculationType)
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
        section=Some(Section(sectionId,sectionName,"",ArrayBuffer(field),!matrixName.isEmpty))
        items+=section.get
      }
      else if (!section.isDefined || (section.get.matrix && matrixName.isEmpty))
        items+=field
      else
        section.get.items.asInstanceOf[ArrayBuffer[Item]]+=field
      nr+=1 
    }
    Instrument(instrumentName,items)    
  }
  


  def +=(s:Iri,p:Iri,o:Iri)(implicit m:Model)={
    m.add(s,p,o)
  }
  def +=(s:Iri,p:Iri,o:String)(implicit m:Model)={
    m.add(toJenaRes(s),p,o)
  }
  def +=(s:Iri,p:Iri,o:Literal)(implicit m:Model)={
    m.add(s,p,o)
  }
  def +=(s:Iri,p:Iri,list:RDFList)(implicit m:Model)={
    m.add(s,p,list)
  } 
  def +=(s:Iri,p:Iri,cont:Container)(implicit m:Model)={
    m.add(s,p,cont)
  } 
  
  def newIri(str:String)=prefix+str.replaceAll("\\s|/|<|>|\\(|\\)","")
  
  def toRdf(item:Item)(implicit m:Model):Unit= item match{
    case sec:Section=>
        val secIri=newIri(sec.name)
        +=(secIri,Rdf.a,MedRed.Section)
        +=(secIri,Dcterms.identifier,sec.name)
        +=(secIri,Dcterms.title,sec.label)
        val secItems:Array[RDFNode]=sec.items.map {i=>
          val iti=newIri(i.name)
          toRdf(i)
          +=(iti,MedRed.ofSection,secIri)
          iti:RDFNode
        }.toArray
        +=(secIri,MedRed.items,m.createList(secItems))
      case question:Question=>
        val qIri=createRdfItem(question)
        +=(qIri,Rdf.a,MedRed.Question)
        val choices:Array[RDFNode]=
          question.choices.map { x => toJenaRes(toRdf(x)) }.toArray
        if (choices.length>0)
          +=(qIri,MedRed.choices,m.createList(choices))
        createRdfVar(question.field,qIri)
      case operation:Operation=>
        val oIri=createRdfItem(operation)
        +=(oIri,Rdf.a,MedRed.Operation)
        +=(oIri,MedRed.calculation,operation.field.computation.get)
       
        createRdfVar(operation.field,oIri)
      case note:Note=>
        val infoIri=newIri(note.name)
        +=(infoIri,Rdf.a,MedRed.Information)
        +=(infoIri,Dcterms.description,note.label)
   
  }
  
  def createRdfVar(field:Field,itemIri:Iri)(implicit m:Model)={
    val varIri=itemIri+"_var"
    +=(itemIri,PPlan.hasOutputVar,varIri)
    +=(varIri,Rdf.a,PPlan.Variable)
    +=(varIri,MedRed.varName,field.variable.varName)
    +=(varIri,MedRed.dataType,Iri(field.fieldType.toXsd.getURI))
  }
  
  def createRdfItem(item:Item)(implicit m:Model):Iri={
    val itemIri=newIri(item.name)
    +=(itemIri,Dcterms.identifier,item.name)
    +=(itemIri,Dcterms.title,item.label)
    if (!item.note.isEmpty)
      +=(itemIri,Dcterms.description,item.note)
    itemIri  
  }
  
  
  def toRdf(choice:Choice)(implicit m:Model)={
    val choiceIri=newIri(choice.label+choice.value)
    +=(choiceIri,Rdf.a,MedRed.Choice)
    +=(choiceIri,Dcterms.title,choice.label)
    +=(choiceIri,MedRed.hasValue,lit(choice.value))
     choiceIri
  }
  
  def toRdf(instr:Instrument):Model={
    implicit val m= ModelFactory.createDefaultModel
    m.setNsPrefix("ex", this.prefix.value)
    m.setNsPrefix("rdf", Rdf.iri.value)
    m.setNsPrefix("medred", MedRed.iri.value)
    m.setNsPrefix("xsd",XSD.getURI)
    m.setNsPrefix("dcterms", Dcterms.iri.value)
    m.setNsPrefix("pplan", PPlan.iri.value)
    val instIri=newIri(instr.name)
    +=(instIri, Rdf.a,MedRed.Instrument)
    +=(instIri,Dcterms.identifier,instr.name)
    val items:Array[RDFNode]=instr.items.map{item=>
      val iri=newIri(item.name)
      toRdf(item)
      +=(iri,MedRed.ofInstrument,instIri)
      iri:RDFNode
    }.toArray
    var current=Iri("")
    items.foreach { r => 
      val itemi=Iri(r.asResource.getURI) 
      if (!current.value.isEmpty)
        +=(itemi,PPlan.isPrecededBy,current)
      current=itemi  
    }
    +=(instIri,MedRed.items,m.createList(items))
    m
  }
  
}

object Rdfizer{  
  def stripQuotes(str:String)=str.replaceAll("^\"|\"$", "")
  def parseChoice(str:String)={
    val split=str.split(", ")
    println(str)
    Choice(split(0),split(1))
  }
 
  
  def main (args:Array[String]):Unit={
    val rdfizer=new Rdfizer(iri("http://example.org/"))
    //val instr=rdfizer.loadRedCap("src/main/resources/instruments/childs_sleep_habits_questionnaire_cshq.csv")
    val instr=rdfizer.loadRedCap("src/main/resources/instruments/expanded_prostate_cancer_index_composite_epic.csv")
    val m=rdfizer.toRdf(instr)
    RDFDataMgr.write(System.out,m,RDFFormat.TURTLE)
    /*
    instr.items.foreach {d=>
      d match {
        case s:Section=>println(s.sectionName)
          println(s.items.map { i => i.name }.mkString(","))
        
      }
    }*/
  }
}