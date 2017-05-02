package ch.hevs.medred

import rdftools.rdf.RdfTerm
import rdftools.rdf.Iri
import rdftools.rdf.api.JenaTools._
import rdftools.rdf.Literal._
import rdftools.rdf.vocab.RDF
import rdftools.rdf.vocab.DCterms

import org.apache.jena.rdf.model.Model
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.rdf.model.RDFNode
import org.apache.jena.riot.RDFFormat
import org.apache.jena.riot.RDFDataMgr
import org.apache.jena.vocabulary.XSD

import ch.hevs.medred.vocab.MedRed
import ch.hevs.medred.vocab.PPlan
import rdftools.rdf.vocab.PROV
import ch.hevs.medred.vocab.Shacl

class Rdfizer(val prefix: Iri) {


  import Rdfizer._
  import rdftools.rdf.RdfSchema._

  def newIri(str: String) = prefix + str.replaceAll("\\s|/|<|>|\\(|\\)", "")

  def toRdf(item: Item)(implicit m: Model): Unit = item match {
    case sec: Section =>
      val secIri = newIri(sec.name)
      +=(secIri, RDF.a, MedRed.Section)
      +=(secIri, DCterms.identifier, sec.name)
      +=(secIri, DCterms.title, sec.label)
      val secItems: Array[RDFNode] = sec.items.map { i =>
        val iti = newIri(i.name)
        toRdf(i)
        +=(iti, MedRed.ofSection, secIri)
        iti: RDFNode
      }.toArray
      +=(secIri, MedRed.items, m.createList(secItems))
    case question: Question =>
      val qIri = createRdfItem(question)
      +=(qIri, RDF.a, MedRed.Question)
      val choices: Array[RDFNode] =
        question.choices.map { x => toJenaRes(toRdf(x)) }.toArray
      if (choices.length > 0)
        +=(qIri, MedRed.choices, m.createList(choices))
      createRdfVar(question.field, qIri)
    case operation: Operation =>
      val oIri = createRdfItem(operation)
      +=(oIri, RDF.a, MedRed.Operation)
      +=(oIri, MedRed.calculation, operation.field.computation.get)

      createRdfVar(operation.field, oIri)
    case note: Note =>
      val infoIri = newIri(note.name)
      +=(infoIri, RDF.a, MedRed.Information)
      +=(infoIri, DCterms.description, note.label)

  }

  def createRdfVar(field: Field, itemIri: Iri)(implicit m: Model) = {
    val varIri = itemIri + "_var"
    +=(itemIri, PPlan.hasOutputVar, varIri)
    field.validation.foreach { valid =>  
      val shapeIri=itemIri+"_shape"
      +=(itemIri,MedRed.validationShape,shapeIri)
      +=(shapeIri,RDF.a,Shacl.PropertyShape)
      +=(shapeIri,Shacl.path,MedRed.dataValue)
      
      valid.propShapes.foreach{ 
        case min:MinInclShape=> 
          +=(shapeIri,Shacl.minInclusive,lit(min.minVal))
        case min:MinExclShape=> 
          +=(shapeIri,Shacl.minExclusive,lit(min.minVal))
        case max:MaxInclShape=> 
          +=(shapeIri,Shacl.maxInclusive,lit(max.maxVal))
        case max:MaxExclShape=> 
          +=(shapeIri,Shacl.maxExclusive,lit(max.maxVal))      }
           
    }
    val shapeIri=itemIri+"_shape"

    +=(varIri, RDF.a, PPlan.Variable)
    +=(varIri, MedRed.varName, field.variable.varName)
    +=(varIri, MedRed.dataType, Iri(field.variable.varType.getURI))
  }

  def createRdfItem(item: Item)(implicit m: Model): Iri = {
    val itemIri = newIri(item.name)
    +=(itemIri, DCterms.identifier, item.name)
    +=(itemIri, DCterms.title, item.label)
    if (!item.note.isEmpty)
      +=(itemIri, DCterms.description, item.note)
    itemIri
  }

  def toRdf(choice: Choice)(implicit m: Model) = {
    val choiceIri = newIri(choice.label + choice.value)
    +=(choiceIri, RDF.a, MedRed.Choice)
    +=(choiceIri, DCterms.title, choice.label)
    +=(choiceIri, MedRed.hasValue, lit(choice.value))
    choiceIri
  }

  def toRdf(instr: Instrument)(implicit m:Model):Iri = {
    val instIri = newIri(instr.name)
    +=(instIri, RDF.a, MedRed.Instrument)
    +=(instIri, DCterms.identifier, instr.name)
    val items: Array[RDFNode] = instr.items.map { item =>
      val iri = newIri(item.name)
      toRdf(item)
      +=(iri, MedRed.ofInstrument, instIri)
      iri: RDFNode
    }.toArray
    var current = Iri("")
    items.foreach { r =>
      val itemi = Iri(r.asResource.getURI)
      if (!current.value.isEmpty)
        +=(itemi, PPlan.isPrecededBy, current)
      current = itemi
    }
    +=(instIri, MedRed.items, m.createList(items))
    instIri
  }
  
  def toRdf(study:Study)(implicit m:Model):Iri={
    val studyIri=newIri(study.name)
    +=(studyIri,RDF.a,MedRed.Study)
    +=(studyIri,DCterms.identifier,study.id)
    +=(studyIri,DCterms.title,study.name)
    +=(studyIri,DCterms.description,study.description)
    val instrs=study.instruments.map{instr=>
      val instrIri=toRdf(instr)
          +=(studyIri,MedRed.instrument,instrIri)

    }
    studyIri
  }

}

object Rdfizer {

  def main(args: Array[String]): Unit = {
    import collection.JavaConversions._
    val mm=RDFDataMgr.loadModel("http://w3id.org/medred/medred#")
    mm.listStatements().foreach { x => println(x)} 
    
    
    val rdfizer = new Rdfizer(iri("http://example.org/"))
    //val instr=rdfizer.loadRedCap("src/main/resources/instruments/childs_sleep_habits_questionnaire_cshq.csv")
    val study = CsvImport.loadStudy("src/main/resources/studies/ProjectDemoDatabase.csv")
    println(study.name)
        implicit val m = ModelFactory.createDefaultModel
    m.setNsPrefix("ex", rdfizer.prefix.value)
    m.setNsPrefix("rdf", RDF.iri.value)
    m.setNsPrefix("medred", MedRed.iri.value)
    m.setNsPrefix("xsd", XSD.getURI)
    m.setNsPrefix("dcterms", DCterms.iri.value)
    m.setNsPrefix("pplan", PPlan.iri.value)
    m.setNsPrefix("sh", Shacl.iri.value)
    rdfizer.toRdf(study)
    //val m = rdfizer.toRdf(instr)
    RDFDataMgr.write(System.out, m, RDFFormat.TURTLE)
    /*
    instr.items.foreach {d=>
      d match {
        case s:Section=>println(s.sectionName)
          println(s.items.map { i => i.name }.mkString(","))
        
      }
    }*/
  }
}