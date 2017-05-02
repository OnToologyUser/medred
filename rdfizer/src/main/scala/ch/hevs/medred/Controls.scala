package ch.hevs.medred

case class Control(id:String)

trait OptionControl extends Control
trait TextControl extends Control

object TextFieldControl extends Control("text") with TextControl
object TextAreaControl extends Control("textarea") with TextControl
object RadioControl extends Control("radio") with OptionControl
object DropdownControl extends Control("dropdown") with OptionControl
object CheckboxControl extends Control("checkbox") with OptionControl
object CalculationControl extends Control("calc")
object UploadControl extends Control("upload")
object BooleanControl extends Control("boolean")
object SliderControl extends Control("slider")

object Control {
  def parse(str:String)=str match {
    case "text"=>TextFieldControl
    case "dropdown"=>DropdownControl
    case "radio"=>RadioControl
    case "checkbox"=>CheckboxControl
    case "file"=>UploadControl
    case "notes"=>TextAreaControl
    case "calc"=>CalculationControl
    case "yesno"=>BooleanControl
    case "slider"=>SliderControl
  //  case _ =>NoType
  }
}
