package ch.hevs.medred

/** Represents an Instrument input control, 
  * e.g., text field, dropdown, etc.
  * @param id The control identifier
 */
case class Control(id:String)

/** Represents an input control with predefined options */
trait OptionControl extends Control

/** Represents a text input control */
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

/** Control companion. Utilities for input controls*/
object Control {
  
  /** parses a control identifier into a Control */
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
    case _ =>throw new IllegalArgumentException(s"No Control for identifier $str")
  }
}
