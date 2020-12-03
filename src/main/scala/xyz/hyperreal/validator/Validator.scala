package xyz.hyperreal.validator

import scala.collection.mutable.ListBuffer
import scalajs.js
import js.JSConverters._
import scala.scalajs.js.UndefOr

object validObject extends scala.Dynamic {

  def applyDynamicNamed(name: String)(fields: (String, Validator[_])*): ObjectValidator = {
    require(name == "apply", "write validObject(field1 = <validation>, ...)")

    new ObjectValidator(fields.toList)
  }

}

abstract class Result[T] { def json: String }
case class Invalid[T](reason: String) extends Result[T] { def json: String = s"""Invalid("$reason")""" }
case class Valid[T](value: js.UndefOr[T]) extends Result[T] {
  def json = s"Valid(${js.JSON.stringify(value.asInstanceOf[js.Any], null.asInstanceOf[js.Array[js.Any]], 2)})"
}

class ObjectValidator(fields: List[(String, Validator[_])]) extends Validator[js.Object]("object") {

  private val fieldSet = fields map (_._1) toSet

  private var _stripUnknown = false

  def validateDefined(v: Any): Result[js.Object] = {
    v match {
      case x: js.Object =>
        val d = js.Object.assign(new js.Object, x).asInstanceOf[js.Dictionary[Any]]

        for ((f, v) <- fields)
          v.validate(d.get(f).orUndefined) match {
            case Invalid(reason) => return Invalid(s"field '$f': $reason")
            case Valid(value) =>
              if (value ne v.asInstanceOf[UndefOr[Any]]) d(f) = value
          }

        if (_stripUnknown)
          d.keySet diff fieldSet foreach d.remove

        Valid(d.asInstanceOf[js.UndefOr[js.Object]])
      case _ => Invalid("not an object")
    }
  }

  def stripUnknown: ObjectValidator = {
    _stripUnknown = true
    this
  }

}

class IntValidator extends RangeValidator[Int]("integer") {

  def validateMin(v: Int, min: Int): Boolean = v >= min

  def validateMax(v: Int, max: Int): Boolean = v <= max

  def validateDefined(v: Any): Result[Int] = {
    v match {
      case x: Int => validateRange(x)
      case _      => invalid
    }
  }

}

class DoubleValidator extends RangeValidator[Double]("double") {

  def validateMin(v: Double, min: Double): Boolean = v >= min

  def validateMax(v: Double, max: Double): Boolean = v <= max

  def validateDefined(v: Any): Result[Double] = {
    v match {
      case x: Double => validateRange(x)
      case _         => invalid
    }
  }

}

class StringValidator extends RangeValidator[String]("string") {

  def validateMin(v: String, min: String): Boolean = v >= min

  def validateMax(v: String, max: String): Boolean = v <= max

  def validateDefined(v: Any): Result[String] = {
    v match {
      case x: String => validateRange(x)
      case _         => invalid
    }
  }

}

class BooleanValidator extends Validator[Boolean]("boolean") {

  def validateDefined(v: Any): Result[Boolean] = {
    v match {
      case x: Boolean => Valid(x)
      case _          => invalid
    }
  }

}

abstract class Validator[T](typeName: String) {

  protected var _required = false
  protected val _valid = new ListBuffer[T]
  protected var _default: js.UndefOr[T] = js.undefined

  def validate(v: Any): Result[T] =
    if (v == js.undefined)
      if (_required)
        Invalid(s"$typeName required")
      else
        Valid(_default)
    else
      validateDefined(v)

  protected def invalid: Invalid[T] =
    Invalid(s"not a${if ("aeiou" contains typeName.head) "n" else ""} $typeName")

  protected def validateDefined(v: Any): Result[T]

  def required: Validator[T] = {
    _required = true
    this
  }

  def valid(v: T): Validator[T] = {
    _valid += v
    this
  }

  def default(v: T): Validator[T] = {
    _default = v
    this
  }

  def min(v: T): Validator[T] = sys.error("min() is not defined for this type")

  def max(v: T): Validator[T] = sys.error("max() is not defined for this type")

}

abstract class RangeValidator[T](typeName: String) extends Validator[T](typeName) {

  protected var _min: js.UndefOr[T] = js.undefined
  protected var _max: js.UndefOr[T] = js.undefined

  override def min(v: T): RangeValidator[T] = {
    _min = v
    this
  }

  override def max(v: T): RangeValidator[T] = {
    _min = v
    this
  }

  protected def validateMin(v: T, min: T): Boolean

  protected def validateMax(v: T, max: T): Boolean

  protected def validateRange(v: T): Result[T] =
    if (_min.isEmpty || validateMin(v, _min.get))
      if (_max.isEmpty || validateMax(v, _max.get))
        Valid(v)
      else
        Invalid("above max value")
    else
      Invalid("below min value")

}
