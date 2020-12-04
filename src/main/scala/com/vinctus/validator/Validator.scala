package com.vinctus.validator

import scala.collection.mutable.ListBuffer
import scalajs.js
import js.JSConverters._
import scala.scalajs.js.UndefOr
import scala.util.matching.Regex

import java.time.ZonedDateTime

object validObject extends scala.Dynamic {
  def applyDynamicNamed(name: String)(fields: (String, Validator[_, _])*): ObjectValidator = {
    require(name == "apply", "write validObject(field1 = <validation>, ...)")
    new ObjectValidator(fields.toList)
  }

}

sealed abstract class Result[R] {
  def json: String

  def valid: Boolean

  def value: js.UndefOr[R]
}

case class Invalid[R](reason: String) extends Result[R] {
  def json: String = s"""Invalid("$reason")"""

  val valid = false

  def value: js.UndefOr[R] = sys.error("invalid")
}

case class Valid[R](value: js.UndefOr[R]) extends Result[R] {
  def json = s"Valid(${js.JSON.stringify(value.asInstanceOf[js.Any], null.asInstanceOf[js.Array[js.Any]], 2)})"

  val valid = true
}

class ObjectValidator(fields: List[(String, Validator[_, _])]) extends Validator[js.Object, js.Object]("object") {

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

class IntValidator extends RangeValidator[Int, Int]("integer", _ == _, _ <= _) {

  def validateDefined(v: Any): Result[Int] = {
    v match {
      case x: Int => validateRange(x)
      case _      => invalid
    }
  }

}

class NumberValidator extends RangeValidator[Double, Double]("double", _ == _, _ <= _) {

  def validateDefined(v: Any): Result[Double] = {
    v match {
      case x: Double => validateRange(x)
      case _         => invalid
    }
  }

}

class DateStringValidator extends RangeValidator[String, ZonedDateTime]("string", _ == _, _ <= _) {

  def validateDefined(v: Any): Result[ZonedDateTime] =
    try {
      v match {
        case s: String => validateRange(s)
        case _         => invalid
      }
    } catch {
      case _: java.time.format.DateTimeParseException => Invalid("invalid ISO timestamp")
    }

  override protected def convert(a: String): ZonedDateTime = ZonedDateTime.parse(a)

}

class StringValidator extends RangeValidator[String, String]("string", _ == _, _ <= _) {

  private var _regex: Option[Regex] = None
  private var _pattern: String = _
  private var _alphanum = false

  def validateDefined(v: Any): Result[String] = {
    v match {
      case s: String =>
        validateRange(s) match {
          case r: Invalid[String] => r
          case r: Valid[String] =>
            if (_regex.isEmpty || _regex.get.matches(s))
              if (!_alphanum || s.forall(_.isLetterOrDigit)) r
              else Invalid("not alphanumeric")
            else Invalid(s"doesn't match pattern '${_pattern}'")
        }
      case _ => invalid
    }
  }

  override def alphanum: Validator[String, String] = {
    _alphanum = true
    this
  }

  override def regex(pattern: String): Validator[String, String] = {
    _pattern = pattern
    _regex = Some(pattern.r)
    this
  }

}

class BooleanValidator extends PrimitiveValidator[Boolean, Boolean]("boolean", _ == _) {

  def validateDefined(v: Any): Result[Boolean] = {
    v match {
      case x: Boolean => validatePrimitive(x)
      case _          => invalid
    }
  }

}

abstract class Validator[T, R](typeName: String) {

  protected var _required = false
  protected var _default: js.UndefOr[R] = js.undefined

  def validate(v: Any): Result[R] =
    if (v == js.undefined)
      if (_required) Invalid(s"$typeName required")
      else Valid(_default)
    else
      validateDefined(v)

  protected def invalid: Invalid[R] =
    Invalid(s"not a${if ("aeiou" contains typeName.head) "n" else ""} $typeName")

  protected def validateDefined(v: Any): Result[R]

  protected def convert(a: T): R = a.asInstanceOf[R]

  def required: Validator[T, R] = {
    _required = true
    this
  }

  def valid(vs: T*): Validator[T, R] = sys.error("valid() is not defined for this type")

  def default(v: R): Validator[T, R] = {
    _default = v
    this
  }

  def min(v: T): Validator[T, R] = sys.error("min() is not defined for this type")

  def max(v: T): Validator[T, R] = sys.error("max() is not defined for this type")

  def regex(pattern: String): Validator[T, R] = sys.error("regex() is not defined for this type")

  def alphanum: Validator[T, R] = sys.error("alphanum() is not defined for this type")

}

abstract class PrimitiveValidator[T, R](typeName: String, eq: (T, T) => Boolean) extends Validator[T, R](typeName) {

  protected val _valid = new ListBuffer[T]

  override def valid(vs: T*): Validator[T, R] = {
    _valid ++= vs
    this
  }

  protected def validatePrimitive(v: T): Result[R] =
    if (_valid.isEmpty || _valid.exists(a => eq(v, a))) Valid(convert(v))
    else Invalid(s"not one of the valid values of ${_valid map (a => s"'$a'") mkString ", "}")

}

abstract class RangeValidator[T, R](typeName: String, eq: (T, T) => Boolean, lte: (T, T) => Boolean)
    extends PrimitiveValidator[T, R](typeName, eq) {

  protected var _min: js.UndefOr[T] = js.undefined
  protected var _max: js.UndefOr[T] = js.undefined

  override def min(v: T): RangeValidator[T, R] = {
    _min = v
    this
  }

  override def max(v: T): RangeValidator[T, R] = {
    _max = v
    this
  }

  protected def validateRange(v: T): Result[R] =
    if (_min.isEmpty || lte(_min.get, v))
      if (_max.isEmpty || lte(v, _max.get))
        validatePrimitive(v)
      else
        Invalid(s"above max value of '${_max}'")
    else
      Invalid(s"below min value of '${_min}'")

}
