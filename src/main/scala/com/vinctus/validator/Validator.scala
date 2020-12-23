package com.vinctus.validator

import scala.collection.mutable.ListBuffer
import scalajs.js
import js.JSConverters._
import scala.scalajs.js.UndefOr
import scala.util.matching.Regex
import java.time.{Instant, ZonedDateTime}
import java.time.temporal.Temporal

object validObject extends scala.Dynamic {

  def applyDynamicNamed(name: String)(fields: (String, Validator[_, _])*): ObjectValidator[_ <: js.Object] = {
    require(name == "apply", "write validObject(field1 = <validation>, ...)")
    new ObjectValidator(fields.toList)
  }

}

sealed abstract class Result[R] {
  def json: String

  def valid: Boolean

  def value: js.UndefOr[R] = sys.error("invalid")

  private[validator] def validate(test: Boolean, error: String): Result[R] =
    if (!valid || test) this else Invalid(error)
}

case class Invalid[R](reason: String) extends Result[R] {
  def json: String = s"""Invalid("$reason")"""

  val valid = false
}

case class Valid[R](override val value: js.UndefOr[R]) extends Result[R] {
  def json = s"Valid(${js.JSON.stringify(value.asInstanceOf[js.Any], null.asInstanceOf[js.Array[js.Any]], 2)})"

  val valid = true
}

class ObjectValidator[O <: js.Object](fields: List[(String, Validator[_, _])])
    extends Validator[js.Object, O]("object") {

  private val fieldSet = fields map (_._1) toSet

  private var _stripUnknown = false

  def validateDefined(v: Any): Result[O] = {
    v match {
      case x: js.Object =>
        val d = js.Object.assign(new js.Object, x).asInstanceOf[js.Dictionary[Any]]

        for ((f, v) <- fields)
          v.validate(d.get(f).orUndefined) match {
            case Invalid(reason) => return Invalid(s"field '$f': $reason")
            case Valid(value) =>
              if (value.isDefined && value != v.asInstanceOf[UndefOr[Any]]) d(f) = value
          }

        if (_stripUnknown)
          d.keySet diff fieldSet foreach d.remove

        Valid(d.asInstanceOf[js.UndefOr[O]])
      case _ => Invalid("not an object")
    }
  }

  def stripUnknown: ObjectValidator[O] = {
    _stripUnknown = true
    this
  }

}

class NumberValidator extends RangeValidator[Double, Double]("number", _ == _, _ <= _) {

  var _integer = false
  var _positive = false
  var _negative = false
  var _nonnegative = false

  private def validateDouble(x: Double) =
    validateRange(x)
      .validate(!_integer || x.isValidInt, "not an integer")
      .validate(!_positive || x > 0, "not positive")
      .validate(!_negative || x < 0, "not negative")
      .validate(!_nonnegative || x >= 0, "not non-negative")

  def validateDefined(v: Any): Result[Double] = {
    v match {
      case s: String if !_strict => validateDouble(java.lang.Double.parseDouble(s))
      case x: Double             => validateDouble(x)
      case _                     => invalid
    }
  }

  def integer: NumberValidator = {
    _integer = true
    this
  }

  def positive: NumberValidator = {
    _positive = true
    this
  }

  def negative: NumberValidator = {
    _negative = true
    this
  }

  def nonnegative: NumberValidator = {
    _nonnegative = true
    this
  }

}

class DateValidator extends RangeValidator[String, Temporal]("string", _ == _, _ <= _) {

  private var _zoned = false

  def validateDefined(v: Any): Result[Temporal] =
    try {
      v match {
        case s: String => validateRange(s)
        case _         => invalid
      }
    } catch {
      case _: java.time.format.DateTimeParseException =>
        Invalid(if (_zoned) "invalid date/time" else "invalid ISO timestamp")
    }

  def zoned: DateValidator = {
    _zoned = true
    this
  }

  override protected def convert(a: String): Temporal = if (_zoned) ZonedDateTime.parse(a) else Instant.parse(a)

}

class StringValidator extends RangeValidator[String, String]("string", _ == _, _ <= _) {

  private var _regex: Option[Regex] = None
  private var _pattern: String = _
  private var _alphanum = false

  def validateDefined(v: Any): Result[String] = {
    v match {
      case s: String =>
        validateRange(s)
          .validate(_regex.isEmpty || _regex.get.matches(s), s"doesn't match pattern '${_pattern}'")
          .validate(!_alphanum || s.forall(_.isLetterOrDigit), "not alphanumeric")
      case _ => invalid
    }
  }

  def alphanum: StringValidator = {
    _alphanum = true
    this
  }

  def regex(pattern: String): StringValidator = {
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
  protected var _strict = false
  protected var _default: js.UndefOr[R] = js.undefined

  def validate(v: Any): Result[_ <: R] =
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

  def strict: Validator[T, R] = {
    _strict = true
    this
  }

  def default(v: R): Validator[T, R] = {
    _default = v
    this
  }

}

abstract class PrimitiveValidator[T, R](typeName: String, eq: (T, T) => Boolean) extends Validator[T, R](typeName) {

  protected val _valid = new ListBuffer[T]

  def valid(vs: T*): PrimitiveValidator[T, R] = {
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

  def min(v: T): RangeValidator[T, R] = {
    _min = v
    this
  }

  def max(v: T): RangeValidator[T, R] = {
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
