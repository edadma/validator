package com.vinctus.validator

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

sealed abstract class Result[T] {
  def json: String

  def valid: Boolean

  def value: js.UndefOr[T]
}

case class Invalid[T](reason: String) extends Result[T] {
  def json: String = s"""Invalid("$reason")"""

  val valid = false

  def value: js.UndefOr[T] = sys.error("invalid")
}

case class Valid[T](value: js.UndefOr[T]) extends Result[T] {
  def json = s"Valid(${js.JSON.stringify(value.asInstanceOf[js.Any], null.asInstanceOf[js.Array[js.Any]], 2)})"

  val valid = true
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

class IntValidator extends RangeValidator[Int]("integer", _ == _, _ <= _) {

  def validateDefined(v: Any): Result[Int] = {
    v match {
      case x: Int => validateRange(x)
      case _      => invalid
    }
  }

}

class NumberValidator extends RangeValidator[Double]("double", _ == _, _ <= _) {

  def validateDefined(v: Any): Result[Double] = {
    v match {
      case x: Double => validateRange(x)
      case _         => invalid
    }
  }

}

class StringValidator extends RangeValidator[String]("string", _ == _, _ <= _) {

  def validateDefined(v: Any): Result[String] = {
    v match {
      case x: String => validateRange(x)
      case _         => invalid
    }
  }

}

class BooleanValidator extends PrimitiveValidator[Boolean]("boolean", _ == _) {

  def validateDefined(v: Any): Result[Boolean] = {
    v match {
      case x: Boolean => validatePrimitiveDefined(x)
      case _          => invalid
    }
  }

}

abstract class Validator[T](typeName: String) {

  protected var _required = false
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

  def valid(vs: T*): Validator[T] = sys.error("valid() is not defined for this type")

  def default(v: T): Validator[T] = {
    _default = v
    this
  }

  def min(v: T): Validator[T] = sys.error("min() is not defined for this type")

  def max(v: T): Validator[T] = sys.error("max() is not defined for this type")

}

abstract class PrimitiveValidator[T](typeName: String, eq: (T, T) => Boolean) extends Validator[T](typeName) {

  protected val _valid = new ListBuffer[T]

  override def valid(vs: T*): Validator[T] = {
    _valid ++= vs
    this
  }

  protected def validatePrimitiveDefined(v: T): Result[T] =
    if (_valid.isEmpty || _valid.exists(a => eq(v, a))) Valid(v)
    else Invalid(s"not one of the valid values of ${_valid map (a => s"'$a'") mkString ", "}")

}

abstract class RangeValidator[T](typeName: String, eq: (T, T) => Boolean, lte: (T, T) => Boolean)
    extends PrimitiveValidator[T](typeName, eq) {

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

  protected def validateRange(v: T): Result[T] =
    if (_min.isEmpty || lte(_min.get, v))
      if (_max.isEmpty || lte(v, _max.get))
        validatePrimitiveDefined(v)
      else
        Invalid(s"above max value of '${_max}'")
    else
      Invalid(s"below min value of '${_min}'")

}
