package com.vinctus

package object validator {

  def validNumber = new NumberValidator

  def validBoolean = new BooleanValidator

  def validString = new StringValidator

  def validDate = new DateValidator

  def validEmptyObject = new ObjectValidator(Nil)

}
