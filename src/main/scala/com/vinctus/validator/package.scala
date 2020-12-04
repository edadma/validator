package com.vinctus

package object validator {

  def validInt = new IntValidator

  def validNumber = new NumberValidator

  def validBoolean = new BooleanValidator

  def validString = new StringValidator

  def validDateString = new DateStringValidator

}
