package com.vinctus

package object validator {

  def validInt = new IntValidator

  def validDouble = new DoubleValidator

  def validBoolean = new BooleanValidator

  def validString = new StringValidator

}
