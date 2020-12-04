package com.vinctus.validator

import scala.scalajs.js.JSON

trait Testing {

  def test(json: String, validator: Validator[_, _]): String = validator.validate(JSON.parse(json)).json

}
