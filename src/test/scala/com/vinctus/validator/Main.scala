package com.vinctus.validator

import scala.scalajs.js.Dynamic
import scalajs.js

object Main extends App {

  val x = Dynamic.literal(a = 123, b = Dynamic.literal(ba = 222, bb = 567), c = "asdf", d = 345)

  println(
    validObject(a = validNumber.required.min(123),
                b = validObject(ba = validInt.required).stripUnknown,
                c = validString.valid("a", "asdf").required).stripUnknown
      .validate(x)
      .json)

//  println(validInt.required.validate(js.undefined))

}
