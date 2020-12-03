package xyz.hyperreal.validator

import scala.scalajs.js.Dynamic
import scalajs.js

object Main extends App {

  val x = Dynamic.literal(a = 123, b = Dynamic.literal(aa = 123, bb = 567), c = 345)

  println(
    validObject(a = validInt.required.min(123), b = validObject(aa = validInt).stripUnknown).stripUnknown
      .validate(x)
      .json)

//  println(validInt.required.validate(js.undefined))

}
