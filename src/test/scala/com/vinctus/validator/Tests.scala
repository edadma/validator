package com.vinctus.validator

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.scalajs.js

class Tests extends AnyFreeSpec with Matchers with Testing {

  "int" in {
    val v = validNumber.integer.min(5).max(200).required

    test("123", v) shouldBe "Valid(123)"
    v.validate(js.undefined).json shouldBe "Invalid(\"number required\")"
    test("4", v) shouldBe "Invalid(\"below min value of '5'\")"
    test("201", v) shouldBe "Invalid(\"above max value of '200'\")"
    test("123.4", v) shouldBe "Invalid(\"not an integer\")"
  }

  "empty object" in {
    test("{}", validEmptyObject) shouldBe "Valid({})"
    test("123", validEmptyObject) shouldBe "Invalid(\"not an object\")"
    validEmptyObject.validate(js.undefined) shouldBe Valid(js.undefined)
    validEmptyObject.required.validate(js.undefined) shouldBe Invalid("object required")
  }

  "simple object" in {
    test("""{"a": 123}""", validObject(a = validNumber.integer)) shouldBe
      """
        |Valid({
        |  "a": 123
        |})
      """.trim.stripMargin
    test("""{"a": 123.4}""", validObject(a = validNumber.integer)) shouldBe "Invalid(\"field 'a': not an integer\")"
  }

  "complex object" in {
    val v = validObject(a = validNumber.min(123).required,
                        b = validObject(ba = validDateString.max("2021")),
                        c = validString.regex("a.*").required).stripUnknown

    test(
      """
        |{
        |  "a": 123,
        |  "b": {
        |    "ba": "2020-12-04T19:48:40.513Z",
        |    "bb": 567
        |  },
        |  "c": "asdf",
        |  "d": 345
        |}
        |""".trim.stripMargin,
      v
    ) shouldBe
      """
        |Valid({
        |  "a": 123,
        |  "b": {
        |    "ba": {
        |      "Ljava_time_ZonedDateTime__f_dateTime": {
        |        "Ljava_time_LocalDateTime__f_date": {
        |          "Ljava_time_LocalDate__f_year": 2020,
        |          "Ljava_time_LocalDate__f_month": 12,
        |          "Ljava_time_LocalDate__f_day": 4,
        |          "Ljava_time_LocalDate__f_bitmap$init$0": 3
        |        },
        |        "Ljava_time_LocalDateTime__f_time": {
        |          "Ljava_time_LocalTime__f_nano": 513000000,
        |          "Ljava_time_LocalTime__f_hour": 19,
        |          "Ljava_time_LocalTime__f_minute": 48,
        |          "Ljava_time_LocalTime__f_second": 40,
        |          "Ljava_time_LocalTime__f_bitmap$init$0": 7
        |        }
        |      },
        |      "Ljava_time_ZonedDateTime__f_offset": {
        |        "Ljava_time_ZoneOffset__f_totalSeconds": 0,
        |        "Ljava_time_ZoneOffset__f_id": "Z",
        |        "Ljava_time_ZoneOffset__f_bitmap$inittrans$0": true
        |      },
        |      "Ljava_time_ZonedDateTime__f_zone": {
        |        "Ljava_time_ZoneOffset__f_totalSeconds": 0,
        |        "Ljava_time_ZoneOffset__f_id": "Z",
        |        "Ljava_time_ZoneOffset__f_bitmap$inittrans$0": true
        |      }
        |    },
        |    "bb": 567
        |  },
        |  "c": "asdf"
        |})
      """.trim.stripMargin
  }

}
