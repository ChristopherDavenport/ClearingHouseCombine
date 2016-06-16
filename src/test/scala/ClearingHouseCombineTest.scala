/**
  * Created by davenpcm on 6/16/16.
  */
import org.scalatest.{FlatSpec, Matchers}
import ClearingHouseCombine._

import scala.util.Failure

class ClearingHouseCombineTest extends FlatSpec with Matchers{

  "takeUntil" should "take up to and the value which satisfies the predicate with equals" in {
    val t = Seq(1,2,3,4,5,6,7,8,9,10)
    t.takeUntil(_ == 7) === Seq(1,2,3,4,5,6,7)
  }

  it should "take up to and the value which satisfies the predicate with greaterThan" in {
    val t = Seq(1,2,3,4,5,6,7,8,9,10)
    t.takeUntil(_ > 7) === Seq(1,2,3,4,5,6,7, 8)
  }

  it should "take up to and the value which satisfies the predicate with lessThan" in {
    val t = Seq(1,2,3,4,5,6,7,8,9,10)
    t.takeUntil(_ < 7) === Seq(1,2,3,4,5,6)
  }

  "dropUntil" should "drop up to and the value which satisfies the predicate with equals" in {
    val t = Seq(1,2,3,4,5,6,7,8,9,10)
    t.dropUntil(_ == 7) === Seq(8, 9, 10)
  }

  it should "drop up to and the value which satisfies the predicate with greaterThan" in {
    val t = Seq(1,2,3,4,5,6,7,8,9,10)
    t.dropUntil(_ > 7) === Seq(9, 10)
  }

  it should "drop up to and the value which satisfies the predicate with lessThan" in {
    val t = Seq(1,2,3,4,5,6,7,8,9,10)
    t.dropUntil(_ < 7) === Seq(7, 8, 9, 10)
  }

  "parseBaseDir" should "parse a single base dir" in {
    val args = Array[String]("-b/home/test/")

    parseBaseDir(args).get === "/home/test/"
  }

  it should "add a trailing slash if it is not provided" in {
    val args = Array[String]("-b/home/test")

    parseBaseDir(args).get === "/home/test/"
  }

  it should "require atleast 1 base Directory" in {
    val args = Array[String]()

    assertResult(true){ parseBaseDir(args).isFailure}
  }

  it should "not accept more than 1 base Directory" in {
    val args = Array[String]("-b/home/test", "-b/home/test/")

    assertResult(true){ parseBaseDir(args).isFailure}
  }

  "parseFileNames" should "parse All Files Given with a -f flag" in {
    val args = Array[String](
      "-b/home/test",
      "-fsfrnslc_1287591.txt",
      "-fsfrnslc_1287597.txt" ,
      "-fsfrnslc_1287591.txt"
    )
    val expected = Seq(
      "sfrnslc_1287591.txt",
      "sfrnslc_1287597.txt",
      "sfrnslc_1287591.txt"
    )

    parseFileNames(args) === expected
  }

  it should "fail if no Files are supplied" in {
    val args = Array[String](
      "-b/home/test"
    )

    assertResult(true){ parseFileNames(args).isFailure}
  }

  "parseFileContent" should "succesfully parse FakeRes file" in {
    val t = FakeFiles.FakeRes

    parseFileContent(t).get === t
  }

  it should "succesfully parse FakePel file" in {
    val t = FakeFiles.FakePel
    parseFileContent(t).get === t
  }

  it should "fail when not given the expected first Line" in {
    val t = Seq(
      "GS|SV|SCT000000000000|CLEARINGHOUSE00|20160615|1046|000000000|X|005010"
    ) ++ FakeFiles.Footer

    assertResult(true){ parseFileContent(t).isFailure }
  }

  it should "fail when not given the expected second line" in {
    val t = Seq(
      "ISA|00|          |00|          |ZZ|SCT001487000000|ZZ|CLEARINGHOUSE00|160615|1046|U|00300|000000000|0|P|:"
    ) ++ FakeFiles.Footer
    assertResult(true){ parseFileContent(t).isFailure }

  }

  it should "fail when not given the second to last line" in {
    val t = FakeFiles.Header ++ Seq("IEA|1|000000000")
    assertResult(true){ parseFileContent(t).isFailure }
  }

  it should "fail when not given the the appropriate last line" in {
    val t = FakeFiles.Header ++ Seq("GE|3|000000000")
    assertResult(true){ parseFileContent(t).isFailure }
  }

  it should "fail when given a non-related file" in {
    val t = Seq(
      "Four score and seven years ago",
      "Live Long and Prosper",
      "There is no try",
      "The question isn't who is going to let me; it's who is going to stop me"
    )

    assertResult(true){ parseFileContent(t).isFailure }
  }

  "parseStudentRecords" should "return a combined sucess on the bodies of fakePel and FakeRes" in {
    val t = FakeFiles.FakePelBody ++ FakeFiles.FakeResBody
    parseStudentRecords(t).get === FakeFiles.CorrectlyParsedCombined
  }

  it should "return the same file sequence when calling get with FakePelBody" in {
    val t = FakeFiles.FakePelBody
    parseStudentRecords(t).get === t
  }

  it should "return the same file sequence when calling get with FakeResBody" in {
    val t = FakeFiles.FakeResBody
    parseStudentRecords(t).get === t
  }

  it should "fail with an unequal number of ST" in {
    val t = FakeFiles.FakePelBody ++ Seq("ST|WhatWhatCoolie|1425")
    assertResult(true){ parseStudentRecords(t).isFailure }
  }

  it should "fail with an unequal number of SE" in {
    val t = FakeFiles.FakePelBody ++ Seq("SE|00000000150")
    assertResult(true){ parseStudentRecords(t).isFailure }
  }

  it should "fail if it is missing a BGN in a student record" in {
    val t = FakeFiles.FakePelBody ++ Seq(
      "ST|WhatWhatCoolie|1425",
      "SE|ImSoAwesome|000000001"
    )
    assertResult(true){ parseStudentRecords(t).isFailure }
  }

  it should "have a 4 in the final line on a parse of FakePel and a new Student Record" in {
    val t = FakeFiles.FakePelBody ++ Seq(
      "ST|WhatWhatCoolie|1425",
      "BGN|EverythingIsAwesome|000000008|201620|ShortsHaveBeenEaten",
      "SE|ImSoAwesome|000000001"
    )

    assertResult(true){ parseStudentRecords(t).get.last.endsWith("4") }
  }

  "combineIntoSingleFile" should "combine the two files into a singularFile" in {
    val pel = ClearingHouseFile(
      "/home/test/pel.txt",
      FakeFiles.FakePel,
      3
    )
    val res = ClearingHouseFile(
      "/home/test/res.txt",
      FakeFiles.FakeRes,
      3
    )
    val baseDir = "/home/test/"
    val t = Seq(pel, res)

    val testDate = java.time.MonthDay.of(6,1)
    val filename = generateFileName("prefix", testDate)

    assertResult(true){ combineIntoSingleFile(baseDir, filename, t).isSuccess }
  }

  it should "have a combined general enrollment equal to the sum of the files it is created from" in {
    val pel = ClearingHouseFile(
      "/home/test/pel.txt",
      FakeFiles.FakePel,
      3
    )
    val res = ClearingHouseFile(
      "/home/test/res.txt",
      FakeFiles.FakeRes,
      3
    )
    val baseDir = "/home/test/"
    val t = Seq(pel, res)

    val testDate = java.time.MonthDay.of(6,1)
    val filename = generateFileName("prefix", testDate)

    combineIntoSingleFile(baseDir, filename, t).get.generalEnrollment === pel.generalEnrollment + res.generalEnrollment
  }

  it should "have an exact name based on baseDir" in {
    val pel = ClearingHouseFile(
      "/home/test/pel.txt",
      FakeFiles.FakePel,
      3
    )
    val res = ClearingHouseFile(
      "/home/test/res.txt",
      FakeFiles.FakeRes,
      3
    )
    val baseDir = "/home/test/"
    val t = Seq(pel, res)

    val testDate = java.time.MonthDay.of(6,1)
    val filename = generateFileName("prefix", testDate)

    combineIntoSingleFile(baseDir, filename, t).get.fileName === "/home/test/prefix0601.clr"
  }

  "generateFileName" should "create a dynamic filename in MMdd" in {
    val testDate = java.time.MonthDay.now()
    val testDay = testDate.getDayOfMonth
    val testMonth = testDate.getMonthValue

    val prefix = "prefix"
    val formattedEnd = f"$testMonth%02d" + f"$testDay%02d"

    generateFileName(prefix, testDate) === s"$prefix$formattedEnd"
  }

  it should "generate an expected filename for a known date" in {
    val testDate = java.time.MonthDay.of(6,1)
    val prefix = "prefix"

    generateFileName(prefix, testDate) === s"${prefix}0601.clr"
  }






}
