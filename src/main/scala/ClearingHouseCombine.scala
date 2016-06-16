import scala.io.Source
import scala.util.Try
import language.postfixOps

/**
  * Created by davenpcm on 6/15/16.
  */
object ClearingHouseCombine extends App{
  // main(args: Array[String]) method inherited from App
  // This means the args Array is present as an array of strings which when invoking the file is invoked via command
  // line arguments

  implicit class UntilSeqWrapper[T](seq: Seq[T]) {
    def takeUntil(predicate: T => Boolean):Seq[T] = {
      seq.span(predicate) match {
        case (head, tail) => head ++ tail.take(1)
      }
    }

    def dropUntil(predicate: T => Boolean):Seq[T] = {
      seq.span(predicate) match {
        case (head, tail) => tail.drop(1)
      }
    }
  }

  case class ClearingHouseFile(fileName: String, content: Seq[String], generalEnrollment: Int)

  def parseBaseDir(arguments: Array[String]): Try[String] = {
    val baseDirArray = for {
      args <- arguments if args.startsWith("-b")
    } yield args.drop(2)

    Try {
      if (baseDirArray.length > 1) throw new Exception("Cannot List More Than 1 Base Directory")
      if (baseDirArray.length == 0) throw new Exception("Must List A Base Directory")
      baseDirArray(0)
    }

  }

  def getFiles(arguments: Array[String]): Seq[String] = {
    val filenames = for {
      args <- arguments if args.startsWith("-f")
      } yield args.drop(2)
    filenames.toSeq
  }

  def parseFileContent(content: Seq[String]): Try[Seq[String]] = {
    Try {
      if (!content.head.startsWith("ISA|00|          |00|          |ZZ|SCT")) {
        throw new Exception("File Does Not Start With Appropriate String - Expected : ISA|00|          |00|          |ZZ|SCT...")
      }
      if (!content.drop(1).head.startsWith("GS|SV|SCT")) {
        throw new Exception("File second line does not start with Appropritate String -  Expected : GS|SV|SCT...")
      }
      if (!content.drop(content.length - 2).head.startsWith("GE|")) {
        throw new Exception("File does not include General Enrollment Line In Second to Last Line Of File - Expected : GE|")
      }
      if (!content.drop(content.length - 1).head.startsWith("IEA|1|000000000")) {
        throw new Exception("File does not end with IEA line - Expected : IEA|1|000000000")
      }
      content
    }
  }

  def getFileContent(fileName: String): Try[Seq[String]] = {
      val lines = Try{ Source.fromFile(fileName).getLines().toSeq}
      lines.flatMap(parseFileContent)
  }

  def parseGeneralEnrollment(content: Seq[String]): Int = {
    val textGE = content.drop(content.length - 2).head.drop(3).takeWhile(_ != '|')
    textGE.toInt
  }

  def getAllFileContent(baseDir: String, files: Seq[String]): Try[Seq[ClearingHouseFile]] = {
    Try {
      files
        .map(fileName => baseDir + fileName)
        .map(absolutePath => Try{
          val content = getFileContent(absolutePath).get
          val generalEnrollment = parseGeneralEnrollment(content)
            ClearingHouseFile(absolutePath, content, generalEnrollment)
          }
        )
        .map(_.get)
    }
  }

  def parseStudentRecords(content: Seq[String], finalContent: Seq[String] = Seq[String](), count: Int = 1): Try[Seq[String]] = {

    def parseAllStudentRecords(content: Seq[String]): Try[Seq[String]] = Try{
      if (content.count(_.startsWith("ST|")) != content.count(_.startsWith("SE|"))) {
        throw new Exception("Not an equal number of Student Record Openings(ST) and Closures(SE)")
      } else content
    }

    def parseStudentRecord(nextStudent: Seq[String]): Try[Seq[String]] = Try{

      def parseStudentString(string: String): String = {
        val formatted : String = f"$count%09d"
        val newString: String = if (string.startsWith("ST|")){
          val onlyImportantST = string.drop(3).takeWhile(_ != '|') + '|'
          "ST|" + onlyImportantST + formatted
        } else if (string.startsWith("SE|")) {
          val onlyImportantSE = string.drop(3).takeWhile(_ != '|') + '|'
          "SE|" + onlyImportantSE + formatted
        } else if (string.startsWith("BGN|")) {
          val beforeformatted = string.drop(4).takeWhile(_ != '|') + '|'
          val afterformatted = string.drop(4).dropWhile(_ != '|').drop(1).dropWhile(_ != '|')
          "BGN|" + beforeformatted + formatted + afterformatted
        } else
          string

        newString
      }

      if (nextStudent.count(_.startsWith("ST|")) != 1){
        throw new Exception("A student Record is not formatted correctly - Expected : ST|")
      }
      else if (nextStudent.count(_.startsWith("BGN|")) != 1) {
        throw new Exception("A student Record is not formatted correctly - Expected : BGN|")
      }
      else if (nextStudent.count(_.startsWith("SE|")) != 1) {
        throw new Exception("A student Record is not formatted correctly - Expected : SE|")
      }
      else nextStudent.map(parseStudentString)
    }



    val parsedContent = parseAllStudentRecords(content)

    val restOfContent = parsedContent.map(_.dropUntil(!_.startsWith("SE|")))
    val nextStudent = parsedContent.map(_.takeUntil(!_.startsWith("SE|")))

    val parsedNextStudent = nextStudent.flatMap(parseStudentRecord)

    if (parsedNextStudent.isSuccess ) {

      val finishedStudent = parsedNextStudent.get
      val finishedRestOfContent = restOfContent.get

      val newFinalContent = finalContent ++ finishedStudent

      if (finishedRestOfContent.isEmpty) {
        Try(newFinalContent)
      } else {
        parseStudentRecords(finishedRestOfContent, newFinalContent, count +1 )
      }
    }
    else {
      println(parsedNextStudent)
      parsedNextStudent
    }

  }

  /**
    * Takes header rows from first file, then removes header from all following. Removes 2 footer rows from all files,
    * then generates new footer row for combined
    *
    * @param files These are the list of all parsed files from the command line arguments
    * @return This returns a single ClearingHouseFile with all the content combined correctly
    */
  def combineIntoSingleFile(baseDir: String, files : Seq[ClearingHouseFile]): Try[ClearingHouseFile] = {
    val filename =  baseDir + "sfrnslc_generated.txt"
    val generalEnrollment = files.map(_.generalEnrollment).sum
    val contentNoFooters = files.map(_.content.takeWhile(!_.startsWith("GE|")))



    val header = contentNoFooters.head.take(2)
    val noHeadersOrFootersContent = contentNoFooters.flatMap(_.drop(2))
    val body = parseStudentRecords(noHeadersOrFootersContent)

    val footer = Seq(
      s"GE|$generalEnrollment|000000000",
      "IEA|1|000000000"
    )

    val parsedContent = body.flatMap(body =>  parseFileContent(header ++ body ++ footer))

    parsedContent.map { content =>
      ClearingHouseFile(
        filename,
        content,
        generalEnrollment
      )
    }
  }

  def tryUsingAutoCloseable[A <: AutoCloseable, R]
  (instantiateAutoCloseable: () => A)
  (transfer: A => scala.util.Try[R])
  : scala.util.Try[R] = Try(instantiateAutoCloseable())
      .flatMap{ autoCloseable => try transfer(autoCloseable) finally autoCloseable.close()}

  def tryPrintToFile(
                      lines: Seq[String],
                      location: java.io.File,
                      bufferSize: Int = 65536
                    ): scala.util.Try[Unit] = {
    tryUsingAutoCloseable(() => new java.io.FileWriter(location)) { //this open brace is the start of the second curried parameter to the tryUsingAutoCloseable method
      fileWriter =>
        tryUsingAutoCloseable(() => new java.io.BufferedWriter(fileWriter, bufferSize)) { //this open brace is the start of the second curried parameter to the tryUsingAutoCloseable method
          bufferedWriter =>
            tryUsingAutoCloseable(() => new java.io.PrintWriter(bufferedWriter)) { //this open brace is the start of the second curried parameter to the tryUsingAutoCloseable method
              printWriter =>
                scala.util.Try(
                  lines.foreach(line => printWriter.println(line))
                )
            }
        }
    }
  }

  def tryPrintToFile(clearingHouseFile: ClearingHouseFile): Try[Unit] = {
    tryPrintToFile(clearingHouseFile.content, new java.io.File(clearingHouseFile.fileName))
  }


  def GenerateFile(arguments: Array[String]): Try[ClearingHouseFile] = {
    val files = getFiles(arguments)
    val baseDir = parseBaseDir(arguments)
    val FileContent = baseDir.flatMap(getAllFileContent(_, files))
    val FinalFile = baseDir.flatMap(baseDir => FileContent.flatMap(combineIntoSingleFile(baseDir, _)))

    FinalFile
  }


  def run(arguments: Array[String]) = {
    val FinalFile = GenerateFile(arguments)

    if (FinalFile isSuccess){
      val WroteFile = FinalFile.flatMap(tryPrintToFile)
      if (WroteFile isSuccess) println("Completed Successfully")
    } else {
      println(FinalFile)
    }

  }

  run(args)

}
