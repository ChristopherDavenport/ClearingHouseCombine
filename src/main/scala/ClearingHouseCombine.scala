import scala.io.Source
import scala.util.Try
import language.postfixOps
import scala.annotation.tailrec

/**
  * Created by davenpcm on 6/15/16.
  */
object ClearingHouseCombine extends App{
  // main(args: Array[String]) method inherited from App
  // This means the args Array is present as an array of strings which when invoking the file is invoked via command
  // line arguments

  /**
    * This class is to make available a function I like. It filters to on a predicate but rather than stopping before
    * what it found, it takes what it found as well as that element. Take keeps up to and including the T which
    * satisfies the predicate and dropUntil drops up to and including the T which satisfies the predicate
    *
    * @param seq A Sequence
    * @tparam T Any Type
    */
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

  /**
    * Command Line Argument Parser to extract the baseDirectory.
    * -b argument is used to indicate where the files are and where the final output file should be placed
    * Example: -b/home/banner/jsTEST/
    *
    * @param arguments This is an array of string, but it is intended to parse the args base array to receive command
    *                  line arguments
    * @return This returns the absolute path of the baseDirectory. Assuming it was passed with the -b argument
    */
  def parseBaseDir(arguments: Array[String]): Try[String] = {
    val baseDirArray = for {
      args <- arguments if args.startsWith("-b")
    } yield args.drop(2)

    Try {
      if (baseDirArray.length > 1) throw new Exception("Cannot List More Than 1 Base Directory")
      if (baseDirArray.length == 0) throw new Exception("Must List A Base Directory")
      val unparsedBaseDir = baseDirArray(0)
      if (unparsedBaseDir.endsWith("/")) unparsedBaseDir else unparsedBaseDir + "/"
    }

  }

  /**
    * Command Line Argument Parser to extract Prefix
    * -p argument used to indicate the prefix for the filename
    * @param arguments This is an array of string, but it is intended to parse the args base array to receive command
    *                  line arguments
    * @return This returns a Try of String for a prefix to name the file
    */
  def parsePrefix(arguments: Array[String]): Try[String] = {
    val prefixArray = for {
      args <- arguments if args.startsWith("-p")
    } yield args.drop(2)

    Try {
      if (prefixArray.length > 1) throw new Exception("Cannot List More Than 1 Prefix")
      if (prefixArray.length == 0) throw new Exception("Must provide A Prefix: -pPREFIX")
      val prefix = prefixArray(0)
      prefix
    }
  }

  /**
    * Command Line Argument Parser to Extract the Files
    * -f is the argument used to indicate the filename
    * Example: -fsfrnslc_1287591.txt
    *
    * @param arguments Command Line Arguments passed as an array of Strings.
    * @return This returns any files indicated in the command line arguments
    */
  def parseFileNames(arguments: Array[String]): Try[Seq[String]] = {
    val filenames = for {
      args <- arguments if args.startsWith("-f")
      } yield args.drop(2)

    Try {
      if (filenames.length == 0) throw new Exception("Must provide atleast 1 file to parse: -fFileName")
      else
        filenames.toSeq
    }

  }

  /**
    * This parses the file contents of the imported file and confirms that it matches the expected format of a Clearing
    * House File, this stops erroneous files from going through the combination process.
    *
    * @param content This is the entire content of the file interpreted as a sequence of Strings
    * @return Either it returns the Sequence because it is valid or an error because it the file is not appropriately
    *         formatted with the header and footer as this program is written for.
    */
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

  /**
    * This is a function which attempts to get the File, which can fail due to IO errors etc. and then it parses that
    * content so it only proceeds if all the constituent files are built correctly.
    *
    * @param fileName The name of the file to parse
    * @return The content of that file if it is valid
    */
  def getFileContent(fileName: String): Try[Seq[String]] = {
      val lines = Try{ Source.fromFile(fileName).getLines().toSeq}
      lines.flatMap(parseFileContent)
  }

  /**
    * General Enrollment is used to build the final footer and should match the last student record in each individual
    * file. We need an accurate count to build the appropriate footer for our final combined document.
    *
    * @param content A files content.
    * @return The number of students whose information is reported in that file.
    */
  def parseGeneralEnrollment(content: Seq[String]): Int = {
    val textGE = content.drop(content.length - 2).head.drop(3).takeWhile(_ != '|')
    textGE.toInt
  }

  /**
    * This is the command line argument to code function. Here we take the baseDirectory and the files and build
    * ClearingHouseFiles
    *
    * @param baseDir This is the baseDirectory that all of the files are in and that the output file will be generated
    *                in
    * @param files These are the names of the individual files to be parsed and brought into the program to generate the
    *              combined file.
    * @return This will generate a single Try for parsing all files. Allowing you to flatMap into all of them rather
    *         than through each individual try. We stayed away from Monad Transformers using this function as a
    *         bottleneck failure point before the merger.
    */
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

  /**
    * This function takes the body of all of the files, where the student records are, and validates and makes sure that
    * the autoincrement variable has been applied to all records, that the body follows the criteria of equal opening
    * and closing tags, and that each student record contains a single ST, BGN, and SE line
    *
    * @param content This is the entire body of the file where all individual student records are.
    * @param finalContent This is all content that has currently been parsed and is ready for delivery.
    * @param count This is the autoincremented number of the amount of students that are contained in the final file.
    *              This is not enforced in code, but the final count should match the General Enrollment of the combined
    *              file
    * @return This should return the body re-parsed from the original files and merged into a singular body ready for
    *         printing into the file between the header and the footer.
    */
  @tailrec
  def parseStudentRecords(content: Seq[String], finalContent: Seq[String] = Seq[String](), count: Int = 1): Try[Seq[String]] = {

    /**
      * This function validates that the content given to parseStudentRecords is valid and will terminate. ST and SE
      * start and stop individual student records so there need to be an equal amount of opening and closing tags.
      *
      * @param content This is the content passed to the function parseStudentRecords, and each remaining set as it is
      *                removed through iterations through the function.
      * @return A Successfully validated Record or an Error indicating that the problem happened at this function.
      */
    def parseAllStudentRecords(content: Seq[String]): Try[Seq[String]] = Try{
      if (content.count(_.startsWith("ST|")) != content.count(_.startsWith("SE|"))) {
        throw new Exception("Not an equal number of Student Record Openings(ST) and Closures(SE)")
      } else content
    }

    /**
      * The purpose of this function is to replace an autoincremented value that is embedded in the source files and
      * to replaces that with our own autoincremented value, the count in the containing function parseStudentRecords.
      * This is necessary as otherwise the values are not consistent and reset between each file.
      *
      * @param nextStudent This is a singular student record which should start at ST and end on an SE It also must contain
      *                    one BGN record. These conditions are all validated in this function.
      * @return This returns either an Error or The Student Record with the AutoIncrement from the independent files
      *         removed and replaced by our global autoincrement.
      */
    def parseStudentRecord(nextStudent: Seq[String]): Try[Seq[String]] = Try{

      /**
        * The replacement function should replace the 9 digit autoincrement on strings starting with ST, SE and BGN with
        * our overall autoincremented number also formatted to 9 digits
        *
        * @param string A string, which is a single line from the document
        * @return Either the original string, if the string does not start with ST, SE or BGN, or a String that is equal
        *         to before but now with the autoincrement number replaced.
        */
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

    if (parsedNextStudent.isSuccess ) { // If everything went well this iteration
      val finishedStudent = parsedNextStudent.get
      val finishedRestOfContent = restOfContent.get
      val newFinalContent = finalContent ++ finishedStudent
      if (finishedRestOfContent.isEmpty) {
        Try(newFinalContent)
      } else {
        parseStudentRecords(finishedRestOfContent, newFinalContent, count +1 )
      }
    }
    else { // If it fails at this iteration
      parsedNextStudent
    }

  }

  /**
    * This function generates a filename for the completed file format is PrefixMMdd.clr
    * Example : 0014806001.clr
    *
    * @param schoolprefix The prefix to be appended to the filename
    * @param date A monthday value, either fixed for testing or dynamic for production
    * @return A string to use as the finished filename
    */
  def generateFileName(schoolprefix: String, date: java.time.MonthDay): String = {
    val formatter = java.time.format.DateTimeFormatter.ofPattern("MMdd")
    val formatted = date.format(formatter)

    schoolprefix + formatted + ".clr"
  }

  /**
    * Takes header rows from first file, then removes header from all following. Removes 2 footer rows from all files,
    * then generates new footer row for combined
    *
    * @param files These are the list of all parsed files from the command line arguments
    * @return This returns a single ClearingHouseFile with all the content combined correctly
    */
  def combineIntoSingleFile(baseDir: String, filename: String, files : Seq[ClearingHouseFile]): Try[ClearingHouseFile] = {
    val fileName =  baseDir + filename
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
        fileName,
        content,
        generalEnrollment
      )
    }
  }

  /**
    *  This function is important as it ensures that our files close. So we do not take system resources and that we are
    *  not overly dependent of the garbage collector
    *
    * @param instantiateAutoCloseable Make Something that is closeable
    * @param transfer Use it to do something
    * @tparam A The autoCloseable
    * @tparam R What we are try to do with it
    * @return What we are trying to do with it or an error guaranteeing that the autocloseable resource is closed
    *         on error or success.
    */
  def tryUsingAutoCloseable[A <: AutoCloseable, R]
  (instantiateAutoCloseable: () => A)
  (transfer: A => scala.util.Try[R])
  : scala.util.Try[R] = Try(instantiateAutoCloseable())
      .flatMap{ autoCloseable => try transfer(autoCloseable) finally autoCloseable.close()}

  /**
    *
    * @param lines A String per line to write
    * @param location Where the file to be written should be
    * @param bufferSize The buffer size, to minimize IO delay
    * @return A Try of Unit, which a Success meaning the file was written successfully and a Failure meaning it was not,
    *         and containing what the error was.
    */
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

  /**
    * A Overloaded wrapper for tryPrintToFile above to take our our dataType ClearingHouseFile and writes that file.
    *
    * @param clearingHouseFile A clearinghouseFile to be written
    * @return A Try of Unit, which a Success meaning the file was written successfully and a Failure meaning it was not,
    *         and containing what the error was.
    */
  def tryPrintToFile(clearingHouseFile: ClearingHouseFile): Try[Unit] = {
    tryPrintToFile(clearingHouseFile.content, new java.io.File(clearingHouseFile.fileName))
  }


  /**
    * This is a Safe To Perform General Function Which does everything beside the final write to disk stage. This can
    * be used to test, although a more robust getFiles may need to be written to use Baked In Examples
    *
    * @param arguments An array of strings that are supposed to represent command line arguments
    * @return A Try of a ClearingHouseFile, the final merged file or the error that stopped it creation.
    */
  def GenerateFile(arguments: Array[String]): Try[ClearingHouseFile] = {
    val fileNames = parseFileNames(arguments)
    val baseDir = parseBaseDir(arguments)
    val prefix = parsePrefix(arguments)

    val fileName = prefix.map(generateFileName(_, java.time.MonthDay.now()))

    val FileContent = for {
      dir <- baseDir
      files <- fileNames
      result <- getAllFileContent(dir, files)
    } yield result

    val FinalFile = for {
      dir <- baseDir
      name <- fileName
      content <- FileContent
      result <- combineIntoSingleFile(dir, name, content)
    } yield result

    FinalFile
  }

  /**
    * This is the Production Function which writes the finished file to disk. If a success it writes the file, otherwise
    * it echos the error to the command line.
    *
    * @param arguments An array of strings that are supposed to represent command line arguments
    */
  def run(arguments: Array[String]) = {
    val FinalFile = GenerateFile(arguments)
    val WroteFile = FinalFile.flatMap(tryPrintToFile)

    if (WroteFile isSuccess) println(Console.GREEN + "Completed Successfully" + Console.RESET)
    else {
      val message = WroteFile.failed.get.getMessage
      println(Console.RED + s"Error: $message" + Console.RESET)
    }

  }

  run(args)

}
