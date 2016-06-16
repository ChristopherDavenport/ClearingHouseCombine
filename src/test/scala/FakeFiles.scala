/**
  * Created by davenpcm on 6/16/16.
  */
object FakeFiles {

  val Header = Seq(
    "ISA|00|          |00|          |ZZ|SCT001487000000|ZZ|CLEARINGHOUSE00|160615|1046|U|00300|000000000|0|P|:",
    "GS|SV|SCT000000000000|CLEARINGHOUSE00|20160615|1046|000000000|X|005010"
  )

  val Footer = Seq(
    "GE|3|000000000",
    "IEA|1|000000000"
  )

  val FakePelBody = Seq(
    "ST|190|000000001",
    "BGN|11|000000001|20160615|1057|LT",
    "ENT|02|M8|DS|00000000||||U2|PEL Spring Semester 2016",
    "SE|22|000000001",
    "ST|190|000000002",
    "BGN|11|000000002|20160615|1057|LT",
    "ENR||UN||||||||N|Y|D8|20160615",
    "ENT|02|M8|DS|00148700||||U2|PEL Spring Semester 2016",
    "SE|23|000000002",
    "ST|190|000000003",
    "BGN|11|000000003|20160615|1057|LT",
    "ENR|EB3|UN||||||||N|Y|D8|20160615",
    "DTP|382|RD8|20160102-20160522",
    "DTP|007|D8|",
    "SUM|||Y",
    "N1|ZZ|SB",
    "ENT|02|M8|DS|000000000||||U2|PEL Spring Semester 2016",
    "NTE||N",
    "SE|24|000000003"
  )

  val FakePel = Header ++ FakePelBody ++ Footer

  val FakeResBody = Seq(
    "ST|190|000000001",
    "BGN|11|000000001|20160615|1057|LT",
    "ENT|02|M8|DS|00000000||||U2|Spring Semester 2016",
    "SE|22|000000001",
    "ST|190|000000002",
    "BGN|11|000000002|20160615|1057|LT",
    "ENR||UN||||||||N|Y|D8|20160615",
    "ENT|02|M8|DS|00000000||||U2|Spring Semester 2016",
    "SE|23|000000002",
    "ST|190|000000003",
    "BGN|11|000000003|20160615|1057|LT",
    "ENR|EB3|UN||||||||N|Y|D8|20160615",
    "DTP|382|RD8|20160102-20160522",
    "DTP|007|D8|",
    "SUM|||Y",
    "N1|ZZ|SB",
    "ENT|02|M8|DS|00000000||||U2|Spring Semester 2016",
    "NTE||N",
    "SE|24|000000003"
  )

  val FakeRes = Header ++ FakeResBody ++ Footer

  val CorrectlyParsedCombined = Seq(
    "ST|190|000000001",
    "BGN|11|000000001|20160615|1057|LT",
    "ENT|02|M8|DS|00000000||||U2|PEL Spring Semester 2016",
    "SE|22|000000001",
    "ST|190|000000002",
    "BGN|11|000000002|20160615|1057|LT",
    "ENR||UN||||||||N|Y|D8|20160615",
    "ENT|02|M8|DS|00148700||||U2|PEL Spring Semester 2016",
    "SE|23|000000002",
    "ST|190|000000003",
    "BGN|11|000000003|20160615|1057|LT",
    "ENR|EB3|UN||||||||N|Y|D8|20160615",
    "DTP|382|RD8|20160102-20160522",
    "DTP|007|D8|",
    "SUM|||Y",
    "N1|ZZ|SB",
    "ENT|02|M8|DS|000000000||||U2|PEL Spring Semester 2016",
    "NTE||N",
    "SE|24|000000003",
    "ST|190|000000004",
    "BGN|11|000000004|20160615|1057|LT",
    "ENT|02|M8|DS|00000000||||U2|Spring Semester 2016",
    "SE|22|000000004",
    "ST|190|000000005",
    "BGN|11|000000005|20160615|1057|LT",
    "ENR||UN||||||||N|Y|D8|20160615",
    "ENT|02|M8|DS|00000000||||U2|Spring Semester 2016",
    "SE|23|000000005",
    "ST|190|000000006",
    "BGN|11|000000006|20160615|1057|LT",
    "ENR|EB3|UN||||||||N|Y|D8|20160615",
    "DTP|382|RD8|20160102-20160522",
    "DTP|007|D8|",
    "SUM|||Y",
    "N1|ZZ|SB",
    "ENT|02|M8|DS|00000000||||U2|Spring Semester 2016",
    "NTE||N",
    "SE|24|000000006"
  )
}
