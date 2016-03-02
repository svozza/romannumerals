object RomanNumerals {

  val numerals = List(("M", 1000), ("CM", 900), ("D", 500), ("CD", 400), ("C", 100),
    ("XC", 90), ("L", 50), ("XL", 40), ("X", 10), ("IX", 9), ("V", 5),
    ("IV", 4), ("I", 1))

  def convert(n : Int) : Either[String,String] = {
    if(n >= 5000) return Left("Numbers of 5000 and over not supported")
    if(n <= 0) return Left("Numbers of 0 and below not supported")
    val roman = numerals.foldLeft(("", n))((acc, pair) => {
      val div = acc._2 / pair._2
      if(div == 0) acc else (acc._1 + (pair._1 * div), acc._2 - (div * pair._2))
    })
    Right(roman._1)
  }

  def main( args:Array[String] ): Unit = {
    assert(convert(5000) == Left("Numbers of 5000 and over not supported"))
    assert(convert(4999) == Right("MMMMCMXCIX"))
    assert(convert(4000) == Right("MMMM"))
    assert(convert(2444) == Right("MMCDXLIV"))
    assert(convert(1999) == Right("MCMXCIX"))
    assert(convert(1981) == Right("MCMLXXXI"))
    assert(convert(1259) == Right("MCCLIX"))
    assert(convert(969) == Right("CMLXIX"))
    assert(convert(499) == Right("CDXCIX"))
    assert(convert(379) == Right("CCCLXXIX"))
    assert(convert(130) == Right("CXXX"))
    assert(convert(99) == Right("XCIX"))
    assert(convert(87) == Right("LXXXVII"))
    assert(convert(49) == Right("XLIX"))
    assert(convert(34) == Right("XXXIV"))
    assert(convert(19) == Right("XIX"))
    assert(convert(14) == Right("XIV"))
    assert(convert(13) == Right("XIII"))
    assert(convert(9) == Right("IX"))
    assert(convert(8) == Right("VIII"))
    assert(convert(5) == Right("V"))
    assert(convert(4) == Right("IV"))
    assert(convert(3) == Right("III"))
    assert(convert(2) == Right("II"))
    assert(convert(1) == Right("I"))
    assert(convert(0) == Left("Numbers of 0 and below not supported"))
    assert(convert(-1) == Left("Numbers of 0 and below not supported"))
  }

}
