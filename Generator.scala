package generation

abstract class Generator {
  import util.Random
  val r = new Random
  def unit: String
  val maxLength = 10
  def generate(): String = {
    val length = r.nextInt(maxLength).max(1)
    (0 until length).map(_ => unit).mkString("")
  }
  def generateMultiple(num: Int) = (1 to num).map(_ => generate).toList
  def choice(list: Array[String]): String = {
    list(r.nextInt(list.length))
  }
}

class SyllableGenerator(syllables: Array[String]) extends Generator {
  def this(sList: String) {
    this(sList.split(","))
  }
  def unit = choice(syllables)
}

class CVGenerator(
  consonants: Array[String],
  vowels: Array[String]) extends Generator {
  def this(cList: String, vList: String) {
    this(
      cList.split(","),
      vList.split(",")
    )
  }
  override def unit = choice(Array(c+v,v+c))
  override val maxLength = 4
  def c = choice(consonants)
  def v = choice(vowels)
}
