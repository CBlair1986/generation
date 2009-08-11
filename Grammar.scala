package generation.grammar

abstract class GrammarGenerator extends Generator {
  override def generate: String =
    choice( Array(a(thing),a(thing)+" and "+a(thing)) )
  def article(thing: String) = choice(Array("the",
    if (startsWithVowel(thing)) "an" else "a")) + thing
  def thing: String
  def adjective: String
  def startsWithVowel(string: String) =
    !("[aeiou]".r.findPrefixOf(string.trim).isEmpty)
  def a(thing: String) = choice( Array(
    article(thing),
    article(adjective + thing),
    article(adjective + " and" + adjective + thing)) )
}

object Test {
  class AGenerator extends GrammarGenerator {
    def thing = choice(" pot, plant, water, oven, man, woman".split(","))
    def adjective = choice((" green, yellow, slovenly, sloth-like,"+
     " good-for-nothing, one-of-a-kind, nondescript, wonderful, age-old,"+
     " beautiful, wondrous, full-of-life, mysterious").split(","))
    def unit = ""
  }
  def main(args: Array[String]): Unit = {
    val a = new AGenerator
    a.generateMultiple(100) foreach println
  }
}
