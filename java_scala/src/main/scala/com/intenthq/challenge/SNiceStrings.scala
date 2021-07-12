package com.intenthq.challenge

object SNiceStrings {

// From http://adventofcode.com/day/5
//  --- Day 5: Doesn't He Have Intern-Elves For This? ---
//
//  Santa needs help figuring out which strings in his text file are naughty or nice.
//
//    A nice string is one with all of the following properties:
//
//    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
//  It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
//    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
//    For example:
//
//    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
//  aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
//    jchzalrnumimnmhp is naughty because it has no double letter.
//    haegwjzuvuyypxyu is naughty because it contains the string xy.
//    dvszwmarrgswjxmb is naughty because it contains only one vowel.
//    How many strings are nice?

  def nice(xs: List[String]): Int =
    xs.count(s => three_vowels(s) && duplicate_letters(s) && !disallowed_strings(s))

  def three_vowels(s: String): Boolean = {
    val vowels = Set[Char]('a', 'e', 'i', 'o', 'u')
    s.filter(vowels.contains).length >= 3
  }

  def duplicate_letters(s: String): Boolean = {
    val whole = s.toCharArray
    val tail = s.tail.toCharArray
    val zipped = whole.zip(tail)
    zipped.exists( pair => pair._1 == pair._2)
  }

  def disallowed_strings(s: String): Boolean = {
    s.contains("ab") ||
    s.contains("cd") ||
    s.contains("pq") ||
    s.contains("xy")
  }

}
