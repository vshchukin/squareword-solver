package org.squarewordsolver

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class MainIntegrationTest extends AnyFlatSpec with Matchers {

  val fixtureN = (List(
    "NIKOLAEV",
    "        ",
    "   KIEV ",
    "        ",
    "     LAK",
    "VENA    ",
    "        ",
    " KANEV  "
  ), List(
    "NIKOLAEV",
    "IAVLNOKE",
    "ALOKIEVN",
    "KVLEAINO",
    "ONEIVLAK",
    "VENAOKIL",
    "EOIVKNLA",
    "LKANEVOI"
  ))

  val fixtureS = (List(
    "SLEZA",
    "     ",
    "  LES",
    "     ",
    "     "
  ), List(
    "SLEZA",
    "AEZSL",
    "ZALES",
    "LZSAE",
    "ESALZ"
  ))

  val fixtureK = (List(
    "KARBOLIT",
    "BLOK    ",
    "   LIBRA",
    "   IL   ",
    "        ",
    "        ",
    "        ",
    "  BOKAL "
  ), List(
    "KARBOLIT",
    "BLOKTRAI",
    "OKTLIBRA",
    "TRAILKOB",
    "LBIRATKO",
    "RIKABOTL",
    "AOLTRIBK",
    "ITBOKALR"
  ))

  val fixtureP = (List(
    "PISATELY",
    "   LIST ",
    "        ",
    "        ",
    " LIPA   ",
    "        ",
    "        ",
    "  LESTY "
  ), List(
    "PISATELY",
    "EYPLISTA",
    "LATYEIPS",
    "YEASLPIT",
    "TLIPAYSE",
    "STEIYLAP",
    "ISYTPAEL",
    "APLESTYI"
  ))

  val fixtureR = (List(
    "REDAKCIY",
    "   KEDR ",
    "        ",
    "        ",
    "        ",
    "    RAK ",
    " DAR    ",
    "  CEDRA "
  ), List(
    "REDAKCIY",
    "YCIKEDRA",
    "ARYICKDE",
    "EKRDAYCI",
    "DAKCIEYR",
    "CIEYRAKD",
    "KDARYIEC",
    "IYCEDRAK"
  ))

  val fixtureLE = (List(
    "LENTO4KA",
    "  TOL   ",
    "   4ELO ",
    " KANT   ",
    "        ",
    "        ",
    "        ",
    "  4EKAN "
  ), List(
    "LENTO4KA",
    "EATOLN4K",
    "ATK4ELON",
    "4KANTELO",
    "NOLK4TAE",
    "K4EANOTL",
    "TNOLAKE4",
    "OL4EKANT"
  ))

  val fixtureKV = (List(
    "KVERSLAG",
    "VERA    ",
    "    VEGA",
    " SAG    ",
    "        ",
    "        ",
    "        ",
    "  LEGAR "
  ), List(
    "KVERSLAG",
    "VERALGKS",
    "RLSKVEGA",
    "LSAGRKVE",
    "ERGLAVSK",
    "AGVSKREL",
    "GAKVESLR",
    "SKLEGARV"
  ))

  val fixtureMA = (List(
    "MAVZOLEY",
    "        ",
    "        ",
    "  LAZO  ",
    " MEL    ",
    " LOM    ",
    "        ",
    "  ZOLA  "
  ), List(
    "MAVZOLEY",
    "ZOMVYEAL",
    "LZYEAMOV",
    "EVLAZOYM",
    "AMELVYZO",
    "YLOMEZVA",
    "OEAYMVLZ",
    "VYZOLAME"
  ))

  val fixtureEU = (List(
    "EURATOM",
    "       ",
    "    AM ",
    "       ",
    "   TOR ",
    "       ",
    "    EUR"
  ), List(
    "EURATOM",
    "RMOEUTA",
    "UETRAMO",
    "AOMURET",
    "MAETORU",
    "TRUOMAE",
    "OTAMEUR"
  ))

  val fixtureCA = (List(
    "KALQSYI",
    "       ",
    " KALYI ",
    "       ",
    "   YL  ",
    "YL     ",
    "       "
  ), List(
    "KALQSYI",
    "LIYKQAS",
    "SKALYIQ",
    "AYQSIKL",
    "IQKYLSA",
    "YLSIAQK",
    "QSIAKLY"
  ))

  val allPuzzles = List(
    fixtureCA,
    fixtureEU,
    fixtureK,
    fixtureKV,
    fixtureLE,
    fixtureMA,
    fixtureN,
    fixtureP,
    fixtureR,
    fixtureS
  )

  private def assertEqualSquarewords(pair: (List[String], List[String])) {
    val (initialPuzzle, solvedPuzzle) = pair
    val actualPuzzleArea = Main generalResultOf initialPuzzle
    println(actualPuzzleArea.linesCache)
    println()
    val expectedLinesCache = PuzzleArea.convert(solvedPuzzle)
    println(expectedLinesCache)
    assert(expectedLinesCache == actualPuzzleArea.linesCache, "expected and actual cache must be equal")
  }

  "Squareword Solver" should "match expected results" in {
    allPuzzles foreach assertEqualSquarewords
  }
}
