import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class GeoHashSuite extends FunSuite with ShouldMatchers {
  import GeoHash._

  test("test decoding") {
    val (lat,lng) = decode("dqcw4bnrs6s7")
    lat should be ( 39.02474 plusOrMinus 0.00001)
    lng should be (-76.51100 plusOrMinus 0.00001)
  }

  test("test encoding") {
    val geohash = "dqcw4bnrs6s7"
    val (lat,lng) = decode(geohash)
    encode(lat,lng) should equal (geohash)
  }

  test("test adjacent") {
    adjacent("dqcw4bnrs6s7", 'top) should equal("dqcw4bnrs6sk")
    adjacent("dqcw4bnrs6s7", 'left) should equal("dqcw4bnrs6s5")
    adjacent("dqcw4bnrs6s7", 'bottom) should equal("dqcw4bnrs6s6")
    adjacent("dqcw4bnrs6s7", 'right) should equal("dqcw4bnrs6se")
  }

  test("test toDir") {
    toDir("top") should equal(Some('top))
    toDir("right") should equal(Some('right))
    toDir("bottom") should equal(Some('bottom))
    toDir("left") should equal(Some('left))
    toDir("foobar") should equal(None)
  }

  test("test within") {
    within("dqcw4bnrs6s7", LatLng(39.02474, -76.51100)) should equal(true)
    within("dqcw4bnrs6s7", LatLng(39.02474, 76.51100)) should equal(false)

    within("xn77hm3", LatLng(35.713257, 139.759027)) should equal(true)
    within("xn77hm3", LatLng(35.713256, 139.759027)) should equal(true)
    within("xn77hm3", LatLng(35.713254, 139.759027)) should equal(true)
    within("xn77hm3", LatLng(35.712254, 139.759027)) should equal(false)
    within("xn77hm3", LatLng(35.711254, 139.759027)) should equal(false)
  }
}
