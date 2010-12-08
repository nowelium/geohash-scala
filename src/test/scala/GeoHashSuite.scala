import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class GeoHashSuite extends FunSuite with ShouldMatchers {
  import GeoHash._
  import java.math.{BigDecimal => JBigDecimal}

  test("test appropriate") {
    var _geohash = "xn77hm3ktjjv"
    (0 to 11).foreach { i =>
      var geohash = _geohash.substring(0, 12 - i)
      var precision = 12 - i
      
      def checklng = {
        var right = adjacent(geohash, 'right)
        var latlng = decode(geohash)
        var r_latlng = decode(right)

        var lng_a = new JBigDecimal(latlng._2.toString)
        var lng_b = new JBigDecimal(r_latlng._2.toString)
        println("lng => " + lng_b.subtract(lng_a).toPlainString)
        lng_b.subtract(lng_a).toPlainString.toDouble should equal(appropriate('longitude)(precision))

        var lat_a = new JBigDecimal(latlng._1.toString)
        var lat_b = new JBigDecimal(r_latlng._1.toString)
        lat_b.subtract(lat_a).doubleValue should equal(0.0D)
      }
      
      def checklat = {
        var top = adjacent(geohash, 'top)
        var latlng = decode(geohash)
        var t_latlng = decode(top)

        var lng_a = new JBigDecimal(latlng._2.toString)
        var lng_b = new JBigDecimal(t_latlng._2.toString)
        lng_b.subtract(lng_a).doubleValue should equal(0.0D)

        var lat_a = new JBigDecimal(latlng._1.toString)
        var lat_b = new JBigDecimal(t_latlng._1.toString)
        println("lat => " + lat_b.subtract(lat_a).toPlainString)
        lat_b.subtract(lat_a).toPlainString.toDouble should equal(appropriate('latitude)(precision))
      }

      checklng
      checklat
    }
  }

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

  test("test encodeRange right right") {
    var geohash = "xn77hm3"
    var r_1 = adjacent(geohash, 'right)
    var r_2 = adjacent(r_1, 'right)
    
    var d = decode(geohash)
    var d_2 = decode(r_2)

    var min = LatLng(d._1, d._2)
    var max = LatLng(d_2._1, d_2._2)

    encodeRange(min, max, 7) should ( contain(geohash) and contain(r_1) and contain(r_2) )
  }

  test("test encodeRange right botton") {
    var geohash = "xn77hm3"
    var r_1 = adjacent(geohash, 'right)
    var r_2 = adjacent(r_1, 'bottom)
    // bottom left
    var r_3 = adjacent(r_2, 'left)
    
    var d = decode(geohash)
    var d_2 = decode(r_2)

    var min = LatLng(d._1, d._2)
    var max = LatLng(d_2._1, d_2._2)

    println( encodeRange(min, max, 7) )
    println(r_1)
    println(r_2)
    println(r_3)
    encodeRange(min, max, 7) should (
      contain(geohash)
      and contain(r_1)
    )
  }
}
