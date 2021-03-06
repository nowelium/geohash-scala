object GeoHash {

  case class LatLng(latitude: Double, longitude: Double)
 
  val bits = List(16, 8, 4, 2, 1)
  val base32 = "0123456789bcdefghjkmnpqrstuvwxyz"
  val dirs = List('right, 'left, 'top, 'bottom)
  val neighbors: Map[Symbol, Map[Symbol, String]] = {
    val m = Map(
      ('right, "bc01fg45238967deuvhjyznpkmstqrwx"),
      ('left, "238967debc01fg45kmstqrwxuvhjyznp"),
      ('top, "p0r21436x8zb9dcf5h7kjnmqesgutwvy"),
      ('bottom, "14365h7k9dcfesgujnmqp0r2twvyx8zb")
    )
    // right{.even = right, .odd = top}
    // left{.even = left, .odd = bottom}
    // top{.even = top, .odd = right}
    // bottom{.even = bottom, .odd = left}
    Map(
      ('right, Map(
        ('even, m('right)), ('odd, m('top))
      )),
      ('left, Map(
        ('even, m('left)), ('odd, m('bottom))
      )),
      ('top, Map(
        ('even, m('top)), ('odd, m('right))
      )),
      ('bottom, Map(
        ('even, m('bottom)), ('odd, m('left))
      ))
    )
  }

  // right{.even = right, .odd = top}
  // left{.even = left, .odd = bottom}
  // top{.even = top, .odd right}
  // bottom{.even = bottom, .odd = left}
  val borders: Map[Symbol, Map[Symbol, String]] = {
    val m = Map(
      ('right, "bcfguvyz"),
      ('left, "0145hjnp"),
      ('top, "prxz"),
      ('bottom, "028b")
    )
    Map(
      ('right, Map(
        ('even, m('right)), ('odd, m('top))
      )),
      ('left, Map(
        ('even, m('left)), ('odd, m('bottom))
      )),
      ('top, Map(
        ('even, m('top)), ('odd, m('right))
      )),
      ('bottom, Map(
        ('even, m('bottom)), ('odd, m('left))
      ))
    )
  }

  val appropriate:Map[Symbol, Map[Int, Double]] = Map(
    'latitude -> Map(
      12 -> 0.000000167638065,
      11 -> 0.000001341104504,
      10 -> 0.00000536441803,
      9 -> 0.00004291534424,
      8 -> 0.000171661376956,
      7 -> 0.00137329101562,
      6 -> 0.00549316406250,
      5 -> 0.04394531250,
      4 -> 0.175781250,
      3 -> 1.406250,
      2 -> 5.6250,
      1 -> 45
    ),
    'longitude -> Map(
      12 -> 0.00000033527613,
      11 -> 0.00000134110452,
      10 -> 0.00001072883605,
      9 -> 0.00004291534424,
      8 -> 0.00034332275392,
      7 -> 0.00137329101562,
      6 -> 0.0109863281250,
      5 -> 0.04394531250,
      4 -> 0.35156250,
      3 -> 1.406250,
      2 -> 11.250,
      1 -> -315
    )
  )

  def toDir(str: String): Option[Symbol] = dirs.find(_.name.equals(str))

  def adjacent(srcHash: String, dir: Symbol): String = {
    val length = srcHash.length
    val last = srcHash.last
    val oddEven = if(length % 2 == 0) 'even else 'odd
    val border = borders(dir)(oddEven)
    val neighbor = neighbors(dir)(oddEven)
    var base = srcHash.substring(0, length - 1)
    if(border.indexOf(last) != -1 && 1 < length){
      base = adjacent(base, dir)
    }
    base + base32(neighbor.indexOf(last))
  }
  
  def decodeBounds(geohash: String): ((Double, Double), (Double, Double)) = {
    def toBitList(s: String) = s.flatMap {
      c => ( "00000" + base32.indexOf(c).toBinaryString ).
        reverse.take(5).reverse.map('1' == )
    }.toList
 
    def split(l: List[Boolean]): (List[Boolean], List[Boolean]) = {
      l match {
        case Nil => ( Nil, Nil )
        case x::Nil => ( x::Nil, Nil )
        case x::y::zs => val (xs,ys) = split( zs );( x::xs, y::ys )
      }
    }
 
    def dehash(xs: List[Boolean], min: Double, max: Double): (Double, Double) = {
      ((min, max) /: xs ){
        case ((min, max), b) =>
          if( b )( ( min + max ) / 2, max )
          else ( min, ( min + max ) / 2 )
       }
    }
    
    val ( xs ,ys ) = split( toBitList( geohash ) )
    ( dehash( ys, -90, 90 ) , dehash( xs, -180, 180 ) )
  }

  def decode(geohash: String): (Double, Double) = {
    decodeBounds(geohash) match {
      case ((minLat, maxLat),(minLng, maxLng)) =>
        ( (maxLat + minLat) / 2, (maxLng + minLng) / 2 )
    }
  }
  
  def encode(latlng: LatLng): String = encode(latlng.latitude, latlng.longitude, 12)
  def encode(latlng: LatLng, precision: Int): String = encode(latlng.latitude, latlng.longitude, precision)
  def encode(lat: Double, lng: Double): String = encode(lat, lng, 12)
  def encode(lat: Double, lng: Double, precision: Int): String = {
    var (minLat, maxLat) = (-90.0, 90.0)
    var (minLng, maxLng) = (-180.0, 180.0)

    (0 until precision).map { p =>
      base32 apply (0 until 5).map { i =>
        if (((5 * p) + i) % 2 == 0) {
          var mid = (minLng + maxLng) / 2.0
          if (lng > mid) {
            minLng = mid
            bits(i)
          } else {
            maxLng = mid
            0
          }
        } else {
          var mid = (minLat + maxLat) / 2.0
          if (lat > mid) {
            minLat = mid
            bits(i)
          } else {
            maxLat = mid
            0
          }
        }
      }.reduceLeft( (a, b) => a | b )
    }.mkString("")
  }

  def encodeRange(min: LatLng, max: LatLng): List[String] = encodeRange(min, max, 12)
  def encodeRange(min: LatLng, max: LatLng, precision: Int): List[String] = {
    if(precision < 1 || 12 < precision){
      throw new java.lang.IndexOutOfBoundsException("precision must be 1 <= precision <= 12")
    }

    var minLat = 0.0D
    var minLon = 0.0D
    var maxLat = 0.0D
    var maxLon = 0.0D

    if(min.latitude < max.latitude){
      minLat = min.latitude
      maxLat = max.latitude
    } else {
      minLat = max.latitude
      maxLat = min.latitude
    }
    if(min.longitude < max.longitude){
      minLon = min.longitude
      maxLon = max.longitude
    } else {
      minLon = max.longitude
      maxLon = min.longitude
    }

    var stepLat = appropriate('latitude)(precision)
    var stepLon = appropriate('longitude)(precision)

    var hashes = Set[String]()
    var lat = minLat
    do {
      var lon = minLon
      do {
        hashes = hashes + encode(lat, lon, precision)

        lon = lon + stepLon
      } while(lon <= maxLon)

      lat = lat + stepLat
    } while(lat <= maxLat)

    hashes.toList
  }

  def within(geohash: String, latlng: LatLng): Boolean = {
    var bounds = decodeBounds(geohash)
    var boundLatitude = bounds._1
    var boundLongitude = bounds._2

    var latitude = BigDecimal(latlng.latitude.toString)
    var longitude = BigDecimal(latlng.longitude.toString)

    // TODO: scaling size
    // current --> latitude <= bounds.scale,
    // tbd: s = max(latitude.scale, bounds.scale); setScale(latitude, s), setScale(bounds, s);
    var boundLats = (
      BigDecimal(boundLatitude._1.toString).setScale(latitude.scale, BigDecimal.RoundingMode.DOWN),
      BigDecimal(boundLatitude._2.toString).setScale(latitude.scale, BigDecimal.RoundingMode.UP)
    )
    var boundLongs = (
      BigDecimal(boundLongitude._1.toString).setScale(longitude.scale, BigDecimal.RoundingMode.UP),
      BigDecimal(boundLongitude._2.toString).setScale(longitude.scale, BigDecimal.RoundingMode.DOWN)
    )

    //if(boundLats._1.compare(latitude) != -1 && latitude.compare(boundLats._2) != 1){
    //  if(boundLongs._1.compare(longitude) != -1 && longitude.compare(boundLongs._2) != 1){

    if(boundLats._1 <= latitude && latitude <= boundLats._2){
      if(boundLongs._1 <= longitude && longitude <= boundLongs._2){
        return true
      }
    }
    return false
  }
}
