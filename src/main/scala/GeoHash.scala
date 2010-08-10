object Geohash {
 
  val bits = List(16, 8, 4, 2, 1)
  val base32 = "0123456789bcdefghjkmnpqrstuvwxyz"
  val neighbors:Map[Symbol, Map[Symbol, String]] = {
    val m = Map(
      ('right, "bc01fg45238967deuvhjyznpkmstqrwx"),
      ('left, "238967debc01fg45kmstqrwxuvhjyznp"),
      ('top, "p0r21436x8zb9dcf5h7kjnmqesgutwvy"),
      ('bottom, "14365h7k9dcfesgujnmqp0r2twvyx8zb")
    );
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
    );
  };
  // right{.even = right, .odd = top}
  // left{.even = left, .odd = bottom}
  // top{.even = top, .odd right}
  // bottom{.even = bottom, .odd = left}
  val borders:Map[Symbol, Map[Symbol, String]] = {
    val m = Map(
      ('right, "bcfguvyz"),
      ('left, "0145hjnp"),
      ('top, "prxz"),
      ('bottom, "028b")
    );
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
    );
  };

  def adjacent( srcHash:String, dir:Symbol ):String = {
    val length = srcHash.length
    val last = srcHash.last
    val oddEven = if(length % 2 == 0) 'even else 'odd
    val border = borders(dir)(oddEven)
    val neighbor = neighbors(dir)(oddEven)
    var base = srcHash.substring(0, length - 1)
    if(border.indexOf(last) != -1){
      base = adjacent(base, dir)
    }
    base + base32(neighbor.indexOf(last))
  }
  
  def decodeBounds( geohash:String ):((Double, Double), (Double, Double)) = {
    def toBitList( s:String ) = s.flatMap {
      c => ( "00000" + base32.indexOf(c).toBinaryString ).
        reverse.take(5).reverse.map('1' == )
    } toList
 
    def split( l:List[Boolean] ):(List[Boolean], List[Boolean]) = {
      l match {
        case Nil => ( Nil, Nil )
        case x::Nil => ( x::Nil, Nil )
        case x::y::zs => val (xs,ys) = split( zs );( x::xs, y::ys )
      }
    }
 
    def dehash( xs:List[Boolean], min:Double, max:Double ):(Double, Double) = {
      ((min, max) /: xs ){
        case ((min, max), b) =>
          if( b )( ( min + max ) / 2, max )
          else ( min, ( min + max ) / 2 )
       }
    }
    
    val ( xs ,ys ) = split( toBitList( geohash ) )
    ( dehash( ys, -90, 90 ) , dehash( xs, -180, 180 ) )
  }

  def decode( geohash:String ):(Double, Double) = {
    decodeBounds(geohash) match {
      case ((minLat, maxLat),(minLng, maxLng)) => ( (maxLat + minLat) / 2, (maxLng + minLng) / 2 )
    }
  }
  
  def encode( lat:Double, lng:Double ):String = encode(lat, lng, 12)
  def encode( lat:Double, lng:Double, precision:Int ):String = {
    var (minLat, maxLat) = (-90.0, 90.0)
    var (minLng, maxLng) = (-180.0, 180.0)

    (0 until precision).map{ p => {
      base32 apply (0 until 5).map{ i => {
        if (((5 * p) + i) % 2 == 0) {
          val mid = (minLng + maxLng) / 2.0
          if (lng > mid) {
            minLng = mid
            bits(i)
          } else {
            maxLng = mid
            0
          }
        } else {
          val mid = (minLat + maxLat) / 2.0
          if (lat > mid) {
            minLat = mid
            bits(i)
          } else {
            maxLat = mid
            0
          }
        }
      }}.reduceLeft( (a, b) => a|b )
    }}.mkString("")
  }
}
