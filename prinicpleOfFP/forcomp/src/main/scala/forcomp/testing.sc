val x1 = "abcadd"
val x2 = "cda"

val x3 = List(x1, x2)


def y(x: String) = x.toList.groupBy(x => x).map {
  case (x1, y1) => (x1, y1.length)
}

x3.flatMap(y).groupBy(x=> x._1).toList.map{ case (x,y)=> (x, y.map(_._2).sum)}.sortWith(_._1 > _._1)



val y1 = List('a', 'b', 'a')
val z1 = y1.foldLeft(List.empty[List[Char]]){
  case (acc, ele) => {
    println(acc)
    val t = List(ele) +: acc
    val p =  acc.map{x => (ele :: x).sorted}
    t ::: p
  }
}.distinct


z1.map{x => x.groupBy(y => y)}.map{_.toList}.map{x => x.map{y => (y._1, y._2.length)}}
//  .map{x => (x, x._2.length)}}

val tt = List('a', 'b', 'c')
val pp = Map('a' -> 1, 'b' -> 2)
println(pp)
val t1 = for {
  x <- tt
  y <- pp.get(x)
} yield y

def subset(xs: List[Int]): List[List[Int]] = {
  def loop(ints: List[Int]): List[List[Int]] = {
   ints match {
     case Nil => List(List.empty[Int])
     case x :: tail => {List(x) :: loop(tail) ::: loop(tail).map{x1 => x +:x1}}.distinct
   }
  }
  loop(xs)
}

subset(List(1,2,3))


