object Test extends App {
  import scala.reflect._;
  def titi: Unit = {
    var truc = 0
    val tata = mirror.reify{() => {
      truc = truc + 6
    }}
    ()
  }
}
