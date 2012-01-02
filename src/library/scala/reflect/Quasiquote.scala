package scala.reflect

// todo. splices should really have type List[Ucode]
// however, I'm temporary changing them to List[Any] or otherwise nested lifting is going to screw things up
// I'd implement nested lifts right away, but they belong to topic/quasiquotes
// when nested lifts are implemented (as a merge from topic/quasiquotes), type of splices should be restored

abstract class Quasiquote(parts: List[String], splices: List[Any], formats: List[String])

class QuasiquoteLiteral(parts: List[String], splices: List[Any], formats: List[String]) extends Quasiquote(parts, splices, formats)

class QuasiquotePattern(parts: List[String], splices: List[Any], formats: List[String]) extends Quasiquote(parts, splices, formats)
