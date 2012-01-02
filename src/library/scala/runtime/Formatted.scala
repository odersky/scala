package scala.runtime

class Formatted(expr: Any, fmt: String) {
  def show = expr formatted fmt
}