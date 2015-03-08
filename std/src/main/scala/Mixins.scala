package psp
package std

trait AndThis {
  def andThis(x: Unit, xs: Unit*): this.type = this
}
