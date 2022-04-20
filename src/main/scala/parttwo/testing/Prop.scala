package parttwo.testing

trait Prop {
  def check: Boolean
  def &&(other: Prop): Prop = new Prop {
    override def check: Boolean = Prop.this.check && other.check
  }
}
