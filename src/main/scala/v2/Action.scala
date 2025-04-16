package v2

case class Action(signals: Set[Int], videoWorkstationMapping: Map[Int, Int]):
  def show:String =
    signals.mkString("(",",",")") +
      ":" +
      videoWorkstationMapping.map((id,value)=>s"$id->$value").mkString("{",",","}")

  override def toString: String = show