/**
 * Projet Scala Master 2 IMSD - 2019/2020
 * Enseignant : Mourad Karoui
 * @authors Abdou Lahat DIOP, Hassanaini Abdourahamane et Amady DIOP
 */

class Tondeuse(val coordonnees: Coordonnees, var X: Int, var Y: Int, var O: String) {

  def positionX(x: Int): Unit ={
    this.X = x
    }

  def positionY(y: Int): Unit ={
    this.Y = y
  }

  def direction(o: String, u : String): Unit ={
    this.O = o
    this.O = u
  }

  def limite(): Boolean = {
    if (0 <= this.X && this.X <= this.coordonnees.coorX && 0 <= this.Y && this.Y <= this.coordonnees.coorY){
      return true
    } else {
      return false
    }
  }

  def orientaion(navigation: String): Unit = {
    navigation match {
      case "A" => this.O match {
        case "N" => this.positionY(this.Y + 1)
        case "S" => this.positionY(this.Y - 1)
        case "E" => this.positionX(this.X + 1)
        case "W" => this.positionX(this.X - 1)
        case _ => ()
      }
      case ("G" | "D" )=> this.O match {
        case "N" => this.direction("W", "E")
        case "S" => this.direction("E", "W")
        case "E" => this.direction("N", "S")
        case "W" => this.direction("S", "N")
        case _ => ()
      }
      case _ => ()
    }
  }

  def parcourir(navigation: String): Boolean = {
    assert(List("A", "G", "D") contains (navigation))
    var tondeuse_bis = new Tondeuse(this.coordonnees, this.X, this.Y, this.O)
    tondeuse_bis.orientaion(navigation)
    return tondeuse_bis.limite()
  }

  def mouvement(direction: String): Unit = {
    for (navigation <- direction){
      if (this.parcourir(navigation.toString)){
        this.orientaion(navigation.toString)
      }
    }
  }
}