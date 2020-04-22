/**
 * Projet Scala Master 2 IMSD - 2019/2020
 * Enseignant : Mourad Karoui
 * @authors : Abdou Lahat DIOP, Hassanaini Abdourahamane et Amady DIOP
 */

import scala.io.Source

object Main extends App {

  val file = "src/Entree/test.txt"
  var input = {
    val src = Source.fromFile(file).getLines().toList
    src
  }

  val coor = input(0).split(" ")
  val pelouse = new Coordonnees(coor(0).toInt, coor(1).toInt)
  input = input.patch(0, Nil, 1)

  for (tond <- 0 to input.length if input.length > 1){

    var position = input(0).toString.split(" ")
    var direction = input(1).toString
    input = input.drop(2)
    var tondeuse_bis = new Tondeuse(pelouse, position(0).toInt, position(1).toInt, position(2))
    tondeuse_bis.mouvement(direction)
    println(s"Résultat de la Tondeuse n°$tond : ${tondeuse_bis.X} ${tondeuse_bis.Y} ${tondeuse_bis.O}")

    }
}
