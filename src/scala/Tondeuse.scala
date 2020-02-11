import java.io._
import scala.collection.mutable.ArrayBuffer

object Tondeuse{

  // Fonction permettant de lire le fichier d'instructions (disponible dans les ressources)
  def lecture(filename: String): ArrayBuffer[String] = {
    // On charge le fichier des instructions
    val url = getClass().getResource(filename)
    // On crée le buffer qui va contenir les instructions
    val buffer = new ArrayBuffer[String]()
    val in = new BufferedReader(new InputStreamReader(url.openStream))
    var inputLine = in.readLine
    // Lecture des lignes
    while (inputLine != null) {
      if (!inputLine.trim.equals("")) {
        buffer += inputLine.trim
      }
      inputLine = in.readLine
    }
    in.close
    buffer
  }

  // Fonction appliquant les règles de déplacement
  def deplacement(x: Int, y: Int, orient: String, instruction: String, lim: Array[Int]): Array[String] ={

    var y_func = y
    var x_func = x
    var orient_func = orient

    var pos_final = new Array[String](3)

    for(j <- (0 to instruction.length - 1).toList){
      if(instruction(j) == 'A'){
        if(orient_func == "N"){
          if(y_func < lim(1)){
            y_func = (y_func + 1)
          }
        }
        else if(orient_func == "E") {
          if(x_func < lim(0)){
            x_func = (x_func + 1)
          }
        }
        else if(orient_func == "S") {
          if(y_func > 0){
            y_func = (y_func- 1)
          }
        }
        else {
          if (x_func > 0) {
            x_func = (x_func - 1)
          }
        }
      }

      else if(instruction(j) == 'D'){
        if(orient_func == "N"){
          orient_func = "E"
        }
        else if(orient_func == "E"){
          orient_func = "S"
        }
        else if(orient_func == "S"){
          orient_func = "W"
        }
        else{
          orient_func = "N"
        }
      }

      else if(instruction(j) == 'G'){
        if(orient_func == "N"){
          orient_func = "W"
        }
        else if(orient_func == "W"){
          orient_func = "S"
        }
        else if(orient_func == "S"){
          orient_func = "E"
        }
        else{
          orient_func = "N"
        }
      }
    }
    pos_final(0) = x_func.toString
    pos_final(1) = y_func.toString
    pos_final(2) = orient_func
    pos_final
  }

  // Fonction main
  def main(args: Array[String]): Unit = {

    // On récupère les instructions
    val filename = "instructions.txt"
    val buffer = lecture(filename)
    
    // On récupère les limites de la pelouse
    var lim = new Array[Int](2)
    lim(0) = (buffer(0).substring(0,1)).toInt
    lim(1) = (buffer(0).substring(2)).toInt

    // Variables de stockages des résultats
    var pos_final = new Array[String](3)

    // Stockage des informations
    var x = 0
    var y = 0
    var orient = "N"
    
    // On applique les règles de déplacement à toutes les tondeuses
    for(j <- (1 to buffer.length - 1).toList){
      if(j % 2 == 1){ // 1 tondeuse a 2 lignes assignées
        x = buffer(j).substring(0,1).toInt
        y = buffer(j).substring(2,3).toInt
        orient = buffer(j).substring(4)
        pos_final = deplacement(x, y, orient, buffer(j+1), lim )
        // Affichage des résultats
        println(pos_final(0))
        println(pos_final(1))
        println(pos_final(2))
      }
    }
  }
}
