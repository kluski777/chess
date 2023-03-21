abstract class piece {
  def move(xy: (Int, Int), takenPos: List[(Int, Int, Boolean)]): Boolean

  def decorum(pos: (Int, Int)): Boolean = pos._1 < 8 && pos._2 < 8 && pos._1 >= 0 && pos._2 >= 0

  def get: (Int, Int)

  def rozstawienie: Unit = println("Position of: " + this + " " + Uni.encrypt(this.get))
}

case class Horse(var pos: (Int, Int)) extends piece {
  def move(xy: (Int, Int), takenPos: List[(Int, Int, Boolean)]): Boolean = {
    if (super.decorum(xy) && (xy._1 - pos._1).abs + (xy._2 - pos._2).abs == 3 && !takenPos.contains((xy._1, xy._2, true))) { // zmiana pozycji o tyle
      pos = xy
      true
    }
    else
      false
  }
  def get: (Int,Int) = (pos._1, pos._2)
  override def toString: String = "N"
}

case class Bishop(var pos: (Int, Int)) extends piece { //skrócić funkcję move na dole
  def move(xy: (Int, Int), takenPos: List[(Int, Int, Boolean)]): Boolean = {
    var i = 0
    if (super.decorum(xy) && (xy._1 - pos._1).abs == (xy._2 - pos._2).abs && xy._2 - pos._2 != 0) { //sprawdza czy goniec w ogóle może sie ruszyć w taką stronę
      while (i < (xy._1 - pos._1).abs) {
        i += 1
        if (xy._1 - pos._1 < 0 && xy._2 - pos._2 > 0 && (takenPos.contains((pos._1 - i, pos._2 + i, true)) || (takenPos.contains((pos._1 - i, pos._2 + i, false)) && i < (xy._1 - pos._1).abs))) //ruch goniec lewo góra
          return false
        else if (xy._1 - pos._1 > 0 && xy._2 - pos._2 > 0 && (takenPos.contains((pos._1 + i, pos._2 + i, true)) || (takenPos.contains((pos._1 + i, pos._2 + i, false)) && i < (xy._1 - pos._1).abs))) // ruch goniec góra prawo
          return false
        else if (xy._1 - pos._1 > 0 && xy._2 - pos._2 < 0 && (takenPos.contains((pos._1 + i, pos._2 - i, true)) || (takenPos.contains((pos._1 + i, pos._2 - i, false)) && i < (xy._1 - pos._1).abs))) // ruch goniec prawo dół
          return false
        else if (xy._1 - pos._1 < 0 && xy._2 - pos._2 < 0 && (takenPos.contains((pos._1 - i, pos._2 - i, true)) || (takenPos.contains((pos._1 - i, pos._2 - i, false)) && i != (xy._1 - pos._1).abs))) //ruch goniec lewo dół
          return false
      }
      pos = xy
      return true
    }
    false
  }

  def get: (Int,Int) = (pos._1, pos._2)

  override def toString: String = "B"
}

case class Pawn(var pos: (Int, Int), pawnMove: ((Int, Int), List[(Int, Int, Boolean)], Int) => Boolean, var enPassant: List[(Int, Int)] = List()) extends piece {
  def move(xy: (Int, Int), takenPos: List[(Int, Int, Boolean)]): Boolean = {
    if (pawnMove((xy._1 - pos._1, xy._2 - pos._2), takenPos.map(f => (f._1 - pos._1, f._2 - pos._2, f._3)).filter(a => (a._2 >= (-2) && a._2 <= 2 && a._1 == 0) || a._1.abs == 1), pos._2) && super.decorum(xy)) {
      pos = xy
      println("New position: " + pos.toString + "\n")
      return true
    }
    if (enPassant.nonEmpty && enPassant.head._1 == xy._1 && enPassant.head._2 == xy._2) {
      pos = xy
      println("New position " + pos.toString + "\n")
      enPassant = enPassant.tail
      return true
    }
    false
  }

  def get:(Int,Int) = (pos._1, pos._2)

  override def toString: String = ""
}

case class Rook(var pos: (Int, Int), var didTheRookMove: List[(Int, Int)]) extends piece { //drugi argument jest potrzebny do zrobienia roszady
  def move(xy: (Int, Int), takenPos: List[(Int, Int, Boolean)]): Boolean = {
    if (super.decorum(xy)) {
      if (xy._2 - pos._2 == 0) { // poziomy ruch
        for (i <- 1 to (xy._1 - pos._1).abs) {
          if (xy._1 - pos._1 > 0 && (takenPos.contains((pos._1 + i, pos._2, true)) || (takenPos.contains((pos._1 + i, pos._2, false)) && i != (xy._1 - pos._1).abs))) // wieża ruch w prawo
            return false
          else if ((xy._1 - pos._1 < 0 && takenPos.contains((pos._1 - i, pos._2, true))) || (takenPos.contains((pos._1 - i, pos._2, false)) && i != (xy._1 - pos._1).abs)) // wieża ruch w lewo
            return false
        }
      }
      else if (xy._1 - pos._1 == 0) { // pionowy ruch
        for (i <- 1 to (xy._2 - pos._2).abs) {
          if (xy._2 - pos._2 > 0 && (takenPos.contains((pos._1, pos._2 + i, true)) || (takenPos.contains((pos._1, pos._2 + i, false)) && i != (xy._2 - pos._2).abs))) // wieża ruch do góry
            return false
          else if (xy._2 - pos._2 < 0 && (takenPos.contains((pos._1, pos._2 - i, true)) || (takenPos.contains((pos._1, pos._2 - i, false)) && i < (xy._2 - pos._2).abs))) // wieża ruch do dołu
            return false
        }
      }
      didTheRookMove = didTheRookMove ++ List(xy)
      pos = xy
      true
    }
    else if (takenPos.isEmpty) { //tj. do roszady
      pos = xy
      true
    }
    else
      false
  }

  def get: (Int,Int) = (pos._1, pos._2)

  override def toString: String = "R"
}

case class Queen(var pos: (Int, Int)) extends piece {
  def move(xy: (Int, Int), takenPos: List[(Int, Int, Boolean)]): Boolean = {
    if (super.decorum(xy) && xy != pos) {
      if (xy._2 - pos._2 == 0) { // poziomy ruch
        for (i <- 1 to (xy._1 - pos._1).abs) {
          if (xy._1 - pos._1 > 0 && (takenPos.contains((pos._1 + i, pos._2, true)) || (takenPos.contains((pos._1 + i, pos._2, false)) && i != (xy._1 - pos._1).abs))) // wieża ruch w prawo
            return false
          else if (xy._1 - pos._1 < 0 && (takenPos.contains((pos._1 - i, pos._2, true)) || (takenPos.contains((pos._1 - i, pos._2, false)) && i != (xy._1 - pos._1).abs))) // wieża ruch w lewo
            return false
        }
      }
      else if (xy._1 - pos._1 == 0) { // pionowy ruch
        for (i <- 1 to (xy._2 - pos._2).abs) {
          if (xy._2 - pos._2 > 0 && (takenPos.contains((pos._1, pos._2 + i, true)) || (takenPos.contains((pos._1, pos._2 + i, false)) && i != (xy._2 - pos._2).abs))) // wieża ruch do góry
            return false
          else if (xy._2 - pos._2 < 0 && (takenPos.contains((pos._1, pos._2 - i, true)) || (takenPos.contains((pos._1, pos._2 - i, false)) && i < (xy._2 - pos._2).abs))) // wieża ruch do dołu
            return false
        }
      }
      else if ((xy._1 - pos._1).abs == (xy._2 - pos._2).abs) {
        var i = 0
        while (i < (xy._1 - pos._1).abs) {
          i += 1
          if (xy._1 - pos._1 < 0 && xy._2 - pos._2 > 0 && (takenPos.contains((pos._1 - i, pos._2 + i, true)) || (takenPos.contains((pos._1 - i, pos._2 + i, false)) && i < (xy._1 - pos._1).abs))) //ruch goniec lewo góra
            return false
          else if (xy._1 - pos._1 > 0 && xy._2 - pos._2 > 0 && (takenPos.contains((pos._1 + i, pos._2 + i, true)) || (takenPos.contains((pos._1 + i, pos._2 + i, false)) && i < (xy._1 - pos._1).abs))) // ruch goniec góra prawo
            return false
          else if (xy._1 - pos._1 > 0 && xy._2 - pos._2 < 0 && (takenPos.contains((pos._1 + i, pos._2 - i, true)) || (takenPos.contains((pos._1 + i, pos._2 - i, false)) && i < (xy._1 - pos._1).abs))) // ruch goniec prawo dół
            return false
          else if (xy._1 - pos._1 < 0 && xy._2 - pos._2 < 0 && (takenPos.contains((pos._1 - i, pos._2 - i, true)) || (takenPos.contains((pos._1 - i, pos._2 - i, false)) && i < (xy._1 - pos._1).abs))) //ruch goniec lewo dół
            return false
        }
      }
      pos = xy
      true
    }
    else
      false
  }

  def get: (Int,Int) = (pos._1, pos._2)

  override def toString: String = "Q"
}

case class King(var pos: (Int, Int), var canTheKingCastle: List[(Int, Int)] = List()) extends piece {
  override def toString: String = "K"

  def move(xy: (Int, Int), takenPos: List[(Int, Int, Boolean)]): Boolean = { // w tym wypadku takenPos to pozycje ktore przeciwna strona atakuje, ustawienie figur mnie nie interere [jeszcze nie zaimplementowane]
    if (super.decorum(xy) && !takenPos.map{f=>(f._1,f._2)}.contains(xy) && ((xy._1 - pos._1).abs == 1 || (xy._2 - pos._2).abs == 1)) { // spoko tylko trzeba ruszyć i krola i wieżę
      pos = xy
      return true
    }
    else if (super.decorum(xy) && canTheKingCastle.contains(xy)) {
      pos = xy
      return true
    }
    false
  }

  def get: (Int,Int) = (pos._1, pos._2)
}

/*
Potencjalnie
podzielenie na kilka fileów
GUI / stronka
*/
//nie przelatywanie ale tak zeby do movea ne przesylac poaycji czarnego (da sie tak?) && zmiana contain w color z var na val (też sie tak da?)
// Roszada
// Jeszcze implementacja szacha i odpowiedź na szacha (to może być trudne)
// brakuje zegara
object Main {
  def main(args: Array[String]): Unit = {
    Uni.game("Standard") // W domyśle miały być różne typy gry - tj. standard, Fischer random itd.
  }
}

abstract class color {
  var contain: List[piece]

  def check: Unit

  def searchToRemove(coords: (Int, Int)): color = {
    val temp: (Boolean, Int) = this.search(coords)
    if (temp._1) {
      this match {
        case _: Black => new Black(contain.updated(temp._2, this.contain.head).tail)
        case _: White => new White(contain.updated(temp._2, this.contain.head).tail)
        case _ => throw new Exception("Da fuq is the type in searchToRemove function?")
      }
    }
    else
      this
  }

  def IsKingAlive: Boolean = this.contain.exists(_.toString == "K")

  def search(coord: (Int, Int)): (Boolean, Int) = {
    for (i <- this.contain.indices) {
      if (this.contain(i).get == coord)
        return (true, i)
    }
    (false, 0)
  }

  def getFromTheInside(x: piece): List[(Int, Int)] = { //jeszcze wieza
    x match {
      case King(_, lista) => lista
      case Pawn(_, _, enPassantList) => enPassantList
      case Rook(_, ruchy) => ruchy
      case _ => List()
    }
  }

  def checkCastling(PGN: List[String], takenPos: List[(Int, Int)]): Boolean

  def toWhite: White = new White(contain)

  def toBlack: Black = new Black(contain)

  def enPassantCheck(PGN: List[String]): Unit
}

class White extends color {
  var contain: List[piece] = this.placing // jak to zamienic na val'a zeby wszystko dzialalo?

  def this(newlist: List[piece]) = {
    this()
    contain = newlist
  }

  def pawnMove(changeOfPos: (Int, Int), taken: List[(Int, Int, Boolean)], row: Int): Boolean = {
    if (changeOfPos == (0, 2) && row == 1 && !taken.exists{smth:(Int,Int,Boolean) => smth._2 == 1 && smth._2 == 2}) //ruch o 2 pola
      return true
    else if (changeOfPos == (0, 1) && !taken.exists{f:(Int,Int,Boolean) => f._1 == 0 && f._2 == 1}) //zwykly ruch do przodu
      return true
    else if (changeOfPos == (-1, 1) && taken.contains((-1,1,false))) // bicie w lewo
      return true
    else if (changeOfPos == (1, 1) && taken.contains((1,1,false))) // bicie w prawo
      return true
    false
  } // r to rzad liczony od zera potrzebny tuple 2D, moznaby zrobic w 1 ifie ale nie wiem czy to byloby optymalne rozwiazanie

  def check: Unit = {
    for (i <- this.contain.indices)
      println(i + 1 + "-th white element is: " + contain(i) + ", and it's on " + Uni.encrypt(contain(i).get) + " square")
  }

  def placing: List[piece] = {
    var go: List[piece] = List()
    for (i <- 0 until 8)
      go = go ++ List(Pawn((i, 1), pawnMove _))
    for (i <- 0 to 1) {
      go = go ++ List(Bishop((i * 3 + 2, 0)))
      go = go ++ List(Horse((i * 5 + 1, 0)))
      go = go ++ List(Rook((i * 7, 0), List((i * 7, 0))))
    }
    go = go ++ List(Queen((3, 0)))
    go = go ++ List(King((4, 0)))
    go
  }

  def checkCastling(PGN: List[String], takenPos: List[(Int, Int)]): Boolean = { //możnaby defaultowo dać opcje roszowania i zabierać ją w forze -> to by skróciło kod
    val kingIndex: Int = search(contain.filter(_.toString == "K").head.get)._2 // indeks krola jak sama nazwa wskazuje
    var didKingMove: Boolean = false
    val rook = this.contain.filter(_.toString == "R")
    if (rook.nonEmpty) {
      for (i <- 0 until PGN.length/3) {
        if (PGN(3 * i + 1).head == 'K' || PGN(3 * i + 1).head == 'O') // jak odseparować ruchy czarnego od białego
          didKingMove = true
      }
      println("Info dotyczące białych wieży")
      if (!didKingMove && rook.exists(f => getFromTheInside(f).head == (7, 0) && getFromTheInside(f).length == 1) && !takenPos.exists(f => f == (5, 0) || f == (6, 0)))
        contain = contain.updated(kingIndex, King(contain(kingIndex).get, getFromTheInside(contain(kingIndex)) ++ List((6, 0))))
      else
        contain = contain.updated(kingIndex, King(contain(kingIndex).get, getFromTheInside(contain(kingIndex)).filterNot { f: (Int, Int) => f == (6, 0) }))
      if (!didKingMove && rook.exists(f => getFromTheInside(f).head == (0, 0) && getFromTheInside(f).length == 1) && !takenPos.exists(f => f == (1, 0) || f == (2, 0) || f == (3, 0))) // roszada w drugą stronę
        contain = contain.updated(kingIndex, King(contain(kingIndex).get, getFromTheInside(contain(kingIndex)) ++ List((2, 0))))
      else
        contain = contain.updated(kingIndex, King(contain(kingIndex).get, getFromTheInside(contain(kingIndex)).filterNot { f: (Int, Int) => f == (2, 0) }))
      !didKingMove
    }
    else
      didKingMove
  }

  def enPassantCheck(PGN: List[String]): Unit = {
    val firstElem = PGN.reverse.head
    if (firstElem.tail.head == '5' && !PGN.exists(f => f.length == 2 && f.tail != "." && f.head == firstElem.head && f.reverse.head == '6')) {
      val pawnsNextTo = contain.filter(_.toString == "").map(_.get).filter(b => b._2 == 4 && (b._1 == Uni.decrypt(firstElem)._1 + 1 || b._1 == Uni.decrypt(firstElem)._1 - 1)).map(f => (f._1, f._2)) //wsp. pionka ktory moze bic w przelocie, zawsze jest jeden do bicia w przelocie
      if (pawnsNextTo.nonEmpty) {
        val whichPawn = search(pawnsNextTo.head)._2
        this.contain = contain.updated(whichPawn, Pawn(contain(whichPawn).get, pawnMove, List((Uni.decrypt(firstElem)._1, Uni.decrypt(firstElem)._2 + 1)))) //jak to napisac bez 2 encryptow???
      }
    }
  }
}

class Black extends color {
  var contain: List[piece] = this.placing

  def pawnMove(changeOfPos: (Int, Int), taken: List[(Int, Int, Boolean)], row: Int): Boolean = {
    if (changeOfPos._2 == (-2) && row == 6 && !taken.exists(f => (f._2 == -1 || f._2 == -2) && f._1 == 0))
      return true
    else if (changeOfPos == (0, -1) && !taken.exists(f => f._2 == -1 && f._1 == 0)) //zwykly ruch do dolu
      return true
    else if (changeOfPos == (-1, -1) && taken.contains((-1, -1, false))) //bicie w prawo
      return true
    else if (changeOfPos == (1, -1) && taken.contains((1, -1, false))) // bicie w lewo
      return true
    false
  } // r to rzad liczony od zera, moznaby zrobic w 1 ifie ale nie wiem czy to byloby optymalne rozwiazanie

  def this(newlist: List[piece]) = {
    this()
    contain = newlist
  }

  def check: Unit = {

    for (i <- this.contain.indices)
      println(i + 1 + "-th black element is: " + contain(i) + ", and it's on " + Uni.encrypt(contain(i).get) + " square")
  }

  def placing: List[piece] = {
    var go: List[piece] = List()
    for (i <- 0 until 8)
      go = go ++ List(Pawn((i, 6), pawnMove _)) // Intellij automatycznie dodaje factory method's
    for (i <- 0 to 1) {
      go = go ++ List(Bishop((i * 3 + 2, 7)))
      go = go ++ List(Horse((i * 5 + 1, 7)))
      go = go ++ List(Rook((i * 7, 7), List((i * 7, 7))))
    }
    go = go ++ List(Queen((3, 7)))
    go = go ++ List(King((4, 7)))
    go
  }

  def checkCastling(PGN: List[String], takenPos: List[(Int, Int)]): Boolean = { //możnaby defaultowo dać opcje roszowania i zabierać ją w forze -> to by skróciło kod
    val kingIndex: Int = search(contain.filter(_.toString == "K").head.get)._2 // indeks krola jak sama nazwa wskazuje
    var didKingMove: Boolean = false
    val rook = contain.filter(_.toString == "R").filter(f=>getFromTheInside(f).head == (7, 7) || getFromTheInside(f).head == (0,7))
    if (rook.nonEmpty){
      for (i <- 0 until PGN.length/3){
        if(PGN(3*i + 2).head == 'K' || PGN(3*i + 2).head == 'O') // jak odseparować ruchy czarnego od białego
          didKingMove = true
      }
      if(!didKingMove && rook.exists(f=>getFromTheInside(f).head == (7,7) && getFromTheInside(f).length == 1) && !takenPos.exists(f => f == (5,7) || f == (6,7)))
        contain = contain.updated(kingIndex, King(contain(kingIndex).get, getFromTheInside(contain(kingIndex)) ++ List((6,7))))
      else
        contain = contain.updated(kingIndex, King(contain(kingIndex).get, getFromTheInside(contain(kingIndex)).filterNot{f:(Int, Int)=>f==(6,7)}))
      if(!didKingMove && rook.exists(f=>getFromTheInside(f).head == (0,7) && getFromTheInside(f).length == 1) && !takenPos.exists(f => f == (1,7) || f == (2,7) || f == (3,7))) // roszada w drugą stronę
        contain = contain.updated(kingIndex, King(contain(kingIndex).get, getFromTheInside(contain(kingIndex)) ++ List((2, 7))))
      else
        contain = contain.updated(kingIndex, King(contain(kingIndex).get, getFromTheInside(contain(kingIndex)).filterNot{f:(Int,Int)=> f == (2,7)}))
      !didKingMove
    }
    else
      didKingMove
  }

  def enPassantCheck(PGN: List[String]): Unit = {
    val firstElem = PGN.reverse.head
    if (firstElem.tail(0) == '4' && !PGN.exists(f => f.length == 2 && f.tail != "." && f.head == firstElem.head && f.reverse.head == '3')) {
      val pawnsNextTo = contain.filter(_.toString == "").map(_.get).filter(b => b._2 == 3 && (b._1 == Uni.decrypt(firstElem)._1 + 1 || b._1 == Uni.decrypt(firstElem)._1 - 1)).map(f => (f._1, f._2)) //wsp. pionka ktory moze bic w przelocie, zawsze jest jeden do bicia w przelocie
      if (pawnsNextTo.nonEmpty) {
        val whichPawn = search(pawnsNextTo.head)._2
        this.contain = this.contain.updated(whichPawn, Pawn(contain(whichPawn).get, pawnMove, List((Uni.decrypt(firstElem)._1, Uni.decrypt(firstElem)._2 - 1)))) //jak to napisac bez 2 decryptow??? Dobre pytanie
      }
    }
  }
}

//czarnemu roszada dziala ale źle się zapisuje -> wieża się nie rusza
//bialemu nie dziala

object Uni { // pokomplikowane to jak nie wiem co
  def game(typ: String):Unit={ // typu się później użyje przy rozpoczynaniu gry w Fischer Random
    var biale: White = new White
    var czarne: Black = new Black
    var PGN: List[String] = List()
    import scala.io.StdIn.readLine
    var canCastle = false
    var i = 1
    while (biale.IsKingAlive && czarne.IsKingAlive) {
      PGN = PGN :+ (i.toString + ".")
      var whichOne = (false, 0) // na pewno da sie to zmienic na 2 oddzielne zmienne ale nie wiem jak to zaimplementowac
      while (!whichOne._1){
        println("White can make " + i + "-th move, insert the square on which piece you wanna move is located")
        whichOne = biale.search(decrypt(readLine()))
        if (whichOne._1) {
          println("On which square do you wanna move?")
          whichOne = (biale.contain(whichOne._2).move(decrypt(readLine()), biale.contain.map(_.get).map{f=>(f._1, f._2, true)} ++ czarne.contain.map(_.get).map{f=>(f._1, f._2, false)}), whichOne._2)
          czarne = czarne.searchToRemove(biale.contain(whichOne._2).get).toBlack
        }
      }
      if(canCastle && biale.contain.exists{f:piece => f.toString == "K" && f.get == (6,0)}) { // sprawdzenie czy figury x zrobiły roszadę
        PGN = PGN :+ "O-O"
        biale.contain.filter(_.get == (7,0)).head.move((5,0),List())
      }
      else if (canCastle && czarne.contain.exists { f: piece => f.toString == "K" && f.get == (6, 0) }) { // sprawdzenie czy figury x zrobiły roszadę
        PGN = PGN :+ "O-O-O"
        biale.contain.filter(_.get == (7, 0)).head.move((5, 0), List())
      }
      else if(i > 1 && PGN.reverse.tail.head.length == 2 && (biale.contain(whichOne._2).get._1, biale.contain(whichOne._2).get._2 - 1) == decrypt(PGN.reverse.tail.head)) {
        PGN = PGN :+ ("ex" + (biale.contain(whichOne._2) + Uni.encrypt(biale.contain(whichOne._2).get))) //zapisuje ruch en passant i wybija pionka
        czarne = czarne.searchToRemove((biale.contain(whichOne._2).get._1, biale.contain(whichOne._2).get._1 - 1)).toBlack
      }
      else
        PGN = PGN :+ (biale.contain(whichOne._2) + Uni.encrypt(biale.contain(whichOne._2).get))
      canCastle = czarne.checkCastling(PGN, czarne.contain.map(_.get) ++ biale.contain.map(_.get))
      czarne.enPassantCheck(PGN) //oczywiście zmienić pola na których stoją czarne na te które atakują, TYLKO JEDEN KOLOR WYSTARCZY PRZESŁAĆ
      println("PGN: " + PGN.reduce(_ + " " + _))
      whichOne = (!whichOne._1, whichOne._2) // da sie to jakos inaczej zastapic na 100%
      if (!czarne.IsKingAlive)
        whichOne = (true, whichOne._2)
      while (!whichOne._1) {
        println("Black can make " + i + " move, insert the square on which piece you wanna move is located")
        whichOne = czarne.search(decrypt(readLine()))
        if (whichOne._1) {
          println("On which square do you wanna move?")
          whichOne = (czarne.contain(whichOne._2).move(decrypt(readLine()), biale.contain.map(_.get).map { f => (f._1, f._2, false) } ++ czarne.contain.map(_.get).map { f => (f._1, f._2, true) }), whichOne._2)
          biale = biale.searchToRemove(czarne.contain(whichOne._2).get).toWhite
        }
      }
      if (canCastle && czarne.contain.exists { f: piece => f.toString == "K" && f.get == (6, 7) }) { // sprawdzenie czy figury x zrobiły roszadę
        PGN = PGN :+ "O-O"
        czarne.contain.filter(_.get == (7, 7)).head.move((5, 7), List())
      }
      else if (canCastle && czarne.contain.exists { f: piece => f.toString == "K" && f.get == (2, 7) }) { // sprawdzenie czy figury x zrobiły roszadę
        PGN = PGN :+ "O-O-O"
        czarne.contain.filter(_.get == (0, 7)).head.move((3, 7), List())
      }
      else if (PGN.last.length == 2 && (czarne.contain(whichOne._2).get._1, czarne.contain(whichOne._2).get._2 + 1) == decrypt(PGN.last)) { // nie da sie tego ifa krócej?
        PGN = PGN :+ ("ex" + (czarne.contain(whichOne._2) + Uni.encrypt(czarne.contain(whichOne._2).get)))
        biale = biale.searchToRemove((czarne.contain(whichOne._2).get._1, czarne.contain(whichOne._2).get._1 + 1)).toWhite
      }
      else
        PGN = PGN :+ (czarne.contain(whichOne._2) + Uni.encrypt(czarne.contain(whichOne._2).get))
      if (i > 2)
        biale.enPassantCheck(PGN)
      canCastle = biale.checkCastling(PGN, biale.contain.map(_.get) ++ czarne.contain.map(_.get)) // tutaj trzebaby zmienić z pól na których stoją czarne na pola które atakują czarne i na których jeszcze stoją
      println("PGN: " + PGN.reduce(_ + " " + _))
      i += 1
    }
  }

  def decrypt(arg: String): (Int, Int) = {
    if (arg.length == 2)
      return (arg.head.toInt - 97, arg.tail.toInt - 1)
    else if (arg == "O-O" || arg == "O-O-O")
      return (9, 9) // cos bez sensu, cokolwiek byle błędu nie było
    throw new Exception("Something bad happend in decrypt method")
  }

  def encrypt(x: (Int, Int)): String = {
    if (x._1 >= 0 && x._1 < 8)
      return (x._1 + 97).toChar.toString + (x._2 + 1).toString
    throw new Exception("Something bad happend in encrypt method")
  }
}
////// NA KONIEC WSZYSTKO UPROSCIC/SKROCIC KOD TAK JAK SIE DA
///// NA PEWNO DA SIE PROSCIEJ NAPISAC RUCHY FIGUR
