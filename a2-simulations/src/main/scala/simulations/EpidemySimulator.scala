package simulations

import math.random
import scala.collection.mutable

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int): Int = (random * i).toInt

  def randomFromOneTo(i: Int): Int = randomBelow(i - 1) + 1

  def biasedCoin(trueProb: Double): Boolean = trueProb <= random

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val moveWithinDays = 5

    val becomeSickDay = 6
    val maybeDieDay = 14 - becomeSickDay
    val becomeImmuneDay = 16 - maybeDieDay
    val becomeHealthyDay = 18 - becomeImmuneDay

    val prevalence = 0.01
    val transmissibilityRate = 0.4
    val deathRate = 0.25
  }

  import SimConfig._

  val persons: List[Person] = (0 to population).map(id => new Person(id)).toList

  case class Room(row: Int, col: Int) {
    private def isAdjacent(x1: Int, x2: Int, max: Int) = {
      val d = (x1 - x2).abs
      d == 1 || d == max - 1
    }

    private def isRowAdjacent(room: Room) = isAdjacent(row, room.row, roomRows)
    private def isColAdjacent(room: Room) = isAdjacent(col, room.col, roomColumns)

    def up = Room((row - 1 + roomRows) % roomRows, col)
    def down = Room((row + 1) % roomRows, col)
    def left = Room(row, (col - 1 + roomColumns) % roomColumns)
    def right = Room(row, (col + 1) % roomColumns)

    def isAdjacent(room: Room): Boolean =
      (isRowAdjacent(room) && col == room.col) || (isColAdjacent(room) && row == room.row)

    def adjacentSet: mutable.HashSet[Room] = mutable.HashSet(up, down, left, right)

    def hasInfectiousPeople = persons.exists(p => this == p.room && p.isInfectous)
  }

  class Person (val id: Int) {
    var infected: Boolean = id < (population * prevalence).toInt
    var sick: Boolean = false
    var immune: Boolean = false
    var dead: Boolean = false

    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def isInfectous: Boolean = infected || sick || immune || dead
    def isHealthy: Boolean = !isInfectous
    def canBeInfected: Boolean = isHealthy
    def isVisiblyInfected: Boolean = sick || dead

    def neighbors = persons.withFilter(p => isNeighbor(p))

    def visiblyInfectedNeighbors = neighbors.withFilter(p => p.isVisiblyInfected)

    def room: Room = Room(row, col)

    def becomeHealthy() {
      assert(infected)
      assert(sick)
      assert(immune)
      assert(!dead)
      infected = false
      sick = false
      immune = false
    }

    def becomeImmune() {
      assert(infected)
      assert(sick)
      assert(!immune)
      assert(!dead)
      immune = true
      changeState()
    }

    def maybeDie() {
      assert(infected)
      assert(sick)
      assert(!immune)
      assert(!dead)
      if (biasedCoin(deathRate)) {
        dead = true
      } else {
        afterDelay(becomeImmuneDay)(becomeImmune())
      }
    }

    def becomeSick() {
      if (!dead) {
        assert(infected)
        assert(!sick)
        assert(!immune)
        assert(!dead)
        sick = true
        changeState()
      }
    }

    def moveToRoom(room: Room) {
      row = room.row
      col = room.col
      if (canBeInfected && room.hasInfectiousPeople && biasedCoin(transmissibilityRate)) {
        infected = true
        changeState()
      }
    }

    def changeState() {
      if (!dead) {
        if (immune) {
          afterDelay(becomeHealthyDay)(becomeHealthy())
        } else if (sick) {
          afterDelay(maybeDieDay)(maybeDie())
        } else if (infected) {
          afterDelay(becomeSickDay)(becomeSick())
        }
      }
    }

    def isNeighbor(p: Person): Boolean = room.isAdjacent(p.room)

    def validAdjacentRooms = {
      val validRooms = room.adjacentSet
      visiblyInfectedNeighbors.foreach(p => validRooms.remove(p.room))
      validRooms.toList
    }

    def tryMoveToNeighboringRoom() {
      if (!dead) {
        val rooms = validAdjacentRooms
        if (!rooms.isEmpty) {
          val x = randomBelow(rooms.size)
          moveToRoom(rooms(x))
        }
        scheduleMove()
      }
    }

    def scheduleMove() {
      afterDelay(randomFromOneTo(moveWithinDays))(tryMoveToNeighboringRoom())
    }
    
    scheduleMove()
    changeState()
  }
}
