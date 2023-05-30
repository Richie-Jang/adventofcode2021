object day23_1 {

    val input =
        """#############
          |#...........#
          |###B#C#C#B###
          |  #D#D#A#A#
          |  #########""".stripMargin

    val goal =
        """#############
          |#...........#
          |###A#B#C#D###
          |  #A#B#C#D#
          |  #########""".stripMargin

    class Room(val char: Char, val index: Int, val content: Vector[Char]) {
        def hasOnlyValidAmphipods: Boolean = content.forall(v => v == char)
        def isEmptyOrHasAllValidAmphipods: Boolean = content.forall(v => v == '.' || v == char)
        def hasAmphipodsWithWrongType: Boolean = !isEmptyOrHasAllValidAmphipods
    }

    class State(val config: Vector[Vector[Char]]) {
        override def toString: String = {
            config.map{ v =>
                v.mkString("")
            }.mkString("\n")
        }
        val hallWay = config(0)
        val rooms = config.drop(1)
        val destinationRooms = Map(
            'A' -> Room('A', 2, rooms.map{v => v(2) }),
            'B' -> Room('B', 4, rooms.map{v => v(4) }),
            'C' -> Room('C', 6, rooms.map{v => v(6) }),
            'D' -> Room('D', 8, rooms.map{v => v(8) }),
        )

        val costMap = Map(
            'A' -> 1,
            'B' -> 10,
            'C' -> 100,
            'D' -> 1000
        )
        def legalHallWayIndices = Vector(0,1,3,5,7,9,10).filter{v => hallWay(v) == '.'}
        def isFinished = destinationRooms.values.forall(v => v.hasOnlyValidAmphipods)

        // char -> index
        private def amphipodsInHallWayThatCanMove: Vector[(Char, Int)] = {
            hallWay.zipWithIndex.filter { case(c, _) =>
                c.isLetter && destinationRooms(c).isEmptyOrHasAllValidAmphipods
            }
        }

        private def roomsWithWrongAmphipods: Vector[Room] = {
            destinationRooms.values.filter(_.hasAmphipodsWithWrongType).toVector
        }

        private def hallWayPathIsClear(start: Int, _end: Int): Boolean = {
            val (a,b) = if start > _end then _end -> start else (start+1) -> (_end+1)
            hallWay.slice(a,b).forall(_ == '.')
        }

        /**
         * Vector[(State, Cost)]
        */
        def nextPossibleStates: Vector[(State, Int)] = {
            var res = Vector.empty[(State, Int)]
            for (c, idx) <- amphipodsInHallWayThatCanMove do {
                val room = destinationRooms(c)
                if hallWayPathIsClear(idx, room.index) then {
                    val y = room.content.lastIndexWhere(p => p == '.') + 1
                    val cost = (math.abs(idx - room.index) + y) * costMap(c)
                    val newConfig = config.map{v =>
                        new StringBuilder(v.mkString(""))
                    }
                    newConfig(0)(idx) = '.'
                    newConfig(y)(room.index) = c
                    res :+= (State(newConfig.map(v => v.toVector)) -> cost)
                }
            }

            for room <- roomsWithWrongAmphipods do {
                val toMoveOption = room.content.zipWithIndex.find(p => p._1 != '.')
                if toMoveOption.isDefined then {
                    for idx <- legalHallWayIndices do {
                        if hallWayPathIsClear(idx, room.index) then {
                            val y = toMoveOption.get._2 + 1
                            val cost = (math.abs(room.index - idx) + y) * costMap(toMoveOption.get._1)
                            val newConfig = config.map { v =>
                                new StringBuilder(v.mkString(""))
                            }
                            newConfig(y)(room.index) = '.'
                            newConfig(0)(idx) = toMoveOption.get._1
                            res :+= (State(newConfig.map(v => v.toVector)) -> cost)
                        }
                    }
                }
            }
            res
        }
    }

    object State {
        def from(inp: String): State = {
            val lines = inp.split("\r\n")
            new State(lines.drop(1).dropRight(1).map { v =>
                v.drop(1).dropRight(1).toVector
            }.toVector)
        }
    }

    def organizeAmphipods(initState: State): Int = {
        val goalState = State.from(goal).toString
        given Ordering[(State,Int)] = Ordering.by(a => -1 * a._2)

        val pq = scala.collection.mutable.PriorityQueue.empty[(State, Int)]
        pq.enqueue(initState -> 0)
        var memo = Map(initState.toString -> 0)

        while pq.nonEmpty do {
            val (cur, cost) = pq.dequeue()
            for n <- cur.nextPossibleStates do {
                val newCost = cost + n._2
                if memo.contains(n._1.toString) then {
                    if newCost < memo(n._1.toString) then {
                        memo = memo.updated(n._1.toString, newCost)
                        pq.enqueue(n._1 -> newCost)
                        println(n._1.toString)
                    }
                } else {
                    memo = memo.updated(n._1.toString, newCost)
                    pq.enqueue(n._1 -> newCost)
                    println(n._1.toString)
                }
            }
        }
        val res = memo.get(goalState)
        if res.isDefined then res.get else Int.MaxValue
    }


    @main def run2(): Unit = {
        val initialState = State.from(input)
        println(organizeAmphipods(initialState))
    }
}
