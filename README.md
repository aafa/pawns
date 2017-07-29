### Implement pawns that walking through 10*10 board

`Scala.js` was utilized here for UI to provide better demo experience.

See demo implementation here [https://aafa.github.io/pawns/]()


### Strategies implemented
#### Random walker
Walks through the board picking random steps from the list of available ones

#### Warnsdorf-kind walker 
Uses "visit cornered places first" heuristic with a fallback to random implementation

Tests shows near-polynomial times (`O(10*n)` is the worst case that was recorded)

#### RailwayVisitor10
Pre-calculated closed-shape path for 10*10 box that suitable for any starting point. 
Several Warnsdorf passes were used to get result of a descent quality.
Still some cells are visited several times, so there's a field to improve.

#### Improve on strategy
1. Pre-calculate closed-shaped tour (for any arbitrary box size) and re-use it
    - that visits every cell just ones
    
    
###  Run and test
- `sbt test` to run tests
- `~fastOptJS` to iterate over scala.js ui updates (see ./pawns/target/scala-2.11/classes/index-dev.html)