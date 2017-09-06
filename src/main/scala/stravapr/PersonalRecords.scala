/* This file is part of strava-pr.
 *
 * strava-pr is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * strava-pr is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with strava-pr.  If not, see <http://www.gnu.org/licenses/>.
 */

package stravapr

class BestTimes private (val distance: Int, bestTimes: Seq[DistanceDuration]) extends Traversable[DistanceDuration] {
  def bestTime: Option[DistanceDuration] = bestTimes.headOption

  override def foreach[U](f: (DistanceDuration) => U): Unit = bestTimes.foreach(f)
}

object BestTimes {
  def empty(distance: Int): BestTimes =
    fromRuns(Set.empty, distance, bestN = 0)

  def fromRuns(runs: Set[Run], distance: Int, bestN: Int, onlyBestOfEachRun: Boolean = false): BestTimes = {
    val bestTimes: Seq[DistanceDuration] =
      runs.flatMap(run => if (onlyBestOfEachRun) run.bestTime(distance) else run.bestTimes(distance)).toSeq.sorted

    new BestTimes(distance, bestTimes.take(bestN))
  }
}

class PersonalRecords private (prMap: Map[Int, BestTimes]) {
  def distances: Seq[Int] = prMap.keys.toSeq.sorted

  def bestTimes(distance: Int): BestTimes = prMap.getOrElse(distance, BestTimes.empty(distance))
}

object PersonalRecords {
  def fromRuns(runs: Set[Run], distances: Seq[Int], showNBest: Int, onlyBestOfEachRun: Boolean = false): PersonalRecords = {
    val prMap = distances.map { distance =>
      distance -> BestTimes.fromRuns(runs, distance, showNBest, onlyBestOfEachRun)
    }.toMap

    new PersonalRecords(prMap)
  }
}