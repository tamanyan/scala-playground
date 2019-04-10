package example

import scala.math.{abs, max}
import scala.util.control.Breaks._

class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
}

object Solution {
    def twoSum(nums: Array[Int], target: Int): Array[Int] = {
      var m: Map[Int,Int] = Map()
      val ans: Array[Int] = Array(1, 2)

      for (i <- 0 until nums.length) {
        m = m + (nums(i) -> i)
      }

      for (i <- 0 until nums.length) {
        val complement = target - nums(i)
        
        if (m.contains(complement) && i != m(complement)) {
          return Array(i, m(complement))
        }
      }
    
      return ans
    }

    def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
      var tmp1: ListNode = l1
      var tmp2: ListNode = l2
      var ans: ListNode = new ListNode(0)
      var nextNum: Int = 0
      var tmpAns: ListNode = ans
  
      while (!(tmp1 == null && tmp2 == null && nextNum == 0)) {
        val tmp1x = if (tmp1 == null) 0 else tmp1.x
        val tmp2x = if (tmp2 == null) 0 else tmp2.x
        tmp1 = if (tmp1 == null) null else tmp1.next 
        tmp2 = if (tmp2 == null) null else tmp2.next 
        val digit = tmp1x + tmp2x + nextNum

        if (digit >= 10) {
          tmpAns.x = digit % 10
          nextNum = 1
        } else {
          tmpAns.x = digit
          nextNum = 0
        }

        if (!(tmp1 == null && tmp2 == null && nextNum == 0)) {
          tmpAns.next = new ListNode(0)
          tmpAns = tmpAns.next
        }
      }

      return ans
    }

    abstract class Notification

    case class Email(sender: String, title: String, body: String) extends Notification

    case class SMS(caller: String, message: String) extends Notification

    case class VoiceRecording(contactName: String, link: String) extends Notification

    def lengthOfLongestSubstring(s: String): Int = {
      val slist = s.toList
      var maxValue = 0

      for (i <- 0 until slist.length) {
        var m: Set[Char] = Set()
        var len = 0

        breakable {
          for (j <- i until slist.length) {
            val char = s(j)

            if (m.contains(char)) break

            m += char
            len += 1
            maxValue = max(maxValue, len)
            // println(m)
          }
        }
      }

      return maxValue
    }
}

object Hello extends Greeting with App {
  // val sol = Solution.twoSum(Array(1, 2, 3), 5)
  // println(sol.mkString(","))

  // val l1 = new ListNode(5)
  // l1.next = new ListNode(4)
  // l1.next.next = new ListNode(3)

  // val l2 = new ListNode(5)
  // l2.next = new ListNode(6)
  // l2.next.next = new ListNode(4)
  // l2.next.next.next = new ListNode(9)
  // println(Solution.addTwoNumbers(l1, l2))
  println(Solution.lengthOfLongestSubstring("abcabcbb"))
  println(Solution.lengthOfLongestSubstring("bbbbb"))
  println(Solution.lengthOfLongestSubstring("pwwkew"))
}

trait Greeting {
  lazy val greeting: String = "hello1"
}
