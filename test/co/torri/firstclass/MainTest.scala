package co.torri.firstclass

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ScalaSpec extends FlatSpec with ShouldMatchers {
	
	it should "iterate with closures over arrays" in {
		
		var strings = Array("a", " ", "string", " ", "array")
		
		var finalString = "" 
		strings.foreach(s => finalString += s)
		finalString should be ("a string array")
	}

    it should "allow tail recursions" in {
        def factorial(x: Int) = {
            def tailfac(x: Int, t: BigInt): BigInt = if (x < 2) t else tailfac(x - 1, t * x)
            tailfac(x - 1, x)
        }

        factorial(100) should be(BigInt("93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000"))
    }
    
    it should "use call-by-value by default and call-by-name optionally" in {
    	
    	def loop: Int = loop
    	
    	def firstWithCallByValue(x: Int, y: Int) = x
    	// infinite loop: firstWithCallByValue(1, loop)

    	def firstWithCallByNameArg(x: Int, y: => Int) = x
    	//infinite loop: firstWithCallByNameArg(loop, 1)
    	firstWithCallByNameArg(1, loop) should be (1)
    }

    it should "allow functions to return functions" in {

        // sum values between an interval, after applying some function on them
        def sum1(f: Int => Int): (Int, Int) => Int = {
            def sumF(a: Int, b: Int): Int = if (a > b) 0 else f(a) + sumF(a + 1, b)
            sumF
        }

        def sum2(f: Int => Int)(a: Int, b: Int): Int = if (a > b) 0 else f(a) + sum2(f)(a + 1, b)

        def sumSquares = sum1(x => x * x)
        sumSquares(1, 5) should be(55)
        sum2(x => x * x)(1, 5) should be(55)
    }

    // Useful for creating Actor's messages
    it should "have case classes" in {
        abstract class A
        case class B(msg: String) {
            /**
             * Case classes and case objects implicitly come with implementations of methods toString, equals and hashCode
             */
            //override def equals(o) = o.isInstanceOf[B] && o.asInstanceOf[B].msg == msg
            /**
             * Case classes implicitly come with nullary accessor methods which retrieve the constructor arguments.
             */
            //def msg: String = msg
        }
        /**
         * Case classes implicitly come with a constructor function, with the same name as the class.
         */
        // def B(msg: String) = new B(String)

        var b = B("hello")

        b.isInstanceOf[B] should be(true)
        b.equals(new B("hello")) should be(true)
        b.equals(new B("bye")) should be(false)
        b.msg should be("hello")
    }

    it should "do pattern matching with case classes" in {
    	
    	class A
    	case class B(n: Int) extends A
    	case class C(s: String, n: Int) extends A
    	case class D(o: AnyRef) extends A
    	
    	def matchCase(a: A): Int = {
    		a match {
    			case B(-1) => 1
    			case B(n)  => 2 * n
    			case C(s, n)  => 3 * s.length + n
    			case D(_)  => 4
    		}
    	}
    	
    	matchCase(B(-1)) should be (1)
    	matchCase(B(10)) should be (20)
    	matchCase(C("txt", 5)) should be (14)
    	matchCase(D(new AnyRef)) should be (4)
    }

    it should "throw NoSuchElementException if an empty stack is popped" in {
        evaluating { throw new Exception } should produce[Exception]
    }

}
