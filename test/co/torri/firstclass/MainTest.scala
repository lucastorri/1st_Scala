package co.torri.firstclass

import java.io.IOException
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ScalaSpec extends FlatSpec with ShouldMatchers {
    //based on: http://www.scala-lang.org/docu/files/ScalaByExample.pdf

    it should "iterate with closures over arrays" in {

        var strings = Array("a", " ", "string", " ", "array")

        var finalString = ""
        strings.foreach(s => finalString += s)
        finalString should be("a string array")
    }

    it should "have object declarations" in {

        object NewObject

        (NewObject) should be(NewObject)
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
        firstWithCallByNameArg(1, loop) should be(1)
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
                case B(n) => 2 * n
                case C(s, n) => 3 * s.length + n
                case D(_) => 4
            }
        }

        matchCase(B(-1)) should be(1)
        matchCase(B(10)) should be(20)
        matchCase(C("txt", 5)) should be(14)
        matchCase(D(new AnyRef)) should be(4)
    }

    it should "inference local variables when using generic methods" in {

        def genericMethod[T](t: T) = t

        var o = "string"
        genericMethod[String](o) should be(o)
        genericMethod(o) should be(o)
    }

    it should "have subtypes in generics" in {

        abstract class A
        case class B extends A
        class Z[T <: A](a: T) {
            def getA: T = a
        }

        var b = new B
        var z = new Z[B](b)
        z.getA should be(b)

        /**
         * View bounds <% are weaker than plain bounds <:: A view bounded type parameter
         * clause [A <% T] only speciÞes that the bounded type A must be convertible to the
         * bound type T, using an implicit conversion.
         */
    }

    //take a better look on 8.2 Variance Annotation and 8.3 Lower Bounds

    it should "have tuples" in {
        var tuple2 = (true, false)
        tuple2 should be(new Tuple2[Boolean, Boolean](true, false))
        tuple2.getClass.getSimpleName should be("Tuple2")

        var tuple4 = (1, 3.2, "text", true)
        tuple4 should be(new Tuple4[Int, Double, String, Boolean](1, 3.2, "text", true))
        tuple4.getClass.getSimpleName should be("Tuple4")

        //max tested tuple size possible: 22 => new Tuple22[Int, ...]
        
        var (x,y) = (1,2)
        x should be (1)
        y should be (2)
        
        (1,2,3) match {
        	case (i,j,k) => {
        		i should be (1)
        		j should be (2)
        		k should be (3)
        	}
        	//unreachable code: case _ => error("should not happen")
        }
    }

    it should "have functions and closures" in {

        var times: ((Int, Int) => Int) = (a, b) => a * b

        times(4, 5) should be(20)

        var times7: (Int => Int) = times(_, 7)

        times7(5) should be(35)

        var times9 = (x: Int) => times(x, 9)

        times9(10) should be(90)
        
        object SomeFunc extends Function1[Int, String] {
        	def apply(i) = i.toString
        }
        SomeFunc.apply(13) should be ("13")
        
        def callblock[T](v: => T	) = v
        callblock {
        	var x = 3
        	var y = 7
        	x * y
        } should be (21)
        (callblock {}).isInstanceOf[Unit] should be (true) 
    }

    it should "have lists" in {

        var emptyList = Nil
        emptyList should be(List())

        var stringList = "Crazy way" :: "to create" :: "a list" :: Nil
        stringList should be(List("Crazy way", "to create", "a list"))

        var intList = 1 :: 2 :: 3 :: Nil
        intList.head should be(1)
        intList.tail should be(2 :: 3 :: Nil)

        var anotherWayToCreateTheIntList = 1 :: List(2, 3)
        intList should be(anotherWayToCreateTheIntList)

        List(1, 2, 3) ::: List(4, 5, 6) should be(List(1, 2, 3, 4, 5, 6))
    }

    it should "match case lists" in {
        def isEmpty(l: List[Any]): Boolean = l match {
            case Nil => true
            case x :: xs => false
        }

        isEmpty(List()) should be(true)
        isEmpty(List(1)) should be(false)

        def matchFullList(l: List[Int]) = l match {
            case (1 :: 2 :: 3 :: Nil) => 1
            case (3 :: tail) => 2
            case (_ :: Nil) => 3
            case _ => 4
        }

        matchFullList(List(1, 2, 3)) should be(1)
        matchFullList(List(3)) should be(2)
        matchFullList(List(3, 2)) should be(2)
        matchFullList(List(3, 2, 1)) should be(2)
        matchFullList(List(4)) should be(3)
        matchFullList(List(5)) should be(3)
        matchFullList(List()) should be(4)
        matchFullList(List(1, 2)) should be(4)
        matchFullList(List(1, 3, 2)) should be(4)

        def last[T](l: List[T]): T = l match {
            case Nil => error("Nil.last")
            case e :: Nil => e
            case _ :: tail => last(tail)
        }

        evaluating { last(List()) } should produce[RuntimeException]
        last(List(1)) should be(1)
        last(List(1, 2)) should be(2)
        last(List(1, 2, 3, 4, 5, 6)) should be(6)
    }

    it should "have for loops iterating over lists" in {

        case class Person(age: Int)
        var persons = Person(10) :: Person(20) :: Person(30) :: Person(40) :: Nil

        (for (p <- persons) yield p) should be(persons)

        (for (p <- persons if p.age > 20) yield p) should be(Person(30) :: Person(40) :: Nil)

        (for (p <- persons if p.age == 20) yield p) should be(Person(20) :: Nil)

        var yieldedTuples = for {
            i <- List.range(0, 2)
            j <- List.range(3, 5)
        } yield (i, j)
        yieldedTuples should be((0, 3) :: (0, 4) :: (1, 3) :: (1, 4) :: Nil)

        def isOdd(x: Int) = x % 2 == 1
        yieldedTuples = for (i <- List.range(0, 2); j <- List.range(3, 5); if isOdd(i + j)) yield (i, j)
        yieldedTuples should be((0, 3) :: (1, 4) :: Nil)

        /*
    	 * Or, to Þnd the names of all authors that have written at least two books in the database:
		 * for (b1 <- books; b2 <- books if b1 != b2;
		 * 		a1 <- b1.authors; a2 <- b2.authors if a1 == a2) yield a1
    	 */
        
        (for (i <- (1 until 10) if i % 2 == 0) yield i) should be (Vector(2, 4, 6, 8))
    }

    it should "use underscore to initialize variables with default values" in {

        object DefaultValues {
            var a: AnyRef = _
            var s: String = _
            var b: Boolean = _
            var i: Int = _
            var d: Double = _
        }

        DefaultValues.a should be (null)
        DefaultValues.s should be (null)
        DefaultValues.b should be (false)
        DefaultValues.i should be (0)
        DefaultValues.d should be (0.0)
    }
    
    it should "have lazy values" in {

    	case object NormalVal {
    		var multiplyFactor = 5
    		val v = 10 * multiplyFactor
    	}
    	NormalVal.multiplyFactor should be (5)
    	NormalVal.multiplyFactor = 7
    	NormalVal.v should be (50)
    	
    	case object LazyVal {
    		var multiplyFactor = 5
    		lazy val v = 10 * multiplyFactor
    	}
    	LazyVal.multiplyFactor should be (5)
    	LazyVal.multiplyFactor = 7
    	LazyVal.v should be (70)
    }
    
    it should "have multiline strings" in {
    	
    	var multilineString = """This
is a
multiline string"""
    	multilineString should be ("This\nis a\nmultiline string")
    }

    /**
     * Since Scala has no checked exceptions, Scala methods must be annotated with one or more throws annotations such that Java code can catch exceptions thrown by a Scala method.
     */
    
    it should "have compound types" in {
    	trait A
    	trait B
    	trait C
    	type T = A with B with C
    	
    	class D extends A with B with C
    	//does not work: class E extends T
    	
    	var d = new D
    	d.isInstanceOf[T] should be (true)
    	
    	def someMethodWithTypeT(t: T) = t
    	
    	someMethodWithTypeT(d) should be (d)
    }

}
//http://stackoverflow.com/questions/2335319/what-are-the-relationships-between-any-anyval-anyref-object-and-how-do-they-ma
