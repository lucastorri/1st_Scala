def f1 = {3} //returns Int
def f2 {3} //returns Unit

val nameHasUpperCase = name.exists(_.isUpperCase)

class Person() {
 // Private age variable, renamed to _age
 private var _age = 0
 var name = ""
 // Getter
 def age = _age
 // Setter
 def age_= (value:Int):Unit = _age = value
}

def f(i: Int = 0) = i.toString
f()
f(3)
f(i=3)

(1 until 5) // (1,2,3,4)
(1 to 5) // (1,2,3,4,5)
(1 until 5 by 2)
(1 to 5 by -1)

def method (args:Int*) = args.foreach (i => println(i))
val list = List(1,2,3)
method (list:_*)

val names = Array("Daniel", "Chris", "Joseph")
names.foreach(println)

val names = List("Daniel", "Chris", "Joseph")
val str = names.foldLeft("") { (acc, n) =>
  acc + ", " + n
}

import scala.collection.JavaConversions._

class Str(str: String) {
    def addABC: Str = new Str(str + "ABC")
    override def toString = str
}
implicit def stringToStr(str: String) = new Str(str)
implicit def strToString(str: Str) = str.toString
"123".addABC.toLowerCase

import java.util.Calendar
import java.util.Date
class IntPlus(i: Int) {
    def weeks = i * 7
    def ago: Date = {
        val cal = Calendar.getInstance
        cal.add(Calendar.DATE, -i)
        cal.getTime
    }
}
implicit def intToIntPlus(i: Int) = new IntPlus(i)
2.weeks.ago

trait Friendly {
  def greet() = "Hi"
}
class Dog extends Friendly {
  override def greet() = "Woof"
}
trait ExclamatoryGreeter extends Friendly {
  override def greet() = super.greet() + "!"
}
var pet: Friendly = new Dog
println(pet.greet())
pet = new Dog with ExclamatoryGreeter
println(pet.greet())

object Functional {
    class PipedObject[T] private[Functional] (value:T) { //private constructor in the given scope
        def |>[R] (f : T => R) = f(this.value)
    }
    implicit def toPiped[T] (value:T) = new PipedObject[T](value)
}
import Functional._
def f(x:Int) = ( //or even without the def and placing the x value
    x
    |> (2 * _)
    |> (_ + 5)
    |> println
)

object Neighbour {
    def unapply(tuple: (Int,Int)): Option[(Int,Int)] = {
        if (Math.abs(tuple._1 - 1) < 2 && Math.abs(tuple._2 - 1) < 2) Some(tuple._1, tuple._2)
        else None
    }
}
def f(p: (Int,Int)): String = p match {
    case Neighbour(i,j) => "sim: (" + i + ", " + j + ")"
    case _ => "nao"
}
println(f((1,2)))
println(f((0,0)))

//duck typing: http://scala.sygneca.com/patterns/duck-typing-done-right
type HasMultX = {       
    def multX(i: Int): Int
}
def value10(o: HasMultX) = o.multX(10)
value10(new {def multX(i:Int) = i * 10})

var * = 3
* * *

"""multi
line
string
for
Scala"""

exit

'symbol

var `valid identifier` = 3
`valid identifier` * 3
Java.net.Proxy.‵type‵() //type is a reserved scala word


var l = List("lucas", "ulcas", "ulcsa", "lcaus")
for (e <- l; el = e.toUpperCase; if el.startsWith("L"); if el.endsWith("S")) println(e)


List(1,2,3,4).foreach { e =>
    e match {
        case i if i < 3 => println(i.toString + " is smaller than 3")
        case i => println(i.toString + " is bigger than or equal 3")
    }
}


val BikeRE = """Bike:\s*\{\s*owner="([\w\s]*)"\s*\}""".r
val CarRE = """Car:\s*\{\s*owner="([\w\s]*)"\s*doors="(\d)*"\s*\}""".r
List (
    "Bike: { owner=\"Lucas\" }",
    "Bike: { owner=\"Bruno\" }",
    "Car:  { owner=\"Paulo\" doors=\"4\" }",
    "Car:  { owner=\"Nice\" doors=\"7\" }",
    "Car:  { owner=\"Isa\" }"
) foreach { s =>
    println(s match {
        case BikeRE("Lucas") => "hello Lucas!"
        case BikeRE(otherOwner) => "welcome " + otherOwner
        case CarRE(owner, doors) if Integer.parseInt(doors) <= 4 => "nice car, " + owner
        case CarRE(owner, doors) => "crazy car, dude"
        case _ => s + " didn't matched."
    })
}



var RE = """^a.*""".r
"abcde" match { case RE() => 'Y'; case _ => 'N' }



case class A(i: Int)
case class B(i: Int, a: A)
List(B(3, A(2)), B(-1, A(0))).foreach { b =>
    println(b match {
        case B(-1, a @ A(0)) => "matched"
        case B(bi, a @ A(ai)) => "not matched " + bi + " " + ai
    })
}


object SouthStatesCapitals extends Enumeration {
    val RS = Value("Porto Alegre")
    val SC = Value("Florianopolis")
    val PR = Value("Curitiba")
}
for (state <- SouthStatesCapitals) println(state.id + "\t" + state)


class Z(i: Int) {
    require (i > 0, "it's smaller than 0")
}
try {
    new Z(-1)
} catch {
    case e => e.getMessage
}
new Z(1)


trait T1 {
  println( "  in T1: x = " + x )
  val x=1
  println( "  in T1: x = " + x )
}
trait T2 {
  println( "  in T2: y = " + y )
  val y="T2"
  println( "  in T2: y = " + y )
}
class Base12 {
  println( "  in Base12: b = " + b )
  val b="Base12"
  println( "  in Base12: b = " + b )
}
class C12 extends Base12 with T1 with T2 {
  println( "  in C12: c = " + c )
  val c="C12"
  println( "  in C12: c = " + c )
}
println( "Creating C12:" )
new C12
println( "After Creating C12" )


abstract class AbstractParent {
  def name: String
}
class ConcreteChild extends AbstractParent {
  val name = "Child"
}


class D(val i: Int)
object D {
    def apply(i: Int) = new D(i)
    def apply(i: Int, j: Int) = new D(i+j)
}
D(7).i
D(8,3).i


def apply[A](xs: A*): List[A] = xs.toList
def unapplySeq[A](x: List[A]): Some[List[A]] = Some(x)


Array(1, 2) sameElements Array(1, 2)


trait A { def a: this.type = this }
class B extends A
new B().a


val truthier: PartialFunction[Boolean, String] = { case true => "truthful" }
val fallback: PartialFunction[Boolean, String] = { case x => "sketchy" }
val tester = truthier orElse fallback
println(tester(1 == 1))
println(tester(2 + 2 == 5))


def ggg = println(1)
def fff(f: => Unit) = 3
fff(ggg) // returns 3
def fff(f: () => Unit) = 3
fff(ggg)// prints 1 and returns 3


loop {}


import scala.xml._
val someXML =
<sammich>
  <bread>wheat</bread>
  <meat>salami</meat>
  <condiments>
    <condiment expired="true">mayo</condiment>
    <condiment expired="false">mustard</condiment>
  </condiments>
</sammich>
someXML match {
  case <sammich>{ingredients @ _*}</sammich> => {
    for (cond @ <condiments>{_*}</condiments> <- ingredients)
      println("condiments: " + cond.text)
  }
}


class A[+String]
class B[-String]
def f[T >: A] = println()
def g[T <: B] = println()


class C(str: String)
def f[T <% C](t: T) = println(t) // a type T that can be converted to B with some view (implicit method)
implicit def any2C(a: AnyRef) = new C(a.toString)
f("oi")
f(3) // fails

def g(i: => Int): String = {
    println("g()")
    i.toString
}
def f(g: (=> Int) => String) = g(10)
def h(g: (=> Int) => String) = "10"
f(g)
h(g)



class MethodsGetter(a: Any) {
    def methods() = a.asInstanceOf[AnyRef].getClass.getMethods
}
implicit def any2MethodsGetter(a: Any) = new MethodsGetter(a)


import scala.PartialFunction
var f1: PartialFunction[Int, String] = { case 1 => "oi" }
var f2: PartialFunction[Int, String] = { case 2 => "tchau" }
var f3: PartialFunction[Int, String] = { case _ => "..." }
var f = f1 orElse f2 orElse f3



import scala.actors._
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
actor {
    alive(9010)
    register('myName, self)
    while (true)  receive {
        case a: String => println("got new message: " + a)
        case b: Array[Byte] => println("array size = " + b.size)
        case c: List[Int] => println(c)
        case (a: String, b: Array[Byte]) => println(a + " size = " + b.size)
        case i: Int => reply(i*2)
    }
}
actor {
    val c = select(Node("127.0.0.1", 9010), 'myName)
    c ! "msg"
    c ! Array.ofDim[Byte](10)
    c ! ("msg", Array.ofDim[Byte](10))
    c ! List(1,2,3,4,5)
    c !? 3
}



class A(val str: String) {
    def this(i: Int) = this(i.toString)
}



var v = 1
while ({v +=1; v < 10}) println(v)



def attempt(operation: => Boolean): Throwable Either Boolean = try {
  Right(operation)
} catch {
  case t: Throwable => Left(t)
}

println(attempt { throw new RuntimeException("Boo!") })
println(attempt { true })
println(attempt { false })



class A {
    type T = Int
}
val x: A#T = 3
x.isInstanceOf[A#T]
7.isInstanceOf[A#T]


class C1 { self1: C1 /*with ...*/ => //gives the object reference created from this class
    def mult(i: Int) = i * 3
    class C2 { self2: C2 =>
        def mult(i: Int) = self1.mult(i * 7)
        class C3 {
            def mult(i: Int) = self2.mult(i * 11)
        }
        var c3 = new C3
    }
    var c2 = new C2
}
var c1 = new C1
c1.c2.c3.mult(3)


trait B
class A
new A with B


def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))
lazy val ints = from(0)
lazy val odds = ints.filter(_ % 2 == 1)
lazy val evens = ints.filter(_ % 2 == 0)
odds.take(10).print
evens.take(10).print


package scala
class SerialVersionUID(uid: Long) extends StaticAnnotation



"""         |99 bottles of beer on the wall
            |99 bottles of beer
            |Take one down, pass it around
            |98 bottles of beer on the wall""".stripMargin


val Some(value1) = Some(1)
val Some(value2) = "a(.)c".r.findFirsIn("abc")


var map = Map(1 -> 2)
map.get(1) match {
    case Some(v) =>
    case None    =>
}


"%s = %d".format("x", 1)






/* Notes:
* Whenever you define a main method to use as the entry point for an application, Scala requires you to put it in an object.
* Do not define main or any other method in a companion object that needs to be visible to Java code as a static method. Define it in a singleton object, instead.
* Remember that import statements are relative, not absolute. To create an absolute path, start with _root_.
* If a method takes no parameters, you can define it without parentheses. Callers must invoke the method without parentheses. If you add empty parentheses, then callers may optionally add parentheses.
* The convention in the Scala community is to omit parentheses when calling a method that has no side-effects. So, asking for the size of a sequence is fine without parentheses, but defining a method that transforms the elements in the sequence should be written with parentheses. This convention signals a potentially tricky method for users of your code.
* It’s also possible to omit the dot (period) when calling a parameterless method or one that takes only one argument. 
* Any method whose name ends with a : binds to the right, not the left.
* Scala does not have checked exceptions, like Java. Even Java’s checked exceptions are treated as unchecked by Scala. There is also no throws clause on method declarations. However, there is a @throws annotation that is useful for Java interoperability
* Except for declaring abstract classes, the abstract keyword is only required on a method in a trait when the method has a body, but it calls the super method which doesn’t have a concrete implementation in parents of the trait.
* When considering whether a “concept” should be a trait or a class, keep in mind that traits as mixins make the most sense for “adjunct” behavior.
* In order to avoid infinite recursion, Scala requires each auxiliary constructor to invoke another constructor defined before it.
* Table 5.1. Visibility Scopes.
        Name             | Keyword          | Description
        -----------------+------------------+--------------
        public           | none             | Public members and types are visible everywhere, across all boundaries.
        protected        | protected        | Protected members are visible to the defining type, to derived types, and to nested types. Protected types are visible only within the same package and subpackages.
        private          | private          | Private members are visible only within the defining type and nested types. Private types are visible only within the same package.
        scoped protected | protected[scope] | Visibility is limited to scope, which can be a package, type, or this (meaning the same instance, when applied to members, or the enclosing package, when applied to types). See the text below for details.
        scoped private   | private[scope]   | Synonymous with scoped protected visibility, except under inheritance (discussed below).
* When a type or member is declared private[P], where P is the enclosing package, then it is equivalent to Java’s package private visibility.
* Be careful when choosing the names of members of traits. If two traits have a member of the same name and the traits are used in the same instance, a name collision will occur even if both members are private.
* Attempts to override a trait-defined val will be accepted by the compiler, but have no effect in Scala version 2.7.X.
* When an instance of a class is followed by parentheses with a list of zero or more parameters, the compiler invokes the apply method for that instance.
* You can have secondary constructors in case classes, but there will be no overloaded apply method generated that has the same argument list. You’ll have to use new to create instances with those constructors.
* We now know four ways to create a two-item tuple (twople?):
        ("Hello", 3.14)
        Pair("Hello", 3.14)
        Tuple2("Hello", 3.14)
        "Hello" → 3.14
* Objects are instantiated automatically and lazily by the runtime system. When an object named MyObject is compiled to a class file, the class file name will be MyObject$.class.
* When a class is sealed, the extending classes of it must be defined in the same source file. Avoid sealed case class hierarchies if the hierarchy changes frequently (for an appropriate definition of “frequently”).
* A common pattern when working with Actors in Scala is to use a case object to represent a message without internal data.
*/