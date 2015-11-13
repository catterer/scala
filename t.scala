/*
def fact(x: Int) : Int = {
    if (x == 0) 1
    else x*fact(x-1)
}

val arr = (new Array[String](3)).map(a=>"a")
arr.foreach(println)

var x = 1
x+=1
*/

def last[Type](list: List[Type]) : Option[Type] = list match {
    case Nil => None
    case lastest :: Nil => Some(lastest)
    case _ :: tail => last(tail)
}

def lastButOne[Type](list: List[Type]) = {
    def _lastButOne[Type](prelast : Option[Type] = None, list : List[Type]) : Option[Type] = list match {
        case Nil => None
        case lastest :: Nil => prelast
        case prel :: tail => _lastButOne(Some(prel), tail)
    }
    _lastButOne(None, list)
}

def getKthElement[Type](k: Int, list: List[Type]) : Option[Type] = list match {
    case Nil => None
    case prel :: tail => {
        if (k == 0)
            Some(prel)
        else
            getKthElement(k-1, tail)
    }
}

def countList[Type](list: List[Type]) = {
    def _countList[Any](n: Int, list: List[Any]) : Int = list match {
        case Nil => n
        case _ :: tail => _countList(n+1, tail)
    }
    _countList(0, list)
}

def reverseList[Type](list: List[Type]) = {
    def _reverseList[Type](to: List[Type], from: List[Type]) : List[Type] = from match {
        case Nil => to
        case some :: tail => _reverseList(some :: to, tail)
    }
    _reverseList(Nil, list)
}

def isPal[Type](list: List[Type]) = reverseList(list) == list

def flatten(list: List[Any]) = {
    def _flatten(acc: List[Any], src: List[Any]) : List[Any] = src match {
        case Nil => acc
        case (head: List[Any]) :: tail => _flatten(_flatten(acc, head), tail)
        case v :: tail => _flatten(v :: acc, tail)
    }
    reverseList(_flatten(Nil, list))
}

def hisFlatten(l: List[Any]): List[Any] = {
    def _flatten(res: List[Any], rem: List[Any]):List[Any] = rem match {
        case Nil => res
        case (h:List[_])::Nil => _flatten(res, h)
        case (h:List[_])::tail => _flatten(res:::h, tail)
        case h::tail => _flatten(res:::List(h), tail)
    }
    _flatten(List(), l)
}

println(reverseList(List("asd", "as", "d")))
println(reverseList(List(1,2,3,4,5)))
println(countList(List(1,2,3,4,5)))
println(isPal(List(1,2,3,2,1)))
println(isPal(List(1,2,3,2,1,2)))
println(flatten(List(1,List(1,2,3), 2,3,2,List("a", "b", List(1,2), "c"),2)))
println(hisFlatten(List(1,List(1,2,3), 2,3,2,List("a", "b", List(1,2), "c"),2)))
