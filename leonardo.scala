def reverseList[Type](list: List[Type]) = {
    def _reverseList[Type](to: List[Type], from: List[Type]) : List[Type] = from match {
        case Nil => to
        case some :: tail => _reverseList(some :: to, tail)
    }
    _reverseList(Nil, list)
}


def myFlatten(list: List[Any]) = {
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

println(myFlatten(List(1,List(1,2,3), 2,3,2,List("a", "b", List(1,2), "c"),2)))
println(hisFlatten(List(1,List(1,2,3), 2,3,2,List("a", "b", List(1,2), "c"),2)))
