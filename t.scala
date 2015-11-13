// http://aperiodic.net/phil/scala/s-99/

// 1
def last[Type](list: List[Type]) : Option[Type] = list match {
    case Nil => None
    case lastest :: Nil => Some(lastest)
    case _ :: tail => last(tail)
}

// 2
def lastButOne[Type](list: List[Type]) = {
    def _lastButOne[Type](prelast : Option[Type] = None, list : List[Type]) : Option[Type] = list match {
        case Nil => None
        case lastest :: Nil => prelast
        case prel :: tail => _lastButOne(Some(prel), tail)
    }
    _lastButOne(None, list)
}

// 3
def getKthElement[Type](k: Int, list: List[Type]) : Option[Type] = list match {
    case Nil => None
    case prel :: tail => {
        if (k == 0)
            Some(prel)
        else
            getKthElement(k-1, tail)
    }
}

// 4
def countList[Type](list: List[Type]) = {
    def _countList[Any](n: Int, list: List[Any]) : Int = list match {
        case Nil => n
        case _ :: tail => _countList(n+1, tail)
    }
    _countList(0, list)
}

// 5
def reverseList[Type](list: List[Type]) = {
    def _reverseList[Type](to: List[Type], from: List[Type]) : List[Type] = from match {
        case Nil => to
        case some :: tail => _reverseList(some :: to, tail)
    }
    _reverseList(Nil, list)
}

// 6
def isPal[Type](list: List[Type]) = reverseList(list) == list

// 7
def flatten(list: List[Any]) = {
    def _flatten(acc: List[Any], src: List[Any]) : List[Any] = src match {
        case Nil => acc
        case (head: List[Any]) :: tail => _flatten(_flatten(acc, head), tail)
        case v :: tail => _flatten(v :: acc, tail)
    }
    reverseList(_flatten(Nil, list))
}

// 8
def removeConsDups[Type](list: List[Type]) = {
    def _removeDups[Type](lastElem: Type, acc: List[Type], list: List[Type]) : List[Type] = list match {
        case Nil => acc
        case h :: tail if h == lastElem => _removeDups(lastElem, acc, tail)
        case h :: tail => _removeDups(h, h :: acc, tail)
    }

    list match {
        case Nil => Nil
        case h :: t => reverseList(_removeDups(h, h :: Nil, t))
    }
}

// 9
def packConsDups[Type](list: List[Type]) = {
    def _packConsDups[Type](acc: List[List[Type]], list: List[Type]) : List[List[Type]] = list match {
        case Nil => acc
        case h :: originTail => acc match {
            case Nil => _packConsDups(List(List(h)), originTail)
            case Nil :: _ => throw new MatchError("wtf man") // don't know how to get rid of the warning : \
            case ((lastElem :: lastEquals) :: equalsTail) =>
                if (lastElem == h)
                    _packConsDups((h :: lastElem :: lastEquals) :: equalsTail, originTail)
                else
                    _packConsDups(List(h) :: acc, originTail)
        }
    }
    reverseList(_packConsDups(Nil, list))
}

// 10
def lengthEnc[Type](list: List[Type]) = {
    def _lengthEnc[Type](acc: List[(Int, Type)], list: List[Type]) : List[(Int, Type)] = list match {
        case Nil => acc
        case h :: t => acc match {
            case Nil => throw new MatchError("wtf man")
            case (n, v) :: accTail =>
                if (v == h)
                    _lengthEnc((n+1, v) :: accTail, t)
                else
                    _lengthEnc((1, h) :: (n, v) :: accTail, t)
        }
    }

    list match {
        case Nil => Nil
        case h :: t => reverseList(_lengthEnc(List((1, h)), t))
    }
}

// 11
def modLengthEnc[Type](list: List[Type]) = {
    def _append(currPatt: (Int, Any), acc: List[Any]) = {
        val (n, v) = currPatt
        if (n == 1)
            v :: acc
        else
            (n, v) :: acc
    }

    def _modLengthEnc[Type](currPatt: (Int, Type), acc: List[Any], list: List[Type]) : List[Any] = list match {
        case Nil => _append(currPatt, acc)
        case h :: t => {
            val (n, v) = currPatt
            if (v == h)
                _modLengthEnc((n+1, v), acc, t)
            else
                _modLengthEnc((1, h), _append(currPatt, acc), t)
        }
    }
    list match {
        case Nil => Nil
        case h :: t => reverseList(_modLengthEnc((1, h), Nil, t))
    }
}

// 12
def lengthDec[Type](list: List[(Int, Type)]) = {
    def _lengthDec[Type](mult: Int, value: Type, acc: List[Type], list: List[(Int, Type)]) : List[Type] = {
        if (mult == 0)
            list match {
                case Nil => acc
                case (n, v) :: tail => _lengthDec(n, v, acc, tail)
            }
        else
            _lengthDec(mult-1, value, value :: acc, list)
    }

    list match {
        case Nil => Nil
        case (n, v) :: t => reverseList(_lengthDec(n, v, Nil, t))
    }
}

// 13
// '10' implementation is direct

// 14
def duplicate[Type](list: List[Type]) = {
    def _duplicate[Type](acc: List[Type], list: List[Type]) : List[Type] = list match {
        case Nil => acc
        case h :: t => _duplicate(h :: h :: acc, t)
    }
    reverseList(_duplicate(Nil, list))
}

// 15
def duplicateN[Type](max: Int, list: List[Type]) = {
    def _duplicateN[Type](max: Int, n: Int, acc: List[Type], list: List[Type]): List[Type] = list match {
        case Nil => acc
        case h :: t if n == 0 => _duplicateN(max, max, acc, t)
        case h :: t if n > 0 => _duplicateN(max, n-1, h :: acc, h :: t)
    }
    reverseList(_duplicateN(max, max, Nil, list))
}

// 16
def dropN[Type](max: Int, list: List[Type]) = {
    def _dropN(max: Int, n: Int, acc: List[Type], list: List[Type]): List[Type] = list match {
        case Nil => acc
        case h :: t if n == 0 => _dropN(max, max, acc, t)
        case h :: t if n > 0 => _dropN(max, n-1, h :: acc, t)
    }
    reverseList(_dropN(max-1, max-1, Nil, list))
}

// 17
def split[Type](pos: Int, list: List[Type]) = {
    def _split(pos: Int, acc: List[Any], list: List[Any]): (List[Any], List[Any]) = list match {
        case Nil => (acc, Nil)
        case h :: t if pos == 0 => (h :: acc, t)
        case h :: t if pos > 0 => _split(pos-1, h :: acc, t)
    }
    val (begin, end) = _split(pos-1, Nil, list)
    (reverseList(begin), end)
}

// 18
def slice(start: Int, end: Int, list: List[Any]) = {
    def _slice(start: Int, end: Int, acc: List[Any], list: List[Any]): List[Any] = list match {
        case Nil => acc
        case h :: t => {
            if (start > 0)
                _slice(start-1, end-1, acc, t)
            else if (end > 0)
                _slice(0, end-1, h :: acc, t)
            else
                acc
        }
    }
    reverseList(_slice(start, end, Nil, list))
}

println(reverseList(List("asd", "as", "d")))
println(reverseList(List(1,2,3,4,5)))
println(countList(List(1,2,3,4,5)))
println(isPal(List(1,2,3,2,1)))
println(isPal(List(1,2,3,2,1,2)))
println(flatten(List(1,List(1,2,3), 2,3,2,List("a", "b", List(1,2), "c"),2)))
println(removeConsDups(List(1,2,3,3,3,3,1,2,3,2,2,1,1,1,3)))
println(packConsDups(List(1,2,3,3,3,3,1,2,3,2,2,1,1,1,3)))
println(lengthEnc(List('a,'b,'c,'c,'c,'c,'a,'b,'c,'b,'b,'a,'a,'a,'c)))
println(modLengthEnc(List('a,'b,'c,'c,'c,'c,'a,'b,'c,'b,'b,'a,'a,'a,'c)))
println(lengthDec(lengthEnc(List('a,'b,'c,'c,'c,'c,'a,'b,'c,'b,'b,'a,'a,'a,'c))))
println(duplicate(List('a,'b,'c,'c,'c,'c,'a,'b,'c,'b,'b,'a,'a,'a,'c)))
println(lengthEnc(duplicate(List('a,'b,'c,'c,'c,'c,'a,'b,'c,'b,'b,'a,'a,'a,'c))))
println(lengthEnc(duplicateN(3, List('a,'b,'c,'c,'c,'c,'a,'b,'c,'b,'b,'a,'a,'a,'c))))
println(dropN(2, List('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k)))
println(split(3, List('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k)))
println(slice(3, 7, List('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k)))
