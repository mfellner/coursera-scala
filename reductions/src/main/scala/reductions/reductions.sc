import reductions.ParallelParenthesesBalancing._

//// false
//parBalance(")".toArray, 9)
//parBalance("(".toArray, 9)
//parBalance(")(".toArray, 9)
//parBalance("))".toArray, 9)
//parBalance("((".toArray, 9)
//parBalance("(((".toArray, 9)
//parBalance(")))".toArray, 9)
//parBalance("((((".toArray, 9)
//parBalance("))))".toArray, 9)
//parBalance("()(".toArray, 9)
//parBalance("())".toArray, 9)
//parBalance("(()".toArray, 9)
//parBalance(")()".toArray, 9)
//parBalance(")()(".toArray, 9)
//parBalance(")()(".toArray, 9)
//parBalance(")()()(".toArray, 9)
//
//// false
//parBalance(")(".toArray, 12)
//parBalance(")))".toArray, 12)
//parBalance(")))(((".toArray, 12)
//parBalance(")))((()".toArray, 12)
//parBalance(")))((()))".toArray, 12)
//parBalance(")))((())))".toArray, 12)

// false
parBalance(")(".toArray, 2)
parBalance(")))".toArray, 3)
parBalance(")))(((".toArray, 3)
parBalance(")))((()".toArray, 3)
parBalance(")))((()))".toArray, 3)
parBalance(")))((())))".toArray, 3)

// true
parBalance("()".toArray, 9)
parBalance("(())".toArray, 9)
parBalance("()()".toArray, 9)
parBalance("()()()".toArray, 9)
parBalance("((()))".toArray, 9)
parBalance("()(())()".toArray, 9)

// true
parBalance("()".toArray, 2)
parBalance("(())".toArray, 3)
parBalance("()()".toArray, 3)
parBalance("()()()".toArray, 1)
parBalance("((()))".toArray, 3)
parBalance("()(())()".toArray, 3)
