package Tutorial4

object Question4 {
  /*
  What expressions can you use to define the abstract state space?

      I. subsets of A
      ---------------

            state space: S = 𝒫(A)
        example element: s = ∅ ∈ S
                         t = {1,2,3} ∈ 𝒫(Int)

      II. tuples
      ----------

            state space: S = A × B      – 2-tuples
                         S = A × B × C  – 3-tuples (and so on...)
       example elements: s = (0, 0) ∈ Int × Int
                         t = ("ABC", 42) ∈ String × Int

      III. sequences of type A
      ------------------------

            state space: S = [A]
       example elements: s = [1,2,3,4,5] ∈ [Int]
                         t = [1,2,3,4,5] ++ t ∈ [Int] (infinite: 1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2...)

      IV. functions from A to B
      -------------------------

            state space: S = A → B
       example elements: s = \x -> x + 1 ∈ Int → Int (Haskell notation)
                         s = λx.(x+1) ∈ Int → Int    (mathematical lambda notation)
                         s = { (x, x+1) ∈ Int × Int | x ∈ Int } (mathematical notation – set of maplets)
         t(names, numbers) = { (names(i), numbers(i)) ∈ String × String | i ∈ [0..|names|) }

      Note that A → Int can be interpreted as "bag of A".

      V. partial functions: e.g. A ⇸ B
      --------------------------------

      All of the above can be combined:
            some strange state space: S = 𝒫(Int → (String, Int))
                     example element: s = { λx.("ABC", 2*x), λx.("XYZ", x*x-3) }
  */

  /** A stack storing data of type A
   *
   * state: s ∈ [A]
   *
   * init: s = [] */
  trait Stack[A] {
    /** Is the stack empty?
     *
     * pre: ⊤
     *
     * post: s = s₀ ∧ returns s = [] */
    def isEmpty: Boolean

    /** Push x onto the stack.
     *
     * pre: ⊤
     *
     * post: s = [x] ++ s₀ */
    def push(x: A): Unit

    /** Pop a value off the stack and return it.
     *
     * pre: s ≠ []
     *
     * post: returns x s.t. s₀ = [x] ++ s */
    def pop(): A
  }
}
