- ~~Improve HIR -> MIR lowering~~
    - Use only one lowering function, returning Lowered
        ```Rust
        enum Lowered {
            Place(mlr::Place),
            Op(mlr::Op),
            Val(mlr::Val),
        }
        ```
    - Lowered has functions `into_place`, `into_op`, `into_val`, which convert.
        - E.g. if something is already Val, then `into_val` does nothing, while `into_place` creates a new temporary.
    - Then the current functions `lower_to_{val,place,op}` never recursively call each other
- ~~Explicit call of trait functions using the `<Type as Trait>::func()` syntax~~
- Associated functions (should almost immediately follow from the previous point)
- Associated types
