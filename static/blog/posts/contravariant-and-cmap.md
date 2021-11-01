In this very first post I would like to explore the Contravariant type class and its arguably confusing `cmap` function. I've actually used it when creating this website, which is when I started to really understand why one would use it.

Looking at the package [documentation](https://pursuit.purescript.org/packages/purescript-contravariant/5.0.0/docs/Data.Functor.Contravariant), the Contravariant type class may appear confusing.
```hs
class Contravariant f where
  cmap :: forall a b. (b -> a) -> f a -> f b
```
The type signature for the `cmap` function certainly didn't help me understand it when I first took a look at it.  At first sight, this signature looks just like the familiar Functor's `map` function.
```hs
map :: forall a b. (a -> b) -> f a -> f b
```
However, the types of the `cmap` mapping function appear to be backwards. How can we get something that holds some `b` by mapping with a function `b -> a`? This makes no sense when we use our intuition for Functor.

## Functions are Functors
One key piece to understanding `cmap` lies in the fact that a function `a -> b` is a Functor. More specifically, it is a Covariant Functor for its output type. This means you can use `map` with a function.
```hs
square :: Int -> Int
square n = n * n

squareAndAdd10 :: Int -> Int
squareAndAdd10 = map (add 10) square

-- squareAndAdd10 10 == 110
```
A `Functor` has a slot for a higher kinded type that takes 1 type variable `f a`. This means we need something with kind `Type -> Type`. A function however, which always has an arity of 1 in PureScript, has kind `Type -> Type -> Type`. It takes 2 type variables, 1 for the input type, and 1 for the output type. This means we will need to partially apply our type constructor for our function. Essentially "locking" that type variable from modification when mapping using the `Functor` instance.

We have 2 choices here, either we lock the input type variable, or we lock the output type variable. Looking at the [source](https://github.com/purescript/purescript-prelude/blob/v5.0.0/src/Data/Functor.purs#L40) for the `Functor` instance on a function, we see that the choice was made to lock the input type variable. This totally makes sense considering our input doesn't change when mapping over a function. Let's see what happens when we create our own function newtype and create an instance for `Functor`.
```hs
-- Create our own newtype 'Arrow' for a function because we
-- cannot overwrite the existing Functor instance for Function.
newtype Arrow input output = Arrow (input -> output)

-- Helper function to run an Arrow function
runArrow :: forall input output. input -> Arrow input output -> output
runArrow input (Arrow fn) = fn input

-- Create an instance for Functor for Arrow
instance Functor (Arrow input) where
  map projector (Arrow fn) = Arrow (\input -> projector (fn input))

sayHello :: String -> String
sayHello name = "Hello, " <> name <> "!"

Arrow sayHello
  # map toUpper
  # runArrow "John"
-- outputs: "HELLO, JOHN!"
```
As you can see, we partially applied our `Arrow` newtype in the Functor instance with a fixed input type, i.e. `(Arrow input)`. This gives us the kind `Type -> Type` which is exactly what Functor requires.

You may have noticed that the implementation for `map` on a function is basically `compose`. You would be right! In fact, that's exactly how `map` is implemented for function in Prelude.
```hs
instance functorFn :: Functor ((->) r) where
  map = compose
```

## Functions are Contravariant Functors
Using the function as Functor example from above, we can apply the same thing for Contravariant. Let's take another look at its definition.
```hs
class Contravariant f where
  cmap :: forall a b. (b -> a) -> f a -> f b
```
Just like Functor, it has a slot for a higher kinded type with 1 type variable, in this case `f` with kind `Type -> Type`. Let's implement our own version of Contravariant for our own function wrapped in a newtype `ContraArrow` just like we did for Functor and `Arrow`. However, unlike Functor, this time we'll choose to "lock" the ouput type variable. As a result, the input type variable is the one that will be mapped.
```hs
-- Create our own newtype 'ContraArrow' for a function
newtype ContraArrow output input = ContraArrow (input -> output)

runContraArrow :: forall input output. input -> ContraArrow output input -> output
runContraArrow input (ContraArrow fn) = fn input

-- Create an instance for Contravariant for ContraArrow
instance Contravariant (ContraArrow output) where
  cmap projector (ContraArrow fn) = ContraArrow (\input -> fn (projector input))

sayHello :: String -> String
sayHello name = "Hello, " <> name <> "!"

ContraArrow sayHello
  # cmap toUpper
  # runContraArrow "John"
-- outputs: "Hello, JOHN!"
```
This example exposes the duality of Contravariant and Covariant Functors. Where one is a pre-processor and the other is a post-processor respectively. The implementation of `ContraArrow` given by the purescript [contravariant](https://pursuit.purescript.org/packages/purescript-contravariant) package is called `Op`. And the implementation for `cmap` is just flipped function composition i.e. the `<<<` operator.

## Conclusion
A Contravariant Functor is a Functor that allows us to do some pre-processing on data. It is an abstraction over modifying data before sending it to e.g. a function. However, this can be anything that takes in some data. In practice, as far as I'm aware, it isn't often used. It is far more common to see the manually implemented form of this abstraction. i.e. `g <<< f`
