In this very first post I would like to explore the why, what and how of the `Contravariant` type class and its arguably confusing `cmap` function. I've actually used it when creating this website, which is when I started to really understand why one would use it.

Looking at the package [documentation](https://pursuit.purescript.org/packages/purescript-contravariant/5.0.0/docs/Data.Functor.Contravariant), the `Contravariant` type class may appear confusing.
```hs
class Contravariant f where
  cmap :: forall a b. (b -> a) -> f a -> f b
```
The type signature for the `cmap` function certainly didn't help me understand it when I first took a look at it.  At first sight, this signature looks just like the familiar `Functor`'s `map` function.
```hs
map :: forall a b. (a -> b) -> f a -> f b
```
However, the types of the `cmap` mapping function appear to be backwards. How can we get something that holds a `b` by mapping with a function `b -> a`? This makes no sense when we use our intuition for `Functor`s.

## Functions are Functors
One key piece to understanding `cmap` lies in the fact that a function `a -> b` is a `Functor`. More specifically, it is a `Functor` for its output type. This means you can use `map` with a function.
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

-- Create an instance for Functor for Arrow
instance Functor (Arrow input) where
  map projector (Arrow fn) = Arrow (\input -> projector (fn input))

-- Helper function to run an Arrow function
runArrow :: forall input output. input -> Arrow input output -> output
runArrow input (Arrow fn) = fn input

sayHello :: String -> String
sayHello name = "Hello, " <> name <> "!"

-- If you want to run this in your own REPL
-- make sure you have Data.String imported
sayHello
  # Arrow
  # map toUpper
  # runArrow "John"
-- outputs: HELLO, JOHN!
```
You may have noticed that the implementation for `map` on a function is basically `compose`. You would be right! In fact, that's exactly how `map` is implemented for function in Prelude.
```hs
instance functorFn :: Functor ((->) r) where
  map = compose
```

## Contravariant Functors
Using the function as `Functor` example, we can apply the same thing for `Contravariant`. Let's take another look at its definition.
```hs
class Contravariant f where
  cmap :: forall a b. (b -> a) -> f a -> f b
```
Just like `Functor`, it has a slot for a higher kinded type with 1 type variable, in this case `f`. However, unlike `Functor`, this time we choose to "lock" the ouput type variable. As a result, the input type variable is the one that will be mapped.

Let's fill in our function in the signature of cmap to see how it works.
```hs
cmap
  :: forall originalInput newInput output
  . (originalInput -> newInput)
  -> Function originalInput output
  -> Function newInput output
-- or shorter but less clear
cmap :: forall a b c. (a -> b) -> Function b c -> Function a c
```





that's exactly how it is defined.
But it can also be given an instance for `Contravariant` by using its input type. Let's consider an example function `add42` which adds `42` to an `Int` with signature:
```hs
add42 :: Int -> Int
```


The key to understanding the `cmap` function lies in 2 pieces of knowledge:
1. The `Contravariant` type class has a higher kinded type `f`.
1. A function is a `Functor`

The first point means that this is indeed a `Functor`, just not the one most of us expect. Rather than the familiar `Covariant Functor`, this type class defines a `Contravariant Functor`. This means that instead of mapping the output of something that generates ouput, a `Contravariant Functor` enables mapping the input of something that consumes input.
