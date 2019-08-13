# yampa-ball

A very basic console animation of a falling ball in
[Haskell](https://www.haskell.org/) using the
[Yampa](https://hackage.haskell.org/package/Yampa) framework. This is meant as
a simple example for functional reactive programming (FRP).

The code is partly inspired by a youtube video of
[jekor](https://www.youtube.com/watch?v=-IpE0CyHK7Q) and the examples found in
[Yampa's repository](https://github.com/ivanperez-keera/Yampa/tree/develop/examples).

## Run

```bash
stack run
```

In order to execute this you need to install the
[stack tool](https://docs.haskellstack.org/).

If you want to change the behavior of the animation you can play around with
the parameters in the first `let` of the `main` function.
