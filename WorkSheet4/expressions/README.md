# Worksheet 4 Sample Project

The code here illustrates a multi-file F# project which can be developed using either Visual Studio or Visual Studio code.

## F# Structure

We follow a good and simple policy of using multiple modules (`Common`, `Tokeniser`,`Parser`,`Evaluater`,`Program`) for the project within one namespace (`Expr`). Module names are always identical to source file names and we have one module per source file. The top-level code is inside the `Program` module which therefore must be last in the source file compile order.

All F# code is written inside a module and therefore indented by one level (see these samples). All source files must start with two lines that define the namespace and the module name. See the samples, and the generic structure below.

```
namespace `MyNameSpace` // namespace
module MyModule = // module
    let f x = x + 1 // definitions in module
```

Code in one module can be referenced from another either using the file (and module) name `Token.tokeniser` or by opening the module `open Tokeniser` and using the file name directly `tokenise`. Keeping module names helps to identify where the code is from and also avoids name clashes.

As always in F# compilation allows only backward references so the module order matters and for example `Common` must be listed before `Tokeniser`.

## Files

The directories exp-vs and exp-vscode contain the project source files in a Visual Studio or VS code project.

## Development with Visual Studio

The project can be edited using Visual Studio opening the `exp-vs\exp-vs.fsproj` file as a project. The order of the source files matters, and can be changed in the project window (moving files up and down)

The project can be compiled and run from Visual Studio as normal.

## Development with Visual Studio Code

The project can be edited in VS Code most conveniently by opening the `exp-vscode` directory. The README file there contains instructions

## Notes

1. if you use FSI for interactive development note these [tips](http://brandewinder.com/2016/02/06/10-fsharp-scripting-tips/).
2. Running tests can obviously be added to the above development flow. VS Code has recently integrated [expecto](https://github.com/haf/expecto) an easy to use F# test framework. This can be used to run fsunit and sccheck easily as in this [very recent tutorial](http://www.prigrammer.com/?p=398).
