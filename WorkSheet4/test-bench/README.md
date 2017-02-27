# Tick4TB

This project contains a testbench based on Expecto and FsCheck for the Tick 4 code.

## Setup

The code requires `Expecto` and `Expecto.FsCheck` from NuGet. When you download these under VS they will install with the lowest F# language version (3.1) as a dependency (reference FSharp.Core). This will then degrade your project to F# 3.1. You can easily correct this by updating FSharp.Core (NuGet Package Management on solution) to the latest version 4.4.0.0.

The code has a dll dependency on the compiled code from my model answer which is contained in `packages/modellib/release`.

## Test operation

Test frameworks run tests and print out warnings or errors when tests fail. They can be configured to print out more or less (the setup you are given as verbose printout). Even so they do not print out tests that pass. That is normally what you want. However it can be disconcerting because when you run tests on correct code nothing happens.

For this example I have also made the test function itself print out what are the inputs and outputs being tested on failing tests. You could modify this to print all tests to see how extensive the testing is. Note the configurable number of tests (by default 2000 in this code which is easily changed).

## Intro

Expecto is a test framework (nuget expecto and expecto.fscheck) which installs smoothly ans allows sophisticated testing. It is used here in a minmal form to do an FsCheck test of your program. The key to success here is writing a D.U. type that correctly represents the possible commands (types `VName` and `CommandSpec` - for obvious reasons only a small number of variable names are tested). Then the FsCheck auto-generated randomised data will work with minimal code. For this test it is necessary to check a sequence of commands, since internal state must be tested. This (difficult) testing is done by using a list of commands as the input data type over which fscheck randomises. The list is applied sequentially to your code and results from your code are compared with those from the model code.

## How to use

Replace the code in envt.fs with your own code

## Files

types.fs - common types for Command and Response
envt.fs - your tick 4 code
program.fs - the testbench code

packages/modellib/release/* - compiled code from my model answer, against which your solution is tested

## Tests implemented

* The first test checks that all commands work correctly
* The second test checks that valid command strings work correctly

Tests compare the operation of your code with that of teh model code for random input. Input data is selected randomly by FsCheck based on the `CommandSpec` list type.
`CommandSpec` is a D.U. type that is translated into valid commands. The FsCheck code automatically selects random commands by choosing values of the D.U.

## Caveats and extensions

This testing is not complete. Although functionality on correct data is very well tested, data which results in error returns is less well tested, for example word separators are always spaces but these should be randomised to check that whitespace detection is correct. For example, to get random separators from FsCheck you would need to generate them from a D.U. and a conversion function:

```
type WhiteSpace =
	| WsSpace
	| WsTab
	| WsRet
	| WsFormFeed

let wsToStr = function | WsSpace -> " " | WsTab -> "\t" | WsRet -> "\r" | WsFormFeed -> "\f"
```

Then modify `CommandSpec` so that each command case included `WhiteSpace list` fields that randomly determine the separators. That would make the search space very large - even though most of it is redundant. In such cases it is best to split things up and have separate tests for operation, each test varying different parts of the input. With Expecto and FsCheck that is easily accomplished, simply write different FsCheck test functions as new tests like the two given.

## Note

Note the idiomatic use of `<|` operator here to make the test function definitions neat.