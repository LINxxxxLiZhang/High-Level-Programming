## Visual Studio Multi-File Project

This project illustrates a multi-file F# project.

This project may run OK as downloaded  but if not can be recreated inside Visual Studio from scratch as follows:

1. New -> Project -> F# -> console application -> exp-vs (name)
3. Delete the auto-generate program.fs source file (using the project window)
4. Using project windows -> ws4-vs project -> add item -> existing item,  Add the downloaded source files, in the order: common,tokeniser,parser,evaluator, evaluator1,program. NB Visual Studio by default copies these files into its project directory and references them from there.


## Frequent Problems for Visual Studio Projects

This modules uses the 4.0 version of F#, which has more complete collection functions (e.g. it includes the useful `List.contains`). By default some version of Visual studio will not install this, and may end up having a dependency on the *previous* 3.1 version of F# core. this will result (for the code here0 in `common.fs` failing to compile because `List.contains` does not exist.

Luckily this can simply be mended. 

Tools -> NuGet Package Manager -> Manage packages for solution-> *Installed tab*

Check FSharp.Core package. It should be `FSharp.Core for F# 4.0` . If instead it is `FSharp.Core for F# 3.1`, go to the *Updates tab*. Update FSharp.Core (and all other visible updates) to the latest compatible version v4.0.0.1. When this succeeds FSharp.Core will be for F# 4.0 and your project will compile correctly.


