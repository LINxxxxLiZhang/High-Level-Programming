## How to create an F# project in VS Code


These instructions will recreate the project here in `exp-vscode` from an empty initial directory. Without following these instructions the project downloaded should work, and can be built and run without further configuration see *Building the project* below.

1. Install VS Code extensions: ionide-fsharp, ionide-fake, ionode-paket
2. Open an empty folder in VS Code which you wish to become the project folder, for example `exp-vscode`.
3. Create f# project: `Ctrl-Shift-P` -> f# -> Create New Project -> console -> `<ENTER>` -> `vscmain`. The project will be created in `exp-vscode/vscmain`. The `paket` and `msbuild` scripts will be created one directory higher in `tiny-vscode`.
4. Copy source files into `exp-vscode/vscmain/src` by copying the downloaded `src` directory.
5. Split the editor window and open `vscmain.fsproj` in one half. Navigate to `Compile` in this. You can now see how the list of project files with entries like: `<Compile Include="XXXX.fs"/>` changes as you use VS Code commands alter the project.
6. Navigate to the auto-generated `vscmain.fs` source file. Delete it (Ctrl-Shift-P -> f# -> *remove current file from project*).
5. Add source (`.fs`)files in `src` to project in order: common,tokeniser,parser, evaluater, evaluater1, program:
 * Open source file
 * `Ctrl-Shift-P` -> f# (in box) -> add file to project
 * Repeat as needed adding all files in compile order
 * Re-order current file with `Ctrl-Shift-UP-ARROW` or `Ctrl-Shift-DOWN-ARROW` if you get this wrong.  When changing or reordering project source files 


```
  <ItemGroup>
    <Compile Include="src/Common.fs" />
    <Compile Include="src/Tokeniser.fs" />
    <Compile Include="src/Parser.fs" />
    <Compile Include="src/evaluater.fs" />
    <Compile Include="src/evaluater1.fs" />
    <Compile Include="src/Program.fs" />
    <None Include="App.config" />
  </ItemGroup>
```

6. Add any other Nuget dependencies (not needed for this code) using Paket - the open source equivalent of Nuget: `Ctrl-Shift-P` -> pak (in box) -> add nuget package. None are needed for the downloaded code.

## Building the project

7. To build and the project run the `build.cmd` or `build.sh` (windows or linux) command from the top-level `exp-vscode directory`. You can do this from the GUI but if you start a command window and go to this directory you can run this command and view its printout. This command will update the `Paket` tool and then run it to download all dependencies and build the project to make `build\vscmain.exe`. You can equivalently use `Ctrl-Shift-P` -> fak (in box)-> FAKE build -> Build from VS Code. You can rebuild again after source changes the same way or using `build.fsx` which will run under `fsi`.
8. To run the built project executable run `build\vscmain.exe` or the equivalent executable under a command window or from the GUI.
