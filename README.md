μPascal Compiler
===============

### Overview
This is a [μPascal](http://hypertextbookshop.com/transPL/Resources/microPascalCFG.html) compiler that is written in
the Haskell programming language. It's a project compiler for the computer science capstone course at
[Montana State University](http://www.cs.montana.edu/course/csci-468).

### Setup
The program is written in [Haskell](http://www.haskell.org/haskellwiki/Haskell) and can be most easily built with
[Stack](https://docs.haskellstack.org/en/stable/README/) (installing `stack` with [Homebrew](https://brew.sh/) is
recommended if on MacOS). Clone the repository or download an archive of the project using the links to the left. Then
issue the following commands in the terminal to compile the driver:

    cd ~/pascal-compiler
    stack install --local-bin-path .

The compiler can then be run using this terminal command:

    ./pascal-compiler input_file.mp

Which will compile the μPascal source code, and output the semantic analyzer data to stdout.
Several test input files are provided in the `pascal-compiler/test/Tests/` folder.

### Objectives
Our main objective for this project was to expand our abilities. Up to this point we have had a mostly imperative
object oriented programming experience at the school, and wanted to use this opportunity to learn about functional
programming as well as compilers. We planned to implement a scanner, parser, and semantic analyzer from scratch; then
tie them all together into a working compiler.

### Progress and Teamwork
So far this has been an enjoyable experience! As a team we've worked very well together and we've been able to learn a
ton about Haskell, functional programming, source control, and compilers. The project is finished, and may receive
random updates in the future if any of us look at this repo in the future and want to touch things up.

### Authors and Contributors
A group of three CS students created this, Tyler Huffman(@iduhetonas), Murph Murphy(@skeet70), and
James Sonntag(@Gabrinthei). All three of us split the workload as evenly as possible, and all of the design and
architecture decisions were cooperatively made. Specific tasks implemented by each member of the team can be seen in the
GitHub repository [issues tracker](https://github.com/skeet70/pascal-compiler/issues), which was used for
team management.
