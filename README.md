Pascal Compiler
===============

### Overview
This is a [microPascal](http://hypertextbookshop.com/transPL/Resources/microPascalCFG.html) compiler that is written in the Haskell programming language. It's a project compiler for the computer science capstone course at [Montana State University](http://www.cs.montana.edu/course/csci-468).

### Setup
The program is written in [Haskell](http://www.haskell.org/haskellwiki/Haskell) and should be compiled with GHC 7.4.2+. Clone the repository or download an archive of the project using the links to the left. Then issue the following commands in the terminal to compile the driver:

    cd ~/pascal-compiler
    ghc --make mp.hs

The compiler can then be run using this terminal command:
    
    ./mp input_file.mp

Which will compile the microPascal source code, and output a semantic analyzer data file. Several test input files are provided in the `pascal-compiler/Tests/` folder.

### Objectives
Our main objective for this project was to expand our abilities. Up to this point we have had a mostly imperative programming experience at the school, and wanted to use this opportunity to learn about functional programming as well as compilers. We planned to implement a scanner, parser, and semantic analyzer from scratch; then tie them all together into a working compiler.

### Progress and Teamwork
So far this has been an enjoyable experience! As a team we've worked very well together and we've been able to learn a ton about Haskell, functional programming, source control, and compilers. Currently, we are working on the design and preliminary implementation of the semantic analyzer. The most current code can be seen in the develop branch.

### Authors and Contributors
A group of three CS students created this, Tyler Huffman(@iduhetonas), Murph Murphy(@skeet70), and James Sonntag(@Gabrinthei). All three of us split the workload as evenly as possible, and all of the design and architecture decisions were cooperatively made. Specific tasks implemented by each member of the team can be seen in the GitHub repository [issues tracker](https://github.com/skeet70/pascal-compiler/issues), which was used for team management.
