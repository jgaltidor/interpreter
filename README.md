interpreter for Programming Language from Robert Harper's Textbook
===================================================================

First, note that Scala version 2.7.4 is required to run this
interpreter.
There was an issue with a bug in Scala 2.7.3.

I have updated my interpreter for *Lnsfpsp* to handle the following
three features:

1. Failures
2. Exceptions
3. Continuations

I have implemented an abstract machine for executing these features
that uses a control stack of frames that captures the state of the
program.
I have also updated my type checker with type inference capabilities
that allows more flexibility with case and match expressions.

Example programs illustrating these new features can be found in
the `testprogs` directory:

*  `testprogs/failures` -      Contains programs using Failure features
*  `testprogs/exceptions` -    Contains programs using Exception features
*  `testprogs/continuations` - Contains programs using Continuation features

The syntax is mostly the same as in Harper's textbook, but differs
in the case of exceptions because Harper does not specify
and exception values.
On p. 253, Harper represents the exception type as a distinguished
sum type, but does not specify the components of the sum type.
For my language, I have specified the exception type to be
SumType(StrType, NumType).
Exceptions encapsulating string messages or error codes can
be raised and handled.
The message or code encapsulated with the exception raised
can be extracted using case or match expressions.
testprogs/exceptions/e01.txt contains an example program
that extracts a string message from an exception and an
error code from another exception.

Script/command for building the interpreter
----------------------------------------------
    build.sh

Running the interpreter
-----------------------
On Unix compliant systems, the interpreter has the following usage:

    usage: interpreter.py <file 1> <file 2> ... <file N>

Otherwise, the following usage is platform independent

    usage: scala mylang.Interpreter <file1> ... <fileN>


Script/command for running intrepreter over examples
---------------------------------------------------
    runexamples.sh

Script/command for running intrepreter over only new examples:  
------------------------------------------------------------------
    runnew.sh

Script/command for running intrepreter over only old examples:
-------------------------------------------------------------------
    runold.sh
