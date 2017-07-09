Guillermo Alicea
07/22/16

-In order to run the CAM program, you must first ensure that you are in
the directory in which cam.ml is stored.

-You must include a file called "input.txt" which contains the cam code
which you would like to test. The format of input.txt MUST be in the form

# <instruction> <# or () if load>
# <instruction> <#>
.
.
.

and it must be properly formed, so that stop will be encountered over the
course of execution. A correct example can be seen in "input.txt", which is provided.

-To compile the program, in the correct directory, in a unix-like environment,
such as eustis,you can use "ocamlc cam.ml -o cam" to compile cam.ml using
ocamlc and to name the executable cam

-Finally, you can run the program by using "./cam" and the output will print to
the screen

**Important to note that the output will be different than the output contained
in output.txt since the output in output.txt was obtained using an array size of 10,
as opposed to 50, however this does not change the CAM process, just the number
of 0's outputted

**output.txt contains the output of 2 test cases and a comparison between the two,
as well as an explanation of the output

**input.txt will contain the example shown in the cam assignment document,
so running the compiled program will produce output corresponding to that example,
with a stack and mem array size of 50.
