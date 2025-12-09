# pgex001
pgplot based fortran examples
###
To compile on linux:
###
	make
###
To compile on MS Windows using Salford/Silverfrost FTN77 and mingw32-make
###
	mingw32-make -f Makefile.ftn77
###
To compile on MS Windows using MinGW gfortran and mingw32-make
###
	mingw32-make -f Makefile.mingw
###
m2b.f
###
The program m2b.f convert an ascii matrix file of dimension WxH with elements of size 8 bits [0,255] in a binary file containing the
same array. The ascii array is organized in rows separated by LF each column element separated by space. The program takes in input
the input file name from the standard input (or redirecting on stdin the content of a file).
###
	./m2b < in.txt
###
	cat in.txt
	arr10x3.asc
This will generate a file called arr10x3.raw containing the binary data.
###
m2b2.f
###
p1_1.f
###
p2.f
###
pgs.f
###
pgs2096x2338.f
###
the program pgs2096x2338 plots an image of dimension width=2096 and height=2338 in grayscales.
Using the pgplot library.
###
pgs4080x3072.f
###
the program pgs4080x3072 plots an image of dimension width=4080 and height=3072 in grayscales.
Using the pgplot library.
###
pgs500x320.f
###
the program pgs500x320 plots an image of dimension width=500 and height=320 in grayscales.
Using the pgplot library.
###
pgsc2096x2338.f
###
the program pgsc2096x2338 plots an image of dimension width=2096 and height=2338 in colors.
Using the pgplot library.
###
pgsc4080x3072.f
###
the program pgsc4080x3072 plots an image of dimension width=4080 and height=3072 in colors.
Using the pgplot library.
###
pgsc500x320.f
###
the program pgsc500x320 plots an image of dimension width=500 and height=320 in colors.
Using the pgplot library.
###
raa.f
###
Read Ascii Array
###
rd.f
###
testbyte.f
###
waa.f
###
Write Ascii Array
###
The program waa writes an ascii array of fimension WxH, in the form of a matrix with column elements separated by spaces 
(ascii decimal 32) and rows separated by Line Feeds (LF) character (ascii decimal 10). The array is filled with random values.
###
waar.f
###
The program waar writes an ascii array of fimension WxH, in the form of a matrix with column elements separated by spaces 
(ascii decimal 32) and rows separated by Line Feeds (LF) character (ascii decimal 10). The array is filled with random incremental
value.
###
waas.f
###
The program waas is designed to compile and work with salford ftn77 fortran compiler and does what the program waa does on Linux.
(waa.f uses a non standard fortran 77 extension)
###
wba.f
###
The program wba writes a binary array with dimension WxH and element size 8 bits, filled with casual numbers.
###
wbar.f
###
The program wbar writes a binary array with dimension WxH and element size 8 bits, filled with incremental numbers.
###
wbas.f
###
The program wbas writes a binary array with dimensions WxH and element size 8 bits, filled with casual numbers. The program
wbas uses the random number generator extension to fortran 77 standard, specific for the salford ftn77 compiler.
###
m2pl.f
###
The program m2pl converts a 2D array 8 bits elements matrix in ascii format to pixel list ascii file X,Y,VAL where x is the coordinate of
the pixel on the a axis (width) and Y is the coordinate of the y axis (height) obtaining a list
###
	x1 y1 val11
	x1 y2 val12
	 ... ...
	x1 yn val1n
	x2 y1 val21
	 ... ...
	xm ym valmn
###
pl2m.f
###
pl2m does the inverse of m2pl: takes as input a pixel list ascii file and creates as output a matrix ascii file.
###
How to compile a c program linked with pgplot (cpgplot).
###
compile to produce the object file
	gcc -c cpgsimple.c -I/usr/local/pgplot
###
then link using fortran compiler/linker
	f77 cpgsimple.o -o cpgsimple -L/usr/local/pgplot -Wl,-rpath=/usr/local/pgplot -lcpgplot -lpgplot -lX11 -lpng
(f77 is just a link to gfortran)
