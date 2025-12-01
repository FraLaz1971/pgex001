      PROGRAM PGS
        IMPLICIT NONE
C PLOT A MAP OF A 2D ARRAY WITH PGPLOT IN GRAY SCALES
        INTEGER I,J,IDIM,JDIM,POSI
C IDIM:DIMENSION OF X AXES(WIDTH), JDIM:DIMENSION OF Y AXES(HEIGHT)
C J scan the Y axes. I scan the x axes.
        REAL ARR(500,320)
        CHARACTER*4 ROW(500)
        CHARACTER*2000 LINE
        CHARACTER*80 IFNAM,OFNAM
        LOGICAL DEBUG
        REAL FG, BG, TR(6)
        INTEGER PGOPEN
C
C Call PGOPEN to initiate PGPLOT and open the output device; PGOPEN
C will prompt the user to supply the device name and type. Always
C check the return code from PGOPEN.
C
        DEBUG=.TRUE.
        DATA ARR /160000*0.0/
        DATA IDIM /500/
        DATA JDIM /320/
        DATA TR /0.0, 1.0, 0.0, 0.0, 0.0, 1.0/
        FG=255.0
        BG=0.0
        PRINT *,'ENTER THE ASCII INPUT FILE NAME'
        READ *,IFNAM
C create output file name
        POSI=INDEX(IFNAM,'.')
        OFNAM=IFNAM(1:POSI)//'ps'
C READ THE ARRAY FROM ASCII FILE
        OPEN(11,FILE=IFNAM,ERR=9000)
        DO 10,J=1,JDIM
            READ(11,100,END=9100) LINE
            READ(LINE,'(500A)') ROW
            DO 20,I=1,IDIM
              READ(ROW(I),'(F4.0)',ERR=9200) ARR(I,J)
20          CONTINUE
10      CONTINUE
        IF (DEBUG) PRINT *,'READ ROWS:',J-1
        CLOSE(11)
C FIND ACTUAL DATA RANGE FOR BETTER SCALING
        FG = ARR(1,1)
        BG = ARR(1,1)
        DO 30,J=1,JDIM
          DO 40,I=1,IDIM
            IF (ARR(I,J) .GT. FG) FG = ARR(I,J)
            IF (ARR(I,J) .LT. BG) BG = ARR(I,J)
40        CONTINUE
30      CONTINUE
        PRINT *,'Data range: BG=',BG,' FG=',FG
         IF (PGOPEN(OFNAM//'/CPS') .LE. 0) STOP
C SET UP PROPER VIEWPORT AND WINDOW
C PAPER SIZE 8 INCHES, ASPECT RATIO 1.0
        CALL PGPAP(8.0, 1.0)
C STANDARD VIEWPORT
        CALL PGSVP(0.0, 1.0, 0.0, 1.0)
C DATA WINDOW
        CALL PGSWIN(0.5, FLOAT(IDIM)+0.5, 0.5, FLOAT(JDIM)+0.5)
        CALL PGGRAY(ARR,IDIM,JDIM,1, IDIM, 1, JDIM, FG, BG, TR)
C PRINT *,ARR
c SUBROUTINE PGGRAY (A, IDIM, JDIM, I1, I2, J1, J2,
c 1 FG, BG, TR)
c INTEGER IDIM, JDIM, I1, I2, J1, J2
c REAL A(IDIM,JDIM), FG, BG, TR(6)
C The transformation matrix TR is used to calculate the world
C coordinates of the center of the "cell" that represents each
C array element. The world coordinates of the center of the cell
C corresponding to array element A(I,J) are given by:
C
C X = TR(1) + TR(2)*I + TR(3)*J
C Y = TR(4) + TR(5)*I + TR(6)*J
C
C Draw gray-scale map of an array in current window. The subsection
C of the array A defined by indices (I1:I2, J1:J2) is mapped onto
C the view surface world-coordinate system by the transformation
C matrix TR. The resulting quadrilateral region is clipped at the edge
C Usually TR(3) and TR(5) are zero -- unless the coordinate
C transformation involves a rotation or shear. The corners of the
C quadrilateral region that is shaded by PGGRAY are given by
C applying this transformation to (I1-0.5,J1-0.5), (I2+0.5, J2+0.5).
C
C Arguments:
C A (input) : the array to be plotted.
C IDIM (input) : the first dimension of array A.
C JDIM (input) : the second dimension of array A.
C I1, I2 (input) : the inclusive range of the first index
C (I) to be plotted.
C J1, J2 (input) : the inclusive range of the second
C index (J) to be plotted.
C FG (input) : the array value which is to appear with the
C foreground color (corresponding to color index 1).
C BG (input) : the array value which is to appear with the
C background color (corresponding to color index 0).
C TR (input) : transformation matrix between array grid and
C world coordinates.
        CALL PGIDEN
        CALL PGCLOS
C DEBUG: PRINT SOME DATA VALUES
        PRINT *,'Sample values:'
        PRINT *,'ARR(1,1)=',ARR(1,1)
        PRINT *,'ARR(IDIM/2,JDIM/2)=',ARR(IDIM/2,JDIM/2)
        PRINT *,'ARR(IDIM,JDIM)=',ARR(IDIM,JDIM)
        GOTO 9999
100     FORMAT(A)
9000    PRINT *,'ERROR: COULD NOT OPEN THE INPUT FILE'
        GOTO 9999
9100    PRINT *,'ERROR: END OF FILE ENCOUNTERED'
        GOTO 9999
9200    PRINT *,'ERROR: READING AN INTEGER INPUT NUMBER'
        PRINT *,'I:',I,' J:',J
9999    STOP
      END
