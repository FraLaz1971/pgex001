      PROGRAM PGSCM
        IMPLICIT NONE
C PLOT A MAP OF A 2D ARRAY WITH PGPLOT IN GRAY SCALES
        INTEGER I,J,IDIM,JDIM,POSI
C IDIM:DIMENSION OF X AXES(WIDTH), JDIM:DIMENSION OF Y AXES(HEIGHT)
C J scan the Y axes. I scan the x axes.
        REAL ARR(2096,2338)
        CHARACTER*4 ROW(2096)
        CHARACTER*8384 LINE
        CHARACTER*80 IFNAM,OFNAM
        LOGICAL DEBUG
        REAL FG, BG, TR(6)
        INTEGER CI
        REAL    CR, CG, CB, X
        INTEGER   ICILO, ICIHI
        INTEGER PGOPEN
C
C Call PGOPEN to initiate PGPLOT and open the output device; PGOPEN
C will prompt the user to supply the device name and type. Always
C check the return code from PGOPEN.
C
        DEBUG=.FALSE.
C        DATA ARR /4900448*0.0/
        DATA IDIM /2096/
        DATA JDIM /2338/
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
        IF (DEBUG) PRINT *,'j:',J
            READ(11,100,END=9100) LINE
            READ(LINE,'(2096A)') ROW
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
        CALL PGSCIR(0, 255)
        CALL PGQCIR(ICILO, ICIHI)
        PRINT *,'COLOR INDEX LOW:',ICILO
        PRINT *,'COLOR INDEX HIGH:',ICIHI
C CREATE CONTINUOUS COLOR TABLE
        DO 50 CI = 0, 255
            X = REAL(CI) / 255.0
C Rainbow from blue to red
            IF (X .LT. 0.25) THEN
                CR = 0.0
                CG = 4.0 * X
                CB = 1.0
            ELSE IF (X .LT. 0.5) THEN
                CR = 0.0
                CG = 1.0
                CB = 1.0 - 4.0 * (X - 0.25)
            ELSE IF (X .LT. 0.75) THEN
                CR = 4.0 * (X - 0.5)
                CG = 1.0
                CB = 0.0
            ELSE
                CR = 1.0
                CG = 1.0 - 4.0 * (X - 0.75)
                CB = 0.0
            END IF
            CALL PGSCR(CI, CR, CG, CB)
            IF (DEBUG) PRINT *,'CI:',CI
            IF (DEBUG) PRINT 110,CR,CG,CB
50      CONTINUE
        CI=0
        CR = 0.0
        CG = 0.0
        CB = 0.0
        CALL PGSCR(CI, CR, CG, CB)

C        SUBROUTINE PGSCR (CI, CR, CG, CB)
C        INTEGER CI
C        REAL    CR, CG, CB
        CALL PGSITF (0)
        CALL PGIMAG(ARR,IDIM,JDIM,1, IDIM, 1, JDIM, BG, FG, TR)
C        CALL PGIMAG(ARR,IDIM,JDIM,1, IDIM, 1, JDIM, FG, BG, TR)
C
c      SUBROUTINE PGIMAG (A, IDIM, JDIM, I1, I2, J1, J2,
c     1                   A1, A2, TR)
c      INTEGER IDIM, JDIM, I1, I2, J1, J2
c      REAL    A(IDIM,JDIM), A1, A2, TR(6)

C Draw a color image of an array in current window. The subsection
C of the array A defined by indices (I1:I2, J1:J2) is mapped onto
C the view surface world-coordinate system by the transformation
C matrix TR. The resulting quadrilateral region is clipped at the edge
C of the window. Each element of the array is represented in the image
C by a small quadrilateral, which is filled with a color specified by
C the corresponding array value.
C
C The subroutine uses color indices in the range C1 to C2, which can
C be specified by calling PGSCIR before PGIMAG. The default values
C for C1 and C2 are device-dependent; these values can be determined by
C calling PGQCIR. Note that color representations should be assigned to
C color indices C1 to C2 by calling PGSCR before calling PGIMAG. On some
C devices (but not all), the color representation can be changed after
C the call to PGIMAG by calling PGSCR again.
C
C Array values in the range A1 to A2 are mapped on to the range of
C color indices C1 to C2, with array values <= A1 being given color
C index C1 and values >= A2 being given color index C2. The mapping
C function for intermediate array values can be specified by
C calling routine PGSITF before PGIMAG; the default is linear.
C
C On devices which have no available color indices (C1 > C2),
C PGIMAG will return without doing anything. On devices with only
C one color index (C1=C2), all array values map to the same color
C which is rather uninteresting. An image is always "opaque",
C i.e., it obscures all graphical elements previously drawn in
C the region.
C
C The transformation matrix TR is used to calculate the world
C coordinates of the center of the "cell" that represents each
C array element. The world coordinates of the center of the cell
C corresponding to array element A(I,J) are given by:
C
C          X = TR(1) + TR(2)*I + TR(3)*J
C          Y = TR(4) + TR(5)*I + TR(6)*J
C
C Usually TR(3) and TR(5) are zero -- unless the coordinate
C transformation involves a rotation or shear.  The corners of the
C quadrilateral region that is shaded by PGIMAG are given by
C applying this transformation to (I1-0.5,J1-0.5), (I2+0.5, J2+0.5).
C
C Arguments:
C  A      (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  A1     (input)  : the array value which is to appear with shade C1.
C  A2     (input)  : the array value which is to appear with shade C2.
C  TR     (input)  : transformation matrix between array grid and
C                    world coordinates.
        CALL PGIDEN
        CALL PGCLOS
C DEBUG: PRINT SOME DATA VALUES
        PRINT *,'Sample values:'
        PRINT *,'ARR(1,1)=',ARR(1,1)
        PRINT *,'ARR(IDIM/2,JDIM/2)=',ARR(IDIM/2,JDIM/2)
        PRINT *,'ARR(IDIM,JDIM)=',ARR(IDIM,JDIM)
        PRINT *,'Image created in file:',OFNAM
        GOTO 9999
100     FORMAT(A)
110     FORMAT('CR:',F4.2,1X,'CG:',F4.2,1X,'CB:',F4.2,1X)
9000    PRINT *,'ERROR: COULD NOT OPEN THE INPUT FILE'
        GOTO 9999
9100    PRINT *,'ERROR: END OF FILE ENCOUNTERED'
        GOTO 9999
9200    PRINT *,'ERROR: READING AN INTEGER INPUT NUMBER'
        PRINT *,'I:',I,' J:',J
9999    STOP
      END
