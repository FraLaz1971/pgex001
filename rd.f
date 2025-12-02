      PROGRAM RD
C Maximum expected dimension
C AINDIM,AJDIM: ACTUAL DIMENSION
      INTEGER INDIM,JDIM,AINDIM,AJDIM
      CHARACTER*80 IFNAM
      LOGICAL DEBUG
      DEBUG=.TRUE.
      PRINT *,'ENTER THE ASCII INPUT FILE NAME'
      READ *,IFNAM
      DEBUG=.FALSE.
C READ DIMENSIONS SCANNING THE FILE
C READ THE ARRAY FROM ASCII FILE
C      CALL GETDIM(IFNAM,AINDIM,AJDIM)
      CALL GUDIM(IFNAM,AINDIM,AJDIM)
      INDIM = AINDIM
      JDIM = AJDIM
      CALL PPC2DA(INDIM,JDIM,IFNAM)      
      STOP
      END
      
      SUBROUTINE GETDIM(IFNAM,AINDIM,AJDIM)
C THIS SUBROUTINE TAKES AS ARGUMENTS:
C INPUT ARGUMENTS
C IFNAME: input file name containing a 2D ascii matrix array
C to check the dimensions
C OUTPUT ARGUMENTS
C AINDIM: this returns as output the actual dimension of the x axis (width)
C AJDIM: this returns as output the actual dimension of the y axis (height)
C WE CAN ADD THAT AJDIM IS THE NUMBER OF ROWS
C AND AINDIM IS THE NUMBER OF COLUMNS
C READ THE DIMENSIONS OF A 2D ARRAY (MATRIX) IN ASCII FORMAT
C THIS FORMAT IS SUPPORTED BY gnuplot FOR READING
      INTEGER MXIDIM,MXJDIM
C Maximum expected dimension
      PARAMETER(MXIDIM=500,MXJDIM=320)
C AINDIM,AJDIM: ACTUAL DIMENSION
      INTEGER I,J,INDIM,JDIM,AINDIM,AJDIM
      REAL ARR(MXIDIM,MXJDIM)
      CHARACTER*4 ROW(MXIDIM)
      CHARACTER*(4*MXIDIM) LINE
      CHARACTER*80 IFNAM
      LOGICAL DEBUG
      INDIM=MXIDIM
      JDIM=MXJDIM
      DEBUG=.FALSE.
C READ DIMENSIONS SCANNING THE FILE
C READ THE ARRAY FROM ASCII FILE
        OPEN(11,FILE=IFNAM,ERR=9000)
        DO 10,J=1,JDIM
            IF (DEBUG) PRINT *,'J:',J
            READ(11,100,END=200) LINE
            READ(LINE,'(5000A)',END=9300) ROW
            IF (J.EQ.1) THEN
              DO 50,I=1,INDIM
                PRINT 110,ROW(I)
                IF (ROW(I).EQ.'    ') GOTO 60
50            CONTINUE
60            AINDIM=I-1
            ENDIF
            DO 20,I=1,AINDIM
              READ(ROW(I),'(F4.0)',ERR=9200) ARR(I,J)
20          CONTINUE
10      CONTINUE
200     PRINT *,'END OF FILE MET'
        AJDIM=J-1
        IF (DEBUG) PRINT *,'FOUND ROWS:',AJDIM
        CLOSE(11)
        PRINT 120,AINDIM,AJDIM
      IF (AINDIM .GT. MXIDIM .OR. AJDIM .GT. MXJDIM) THEN
          PRINT *,'ERROR: DIMENSIONS EXCEED MAXIMUM ALLOWED'
          PRINT *,'ACTUAL: ',AINDIM,'x',AJDIM
          PRINT *,'MAXIMUM: ',MXIDIM,'x',MXJDIM
          STOP
      ENDIF
        GOTO 9999
100     FORMAT(A)
110     FORMAT(A,1X)
120     FORMAT('ACTUAL MATRIX DIMENSIONS, WIDTH:',I5,' HEIGHT:',I5)
9000    PRINT *,'ERROR: COULD NOT OPEN THE INPUT FILE'
        GOTO 9999
9200    PRINT *,'ERROR: PROBLEM IN SAVING ROW ELEMENTS'
        PRINT *,'INTO REAL ARRAY ELEMENTS'
        GOTO 9999
9300    PRINT *,'ERROR: END OF FILE ENCOUNTERED WHILE'
        PRINT *,'TOKENIZING THE LINE IN AN ARRAY OF STRINGS'
        GOTO 9999
9999    STOP
      END
      
      SUBROUTINE GUDIM(IFNAM,AINDIM,AJDIM)
      IMPLICIT NONE
C GETTING DIMENSION OF THE ASCII MATRIX
C SCANNING THE FILE AS UNFORMATTED
      CHARACTER*1 RBYTE
      CHARACTER*3 BNUM
C CHARACTER COUNTER
      INTEGER CN,CNT
C ROW COUNTER
      INTEGER CR
C Maximum expected dimension
      INTEGER MXIDIM,MXJDIM
C Maximum expected dimension
      PARAMETER(MXIDIM=500,MXJDIM=320)
C AINDIM,AJDIM: ACTUAL DIMENSION
      INTEGER I,J,INDIM,JDIM,AINDIM,AJDIM
      REAL ARR(MXIDIM,MXJDIM)
      CHARACTER*4 ROW(MXIDIM)
      CHARACTER*(4*MXIDIM) LINE
      CHARACTER*80 IFNAM
      LOGICAL DEBUG
      INDIM=MXIDIM
      JDIM=MXJDIM
      DEBUG=.FALSE.
C READ DIMENSIONS SCANNING THE FILE
C READ THE ARRAY FROM ASCII FILE
        OPEN(11,FILE=IFNAM,FORM='UNFORMATTED',ACCESS='DIRECT',
     &   RECL=1,ERR=9000)
C SCAN THE WHOLE FILE
            BNUM=' '
            CN=1
            CR=1
            I=1
            J=1
            DO 10,CNT=1,4*MAX(MXIDIM,MXJDIM)**2
                READ(11,REC=CNT,ERR=200) RBYTE
                LINE(CR:)=RBYTE
                CR = CR + 1
                IF ((ICHAR(RBYTE).NE.32).AND.(ICHAR(RBYTE).NE.10)) THEN
                    BNUM(CN:)=RBYTE
                    CN = CN + 1
                ELSE IF (ICHAR(RBYTE).EQ.10) THEN
                    IF (DEBUG) PRINT 300,CNT,RBYTE,ICHAR(RBYTE)
                        IF (DEBUG) PRINT *, 'LINE: ',LINE(:I*4)
                        LINE=' '
                        INDIM=I-1
                        I=1
                        J=J+1
                        CR=1
                ELSE IF (CN.GT.1) THEN
                    BNUM(CN:)=' '
                    IF (DEBUG) PRINT *, 'BNUM: ',BNUM
                    BNUM=' '
                    CN=1
                    I=I+1
                ELSE
                    CONTINUE
                END IF
                IF (DEBUG) PRINT 300,CNT,RBYTE,ICHAR(RBYTE)
10          CONTINUE
200         PRINT *,'READ CHARACTERS:',CNT-1
            JDIM=J-1
          PRINT *,'DIMENSIONS: ',INDIM,'x',JDIM
          AINDIM=INDIM
          AJDIM=JDIM
        CLOSE(11)
        PRINT 120,AINDIM,AJDIM
      IF (AINDIM .GT. MXIDIM .OR. AJDIM .GT. MXJDIM) THEN
          PRINT *,'ERROR: DIMENSIONS EXCEED MAXIMUM ALLOWED'
          PRINT *,'ACTUAL: ',AINDIM,'x',AJDIM
          PRINT *,'MAXIMUM: ',MXIDIM,'x',MXJDIM
          STOP
      ENDIF
        GOTO 9999
120     FORMAT('ACTUAL MATRIX DIMENSIONS, WIDTH:',I5,' HEIGHT:',I5)
300     FORMAT('CNT:',I5,' READ:',A5,' : ',I5)
9000    PRINT *,'ERROR: COULD NOT OPEN THE INPUT FILE'
        GOTO 9999
9999    CONTINUE
       END
C SUBROUTINE PGPLOT IN GRAYSCALES 2D ARRAY
      SUBROUTINE PPC2DA(INDIM,JDIM,IFNAM)
C INPUT ARGUMENTS
C INDIM: the dimension of the x axis (width)
C JDIM: the  dimension of the y axis (height)
C IFNAM: input file name containing a 2D ascii matrix array
C to plot
C WE CAN ADD THAT JDIM IS THE NUMBER OF ROWS
C and INDIM IS THE NUMBER OF COLUMNS
      IMPLICIT NONE
      CHARACTER*1 RBYTE
      CHARACTER*5 BNUM
      LOGICAL DEBUG
C CHARACTER COUNTER
      INTEGER CN,CNT
C ROW COUNTER
      INTEGER CR
C Maximum expected dimension
      INTEGER MXIDIM,MXJDIM
      PARAMETER(MXIDIM=500,MXJDIM=320)
C AINDIM,AJDIM: ACTUAL DIMENSION
      INTEGER I,J,INDIM,JDIM,AINDIM,AJDIM,POSI
      REAL ARR(MXIDIM,MXJDIM)
C      REAL ARR(2096,2338)
      CHARACTER*80 IFNAM,OFNAM
      CHARACTER*(4*MXIDIM) LINE
      REAL FG, BG, TR(6)
      INTEGER PGOPEN
      DATA TR /0.0, 1.0, 0.0, 0.0, 0.0, 1.0/ 
      INDIM=MXIDIM
      JDIM=MXJDIM
      DEBUG=.FALSE.
C READ THE ARRAY FROM ASCII FILE
        PRINT *,'PLOTTING IMAGE FILE ',IFNAM
        PRINT *,'NOW READING'
        OPEN(11,FILE=IFNAM,FORM='UNFORMATTED',ACCESS='DIRECT',
     &   RECL=1,ERR=9000)
C SCAN THE WHOLE FILE
            BNUM=' '
            CN=1
            CR=1
            I=1
            J=1
            DO 10,CNT=1,4*MAX(INDIM,JDIM)**2
                READ(11,REC=CNT,ERR=200) RBYTE
                LINE(CR:)=RBYTE
                CR = CR + 1
                IF ((ICHAR(RBYTE).NE.32).AND.(ICHAR(RBYTE).NE.10)) THEN
                    BNUM(CN:)=RBYTE
                    CN = CN + 1
                ELSE IF (ICHAR(RBYTE).EQ.10) THEN
                    IF (DEBUG) PRINT 300,CNT,RBYTE,ICHAR(RBYTE)
                        IF (DEBUG) PRINT *, 'LINE: ',LINE(:I*4)
                        LINE=' '
                        INDIM=I-1
                        I=1
                        J=J+1
                        CR=1
                ELSE IF (CN.GT.1) THEN
                    BNUM(CN:)=' '
                    IF (DEBUG) PRINT *, 'BNUM: ',BNUM
                    READ(BNUM,'(F4.0)')ARR(I,J)
                    BNUM=' '
                    CN=1
                    I=I+1
                ELSE
                    CONTINUE
                END IF
                IF (DEBUG) PRINT 300,CNT,RBYTE,ICHAR(RBYTE)
10          CONTINUE
200         CONTINUE
            JDIM=J-1
          AINDIM=INDIM
          AJDIM=JDIM
        CLOSE(11)
      IF (AINDIM .GT. MXIDIM .OR. AJDIM .GT. MXJDIM) THEN
          PRINT *,'ERROR: DIMENSIONS EXCEED MAXIMUM ALLOWED'
          PRINT *,'ACTUAL: ',AINDIM,'x',AJDIM
          PRINT *,'MAXIMUM: ',MXIDIM,'x',MXJDIM
          STOP
      ENDIF
C FIND ACTUAL DATA RANGE FOR BETTER SCALING
        FG = ARR(1,1)
        BG = ARR(1,1)
        DO 30,J=1,JDIM
          DO 40,I=1,INDIM
            IF (ARR(I,J) .GT. FG) FG = ARR(I,J)
            IF (ARR(I,J) .LT. BG) BG = ARR(I,J)
40        CONTINUE
30      CONTINUE
        PRINT *,'Data range: BG=',BG,' FG=',FG
C create output file name
         POSI=INDEX(IFNAM,'.')
         OFNAM=IFNAM(1:POSI)//'ps'
         IF (PGOPEN(OFNAM//'/CPS') .LE. 0) STOP
C SET UP PROPER VIEWPORT AND WINDOW
C PAPER SIZE 8 INCHES, ASPECT RATIO 1.0
        CALL PGPAP(8.0, 1.0)
C STANDARD VIEWPORT
        CALL PGSVP(0.0, 1.0, 0.0, 1.0)
C DATA WINDOW
        CALL PGSWIN(0.5, FLOAT(INDIM)+0.5, 0.5, FLOAT(JDIM)+0.5)
C        CALL PGGRAY(ARR,INDIM,JDIM,1, INDIM, 1, JDIM, FG, BG, TR)
        CALL PGGRAY(ARR,MXIDIM,MXJDIM,1, INDIM, 1, JDIM, FG, BG, TR)
        CALL PGIDEN
        CALL PGCLOS
C DEBUG: PRINT SOME DATA VALUES
        PRINT *,'Sample values:'
        PRINT *,'ARR(1,1)=',ARR(1,1)
        PRINT *,'ARR(INDIM/2,JDIM/2)=',ARR(INDIM/2,JDIM/2)
        PRINT *,'ARR(INDIM,JDIM)=',ARR(INDIM,JDIM)
        GOTO 9999
300     FORMAT('CNT:',I5,' READ:',A5,' : ',I5)
9000    PRINT *,'ERROR: COULD NOT OPEN THE INPUT FILE'
        GOTO 9999
9999    STOP
       END
