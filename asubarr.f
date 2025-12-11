C maximum dimension of the array is 5000x5000
      PROGRAM TB
	    IMPLICIT NONE
        CHARACTER*80 IFNAM
	    INTEGER FY,LY,FX,LX
	    PRINT *,'ENTER INPUT FILENAME'
	    READ *,IFNAM
	    PRINT *,'ENTER FIRST X INDEX'
	    READ *,FX
	    PRINT *,'ENTER LAST X INDEX'
	    READ *,LX
	    PRINT *,'ENTER FIRST Y INDEX'
	    READ *,FY
	    PRINT *,'ENTER LAST Y INDEX'
	    READ *,LY
	    CALL M2SM(IFNAM,FX,LX,FY,LY)
        STOP
      END
C READ ASCII 8 BITS ARRAY (MATRIX)
C WITH FIRST Y AND LAST X
C      FIRST X AND LAST X
C AS INPUT
C AND SAVE TO ASCII MATRIX FILE
      SUBROUTINE M2SM(IFNAM,FX,LX,FY,LY)
        IMPLICIT NONE
        INTEGER FY,LY,FX,LX,X,Y
        CHARACTER*80 IFNAM,OFNAM
        CHARACTER*1 RBYTE
        CHARACTER*10 SUF1
        INTEGER MXIDIM,MXJDIM,IVAL,I3VAL,POSI
C Maximum expected dimension
        PARAMETER(MXIDIM=5000,MXJDIM=5000)
C MXIDIM is the maximum dimension on the Y axis (width)
C MXJDIM is the maximum dimension on the X axis (height)
        CHARACTER*(4*MXIDIM) LINE
        CHARACTER*(4*MXIDIM) OLINE
        CHARACTER*4 BNUM
        INTEGER CN,CNT,CNT2,SP
        LOGICAL DEBUG, F
C WIDTH AND HEIGHT ARE DIMENSION OF THE SUBARRAY
C TW IS THE TOTAL WIDTH OF THE INPUT MATRIX
C TH IS THE TOTAL HEIGHT OF THE INPUT MATRIX
        INTEGER I,J,WIDTH,HEIGHT,TW,TH
        WIDTH=LX-FX+1
        HEIGHT=LY-FY+1
        DEBUG = .FALSE.
C Create output file name, same basename, pre extension,.asc extension
        WRITE(SUF1,'(BZ,I4,''x'',I4,''_'')')WIDTH,HEIGHT
        DO 40,I=1,LEN(SUF1)
          IF (SUF1(I:I).EQ.' ')SUF1(I:I)='_'
40      CONTINUE
        POSI=INDEX(IFNAM,'.')
        OFNAM=IFNAM(1:POSI-1)//SUF1//'.asc'
        OPEN(11,FILE=IFNAM,ERR=9000)
        OPEN(12,FILE=OFNAM,ERR=9500)
        F=.FALSE.
        BNUM=' '
        CN=1
        CNT=0
        TW=20000
        TH=5000
C POSITION OF THE OUTPUT SUBARR IS
C X=MOD(CNT,TW)
C Y=CNT/TW+1
        IF (DEBUG) PRINT *,'FX: ',FX,' LX: ',LX,' FY: ',FY,' LY: ',LY
        DO 10,J=1,MXJDIM
            SP=0
            LINE=' '
            READ(11,100,ERR=9100,END=30) LINE
       IF((J.GE.FY).AND.(J.LE.LY)) THEN
            DO 20,I=1,LEN(LINE)
                RBYTE = LINE(I:I)
                READ(RBYTE, '(I1)') IVAL
                IF(DEBUG)PRINT *,'I:',I,'RBYTE: ',RBYTE,' IVAL ',IVAL
                IF (RBYTE.NE.CHAR(32)) THEN
                  BNUM(CN:CN) = RBYTE
                  CN = CN + 1
                ELSE IF ((CN.GT.1).AND.(RBYTE.EQ.CHAR(32)).AND.
     & (BNUM(CN-1:CN-1).NE.CHAR(32)) ) THEN
                    X=MOD(CNT,TW)+1
                    READ(BNUM, '(I3)', ERR=9300) I3VAL
                    IF (DEBUG) PRINT *,'BNUM:',BNUM,' I3VAL'
     &,I3VAL,' X',X,' Y',J
                    IF (DEBUG) PRINT *,'WIDTH: ',WIDTH
                    IF((X.GE.FX).AND.(X.LE.(FX+WIDTH-1))) THEN
                    PRINT 200,MOD((X-FX)*4,WIDTH*4)+1,
     &MOD((X-FX)*4,WIDTH*4)+4,MOD((X-FX),WIDTH*4)+1,J
                    PRINT *,'writing ',I3VAL
                      WRITE(OLINE(MOD((X-FX)*4,WIDTH*4)+1:
     &MOD((X-FX)*4,WIDTH*4)+4),'(I3,1X)',ERR=9400) I3VAL
                    ELSE
                      CONTINUE
                    END IF
                    CNT = CNT + 1
                    BNUM=' '
                    CN=1
                    SP=0
                ELSE
                    IF (DEBUG) PRINT *,'REST OF THE OPTIONS'
                    IF (SP.GT.4) THEN
                    IF (.NOT.F) THEN
                        TW=CNT
                        IF (DEBUG) PRINT *,'TOTAL WIDTH:',TW
                        F=.TRUE.
                    END IF
                    GOTO 15
                    END IF
                    SP=SP+1
                END IF
20          CONTINUE
15            IF (DEBUG) PRINT *,'PROCESSED LINE',J
              WRITE(12,'(A)',ERR=9400) OLINE(1:WIDTH*4)
             END IF
10      CONTINUE
C END OF ROWS TO PROCESS: GOTO 30
30      CONTINUE
        TH=J-1
        CLOSE(11)
        CLOSE(112)
        PRINT *,'PROCESSED ',CNT,' BYTES/ELEMENTS'
        PRINT *,'TOTAL WIDTH',TW,'TOTAL HEIGHT',TH
        PRINT *,'SUBARRAY WIDTH',WIDTH,'SUBARRAY HEIGHT',HEIGHT

        GOTO 9999
100     FORMAT(A)
200     FORMAT('(X-1)*4+1 ',I5,' (X-1)*4+4 ',I5,' X=',I4,' Y=',I4)
9000    PRINT *,'ERROR IN OPENING INPUT FILE ',IFNAM
        GOTO 9999
9100    PRINT *,'ERROR IN READING THE ROW',J
        GOTO 9999
9300    PRINT *, 'ERROR CONVERTING NUMBER: ', BNUM
        PRINT *, 'AT POSITION: ', I, J
        GOTO 9999
9400    PRINT *,'ERROR IN WRITING ASCII DATA'
9500    PRINT *, 'ERROR OPENING OUTPUT FILE'
9999    STOP
      END
