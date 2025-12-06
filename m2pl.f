C READS AN ASCII MATRIX 2D ARRAY (8 BITS ELEMENTS)
C AND WRITES A PIXEL LIST (X,Y,VAL)
C COLUMN ELEMENTS ARE SEPARATED BY SPACES (ascii decimal 32)
C ROWS ARE SEPARATED BY LF (ascii decimal 10 )
      PROGRAM CNV
        IMPLICIT NONE
        CHARACTER*80 IFNAM
	    PRINT *,'ENTER INPUT FILENAME'
        READ (*,*,END=9000,ERR=9100) IFNAM
        CALL M2PL(IFNAM)
        GOTO 9999
9000    PRINT *, 'INPUT FILE NAME NOT ENTERED: EXITING'
        GOTO 9999
9100    PRINT *, 'ERROR IN READING INPUT FILE NAME: EXITING'
9999    STOP
      END

      SUBROUTINE M2PL(IFNAM)
        IMPLICIT NONE
        CHARACTER*80 IFNAM,OFNAM
        CHARACTER*1 RBYTE
        INTEGER MXIDIM,MXJDIM,IVAL,I3VAL
C Maximum expected dimension
      PARAMETER(MXIDIM=5000,MXJDIM=5000)
C MXIDIM is the maximum dimension on the Y axis (width)
C MXJDIM is the maximum dimension on the X axis (height)
C        INTEGER ROW(MXIDIM)
        CHARACTER*(4*MXIDIM) LINE
        CHARACTER*5 BNUM
        INTEGER CN,CNT,SP,X
        LOGICAL DEBUG
        INTEGER I,J,POSI
        DEBUG = .FALSE.
        POSI=INDEX(IFNAM,'.')
        OFNAM=IFNAM(1:POSI)//'pl'
        OPEN(11,FILE=IFNAM,ERR=9000)
        OPEN(12,FILE=OFNAM,ERR=9500)
        BNUM=' '
        CN=1
        CNT=1
        X=1
        DO 10,J=1,MXJDIM
            SP=0
            LINE=' '
            READ(11,100,ERR=9100,END=30) LINE
            DO 20,I=1,LEN(LINE)
                RBYTE = LINE(I:I)
                IVAL = ICHAR(RBYTE)
                IF (DEBUG) PRINT *,'I:',I,'RBYTE: ',RBYTE,' IVAL ',IVAL
                IF ( (RBYTE.NE.CHAR(32)).AND.(RBYTE.NE.CHAR(10)) )
     &          THEN
                  BNUM(CN:CN) = RBYTE
                  CN = CN + 1
                ELSE IF ((CN.GT.1).AND. (RBYTE.EQ.CHAR(32)).AND.
     & (BNUM(CN-1:CN-1).NE.CHAR(32)) ) THEN
                    READ(BNUM, '(I3)', ERR=9300) I3VAL
                    IF (DEBUG) PRINT *,'BNUM:',BNUM,' I3VAL',I3VAL
                    WRITE(12,200,ERR=9400) X,J,MOD(I3VAL,256)
                    CNT = CNT + 1
                    X = X + 1
                    BNUM=' '
                    CN=1
                    SP=0
                ELSE
                    IF (DEBUG) PRINT *,'REST OF THE OPTIONS'
                    IF (SP.GT.4) THEN
                      X=1
                      GOTO 15
                    END IF
                    SP=SP+1
                END IF
20          CONTINUE
15          IF (DEBUG)  PRINT *,'PROCESSED LINE',J
10      CONTINUE
C END OF ROWS TO PROCESS: GOTO 30
30      CONTINUE
        CLOSE(11)
        CLOSE(12)
        PRINT *,'WROTE ',CNT-1,' ROWS/ELEMENTS'
100     FORMAT(A)
200     FORMAT(I5,1X,I5,1X,I3)
        GOTO 9999
9000    PRINT *,'ERROR IN OPENING INPUT FILE ',IFNAM
        GOTO 9999
9100    PRINT *,'ERROR IN READING THE ROW',J
        GOTO 9999
9300    PRINT *, 'ERROR CONVERTING NUMBER: ', BNUM
        PRINT *, 'AT POSITION: ', I, J
        GOTO 9999
9400    PRINT *,'ERROR IN WRITING PIXEL LIST DATA'
        GOTO 9999
9500    PRINT *, 'ERROR OPENING OUTPUT FILE ',OFNAM
9999    CONTINUE
      END
