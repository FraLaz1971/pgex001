C READS AN ASCII MATRIX 2D ARRAY (8 BITS ELEMENTS)
C AND WRITES A PIXEL LIST (X,Y,VAL)
C COLUMN ELEMENTS ARE SEPARATED BY SPACES (ascii decimal 32)
C ROWS ARE SEPARATED BY LF (ascii decimal 10 )
      PROGRAM CNV2
        IMPLICIT NONE
        CHARACTER*80 IFNAM
	    PRINT *,'ENTER PIXEL LIST INPUT FILENAME'
        READ (*,*,END=9000,ERR=9100) IFNAM
        CALL PL2M(IFNAM)
        GOTO 9999
9000    PRINT *, 'INPUT FILE NAME NOT ENTERED: EXITING'
        GOTO 9999
9100    PRINT *, 'ERROR IN READING INPUT FILE NAME: EXITING'
9999    STOP
      END

      SUBROUTINE PL2M(IFNAM)
        IMPLICIT NONE
        LOGICAL XR,YR,VR
        CHARACTER*80 IFNAM,OFNAM
        CHARACTER*1 RBYTE
        INTEGER MAXDIM,MXIDIM,MXJDIM
        INTEGER I5VAL,I5VALM,J5VAL,J5VALM,I3VAL,IVAL
C Maximum expected dimension
      PARAMETER(MXIDIM=5000,MXJDIM=5000,MAXDIM=25000000)
C MXIDIM is the maximum dimension on the Y axis (width)
C MXJDIM is the maximum dimension on the X axis (height)
C MAXJDIM is the maximum dimension on the toal 2D array (width*height)
        CHARACTER*5 BNUM
        CHARACTER*16 LINE
        CHARACTER*(4*MXIDIM) OLINE
C        INTEGER ROW(MXIDIM)
        INTEGER CN,CNT,SP,X
        LOGICAL DEBUG
        INTEGER I,IO,J,JO,POSI
        DEBUG = .FALSE.
        POSI=INDEX(IFNAM,'.')
        OFNAM=IFNAM(1:POSI)//'asc'
        OPEN(11,FILE=IFNAM,ERR=9000)
        OPEN(12,FILE=OFNAM,ERR=9500)
        BNUM=' '
        CN=1
        CNT=1
        X=1
        J5VAL=-1
        J5VALM=-1
        OLINE = ' '
        XR=.FALSE.
        YR=.FALSE.
        VR=.FALSE.
        IO=1
        DO 10,J=1,MAXDIM
            READ(11,200,ERR=9100,END=30) LINE
            IF (DEBUG) PRINT *,'LINE:',LINE
          DO 20,I=1,LEN(LINE)
                RBYTE = LINE(I:I)
                IVAL = ICHAR(RBYTE)
                IF (DEBUG) PRINT *,'I:',I,'RBYTE: ',
     & RBYTE,' IVAL ',IVAL
                IF ( (RBYTE.NE.CHAR(32)).AND.(RBYTE.NE.CHAR(10)) )
     &          THEN
                  BNUM(CN:CN) = RBYTE
                  CN = CN + 1
                ELSE IF ((CN.GT.1).AND. (RBYTE.EQ.CHAR(32)).AND.
     & (BNUM(CN-1:CN-1).NE.CHAR(32)) ) THEN
                  IF (DEBUG) PRINT *,'FOUND A NUMBER! :',BNUM
                  IF (DEBUG) PRINT *,'XR',XR
                  IF (DEBUG) PRINT *,'YR',YR
                  IF (DEBUG) PRINT *,'VR',VR
             IF((.NOT.XR).AND.(.NOT.YR).AND.(.NOT.VR)) THEN
                  IF(DEBUG)PRINT *,'X BRANCH'
                  READ(BNUM, '(I5)', ERR=9300) I5VAL
                  IF(DEBUG)PRINT *,'READ THE X COORDINATE :',I5VAL
                  XR = .TRUE.
             ELSE IF (XR.AND.(.NOT.YR).AND.(.NOT.VR)) THEN
                  READ(BNUM, '(I5)', ERR=9300) J5VAL
C                  PRINT *,'J5VALM:',J5VALM,'J5VAL:',J5VAL
                  IF(DEBUG)PRINT *,'READ THE Y COORDINATE :',J5VAL
                  IF((ABS(J5VALM-J5VAL).EQ.1)) THEN
C                          PRINT *,'WRITING OLINE: ',OLINE
                          IF (DEBUG) PRINT *,'IO: ',IO
                          WRITE(12,200,ERR=9400) OLINE(1:(IO-1)*4)
                         OLINE = ' '
                         IO=1
                   END IF
                  J5VALM=J5VAL
                  YR=.TRUE.
             ELSE IF (XR.AND.YR.AND.(.NOT.VR)) THEN
                  READ(BNUM, '(I3)', ERR=9300) I3VAL
C                  PRINT *,'BNUM:',BNUM,' I3VAL',I3VAL
                  IF (DEBUG) PRINT *,'READ THE MATRIX VALUE :',I3VAL
                  WRITE(OLINE((IO-1)*4+1:(IO-1)*4+4),100) I3VAL
C                  PRINT *,'(',(IO-1)*5+1,':',(IO-1)*5+5,')'
C                  PRINT *,'OLINE :',OLINE,'IO=',IO
                  IO=IO+1
                  XR=.FALSE.
                  YR=.FALSE.
                  VR=.FALSE.
             ELSE
                  IF (DEBUG) PRINT *,'XR=YR=VR=.TRUE.'
                  CONTINUE
             END IF
             CNT = CNT + 1
             X = X + 1
C ERASE THE STRING CONTAINING THE NUMBER
C AND ITS COUNTER CN
             BNUM=' '
             CN=1
             SP=0
                ELSE
                    IF (DEBUG) PRINT *,'REST OF THE OPTIONS',SP
                    IF (SP.GT.4) THEN
                      X=1
                      GOTO 15
                    END IF
                    SP=SP+1
                END IF
20        CONTINUE
15        IF (DEBUG) PRINT *,'PROCESSED INPUT LINE',J
10      CONTINUE
C END OF ROWS TO PROCESS: GOTO 30
30      CONTINUE
        IF (DEBUG) PRINT *,'IO: ',IO
        WRITE(12,200,ERR=9400) OLINE(1:(IO-1)*4)
        CLOSE(11)
        CLOSE(12)
        PRINT *,'PROCESSED ',CNT-1,' ELEMENTS'
100     FORMAT(I4,1X)
200     FORMAT(A)
300     FORMAT(I5,1X,I5,1X,I3)
        GOTO 9999
9000    PRINT *,'ERROR IN OPENING INPUT FILE ',IFNAM
        GOTO 9999
9100    PRINT *,'ERROR IN READING THE ROW',J
        GOTO 9999
9300    PRINT *, 'ERROR CONVERTING NUMBER: ', BNUM
        PRINT *, 'AT POSITION: ', I, J
        GOTO 9999
9400    PRINT *,'ERROR IN WRITING MATRIX DATA'
        GOTO 9999
9500    PRINT *, 'ERROR OPENING OUTPUT FILE ',OFNAM
9999    CONTINUE
      END
