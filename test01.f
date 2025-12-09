      PROGRAM TST1
        LOGICAL DEBUG
        INTEGER CN,CNT,SP,XC
        LOGICAL XR,YR,VR
        INTEGER X(10),Y(10),V(10),I,J
        CHARACTER*1 RBYTE
        INTEGER MXIDIM,MXJDIM,IVAL,I3VAL,I5VAL,J5VAL
        PARAMETER(MXIDIM=7,MXJDIM=3)
        CHARACTER*16 LINE
        CHARACTER*(5*MXIDIM) OLINE
        CHARACTER*80 OFNAM
        CHARACTER*5 BNUM
        DATA X/1,2,3,4,5,6,7,1,2,3/
        DATA Y/1,1,1,1,1,1,1,2,2,2/
        DATA V/25,69,238,40,61,152,226,160,105,101/
        DEBUG = .FALSE.
        OFNAM='outfile.asc'
        OPEN(12,FILE=OFNAM)
        BNUM=' '
        CN=1
        CNT=1
        XC=1
        J5VAL=-1
        J5VALM=-1
        OLINE = ' '
        XR=.FALSE.
        YR=.FALSE.
        VR=.FALSE.
        IO=1
        DO 10,J=1,10
            WRITE(LINE(1:15),300)X(J),Y(J),V(J)
C            PRINT *,'INPUT LINE: ',LINE
          DO 20,I=1,LEN(LINE)
                RBYTE = LINE(I:I)
                IVAL = ICHAR(RBYTE)
C                IF(IVAL.NE.32) PRINT *,'I:',I,'RBYTE: ',
C     & RBYTE,' IVAL ',IVAL
                IF ( (RBYTE.NE.CHAR(32)).AND.(RBYTE.NE.CHAR(10)) )
     &          THEN
                  BNUM(CN:CN) = RBYTE
                  CN = CN + 1
                ELSE IF ((CN.GT.1).AND. (RBYTE.EQ.CHAR(32)).AND.
     & (BNUM(CN-1:CN-1).NE.CHAR(32)) ) THEN
                  IF(DEBUG)PRINT *,'FOUND A NUMBER! :',BNUM
                  IF(DEBUG)PRINT *,'XR',XR
                  IF(DEBUG)PRINT *,'YR',YR
                  IF(DEBUG)PRINT *,'VR',VR
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
                          PRINT *,'WRITING OLINE: ',OLINE
                          WRITE(12,200) OLINE
                         OLINE = ' '
                         IO=1
                   END IF
                  J5VALM=J5VAL
                  YR=.TRUE.
             ELSE IF (XR.AND.YR.AND.(.NOT.VR)) THEN
                  READ(BNUM, '(I3)', ERR=9300) I3VAL
C                  PRINT *,'BNUM:',BNUM,' I3VAL',I3VAL
                  IF(DEBUG)PRINT *,'READ THE MATRIX VALUE :',I3VAL
                  WRITE(OLINE((IO-1)*5+1:(IO-1)*5+5),100) I3VAL
                  PRINT *,'(',(IO-1)*5+1,':',(IO-1)*5+5,')'
                  PRINT *,'OLINE :',OLINE,'IO=',IO
                  IO=IO+1
                  XR=.FALSE.
                  YR=.FALSE.
                  VR=.FALSE.
             ELSE
                  PRINT *,'XR=YR=VR=.TRUE.'
                  CONTINUE
             END IF
             CNT = CNT + 1
             XC = XC + 1
C ERASE THE STRING CONTAINING THE NUMBER
C AND ITS COUNTER CN
             BNUM=' '
             CN=1
             SP=0
                ELSE
                    IF (DEBUG) PRINT *,'REST OF THE OPTIONS',SP
                    IF (SP.GT.4) THEN
                      XC=1
                      GOTO 15
                    END IF
                    SP=SP+1
                END IF
20        CONTINUE
15          IF (DEBUG)  PRINT *,'PROCESSED INPUT LINE',J
c15          CONTINUE
10      CONTINUE
        PRINT *,'WRITING OLINE: ',OLINE
        WRITE(12,200) OLINE
        CLOSE(12)
100     FORMAT(I4,1X)
200     FORMAT(A)
300     FORMAT(I5,1X,I5,1X,I3)
        GOTO 9999
9000    PRINT *,'ERROR IN WRITING ON OUTPUT FILE ',OFNAM
        GOTO 9999
9300    PRINT *, 'ERROR CONVERTING NUMBER: ', BNUM
        PRINT *, 'AT POSITION: ', I, J
        GOTO 9999
9999    STOP
        END
