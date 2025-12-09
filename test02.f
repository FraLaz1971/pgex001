      PROGRAM TST2
        INTEGER I,J
        LOGICAL XR,YR,VR
        DATA XR/.FALSE./,YR/.FALSE./,VR/.FALSE./
        DO 20,J=1,10
          DO 10,I=1,40
            IF(MOD(I,5).EQ.0) THEN
            PRINT *,'XR=',XR
            PRINT *,'YR=',YR
            PRINT *,'VR=',VR
            IF((.NOT.XR).AND.(.NOT.YR).AND.(.NOT.VR)) THEN
                PRINT *,'X BRANCH'
                XR=.TRUE.
            ELSE IF (XR.AND.(.NOT.YR).AND.(.NOT.VR)) THEN
                PRINT *,'Y BRANCH'
                YR=.TRUE.
            ELSE IF (XR.AND.YR.AND.(.NOT.VR)) THEN
                PRINT *,'V BRANCH'
                XR=.FALSE.
                YR=.FALSE.
                VR=.FALSE.
            ELSE
                PRINT *,'UNEXPECTED BRANCH'
            END IF
          END IF
10        CONTINUE
20      CONTINUE
      END

