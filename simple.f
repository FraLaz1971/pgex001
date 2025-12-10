      PROGRAM SIMPLE
        IMPLICIT NONE
        INTEGER I,IER,PGBEG
        REAL XR(100),YR(100),XS(5),YS(5)
        DATA XS/1.0,2.0,3.0,4.0,5.0/
        DATA YS/1.0,4.0,9.0,16.0,25.0/
        IER=PGBEG(0,'?',1,1)
        IF(IER.NE.1) GOTO 9000
C SUBROUTINE PGENV (XMIN, XMAX, YMIN, YMAX, JUST, AXIS)
C REAL XMIN, XMAX, YMIN, YMAX
C INTEGER JUST, AXIS
C Define coordinate range of graph (0<x<10,0<y<30)
C X A and Y scaled independently (JUST=0) and draw axes (AXIS=1),
        CALL PGENV(0.0,10.0,0.0,30.0,0,1)
        CALL PGLAB('(x)','(y)','A Simple Graph')
        CALL PGPT(5,XS,YS,9)
        DO 10,I=1,60
          XR(I)=0.1*I
          YR(I)=XR(I)*XR(I)
10      CONTINUE
        CALL PGLINE(60,XR,YR)
        CALL PGEND
        GOTO 9999
9000    PRINT *,'ERROR IN BEGINNING PLOT'
9999    STOP
      END
