      PROGRAM EX1
        IMPLICIT NONE
        INTEGER PGOPEN,I
        REAL XS(9),YS(9),XR(101),YR(101)
C Compute numbers to be plotted
        DO 10,I=1,101
          XR(I)=0.1*(I-1)
          YR(I)=XR(I)**2*EXP(-XR(I))
10      CONTINUE
        DO 20,I=1,9
          XS(I)=I
          YS(I)=XS(I)**2*EXP(-XS(I))
20      CONTINUE
C Open graphics device
        IF(PGOPEN('?').LT.1) STOP
C SUBROUTINE PGENV (XMIN, XMAX, YMIN, YMAX, JUST, AXIS)
C REAL XMIN, XMAX, YMIN, YMAX
C INTEGER JUST, AXIS
C Define coordinate range of graph (0<x<10,0<y<0.65)
C X A and Y scaled independently (JUST=0) and draw axes (AXIS=0),
        CALL PGENV(0.0,10.0,0.0,0.65,0,0)
C Label the axes (note the use of \u and \d for raising exponent)
        CALL PGLAB('X','Y','PGPLOT Graph: y = x\u2\de\u(-x)')
C Plot the line graph
        CALL PGLINE(101,XR,YR)
C Plot symbols at selected points
        CALL PGPT(9,XS,YS,18)
C Close the graphics device
        CALL PGCLOS
      END
