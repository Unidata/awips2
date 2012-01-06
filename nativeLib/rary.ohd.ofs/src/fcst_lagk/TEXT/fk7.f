C MODULE FK7
C-----------------------------------------------------------------------
C
      SUBROUTINE FK7 (P,C,QB,NDT,COTIME,IBUG)
C.......................................................................
C
C     THIS SUBROUTINE CONTROLS THE ATTENUATION (K) OPERATION.
C.......................................................................
C
C     SUBROUTINE ORIGINALLY PROGRAMMED BY
C                 GEORGE F. SMITH - HRL   DECEMBER 1979
C                UPDATED JUNE 1989 - GFS - FOR FT. WORTH
C                 TRANSMISSION LOSS COMPUTATIONS
C                 (CALL TO FTWTL7 ADDED)
C.......................................................................
C
C        VARIABLES IN ARGUMENT LIST
C
C        1. P      - THE P ARRAY
C        2. C      - THE C ARRAY
C        3. QB     - INPUT  - LAGGED INFLOW
C                    OUTPUT - ROUTED (ATTENUATED) OUTFLOW
C        4. NDT    - THE NUMBER OF TIME STEPS TO BE EXECUTED
C        5. COTIME - TIME (IN HOURS RELATIVE TO START OF RUN)
C                    AT WHICH CARRYOVER WILL BE SAVED
C        6. IBUG   - PRINT DEBUG FLAG, PRINT IF IBUG = 1
C.......................................................................
C
      INCLUDE 'common/fdbug'
C
      DIMENSION P(1),C(1),QB(1),CONKQ(2)
C
      LOGICAL CONK,QDT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_lagk/RCS/fk7.f,v $
     . $',                                                             '
     .$Id: fk7.f,v 1.2 2000/03/13 20:51:22 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG.EQ.1) WRITE(IODBUG,*) 'ENTER FK7'
C
      IF(NDT.LE.0)RETURN
C
C  TWO VARIABLES FOR FT. WORTH TRANSMISSION LOSS COMPUTATIONS
C
      TLRC=P(11)
      QBNTL=P(12)
C
      ITA=P(5)
      XTA=ITA/2.0
      IBK=P(18)
      IBKTB=IBK+1
      NPKQ=P(IBK)
C
C.......................................................................
C
C     SET CONSTANT K FLAG.
C     IF CONSTANT K IS USED - CREATE TABLE WITH ONE PAIR OF K,Q VALUES.
C.......................................................................
C
      CONK=.FALSE.
      IF(IFIX(P(IBK)).NE.0)GO TO 10
      CONK=.TRUE.
      CONKQ(1)=P(IBKTB)
      CONKQ(2)=1.E20
      NPKQ=1
C
   10 X2=C(2)
      Y2=C(3)
      Y1=C(4)
C
C.......................................................................
C
C     PERFORM ATTENUATION ON INFLOW TIME SERIES.
C     ACTUALLY ROUTE AT ONE HALF ORIGINAL INTERVAL - BUT ONLY STORE
C     OUTFLOW AT WHOLE INTERVALS.
C.......................................................................
C
      DO 100 I=1,NDT
      QDT=.FALSE.
      X1=X2
      X2=QB(I)
      Y0=Y1
      Y1=Y2
      QPREV=Y2
C
      X12=(X1+X2)/2.
      Y01=(Y0+Y1)/2.
C
      IF(CONK)GO TO 20
C
C.......................................................................
C
C     COMPUTE ONCE FOR EACH HALF ROUTING INTERVAL WITH VARIABLE K.
C     IF K LT (HALF INTERVAL)/4.0 - CALL FQDT7 WHICH ROUTES AT
C     ONE QUARTER OF CURRENT ROUTINE INTERVAL - I.E. AT ONE EIGHTH
C     OF WHOLE (ORIGINAL) ROUTING INTERVAL.
C.......................................................................
C
      CALL FCMPK7(X1,X12,Y01,Y1,Y12,NPKQ,P(IBKTB),XTA,CONK,QDT,IBUG)
C
      IF (QDT) THEN
         CALL FQDT7(X1,X12,Y01,Y1,Y12,NPKQ,P(IBKTB),XTA,CONK,QDT,IBUG)
	 ENDIF
C
      CALL FCMPK7(X12,X2,Y1,Y12,Y2,NPKQ,P(IBKTB),XTA,CONK,QDT,IBUG)
C
      IF (QDT) THEN
         CALL FQDT7(X12,X2,Y1,Y12,Y2,NPKQ,P(IBKTB),XTA,CONK,QDT,IBUG)
	 ENDIF
C
      GO TO 90
C
C.......................................................................
C
C     COMPUTE ONCE FOR EACH HALF ROUTING INTERVAL WITH CONSTANT K.
C.......................................................................
C
   20 CALL FCMPK7(X1,X12,Y01,Y1,Y12,NPKQ,CONKQ,XTA,CONK,QDT,IBUG)
C
      CALL FCMPK7(X12,X2,Y1,Y12,Y2,NPKQ,CONKQ,XTA,CONK,QDT,IBUG)
C
C.......................................................................
C
C     STORE THE COMPUTED OUTFLOW IN QB AT THE END OF EACH WHOLE
C     ROUTING INTERVAL.
C.......................................................................
C
 90   IF (TLRC.GT.0.0) CALL FTWTL7 (IBUG,TLRC,QBNTL,QB(I),QPREV,Y2,Y2)
C
      QB(I)=Y2
C
  100 CONTINUE
C.......................................................................
C
C      STORE CARRYOVER VALUES FOR K OPERATION.
C.......................................................................
C
      C(2)=X2
      C(4)=C(3)
      IF(NDT.GT.1)C(4)=QB(NDT-1)
      C(3)=Y2
C
      IF (IBUG.EQ.1) WRITE (IODBUG,*) 'EXIT FK7'
C
      RETURN
      END
