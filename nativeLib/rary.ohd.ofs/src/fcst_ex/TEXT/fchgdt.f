C MEMBER FCHGDT
C  (from old member FCEX10)
C
      SUBROUTINE FCHGDT(QIN,IDT,QOUT,IODT,QZERO)
C.......................................
C     THIS SUBROUTINE CHANGES THE TIME INTERVAL OF THE ORDINATE SPACING
C        OF A TIME SERIES OF INSTANTANEOUS VALUES.  THE TIME
C        INTERVAL CAN BE INCREASED OR DECREASED.  IF THE TIME
C        INTERVAL IS REDUCED, ONE CARRYOVER VALUE IS NEEDED
C        (QZERO=VALUE AT ITME ZERO).  LINEAR INTERPOLATION
C        IS USED FOR ALL OUTPUT ORDINATES THAT DO NOT
C        CORRESPOND TO AN INPUT ORINATE.  QIN CAN CONTAIN
C        MISSING VALUES.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C        ERIC ANDERSON - HRL   OCT. 1979
C.......................................
      DIMENSION QIN(1),QOUT(1)
C
C     COMMON BLOCKS.
      COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,NOW(5),
     1   LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/fchgdt.f,v $
     . $',                                                             '
     .$Id: fchgdt.f,v 1.1 1995/09/17 18:57:34 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** FCHGDT ENTERED)
C     NO DEBUG OUTPUT.
C.......................................
C     CHECK IF IODT IS A MULTIPLE OF IDT.
      MULT=0
      IF((IODT/IDT)*IDT.EQ.IODT) MULT=1
C
C     COMPUTE OFFSET TO ZERO HOUR.
      INZ=(IDA-IDADAT)*24/IDT+(IHR-IODT)/IDT
      IOZ=(IDA-IDADAT)*24/IODT+(IHR-IODT)/IODT
C
C     INITIAL TIME AND VALUES.
      KDA=IDA
      KHR=IHR
      I=1
      DT=IDT
C.......................................
C     MAIN COMPUTATIONAL LOOP.
C        I= LOCATION IN QOUT RELATIVE TO ZERO HOUR
C        J= LOCATION IN QIN RELATIVE TO ZERO HOUR OF
C           FIRST QIN VALUE USED IN THE LINEAR INTERPOLATION
C           TO GET QOUT.
  100 J=(IODT*I)/IDT
      IF(MULT.EQ.0) GO TO 105
C
C     IODT IS A MULTIPLE OF IDT--NO NEED TO CHECK FOR MISSING VALUES.
  104 QOUT(IOZ+I)=QIN(INZ+J)
      GO TO 120
C.......................................
C     CHECK IF INTERPOLATION NEEDED TO GET QOUT VALUE.
  105 IF(J.EQ.0) GO TO 110
      N=IODT*I-J*IDT
      IF(N.EQ.0) GO TO 104
C
C     USE LINEAR INTERPOLATION.
C     CHECK FOR MISSING QIN VALUES.
      Q1=QIN(INZ+J)
      Q2=QIN(INZ+J+1)
      IF(IFMSNG(Q1).EQ.1) GO TO 115
      IF(IFMSNG(Q2).EQ.1) GO TO 115
      FN=N
      FRAC=FN/DT
      QOUT(IOZ+I)=Q1+FRAC*(Q2-Q1)
      GO TO 120
C
C     NEED TO USE QZERO-(IODT*I).LT.IDT.
  110 IF(IFMSNG(QZERO).EQ.1) GO TO 115
      Q2=QIN(INZ+1)
      IF(IFMSNG(Q2).EQ.1) GO TO 115
      FN=IODT*I
      FRAC=FN/DT
      QOUT(IOZ+I)=QZERO+FRAC*(Q2-QZERO)
      GO TO 120
C
C     ONE OR BOTH QIN VALUES NEED FOR INTERPOLATION ARE MISSING.
  115 QOUT(IOZ+I)=-999.0
C.......................................
C     CHECK FOR END OF LOOP.
  120 IF((KDA.EQ.LDA).AND.(KHR.EQ.LHR)) GO TO 190
C
C     INCREMENT TIME AND COUNTER.
      I=I+1
      KHR=KHR+IODT
      IF(KHR.LE.24) GO TO 100
      KHR=IODT
      KDA=KDA+1
      GO TO 100
C     END OF FCHGDT COMPUTATIONS.
C.......................................
  190 CONTINUE
      RETURN
      END
