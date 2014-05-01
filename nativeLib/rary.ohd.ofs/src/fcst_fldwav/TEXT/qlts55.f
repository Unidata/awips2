      SUBROUTINE QLTS55(T1,LTT1,QL,LTQL,QLI,I,NQL,LQ1,LQN,QLTSU,QLTSD,
     . K1,K10)
C
C      THIS SUBROUTINE COMPUTES LATERAL INFLOW FROM TIME SERIES
C      FLOW RATE IS AMOUNT PER FOOT
C
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1

      INCLUDE 'common/ofs55'
C
      DIMENSION T1(*),LTT1(*),QL(*),LTQL(*),QLI(*),NQL(K1),LQ1(K10,K1)
      DIMENSION LQN(K10,K1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/qlts55.f,v $
     . $',                                                             '
     .$Id: qlts55.f,v 1.1 1999/04/23 18:08:51 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HQLTS,4H55  /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      J=JJ
      QLTSU=0.
      QLTSD=0.
      NQLJ=NQL(J)
      DO 10 L=1,NQLJ
      IR1=LQ1(L,J)
      IRN=LQN(L,J)-1
      IF(I.GE.IR1.AND.I.LE.IRN) GO TO 20
   10 CONTINUE
      GO TO 30
   20 TX=TT

      LT1=LTT1(J)
      LJ=LCAT21(L,J,NQL)
      LLJ=LTQL(LJ)-1
      CALL INTERP55(T1(LT1),NU,TX,IT1,IT2,TINP)
      IF(TX.GE.T1(LT1)) THEN
        QLTSU=QL(IT1+LLJ)+TINP*(QL(IT2+LLJ)-QL(IT1+LLJ))
      ELSE
        QLTSU=QLI(LJ)+(QL(IT1+LLJ)-QLI(LJ))*TX/T1(LT1)
      ENDIF
      TX=TT-DT/TIMF
      IF(TX.LE.0.0) TX=0.0
      CALL INTERP55(T1(LT1),NU,TX,IT1,IT2,TINP)
      IF(TX.GE.T1(LT1)) THEN
        QLTSD=QL(IT1+LLJ)+TINP*(QL(IT2+LLJ)-QL(IT1+LLJ))
      ELSE
        QLTSD=QLI(LJ)+(QL(IT1+LLJ)-QLI(LJ))*TX/T1(LT1)
      ENDIF
   30 CONTINUE
      RETURN
      END
