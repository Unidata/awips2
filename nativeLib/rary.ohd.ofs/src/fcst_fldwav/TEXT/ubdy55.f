      SUBROUTINE UBDY55(PO,QU,YU,YJ,KU,ST1,LTST1,T1,LTT1,QTC,LTQTC,YDI,
     . QDI,STM,C,D,ISS,LRT,IRT1,QSAVE,KRCH,IOBS,N1,L1,L2,MRU,NJUM,
     . K1,K2,K15)
C
C      THIS SUBROUTINE SETS UP UPSTREAM BOUNDARY CONDITIONS
C
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/M655/KTIME,DTHYD,J1
      COMMON/NPC55/NP,NPST,NPEND
      COMMON/NETWK55/NET

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'

      DIMENSION PO(*),ST1(*),LTST1(*),T1(*),LTT1(*),QTC(*),LTQTC(*)
      DIMENSION QU(K2,K1),YU(K2,K1),KRCH(K2,K1),QDI(K2,K1),YDI(K2,K1)
      DIMENSION STM(K1),YJ(K1),KU(K1),C(K15),D(4,K15),MRU(K1)
      DIMENSION NJUM(K1)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/ubdy55.f,v $
     . $',                                                             '
     .$Id: ubdy55.f,v 1.3 2000/12/19 15:57:19 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/ 'UBDY55  '  /
C
      CALL FPRBUG(SNAME,1,55,IBUG)
C
      J=JJ
C NETWORK-RIVER U/S
      IF (J.GT.JN) THEN
        JOUT=MRU(J)
        IOUT=NJUM(J)
      YQ1=0.5*(YU(IOUT,JOUT)+YU(IOUT+1,JOUT))
      YU(1,J)=YQ1
      C(1)=0.0
      D(1,1)=1.0
      D(2,1)=0.0
        GOTO 999
        ENDIF
      L=LRT
      YQM=STM(J)
      IF(KU(J).EQ.2) YQM=ABS(YQM)
      LT1=LTT1(J)
      IF(KRCH(ISS,J).LT.10.OR.KRCH(ISS,J).GT.30) THEN
        YQ1=QU(ISS,J)
        IF(YQ1.LT.YQM) YQ1=YQM
        IF(ISS.GT.1) GO TO 70
        IF(IOBS.GE.0) GO TO 40
        CALL MATHQ55(YQ1,PO(LOTPG),PO(LORHO),PO(LOGAMA),PO(LOYQI),K1)
        GO TO 50
   40   YQ1=YJ(J)
        IF(YQ1.LT.YQM) YQ1=YQM
        IF(KU(J).GE.1) THEN
          TCYCL=NU*DTHYD
          IF(DTHYD.LE.0.) TCYCL=T1(NU+LT1-1)
          NCYCL=TT/TCYCL
          TX=TT-TCYCL*NCYCL
          LJ=LTST1(J)-1
          CALL INTERP55(T1(LT1),NU,TX,IT1,IT2,TINP)
          IF(TT.EQ.TCYCL) THEN 
            YQ1=ST1(NU+LJ)
          ELSEIF(TX.GE.T1(LT1)) THEN
             YQ1=ST1(IT1+LJ)+(ST1(IT2+LJ)-ST1(IT1+LJ))*TINP
          ELSE
             IF(KU(J).EQ.1) YQ1=YDI(1,J)+(ST1(1+LJ)-YDI(1,J))*TX/T1(LT1)
             IF(KU(J).EQ.2) YQ1=QDI(1,J)+(ST1(1+LJ)-QDI(1,J))*TX/T1(LT1)
          END IF
          IF(YQ1.LT.YQM) YQ1=YQM
cc        print*, it1,it2,tt,yq1,st1(it1+lj),st1(it2+lj),lj,tcycl,ncycl,tx
        END IF
   50   IF(KU(J)-2) 60,70,70
   60   YU(ISS,J)=YQ1
        C(N1)=0.0
        D(L1,N1)=1.0
        D(L2,N1)=0.0
        GO TO 85
   70   IF(ISS.EQ.1) GO TO 75
        IF(ISS.EQ.IRT1) GO TO 72
        YQ1=QSAVE
        GO TO 75
   72   IF(NP.LE.-1) THEN
          TX=TT
          LJ=LCAT21(L,J,NGAGE)
          LLJ=LTQTC(LJ)-1
          CALL INTERP55(T1(LT1),NU,TX,IT1,IT2,TINP)
          IF(TX.GE.T1(LT1)) THEN
            YQ1=QTC(IT1+LLJ)+(QTC(IT2+LLJ)-QTC(IT1+LLJ))*TINP
          ELSE
            YQ1=QDI(1,J)+(QTC(1+LJ)-QDI(1,J))*TX/T1(LT1)
          END IF
        END IF
   75   QU(ISS,J)=YQ1
   80   CONTINUE
        C(N1)=0.0
        D(L1,N1)=0.0
        D(L2,N1)=1.0
   85 CONTINUE
      ENDIF
  999 RETURN
      END
