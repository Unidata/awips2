C$PRAGMA C (SETCMP)
C MEMBER ENGL55
C DESC -- THE FUNCTION OF THIS SUBROUTINE IS TO CONVERT TIME SERIES
C DESC -- DATA FROM METRIC UNITS TO ENGLISH UNITS AND BACK AGAIN.
C
C
C jgg calling signature changed by jgg 7/04 for HSD r23-48 
C jgg      SUBROUTINE ENGL55(ICHK,STN,LTSTN,ST1,LTST1,POLH,LTPOLH,QL,LTQL,
C jgg     . STT,LTSTT,STQ,LTSTQ,QSTR,LTQSR,KD,KU,NQL,NUMLAD,LAD,CHCTW,NGAGE,
C jgg     . NSTR,KTYP,NRCP,RHI,RQI,YQD,QYQD,TIDE,XNOS,STE,LTSTE,NB,NYQD,NRC,
C jgg     . IOBS,KPL,JN,NU,NPO,NLOCK,K1,K14,K16)

      SUBROUTINE ENGL55(ICHK,STN,LTSTN,ST1,LTST1,POLH,LTPOLH,QL,LTQL,
     . STT,LTSTT,STQ,LTSTQ,QSTR,LTQSR,KD,KU,NQL,NUMLAD,LAD,CHCTW,NGAGE,
     . NSTR,KTYP,NRCP,RHI,RQI,YQD,QYQD,TIDE,XNOS,STE,LTSTE,NB,NYQD,NRC,
     . IOBS,KPL,JN,NU,NPO,NLOCK,KRCH,K1,K2,K14,K16)
C
C           THIS SUBROUTINE WAS WRITTEN BY:
C           JANICE LEWIS      HRL   JANUARY, 1999     VERSION NO. 1
C
      INCLUDE 'common/fdbug'
      COMMON/NETWK55/NET

C
      DIMENSION STN(*),ST1(*),LTST1(*),POLH(*),LTPOLH(*),QL(*),LTQL(*)
      DIMENSION STT(*),LTSTT(*),STQ(*),LTSTQ(*), QSTR(*),LTQSR(*)
C jgg changed by jgg 7/04 for HSD r23-48       
C jgg      DIMENSION TIDE(*),XNOS(*),STE(*),LTSTE(*)
      DIMENSION TIDE(*),XNOS(*),STE(*),LTSTE(*),KRCH(K2,K1)
      DIMENSION KD(K1),KU(K1),NQL(K1),NUMLAD(K1),LAD(K16,K1)
      DIMENSION CHCTW(K16,K1),NGAGE(K1),NSTR(K1),KTYP(K14,K1),NB(K1)
      DIMENSION NRCP(K16,K1),RHI(112,K16,K1),RQI(112,K16,K1)
      DIMENSION YQD(*),QYQD(*)

      CHARACTER*8 SNAME
C
ckwz  Added for r21-67
c In this procedure, variables STN,ST1,POLH,QL,STT,STQ,QSTR,and STE are
c all refer to array variable D in fdriv3.f.  In some cases, some part
c of the D array would converted twice, and this fix is to prevent that.
      integer, dimension(2,100) :: setList
      integer setsLength,stat
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/engl55.f,v $
     . $',                                                             '
     .$Id: engl55.f,v 1.9 2004/07/21 12:00:18 jgofus Exp $
     . $' /
C    ===================================================================
C
C
C
C
      DATA SNAME/'ENGL55  '/
	  DATA setsLength/0/
C
C
CC      WRITE(6,9998)
CC 9998 FORMAT(2X,'=+=+=+ ENTERING ENGL55 +=+=+=')
      CALL FPRBUG(SNAME,1,55,IBUG)
C
CC      WRITE(6,9999) NRC
CC 9999 FORMAT(/3X,'***** NRC = ',I2,'  *********')
      IF(ICHK.EQ.1) GO TO 1
C
C       CONVERT FROM ENGLISH TO METRIC UNITS
C
      STGE=.3048
      FLOW=.0283
      IF(IBUG.GT.0) WRITE(IODBUG,2)
 2    FORMAT(//15X,99('*')/15X,'********** BEGIN CONVERTING FROM ENGLISH
     . TO METRIC UNITS (THESE VALUES IN METRIC UNITS) ***********'/
     . 15X,99('*'))
      GO TO 3
C
C       CONVERT FROM METRIC TO ENGLISH UNITS
C
    1 STGE=3.2808
      FLOW=35.3147
      IF(IBUG.GT.0) WRITE(IODBUG,4)
 4    FORMAT(//15X,100('*')/15X,'********** BEGIN CONVERTING FROM METRIC
     . TO ENGLISH UNITS (THESE VALUES IN ENGLISH UNITS) ***********'/
     .15X,99('*'))
C
    3 IF(KD(1)-2) 5,5,15
C
C       CONVERT DOWNSTREAM BOUNDARY
C
    5 LSTN=LTSTN+NU-1
      TYPD=STGE
      IF(KD(1).EQ.2) TYPD=FLOW
C
ckwz init. setList and setsLength. r21-67
      setList(1,1)=LTSTN
      setList(2,1)=LSTN
      setsLength=1

      DO 10 K=LTSTN,LSTN
      IF(STN(K).GT.-900.0) STN(K)=STN(K)*TYPD
   10 CONTINUE

      IF(KD(1).EQ.0) THEN
        DO 11 K=1,NU
          TIDE(K)=TIDE(K)*TYPD
          XNOS(K)=XNOS(K)*TYPD
   11   CONTINUE
        XNOS(K+1)=XNOS(K+1)*TYPD
      ENDIF

C
      IF(IBUG.EQ.0) GO TO 15
      WRITE(IODBUG,12)
   12 FORMAT(1H0,10X,30HDOWNSTREAM BOUNDARY HYDROGRAPH)
      WRITE(IODBUG,14) (STN(K),K=LTSTN,LSTN)
   14 FORMAT(1H ,10X,10F12.2)
      IF(KD(1).EQ.0) THEN
        WRITE(IODBUG,16)
   16   FORMAT(1H0,10X,'NOS TIDE HYDROGRAPH')
        WRITE(IODBUG,14) (XNOS(K),K=1,NU+1)
        IF(ICHK.EQ.1) THEN
          WRITE(IODBUG,20)
   20     FORMAT(1H0,10X,'ADJUSTED TIDE DOWNSTREAM BOUNDARY HYDROGRAPH')
          WRITE(IODBUG,14) (TIDE(K),K=1,NU)
        ENDIF
      ENDIF
C
C        CONVERT THE DOWNSTREAM RATING CURVE
C
   15 IF(NYQD.EQ.0) GO TO 170
      DO 160 K=1,NYQD
        YQD(K)=YQD(K)*STGE
        QYQD(K)=QYQD(K)*FLOW
  160 CONTINUE
      IF(IBUG.EQ.0) GO TO 170
      WRITE(IODBUG,17)
   17 FORMAT(1H0,10X,23HDOWNSTREAM RATING CURVE)
      WRITE(IODBUG,14) (YQD(K),K=1,NYQD)
      WRITE(IODBUG,14) (QYQD(K),K=1,NYQD)

 170  IF(NRC.EQ.0) GO TO 200
      DO 180 J=1,JN
        IF(NUMLAD(J).EQ.0) GO TO 180
        NUM=NUMLAD(J)
        DO 175 I=1,NUM
          NR=NRCP(I,J)
          IF(NR.EQ.0) GO TO 175
          WRITE(IODBUG,18) I,J
   18     FORMAT(10X,'RATING CURVE FOR DAM ',I3,' ON RIVER',I3)
          DO 172 K=1,NR
            RHI(K,I,J)=RHI(K,I,J)*STGE
            RQI(K,I,J)=RQI(K,I,J)*FLOW
  172     CONTINUE
          IF(IBUG.NE.0) THEN
            WRITE(IODBUG,14) (RHI(K,I,J),K=1,NR)
            WRITE(IODBUG,14) (RQI(K,I,J),K=1,NR)
          ENDIF
  175   CONTINUE
  180 CONTINUE
C
  200 LOCK=0
      LLD=1

      K2TT=0
      K2QL=0
      K2PL=0
      K2SR=0
      DO 100 J=1,JN-NET
C
C       CONVERT UPSTREAM BOUNDARIES
C
      LOST=LTST1(J)
      LST1=LOST+NU-1
      TYPD=STGE
      IF(KU(1).EQ.2) TYPD=FLOW
C
ckwz r21-67
      call setcmp(setList,setsLength,LOST,LST1,stat)
      if (stat.eq.0) goto 71
      DO 21 K=LOST,LST1
      ST1(K)=ST1(K)*TYPD
   21 CONTINUE
C
71    IF(IBUG.EQ.0) GO TO 24
      WRITE(IODBUG,22) J
   22 FORMAT(1H0,10X,42HUPSTREAM BOUNDARY HYDROGRAPH FOR RIVER NO.,I2)
      WRITE(IODBUG,14) (ST1(K),K=LOST,LST1)
C
C       CONVERT OBSERVED DATA
C
   24 IF(IOBS.EQ.0) GO TO 50
      IF(NGAGE(J).EQ.0) GO TO 50
      NGAG=NGAGE(J)
      IF(KPL) 50,50,25
   25 TYPD=STGE
      IF(KPL.EQ.2) TYPD=FLOW
C
      IF(IBUG.GE.1) WRITE(IODBUG,27) J
   27 FORMAT(1H0,10X,40HOBSERVED STAGE HYDROGRAPHS FOR RIVER NO.,I2)
      K1TT=K2TT+1
      K2TT=K1TT+NGAG-1
      DO 40 K=K1TT,K2TT
        KJ=LTSTT(K)-1
        IF(KPL.EQ.3) MJ=LTSTQ(K)-1
ckwz r21-67
      call setcmp(setList,setsLength,KJ+1,KJ+NU,stat)
	  if (stat.eq.0) goto 72
        DO 30 L=1,NU
          IK=IFMSNG(STT(L+KJ))
          IF(IK.NE.1) STT(L+KJ)=STT(L+KJ)*TYPD
c          IF(KPL.EQ.3)THEN
c            IK=IFMSNG(STQ(L+MJ))
c            IF(IK.NE.1) STQ(L+MJ)=STQ(L+MJ)*FLOW
c          ENDIF

   30   CONTINUE
ckwz r21-67
   72 call setcmp(setList,setsLength,MJ+1,MJ+NU,stat)
      if (stat.eq.0) goto 73
        DO 31 L=1,NU
c          IK=IFMSNG(STT(L+KJ))
c          IF(IK.NE.1) STT(L+KJ)=STT(L+KJ)*TYPD
          IF(KPL.EQ.3)THEN
            IK=IFMSNG(STQ(L+MJ))
            IF(IK.NE.1) STQ(L+MJ)=STQ(L+MJ)*FLOW
          ENDIF

   31   CONTINUE
   73   IF(IBUG.GE.1) WRITE(IODBUG,14) (STT(L+KJ),L=1,NU)
        IF(IBUG.GE.1.AND.KPL.EQ.3) WRITE(IODBUG,14) (STQ(L+MJ),L=1,NU)
   40 CONTINUE
C
      IF(ICHK.EQ.1) GO TO 50
      IF(IOBS.LE.1.OR.NGAG.EQ.NB(J)) GO TO 50
      IF(IBUG.GE.1) WRITE(IODBUG,42) J
   42 FORMAT(1H0,10X,'ADJUSTED STAGE HYDROGRAPHS FOR RIVER NO.',I2)
C
CC      WRITE(6,9991) NRT,K,J
CC 9991 FORMAT(/5X,'===== NRT,K,J =',3I5,'  =====')
      DO 48 K=1,NGAG
      KJ=LCAT21(K,J,NGAGE)
      LTSE=LTSTE(KJ)
      LSTE=LTSE+NU-1
CC      WRITE(6,9990) K,LKJ,LTSE,LSTE,TYPD
CC 9990 FORMAT(/5X,'=====    K       LKJ      LOTT     LSTT TYPD='/
CC     . 10X,I5,3I10,F6.3/)
ckwz r21-67
      call setcmp(setList,setsLength,LTSE,LSTE,stat)
	  if (stat.eq.0) goto 74
      DO 46 L=LTSE,LSTE
      IK=IFMSNG(STE(L))
      IF(IK.EQ.1) GO TO 46
      STE(L)=STE(L)*TYPD
   46 CONTINUE
   74 IF(IBUG.GE.1) WRITE(IODBUG,14) (STE(L),L=LTSE,LSTE)
   48 CONTINUE
C
C
C       CONVERT LATERAL FLOWS
C
   50 IF(NQL(J).EQ.0) GO TO 75
      NQ=NQL(J)
C
      IF(IBUG.GE.1) WRITE(IODBUG,52) J
   52 FORMAT(1H0,10X,40HLATERAL INFLOW HYDROGRAPHS FOR RIVER NO.,I2)
C
      K1QL=K2QL+1
      K2QL=K1QL+NQ-1
      DO 70 K=K1QL,K2QL
      KJ=LTQL(K)-1
ckwz r21-67
      call setcmp(setList,setsLength,KJ+1,KJ+NU,stat)
      if (stat.eq.0) goto 76
      DO 60 L=1,NU
      QL(L+KJ)=QL(L+KJ)*FLOW
   60 continue
   76 IF(IBUG.GE.1) WRITE(IODBUG,14) (QL(L+KJ),L=1,NU)
   70 CONTINUE
C
C       CONVERT POOL ELEVATIONS
C
   75 IF(ICHK.EQ.0) GO TO 92
      IF(NLOCK.EQ.0) GO TO 92
      IF(NUMLAD(J).EQ.0) GO TO 92
      NUM=NUMLAD(J)
C
      IF(IBUG.GE.1) WRITE(IODBUG,77) J
   77 FORMAT(1H0,10X,25HPOOL LEVELS FOR RIVER NO.,I2)
C
      K1PL=K2PL+1
      K2PL=K1PL+NUM-1
      
C jgg following changes made 7/04 for HSD r23-48 as per jls     
C jgg      DO 90 K=K1PL,K2PL
C jgg      KJ=LTPOLH(K)-1

      LCK=0

      DO 90 K=1,NUM
      LDD=IABS(LAD(K,J))
      IF(KRCH(LDD,J).NE.28) GO TO 90
      LCK=LCK+1
      KJ=LTPOLH(LCK)-1
C jgg end of changes

CC      IF(LAD(K,J).LE.0.OR.CHCTW(K,J).LT.0.) GO TO 85
ckwz r21-67
      call setcmp(setList,setsLength,KJ+1,KJ+NU,stat)
	  if (stat.eq.0) goto 78
      DO 80 L=1,NU
        IK=IFMSNG(POLH(L+KJ))
        IF(IK.NE.1) POLH(L+KJ)=POLH(L+KJ)*STGE
   80 CONTINUE
   78 IF(IBUG.GE.1) WRITE(IODBUG,14) (POLH(L+KJ),L=1,NU)
CC 85   IF(IVER.GE.3) LLD=LLD+2
   90 CONTINUE
C
C          CONVERT OUTPUT TIME SERIES DATA
C
   92 IF(ICHK.EQ.1) GO TO 100
      IF(NSTR(J).EQ.0) GO TO 100
      NSR=NSTR(J)
C
      IF(IBUG.GE.1) WRITE(IODBUG,94) J
   94 FORMAT(1H0,10X,37HOUTPUT TIME SERIES DATA FOR RIVER NO.,I2)
      K1SR=K2SR+1
      K2SR=K1SR+NSR-1
      KK=0
      DO 98 K=K1SR,K2SR
      KJ=LTQSR(K)-1
      KK=KK+1
      KTP=KTYP(KK,J)
      TYPE=STGE
      IF(KTP.EQ.2) TYPE=FLOW
ckwz r21-67
      call setcmp(setList,setsLength,KJ+1,KJ+NU,stat)
      if (stat.eq.0) goto 79
      DO 95 L=1,NU
      QSTR(L+KJ)=QSTR(L+KJ)*TYPE
   95 CONTINUE
   79 IF(IBUG.GE.1) WRITE(IODBUG,14) (QSTR(L+KJ),L=1,NU)
   98 CONTINUE
  100 CONTINUE
      IF(ICHK.EQ.0.AND.IBUG.GT.0) WRITE(IODBUG,500)
 500  FORMAT(//15X,67('*')/15X,'********** END CONVERTING FROM ENGLISH T
     .O METRIC UNITS **********'/15X,67('*'))
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
      IF(ICHK.EQ.1.AND.IBUG.GT.0) WRITE(IODBUG,510)
 510  FORMAT(//15X,67('*')/15X,'********** END CONVERTING FROM METRIC TO
     . ENGLISH UNITS **********'/15X,67('*'))
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,2H**,1X,2A4,8H EXITED.)
      RETURN
      END





