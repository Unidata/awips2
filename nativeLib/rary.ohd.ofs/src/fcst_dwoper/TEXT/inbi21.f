C MODULE FCINBI21
C DESC -- THIS SUBROUTINE DETERMINES THE INITIAL CONDITIONS AT THE
C DESC -- INTERNAL BOUNDARIES
C                             LAST UPDATE: 03/01/94.13:08:21 BY $WC30JL
C
C
C @PROCESS LVL(77)
C
      SUBROUTINE INBI21(Z,INTB,I,J,LLD,IVER,QQ,YDS,YLLI,NUMLAD,LAD,PLTI,
     1   CHCTW,NUMRCP,HS,K7,IRC,LRC)
      COMMON/M121/N,NU,NS1,JN,JJ,KIT,G,DT,TT,TIMF,F1,GZN,NYQD
      COMMON/OX21/LOFKC,LONT1,LONQL,LOLQ,LZRCP,LZQQD,LZYQD,LONT
      COMMON/SS21/ NCS,A,B,DB,R,DR,AT,BT,NCSS,P,DP,ZH,NP,NPEND
      DIMENSION Z(*),NUMLAD(*),LAD(*),PLTI(*),CHCTW(*),HS(K7,*)
      DIMENSION NUMRCP(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/inbi21.f,v $
     . $',                                                             '
     .$Id: inbi21.f,v 1.3 1998/10/14 14:18:48 page Exp $
     . $' /
C    ===================================================================
C
C
      LDJ=0
      J1=J-1
      DO 5 J2=1,J1
      LDJ=LDJ+NUMLAD(J2)
    5 CONTINUE
      NUM=NUMLAD(J)
      IF(NUM.EQ.0) GO TO 100
      LLAD=LCAT21(1,J,NUMLAD)-1
      DO 20 L=1,NUM
      LJ=L+LLAD
      IF(IVER.LT.3) LLD=LJ
      IF(IVER.GE.3) LLD=(LDJ+L-1)*2+1
      IF(I.EQ.LAD(LLD)) GO TO 30
      IF(I.EQ.IABS(LAD(LLD))) GO TO 40
   20 CONTINUE
      GO TO 100
C
C  LOCK AND DAM
   30 YLLI=PLTI(LJ)
      INTB=1
      GO TO 50
C
C   HEAD RATING CURVE
   40 CH=CHCTW(LJ)
      INTB=1
      IF(ABS(CH-0.00).GT.0.0001) INTB=0
      IF(INTB.EQ.0) GO TO 50
      NRCP=NUMRCP(IRC)
      LRC=LRC-NRCP
      CALL RATC21(QQ,YLLI,DQRT,NRCP,Z(LZQQD+LRC),Z(LZYQD+LRC))
      IRC=IRC-1
cc 50   IF(IVER.GE.3) LLD=LLD+2
 50   continue
  100 CONTINUE
      RETURN
      END
