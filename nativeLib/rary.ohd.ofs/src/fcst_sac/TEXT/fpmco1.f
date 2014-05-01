C MEMBER FPMCO1
C  (from old member FCEX1)
C
      SUBROUTINE FPMCO1(PL,CL,IPLPM,IFRZE)
C.......................................
C     OBTAINS PARAMETERS FROM PL AND STORES IN FSMPM1 AND FPMFG1.
C         ALSO OBTAINS CARRYOVER AND STORES IN FSMCOS1.
C.......................................
C     WRITTEN BY ERIC ANDERSON - HRL    JUNE 1980
C.......................................
      REAL LZTWM,LZFSM,LZFPM,LZSK,LZPK,LZTWC,LZFSC,LZFPC
      DIMENSION PL(1),CL(1)
C
C     COMMON BLOCKS
      COMMON/FSMPM1/UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC,REXP,LZTWM,
     1LZFSM,LZFPM,LZSK,LZPK,PFREE,SIDE,SAVED,PAREA
      COMMON/FSMCO1/UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC,FGCO(6),RSUM(7),
     1PPE,PSC,PTA,PWE
      COMMON/FPMFG1/FGPM(10)
      COMMON/FSMPT1/IROC,IROCO,ISC,ISCO,IET,IETCO,LWE,NXCO,NXFCO
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sac/RCS/fpmco1.f,v $
     . $',                                                             '
     .$Id: fpmco1.f,v 1.1 1995/09/17 18:58:05 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     SOIL-MOISTURE PARAMETERS
      UZTWM=PL(IPLPM+2)
      UZFWM=PL(IPLPM+3)
      UZK=PL(IPLPM+4)
      PCTIM=PL(IPLPM+5)
      ADIMP=PL(IPLPM+6)
      RIVA=PL(IPLPM+7)
      ZPERC=PL(IPLPM+8)
      REXP=PL(IPLPM+9)
      LZTWM=PL(IPLPM+10)
      LZFSM=PL(IPLPM+11)
      LZFPM=PL(IPLPM+12)
      LZSK=PL(IPLPM+13)
      LZPK=PL(IPLPM+14)
      PFREE=PL(IPLPM+15)
      RSERV=PL(IPLPM+16)
      SIDE=PL(IPLPM+17)
C
C     COMPUTED PARAMETERS
      SAVED=RSERV*(LZFPM+LZFSM)
      PAREA=1.0-PCTIM-ADIMP
C.......................................
C     INITIAL STATE VARIABLES
      UZTWC=CL(1)
      UZFWC=CL(2)
      LZTWC=CL(3)
      LZFSC=CL(4)
      LZFPC=CL(5)
      ADIMC=CL(6)
      IF(NXCO.EQ.0) GO TO 103
      IF(IET.EQ.0) GO TO 101
      PPE=CL(IETCO+6)
  101 IF(ISC.EQ.0) GO TO 102
      PSC=CL(ISCO+6)
  102 IF(IROC.EQ.0) GO TO 103
      J=IROCO+6-1
      DO 104 I=1,7
  104 RSUM(I)=CL(J+I)
      GO TO 105
  103 DO 106 I=1,7
  106 RSUM(I)=0.0
  105 FGCO(1)=-999.0
      IF(IFRZE.EQ.0) RETURN
      DO 107 I=1,6
      J=NXCO+6+I
  107 FGCO(I)=CL(J)
      PTA=CL(NXCO+13)
      IF(LWE.GT.0) PWE=CL(NXCO+14)
C.......................................
C     FROZEN GROUND PARAMETERS.
      LP=IFRZE+7
      DO 110 I=1,10
      J=I-1
  110 FGPM(I)=PL(LP+J)
C.......................................
      RETURN
      END
