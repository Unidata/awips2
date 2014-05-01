C MODULE PUC51
C***********************************************************************
C
      SUBROUTINE PUC51(PO,CO)
C
C     ROUTINE PUNCHESS THE INFORMATION STORED IN THE P ARRAY FOR THE
C     SSARRESV RESERVOIR OPERATION.
C
C***********************************************************************
C     PROGRAMMED BY KUANG HSU  OCTOBER 1994
C***********************************************************************
      REAL NORMP,NORMQ
      DIMENSION PO(*),CO(*),PR51(2),X(365),Y(365),Z(365),AMP(2)
      CHARACTER*4  FNV(6)
      CHARACTER*28 FMT0,FMT2
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fengmt'
      REAL*8 GTSKW(15),BWTYP(2),ENGMET(2),KEYWD(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc51.f,v $
     . $',                                                             '
     .$Id: puc51.f,v 1.4 2006/03/16 16:30:24 xfan Exp $
     . $' /
C    ===================================================================
C
C
      DATA PR51,BLANK/4HPUC5,4H1   ,4H    /
C
      DATA GTSKW/8HINSTQI1 ,8HINSTQI2 ,8HINSTQO1 ,8HINSTQO2 ,8HMEANQOUT,
     1           8HPOOL    ,8HSTORAGE ,8HOBSQO   ,8HOBSQOM  ,8HOBSH    ,
     1           8HTRIBQL1 ,8HTRIBQL2 ,8HBACKQI1 ,8HBACKQI2 ,8HBACKQIM /
      DATA KEYWD/8HELVSSTOR,8HQVSEL   ,8H        /
      DATA BWTYP/8HFLOW    ,8HELEV    /
      DATA ENGMET/8HENGLISH ,8HMETRIC  /
      DATA AMP/1H&,1H /
      DATA FMT0/'( A8,1X,    F10.0,  T72,A1) '/
      DATA FMT2/'( A8,1X,    F10.2,  T72,A1) '/
      DATA  FNV/'   1','   2','   3','   4','   5','   6'/
C
C***********************************************************************
C----------------------------
C  SET DEBUG AND TRACE LEVELS
C
      IBUG = 0
      CALL FPRBUG(PR51,1,51,IBUG)
      IF (ITRACE .GT. 0) IBUG=1
      IF (IPBUG .GT. 0) IBUG=2
C
      NRES=PO(9)
      CALL ETOM51(PO)

C dws    PO(12) was placed into an integer to replace it in the rest
C dws     of the statements to avoid compiler warnings ... 2006-01-23
 
      NUPO12 = PO(12)

C
      WRITE(IPU,700)
 700  FORMAT('SSARRESV')
C
C     PUNCH HEADING
      WRITE(IPU,715) (PO(I),I=2,6)
 715  FORMAT('TITLE',2X,1H',5A4,1H')
C
C     PUNCH UNITS
      METSAR=PO(8)
      IUN = 2
      IF(METSAR.LE.0) IUN=1
      WRITE(IPU,717) ENGMET(IUN)
 717  FORMAT('UNITS',2X,A8)
C
C***********************************************************************
C     PUNCH INFLOW INFORMATION
C***********************************************************************
C
      WRITE(IPU,710)
 710  FORMAT('INFLOW')
C
C     INFLOW TIME SERIES INFORMATION
      WRITE(IPU,520)
      LTS=NUPO12+1
      DO 25 I=1,2
      IF(PO(LTS).EQ.BLANK)GO TO 22
      IT=PO(LTS+3)
      WRITE(IPU,750) GTSKW(I),PO(LTS),PO(LTS+1),PO(LTS+2),IT
 750  FORMAT(A8,2X,2A4,2X,A4,2X,I2)
 22   LTS=LTS+5
 25   CONTINUE
      WRITE(IPU,525)
C
C     INFLOW CARRYOVER INFORMATION
C
      LCOI = 1
      QIN = CO(LCOI)
C
      LTS1 = NUPO12+1
      IF(PO(LTS1).NE.BLANK) GO TO 28
      WRITE(IPU,530)
      WRITE(IPU,1610) QIN
      WRITE(IPU,535)
 28   CONTINUE
      WRITE(IPU,650)
 650  FORMAT('ENDINFLW')
C
C     END OF INFLOW INFORMATION
C
C***********************************************************************
C     PUNCH UPERBKWR INFORMATION
C***********************************************************************
C
      IF(NRES.LE.1) GO TO 1000
      IRES=1
      LPO=PO(11)
      ISTYP=PO(LPO)
      IRES=1
      LCO=LCOI+(NRES-IRES)*4+1      
      IF(ISTYP.EQ.3) GO TO 300
      LPO=LPO+1
      WRITE(IPU,1710)
1710  FORMAT('UPERBKWR')
      NTTS=PO(NUPO12)
      NTS=(NTTS-2)/NRES
      LTS=NUPO12+(NRES-IRES)*NTS*5+11
      CALL PUCB51(ISTYP,IRES,NRES,LPO,LTS,LCO,PO,CO)
C
      WRITE(IPU,1650)
 1650 FORMAT('ENDUPERB')
      GO TO 1000
C
C     END OF UPERBKWR INFORMATION
C
C
C***********************************************************************
C     PUNCH 3-VAR INFORMATION
C***********************************************************************
C
 300  CONTINUE
      WRITE(IPU,3710)
3710  FORMAT('3-VAR')
      IRES=1
      LPO=PO(11)
      LPO=LPO+1
C
      WRITE(IPU,510)
      NVAL=PO(LPO)
C
C  ELEVATION-STORAGE CURVE NOT NEEDED
      LPO=LPO+3*NVAL+1
C
C
C  PUNCH BACKWATER TABLE
C
      NVAL=PO(LPO)
      LBWTYP=LPO+3*NVAL+1
      IBWTYP=PO(LBWTYP)
      DO 135 I=1,NVAL
      IX=LPO+(I-1)*3+1
      X(I)=PO(IX)
      IY=IX+1
      Y(I)=PO(IY)
      IZ=IY+1
      Z(I)=PO(IZ)
 135  CONTINUE
C
      IAMP=1
      IB=1
      IE=NVAL
      IF(IE .GT. 2) IE=2
      IF(IE.GE.NVAL) IAMP=2
      WRITE(IPU,1820) (X(I),Y(I),Z(I),I=IB,IE),AMP(IAMP)
1820  FORMAT('3VARTABL',1X,2(F10.1,2F10.2),1X,A1)
      IF(IE .GE. NVAL)GO TO 150
      IB=IE+1
      IE=IE+2
      IF(IE .GT. NVAL)IE=NVAL
 140  IF(IE.GE.NVAL) IAMP=2
      IF(IE.GT.IB) WRITE(IPU,1825) (X(I),Y(I),Z(I),I=IB,IE),AMP(IAMP)
1825  FORMAT(9X,2(F10.0,2F10.2),1X,A1)
      IF(IE.EQ.IB) WRITE(IPU,1826) (X(I),Y(I),Z(I),I=IB,IE)
 1826 FORMAT(9X,F10.0,2F10.2)
      IF(IE .GE. NVAL)GO TO 150
      IB=IE+1
      IE=IE+2
      IF(IE .GT. NVAL)IE=NVAL
      GO TO 140
C
 150  CONTINUE
      LPO=LPO+3*NVAL+1
C
C  PUNCH FIRST INDEPENDENT VARIABLE BACKWATER CONTROL TYPE
      ICON1=PO(LPO)
      WRITE(IPU,3715) BWTYP(ICON1)
3715  FORMAT('BACKWATR',5X,2A4)
      LPO=LPO+1
C
C  PUNCH SECOND INDEPENDENT VARIABLE CONTROL TYPE
C      ICON2=PO(LPO)
C      WRITE(IPU,3716) BWTYP(ICON2)
C 3716 FORMAT('CONTROL2',5X,2A4)
      LPO=LPO+1
C
C  PUNCH DEPENDENT VARIABLE TYPE
C      ISITE=PO(LPO)
C      WRITE(IPU,3717) BWTYP(ISITE)
C 3717 FORMAT('SITE',2X,2A4)
C
      WRITE(IPU,515)
      LPO=LPO+1
C
C***********************************************************************
C     TIME SERIES INFORMATION
      WRITE(IPU,520)
      NTTS=PO(NUPO12)
      NTS=(NTTS-2)/NRES
      LTS=NUPO12+(NRES-IRES)*NTS*5+11
      LTSQL1=LTS+40
      LTSQL2=LTSQL1+5
      DO 155 I=1,NTS
      II=I+2
      IF(PO(LTS).EQ.BLANK)GO TO 152
      IT=PO(LTS+3)
      WRITE(IPU,750) GTSKW(II),PO(LTS),PO(LTS+1),PO(LTS+2),IT
152   LTS=LTS+5
155   CONTINUE
      WRITE(IPU,525)
C
C     CARRYOVER INFORMATION
C
      IF(PO(LTSQL1).NE.BLANK .OR. PO(LTSQL2).EQ.BLANK)
     & GO TO 160
      WRITE(IPU,530)
      QOI = CO(LCO)
C      EL1 = CO(LCO+1)
C      STO = CO(LCO+2)
      QL1 = CO(LCO+3)
      LCO=LCO+4
C
C
C      WRITE(IPU,1610) QOI
C      WRITE(IPU,1612) EL1
      WRITE(IPU,1616) QL1
      WRITE(IPU,535)
C
 160  WRITE(IPU,3750)
 3750 FORMAT('END3-VAR')
      GO TO 1000
C
C     END OF 3-VAR INFORMATION
C
C***********************************************************************
C     PUNCH LWERBKWR INFORMATION
C***********************************************************************
C
 1000 CONTINUE
      IRES=NRES
      LPO=PO(10)
      ISTYP=PO(LPO)
      LPO=LPO+1
      IRES=NRES
      LCO=LCOI+(NRES-IRES)*4+1      
      IF(ISTYP.NE.2) GO TO 2000
      WRITE(IPU,1705)
 1705 FORMAT('LWERBKWR')
      NTTS=PO(NUPO12)
      NTS=(NTTS-2)/NRES
      LTS=NUPO12+(NRES-IRES)*NTS*5+11
      CALL PUCB51(ISTYP,IRES,NRES,LPO,LTS,LCO,PO,CO)
C
      WRITE(IPU,1655)
 1655 FORMAT('ENDLWERB')
      GO TO 9000
C
C     END OF LWERBKWR INFORMATION
C
C***********************************************************************
C     PUNCH SAR INFORMATION
C***********************************************************************
C
 2000 CONTINUE
      WRITE(IPU,2710)
2710  FORMAT('SAR')
      WRITE(IPU,510)
      IRES=NRES
      LPO=PO(10)
      LPO=LPO+1
      NVAL=PO(LPO)
      IX=LPO
      IY=IX+NVAL
      IZ=IY+NVAL
      DO 205 I=1,NVAL
 205  X(I)=PO(IX+I )
      DO 207 I=1,NVAL
 207  Y(I)=PO(IY+I)
      DO 209 I=1,NVAL
 209  Z(I)=PO(IZ+I)
C
C  PUNCH ELVSSTOR CURVE
      IAMP=1
      IB=1
      IE=NVAL
      IF(IE .GT. 6) IE=6
      JP=IE-IB+1
      FMT2(9:12)=FNV(JP)
      WRITE(IPU,FMT2) KEYWD(1),(X(I),I=IB,IE),AMP(IAMP)
      IF(IE .GE. NVAL)GO TO 1116
      IB=IE+1
      IE=IE+6
      IF(IE .GT. NVAL)IE=NVAL
 1115 JP=IE-IB+1
      FMT2(9:12)=FNV(JP)
      WRITE(IPU,FMT2) KEYWD(3),(X(I),I=IB,IE),AMP(IAMP)
      IF(IE .GE. NVAL)GO TO 1116
      IB=IE+1
      IE=IE+6
      IF(IE .GT. NVAL)IE=NVAL
      GO TO 1115
C
1116  CONTINUE
      IB=1
      IE=NVAL
      IF(IE .GT. 6) IE=6
 1117 IF(IE.GE.NVAL) IAMP=2
      JP=IE-IB+1
      FMT0(9:12)=FNV(JP)
      WRITE(IPU,FMT0) KEYWD(3),(Y(I),I=IB,IE),AMP(IAMP)
      IF(IE .GE. NVAL)GO TO 1118
      IB=IE+1
      IE=IE+6
      IF(IE .GT. NVAL)IE=NVAL
      GO TO 1117
C
 1118  CONTINUE
C
C  PUNCH QVSEL CURVE
      IF(Z(NVAL).LE.1.0) GO TO 1120
      IAMP=1
      IB=1
      IE=NVAL
      IF(IE .GT. 6) IE=6
      IF(IE.GE.NVAL) IAMP=2
      JP=IE-IB+1
      FMT0(9:12)=FNV(JP)
      WRITE(IPU,FMT0) KEYWD(2),(Z(I),I=IB,IE),AMP(IAMP)
      IF(IE .GE. NVAL)GO TO 1120
      IB=IE+1
      IE=IE+6
      IF(IE .GT. NVAL) IE=NVAL
 1119 IF(IE.GE.NVAL) IAMP=2
      JP=IE-IB+1
      FMT0(9:12)=FNV(JP)
      WRITE(IPU,FMT0) KEYWD(3),(Z(I),I=IB,IE),AMP(IAMP)
      IF(IE .GE. NVAL)GO TO 1120
      IB=IE+1
      IE=IE+6
      IF(IE .GT. NVAL)IE=NVAL
      GO TO 1119
C
1120  CONTINUE
      LPO=LPO+3*NVAL+1
C
C  PUNCH MAXEL
      ELMAX=PO(LPO)
      WRITE(IPU,1730) ELMAX
1730  FORMAT('MAXEL',1X,F10.2)
      LPO=LPO+1
C
C  PUNCH MINEL
      ELMIN=PO(LPO)
      WRITE(IPU,1731) ELMIN
1731  FORMAT('MINEL',1X,F10.2)
      LPO=LPO+1
C
C  PUNCH MINQREL
      QRELMN=PO(LPO)
      WRITE(IPU,1732) QRELMN
 1732 FORMAT('MINQREL',1X,F10.0)
C
      WRITE(IPU,515)
C
      LPO=LPO+1
C
C***********************************************************************
C     SAR TIME SERIES INFORMATION
C
      WRITE(IPU,520)
      NTTS=PO(NUPO12)
      NTS=(NTTS-2)/NRES
      LTS=NUPO12+(NRES-IRES)*NTS*5+11
      LTSQO1=LTS
      LTSQO2=LTSQO1+5
      DO 255 I=1,NTS
      II=I+2
      IF(PO(LTS).EQ.BLANK)GO TO 252
      IT=PO(LTS+3)
      WRITE(IPU,750) GTSKW(II),PO(LTS),PO(LTS+1),PO(LTS+2),IT
252   LTS=LTS+5
255   CONTINUE
      WRITE(IPU,525)
C
C     SAR CARRYOVER INFORMATION
C
      WRITE(IPU,530)
      QOI = CO(LCO)
      EL1 = CO(LCO+1)
      STO = CO(LCO+2)
      LCO=LCO+4
C
      WRITE(IPU,1610) QOI
      WRITE(IPU,1612) EL1
      WRITE(IPU,1614) STO
1610  FORMAT('Q-INST ',1X,F10.0)
1612  FORMAT('POOL   ',1X,F10.2)
1614  FORMAT('STORAGE',1X,F10.0)
 1616 FORMAT('TRIBQL ',1X,F10.0)
      WRITE(IPU,535)
C
      WRITE(IPU,2650)
 2650 FORMAT('ENDSAR')
C
C     END OF SAR INFORMATION
9000  CONTINUE
C
      WRITE(IPU,701)
 701  FORMAT('END')
C
 510  FORMAT('PARMS')
 515  FORMAT('ENDP')
 520  FORMAT('TIME-SERIES')
 525  FORMAT('ENDTS')
 530  FORMAT('CARRYOVER')
 535  FORMAT('ENDCO')
      RETURN
      END
