C MEMBER PRP51
C***********************************************************************
C
C@PROCESS LVL(77)
C
      SUBROUTINE PRP51(PO)
C
C     ROUTINE PRINTS THE INFORMATION STORED IN THE P ARRAY FOR THE
C     SSARRESV RESERVOIR OPERATION.
C
C***********************************************************************
C     PROGRAMMED BY KUANG HSU  OCTOBER 1994
C***********************************************************************
      DIMENSION PO(*),PR51(2),X(365),Y(365),Z(365),BWTYP(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fengmt'
      INCLUDE 'common/unit51'
      REAL*8 GTSKW(15)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp51.f,v $
     . $',                                                             '
     .$Id: prp51.f,v 1.3 2006/03/16 16:28:44 xfan Exp $
     . $' /
C    ===================================================================
C
C
      DATA PR51,BLANK/4HPRP5,4H1   ,4H    /
C
      DATA GTSKW/8HINSTQI1 ,8HINSTQI2 ,8HINSTQO1 ,8HINSTQO2 ,8HMEANQOUT,
     1           8HPOOL    ,8HSTORAGE ,8HOBSQO   ,8HOBSQOM  ,8HOBSH    ,
     1           8HTRIBQL1 ,8HTRIBQL2 ,8HBACKQI1 ,8HBACKQI2 ,8HBACKQIM /
      DATA BWTYP/4HFLOW,4HELEV/
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
      NRES = PO(9)
      CALL ETOM51(PO)

C dws    PO(12) was placed into an integer to replace it in the rest
C dws     of the statements to avoid compiler warnings ... 2006-01-23

      NUPO12 = PO(12)

C
C     PRINT HEADING
      WRITE(IPR,700) (PO(I),I=2,6)
 700  FORMAT(/10X,'RESERVOIR OPERATION FOR ',5A4)
      ITIM = PO(7)
      WRITE(IPR,705) ITIM
 705  FORMAT(/10X,'COMPUTATIONAL TIME INTERVAL FOR THE OPERATION =',
     1 I3,7H HOURS.)
C
C***********************************************************************
C     PRINT INFLOW INFORMATION
C***********************************************************************
C
      WRITE(IPR,710)
 710  FORMAT(//,40X,'***** INFLOW INFORMATION *****')
C
C     INFLOW TIME SERIES INFORMATION
      WRITE(IPR,745)
 745  FORMAT(/10X,20H INFLOW TIME SERIES:,13X,2HID,10X,4HTYPE,
     1 10X,8HTIME(HR)/)
      LTS=NUPO12+1
      DO 25 I=1,2
      IF(PO(LTS).EQ.BLANK)GO TO 22
      IT=PO(LTS+3)
      WRITE(IPR,750) GTSKW(I),PO(LTS),PO(LTS+1),PO(LTS+2),IT
 750  FORMAT(21X,A8,10X,2A4,8X,A4,12X,I2)
 22   LTS=LTS+5
 25   CONTINUE
C
C     END OF INFLOW INFORMATION
C
C***********************************************************************
C     PRINT UPERBKWR INFORMATION
C***********************************************************************
C
      IF(NRES.LE.1) GO TO 1000
      IRES=1
      LPO=PO(11)
      ISTYP=PO(LPO)
      LPO=LPO+1
      IF(ISTYP.EQ.3) GO TO 300
      WRITE(IPR,1710)
1710  FORMAT(//,40X,'**** UPERBKWR INFORMATION *****')
      NTTS=PO(NUPO12)
      NTS=(NTTS-2)/NRES
      LTS=NUPO12+(NRES-IRES)*NTS*5+11
      CALL PRPB51(LPO,LTS,PO)
      GO TO 1000
C
C     END OF UPERBKWR INFORMATION
C
C
C***********************************************************************
C     PRINT 3-VAR INFORMATION
C***********************************************************************
C
 300  CONTINUE
      WRITE(IPR,3710)
3710  FORMAT(//,40X,'**** 3-VAR INFORMATION*****')
      IRES=1
      LPO=PO(11)
      LPO=LPO+1
      NVAL=PO(LPO)
      LPO=LPO+3*NVAL+1
C
C  CONVERTS THREE-VARIABLE TABLE
C  THREE VALUES PER POINT IN THE ORDER OF 
C  SECOND INDEPENDENT VARIABLE (Z), DISCHARGE OR ELEVATION
C  FIRST INDEPENDENT VARIABLE (X), DISCHARGE OR ELEVATION,
C  DEPENDENT VARIABLE (Y), DISCHARGE OR ELEVATION, 
C  
320   CONTINUE
      NVAL=PO(LPO)
      LCON1=LPO+3*NVAL+1
      ICON1=PO(LCON1)
      LCON2=LCON1+1
      ICON2=PO(LCON2)
      LSITE=LCON2+1
      ISITE=PO(LSITE)
C
      DO 335 I=1,NVAL
      IZ=LPO+(I-1)*3+1
      Z(I)=PO(IZ)
      IX=IZ+1
      X(I)=PO(IX)
      IY=IX+1
      Y(I)=PO(IY)
 335  CONTINUE
C
      WRITE(IPR,3820)
      IB=1
      IE=NVAL
      IF(IE .GT. 3) IE=3
 340  WRITE(IPR,3825) (Z(I),X(I),Y(I),I=IB,IE)
      IF(IE .GE. NVAL)GO TO 350
      IB=IE+1
      IE=IE+3
      IF(IE .GT. NVAL) IE=NVAL
      GO TO 340
C
3820  FORMAT(/10X,'THREE-VARIABLE TABLE: ',
     & //23X,8HIND-VAR2,3X,8HIND-VAR1,4X,7HDEP-VAR,
     & 3X,8HIND-VAR2,3X,8HIND-VAR1,4X,7HDEP-VAR,
     & 3X,8HIND-VAR2,3X,8HIND-VAR1,4X,7HDEP-VAR)
3825  FORMAT(20X,3(3F11.2))
C
 350  CONTINUE
      LPO=LPO+3*NVAL+1
C
C  PRINT FIRST INDEPENDENT VARIABLE CONTROL TYPE
      ICON1=PO(LPO)
      WRITE(IPR,3715) BWTYP(ICON1)
3715  FORMAT(/10X,
     & 'FIRST INDEPENDENT VARIBLE, X,',
     & ' BACKWATER CONTROL AT DWONSTREAM RESERVOIR:',5X,A4) 
       LPO=LPO+1
C
C  PRINT SECOND INDEPENDENT VARIABLE CONTROL TYPE
      ICON2=PO(LPO)
      WRITE(IPR,3716) BWTYP(ICON2)
 3716 FORMAT(/10X,'SECOND INDEPENDENT VARIBLE, Z,',
     & ' AT UPSTREAM STATION:',5X,A4)
      LPO=LPO+1
C
C  PRINT DEPENDENT VARIABLE TYPE
      ISITE=PO(LPO)
      WRITE(IPR,3717) BWTYP(ISITE)
 3717 FORMAT(/10X,'DEPENDENT VARIBLE, Y,',
     & ' AT UPSTREAM STATION:',5X,A4)
      LPO=LPO+1
C
C***********************************************************************
C     3-VAR TIME SERIES INFORMATION
      WRITE(IPR,3745)
3745  FORMAT(/10X,20H        TIME SERIES:,13X,2HID,10X,4HTYPE,
     1 10X,8HTIME(HR)/)
      NTTS=PO(NUPO12)
      NTS=(NTTS-2)/NRES
      LTS=NUPO12+(NRES-IRES)*NTS*5+11
      DO 355 I=1,NTS
      II=I+2
      IF(PO(LTS).EQ.BLANK)GO TO 352
      IT=PO(LTS+3)
      WRITE(IPR,3750) GTSKW(II),PO(LTS),PO(LTS+1),PO(LTS+2),IT
3750  FORMAT(21X,A8,10X,2A4,8X,A4,12X,I2)
352   LTS=LTS+5
355   CONTINUE
C
C     END OF 3-VAR INFORMATION
C
C***********************************************************************
C     PRINT LWERBKWR INFORMATION
C***********************************************************************
C
 1000 CONTINUE
      IRES=NRES
      LPO=PO(10)
      ISTYP=PO(LPO)
      LPO=LPO+1
      IF(ISTYP.NE.2) GO TO 2000
      WRITE(IPR,1705)
 1705 FORMAT(//,40X,'**** LWERBKWR INFORMATION *****')
      NTTS=PO(NUPO12)
      NTS=(NTTS-2)/NRES
      LTS=NUPO12+(NRES-IRES)*NTS*5+11
      CALL PRPB51(LPO,LTS,PO)
      GO TO 9000
C
C     END OF LWERBKWR INFORMATION
C
C
C***********************************************************************
C     PRINT SAR INFORMATION
C***********************************************************************
C
 2000 CONTINUE
      WRITE(IPR,2710)
2710  FORMAT(//,40X,'**** SAR INFORMATION *****')
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
      WRITE(IPR,1720)
1720  FORMAT(/10X,'STORAGE VS. ELEVATION CURVE:')
      IB=1
      IE=NVAL
      IF(IE .GT. 8)IE=8
210   WRITE(IPR,1725)UNITL,(X(I),I=IB,IE)
      WRITE(IPR,1726)UNITST,(Y(I),I=IB,IE)
      IF(Z(NVAL).GE.1.0) WRITE(IPR,1727)UNITQ,(Z(I),I=IB,IE)
1725  FORMAT(/20X,' ELEV(',A4,1H),8F11.2)
1726  FORMAT(20X,' STOR(',A4,1H),8F11.0)
1727  FORMAT(20X,'DISCH(',A4,1H),8F11.1)
      IF(IE .GE. NVAL)GO TO 220
      IB=IE+1
      IE=IE+8
      IF(IE .GT. NVAL)IE=NVAL
      GO TO 210
C
220   CONTINUE
      LPO=LPO+3*NVAL+1
C
C  PRINT MAXEL
      ELMAX=PO(LPO)
      WRITE(IPR,2730)UNITL,ELMAX
2730  FORMAT(/10X,'MAXIMUM ELEVATION (',A4,2H)=,F15.2)
      LPO=LPO+1
C
C  PRINT MINEL
      ELMIN=PO(LPO)
      WRITE(IPR,2731)UNITL,ELMIN
2731  FORMAT(/10X,'MINIMUM ELEVATION (',A4,2H)=,F15.2)
      LPO=LPO+1
C
C  PRINT MINQREL
      QRELMN=PO(LPO)
      WRITE(IPR,2732)UNITQ,QRELMN
 2732 FORMAT(/10X,'MINIMUM RESERVOIR RELEASE (',A4,2H)=,F15.0)
      LPO=LPO+1
C
C***********************************************************************
C     SAR TIME SERIES INFORMATION
      WRITE(IPR,2745)
2745  FORMAT(/10X,20H        TIME SERIES:,13X,2HID,10X,4HTYPE,
     1 10X,8HTIME(HR)/)
      NTTS=PO(NUPO12)
      NTS=(NTTS-2)/NRES
      LTS=NUPO12+(NRES-IRES)*NTS*5+11
      DO 255 I=1,NTS
      II=I+2
      IF(PO(LTS).EQ.BLANK)GO TO 252
      IT=PO(LTS+3)
      WRITE(IPR,2750) GTSKW(II),PO(LTS),PO(LTS+1),PO(LTS+2),IT
2750  FORMAT(21X,A8,10X,2A4,8X,A4,12X,I2)
252   LTS=LTS+5
255   CONTINUE
C
C     END OF SAR INFORMATION
C
9000  CONTINUE
C
      RETURN
      END
