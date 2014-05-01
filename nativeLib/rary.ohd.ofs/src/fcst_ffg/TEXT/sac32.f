C MEMBER SAC32
C  (from old member FCEX32)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/08/95.14:08:22 BY $WC20SV
C
C  =====================================================================
C  VERSION 1.10
C
C @PROCESS LVL(77)
      SUBROUTINE SAC32(PL,EPDIST,IFRZE,KINT,DT,EP,PXV,STRO,TA,IDAY,
     .                 IHOUR,MONTH,IYEAR,TZCODE,WE,AESC)
C
C.......................................................................
C  THIS ROUTINE COMPUTES THE RUNOFF USING THE SMA-SAC MODEL
C
C.......................................................................
C  INITIALY WRITTEN BY
C       JANICE LEWIS, HYDROLOGIC RESEARCH LAB, DEC 1991
C
C  Added arguments (MONTH,IYEAR,KDA,IOUTYP,OPNAME) to FLAND1 call
C  and added common blocks WHERE and OUTCTL for ICP.
C       Tim Sweeney, HRL                              Sept 1995
C
C  Added debug output of carryovers before FLAND1.
c       Tim Sweeney, HL                               Nov 2000
C.......................................................................
C  PRINCIPLE VARIABLES...
C
C.......................................................................
      REAL LZTWM,LZFSM,LZFPM,LZSK,LZPK,LZTWC,LZFSC,LZFPC,LZTWC1,
     1     LZFSC1,LZFPC1
C.......................................................................
      INCLUDE 'common/fctime'
      INCLUDE 'common/fdbug'
      COMMON/FSMPM1/UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC,REXP,LZTWM,
     1              LZFSM,LZFPM,LZSK,LZPK,PFREE,SIDE,SAVED,PAREA
      COMMON/FSMCO1/UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC,FGCO(6),RSUM(7),
     1              PPE,PSC,PTA,PWE
      COMMON/FSMPT1/IROC,IROCO,ISC,ISCO,IET,IETCO,LWE,NXCO,NXFCO
      COMMON/FSUMS1/SROT,SIMPVT,SRODT,SROST,SINTFT,SGWFP,SGWFS,SRECHT,
     1              SETT,SE1,SE3,SE4,SE5
      COMMON /SACIN/ UZTWC1,UZFWC1,LZTWC1,LZFSC1,LZFPC1,ADIMC1
      INCLUDE 'common/where'
      COMMON /OUTCTL/ IOUTYP
C.......................................................................
      DIMENSION SNAME(2),EPDIST(24),PL(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ffg/RCS/sac32.f,v $
     . $',                                                             '
     .$Id: sac32.f,v 1.4 2005/10/21 13:40:54 wkwock Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
      DATA SNAME/4hSAC3,4h2   /
C.......................................................................
C
C  CALL DEBUG CHECK ROUTINE
C  TRACE LEVEL=1, DEBUG FLAG=IBUG
C
      VERS=1.11
C
      CALL FPRBUG (SNAME,1,32,IBUG)
CC      IF(IBUG.GT.0) WRITE(IODBUG,7001) SNAME,VERS
CC 7001 FORMAT(/5X,2A4,' DEBUG OUTPUT.',5X,'VERSION:',F4.2)
C.......................................................................
C     PRINT HEADING FOR DETAILED OUTPUT
      IPRINT=0
      IF (IBUG.EQ.1) IPRINT=1
      IF (IPRINT.EQ.0) GO TO 100
C     GET ACTUAL TIME FOR START OF EXECUTION PERIOD.
CC      CALL MDYH1 (LDACPD,LHRCPD,MONTH,IDAY,IYEAR,IHRR,NOUTZ,NOUTDS,
CC     .            TZCODE)
CC      WRITE (IODBUG,903)
CC  903 FORMAT (' ')
      WRITE (IODBUG,902) (PL(I),I=3,7),MONTH,IYEAR,TZCODE
  902 FORMAT ('0','DETAILED SOIL-MOISTURE ACCOUNTING OUTPUT FOR',
     11X,5A4,5X,I2,'/',I4,3X,'TIME ZONE=',A4,10X,'UNITS ARE MM.')
ckwz.r26-28.4/28/05.add frost index header
      if (IFRZE .LE. 0) then  !no frost index
	    WRITE (IOUT,904)
	  else   !yes frost index or FGIX
	    WRITE (IOUT,905)
	  endif
  904 FORMAT ('0','DAY HR  UZTWC  UZFWC  LZTWC',
     1 '  LZFSC  LZFPC  ADIMC   PERC',4X,'IMP',4X,'DIR',4X,
     2 'SUR',4X,'INT',4X,'SUP',4X,'PRI  TOT-RO  ET-DMD ',
     3 'ACT-ET  RAIN+MELT')
  905 FORMAT ('0','DAY HR  UZTWC  UZFWC  LZTWC',
     1 '  LZFSC  LZFPC  ADIMC     FGIX   PERC',4X,'IMP',4X,'DIR',4X,
     2 'SUR',4X,'INT',4X,'SUP',4X,'PRI  TOT-RO  ET-DMD ',
     3 'ACT-ET  RAIN+MELT')
C.......................................................................
  100 UZTWC = UZTWC1
      UZFWC = UZFWC1
      LZTWC = LZTWC1
      LZFSC = LZFSC1
      LZFPC = LZFPC1
      ADIMC = ADIMC1
      SROT   = 0.0
      SIMPVT = 0.0
      SRODT  = 0.0
      SROST  = 0.0
      SINTFT = 0.0
      SGWFP  = 0.0
      SGWFS  = 0.0
      SRECHT = 0.0
      SETT   = 0.0
      SE1    = 0.0
      SE3    = 0.0
      SE4    = 0.0
      SE5    = 0.0
C
      IF(IPRINT.EQ.1) WRITE(IODBUG,908) UZTWC,UZFWC,LZTWC,
     +                LZFSC,LZFPC,ADIMC
  908 FORMAT( 4X,'CO:',F7.2,F7.3,F7.2,F7.3,2F7.2)
C
      CALL FLAND1(KINT,DT,PXV,EP,EPDIST,STRO,IPRINT,IDAY,IHOUR,
     1      IODBUG,IFRZE,TA,LWE,WE,ISC,AESC,IBUG,MONTH,IYEAR,
     2      KDA,IOUTYP,OPNAME)
      RETURN
      END
