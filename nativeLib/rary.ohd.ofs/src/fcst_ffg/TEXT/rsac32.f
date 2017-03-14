C MEMBER RSAC32
C  (from old member FCEX32)
C VERSION 1.10
C######################################################################
C***********************************************************************
C @PROCESS LVL(77)
      SUBROUTINE RSAC32(PL,CL,EPDIST,IFRZE,FGCO1,IDT,IPRINT)
C.......................................................................
C  THIS ROUTINE RETRIEVES PARAMETERS AND CARRIVER FOR SAC-SMA MODEL
C
C.......................................................................
C  INITIALY WRITTEN BY
C         JANICE LEWIS, HYDROLOGIC RESEARCH LAB, OCT 1991
C.......................................................................
C  PRINCIPLE VARIABLES...
C
C  FOR DEFINITION OF VARIABLES IN COMMON BLOCKS, SEE SECTION IX.3.3C
C  OF THE NWSRFS USER'S MANUAL.
C
C.......................................................................
      DIMENSION PL(*),CL(*),EPDIST(24),SNAME(2)
C.......................................................................
      REAL LZTWM,LZFSM,LZFPM,LZSK,LZPK,LZTWC,LZFSC,LZFPC,LZTWC1,
     1     LZFSC1,LZFPC1
C.......................................................................
      INCLUDE 'common/fctime'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fsacpr'
      COMMON/FSMPM1/UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC,REXP,LZTWM,
     1              LZFSM,LZFPM,LZSK,LZPK,PFREE,SIDE,SAVED,PAREA
      COMMON/FSMCO1/UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC,FGCO(6),RSUM(7),
     1              PPE,PSC,PTA,PWE
      COMMON/FSUMS1/SROT,SIMPVT,SRODT,SROST,SINTFT,SGWFP,SGWFS,SRECHT,
     1              SETT,SE1,SE3,SE4,SE5
      COMMON/FSMPT1/IROC,IROCO,ISC,ISCO,IET,IETCO,LWE,NXCO,NXFCO
      COMMON /SACIN/ UZTWC1,UZFWC1,LZTWC1,LZFSC1,LZFPC1,ADIMC1
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ffg/RCS/rsac32.f,v $
     . $',                                                             '
     .$Id: rsac32.f,v 1.2 1995/11/14 20:30:40 erb Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
C
      DATA SNAME/4HRSAC,4H32  /
C.......................................................................

C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS ROUTINE=1.
C     CHECK TO SEE IF DEBUG OUTPUT IS NEEDED FOR THIS OPERATION.
      CALL FPRBUG(SNAME,1,32,IBUG)
C.......................................................................
C     VALUES OF CONTROL VARIABLES.
      IET=0
      ISC=0
      IROC=0
      ISM=0
      NXCO=PL(23)
      ISUM=0
      IPRINT=0
      IF (IBUG.EQ.1) IPRINT=1
CC      IF (NOFRZE.EQ.1) IPRINT=0
      IPLPM=PL(20)
      IPLET=PL(21)
      NV=24/IDT
      IFRZE=PL(24)
      NC=NXCO+6
      IF (IFRZE.EQ.0) GO TO 103
      ITTA=PL(IFRZE+3)
      NTA=24/ITTA
      LTAD=(LHR/ITTA)*ITTA
      NXFCO=PL(IFRZE+6)
      LWE=PL(IFRZE+4)
      IF (LWE.EQ.0) GO TO 104
      ITWE=PL(IFRZE+LWE+2)
      NWE=24/ITWE
      LWED=(LHR/ITWE)*ITWE
  104 LFI=PL(IFRZE+5)
      IF (LFI.EQ.0) GO TO 109
      ITFI=PL(IFRZE+LFI+1)
      NFI=24/ITFI
  109 NC=NC+6+NXFCO
  103 LP=PL(25)
C.......................................................................
C     DEBUG OUTPUT - PRINT PL( ) AND CL( )
      IF (IBUG.EQ.0) GO TO 105
      WRITE (IODBUG,910) LP,NC
  910 FORMAT(1H0,29HCONTENTS OF PL AND CL ARRAYS.,5X,
     121HNUMBER OF VALUES--PL=,I3,2X,3HCL=,I2)
      WRITE (IODBUG,901) (PL(I),I=1,LP)
  901 FORMAT (1H0,15F8.3)
      WRITE (IODBUG,901) (CL(I),I=1,NC)
C.......................................................................
C     DEFINE DAILY ET-DISTRIBUTION FOR INTERNAL TIME
C     UNIFORM DAILY ET DISTRIBUTION
  105 V=1.0/NV
      DO 107 I=1,NV
  107 EPDIST(I)=V
C.......................................................................
C     PRINT HEADING FOR DETAILED OUTPUT
  110 IF (IBUG.EQ.1) IPRINT=1
      IF (IPRINT.EQ.0) GO TO 115
C     GET ACTUAL TIME FOR START OF EXECUTION PERIOD.
      CALL MDYH1 (LDACPD,LHRCPD,MONTH,IDAY,IYEAR,IHOUR,NOUTZ,NOUTDS,
     .            TZCODE)
CC      IF(IBUG.EQ.0) GO TO 115
CC      WRITE (IODBUG,903)
CC  903 FORMAT (1H1)
CC      WRITE (IODBUG,902) (PL(I),I=3,7),MONTH,IYEAR,TZCODE
CC  902 FORMAT (1H0,44HDETAILED SOIL-MOISTURE ACCOUNTING OUTPUT FOR,
CC     11X,5A4,5X,I2,1H/,I4,3X,10HTIME ZONE=,A4,10X,13HUNITS ARE MM.)
CC      WRITE (IODBUG,904)
CC  904 FORMAT (1H0,3HDAY,1X,2HHR,2X,5HUZTWC,2X,5HUZFWC,2X,5HLZTWC,
CC     12X,5HLZFSC,2X,5HLZFPC,2X,5HADIMC,3X,4HPERC,4X,3HIMP,4X,3HDIR,4X,
CC     23HSUR,4X,3HINT,4X,3HSUP,4X,3HPRI,2X,6HTOT-RO,2X,6HET-DMD,1X,
CC     36HACT-ET,2X,9HRAIN+MELT)
C.......................................................................
C     PARAMETER VALUES
  115 CALL FPMCO1(PL,CL,IPLPM,IFRZE)
C
C     SET FROST INDEX TO ZERO IF NOFRZE=1
      IF ((IFRZE.GT.0).AND.(NOFRZE.EQ.1)) FGCO(1)=0.0
      FGCO1 = FGCO(1)
C
C     STORE INITIAL CARRYOVER
      UZTWC1=UZTWC
      UZFWC1=UZFWC
      LZTWC1=LZTWC
      LZFSC1=LZFSC
      LZFPC1=LZFPC
      ADIMC1=ADIMC
C
C     CHECK FOR CHANGES TO CARRYOVER.
      BALADJ=0.0
      NM=0
      J=0
      L=IHR-IDT
  160 CALL FCKCO1(IDA,L,NOUTZ,NOUTDS,J,IFRZE,BALADJ,IBUG,IPRINT,IODBUG)
C
C     INITIALIZE SUMS
      SROT=0.0
      SIMPVT=0.0
      SRODT=0.0
      SROST=0.0
      SINTFT=0.0
      SGWFP=0.0
      SGWFS=0.0
      SRECHT=0.0
      SETT=0.0
      SPRT=0.0
      SPET=0.0
      SE1=0.0
      SE3=0.0
      SE4=0.0
      SE5=0.0
      SRO=0.0
      SRECH=0.0
      SET=0.0
      SPR=0.0
C
      RETURN
      END
