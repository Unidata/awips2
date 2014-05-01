C MEMBER FRCLEX
C-----------------------------------------------------------------------
C     DESC - THIS ROUTINE READS THE CALIBRATION FILES FOR THE
C     DESC - FORECAST COMPONENT EXECUTION PROGRAM.
C     DESC - IT IS CALLED FROM FCTSRD.
C     DESC - THIS ROUTINE WILL BE USED UNTIL THE PROCESSED DATA
C     DESC - FILES ARE IMPLEMENTED, AT WHICH TIME A ROUTINE
C     DESC - TO READ DATA FROM THOSE FILES FOR THE FC EXECUTION
C     DESC - WILL BE WRITTEN.
C.............................
C      ORIGINALLY WRITTEN BY GEORGE F SMITH - HRL - 4/20/80
C.............................
      SUBROUTINE FRCLEX (EXTL,D,W,LW,IDT,NPDT,UNITS,IERR)
C
      INCLUDE 'common/fctime'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'common/ionum'
      INCLUDE 'clbcommon/crwctl'
      COMMON/BHTIME/IBHREC,CMONTH,CDAY,CYEAR,CHRMN,CSCNDS,LTSHDR
      COMMON/INTLRW/INTLRD,INTLWT
C.............................
C
      DIMENSION EXTL(*),D(1),W(LW)
      DIMENSION SBNAME(2),OLDOPN(2),FNAME(3),STAID(3),DESCRP(5)
C
      INTEGER CMONTH,CDAY,CYEAR,CHRMN,CSCNDS,YEAR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fdriv/RCS/frclex.f,v $
     . $',                                                             '
     .$Id: frclex.f,v 1.2 1998/10/14 15:05:42 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA SBNAME/4HFRCL,4HEX  /
      DATA DESCRP/5*4H    /
C
      IZERO=0
C
      DO 5 I=1,2
      OLDOPN(I)=OPNAME(I)
    5 OPNAME(I)=SBNAME(I)
      IOLDOP=IOPNUM
      IOPNUM=0
C
      IF(ITRACE.GE.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,17H **FRCLEX ENTERED)
C.......................................................................
C
C     ZERO CALIBRATION R/W BOOKKEEPING SYSTEM
C
      INTLRD=0
      INTLWT=0
      IERROR=0
C
      DO 10 I=1,3
      FNAME(I)=EXTL(I)
   10 STAID(I)=EXTL(6+I)
      DTYPE=EXTL(4)
      IDTI=EXTL(5)
      IBHREC=EXTL(6)
      CMONTH=EXTL(10)
      CDAY=EXTL(11)
      CYEAR=EXTL(12)
      CHRMN=EXTL(13)
      CSCNDS=EXTL(14)
C....................
C
      NDDATA=LDARUN-IDARUN+1
      NVPD=(24/IDTI)*NPDT
      NDT=NVPD*31
C
      IF (LW .LT. NDT) THEN
        WRITE(IPR,600)IDTI,LW
  600   FORMAT('0',10X,'**ERROR** THE TIME STEP FOR THE TIME SERIES '
     1   ,'BEING READ (',I2,' HOURS)',/
     2   ,11X,' NEEDS MORE WORKING SPACE THAN IS AVAILABLE (',I5
     3   ,' LOCATIONS ARE AVAILABLE)')
        CALL ERROR
        IERR=1
        GO TO 9999
      ENDIF
C
      IDAX=IDARUN
      IHRX=12
      IF(IHRRUN.EQ.24)IDAX=IDAX+1
C
      CALL MDYH1(IDAX,IHRX,IMONTH,IDAY,IYEAR,IHOUR,NLSTZ,IZERO,TZC)
C
      CALL MDYH1(LDARUN,LHRRUN,LMONTH,LDAY,LYEAR,LHOUR,NLSTZ,IZERO,TZC)
C
      LCMNTH=LMONTH
      IF(LMONTH.LT.IMONTH)LCMNTH=LCMNTH+12
C
      ICMNTH=IMONTH
      MONTH=IMONTH
      YEAR=IYEAR
C
      ILOCW=(IDAY-1)*NVPD + 1
      KOUNT=0
C
   30 IF(ICMNTH.GT.LCMNTH)GO TO 9999
C
      CALL RDFILE(1,FNAME,1,NXTRD,LPTR,1,MONTH,YEAR,1,NDT,W)
C
      IF (IERROR .NE. 0) THEN
        IERROR=0
        IERR=1
        WRITE(IPR,602)FNAME,MONTH,YEAR
  602   FORMAT('0',10X,'**ERROR** UNABLE TO READ DATA FOR TIME SERIES '
     $    ,3A4,/,23X,'MONTH=',I3,', YEAR=',I5)
        CALL ERROR
        GO TO 9999
      ENDIF
C
C  Get number of days in month (NLOCW) using subroutine DDGCDM
C
      CALL DDGCDM(YEAR,MONTH,NLOCW)
C
      NLOCW=NLOCW*NVPD
      IF(NDDATA-KOUNT.LT.(NLOCW-ILOCW+1)/NVPD)
     1       NLOCW=(NDDATA-KOUNT)*NVPD + ILOCW - 1
C
      LOCD=KOUNT*NVPD
      KOUNT=KOUNT+(NLOCW-ILOCW+1)/NVPD
C
      J=0
      DO 40 I=ILOCW,NLOCW
      J=J+1
   40 D(LOCD+J)=W(I)
C
      ILOCW=1
      ICMNTH=ICMNTH+1
      MONTH=MONTH+1
      IF (MONTH .GT. 12) THEN
        MONTH=1
        YEAR=YEAR+1
      ENDIF
      GO TO 30
C
 9999 IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
C
      RETURN
      END
