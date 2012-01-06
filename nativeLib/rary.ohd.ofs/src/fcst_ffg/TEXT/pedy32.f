C MEMBER PEDY32
C  (from old member FCEX32)
C VERSION 1.10
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/08/95.14:08:22 BY $WC20SV
C
C#######################################################################
C @PROCESS LVL(77)
      SUBROUTINE PEDY32(IMO,IDA,IVOPT,PE,TAVG)
C.......................................................................
C  THIS ROUTINE COMPUTES THE PED AND Y PARAMETERS FOR API-CONT MODEL
C.......................................................................
C     INITIALLY WRITTEN BY -- JANICE LEWIS, HRL, DEC 1991
C             (EXTRACTED FROM ROUTINE FCAPIC24)
c     Updated cb RSPM24 to match ex24 by adding APIX & APIKS
c           Tim Sweeney    HRL                                 Nov 1995
C.......................................................................
      COMMON/RSPM24/APIK,AIXW,AIXD,CW,CD,SMIX,CS,FRSX,APIX,PEX,PEN,
     -EFC,PIMPV,RIVA,RVAI,WKW,WKD,AEIK,AEIX,AEIN,ATIR,ATIX,ATIN,APIKS
      COMMON/RSCO24/API,SMI,AEI,ATI,FI,FEI,Y,PED
C.......................................................................
      DIMENSION JDM(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ffg/RCS/pedy32.f,v $
     . $',                                                             '
     .$Id: pedy32.f,v 1.2 1995/11/01 20:24:30 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
      DATA JDM/0,31,59,90,120,151,181,212,243,273,304,334/
      DATA PI/3.1416/
C.......................................................................
C  COMPUTE JULIAN DAY -- IGNORE LEAP YEAR
C
   50 JDA=JDM(IMO)+IDA
      FJDA=JDA
C
C.......................................................................
C  COMPUTE DAILY PE OR SEASONAL PE ADJUSTMENT
      FMJD=FJDA-105.0
      PED=(PEX+PEN)*0.5+0.5*(PEX-PEN)*SIN(2.0*PI*FMJD/365.0)
      IF (IVOPT.EQ.1) PED=PE*PED
      IF (IVOPT.NE.0) GO TO 60
C
C**********************************************************
C   COMPUTE WEEK NUMBER AND POSITION BETWEEN WET AND DRY
C**********************************************************
C
      WK=FJDA/7.0
      CALL WEEK24(WK,WKW,WKD,F,IRISE)
      Y=(1.0+COS(PI*(1.0-F)))*0.5
      IF (IRISE.EQ.0) Y=Y**CS
      GO TO 100
C
   60 IF (IVOPT.NE.1) GO TO 70
C****************************************
C    COMPUTE AEI AND POSITION
C****************************************
C
      AEI=AEIK*AEI+PED
      IF (AEI.GT.AEIX)  AEI=AEIX
      IF (AEI.LT.AEIN)  AEI=AEIN
      Y=(AEI-AEIN)/(AEIX-AEIN)
      GO TO 100
C
C*************************************************
C   COMPUTE ATI AND POSITION
C*************************************************
C
   70 ATI=ATI+ATIR*(TAVG-ATI)
      IF (ATI.GT.ATIX) ATI=ATIX
      IF (ATI.LT.ATIN) ATI=ATIN
      Y=(ATI-ATIN)/(ATIX-ATIN)
  100 RETURN
      END
