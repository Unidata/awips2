C MEMBER PDPLOT
C  (from old member PPDBPLOT)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/11/94.08:27:20 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDPLOT (K,DATA,MSNG,IMO,IDAY,IYR,IHR,ITME)
C
C
C          ROUTINE:  PDPLOT
C
C             VERSION:  1.0.0
C
C                DATE:  1-11-84
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE IS CALLED TO SET UP THE STATE BOUNDARY PARAMETER
C    DATA TO PLOT MDR DATA.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C          K       I     I     1     PERIOD BEING PLOTTED(1-4)
C       DATA      I*2    I   # OF STA  MDR DATA
C       MSNG      I*2    I     1      MISSING DATA VALUE
C       IMO        I     I     1      MONTH OF DAT
C       IDAY       I     I     1      DAY OF DATA
C       IYR        I     I     1      YEAR OF DATA
C       IHR        I     I     1      HOUR OF DATA
C        ITME      A     I     1      TIME ZONE CODE(IE.EST,CDT)
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      COMMON/XMDR/MDRSTA,ICTMDR,MDRST6,ICMDR,NCMDR,IRMDR,NRMDR,NMDR,
     *            MDR,MDRNUV
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IDSTA(2),IWORK(85)
      INTEGER*2 DATA(8000),MSNG
      INTEGER PTR
      REAL ARRAY(3000),PLOT(85)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/pdplot.f,v $
     . $',                                                             '
     .$Id: pdplot.f,v 1.1 1995/09/17 19:09:29 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C***********************************************************************
C
C
      IF (IPDTR.EQ.1) WRITE (IOGDB,10)
10    FORMAT (' ENTER PDPLOT')
C
C
C  READ IN PPPDB CONTROLS
C
      CALL RPPPCO (ISTAT)
      IF (ISTAT.NE.0) GO TO 80
C
C  READ STBN PARAMETER RECORD
      CALL UMEMST (IBLNK,IDSTA,2)
      PTR=0
      LARRAY=3000
      CALL RPPREC (IDSTA,'STBN',PTR,LARRAY,ARRAY,NFILL,IPTRNX,ISTAT)
      IF (ISTAT.EQ.0) GO TO 20
      GO TO (100,40,60),ISTAT
C
C  FILL IN COMMON XMDR
20    MDRSTA=0
      ICTMDR=0
      MDRST6=0
      ICMDR=ARRAY(2)
      NCMDR=ARRAY(3)
      IRMDR=ARRAY(4)
      NRMDR=ARRAY(5)
      NMDR=NCMDR * NRMDR
      MDR=0
      MDRNUV=0
C
      CALL UPAGE (LP)
C
      WRITE (LP,30) K,IMO,IDAY,IYR,IHR,ITME
30    FORMAT (' SUMMED HOURLY MDR VALUES ',
     *       'FOR PERIOD ',I4,' OF THE HYDROLOGIC DAY WHICH ',
     *       'IS THE 6 HOUR PERIOD ENDING AT ',2(I2.2,'/'),I4.4,2X,
     *       'HR ',I2.2,1X,A4 /
     *       ' NOTE: (-) SIGN INDICATES AN ESTIMATED MDR VAZUE')
C
C  CALL PLOT PROGRAM
      CALL XMDRPL (ARRAY(8),DATA,MSNG,IWORK,PLOT)
      GO TO 120
C
C  ERROR MESSAGES
C
40    WRITE (LPE,50) ISTAT
50    FORMAT (' **ERROR** IN PDPLOT. PARAMETER RECORD FOR ',
     *       ' STBN NOT FOUND. ISTAT=',I2)
      GO TO 140
C
60    WRITE (LPE,70) NFILL,ISTAT
70    FORMAT (' **ERROR** IN PDPLOT. WORK ARRAY TOO SMALL FOR ',
     *       ' PARAMETER DATA. SHOULD BE REDIMENSIONED TO ',I4,
     *       ' ISTAT=' ,I2)
      GO TO 140
C
C  SYSTEM ERROR
80    WRITE (LPE,90) ISTAT
90    FORMAT (' **ERROR** IN PDPLOT. CALLING RPPPCO. ISTAT=',I2)
      GO TO 140
C
100   WRITE (LPE,110) ISTAT
110   FORMAT (' **ERROR** SYSTEM ERROR IN PDPLOT ACCESSING ',
     *       ' PARAMETER FILE. ISTAT=',I2)
      GO TO 140
C
120    IF (IPDDB.EQ.1) WRITE (IOGDB,130)
130   FORMAT (' EXIT PDPLOT')
C
140   RETURN
C
      END
