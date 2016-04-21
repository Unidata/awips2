      SUBROUTINE TADJ_MPEUTIL(LYEAR,LMON,LDAY,LHOUR,LMIN,LSEC,KADJ,IADJ)
C
C****************************************************************
C   This routine is extracted from the SHEF decoder and modified
C   to exclude certain error returns.
C****************************************************************
C
C      This routine makes adjustments to the current date/time
C      by adding the value of IADJ. The units of LADJ are given
C      in the code IADJ where:
C           1 = minutes
C           2 = hours
C           3 = days
C           4 = months
C           5 = years
C           6 = months; end of month
C           7 = seconds
C
C      It is intended that the range of the value LADJ
C      be plus/minus 99, except for minutes where the range
C      may be plus/minus 1440. Also the value of the year
C      may not change by more than one.
c      LYEAR is the 4 digit year.
C
      DIMENSION IDAY(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/awips/whfs/dev/HP/precip_proc/source/pproc_util/src/RCS/tadj.f,v $
     . $',                                                             '
     .$Id: tadj.f,v 1.1 2001/10/11 19:11:20 pst Exp $
     . $' /
C    ===================================================================
C
      DATA IDAY /31,28,31,30,31,30,31,31,30,31,30,31/
C
C
C.... IS THERE AN ADJUSTMENT TO MAKE?
C
      IF( KADJ.EQ.0 ) GO TO 900
      LADJ = KADJ
C
C.... CHECK IADJ TO SEE WHAT'S BEING ADJUSTED
C
      GO TO (100,200,300,400,500,600,50), IADJ
C
C.... ADJUSTING SECONDS
C
   50 LSEC = LSEC + LADJ
      IF( LSEC.LT.60 ) GO TO 60
      LADJ = LSEC/60
      LSEC = LSEC - LADJ*60
      GO TO 100
C
   60 IF( LSEC.GE.0 ) GO TO 900
      LADJ = (LSEC-60)/60
      LSEC = LSEC - LADJ*60
      IF( LSEC.NE.60 ) GO TO 100
      LSEC = 0
      LADJ = LADJ + 1
C
C.... ADJUSTING MINUTES
C
  100 LMIN = LMIN + LADJ
      IF( LMIN.LT.60 ) GO TO 110
      LADJ = LMIN/60
      LMIN = LMIN - LADJ*60
      GO TO 200
C
  110 IF( LMIN.GE.0 ) GO TO 900
      LADJ = (LMIN-60)/60
      LMIN = LMIN - LADJ*60
      IF( LMIN.NE.60 ) GO TO 200
      LMIN = 0
      LADJ = LADJ + 1
C
C.... ADJUSTING HOURS
C
  200 LHOUR = LHOUR + LADJ
      IF( LHOUR.LT.24 ) GO TO 210
      LADJ = LHOUR/24
      LHOUR = LHOUR - LADJ*24
      GO TO 300
C
  210 IF( LHOUR.GE.0 ) GO TO 900
      LADJ = (LHOUR-24)/24
      LHOUR = LHOUR - LADJ*24
      IF( LHOUR.NE.24 ) GO TO 300
      LHOUR = 0
      LADJ = LADJ + 1
C
C.... ADJUSTING DAYS
C
C
C     CALCULATE IF THE CURRENT YEAR IS A LEAP YEAR
C
  300 CALL SHLEAP(LYEAR,LEAP)
C
C    CALCULATE THE ORDINAL DAY OF THIS YEAR
C
      IA = 30*(LMON+2) + (55*(LMON+2))/100 - 2*((LMON+10)/13)
     *     - 91 + (LEAP*(LMON+10))/13 + LDAY
C
C     ADJUST IT
C
      IA = IA + LADJ
      IF( IA.LE.(365+LEAP) ) GO TO 310
      LADJ = 1
      IA = IA - 365 - LEAP
      LEAP = 1 - LEAP
      CALL SHCAL(IA,LEAP,LMON,LDAY)
      GO TO 500
C
  310 IF( IA.LE.0 ) GO TO 320
      CALL SHCAL(IA,LEAP,LMON,LDAY)
      GO TO 900
C
  320 LADJ = -1
      LEAP = 1 - LEAP
      IA = IA + 365 + LEAP
      CALL SHCAL(IA,LEAP,LMON,LDAY)
      GO TO 500
C
C.... ADJUSTING MONTHS
C
  400 LMON = LMON + LADJ
      IF( LMON.LT.13 ) GO TO 410
      LADJ = LMON/12
      LMON = LMON - LADJ*12
      IF( LMON.NE.0 ) GO TO 500
      LMON = 12
      LADJ = LADJ - 1
      GO TO 500
C
  410 IF( LMON.GT.0 ) GO TO 900
      LADJ = (LMON-12)/12
      LMON = LMON - LADJ*12
C
C.... ADJUSTING THE YEAR
C
  500 LYEAR = LYEAR + LADJ
      GO TO 900
C
C.... ADJUSTING MONTHS; END OF MONTH
C        IS THIS ACTUALLY THE END OF A MONTH?
  600 LEAP = 0
      IF( LMON.NE.2 ) GO TO 610
      CALL SHLEAP(LYEAR,LEAP)
  610 IA = IDAY(LMON) + LEAP
      IF( IA.NE.LDAY ) GO TO 900
C
C     YES IT IS - ADJUST IT
C
      LMON = LMON + LADJ
      IF( LMON.LT.13 ) GO TO 620
      LADJ = LMON/12
      LMON = LMON - LADJ * 12
      IF( LMON.NE.0 ) GO TO 630
      LMON = 12
      LADJ = LADJ - 1
      GO TO 630
C
  620 IF( LMON.GT.0 ) GO TO 640
      LADJ = (LMON-12)/12
      LMON = LMON - LADJ*12
C
C     DO THE YEAR
C
  630 LYEAR = LYEAR + LADJ
C
C     NOW GET THE CORRECT DAY
C
  640 LEAP = 0
      IF( LMON.NE.2 ) GO TO 650
      CALL SHLEAP(LYEAR,LEAP)
  650 LDAY = IDAY(LMON) + LEAP
C
C.... RETURN
C
  900 RETURN
C
      END
      SUBROUTINE SHLEAP_MPEUTIL(LYEAR,LEAP)
C
C---------------------------------------------------------------C
C   VERSION 1.0 JULY 1982    GEOFFREY M BONNIN  MRFC            C
C---------------------------------------------------------------C
C
C      Returns LEAP = 1 if LYEAR is a leap year. LEAP = 0 otherwise.
C      LYEAR is the 4 digit year.
C
      LEAP = LYEAR
      IA = (LEAP - (LEAP/4  )*4   + 3  )/4
      IB = (LEAP - (LEAP/100)*100 + 99 )/100
      IC = (LEAP - (LEAP/400)*400 + 399)/400
      LEAP = 1 - IA + IB - IC
C
      RETURN
      END
      SUBROUTINE SHCAL_MPEUTIL(IORD,LEAP,LMON,LDAY)
C
C---------------------------------------------------------------C
C   VERSION 1.0 JULY 1982    GEOFFREY M BONNIN  MRFC            C
C           1.1 FEB 1984  TABLELESS DATE CONVERSION             C
C---------------------------------------------------------------C
C
C
C     Calculate the calender month and day given the ordinal
C     day in IORD. LEAP is 1 if a leap year, 0 otherwise.
C
      IA = IORD + ( (305+IORD-LEAP)/365 ) * (2-LEAP)
      LMON = ( (IA+91)*20 )/611 - 2
      LDAY = IA + 30 - 30*LMON - (56*LMON)/100
C
      RETURN
      END
