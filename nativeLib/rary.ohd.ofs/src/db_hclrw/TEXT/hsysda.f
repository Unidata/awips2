C MODULE HSYSDA
C-----------------------------------------------------------------------
C
C  ROUTINE TO GET THE CURRENT HYDRLOGIC DAY USING THE SYSTEM CLOCK.
C
C  THE HYDROLOGIC DAY CAN BE DIFFERENT FROM THE CURRENT DAY BECAUSE
C  THE COMPUTER CLOCK INCREMENTS THE CURRENT DAY AT MIDNIGHT BUT THE
C  HYDROLOGIC DAY INCREMENTS AT 12Z. EACH USER CAN SET THE
C  Z-TIME AT WHICH THE CALENDER DATE CHANGES FROM ONE DATE TO
C  THE NEXT.
C
C  ARGUMENT LIST:
C
C     NAME    TYPE   I/O   DIM   DESCRIPTION
C     ----    ----   ---   ---   -----------
C     JULDAT   I      O     1    JULIAN DATE
C
      SUBROUTINE HSYSDA (JULDAT)
C
      CHARACTER*8 RTNNAM,RTNOLD
C
      INTEGER   IDATE(8)
C
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hdflts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hsysda.f,v $
     . $',                                                             '
     .$Id: hsysda.f,v 1.4 2001/06/13 12:08:00 mgm Exp $
     . $' /
C    ===================================================================
C
C   
      RTNNAM='HSYSDA'
C      
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'ENTER ',RTNNAM

C  GET CURRENT TIME IN INTERNAL UNITS
      CALL HGETTM (IDATE,'INTL')

      IF (IHCLDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' IDATE(1)=',IDATE(1),
     *      ' IDATE(2)=',IDATE(2),
     *      ' IDATE(3)=',IDATE(3),
     *      ' IDATE(4)=',IDATE(4),
     *      ' IDATE(5)=',IDATE(5),
     *      ' IDATE(6)=',IDATE(6),
     *      ' IDATE(7)=',IDATE(7),
     *      ' '
         WRITE (IOGDB,'(A,A4)') ' IDATE(8)=',IDATE(8)
         ENDIF

      JULDAT=IDATE(1)/24+1
      INTHR=MOD(IDATE(1),24)

      IF (IHCLDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' JULDAT=',JULDAT,
     *      ' INTHR=',INTHR,
     *      ' '
         ENDIF

C  CONVERT ZOFF VALUE FROM Z-TIME TO INTERNAL TIME
      ICKHR=(ZOFF/100)+LOCAL-NLSTZ
      ICKMIN=MOD(IABS(ZOFF),100)

      IF (IHCLDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' ZOFF=',ZOFF,
     *      ' LOCAL=',LOCAL,
     *      ' NLSTZ=',NLSTZ,
     *      ' ICKHR=',ICKHR,
     *      ' ICKMIN=',ICKMIN,
     *      ' '
         ENDIF

      IF (ZOFF.GE.0.OR.ICKMIN.EQ.0) GO TO 10

C  CONVERT FROM NUMBER OF MINUTES BEFORE THE Z-HOUR TO NUMBER OF
C  AFTER THE HOUR
      ICKMIN=60-ICKMIN
      ICKHR=ICKHR-1


10    INTHM=INTHR*100+IDATE(6)
      ICKHM=ICKHR*100+ICKMIN

C  CHECK IF PASSED THE Z-TIME AT WHICH THE CALENDER DATE SHOULD
C  CHANGE TO NEXT DAY
      IF (INTHM.GE.ICKHM) JULDAT=JULDAT+1

C  CHECK IF THE Z-TIME OFFSET BEYOND 12 HOURS (RESULTING IN THE INTERVAL
C  TIME TO SWITCH OVER IN THE RANGE 0 TO 2), NORMALIZE THE OFFSET
C  CHECK HOUR TO THE RANGE 0 TO 24, CHECK AGAIN, AND IF THE SYSTEM CLOCK
C  HAS NOT PASSED THE OFFSET TIME, BRING THE JULIAN DAY BACK ONE AS IT
C  HAS ALREADY BEEN INCREMENTED
      IF (ICKHM.GE.2400.AND.(ICKHM-2400).GT.INTHM) JULDAT=JULDAT-1

      IF (IHCLDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' INTHM=',INTHM,
     *      ' ICKHM=',ICKHM,
     *      ' JULDAT=',JULDAT,
     *      ' '
         ENDIF

C  CONVERT JULIAN DAY TO MONTH, DAY AND YEAR
      JNTHR=1
      CALL UMEMOV(TIME(3), ITZC, 1)      
      CALL MDYH2 (JULDAT,JNTHR,NMON,NDAY,NYR,NHR,JTZ,JDSAV,ITZC)

C  FILL TDATES ARRAY
      TDATES(1)=0
      TDATES(2)=NMON
      TDATES(3)=NDAY
CY2      TDATES(4)=MOD(NYR,100)
      TDATES(4)=NYR
      TDATES(5)=0
      TDATES(6)=IBLNK
      TDATES(7)=0

      IF (IHCLDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' TDATES(1)=',TDATES(1),
     *      ' TDATES(2)=',TDATES(2),
     *      ' TDATES(3)=',TDATES(3),
     *      ' TDATES(4)=',TDATES(4),
     *      ' TDATES(5)=',TDATES(5),
     *      ' TDATES(7)=',TDATES(7),
     *      ' '
         WRITE (IOGDB,'(A,A4)') ' TDATES(6)=',TDATES(6)
         ENDIF

      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'EXIT ',RTNNAM

      RETURN
 
      END
