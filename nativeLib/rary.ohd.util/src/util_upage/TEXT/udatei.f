C$PRAGMA C (UDATL)
C MEMBER UDATEI
C-----------------------------------------------------------------------
C  ROUTINE  UDATEI  RETURNS THE CURRENT MONTH, DAY, YEAR, MINUTES,
C  AND SECONDS; AND THE JULIAN DATE.
C
C  ARGUMENT LIST:
C
C     ARGUMENT   TYPE   I/O   DIM   CONTENTS
C     --------   ----   ---   ---   --------
C     IMON       I*4    O     1     MONTH
C     IDAY       I*4    O     1     DAY
C     IYEAR      I*4    O     1     YEAR
C     IHRMIN     I*4    O     1     HOURS AND MINUTES (MILITARY TIME)
C     ISEC       I*4    O     1     SECONDS
C     JULDAT     I*4    O     1     JULIAN DATE
C     ISTAT      I*4    O     1     STATUS CODE (ALWAYS 0)
C-----------------------------------------------------------------------
      SUBROUTINE UDATEI (IMON,IDAY,IYEAR,IHRMIN,ISEC,JULDAT,ISTAT)

      INTEGER   IDATE(6)

      INCLUDE 'ucmdbx'
      INCLUDE 'upagex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_upage/RCS/udatei.f,v $
     . $',                                                             '
     .$Id: udatei.f,v 1.3 2002/02/11 16:34:16 dws Exp $
     . $' /
C    ===================================================================
C


        IF (ICMTRC.GT.0) THEN
          NPSNLN(ICMPRU)=NPSNLN(ICMPRU)+1
          WRITE (ICMPRU,'('' *** ENTER UDATEI'')')
        ENDIF

        ISTAT=0

        CALL UDATL (IDATE)

        JULDAT=IDATE(2)
        IMON=IDATE(3)
        IDAY=IDATE(4)
        IYEAR=IDATE(1)
        IHRMIN=IDATE(5)
        ISEC=IDATE(6)

        IF (ICMDBG.GT.0) THEN
          NPSNLN(ICMPRU)=NPSNLN(ICMPRU)+1
          WRITE (ICMPRU,'('' IMON='',I2,''   IDAY='',I2,''   IYEAR='',
     $      I4,''   IHRMIN='',I4,''    ISEC='',I4,''   JULDAT='',I4)')
     $      IMON,IDAY,IYEAR,IHRMIN,ISEC,JULDAT
        ENDIF

        IF (ICMTRC.GT.0) THEN
          NPSNLN(ICMPRU)=NPSNLN(ICMPRU)+1
          WRITE (ICMPRU,'('' *** EXIT UDATEI'')')
        ENDIF

      RETURN
      END
