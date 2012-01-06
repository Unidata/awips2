C$PRAGMA C (UDATL)
C MEMBER UDATC1
C-----------------------------------------------------------------------
C  ROUTINE TO RETURN CURRENT DATE AND TIME IN CHARACTER FORM.

      SUBROUTINE UDATC1 (MONTH,DAY,YEAR,TIME)

      CHARACTER*2   MONTH,DAY
      CHARACTER*4   YEAR,TIME
      INTEGER       IDATE1(6)

      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_upage/RCS/udatc1.f,v $
     . $',                                                             '
     .$Id: udatc1.f,v 1.3 2002/02/11 16:33:52 dws Exp $
     . $' /
C    ===================================================================
C

        IF (ICMTRC.GT.0) THEN
           CALL ULINE2 (ICMPRU,1)
           WRITE (ICMPRU,'('' *** ENTER UDATC1'')')
        ENDIF

C          Get current local date and time (4-digit year)

        CALL UDATL (IDATE1)

C          Convert month, day, year, time to characters

        WRITE(MONTH,'(I2.2)') IDATE1(3)
        WRITE(DAY,  '(I2.2)') IDATE1(4)
        WRITE(YEAR, '(I4.4)') IDATE1(1)
        WRITE(TIME, '(I4.4)') IDATE1(5)

        IF (ICMDBG.GT.0) THEN
           CALL ULINE2 (ICMPRU,1)
           WRITE (ICMPRU,'('' MONTH='',A,''   DAY='',A,''   YEAR='',A,
     $                   ''   TIME='',A)') MONTH,DAY,YEAR,TIME
        ENDIF

        IF (ICMTRC.GT.0) THEN
           CALL ULINE2 (ICMPRU,1)
           WRITE (ICMPRU,'('' *** EXIT UDATC1'')')
        ENDIF

      RETURN
      END
