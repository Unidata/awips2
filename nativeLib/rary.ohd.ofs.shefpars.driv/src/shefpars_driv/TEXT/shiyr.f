C  =====================================================================
C  pgm: SHIYR .. Increment the calendar date by the given num of years
C
C  use:     CALL SHIYR(JYR,INC,ISTAT)
C
C  i/o: JYR ...... year number (1753-2199) - INT
C   in: INC ...... number of months (pos or neg) for increment - INT
C  out: ISTAT .... if 0 then no error, if 2 then bad year number - INT
C  =====================================================================
      SUBROUTINE SHIYR(JYR,INC,ISTAT)

      INTEGER    JYR,INC,ISTAT,JYRT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shiyr.f,v $
     . $',                                                             '
     .$Id: shiyr.f,v 1.3 1998/04/07 19:14:21 page Exp $
     . $' /
C    ===================================================================
C

        JYRT  = JYR
        ISTAT = 0

        IF (JYRT.GE.0 .AND. JYRT.LE.99) THEN
            JYRT = JYRT+INC
            JYRT = JYRT - (100*(JYRT/100))
          ELSE
            JYRT = JYRT+INC
            IF (JYRT.LT.1753 .OR. JYRT.GT.2199) ISTAT = 2
        ENDIF

        JYR = JYRT

      RETURN
      END
