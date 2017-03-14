C  =====================================================================
C  pgm: SHGCAL .. Get frm jul-dt, cal-dt (get mo, da)
C
C  use:     CALL SHGCAL(JUL,LYR,LMO,LDA)
C
C   in: JUL ...... day of year (001-366) - INT
C   in: LYR ...... year number (1753-2199) - INT
C  out: LMO ...... month number (01-12) - INT
C  out:            (note, if output is 13, then JUL was too large)
C  out: LDA ...... day number (01-31) - INT
C  =====================================================================
      SUBROUTINE SHGCAL(JUL,LYR,LMO,LDA)

      INTEGER    JUL,LYR,LMO,LDA,II,JULT,LYRT,LMOT,LDAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shgcal.f,v $
     . $',                                                             '
     .$Id: shgcal.f,v 1.3 1998/04/07 19:12:44 page Exp $
     . $' /
C    ===================================================================
C

        JULT = JUL
        LYRT = LYR

        II = 1+(LYRT+3)/4-LYRT/4
        IF( LYRT .EQ. 1900 ) II = II+1
        IF( LYRT .EQ. 1800 ) II = II+1
        IF( LYRT .EQ. 2100 ) II = II+1
        IF( JULT .GT. (61-II) ) JULT = JULT+II
        LMOT = (JULT*67+2012)/2048
        LDAT = JULT-(LMOT*489)/16+30

        LMO = LMOT
        LDA = LDAT

      RETURN
      END
