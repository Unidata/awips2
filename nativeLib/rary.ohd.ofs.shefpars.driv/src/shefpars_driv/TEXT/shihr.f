C  =====================================================================
C  pgm: SHIHR .. Increment calendar date by the given num of hours
C
C  use:     CALL SHIHR(JDA,JHR,INC)
C
C  i/o: JDA ...... day number (1-31 plus possible increment) - INT
C  i/o: JHR ...... hour number (0-23) - INT
C   in: INC ...... number of hours to increment time - INT
C
C  cmt: The following sequence must be run after this routine:
C  cmt:       CALL SHGJUL(JUL,LYR,LMO,JDA)
C  cmt:       CALL SHIJUL(JUL,LYR,INC)
C  cmt:       CALL SHGCAL(JUL,LYR,LMO,JDA)
C  cmt:  (where "INC" is zero or any further increment values)
C  =====================================================================
      SUBROUTINE SHIHR(JDA,JHR,INC)

      INTEGER        JDA,JHR,INC,JDAT,JHRT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shihr.f,v $
     . $',                                                             '
     .$Id: shihr.f,v 1.2 1997/12/31 20:30:17 page Exp $
     . $' /
C    ===================================================================
C

        JHRT = JHR
        JDAT = INC/24
        JHRT = JHRT + (INC-(24*JDAT))

        IF (JHRT .LT. 0) THEN
            JHRT = JHRT+24
            JDAT = JDAT-1
          ELSEIF (JHRT .GT. 23) THEN
            JHRT = JHRT-24
            JDAT = JDAT+1
        ENDIF

        JHR = JHRT
        JDA = JDAT+JDA

      RETURN
      END
