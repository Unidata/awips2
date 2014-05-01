C  =====================================================================
C  pgm: SHIMN .. Increment calendar date by the given num of minutes
C
C  use:     CALL SHIMN(JHR,JMN,INC)
C
C  i/o: JHR ...... hour number (1-24 plus possible increment) - INT
C  i/o: JMN ...... minute number (0-59) - INT
C   in: INC ...... number of minutes to increment time - INT
C
C  cmt: The following sequence must be run after this routine:
C  cmt:       CALL SHIHR(LDA,JHR,INC)
C  cmt:       CALL SHGJUL(JUL,LYR,LMO,LDA)
C  cmt:       CALL SHIJUL(JUL,LYR,INC)
C  cmt:       CALL SHGCAL(JUL,LYR,LMO,LDA)
C  cmt:  (where "INC" is zero or any further increment values)
C  =====================================================================
      SUBROUTINE SHIMN(JHR,JMN,INC)

      INTEGER        JHR,JMN,INC,JHRT,JMNT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shimn.f,v $
     . $',                                                             '
     .$Id: shimn.f,v 1.2 1997/12/31 20:30:42 page Exp $
     . $' /
C    ===================================================================
C

        JMNT = JMN
        JHRT = INC/60
        JMNT = JMNT + (INC-(60*JHRT))

        IF (JMNT .LT. 0) THEN
            JMNT = JMNT+60
            JHRT = JHRT-1
          ELSEIF (JMNT .GE. 60) THEN
            JMNT = JMNT-60
            JHRT = JHRT+1
        ENDIF

        JMN = JMNT
        JHR = JHRT+JHR

      RETURN
      END
