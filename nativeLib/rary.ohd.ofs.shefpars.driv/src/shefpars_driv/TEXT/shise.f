C  =====================================================================
C  pgm: SHISE .. Increment calendar date by the given num of seconds
C
C  use:     CALL SHISE(JMN,JSE,INC)
C
C  i/o: JMN ...... minute number (0-59 plus possible increment) - INT
C  i/o: JSE ...... second number (0-59) - INT
C   in: INC ...... number of seconds to increment time - INT
C
C  cmt: The following sequence must be run after this routine:
C  cmt:       CALL SHIMN(LHR,JMN,INC)
C  cmt:       CALL SHIHR(LDA,LHR,INC)
C  cmt:       CALL SHGJUL(JUL,LYR,LMO,LDA)
C  cmt:       CALL SHIJUL(JUL,LYR,INC)
C  cmt:       CALL SHGCAL(JUL,LYR,LMO,LDA)
C  cmt:  (where "INC" is zero or any further increment values)
C  =====================================================================
      SUBROUTINE SHISE(JMN,JSE,INC)

      INTEGER        JMN,JSE,INC,JMNT,JSET
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shise.f,v $
     . $',                                                             '
     .$Id: shise.f,v 1.2 1997/12/31 20:32:38 page Exp $
     . $' /
C    ===================================================================
C

        JSET = JSE
        JMNT = INC/60
        JSET = JSET + (INC-(60*JMNT))

        IF (JSET .LT. 0) THEN
            JSET = JSET+60
            JMNT = JMNT-1
          ELSEIF (JSET .GE. 60) THEN
            JSET = JSET-60
            JMNT = JMNT+1
        ENDIF

        JSE = JSET
        JMN = JMNT+JMN

      RETURN
      END
