C  =====================================================================
C  pgm: SHHRFX .. Constrain the hour range to 0-23 as opposed to 0-24
C
C  use:     CALL SHHRFX(IYR,IMO,IDA,IHR)
C
C  i/o: IYR .......... zulu date year number - INT
C  i/o: IMO .......... zulu date month number - INT
C  i/o: IDA .......... zulu date day number - INT
C  i/o: IHR .......... zulu date hour number (0-23 for output) - INT
C
C  rqd: SHGJUL,SHIJUL,SHGCAL
C
C  cmt: The original SHEF Code allowed hours in the range 0-24.  This
C  cmt:  filter adjusts an output date that has an hour number of 24 to
C  cmt:  a date with an hour number of 0 by incrementing it by one day.
C  cmt:  It does this for output only, without altering the logic of the
C  cmt:  parsing code.  Note that there are conflicts in the comments 
C  cmt:  throughout the parsing code about whether the range is
C  cmt:  0-24 or 0-23.
C  =====================================================================
      SUBROUTINE SHHRFX(IYR,IMO,IDA,IHR)

      EXTERNAL           SHGJUL,SHIJUL,SHGCAL

      INTEGER            IYR,IMO,IDA,IHR,JUL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shhrfx.f,v $
     . $',                                                             '
     .$Id: shhrfx.f,v 1.2 2000/03/01 16:08:00 dws Exp $
     . $' /
C    ===================================================================
C

C        If the hour is 24, convert the yy-mm-dd to julda (SHGJUL), add
C         one to the julda (SHIJUL), and recover the yy-mm-dd (SHGCAL)

        IF (IHR .EQ. 24) THEN
          IHR = 0
          CALL SHGJUL(JUL,IYR,IMO,IDA)
          CALL SHIJUL(JUL,IYR,1)
          CALL SHGCAL(JUL,IYR,IMO,IDA)
        ENDIF

      RETURN
      END
