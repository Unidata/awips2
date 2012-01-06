C  =====================================================================
C  pgm: SHERRK .. Record number of warnings and errors and last values
C
C  use:     CALL SHERRK(CMD,NUM,NWAR,NERR)
C
C   in: CMD ..... control character for command to be performed - CHAR*1
C   in:             'I' ... initialize all four data values to zero - no
C   in:                     of warngs, no of errs, last warng, last err
C   in:             'W' ... increment the number of warnings by one and
C   in:                     get the new number of warnings and errors
C   in:                     (also set last warning to "NUM")
C   in:             'E' ... increment the number of errors by one and
C   in:                     get the new number of warnings and errors
C   in:                     (also set last error to "NUM")
C   in:             'N' ... Get the number of warnings and errors in
C   in:                     arguments "NWAR" and "NERR"
C   in:             'L' ... get the last warning and error numbers in
C   in:                     arguments "NWAR" and "NERR"
C   in: NUM ..... warng or err number entered for commands 'W','E" - INT
C  out: NWAR .... total number of warnings for commands 'N','W','E';
C  out:           last warning number for command 'L'
C  out: NERR .... total number of errors for commands 'N','W','E';
C  out:           last error number for command 'L'
C  =====================================================================
      SUBROUTINE SHERRK(CMD,NUM,NWAR,NERR)

      INTEGER         NUM,NWAR,NERR,SVNWAR,SVNERR,SVLWAR,SVLERR
      CHARACTER*1     CMD

      SAVE    SVNWAR,SVNERR,SVLWAR,SVLERR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sherrk.f,v $
     . $',                                                             '
     .$Id: sherrk.f,v 1.3 1996/07/11 19:57:27 dws Exp $
     . $' /
C    ===================================================================
C

      DATA    SVNWAR,SVNERR,SVLWAR,SVLERR / 0, 0, 0, 0 /

        IF (CMD .EQ. 'W') THEN

            SVNWAR = SVNWAR+1
            SVLWAR = NUM
            NWAR   = SVNWAR
            NERR   = SVNERR

        ELSEIF (CMD .EQ. 'E') THEN

            SVNERR = SVNERR+1
            SVLERR = NUM
            NWAR   = SVNWAR
            NERR   = SVNERR

        ELSEIF (CMD .EQ. 'I') THEN

            SVNWAR = 0
            SVNERR = 0
            SVLWAR = 0
            SVLERR = 0

        ELSEIF (CMD .EQ. 'N') THEN

            NWAR = SVNWAR
            NERR = SVNERR

        ELSEIF (CMD .EQ. 'L') THEN

            NWAR = SVLWAR
            NERR = SVLERR

        ENDIF

      RETURN
      END
