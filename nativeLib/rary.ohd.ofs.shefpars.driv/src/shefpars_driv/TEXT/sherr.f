C  =====================================================================
C  pgm: SHERR .. Output warning-error messages and counts, reset KHPOS
C
C  use:     CALL SHERR(CMD,NUM,KHPOS,KHAR)
C
C   in: CMD ..... control char for command to be performed - CHAR*1
C   in:             'I' ... Initialize number of warngs/errs to 0
C   in:             'S' ... output summary of number of errs and warns
C   in:             'W' ... output warning message, no change to KHPOS
C   in:             'E' ... output error message, set KHPOS=1 if higher
C   in:             'A' ... output error message, no change to KHPOS
C   in: NUM ..... number of warning or error message - INT
C  i/o: KHPOS ... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  i/o:           (set to 1 if greater than 1 for fatal line error)
C  out: KHAR .... current char is set to ':' if 'CMD' is 'E' - CHAR*1
C   in: (subrtn)  max num of errors allowed is entered by rtn SHMAXE
C
C  rqd: SHMAXE,SHERRM,SHLINE,SHERRS,SHERRK
C  =====================================================================
      SUBROUTINE SHERR(CMD,NUM,KHPOS,KHAR)

      EXTERNAL        SHMAXE,SHERRM,SHLINE,SHERRS,SHERRK

      INTEGER         INITZ,NOFER
      INTEGER         KHPOS,NUM,NWAR,NERR
      CHARACTER*1     CMD
      CHARACTER*1     KHAR

      SAVE    INITZ,NOFER
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sherr.f,v $
     . $',                                                             '
     .$Id: sherr.f,v 1.6 2000/03/14 14:23:45 page Exp $
     . $' /
C    ===================================================================
C

      DATA    INITZ,NOFER / 0, 500 /

        IF     (CMD.EQ.'I' .OR. INITZ.EQ.0) THEN
            INITZ = 1
            CALL SHERRK('I',NUM,NWAR,NERR)
            CALL SHMAXE('GET_VALUE   ',KHAR,KHPOS,NOFER)
        ENDIF

        IF     (CMD.EQ.'S') THEN
            CALL SHERRK('N',NUM,NWAR,NERR)
            CALL SHERRS('WRITE_SUMMARY   ',NWAR,NERR)
            CALL SHLINE('WRITE_NUM_LINES ',KHAR,KHPOS)

        ELSEIF (CMD.EQ.'W' .AND. KHPOS.NE.1) THEN
            CALL SHERRK('W',NUM,NWAR,NERR)
            CALL SHLINE('WRITE_WARNING   ',KHAR,KHPOS)
            CALL SHERRM('W',NUM)

        ELSEIF ((CMD.EQ.'E' .OR. CMD.EQ.'A') .AND. KHPOS.NE.1) THEN
            CALL SHERRK('E',NUM,NWAR,NERR)
            CALL SHLINE('WRITE_ERROR     ',KHAR,KHPOS)
            CALL SHERRM('E',NUM)

            IF (CMD.EQ.'E') THEN
              KHAR  = ':'
              KHPOS = 1
            ENDIF

            IF (NOFER.GT.0 .AND. NERR.GE. NOFER) THEN
              CALL SHERRM('M',69)
              KHPOS = -1
            ENDIF

        ENDIF

      RETURN
      END
