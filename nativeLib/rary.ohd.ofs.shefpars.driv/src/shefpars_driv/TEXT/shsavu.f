C  =====================================================================
C  pgm: SHSAVU .. Buffer for logical unit numbers
C
C  use:     CALL SHSAVU(CMD,LUN)
C
C   in: CMD ..... routine command to save or retrieve "LUN" - CHAR*12
C   in:             'G_SHEFIN' .... Retrieve shefin file logical unt num
C   in:             'G_SHEFOUT' ... Retrieve shefout file logcl unt num
C   in:             'G_SHEFPARM' .. Retrieve shefparm file logcl unt num
C   in:             'G_SHEFERROR' . Retrieve sheferror file log unt num
C   in:             'G_SHEFCOPY' .. Retrieve shefcopy file logcl unt num
C   in:             'P_SHEFIN' .... Save shefin file logical unt num
C   in:             'P_SHEFOUT' ... Save shefout file logical unt num
C   in:             'P_SHEFPARM' .. Save shefparm file logical unt num
C   in:             'P_SHEFERROR' . Save sheferror file logical unt num
C   in:             'P_SHEFCOPY' .. Save shefcopy file logical unt num
C  i/o: LUN ..... logical unit number for file of concern - INT
C
C  cmt: The "shefin", "shefout", and "shefparm" files must be opened on
C  cmt:  three different logical unit numbers.  The "sheferror" file
C  cmt:  should be opened on a fourth unit but can be set to -1 if no
C  cmt:  warnings or errors are desired.  If a copy of all the shef
C  cmt:  messages are desired, open a "shefcopy" file (usually this
C  cmt:  file is opened on the same unit used for the "sheferror" file),
C  cmt:  else it can be set to -1.
C  =====================================================================
      SUBROUTINE SHSAVU(CMD,LUN)

      CHARACTER*12  CMD
      INTEGER       LUN,SVLUNI,SVLUNO,SVLUNP,SVLUNE,SVLUNC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shsavu.f,v $
     . $',                                                             '
     .$Id: shsavu.f,v 1.2 1996/03/18 17:51:00 page Exp $
     . $' /
C    ===================================================================
C

      SAVE          SVLUNI,SVLUNO,SVLUNP,SVLUNE,SVLUNC

      DATA    SVLUNI,SVLUNO,SVLUNP,SVLUNE,SVLUNC / -1,-1,-1,-1,-1 /

        IF (CMD .EQ. 'G_SHEFIN    ') THEN
          LUN = SVLUNI
        ELSEIF (CMD .EQ. 'G_SHEFOUT   ') THEN
          LUN = SVLUNO
        ELSEIF (CMD .EQ. 'G_SHEFPARM  ') THEN
          LUN = SVLUNP
        ELSEIF (CMD .EQ. 'G_SHEFERROR ') THEN
          LUN = SVLUNE
        ELSEIF (CMD .EQ. 'G_SHEFCOPY  ') THEN
          LUN = SVLUNC

        ELSEIF (CMD .EQ. 'P_SHEFIN    ') THEN
          SVLUNI = LUN
          IF (SVLUNI.LT.0 .OR. SVLUNI.GT.99) SVLUNI = -1
        ELSEIF (CMD .EQ. 'P_SHEFOUT   ') THEN
          SVLUNO = LUN
          IF (SVLUNO.LT.0 .OR. SVLUNO.GT.99) SVLUNO = -1
        ELSEIF (CMD .EQ. 'P_SHEFPARM  ') THEN
          SVLUNP = LUN
          IF (SVLUNP.LT.0 .OR. SVLUNP.GT.99) SVLUNP = -1
        ELSEIF (CMD .EQ. 'P_SHEFERROR ') THEN
          SVLUNE = LUN
          IF (SVLUNE.LT.0 .OR. SVLUNE.GT.99) SVLUNE = -1
        ELSEIF (CMD .EQ. 'P_SHEFCOPY  ') THEN
          SVLUNC = LUN
          IF (SVLUNC.LT.0 .OR. SVLUNC.GT.99) SVLUNC = -1
        ENDIF

      RETURN
      END
