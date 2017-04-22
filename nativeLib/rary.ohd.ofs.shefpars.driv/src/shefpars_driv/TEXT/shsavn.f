C  =====================================================================
C  pgm: SHSAVN .. Buffer and save/restore 7-am data flag
C
C  use:      CALL SHSAVN(CMD,ITEM,NFLAG)
C
C   in: CMD ....... routine command to save or retrieve code - CHAR*1
C   in:               'I' ... Initialize buffer 7-am data flag to 0
C   in:               'P' ... Put 7-am data flag into the buffer
C   in:               'G' ... Get 7-am data flag from the buffer
C   in:               'S' ... Save buffer 7-am data flag in array
C   in:               'R' ... Restore buffer 7-am data flag from array
C   in: ITEM ...... field number of code to save or retrieve - INT
C  i/o: NFLAG ..... 7-am data flag (0 = off, 1 = on) - INT
C
C  cmt: Hardwired for 200 fields (or items).
C  =====================================================================
      SUBROUTINE SHSAVN(CMD,ITEM,NFLAG)

      CHARACTER*1   CMD
      INTEGER       ITEM,NFLAG,SVNFLA,SVARRN(200),II

      SAVE          SVNFLA,SVARRN
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shsavn.f,v $
     . $',                                                             '
     .$Id: shsavn.f,v 1.2 1997/12/31 20:39:45 page Exp $
     . $' /
C    ===================================================================
C

      DATA          SVNFLA / 0 /

        IF (CMD .EQ. 'I') THEN
            SVNFLA = 0

        ELSEIF (CMD .EQ. 'P') THEN
            SVNFLA = NFLAG

        ELSEIF (CMD .EQ. 'G') THEN
            NFLAG = SVNFLA

        ELSEIF (CMD .EQ. 'S') THEN
             II = ITEM
            SVARRN(II) = SVNFLA

        ELSEIF (CMD .EQ. 'R') THEN
             II = ITEM
            SVNFLA = SVARRN(II)

        ENDIF

      RETURN
      END
