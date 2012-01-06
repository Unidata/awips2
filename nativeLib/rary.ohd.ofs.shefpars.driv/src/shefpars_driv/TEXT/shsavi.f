C  =====================================================================
C  pgm: SHSAVI .. Buffer and save/restore packed duration code
C
C  use:      CALL SHSAVI(CMD,ITEM,IDCO)
C
C   in: CMD ....... routine command to save or retrieve code - CHAR*1
C   in:               'I' ... Initialize buffer packed dur code to 5000
C   in:               'P' ... Put packed duration code into the buffer
C   in:               'G' ... Get packed duration code from the buffer
C   in:               'S' ... Save buffer packed duration code in array
C   in:               'R' ... Restore buffer packd dur code from array
C   in: ITEM ...... field number of code to save or retrieve - INT
C  i/o: IDCO ...... packed duration code - INT
C
C  cmt: Hardwired for 200 fields (or items).
C  =====================================================================
      SUBROUTINE SHSAVI(CMD,ITEM,IDCO)

      CHARACTER*1   CMD
      INTEGER       ITEM,IDCO,SVIDCO,SVARRI(200),II

      SAVE          SVIDCO,SVARRI
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shsavi.f,v $
     . $',                                                             '
     .$Id: shsavi.f,v 1.3 1997/12/31 20:35:58 page Exp $
     . $' /
C    ===================================================================
C

      DATA          SVIDCO / 5000 /

        IF (CMD .EQ. 'I') THEN
            SVIDCO = 5000

        ELSEIF (CMD .EQ. 'P') THEN
            SVIDCO = IDCO

        ELSEIF (CMD .EQ. 'G') THEN
            IDCO = SVIDCO

        ELSEIF (CMD .EQ. 'S') THEN
             II = ITEM
            SVARRI(II) = SVIDCO

        ELSEIF (CMD .EQ. 'R') THEN
             II = ITEM
            SVIDCO = SVARRI(II)

        ENDIF

      RETURN
      END
