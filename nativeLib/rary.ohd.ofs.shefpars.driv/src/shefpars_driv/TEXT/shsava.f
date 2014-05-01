C  =====================================================================
C  pgm: SHSAVA .. Buffer and save/restore minute adjustment
C
C  use:      CALL SHSAVA(CMD,ITEM,LADJ)
C
C   in: CMD ....... routine command to save or retrieve code - CHAR*1
C   in:               'I' ... Initialize buffer minute adjustment to 0
C   in:               'P' ... Put minute adjustment into the buffer
C   in:               'G' ... Get minute adjustment from the buffer
C   in:               'S' ... Save buffer minute adjustment in array
C   in:               'R' ... Restore buffer minute adjustmt from array
C   in: ITEM ...... field number of code to save or retrieve - INT
C  i/o: LADJ ...... minute adjustment - INT
C
C  cmt: Hardwired for 200 fields (or items).
C  =====================================================================
      SUBROUTINE SHSAVA(CMD,ITEM,LADJ)

      CHARACTER*1   CMD
      INTEGER       ITEM,LADJ,SVLADJ,SVARRA(200),II

      SAVE          SVLADJ,SVARRA
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shsava.f,v $
     . $',                                                             '
     .$Id: shsava.f,v 1.2 1997/12/31 20:35:23 page Exp $
     . $' /
C    ===================================================================
C

      DATA          SVLADJ / 0 /

        IF (CMD .EQ. 'I') THEN
            SVLADJ = 0

        ELSEIF (CMD .EQ. 'P') THEN
            SVLADJ = LADJ

        ELSEIF (CMD .EQ. 'G') THEN
            LADJ = SVLADJ

        ELSEIF (CMD .EQ. 'S') THEN
             II = ITEM
            SVARRA(II) = SVLADJ

        ELSEIF (CMD .EQ. 'R') THEN
             II = ITEM
            SVLADJ = SVARRA(II)

        ENDIF

      RETURN
      END
