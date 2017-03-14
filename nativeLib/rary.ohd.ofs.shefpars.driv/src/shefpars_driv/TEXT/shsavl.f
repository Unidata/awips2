C  =====================================================================
C  pgm: SHSAVL .. Buffer and save/restore observation date (L-date)
C
C  use:      CALL SHSAVL(CMD,ITEM,LYR,LMO,LDA,LHR,LMN,LSE)
C
C   in: CMD ....... routine command to save or retrieve dates - CHAR*1
C   in:               'I' ... Initialize buffer L-date to 0
C   in:               'P' ... Put (y,m,d,h,n,s) into the buffer L-date
C   in:               'G' ... Get (y,m,d,h,n,s) from the buffer L-date
C   in:               'S' ... Save buffer L-date in an array
C   in:               'R' ... Restore buffer L-date from array
C   in: ITEM ...... field number of dates to save or retrieve - INT
C  i/o: LYR ....... year number (1753-2199) - INT
C  i/o: LMO ....... month number (1-12) - INT
C  i/o: LDA ....... day number (1-31) - INT
C  i/o: LHR ....... hour number (0-24) - INT
C  i/o: LMN ....... minute number (0-59) - INT
C  i/o: LSE ....... second number (0-59) - INT
C
C  cmt: Hardwired for 200 fields (or items).
C  =====================================================================
      SUBROUTINE SHSAVL(CMD,ITEM,LYR,LMO,LDA,LHR,LMN,LSE)

      CHARACTER*1   CMD
      INTEGER       ITEM,LYR,LMO,LDA,LHR,LMN,LSE,II
      INTEGER       SVLYR,SVLMO,SVLDA,SVLHR,SVLMN,SVLSE,SVARRL(1200)

      SAVE          SVLYR,SVLMO,SVLDA,SVLHR,SVLMN,SVLSE,SVARRL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shsavl.f,v $
     . $',                                                             '
     .$Id: shsavl.f,v 1.3 1998/04/07 19:15:53 page Exp $
     . $' /
C    ===================================================================
C

      DATA     SVLYR,SVLMO,SVLDA,SVLHR,SVLMN,SVLSE / 0,0,0,0,0,0 /

        IF (CMD .EQ. 'I') THEN
            SVLYR = 0
            SVLMO = 0
            SVLDA = 0
            SVLHR = 0
            SVLMN = 0
            SVLSE = 0
 
        ELSEIF (CMD .EQ. 'P') THEN
            SVLYR = LYR
            SVLMO = LMO
            SVLDA = LDA
            SVLHR = LHR
            SVLMN = LMN
            SVLSE = LSE
 
        ELSEIF (CMD .EQ. 'G') THEN
            LYR = SVLYR
            LMO = SVLMO
            LDA = SVLDA
            LHR = SVLHR
            LMN = SVLMN
            LSE = SVLSE

        ELSEIF (CMD .EQ. 'S') THEN
             II = (ITEM-1)*6
            SVARRL(II+1) = SVLYR
            SVARRL(II+2) = SVLMO
            SVARRL(II+3) = SVLDA
            SVARRL(II+4) = SVLHR
            SVARRL(II+5) = SVLMN
            SVARRL(II+6) = SVLSE

        ELSEIF (CMD .EQ. 'R') THEN
             II = (ITEM-1)*6
            SVLYR = SVARRL(II+1)
            SVLMO = SVARRL(II+2)
            SVLDA = SVARRL(II+3)
            SVLHR = SVARRL(II+4)
            SVLMN = SVARRL(II+5)
            SVLSE = SVARRL(II+6)

        ENDIF

      RETURN
      END
