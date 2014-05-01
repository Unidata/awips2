C  =====================================================================
C  pgm: SHEDLN .. Edit new message line for oddities and to get "LEND"
C
C  use:     CALL SHEDLN(LBEG,LEND,LINE)
C
C  i/o: LBEG ........ location of first input char in "LINE" - INT
C  out: LEND ........ location of last non-blank char in "LINE" - INT
C   in: LINE ........ string containing an input shef message - CHAR*(*)
C  =====================================================================
      SUBROUTINE SHEDLN(LBEG,LEND,LINE)

      INTRINSIC      CHAR
      CHARACTER*1    CHAR
 
      INTRINSIC      LEN,ICHAR
      INTEGER        LEN,ICHAR

      CHARACTER*(*)  LINE
      CHARACTER*1    KNULL,LNFEED,CARRTN,TAB
      INTEGER        LBEG,LEND,MAXEND,INITZ,ICOUMX,ICOU,II,JJ
      INTEGER        JBEG,JEND,KEND

      SAVE           INITZ,MAXEND,KNULL,LNFEED,CARRTN,TAB
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shedln.f,v $
     . $',                                                             '
     .$Id: shedln.f,v 1.6 2004/06/24 15:21:31 dws Exp $
     . $' /
C    ===================================================================
C

      DATA           INITZ,ICOUMX / 0, 51 /

        IF (INITZ .EQ. 0) THEN
            INITZ  = 1
            KNULL  = CHAR(0)
            TAB    = CHAR(9)
            LNFEED = CHAR(10)
            CARRTN = CHAR(13)
            MAXEND = LEN(LINE)
        ENDIF

        JBEG = LBEG

C                   For Satellite Broadcast Network comms control code
C                    we want to bypass the first part of the line

        IF (LINE(JBEG:JBEG) .EQ. '@') THEN
          II = JBEG+1
          IF (II .LT. MAXEND) THEN
             JJ = ICHAR(LINE(II:II))
             JJ = JBEG + JJ + JJ
             IF (JJ.GT.JBEG .AND. JJ.LE.MAXEND) JBEG = JJ
          ENDIF
        ENDIF

C                   Get end of line with 15 char rule
C                   Set JEND to last usable char (not blank, tab, ctl-M)
C                   For HP quirk, may need to look for null that could
C                     exist after the insertion of "NNNN"

        ICOU = ICOUMX
        KEND = JBEG
        JEND = JBEG-1
  104   IF (KEND.GE.MAXEND .OR. LINE(KEND:KEND).EQ.KNULL .OR.
     $      ICOU.LE.0) GOTO 106
          IF (LINE(KEND:KEND).EQ.' ' .OR. LINE(KEND:KEND).EQ.TAB) THEN
            ICOU = ICOU-1
          ELSE
            ICOU = ICOUMX
            IF (LINE(KEND:KEND) .NE. CARRTN) JEND = KEND
          ENDIF
          KEND = KEND+1
          GOTO 104
  106   CONTINUE

C                   For some, they need to ignore a trailing & or =

        IF ( LINE(JBEG:JBEG) .NE. ':') THEN
  122     IF (LINE(JEND:JEND).NE.'=' .AND.
     $        LINE(JEND:JEND).NE.'&'       ) GOTO 124
            JEND = JEND-1
            GOTO 122
  124     CONTINUE
        ENDIF

C                   Another data quirk, skip linefeeds at line beginning

  130   IF (LINE(JBEG:JBEG) .NE. LNFEED) GOTO 140
          JBEG = JBEG+1
          GOTO 130
  140   CONTINUE

C                   Someday, could skip leading blank chars!!!

C 150   IF (LINE(JBEG:JBEG) .NE. ' ') GOTO 160
C         JBEG = JBEG+1
C         GOTO 150
C 160   CONTINUE

        LBEG = JBEG
        LEND = JEND

      RETURN
      END
