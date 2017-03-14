C  =====================================================================
C  pgm: MXC_NXWD .. Get the next word, allows for comma delim, uses dflt
C
C  use:     CALL MXC_NXWD(LINE,LAST,WORD,LWORD,KDEFLT)
C
C   in: LINE ...... character input string containing words - CHAR*(*)
C  i/o: LAST ...... last character to be used in LINE, must be
C  i/o:             initialized to zero for first pass, value is
C  i/o:             updated inside this routine, set to -1 when the
C  i/o:             end of the string is reached - INT
C  out: WORD ...... next word found, else set to KDEFLT - CHAR(*)
C  out: LWORD ..... number of characters in output word, set to
C  out:             zero for null word, set to -1 for no word - INT
C   in: KDEFLT .... default word for the output word if the input line
C   in:             does not have any more words - CHAR*(*)
C
C  cmt: There are 6 delimiter pairs that can be used in addition to
C  cmt:  blanks and commas:    '  '
C  cmt:                        "  "
C  cmt:                        (  )
C  cmt:                        [  ]
C  cmt:                        {  }
C  cmt:                        `  `
C  =====================================================================
      SUBROUTINE MXC_NXWD(LINE,LAST,WORD,LWORD,KDEFLT)

      INTRINSIC       LEN
      INTEGER         LEN

      CHARACTER*(*)   LINE,WORD,KDEFLT
      CHARACTER*1     KHAR,DELIM,DELIMB(6),DELIME(6)
      INTEGER         LAST,LWORD,II,JJ,LIMIT,MAXW,MAXL
      INTEGER         IHAVC,NXDELM,MXDELM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxc_nxwd.f,v $
     . $',                                                             '
     .$Id: mxc_nxwd.f,v 1.1 2001/06/13 18:33:22 dws Exp $
     . $' /
C    ===================================================================
C

      DATA    MXDELM / 6 /
      DATA    DELIMB / '''', '"', '(', '[', '{', '`' /
      DATA    DELIME / '''', '"', ')', ']', '}', '`' /

        WORD  = KDEFLT
        LWORD = -1

        IF (LAST .GE. 0) THEN
          DELIM = ' '
          MAXL  = LEN(LINE)
          MAXW  = LEN(WORD)

          IHAVC = 0
          LIMIT = MAXL
          IF (LAST .LE. 0) THEN
            IHAVC = 1
          ELSEIF (LAST .LE. LIMIT) THEN
            IF (LINE(LAST:LAST) .EQ. ',') IHAVC = 1
          ENDIF

          II = 0
  110     IF (LAST .GE. LIMIT) GOTO 140
            JJ   = LAST
            LAST = LAST+1
            KHAR = LINE(LAST:LAST)
            IF (DELIM .NE. ' ') THEN
              IF (KHAR .EQ. DELIM) THEN
                LIMIT = 0
              ELSEIF (LAST .GE. LIMIT) THEN
                JJ = LAST
                LIMIT = 0
              ENDIF
              IF (LIMIT .EQ. 0) THEN
                IF (II .LE. JJ) THEN
                  WORD = LINE(II:JJ)
                  LWORD = JJ-II+1
                  IF (LWORD .GT. MAXW) LWORD = MAXW
                ELSE
                  LWORD = 0
                ENDIF
              ENDIF
            ELSEIF (II .EQ. 0) THEN
              NXDELM = MXDELM
  120         IF (NXDELM .LE. 0) GOTO 130
                IF (KHAR .EQ. DELIMB(NXDELM)) THEN
                  DELIM = DELIME(NXDELM)
                  II = LAST+1
                  IHAVC = 0
                  NXDELM = 0
                ELSE
                  NXDELM = NXDELM-1
                ENDIF
                GOTO 120
  130         CONTINUE
              IF (DELIM.EQ.' ' .AND. KHAR.NE.' ') THEN
                IF (KHAR .EQ. ',') THEN
                  IF (IHAVC .EQ. 1) THEN
                    LIMIT = 0
                    IF (II.GT.0 .AND. II.LE.JJ) THEN
                      WORD = LINE(II:JJ)
                      LWORD = JJ-II+1
                      IF (LWORD .GT. MAXW) LWORD = MAXW
                    ELSE
                      LWORD = 0
                    ENDIF
                  ELSE
                    IHAVC = 1
                  ENDIF
                ELSE
                  IHAVC = 0
                  II = LAST
                  IF (LAST .GE. LIMIT) THEN
                    WORD = LINE(II:II)
                    LWORD = 1
                    LIMIT = 0
                  ENDIF
                ENDIF
              ENDIF
            ELSE
              IF (LAST .GE. LIMIT) THEN
                JJ = LAST
                LIMIT = 0
              ELSEIF (KHAR.EQ.' ' .OR. KHAR.EQ.',') THEN
                LIMIT = 0
              ENDIF
              IF (LIMIT .EQ. 0) THEN
                IF (II .LE. JJ) THEN
                  WORD = LINE(II:JJ)
                  LWORD = JJ-II+1
                  IF (LWORD .GT. MAXW) LWORD = MAXW
                ELSE
                  LWORD = 0
                ENDIF
              ENDIF
            ENDIF

            GOTO 110
  140     CONTINUE
          IF (LAST .GE. MAXL) LAST = -1
        ENDIF

      RETURN
      END
