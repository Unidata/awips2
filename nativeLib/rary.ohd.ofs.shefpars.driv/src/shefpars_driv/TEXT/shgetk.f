C  =====================================================================
C  pgm: SHGETK .. Get valid char, edit out comments, double blnks, tabs
C
C  use:     CALL SHGETK(KHAR,KHPOS)
C
C  out: KHAR ........ next char obtain, else "eol" or "eof" - CHAR*1
C  i/o: KHPOS ....... position of last character - INT
C  i/o:                 neg ... error - skip this rtn
C  i/o:                   0 ... end of file - skip this rtn
C  i/o:                   1 ... error eol - read line, set 4, get char
C  i/o:                                   - or set to 0 for eof
C  i/o:                   2 ... end of line - read line, set 4, get char
C  i/o:                                     - or set to 0 for eof
C  i/o:                   3 ... beg of buffer - inc, get first char
C  i/o:                4-82 ... last char pos - inc, get char
C  i/o:                  83 ... last char pos - set to 2 for eol
C  i/o:               (note, must initialize KHPOS = 2 for first pass)
C
C  rqd: SHLINE,SHERR
C  =====================================================================
      SUBROUTINE SHGETK(KHAR,KHPOS)

      EXTERNAL       SHLINE,SHERR
      INTRINSIC      CHAR,ICHAR

      INTEGER        KHPOS,KLOOP,NCOLON,NQUOTE,ICHAR,DIFF,INITZ
      CHARACTER*1    KHAR,LASTCH,CHAR,SMLA,SMLZ,CAPA,NQUOKH,TAB

      PARAMETER( SMLA='a', SMLZ='z', CAPA='A' )

      SAVE           LASTCH,DIFF,INITZ,NQUOTE,NQUOKH,TAB
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shgetk.f,v $
     . $',                                                             '
     .$Id: shgetk.f,v 1.5 2000/03/14 14:25:24 page Exp $
     . $' /
C    ===================================================================
C

      DATA           LASTCH,DIFF,INITZ,NQUOTE / 'X', 0, 0, 1 /
      DATA           NQUOKH / '"' /

C                   Initial values

        IF (INITZ .EQ. 0) THEN
          INITZ = 1
          DIFF  = ICHAR(SMLA)-ICHAR(CAPA)
          TAB   = CHAR(9)
        ENDIF

C                   Set flag for no ":", loop until valid char obtained

        NCOLON = 1
        KLOOP  = 0
   40   IF (KLOOP .NE. 0) GOTO 50

C                   If prev char is i/o err (KHPOS=neg), set to 'eof' to
C                    elim further err messeges; if 'eof' (KHPOS=0), skip
C                   Else if at 'eol' (KHPOS=1 or 2), get new input line
C                   Else if last char in line (KHPOS=LEND), set to 'eol'
C                   Else if still in line (KHPOS>2), get next char

          IF (KHPOS .LE. 0) THEN
              KHPOS = 0
              KHAR  = ':'
            ELSEIF (KHPOS .LE. 2) THEN
              CALL SHLINE('GET_NEW_LINE    ',KHAR,KHPOS)
              NQUOTE = 1
              IF (KHPOS .LT. 0) THEN
                CALL SHERR('E',4,KHPOS,KHAR)
                KHPOS = -1
              ENDIF
              IF (KHPOS.GT.2) THEN
                CALL SHLINE('WRITE_COPY      ',KHAR,KHPOS)
                CALL SHLINE('GET_NEXT_CHAR   ',KHAR,KHPOS)
              ENDIF
            ELSE
              CALL SHLINE('GET_NEXT_CHAR   ',KHAR,KHPOS)
          ENDIF

C                   If error, end-of-file, or end-of-line; end loop
C                   Else if have colon, reverse colon flag; cont loop
C                   Else if colon flag off and double blank; cont loop
C                    with conversion of letter chars to caps

          IF (KHPOS .LE. 2) THEN
              KLOOP =  1
CC          ELSEIF (KHAR .EQ. ':') THEN
CC                 Use above to skip comments in quotes
CC                 Use below to keep comments in quotes
            ELSEIF (KHAR.EQ.':' .AND. NQUOTE.EQ.1) THEN
              NCOLON = 1-NCOLON
            ELSEIF (NCOLON .EQ. 1) THEN
              IF (NQUOTE .EQ. 1) THEN
                  IF (KHAR.NE.' ' .OR. LASTCH.NE.' ' .OR.
     $                KHAR.NE.TAB .OR. LASTCH.NE.TAB      ) THEN
                      KLOOP = 1
                      IF (KHAR.GE.SMLA .AND. KHAR.LE.SMLZ) THEN
                          KHAR = CHAR( ICHAR(KHAR)-DIFF )
                        ELSEIF (KHAR.EQ.'"' .OR. KHAR.EQ.'''') THEN
                          NQUOTE = 1-NQUOTE
                          NQUOKH = KHAR
                        ELSEIF (KHAR.EQ.TAB) THEN
                          KHAR = ' '
                      ENDIF
                  ENDIF
                ELSE
                  KLOOP = 1
                  IF (KHAR .EQ. NQUOKH) NQUOTE = 1-NQUOTE
              ENDIF
          ENDIF

          GOTO 40
   50   CONTINUE

C                   Save char to check for multiple blanks

        LASTCH = KHAR

      RETURN
      END
