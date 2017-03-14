C  =====================================================================
C  pgm: SH2ST1 .. Get next string (letters only) and right pad blank
C
C  use:     CALL SH2ST1(KHAR,KHPOS,NOFLET,KHLET)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: NOFLET .... number of non-blank chars found - INT
C  out:             (set to -1 if bad char is found)
C  out: KHLET ..... string of next letters found, blnk padded - CHAR*4
C  out:             (set to blank if bad char is found)
C  out:             (Note, search stops at end of string length)
C
C  rqd: SHGETK
C
C  cmt: Routine will NOT process if current char is "end-of-line".
C  cmt: "KHAR" must be guaranteed not blank if "KHPOS" is less than 2.
C  =====================================================================
      SUBROUTINE SH2ST1(KHAR,KHPOS,NOFLET,KHLET)

      EXTERNAL       SHGETK

      INTRINSIC      LEN
      INTEGER        LEN

      CHARACTER*4    KHLET
      CHARACTER*1    KHAR
      INTEGER        KHPOS,NOFLET,NCHAR,MAX
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh2st1.f,v $
     . $',                                                             '
     .$Id: sh2st1.f,v 1.2 1996/03/18 22:53:55 page Exp $
     . $' /
C    ===================================================================
C

        KHLET = '    '
        NCHAR = 0

C                   If current char position is in a message line

        IF (KHPOS .GT. 2) THEN

C                   Loop while char is not blank nor is output str full

          MAX = LEN(KHLET)
   30     IF (KHAR.EQ.' ' .OR. KHPOS.LE.2 .OR. NCHAR.GE.MAX) GOTO 40

C                   If cap letter place in string, get next char

            IF (KHAR.GE.'A' .AND. KHAR.LE.'Z') THEN
              NCHAR = NCHAR+1
              KHLET(NCHAR:NCHAR) = KHAR
              CALL SHGETK(KHAR,KHPOS)

C                   Else output blank string and force end of loop

            ELSE
              NCHAR = -1
              MAX   = -1
              KHLET = '    '
            ENDIF

            GOTO 30
   40     CONTINUE

        ENDIF

C                   Output number of chars in string, else -1

        NOFLET = NCHAR

      RETURN
      END
