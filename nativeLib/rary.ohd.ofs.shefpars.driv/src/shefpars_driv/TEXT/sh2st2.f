C  =====================================================================
C  pgm: SH2ST2 .. Get next string (letters, _, digits), right pad blank
C
C  use:     CALL SH2ST2(KHAR,KHPOS,NOFSTR,KHSTR)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: NOFSTR .... number of non-blank chars found - INT
C  out:             (set to -1 if bad char is found)
C  out: KHSTR ..... string of next chars found, blank padded - CHAR*8
C  out:             (set to blank if bad char is found)
C  out:             (Note, search stops at end of string length)
C
C  rqd: SHGETK
C
C  cmt: Routine will NOT process if current char is "end-of-line".
C  cmt: "KHAR" must be guaranteed not blank if "KHPOS" is less than 2.
C  =====================================================================
      SUBROUTINE SH2ST2(KHAR,KHPOS,NOFSTR,KHSTR)

      EXTERNAL       SHGETK

      INTRINSIC      LEN
      INTEGER        LEN

      CHARACTER*8    KHSTR
      CHARACTER*1    KHAR
      INTEGER        KHPOS,NOFSTR,NCHAR,MAX
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh2st2.f,v $
     . $',                                                             '
     .$Id: sh2st2.f,v 1.3 1998/04/07 19:08:55 page Exp $
     . $' /
C    ===================================================================
C

        KHSTR = '        '
        NCHAR = 0

C                   If current char position is in a message line

        IF (KHPOS .GT. 2) THEN

C                   Loop while char is not blank nor is output str full

          MAX = LEN(KHSTR)
   50     IF (KHAR.EQ.' ' .OR. KHPOS.LE.2 .OR. NCHAR.GE.MAX) GOTO 60

C                   If not cap letter nor digit, set error indicator

            IF (KHAR.LT.'A' .OR. KHAR.GT.'Z' .AND. KHAR.NE.'_') THEN
              IF (KHAR.LT.'0' .OR. KHAR.GT.'9') NCHAR = -1
            ENDIF

C                   If letter or digit, add to string and get next char

            IF (NCHAR .GE. 0) THEN
              NCHAR  = NCHAR+1
              KHSTR(NCHAR:NCHAR) = KHAR
              CALL SHGETK(KHAR,KHPOS)

C                   Else output blank string and force end of loop

            ELSE
              MAX   = -1
              KHSTR = '        '
            ENDIF

            GOTO 50
   60     CONTINUE
        ENDIF

C                   Output number of chars in string, else -1

        NOFSTR = NCHAR

      RETURN
      END
