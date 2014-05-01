C  =====================================================================
C  pgm: SH2NUM .. Get positive num up to non-digit or max num of digits
C
C  use:     CALL SH2NUM(KHAR,KHPOS,MXLEN,NOFDIG,NUMPOS)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C   in: MXLEN ..... maximum number of digits allowed - INT
C  out: NOFDIG .... number of chars found as digits - INT
C  out: NUMPOS .... positive integer found, else 0 - INT
C  out:             (Note, search stops at end of string length)
C
C  rqd: SHGETK
C
C  cmt: Routine will NOT process if current char is "end-of-line".
C  cmt: "KHAR" must be guaranteed not blank if "KHPOS" is less than 2.
C  =====================================================================
      SUBROUTINE SH2NUM(KHAR,KHPOS,MXLEN,NOFDIG,NUMPOS)

      EXTERNAL       SHGETK

      INTRINSIC      ICHAR
      INTEGER        ICHAR

      CHARACTER*1    KHAR
      INTEGER        KHPOS,MXLEN,NOFDIG,NUMPOS,NUMSUM,NCHAR,MAX
      INTEGER        IZERO
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh2num.f,v $
     . $',                                                             '
     .$Id: sh2num.f,v 1.1 1995/11/29 22:20:47 dws Exp $
     . $' /
C    ===================================================================
C

        NUMSUM = 0
        NCHAR  = 0

C                   If current char position is in a message line

        IF (KHPOS .GT. 2) THEN

C                   Loop while char is a digit and MXLEN not exceeded

          IZERO = ICHAR('0')
          MAX   = MXLEN
   70     IF (NCHAR.GE.MAX .OR. KHAR.LT.'0' .OR. KHAR.GT.'9') GOTO 80
            NCHAR  = NCHAR+1
            NUMSUM = 10*NUMSUM + (ICHAR(KHAR)-IZERO)
            CALL SHGETK(KHAR,KHPOS)
            GOTO 70
   80     CONTINUE

        ENDIF

C                   Output number of digits and integer

        NOFDIG = NCHAR
        NUMPOS = NUMSUM

      RETURN
      END
