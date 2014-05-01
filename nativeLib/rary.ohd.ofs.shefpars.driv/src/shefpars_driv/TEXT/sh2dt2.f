C  =====================================================================
C  pgm: SH2DT2 .. Check current char; if digit, save and get next char
C
C  use:     CALL SH2DT2(KHAR,KHPOS,NOFD,KHARSV)
C
C  i/o: KHAR ...... last buffer char obtained (current char) - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  i/o: NOFD ...... num of digits, increment if digit is found - INT
C  out: KHARSV .... saved char if digit found, else unchanged - CHAR*1
C
C  rqd: SHGETK
C  =====================================================================
      SUBROUTINE SH2DT2(KHAR,KHPOS,NOFD,KHARSV)

      EXTERNAL       SHGETK

      CHARACTER*1    KHAR,KHARSV
      INTEGER        KHPOS,NOFD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh2dt2.f,v $
     . $',                                                             '
     .$Id: sh2dt2.f,v 1.1 1995/11/29 22:20:43 dws Exp $
     . $' /
C    ===================================================================
C

        IF (KHAR.GE.'0' .AND. KHAR.LE.'9') THEN
          NOFD   = NOFD+1
          KHARSV = KHAR
          CALL SHGETK(KHAR,KHPOS)
        ENDIF

      RETURN
      END
