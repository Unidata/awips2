C  =====================================================================
C  pgm: SH2NXD .. Go to next delimiter (blank, comma, slash, s/d quotes)
C
C  use:     CALL SH2NXD(KHAR,KHPOS)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C
C  rqd: SHGETK,SH2BLA
C  =====================================================================
      SUBROUTINE SH2NXD(KHAR,KHPOS)

      EXTERNAL       SHGETK,SH2BLA

      CHARACTER*1    KHAR
      INTEGER        KHPOS,NOFBLA
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh2nxd.f,v $
     . $',                                                             '
     .$Id: sh2nxd.f,v 1.1 2000/03/14 14:14:25 page Exp $
     . $' /
C    ===================================================================
C

C                   Loop until next char is a delimiter (or eol)

   10   IF (KHPOS.LE.2  .OR. KHAR.EQ.' ' .OR. KHAR.EQ.'/' .OR.
     $      KHAR.EQ.',' .OR. KHAR.EQ.'"' .OR. KHAR.EQ.''''    ) GOTO 20
          CALL SHGETK(KHAR,KHPOS)
          GOTO 10
   20   CONTINUE

        IF (KHPOS .GT. 2) CALL SH2BLA(KHAR,KHPOS,NOFBLA)

      RETURN
      END
