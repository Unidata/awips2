C  =====================================================================
C  pgm: SH3DLM .. Check if next char is a good delimiter or unexpected
C
C  use:     CALL SH3DLM(KHAR,KHPOS,NDIG)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  i/o: NDIG ...... number of digits in the last data value - INT
C  i/o:             (can be made zero if unexpected char is next)
C
C  rqd: SHERR,SH2FND,SH2BLA
C
C  cmt: To accept more suspect data, add a slash to the "SH2FND"
C  cmt:  calls as "CALL SH2FND(KHAR,KHPOS,'/,')" in order to skip only
C  cmt:  to the next data element instead of the next station.
C  =====================================================================
      SUBROUTINE SH3DLM(KHAR,KHPOS,NDIG)

      EXTERNAL       SHERR,SH2FND,SH2BLA

      CHARACTER*1    KHAR,COMMA
      INTEGER        KHPOS,NDIG,NOC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3dlm.f,v $
     . $',                                                             '
     .$Id: sh3dlm.f,v 1.1 1995/11/29 22:21:03 dws Exp $
     . $' /
C    ===================================================================
C

      DATA           COMMA / ',' /

C                   Check if trailing chars exist as an error

        IF (KHPOS .GT. 2) THEN
          IF (NDIG .EQ. 0) THEN
            IF(KHAR.NE.','.AND.KHAR.NE.'/')CALL SHERR('W',52,KHPOS,KHAR)
            CALL SH2FND(KHAR,KHPOS,COMMA)
          ELSE
            IF (KHAR .EQ. ' ') THEN
              CALL SH2BLA(KHAR,KHPOS,NOC)
              IF (KHPOS.GT.2 .AND. KHAR.NE.'/' .AND. KHAR.NE.',') THEN
                CALL SHERR('W',51,KHPOS,KHAR)
              ENDIF
            ELSEIF (KHAR.NE.'/' .AND. KHAR.NE.',') THEN
C               Next stmt could be eliminated to accept more data
              NDIG = 0
              CALL SHERR('W',51,KHPOS,KHAR)
              CALL SH2FND(KHAR,KHPOS,COMMA)
            ENDIF
          ENDIF
        ENDIF

      RETURN
      END
