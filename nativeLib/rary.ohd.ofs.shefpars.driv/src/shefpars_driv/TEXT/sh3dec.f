C  =====================================================================
C  pgm: SH3DEC .. Check if decimal appeared in value for certain p-codes
C
C  use:     CALL SH3DEC(KHAR,KHPOS,VALUE,NDEC)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  i/o: VALUE ..... data value - DOUBLE PRECISION
C   in: NDEC ...... number of decimals in value (0 or 1) - INT
C
C  rqd: SHERR,SHSAVD,SHSAVP
C
C  cmt: This rtn limited to English units only (KODU = 1).
C
C  cmt: This rtn applies to parameter codes "PP" and "PC" only.
C  cmt: To make the lack of a decimal an error instead of a warning,
C  cmt:  use "'E'" instead of "'W'" in the call to "SHERR".
C  =====================================================================
      SUBROUTINE SH3DEC(KHAR,KHPOS,VALUE,NDEC)

      EXTERNAL       SHERR,SHSAVD,SHSAVP

      CHARACTER*1        KHAR
      CHARACTER*8        PARCOD
      CHARACTER*2        KHOD
      INTEGER            KHPOS,NDEC,KODU,III
      DOUBLE PRECISION   VALUE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3dec.f,v $
     . $',                                                             '
     .$Id: sh3dec.f,v 1.1 1995/11/29 22:21:02 dws Exp $
     . $' /
C    ===================================================================
C

        IF (KHPOS .NE. 1) THEN

          IF (NDEC.EQ.0 .AND. VALUE.GT.0D0) THEN
            CALL SHSAVP('G',III,PARCOD)
            KHOD(1:1) = PARCOD(1:1)
            KHOD(2:2) = PARCOD(2:2)
            IF (KHOD.EQ.'PP' .OR. KHOD.EQ.'PC') THEN
              CALL SHSAVD('G',III,KODU)
              IF (KODU .EQ. 1) THEN
                CALL SHERR('W',58,KHPOS,KHAR)
                VALUE = VALUE/100D0
              ENDIF
            ENDIF
          ENDIF

        ENDIF

      RETURN
      END
