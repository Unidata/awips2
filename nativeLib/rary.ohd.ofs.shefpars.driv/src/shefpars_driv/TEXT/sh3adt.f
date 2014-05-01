C  =====================================================================
C  pgm: SH3ADT .. Adjust value for trace or for alternate units
C
C  use:     CALL SH3ADT(KHAR,KHPOS,VALUE)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  i/o: VALUE ..... data value - DOUBLE PRECISION
C
C  rqd: SHERR,SHFACT,SHSAVD,SHSAVP
C  =====================================================================
      SUBROUTINE SH3ADT(KHAR,KHPOS,VALUE)

      EXTERNAL       SHERR,SHFACT,SHSAVD,SHSAVP

      CHARACTER*1        KHAR,KH1,KH2
      CHARACTER*8        PARCOD
      INTEGER            KHPOS,KODU,JJ,IVAL,III
      DOUBLE PRECISION   VALUE,FACTOR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3adt.f,v $
     . $',                                                             '
     .$Id: sh3adt.f,v 1.3 1996/07/11 19:55:20 dws Exp $
     . $' /
C    ===================================================================
C

        IF (KHPOS .NE. 1) THEN

          CALL SHSAVP('G',III,PARCOD)

          IF (VALUE .LT. -8D10) THEN

            JJ = 0
            KH1 = PARCOD(1:1)
            KH2 = PARCOD(2:2)
            IF (KH1 .EQ. 'S') THEN
              IF (KH2.EQ.'D' .OR. KH2.EQ.'F' .OR. KH2.EQ.'W') JJ = 1
             ELSEIF (KH1 .EQ. 'P') THEN
CC              'Y' for code 'PY' is not needed, already made 'PP'
              IF (KH2.EQ.'C' .OR. KH2.EQ.'P') JJ = 1
            ENDIF

            IF (JJ .EQ. 1) THEN
              VALUE = 0.001D0
             ELSE
              CALL SHERR('E',31,KHPOS,KHAR)
            ENDIF

          ELSE

            CALL SHSAVD('G',III,KODU)

            IF (KODU .EQ. 0) THEN
              IVAL = VALUE - 0.01D0
              IF (IVAL.NE.-9999 .AND. IVAL.NE.-9002) THEN
                CALL SHFACT('GET_VALUE   ',KHAR,KHPOS,PARCOD,FACTOR)
                IF (KHPOS .GT. 1) THEN
                  IF (FACTOR .LT. 0D0) THEN
                    VALUE = VALUE*1.8D0 + 32.0D0
                   ELSE
                    VALUE = VALUE*FACTOR
                  ENDIF
                ENDIF
              ENDIF
            ENDIF

          ENDIF

        ENDIF

      RETURN
      END
