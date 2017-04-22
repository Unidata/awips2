C  =====================================================================
C  pgm: SH3DT6 .. Get data duration code for "DV" keyword
C
C  use:     CALL SH3DT6(KHAR,KHPOS)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C
C  rqd: SHGETK,SHERR,SH2NUM,SHSAVI
C  =====================================================================
      SUBROUTINE SH3DT6(KHAR,KHPOS)

      EXTERNAL       SHGETK,SHERR,SH2NUM,SHSAVI

      CHARACTER*1    KHAR
      INTEGER        KHPOS,IDCODD,ITEMP,IFOU,NUMBER,III
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3dt6.f,v $
     . $',                                                             '
     .$Id: sh3dt6.f,v 1.2 1999/01/20 15:07:42 page Exp $
     . $' /
C    ===================================================================
C

        IF (KHPOS .GT. 2 ) THEN

          IDCODD = -1
          CALL SHGETK(KHAR,KHPOS)

          IF (KHAR .EQ. 'Z') THEN
              IDCODD = 5000
              CALL SHGETK(KHAR,KHPOS)
          ELSE
            IF (KHAR .EQ. 'S') THEN
              ITEMP = 7000
            ELSEIF (KHAR .EQ. 'N') THEN
              ITEMP = 0000
            ELSEIF (KHAR .EQ. 'H') THEN
              ITEMP = 1000
            ELSEIF (KHAR .EQ. 'D') THEN
              ITEMP = 2000
            ELSEIF (KHAR .EQ. 'M') THEN
              ITEMP = 3000
            ELSEIF (KHAR .EQ. 'Y') THEN
              ITEMP = 4000
            ELSE
              ITEMP = -1
              CALL SHERR('E',23,KHPOS,KHAR)
            ENDIF

            IF (ITEMP .NE. -1) THEN
              CALL SHGETK(KHAR,KHPOS)
              CALL SH2NUM(KHAR,KHPOS,2,IFOU,NUMBER)
              IF (IFOU .GT. 0) THEN
                IDCODD = ITEMP + NUMBER
              ELSE
                CALL SHERR('E',24,KHPOS,KHAR)
              ENDIF
            ENDIF
          ENDIF

          IF (IDCODD .NE. -1) CALL SHSAVI('P',III,IDCODD)

        ENDIF

      RETURN
      END
