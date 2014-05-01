C  =====================================================================
C  pgm: SH3DT5 .. Get data units code for "DU" keyword
C
C  use:     CALL SH3DT5(KHAR,KHPOS)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C
C  rqd: SHGETK,SHERR,SHSAVD
C  =====================================================================
      SUBROUTINE SH3DT5(KHAR,KHPOS)

      EXTERNAL       SHGETK,SHERR,SHSAVD

      CHARACTER*1    KHAR
      INTEGER        KHPOS,KODU,III
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3dt5.f,v $
     . $',                                                             '
     .$Id: sh3dt5.f,v 1.1 1995/11/29 22:21:10 dws Exp $
     . $' /
C    ===================================================================
C

        IF (KHPOS .GT. 2 ) THEN

            CALL SHGETK(KHAR,KHPOS)

            KODU = -1
            IF       (KHAR .EQ. 'S') THEN
                KODU = 0
              ELSEIF (KHAR .EQ. 'E') THEN
                KODU = 1
            ENDIF

            IF (KODU .NE. -1) THEN
                CALL SHSAVD('P',III,KODU)
                CALL SHGETK(KHAR,KHPOS)
              ELSE
                CALL SHERR('E',22,KHPOS,KHAR)
            ENDIF

        ENDIF

      RETURN
      END
