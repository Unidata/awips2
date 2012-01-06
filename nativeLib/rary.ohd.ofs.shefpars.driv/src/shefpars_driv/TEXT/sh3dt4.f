C  =====================================================================
C  pgm: SH3DT4 .. Get data qualifier for "DQ" keyword or at end of value
C
C  use:     CALL SH3DT4(KHAR,KHPOS,KWAL)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: KWAL ...... data qualifier found, else set to blank - CHAR*1
C  out:             (set to '-' if code is in error)
C
C  rqd: SHGETK,SHSAVQ,SHQUAL,SHERR,SH2NXD
C  =====================================================================
      SUBROUTINE SH3DT4(KHAR,KHPOS,KWAL)

      EXTERNAL       SHGETK,SHSAVQ,SHQUAL,SHERR,SH2NXD

      CHARACTER*1    KHAR,KWAL
      INTEGER        KHPOS,III
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3dt4.f,v $
     . $',                                                             '
     .$Id: sh3dt4.f,v 1.4 2001/06/13 14:11:15 dws Exp $
     . $' /
C    ===================================================================
C

        KWAL = ' '
        IF (KHPOS .GT. 2) THEN

          CALL SHQUAL('GET_VALUE   ',KHAR,KHPOS,KWAL)

          IF (KWAL.NE.' ' .AND. KWAL.NE.'-') THEN
            CALL SHGETK(KHAR,KHPOS)
            IF (KHPOS.GT.2  .AND. KHAR.NE.' '  .AND.
     $          KHAR.NE.'"' .AND. KHAR.NE.'''' .AND.
     $          KHAR.NE.'/' .AND. KHAR.NE.','       ) THEN
              CALL SHERR('A',84,KHPOS,KHAR)
              CALL SH2NXD(KHAR,KHPOS)
              KWAL = '-'
            ELSE
              CALL SHSAVQ('P',III,KWAL)
            ENDIF
          ENDIF

        ENDIF

      RETURN
      END
