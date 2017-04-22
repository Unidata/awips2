C  =====================================================================
C  pgm: SH3DT7 .. Get data time interval for "DI" keyword (.E format)
C
C  use:     CALL SH3DT7(KHAR,KHPOS,IUNIT,INTVAL)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: IUNIT ..... time unit for time series interval - INT
C  out:               1 = minute   5 = year
C  out:               2 = hour     6 = end-of-month
C  out:               3 = day      7 = second
C  out:               4 = month
C  out: INTVAL .... time interval for given time unit (+ or -) - INT
C
C  rqd: SHGETK,SHERR,SH2NUM
C  =====================================================================
      SUBROUTINE SH3DT7(KHAR,KHPOS,IUNIT,INTVAL)

      EXTERNAL       SHGETK,SHERR,SH2NUM

      CHARACTER*1    KHAR
      INTEGER        KHPOS,IUNIT,INTVAL,NOFD,NUMBER,ISIGN
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3dt7.f,v $
     . $',                                                             '
     .$Id: sh3dt7.f,v 1.1 1995/11/29 22:21:12 dws Exp $
     . $' /
C    ===================================================================
C

        IF (KHPOS .GT. 2 ) THEN

          CALL SHGETK(KHAR,KHPOS)

          IF (KHAR .EQ. 'S') THEN
            IUNIT = 7
          ELSEIF (KHAR .EQ. 'N') THEN
            IUNIT = 1
          ELSEIF (KHAR .EQ. 'H') THEN
            IUNIT = 2
          ELSEIF (KHAR .EQ. 'D') THEN
            IUNIT = 3
          ELSEIF (KHAR .EQ. 'M') THEN
            IUNIT = 4
          ELSEIF (KHAR .EQ. 'Y') THEN
            IUNIT = 5
          ELSEIF (KHAR .EQ. 'E') THEN
            IUNIT = 6
          ELSE
            IUNIT = -9999
            CALL SHERR('E',25,KHPOS,KHAR)
          ENDIF

          IF (IUNIT .NE. -9999) THEN
            CALL SHGETK(KHAR,KHPOS)
            ISIGN = 1
            IF (KHAR.EQ.'-')                  ISIGN = -1
            IF (KHAR.EQ.'-' .OR. KHAR.EQ.'+') CALL SHGETK(KHAR,KHPOS)
            CALL SH2NUM(KHAR,KHPOS,2,NOFD,NUMBER)
            IF (NOFD .GT. 0) THEN
              INTVAL = ISIGN*NUMBER
            ELSE
              INTVAL = -9999
              CALL SHERR('E',26,KHPOS,KHAR)
            ENDIF
          ENDIF

        ENDIF

      RETURN
      END
