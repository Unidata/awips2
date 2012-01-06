C  =====================================================================
C  pgm: SH2LOC .. Adjust local to zulu time depending on daylight svgs
C
C  use:     CALL SH2LOC(KHAR,KHPOS,NY,NM,ND,NH,NN,NS,IADJ,II)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C   in: NY ........ year number (1753-2199) and may be altered - INT
C   in: NM ........ month number (1-12), else unchanged - INT
C   in: ND ........ day number (1-31), else unchanged - INT
C   in: NH ........ hour number (0-24), else unchanged - INT
C   in: NN ........ minute number (0-59), else unchanged - INT
C   in: NS ........ second number (0-59), else unchanged - INT
C  out: IADJ ...... adjusted time in minutes output - INT
C   in: II ........ incoming time flag, 0 for local, 1 for zulu - INT
C
C  rqd: SHERR,SHSAVA,SHSAVS
C
C  cmt: This rtn good for years 1976 thru 2040.
C  cmt: Check "http://webexhibits.org/daylightsaving/b.html" for
C  cmt:  DST begin and end dates.
C  =====================================================================
      SUBROUTINE SH2LOC(KHAR,KHPOS,NY,NM,ND,NH,NN,NS,IADJ,II)

      EXTERNAL       SHERR,SHSAVA,SHSAVS

      CHARACTER*1    KHAR
      INTEGER        KHPOS,NY,NM,ND,NH,NN,NS
      INTEGER        LADJ,LSVG,IADJ,II,IY,ID,IA,III,ITABLE(2,65)
      INTEGER        IDATIM(7)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/shefpars_driv/RCS/sh2loc.f,v $
     . $',                                                             '
     .$Id: sh2loc.f,v 1.4 2006/05/11 20:59:40 jgofus Exp jgofus $
     . $' /
C    ===================================================================
C

C      ITABLE(1,I) IS THE CHANGE OVER DATE IN APRIL TO DAYLIGHT TIME.
C      ITABLE(2,I) IS THE CHANGE OVER DATE IN OCTOBER TO STANDARD TIME.
C      THE CHANGE IS ASSUMED TO OCCUR AT 2AM.

      DATA ITABLE
     $            / 26,31, 24,30, 30,29, 29,28, 27,26, 26,25,
     $              25,31, 24,30, 29,28, 28,27, 27,26,  5,25,
     $               3,30,  2,29,  1,28,  7,27,  5,25,  4,31,
     $               3,30,  2,29,  7,27,  6,26,  5,25,  4,31,
     $               2,29,  1,28,  7,27,  6,26,  4,31,  3,30,
     $               2,29, 11, 4,  9, 2,  8, 1, 14, 7, 13, 6,
     $              11, 4, 10, 3,  9, 2,  8, 1, 13, 6, 12, 5,
     $              11, 4, 10, 3,  8, 1, 14, 7, 13, 6, 12, 5,
     $              10, 3,  9, 2,  8, 1, 14, 7, 12, 5, 11, 4,
     $              10, 3,  9, 2, 14, 7, 13, 6, 12, 5, 11, 4,
     $               9, 2,  8, 1, 14, 7, 13, 6, 11, 4         /

C  TEST IF ADJUSTMENT NECESSARY

      CALL SHSAVA('G',III,LADJ)
      CALL SHSAVS('G',III,LSVG)

      IADJ = LADJ
      IF (KHPOS.NE.1 .AND. LSVG.EQ.1) THEN

C         ICOND = 0 ....... Summer time so must use daylight savings
C         ICOND = 1 ....... Year after 2006, check month for 3 or 11
C         ICOND = 1 ....... Year before 2007, check month for 4 or 10
C         ICOND = 2 ....... Definitely winter, no daylight savings

        ICOND = 0
        IF (NM.GT.4 .AND. NM.LT.10) THEN
            ICOND = 2
        ELSEIF (NM.EQ.3 .OR. NM.EQ.11) THEN
          IF (NY.GE.2007 .AND. NY.LE.2040) THEN
            ICOND = 1
          ELSEIF (NY.GE.7 .AND. NY.LE.40) THEN
            ICOND = 1
          ENDIF
        ELSEIF (NM.EQ.4 .OR. NM.EQ.10) THEN
          IF (NY.GE.1976 .AND. NY.LE.2006) THEN
            ICOND = 1
          ELSEIF (NY.GE.76 .AND. NY.LE.99) THEN
            ICOND = 1
          ELSEIF (NY.GE.0 .AND. NY.LE.6) THEN
            ICOND = 1
          ELSE
            ICOND = 2
          ENDIF
        ENDIF

C         Now act on ICOND

        IF (ICOND .EQ. 2) THEN
          IADJ = IADJ - 60
        ELSEIF (ICOND .EQ. 1) THEN
          IF (NY.GE.1976 .AND. NY.LE.2040) THEN
            IY = NY-1975
          ELSEIF (NY.GE.76 .AND. NY.LE.99) THEN
            IY = NY-75
          ELSEIF (NY.GE.0 .AND. NY.LE.40) THEN
            IY = NY+25
          ELSE
            CALL SHERR('E',48,KHPOS,KHAR)
          ENDIF

          IF (KHPOS .NE. 1) THEN
            IF (NM.EQ.3 .OR. NM.EQ.4) THEN
              ID = ITABLE(1,IY)
              IF (ND .GT. ID) THEN
                IADJ = IADJ - 60
              ELSEIF (ND .EQ. ID) THEN
                IF (II .EQ. 0) THEN
                  IF (NH .EQ. 2) THEN
                    IF(NN.GT.0.OR.NS.GT.0) CALL SHERR('E',44,KHPOS,KHAR)
                  ELSEIF (NH .GT. 2) THEN
                    IADJ = IADJ - 60
                  ENDIF
                ELSE
                  IA = 2 + (IADJ/60)
                  IF (NH .EQ. IA) THEN
                    IF(NN.GT.0.OR.NS.GT.0) IADJ = IADJ - 60
                  ELSEIF (NH .GT. IA) THEN
                    IADJ = IADJ - 60
                  ENDIF
                ENDIF
              ENDIF
            ELSE
              ID = ITABLE(2,IY)
              IF (ND .LT. ID) THEN
                IADJ = IADJ - 60
              ELSEIF (ND .EQ. ID) THEN
                IF (II .EQ. 0) THEN
                  IF (NH .EQ. 2) THEN
                    IF(NN.EQ.0.AND.NS.EQ.0) IADJ = IADJ - 60
                  ELSEIF (NH .LT. 2) THEN
                    IADJ = IADJ - 60
                  ENDIF
                ELSE
                  IA = 1 + (IADJ/60)
                  IF (NH .EQ. IA) THEN
                    IF(NN.EQ.0.AND.NS.EQ.0) IADJ = IADJ - 60
                  ELSEIF (NH .LT. IA) THEN
                    IADJ = IADJ - 60
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF

        ENDIF

      ENDIF

      RETURN
      END
