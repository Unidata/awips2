C  =====================================================================
C  pgm: SHFMT1 .. Get next line with a format type code (.A, .B, or .E)
C
C  use:      CALL SHFMT1(KHAR,KHPOS,FORMT,LFORMT)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  i/o: FORMT ..... format found (A, B, E, else blank) - CHAR*1
C  i/o:             (must be initialized to blank for new shef file)
C  out: LFORMT .... format of previous message - CHAR*1
C
C  rqd: SHGETK,SH2BEG
C  =====================================================================
      SUBROUTINE SHFMT1(KHAR,KHPOS,FORMT,LFORMT)

      EXTERNAL       SHGETK,SH2BEG

      CHARACTER*1    KHAR,FORMT,LFORMT
      INTEGER        KHPOS
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shfmt1.f,v $
     . $',                                                             '
     .$Id: shfmt1.f,v 1.1 1996/03/19 11:36:43 page Exp $
     . $' /
C    ===================================================================
C

C                   Save prev fmt code, loop until new code (or err/eof)

        LFORMT = FORMT
        FORMT  = ' '
  100   IF (FORMT .NE. ' ') GOTO 130

C                   Rtn "sh2beg" gets next "first" char starting a line;
C                    1) fatal error or end-of-file, set format to -1,
C                    2) char must be a dot,  3) must be in first column,
C                    4) get next char, set format if A,B, or E, else err

          CALL SH2BEG(KHAR,KHPOS)

          IF (KHPOS .LE. 0) THEN
              FORMT = '-'
          ELSEIF (KHAR .NE. '.') THEN
CCC           CALL SHERR('W',5,KHPOS,KHAR)
              KHPOS = 1
          ELSEIF (KHPOS .NE. 4) THEN
              CALL SHERR('E',6,KHPOS,KHAR)
              KHPOS = 1
          ELSE
              CALL SHGETK(KHAR,KHPOS)
              IF (KHAR.EQ.'A' .OR. KHAR.EQ.'B' .OR. KHAR.EQ.'E') THEN
                FORMT = KHAR
              ELSE
                CALL SHERR('E',7,KHPOS,KHAR)
                KHPOS = 1
              ENDIF
              CALL SHGETK(KHAR,KHPOS)
          ENDIF

C                   If Format ".E" check for ".END" of prev "B" message

          IF (FORMT.EQ.'E' .AND. KHAR.EQ.'N') THEN
              CALL SHGETK(KHAR,KHPOS)
              IF (KHAR .EQ. 'D') THEN
                CALL SHGETK(KHAR,KHPOS)
                IF (KHPOS.GT.2 .AND. KHAR.NE.' ') THEN
                  CALL SHERR('E',8,KHPOS,KHAR)
                ELSE
                  IF (LFORMT .EQ. 'B') THEN
                    LFORMT = ' '
                  ELSE
                    CALL SHERR('W',68,KHPOS,KHAR)
                  ENDIF
                ENDIF
              ELSE
                CALL SHERR('E',8,KHPOS,KHAR)
              ENDIF
              FORMT = ' '
              KHPOS = 1
          ENDIF

          GOTO 100
  130   CONTINUE

      RETURN
      END
