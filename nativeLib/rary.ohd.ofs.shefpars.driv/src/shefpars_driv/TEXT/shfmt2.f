C  =====================================================================
C  pgm: SHFMT2 .. Handle format specifier continuations and revisions
C
C  use:     CALL SHFMT2(KHAR,KHPOS,FORMT,LFORMT,LPOS,IREV,ICONT)
C
C  i/o: KHAR ...... next char entered and/or returned - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  i/o:                 (see routine "shgetk")
C   in: FORMT ..... format found (A, B, E, else blank) - CHAR*1
C   in: LFORMT .... format for the previous shef message line - CHAR*1
C   in: LPOS ...... line status, "KHPOS", for the previous command - INT
C  i/o: IREV ...... revision code (0 = no, 1 = yes) - INT
C  i/o:               (set to new code in this routine)
C  out: ICONT ..... continuation code (0 = no, 1 = yes) - INT
C
C  rqd: SHGETK,SHERR,SH2BLA
C  =====================================================================
      SUBROUTINE SHFMT2(KHAR,KHPOS,FORMT,LFORMT,LPOS,IREV,ICONT)

      EXTERNAL       SHGETK,SHERR,SH2BLA

      CHARACTER*1    KHAR,FORMT,LFORMT
      INTEGER        KHPOS,IREV,ICONT,LREV,LPOS,NUM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shfmt2.f,v $
     . $',                                                             '
     .$Id: shfmt2.f,v 1.3 1998/07/02 20:17:25 page Exp $
     . $' /
C    ===================================================================
C

C                   Save previous revision code, if no error and have
C                    a format code, check for revision and/or
C                    continuation codes, get next char if found

        LREV  = IREV

        IREV  = 0
        ICONT = 0
        IF (KHPOS.GT.0 .AND. FORMT.NE.' ') THEN

            IF (KHAR .EQ. 'R') THEN
              IREV = 1
              CALL SHGETK(KHAR,KHPOS)
            ENDIF

            IF (KHAR.GE.'0' .AND. KHAR.LE.'9') THEN
              ICONT = 1
              CALL SHGETK(KHAR,KHPOS)
              IF (KHAR.GE.'0' .AND. KHAR.LE.'9') THEN
                CALL SHGETK(KHAR,KHPOS)
              ENDIF
            ENDIF

C                 If message is a continuation, following must be true:
C                  1) previous message had no error, LPOS,
C                  2) previous format code must be the same, LFORMT,
C                  3) if this is a revision, prev must be revision, LREV
C                  4) if previous is revision, force this to be revision

            IF (KHPOS.GT.2 .AND. ICONT.EQ.1) THEN
              IF (LPOS .LE. 1) THEN
                CALL SHERR('E',11,KHPOS,KHAR)
                FORMT = ' '
              ELSEIF (LFORMT .NE. FORMT) THEN
                CALL SHERR('E',9,KHPOS,KHAR)
                FORMT = ' '
              ELSEIF (IREV.EQ.1 .AND. LREV.NE.1) THEN
                CALL SHERR('E',10,KHPOS,KHAR)
                FORMT = ' '
              ELSEIF (LREV .EQ. 1) THEN
                IREV = 1
              ENDIF
            ENDIF

C                 Must have at least one blank, then something else

            IF (KHPOS.GT.2 .AND. KHAR.NE.' ') THEN
              CALL SHERR('E',8,KHPOS,KHAR)
            ELSE
              CALL SH2BLA(KHAR,KHPOS,NUM)
              IF (KHPOS .LE. 2) CALL SHERR('W',67,KHPOS,KHAR)
            ENDIF

        ENDIF

      RETURN
      END
