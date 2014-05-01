C  =====================================================================
C  pgm: SHDCOD .. Decode program for parsing SHEF data files
C
C  use:     CALL SHDCOD()
C
C  rqd: SHFMT1,SHFMT2,SHDECA,SHDECB,SHDECE,SHERR,SHLINE
C
C  cmt: Note, variable QUO is defined here with a length of 80 and
C  cmt:  carried to lower subroutines only so that its size can be
C  cmt:  defined in one place (it is used by routine SHQUOT later).
C  =====================================================================
      SUBROUTINE SHDCOD()

      EXTERNAL       SHFMT1,SHFMT2,SHDECA,SHDECB,SHDECE,SHERR,SHLINE

      CHARACTER*1    KHAR,FORMT,LFORMT
      CHARACTER*80   QUO
      INTEGER        KHPOS,LPOS,IREV,ICONT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shdcod.f,v $
     . $',                                                             '
     .$Id: shdcod.f,v 1.5 2006/03/07 15:18:49 dws Exp $
     . $' /
C    ===================================================================
C

C                   Initialize the num of warngs and errs to 0 in SHERR
C                   Initialize the num of lines in a message file to 0,
C                    KHPOS = 2, and KHAR = ':' in SHLINE

        KHPOS = 2
        KHAR  = ':'
        CALL SHERR('I',0,KHPOS,KHAR)
        CALL SHLINE('INITIALIZE      ',KHAR,KHPOS)

C                   Loop thru message formats until end of file or error

        FORMT = ' '
        IREV  = 0
   30   IF (KHPOS .LE. 0) GOTO 300
            LPOS = KHPOS

C                   Get format type (A, B, or E) in SHFMT1
C                   Check for revision and/or continuation codes, SHFMT2

            CALL SHFMT1(KHAR,KHPOS,FORMT,LFORMT)
            CALL SHFMT2(KHAR,KHPOS,FORMT,LFORMT,LPOS,IREV,ICONT)

C                   If format type found, parse the rest of the message

            IF (FORMT .EQ. 'A') THEN
                CALL SHDECA(KHAR,KHPOS,IREV,ICONT,QUO)
              ELSEIF (FORMT .EQ. 'B') THEN
                CALL SHDECB(KHAR,KHPOS,IREV,ICONT,QUO,FORMT)
              ELSEIF (FORMT .EQ. 'E') THEN
                CALL SHDECE(KHAR,KHPOS,IREV,ICONT,QUO)
            ENDIF

            GO TO 30
  300   CONTINUE

      RETURN
      END
