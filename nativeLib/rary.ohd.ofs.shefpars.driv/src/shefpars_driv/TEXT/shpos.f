C  =====================================================================
C  pgm: SHPOS .. Get "positional data": sta id, yr-mon-da-hr, min-adjust
C
C  use:     CALL SHPOS(KHAR,KHPOS,KHID)
C
C  i/o: KHAR ...... next char entered and/or returned - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: KHID ...... obtained positional data station id - CHAR*8
C
C  rqd: SH2BLA,SH2ST2,SH3DT0,SH3TZ0,SHERR,SHSAVL
C  =====================================================================
      SUBROUTINE SHPOS(KHAR,KHPOS,KHID)

      EXTERNAL       SH2BLA,SH2ST2,SH3DT0,SH3TZ0,SHERR,SHSAVL

      CHARACTER*1    KHAR
      CHARACTER*8    KHID
      INTEGER        KHPOS,LY,LM,LD,LH,LN,LS,NOC,III
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shpos.f,v $
     . $',                                                             '
     .$Id: shpos.f,v 1.6 2000/03/14 14:21:50 page Exp $
     . $' /
C    ===================================================================
C

      IF (KHPOS .GT. 2) THEN

C                   Get station id "KHID" as 3 to 8 chars

        CALL SH2ST2(KHAR,KHPOS,NOC,KHID)
         IF (KHPOS.GT.2 .AND. NOC.LT.0) THEN
           CALL SHERR('E',13,KHPOS,KHAR)
         ELSEIF (KHPOS.GT.2 .AND. NOC.LT.3) THEN
           CALL SHERR('E',47,KHPOS,KHAR)
         ENDIF

C                   Get at least one blank

        CALL SH2BLA(KHAR,KHPOS,NOC)
         IF (KHPOS .EQ. 2) THEN
           CALL SHERR('E',12,KHPOS,KHAR)
          ELSEIF (KHPOS.GT.2 .AND. NOC.LE.0) THEN
           CALL SHERR('E',14,KHPOS,KHAR)
         ENDIF

C                   Convert next 4 to 8 digits (ccyymmdd, yymmdd, mmdd)

        CALL SH3DT0(KHAR,KHPOS,LY,LM,LD)

C                   Get at least one blank if not end of line

        IF (KHPOS .GT. 2) THEN
          CALL SH2BLA(KHAR,KHPOS,NOC)
          IF (KHPOS.GT.2 .AND. NOC.LE.0) CALL SHERR('E',18,KHPOS,KHAR)
        ENDIF

C                   Get time zone, hr adjust, trailing blanks; if found

        CALL SH3TZ0(KHAR,KHPOS,LH,LN,LS)
        CALL SH2BLA(KHAR,KHPOS,NOC)
CCC      IF (KHPOS.GT.2 .AND. NOC.LE.0) CALL SHERR('E',18,KHPOS,KHAR)

C                   Look for 'D' or '/' after date or time zone

        IF (KHPOS.GT.2 .AND. KHAR.NE.'D' .AND. KHAR.NE.'/') THEN
          CALL SHERR('E',83,KHPOS,KHAR)
        ENDIF

C                   Put observation date in L-dates buffer

        CALL SHSAVL('P',III,LY,LM,LD,LH,LN,LS)

      ENDIF

      RETURN
      END
