C  =====================================================================
C  pgm: SHTYPD .. Decode data type and date codes for inside ".B" format
C
C  use:     CALL SHTYPD(KHAR,KHPOS,JC1,JC2,JC3,JC4,JC5,JC6,LCHAR,LNULL)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: JC1 ....... if 1 then zulu date was updated - INT
C  out: JC2 ....... if 1 then creation date updated - INT
C  out: JC3 ....... if 1 then data qualifier code updated - INT
C  out: JC4 ....... if 1 then type of units code updated - INT
C  out: JC5 ....... if 1 then duration code updated - INT
C  out: JC6 ....... if 1 then date increments updated - INT
C  i/o: LCHAR ..... last initial char observed (initz to '/') - CHAR*1
C  out: LNULL ..... if 1 then have null data set, else 0 (initz 0) - INT
C
C  rqd: SHGETK,SH2BLA,SHERR,SH2TYD
C
C  cmt: Keywords: DY,DM,DD,DH,DN,DS,DJ,DC,DR,DQ,DU,DV
C  =====================================================================
      SUBROUTINE SHTYPD(KHAR,KHPOS,JC1,JC2,JC3,JC4,JC5,JC6,LCHAR,LNULL)

      EXTERNAL       SHGETK,SH2BLA,SHERR,SH2TYD

      CHARACTER*1    KHAR,LCHAR,LCHARX
      INTEGER        KHPOS,LNULL,NOFBL,JC1,JC2,JC3,JC4,JC5,JC6
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shtypd.f,v $
     . $',                                                             '
     .$Id: shtypd.f,v 1.1 1995/11/29 22:22:13 dws Exp $
     . $' /
C    ===================================================================
C

      LNULL = 0
      IF (KHPOS .GT. 2) THEN

        CALL SH2BLA(KHAR,KHPOS,NOFBL)
        LCHARX = KHAR
  100   IF (LNULL.EQ.1 .OR. (KHAR.NE.'D' .AND. KHAR.NE.'/')) GOTO 110
          IF (KHAR .EQ. '/') THEN
            CALL SHGETK(KHAR,KHPOS)
            CALL SH2BLA(KHAR,KHPOS,NOFBL)
            IF (LCHAR .EQ. '/') THEN
              LNULL = 1
            ELSEIF (KHPOS .GT. 2) THEN
              LCHARX = KHAR
              LCHAR  = KHAR
            ENDIF
          ELSE
            LCHARX = KHAR
            LCHAR  = KHAR
            CALL SHGETK(KHAR,KHPOS)

            CALL SH2TYD(KHAR,KHPOS,JC1,JC2,JC3,JC4,JC5,JC6)

            CALL SH2BLA(KHAR,KHPOS,NOFBL)

            IF (KHPOS.GT.2 .AND. KHAR.NE.'/') THEN
              IF (NOFBL.EQ.0 .AND. KHAR.EQ.'D') THEN
                CALL SHERR('W',55,KHPOS,KHAR)
              ELSEIF (NOFBL.EQ.0 .AND. KHAR.NE.'D') THEN
                CALL SHERR('E',56,KHPOS,KHAR)
              ELSEIF (NOFBL.GT.0 .AND. KHAR.NE.'D') THEN
                CALL SHERR('W',57,KHPOS,KHAR)
              ENDIF
            ENDIF

          ENDIF
          GOTO 100
  110   CONTINUE

        LCHAR = LCHARX

      ENDIF

      RETURN
      END
