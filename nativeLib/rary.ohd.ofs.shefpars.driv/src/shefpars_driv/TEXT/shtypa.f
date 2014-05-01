C  =====================================================================
C  pgm: SHTYPA .. Decode data type and date codes for ".A" format
C
C  use:     CALL SHTYPA(KHAR,KHPOS)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C
C  rqd: SHGETK,SH2BLA,SHERR,SH2TYA
C
C  cmt: Keywords: DY,DM,DD,DH,DN,DS,DJ,DC,DR,DQ,DU,DV
C  =====================================================================
      SUBROUTINE SHTYPA(KHAR,KHPOS)

      EXTERNAL       SHGETK,SH2BLA,SHERR,SH2TYA

      CHARACTER*1    KHAR
      INTEGER        KHPOS,NOFBL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shtypa.f,v $
     . $',                                                             '
     .$Id: shtypa.f,v 1.1 1995/11/29 22:22:12 dws Exp $
     . $' /
C    ===================================================================
C

      IF (KHPOS .GT. 2) THEN

        CALL SH2BLA(KHAR,KHPOS,NOFBL)

  100   IF (KHAR.NE.'D' .AND. KHAR.NE.'/') GOTO 110
          IF (KHAR .EQ. '/') THEN
            CALL SHGETK(KHAR,KHPOS)
            CALL SH2BLA(KHAR,KHPOS,NOFBL)
          ELSE
            CALL SHGETK(KHAR,KHPOS)

            CALL SH2TYA(KHAR,KHPOS)

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

      ENDIF

      RETURN
      END
