C  =====================================================================
C  pgm: SHTYPE .. Decode data type and date codes for ".E" format
C
C  use:     CALL SHTYPE(KHAR,KHPOS,IUNIT,INTVAL,LCHAR,LNULL,ITMSER)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: IUNIT ..... time unit for time series interval - INT
C  out: INTVAL .... time interval for given time unit (+ or -) - INT
C  i/o: LCHAR ..... last initial char observed (initz to '/') - CHAR*1
C  out: LNULL ..... if 1 then have null data set, else 0 (initz 0) - INT
C  out: ITMSER .... time series indicator, may be set to 0 - INT
C
C  rqd: SHGETK,SH2BLA,SHERR,SH2TYA,SH3DT7
C
C  cmt: Keywords: DY,DM,DD,DH,DN,DS,DJ,DC,DR,DQ,DU,DV,DI
C  =====================================================================
      SUBROUTINE SHTYPE(KHAR,KHPOS,IUNIT,INTVAL,LCHAR,LNULL,ITMSER)

      EXTERNAL       SHGETK,SH2BLA,SHERR,SH2TYA,SH3DT7

      CHARACTER*1    KHAR,LCHAR,LCHARX
      INTEGER        KHPOS,IUNIT,INTVAL,LNULL,ITMSER,NOFBL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shtype.f,v $
     . $',                                                             '
     .$Id: shtype.f,v 1.2 2000/03/14 14:26:11 page Exp $
     . $' /
C    ===================================================================
C

      IF (KHPOS .GT. 2) THEN

        CALL SH2BLA(KHAR,KHPOS,NOFBL)

        LCHARX = KHAR
        LNULL  = 0
  100   IF (LNULL.EQ.1 .OR. (KHAR.NE.'D' .AND. KHAR.NE.'/')) GOTO 110
          IF (KHAR .EQ. '/') THEN
            LCHARX = KHAR
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
            ITMSER = 0
            CALL SHGETK(KHAR,KHPOS)

            IF (KHAR .EQ. 'I') THEN
              CALL SH3DT7(KHAR,KHPOS,IUNIT,INTVAL)
             ELSE
              CALL SH2TYA(KHAR,KHPOS)
            ENDIF

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
