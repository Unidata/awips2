C  =====================================================================
C  pgm: SH2TYA .. Data type and date codes for routine "shtypa" (.A fmt)
C
C  use:     CALL SH2TYA(KHAR,KHPOS)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C
C  rqd: SH3DT1,SH3DT2,SH3DT3,SHERR,SH3DT4,SH3DT5,SH3DT6
C  rqd: SH3DT8,SHSAVM,SHSAVL
C
C  cmt: Keywords: DT,DY,DM,DD,DH,DN,DS,DJ,DC,DR,DQ,DU,DV
C  =====================================================================
      SUBROUTINE SH2TYA(KHAR,KHPOS)

      EXTERNAL       SH3DT1,SH3DT2,SH3DT3,SHERR
      EXTERNAL       SH3DT4,SH3DT5,SH3DT6,SH3DT8,SHSAVM,SHSAVL

      CHARACTER*1    KHAR,KWAL
      INTEGER        KHPOS,LY,LM,LD,LH,LN,LS,III
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh2tya.f,v $
     . $',                                                             '
     .$Id: sh2tya.f,v 1.3 2000/03/14 14:15:17 page Exp $
     . $' /
C    ===================================================================
C

            CALL SHSAVL('G',III,LY,LM,LD,LH,LN,LS)
            IF     (KHAR .EQ. 'T') THEN
              CALL SH3DT1(KHAR,KHPOS,7,LY,LM,LD,LH,LN,LS)
              CALL SHSAVM('I',III,III,III,III,III,III,III,III)
            ELSEIF (KHAR .EQ. 'Y') THEN
              CALL SH3DT1(KHAR,KHPOS,6,LY,LM,LD,LH,LN,LS)
              CALL SHSAVM('I',III,III,III,III,III,III,III,III)
            ELSEIF (KHAR .EQ. 'M') THEN
              CALL SH3DT1(KHAR,KHPOS,5,LY,LM,LD,LH,LN,LS)
              CALL SHSAVM('I',III,III,III,III,III,III,III,III)
            ELSEIF (KHAR .EQ. 'D') THEN
              CALL SH3DT1(KHAR,KHPOS,4,LY,LM,LD,LH,LN,LS)
              CALL SHSAVM('I',III,III,III,III,III,III,III,III)
            ELSEIF (KHAR .EQ. 'H') THEN
              CALL SH3DT1(KHAR,KHPOS,3,LY,LM,LD,LH,LN,LS)
              CALL SHSAVM('I',III,III,III,III,III,III,III,III)
            ELSEIF (KHAR .EQ. 'N') THEN
              CALL SH3DT1(KHAR,KHPOS,2,LY,LM,LD,LH,LN,LS)
              CALL SHSAVM('I',III,III,III,III,III,III,III,III)
            ELSEIF (KHAR .EQ. 'S') THEN
              CALL SH3DT1(KHAR,KHPOS,1,LY,LM,LD,LH,LN,LS)
              CALL SHSAVM('I',III,III,III,III,III,III,III,III)
            ELSEIF (KHAR .EQ. 'J') THEN
              CALL SH3DT2(KHAR,KHPOS,LY,LM,LD)
              CALL SHSAVM('I',III,III,III,III,III,III,III,III)
            ELSEIF (KHAR .EQ. 'C') THEN
              CALL SH3DT3(KHAR,KHPOS,8,LY,LM,LD)
            ELSEIF (KHAR .EQ. 'Q') THEN
              CALL SHGETK(KHAR,KHPOS)
              CALL SH3DT4(KHAR,KHPOS,KWAL)
              IF (KWAL.EQ.' ' .OR. KWAL.EQ.'-')
     $         CALL SHERR('E',85,KHPOS,KHAR)
            ELSEIF (KHAR .EQ. 'U') THEN
              CALL SH3DT5(KHAR,KHPOS)
            ELSEIF (KHAR .EQ. 'V') THEN
              CALL SH3DT6(KHAR,KHPOS)
            ELSEIF (KHAR .EQ. 'R') THEN
              CALL SHSAVM('I',III,III,III,III,III,III,III,III)
              CALL SH3DT8(KHAR,KHPOS)
            ELSE
              CALL SHERR('E',20,KHPOS,KHAR)
            ENDIF
            CALL SHSAVL('P',III,LY,LM,LD,LH,LN,LS)

      RETURN
      END
