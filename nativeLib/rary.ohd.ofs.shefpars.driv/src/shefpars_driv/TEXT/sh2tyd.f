C  =====================================================================
C  pgm: SH2TYD .. Data type and date codes for routine "shtypd" (.B fmt)
C
C  use:     CALL SH2TYD(KHAR,KHPOS,JC1,JC2,JC3,JC4,JC5,JC6)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: JC1 ....... if 1 then zulu date was updated - INT
C  out: JC2 ....... if 1 then creation date updated - INT
C  out: JC3 ....... if 1 then data qualifier code updated - INT
C  out: JC4 ....... if 1 then type of units code updated - INT
C  out: JC5 ....... if 1 then duration code updated - INT
C  out: JC6 ....... if 1 then date increments updated - INT
C
C  rqd: SHGETK,SH3DT1,SH3DT2,SH3DT3,SHERR,SH3DT4,SH3DT5,SH3DT6
C  rqd: SH3DT8,SHSAVM,SHSAVL
C
C  cmt: Keywords: DT,DY,DM,DD,DH,DN,DS,DJ,DC,DR,DQ,DU,DV
C  =====================================================================
      SUBROUTINE SH2TYD(KHAR,KHPOS,JC1,JC2,JC3,JC4,JC5,JC6)

      EXTERNAL       SHGETK,SH3DT1,SH3DT2,SH3DT3,SHERR
      EXTERNAL       SH3DT4,SH3DT5,SH3DT6,SH3DT8,SHSAVM,SHSAVL

      CHARACTER*1    KHAR,KWAL
      INTEGER        KHPOS,LY,LM,LD,LH,LN,LS,III
      INTEGER        JC1,JC2,JC3,JC4,JC5,JC6
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh2tyd.f,v $
     . $',                                                             '
     .$Id: sh2tyd.f,v 1.3 2000/03/14 14:15:44 page Exp $
     . $' /
C    ===================================================================
C

            CALL SHSAVL('G',III,LY,LM,LD,LH,LN,LS)
            IF     (KHAR .EQ. 'T') THEN
              CALL SH3DT1(KHAR,KHPOS,7,LY,LM,LD,LH,LN,LS)
              IF (KHPOS .NE. 1) JC1 = 1
            ELSEIF (KHAR .EQ. 'Y') THEN
              CALL SH3DT1(KHAR,KHPOS,6,LY,LM,LD,LH,LN,LS)
              IF (KHPOS .NE. 1) JC1 = 1
            ELSEIF (KHAR .EQ. 'M') THEN
              CALL SH3DT1(KHAR,KHPOS,5,LY,LM,LD,LH,LN,LS)
              IF (KHPOS .NE. 1) JC1 = 1
            ELSEIF (KHAR .EQ. 'D') THEN
              CALL SH3DT1(KHAR,KHPOS,4,LY,LM,LD,LH,LN,LS)
              IF (KHPOS .NE. 1) JC1 = 1
            ELSEIF (KHAR .EQ. 'H') THEN
              CALL SH3DT1(KHAR,KHPOS,3,LY,LM,LD,LH,LN,LS)
              IF (KHPOS .NE. 1) JC1 = 1
            ELSEIF (KHAR .EQ. 'N') THEN
              CALL SH3DT1(KHAR,KHPOS,2,LY,LM,LD,LH,LN,LS)
              IF (KHPOS .NE. 1) JC1 = 1
            ELSEIF (KHAR .EQ. 'S') THEN
              CALL SH3DT1(KHAR,KHPOS,1,LY,LM,LD,LH,LN,LS)
              IF (KHPOS .NE. 1) JC1 = 1
            ELSEIF (KHAR .EQ. 'J') THEN
              CALL SH3DT2(KHAR,KHPOS,LY,LM,LD)
              IF (KHPOS .NE. 1) JC1 = 1
            ELSEIF (KHAR .EQ. 'C') THEN
              CALL SH3DT3(KHAR,KHPOS,8,LY,LM,LD)
              IF (KHPOS .NE. 1) JC2 = 1
            ELSEIF (KHAR .EQ. 'Q') THEN
              CALL SHGETK(KHAR,KHPOS)
              CALL SH3DT4(KHAR,KHPOS,KWAL)
              IF (KWAL.EQ.' ' .OR. KWAL.EQ.'-')
     $         CALL SHERR('E',85,KHPOS,KHAR)
              IF (KHPOS .NE. 1) JC3 = 1
            ELSEIF (KHAR .EQ. 'U') THEN
              CALL SH3DT5(KHAR,KHPOS)
              IF (KHPOS .NE. 1) JC4 = 1
            ELSEIF (KHAR .EQ. 'V') THEN
              CALL SH3DT6(KHAR,KHPOS)
              IF (KHPOS .NE. 1) JC5 = 1
            ELSEIF (KHAR .EQ. 'R') THEN
              CALL SHSAVM('I',III,III,III,III,III,III,III,III)
              CALL SH3DT8(KHAR,KHPOS)
              IF (KHPOS .NE. 1) JC6 = 1
            ELSE
              CALL SHERR('E',20,KHPOS,KHAR)
            ENDIF
            CALL SHSAVL('P',III,LY,LM,LD,LH,LN,LS)

      RETURN
      END
