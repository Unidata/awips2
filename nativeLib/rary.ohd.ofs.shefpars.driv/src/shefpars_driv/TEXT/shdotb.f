C  =====================================================================
C  pgm: SHDOTB .. Decode the body of shef .B format messages
C
C  use:     CALL SHDOTB(KHAR,KHPOS,MREC,IREV,JKHID,QUO)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C   in: MREC ...... number of fields in the .B message discription - INT
C   in: IREV ...... revision code (0 = no, 1 = yes) - INT
C   in: JKHID ..... group name of .B message - CHAR*8
C   in: QUO ....... string for quotes, only carried thru this rtn in
C   in:             order to define its size in one place - CHAR*(*)
C
C  rqd: SH2BEG,SH2BLA,SH2SKP,SH2ST2,SH3ADD,SH3ADK,SH3ADT,SH3DEC,SH3DLM
C  rqd: SH3DT4,SHDBLE,SHERR,SHGETK,SHTYPD,SHSAVD,SHSAVI,SHSAVK,SHSAVL
C  rqd: SHSAVM,SHSAVP,SHSAVQ,SHOZA1,SHOUT,SHSAVA,SHSAVS,SHPROB,SHHRFX
C  rqd: SHQUOT,SH2NXD
C  =====================================================================
      SUBROUTINE SHDOTB(KHAR,KHPOS,MREC,IREV,JKHID,QUO)

      EXTERNAL       SH2BEG,SH2BLA,SH2SKP,SH2ST2,SH3ADD,SH3ADK,SH3ADT
      EXTERNAL       SH3DEC,SH3DLM,SH3DT4,SHDBLE,SHERR,SHGETK,SHTYPD
      EXTERNAL       SHSAVD,SHSAVI,SHSAVK,SHSAVL,SHSAVM,SHSAVP,SHPROB
      EXTERNAL       SHSAVQ,SHOZA1,SHOUT,SHSAVA,SHSAVS,SHHRFX,SHQUOT
      EXTERNAL       SH2NXD

      CHARACTER*1    KHAR,KWAL,LWAL,LCHAR,KKK
      CHARACTER*8    KHID,JKHID,PARCOD,PPP
      CHARACTER*(*)  QUO
      INTEGER        KHPOS,LNULL,IDUR,NDIG,NDEC,MREC,IREV,IDOTE
      INTEGER        K1,K2,K3,K4,K5,K6,N1,N2,N3,N4,N5,N6,III,IERR
      INTEGER        ITEM,JC1,JC2,JC3,JC4,JC5,JC6,NOC,NOFBL
      DOUBLE PRECISION   VALUE
      REAL               CODP
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shdotb.f,v $
     . $',                                                             '
     .$Id: shdotb.f,v 1.5 2000/03/14 14:20:41 page Exp $
     . $' /
C    ===================================================================
C

      DATA     IDOTE / 0 /

C                   Loop thru each sta per messg line or each messg line

   50 IF (KHPOS.LE.0 .OR. KHAR.EQ.'.') GOTO 100
        ITEM  = 0
        LCHAR = '/'
        JC1 = 0
        JC2 = 0
        JC3 = 0
        JC4 = 0
        JC5 = 0
        JC6 = 0

C                   Get sta id at messg beginning and at least 1 blank

        CALL SH2ST2(KHAR,KHPOS,NOC,KHID)
          IF (KHPOS.GT.2 .AND. NOC.LT.0) CALL SHERR('E',13,KHPOS,KHAR)
          IF (KHPOS.GT.2 .AND. NOC.LT.3) CALL SHERR('E',47,KHPOS,KHAR)
        CALL SH2BLA(KHAR,KHPOS,NOC)
          IF (KHPOS .GT. 2) THEN
           IF (KHAR.NE.'/' .AND. NOC.LE.0) CALL SHERR('E',14,KHPOS,KHAR)
          ENDIF

C                   Loop thru data items for current sta on messg line

   70 IF (KHPOS.LE.2 .OR. KHAR.EQ.',') GOTO 80

C                   See if too many items, else get next data headers

        IF (ITEM .GE. MREC) THEN
          IF (KHAR .EQ. '/') THEN
            CALL SHGETK(KHAR,KHPOS)
            CALL SH2BLA(KHAR,KHPOS,NOC)
          ELSE
            CALL SHERR('E',41,KHPOS,KHAR)
            KHPOS = 1
          ENDIF
        ELSE
          ITEM = ITEM + 1
          CALL SHSAVP('R',ITEM,PPP)
          CALL SHSAVA('R',ITEM,III)
          CALL SHSAVS('R',ITEM,III)
          CALL SHSAVN('R',ITEM,III)
          CALL SHSAVJ('R',ITEM,III)
          CALL SHSAVP('G',III,PARCOD)
          CALL SHPROB('GET_VALUE   ',KHAR,KHPOS,PARCOD,CODP)

          IF(JC1.EQ.0) CALL SHSAVL('R',ITEM,III,III,III,III,III,III)
          IF(JC2.EQ.0) CALL SHSAVK('R',ITEM,III,III,III,III,III,III)
          IF(JC3.EQ.0) CALL SHSAVQ('R',ITEM,KKK)
          IF(JC4.EQ.0) CALL SHSAVD('R',ITEM,III)
          IF(JC5.EQ.0) CALL SHSAVI('R',ITEM,III)
          IF(JC6.EQ.0) CALL SHSAVM('R',ITEM,III,III,III,III,III,III,III)

C                   Get date, data type elements - SHTYPD
C                   Adjust forecast date to zulu - SH3ADK
C                   Test for correct duration    - SH3ADD
C                   Get the real value           - SHDBLE
C                   Check for trailing qualifier - SH3DT4
C                   Check if decimal required    - SH3DEC
C                   Adjust for trace value       - SH3ADT
C                   Look for quote               - SHQUOT
C                   Check if nxt char is a delim - SH3DLM
C                   Output data to shefout file  - SHOZA1
C                   Get overall qualifier        - SHSAVQ
C                   Output data to shefout file  - SHOUT

          CALL SHTYPD(KHAR,KHPOS,JC1,JC2,JC3,JC4,JC5,JC6,LCHAR,LNULL)
          CALL SH3ADK(KHAR,KHPOS,K1,K2,K3,K4,K5,K6)
          CALL SH3ADD(KHAR,KHPOS,IDUR)

          NDIG = 0
          IF (LNULL .EQ. 0) THEN
            CALL SHDBLE(KHAR,KHPOS,VALUE,NDIG,NDEC)

            IF (NDIG .LE. 0) THEN
              IF (KHPOS .GT. 2) THEN
                CALL SH2BLA(KHAR,KHPOS,NOFBL)
                IF (KHAR.EQ.'''' .OR. KHAR.EQ.'"') THEN
                  CALL SHERR('A',86,KHPOS,KHAR)
                  CALL SHQUOT(KHAR,KHPOS,QUO)
                ELSE IF (KHAR.NE.'/' .AND. KHAR.NE.',') THEN
                  CALL SHERR('A',78,KHPOS,KHAR)
                  CALL SH2NXD(KHAR,KHPOS)
                  CALL SHQUOT(KHAR,KHPOS,QUO)
                ENDIF
              ENDIF

            ELSE
              CALL SH3DT4(KHAR,KHPOS,LWAL)
              CALL SHQUOT(KHAR,KHPOS,QUO)
              IF (LWAL .EQ. '-') THEN
                NDIG = -1

              ELSE
                CALL SH3DEC(KHAR,KHPOS,VALUE,NDEC)
                CALL SH3ADT(KHAR,KHPOS,VALUE)
                CALL SHQUOT(KHAR,KHPOS,QUO)
                CALL SH3DLM(KHAR,KHPOS,NDIG)
              ENDIF
            ENDIF
          ENDIF

          IF (NDIG .GT. 0) THEN
            CALL SHOZA1(KHAR,KHPOS,N1,N2,N3,N4,N5,N6)
            IF (KHPOS .NE. 1) THEN
              CALL SHSAVQ('G',III,KWAL)
CCC           CALL SHSAVP('G',III,PARCOD)

C        Constrain hour to 0-23:  Since the original shefpars allows an
C          hour of 0-24 in some places, a filter is placed here to move
C          a date that has an hour of 24 to the next day with an hour
C          of 1 (done 18 Sep 1997)

              CALL SHHRFX(N1,N2,N3,N4)
              CALL SHHRFX(K1,K2,K3,K4)

C        Output the decoded shef message

              CALL SHOUT(KHID,N1,N2,N3,N4,N5,N6,K1,K2,K3,K4,K5,K6,
     $                   PARCOD,IDUR,CODP,VALUE,KWAL,IREV,JKHID,IDOTE,
     $                   QUO,IERR)
              IF (IERR .NE. 0) CALL SHERR('E',70,KHPOS,KHAR)
            ENDIF
          ENDIF
        ENDIF

        CALL SH2BLA(KHAR,KHPOS,NOC)
        GOTO 70
   80 CONTINUE

        IF (KHPOS.NE.1 .AND. ITEM.LT.MREC) THEN
          IF (LCHAR.NE.'/' .OR. ITEM.NE.MREC-1) THEN
            CALL SHERR('W',42,KHPOS,KHAR)
          ENDIF
        ENDIF
        CALL SH2SKP(KHAR,KHPOS,',')
        CALL SH2BLA(KHAR,KHPOS,NOC)
        IF (KHPOS .LE. 2) CALL SH2BEG(KHAR,KHPOS)
        GOTO 50
  100 CONTINUE

      RETURN
      END
