C  =====================================================================
C  pgm: SHDBLE .. Get double precision number from next chars
C
C  use:     CALL SHDBLE(KHAR,KHPOS,VALUE,NDIG,NDEC)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: VALUE ..... double precision number found, else 0.0 - DBLE PRCN
C  out: NDIG ...... number of digits found, else 0 (1 for M,T) - INT
C  out:             (set to -1 if bad data warning, but continue line)
C  out: NDEC ...... number of decimal chars found (0 or 1) - INT
C
C  rqd: SHGETK,SH2BLA,SH2SKP,SHERR,SH2NXD
C
C  cmt: Note, maximum number of digits before or after decimal is 10 and
C  cmt:  limited to the size of an integer.
C  =====================================================================
      SUBROUTINE SHDBLE(KHAR,KHPOS,VALUE,NDIG,NDEC)

      EXTERNAL           SH2BLA,SHGETK,SH2SKP,SHERR,SH2NXD

      INTEGER            ICHAR
      INTRINSIC          ICHAR

      DOUBLE PRECISION   DBLE
      INTRINSIC          DBLE

      CHARACTER*1        KHAR
      INTEGER            KHPOS,NDIG,NDEC
      INTEGER            NOFBL,MX,NSUM1,NSUM2,NCH1,NCH2,ISIGN,IZERO
      DOUBLE PRECISION   VALUE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shdble.f,v $
     . $',                                                             '
     .$Id: shdble.f,v 1.5 2001/06/13 14:11:51 dws Exp $
     . $' /
C    ===================================================================
C

      VALUE = 0D0
      NDIG  = 0
      NDEC  = 0

      IF (KHPOS .GT. 2) THEN

        CALL SH2BLA(KHAR,KHPOS,NOFBL)

        IF (KHPOS .GT. 2) THEN
          IF (KHAR.EQ.'M' .OR. KHAR.EQ.'m') THEN
            VALUE = -9999D0
            NDIG  = 1
            CALL SHGETK(KHAR,KHPOS)
CCC         CALL SH2BLA(KHAR,KHPOS,NOFBL)
          ELSEIF (KHAR.EQ.'T' .OR. KHAR.EQ.'t') THEN
            VALUE = -9D10
            NDIG  = 1
            CALL SHGETK(KHAR,KHPOS)
CCC         CALL SH2BLA(KHAR,KHPOS,NOFBL)
          ELSEIF (KHAR .EQ. '?') THEN
            VALUE = -9999D0
CC          NDIG  = 1
            NDIG  = 0
            CALL SHERR('E',53,KHPOS,KHAR)
CC          CALL SHERR('W',53,KHPOS,KHAR)
            CALL SH2SKP(KHAR,KHPOS,'?')
CCC         CALL SH2BLA(KHAR,KHPOS,NOFBL)
          ELSE
            ISIGN = 0
            IF (KHAR.EQ.'-') ISIGN = 1
            IF (KHAR.EQ.'+') ISIGN = 2
            IF (KHAR.EQ.'-' .OR. KHAR.EQ.'+') THEN
              CALL SHGETK(KHAR,KHPOS)
CCC           CALL SH2BLA(KHAR,KHPOS,NOFBL)
            ENDIF

            IZERO = ICHAR('0')
            MX    = 10
            NSUM1 = 0
            NSUM2 = 0
            NCH1  = 0
            NCH2  = 0
   40       IF (NCH1.GE.MX .OR. KHAR.LT.'0' .OR. KHAR.GT.'9') GOTO 50
              NCH1  = NCH1+1
              NSUM1 = 10*NSUM1 + (ICHAR(KHAR)-IZERO)
              CALL SHGETK(KHAR,KHPOS)
              GOTO 40
   50       CONTINUE

            IF (KHAR .EQ. '.') THEN
              NDEC = 1
              CALL SHGETK(KHAR,KHPOS)
   60         IF (NCH2.GE.MX .OR. KHAR.LT.'0' .OR. KHAR.GT.'9') GOTO 70
                NCH2  = NCH2+1
                NSUM2 = 10*NSUM2 + (ICHAR(KHAR)-IZERO)
                CALL SHGETK(KHAR,KHPOS)
                GOTO 60
   70         CONTINUE
            ENDIF

            NDIG = NCH1+NCH2

C                   If no digits but have sign, allow for mult signs

            IF (NDIG.EQ.0 .AND. ISIGN.GT.0) THEN
              IF (ISIGN .EQ. 1) CALL SH2SKP(KHAR,KHPOS,'-')
              IF (ISIGN .EQ. 2) CALL SH2SKP(KHAR,KHPOS,'+')
            ENDIF

C                   If have digit or sign, next char cannot be weird

            IF (NDIG.NE.0 .OR. ISIGN.NE.0 .OR. NDEC.NE.0) THEN
              IF (KHPOS.GT.2  .AND. KHAR.NE.' ' .AND. KHAR.NE.'/'  .AND.
     $            KHAR.NE.',' .AND. KHAR.NE.'"' .AND. KHAR.NE.'''' .AND.
     $            (KHAR.LT.'A' .OR. KHAR.GT.'Z') .AND.
     $            (KHAR.LT.'a' .OR. KHAR.GT.'z') ) THEN
                CALL SHERR('A',78,KHPOS,KHAR)
                NDIG  = -1
                ISIGN =  0
                NDEC  =  0

                CALL SH2NXD(KHAR,KHPOS)
              ENDIF
            ENDIF

            IF (NDIG .GT. 0) THEN
              IF (NCH2 .EQ. 0) THEN
                VALUE = DBLE(NSUM1)
              ELSEIF (NCH2 .EQ. 1) THEN
                VALUE = DBLE(NSUM1) + (DBLE(NSUM2)/10D0)
              ELSEIF (NCH2 .EQ. 2) THEN
                VALUE = DBLE(NSUM1) + (DBLE(NSUM2)/100D0)
              ELSEIF (NCH2 .EQ. 3) THEN
                VALUE = DBLE(NSUM1) + (DBLE(NSUM2)/1000D0)
              ELSE
                VALUE = DBLE(NSUM1) + (DBLE(NSUM2)/(10D0**NCH2))
              ENDIF
              IF (ISIGN .EQ. 1) VALUE = -VALUE
            ENDIF

            IF (NDIG .EQ. 0) THEN
              IF (ISIGN .GT. 0) THEN
                VALUE = -9999D0
                NDIG  = 1
CCC             CALL SH2SKP(KHAR,KHPOS,'+')
              ELSEIF (NDEC .EQ. 1) THEN
                IF (KHPOS.LE.2 .OR. KHAR.EQ.' ') THEN
                  VALUE = -9999D0
                  NDIG  = 1
                ELSEIF (KHAR.EQ.'/' .OR. KHAR.EQ.',') THEN
                  VALUE = -9999D0
                  NDIG  = 1
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF

      ENDIF

      RETURN
      END
