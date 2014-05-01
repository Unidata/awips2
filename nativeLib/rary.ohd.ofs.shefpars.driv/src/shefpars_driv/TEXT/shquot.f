C  =====================================================================
C  pgm: SHQUOT .. Get quote associated with a value from next chars
C
C  use:     CALL SHQUOT(KHAR,KHPOS,QUO)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: QUO ....... string of chars betwee quotes if found - CHAR*(*)
C
C  rqd: SHGETK
C  =====================================================================
      SUBROUTINE SHQUOT(KHAR,KHPOS,QUO)

      INTRINSIC          LEN
      EXTERNAL           SH2BLA,SHGETK,SH2SKP,SHERR
 
      CHARACTER*1        KHAR,NQKH
      CHARACTER*(*)      QUO
      INTEGER            KHPOS,NUM,NOFBL,LEN,MX
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shquot.f,v $
     . $',                                                             '
     .$Id: shquot.f,v 1.1 1997/12/31 20:34:47 page Exp $
     . $' /
C    ===================================================================
C

        QUO  = ' '

        IF (KHPOS .GT. 2) THEN

          CALL SH2BLA(KHAR,KHPOS,NOFBL)
          IF (KHPOS .GT. 2) THEN
            IF (KHAR.EQ.'"' .OR. KHAR.EQ.'''') THEN
              NQKH = KHAR
              CALL SHGETK(KHAR,KHPOS)
              CALL SH2BLA(KHAR,KHPOS,NOFBL)
              MX  = LEN(QUO)+1
              NUM = 0
  100         IF (KHPOS.LE.2 .OR. KHAR.EQ.NQKH) GOTO 110
                NUM = NUM+1
                IF (NUM .LT. MX) QUO(NUM:NUM) = KHAR
                IF (NUM .EQ. MX) CALL SHERR('W',81,KHPOS,KHAR)
                CALL SHGETK(KHAR,KHPOS)
                  GOTO 100
  110         CONTINUE
              IF (KHAR .EQ. NQKH) CALL SHGETK(KHAR,KHPOS)
            ENDIF
          ENDIF

        ENDIF

      RETURN
      END
