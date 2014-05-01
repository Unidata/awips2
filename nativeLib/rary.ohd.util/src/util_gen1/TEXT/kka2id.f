C  =====================================================================
C  pgm: KKA2ID .. Convert given text word to an integer, use default
C
C  use:     CALL KKA2ID(WORD,LWORD,INTGR,IDEFLT)
C
C   in: WORD ..... character string to be converted to an int - CHAR*(*)
C   in: LWORD .... number of characters in the input string - INT
C  out: INTGR .... output integer value, else set to IDEFLT if a
C  out:            bad character is found - INT
C   in: IDEFLT ... default integer for bad input (note if LWORD <= 0,
C   in:            INTGR is left unchanged), IDEFLT can be the same
C   in:            variable as INTGR if a bad string should result in
C   in:            not changing the original value of INTGR - INT
C
C  cmt: Rules: leading blanks are skipped
C  cmt:        + and - signs are considered
C  cmt:        if LWORD is zero, skip routine and INTGR is unchanged
C  cmt:        if a bad character is encountered, set INTGR to IDEFLT
C  cmt:        trailing blanks are bad characters
C  =====================================================================
      SUBROUTINE KKA2ID(WORD,LWORD,INTGR,IDEFLT)

      INTRINSIC       LEN
      INTEGER         LEN

      CHARACTER*(*)   WORD
      CHARACTER*1     KHAR,KLIST(10)
      INTEGER         LWORD,INTGR,IDEFLT,MAXW,NEG,NUM,LIMIT,IHN
      INTEGER         II,JJ,KK,LL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/kka2id.f,v $
     . $',                                                             '
     .$Id: kka2id.f,v 1.1 2001/06/13 09:02:53 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA    KLIST / '0','1','2','3','4','5','6','7','8','9' /

        IF (LWORD .GT. 0) THEN
          MAXW  = LEN(WORD)
          LIMIT = LWORD
          IF (LIMIT .GT. MAXW) LIMIT = MAXW

          NEG = 1
          II  = 0
          JJ  = LIMIT
  100     IF (II .GE. JJ) GOTO 120
            II = II+1
            KHAR = WORD(II:II)
            IF (KHAR .EQ. '+') THEN
              JJ = 0
              II = II+1
            ELSEIF (KHAR .EQ. '-') THEN
              JJ = 0
              II = II+1
              NEG = -1
            ELSEIF (KHAR .NE. ' ') THEN
              JJ = 0
            ENDIF
            GOTO 100
  120     CONTINUE

          NUM = 0
          IHN = 0
          JJ  = LIMIT
  130     IF (II .GT. JJ) GOTO 160
            KHAR = WORD(II:II)
            KK = 1
            LL = 10
  140       IF (KK .GT. LL) GOTO 150
              IF (KHAR .EQ. KLIST(KK)) THEN
                LL  = 0
                NUM = 10*NUM + (KK-1)
                IHN = 1
              ENDIF
              KK = KK+1
              GOTO 140
  150       CONTINUE
            IF (LL .NE. 0) THEN
              JJ = 0
            ENDIF
            II = II+1
            GOTO 130
  160     CONTINUE

          IF (JJ.NE.0 .AND. IHN.EQ.1) THEN
            INTGR = NUM
            IF (NEG .LT. 0) INTGR = -INTGR
          ELSE
            INTGR = IDEFLT
          ENDIF
        ENDIF

      RETURN
      END
