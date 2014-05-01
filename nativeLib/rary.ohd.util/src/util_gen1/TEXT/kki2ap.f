C MODULE KKI2AP
C  =====================================================================
C  pgm: KKI2AP .. Convert integer into a string, output length
C
C  cmt:   VARIABLES:
C  cmt:      NUMBR - INTEGER VALUE TO CONVERT
C  cmt:      KSTR  - OUTPUT STRING
C  cmt:      LKSTR - CHARACTER LENGTH
C  =====================================================================
      SUBROUTINE KKI2AP(NUMBR,KSTR,LKSTR)

      INTRINSIC      LEN
      INTEGER        LEN

      CHARACTER*(*)  KSTR
      CHARACTER*10   KHRSTR
      CHARACTER*10   KHR
      CHARACTER*1    KHTEMP
      INTEGER        NUMBR,LKSTR,LENKH,IX,IY,IZ
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/kki2ap.f,v $
     . $',                                                             '
     .$Id: kki2ap.f,v 1.1 2001/06/13 09:17:04 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA   KHR / '0123456789' /

        IF (NUMBR .LE. 0) THEN
          KSTR  = '0'
          LKSTR = 1

        ELSE
          KHRSTR = ' '
          IX = NUMBR
          LENKH = 0
  100     IF (IX.LE.0 .OR. LENKH.GE.10) GOTO 120
            LENKH = LENKH+1
            IY = IX/10
            IZ = IX - 10*IY + 1
            IX = IY
            KHRSTR(LENKH:LENKH) = KHR(IZ:IZ)
            GOTO 100
  120     CONTINUE

          IX = 1
          IY = LENKH
  140     IF (IX .GE. IY) GOTO 160
            KHTEMP = KHRSTR(IX:IX)
            KHRSTR(IX:IX) = KHRSTR(IY:IY)
            KHRSTR(IY:IY) = KHTEMP
            IX = IX+1
            IY = IY-1
            GOTO 140
  160     CONTINUE

          IX = LEN(KSTR)
          IF (IX .LT. LENKH) LENKH = IX

          KSTR  = KHRSTR
          LKSTR = LENKH

        ENDIF

      RETURN
      END
