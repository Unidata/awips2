C  ============================================================================
      SUBROUTINE FMTNUM(IP,NUM,INFMT,OUTFMT)

      CHARACTER*4    INFMT,OUTFMT
      INTEGER        IP,NUM

      CHARACTER*4    KX,TMPFMT
      INTEGER        L1,L2,LX,TMPNUM
      INTEGER        L1SHFT(3),L2SHFT(3)

      EQUIVALENCE    (TMPNUM,TMPFMT),(LX,KX)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_lx/RCS/fmtnum_lx.f,v $
     . $',                                                             '
     .$Id: fmtnum_lx.f,v 1.2 2002/10/10 17:59:53 dws Exp $
     . $' /
C    ===================================================================
C

      DATA    L1SHFT /   1,   256,    65536 /
      DATA    L2SHFT / 256, 65536, 16777216 /

        IF (IP.GT.0 .AND. IP.LT.4 .AND. NUM.GE.0 .AND. NUM.LT.100) THEN

          TMPFMT = INFMT

          L1 = NUM/10
          L2 = MOD(NUM,10)
          LX = TMPNUM + L1*L1SHFT(IP) + L2*L2SHFT(IP)

          OUTFMT = KX

        ENDIF

      RETURN
      END
