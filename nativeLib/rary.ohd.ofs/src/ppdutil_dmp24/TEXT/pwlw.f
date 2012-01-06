C MODULE PWLW
C-----------------------------------------------------------------------
C
      SUBROUTINE PWLW (INTG,LW)
C
C  PGM: PWLW(INTG,LW) .. PUT LEFT TWO BYTES OF INTG INTO RIGHT 2 OF LW
C
C   IN: INTG .... 4-BYTE INPUT WORD (FIRST 2 BYTES TO BE USED) - INT
C  OUT: LW ...... LEFT SHORT-WORD OF INTG (0 IN LEFT 2 BYTES) - INT
C
      CHARACTER*1  INTG(4),LW(4),CI,CJ,LZERO
      INTEGER      IZERO
      EQUIVALENCE( IZERO,LZERO )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmp24/RCS/pwlw.f,v $
     . $',                                                             '
     .$Id: pwlw.f,v 1.3 2002/02/11 20:47:40 dws Exp $
     . $' /
C    ===================================================================
C
      DATA         IZERO / 0 /
C
C
      CI = INTG(1)
      CJ = INTG(2)
      LW(1) = LZERO
      LW(2) = LZERO
      LW(3) = CI
      LW(4) = CJ
C
      RETURN
C
      END
