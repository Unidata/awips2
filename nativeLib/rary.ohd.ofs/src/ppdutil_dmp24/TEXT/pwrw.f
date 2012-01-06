C MODULE PWRW
C-----------------------------------------------------------------------
C
C  PGM: PWRW(INTG,RW) .. PUT RIGHT TWO BYTES OF INTG INTO RIGHT 2 OF RW
C
C   IN: INTG .... 4-BYTE INPUT WORD (LAST 2 BYTES TO BE USED) - INT
C  OUT: RW ...... RIGHT SHORT-WORD OF INTG (0 IN LEFT 2 BYTES) - INT
C  =====================================================================
C
      SUBROUTINE PWRW (INTG,RW)
C
      CHARACTER*1    INTG(4),RW(4),CI,CJ,LZERO
      INTEGER      IZERO
      EQUIVALENCE( IZERO,LZERO )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmp24/RCS/pwrw.f,v $
     . $',                                                             '
     .$Id: pwrw.f,v 1.2 2002/02/11 20:48:45 dws Exp $
     . $' /
C    ===================================================================
C
      DATA         IZERO / 0 /
C
C
C
        CI = INTG(3)
        CJ = INTG(4)
        RW(1) = LZERO
        RW(2) = LZERO
        RW(3) = CI
        RW(4) = CJ
C
C
C
      RETURN
      END
