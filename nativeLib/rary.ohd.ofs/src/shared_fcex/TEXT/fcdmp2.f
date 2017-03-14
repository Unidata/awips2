C MEMBER FCDMP2
C  (from old member FCFCDMP2)
C
C DESC 'DEBUGGING DUMP OF COMMON FCIOBF'
C.......................................................................
      SUBROUTINE FCDMP2(IR,NWORDS)
C
C  PRODUCES DEBUGGING DUMP OF COMMON BLOCK FCIOBF
C
C  ROUTINE ORIGINALLY WRITTEN BY --
C    ED JOHNSON -- HRL -- 7 NOV 1979
C.......................................................................
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fciobf'
      DIMENSION IZZBUF(1)
      EQUIVALENCE (IZZBUF(1),ZZZBUF(1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/fcdmp2.f,v $
     . $',                                                             '
     .$Id: fcdmp2.f,v 1.1 1995/09/17 19:19:33 dws Exp $
     . $' /
C    ===================================================================
C
      N=NWORDS
      IF(N.LT.1.OR.N.GT.MZZBUF)N=MZZBUF
      WRITE(IODBUG,900)IR,NWORDS,N,MZZBUF
 900  FORMAT(44H0DEBUGGIN PRINT OF CONTENTS OF COMMON FCIOBF,/,
     .  8H RECORD=,I6,1X,I6,18H WORDS REQUESTED, ,I6,
     .  16H DUMPED. MZZBUF=,I6)
 901  FORMAT(1H0)
 902  FORMAT(1H ,10F8.3)
 903  FORMAT(1H ,10I8)
 904  FORMAT(1H ,20A4)
      WRITE(IODBUG,901)
      WRITE(IODBUG,902)(ZZZBUF(I),I=1,N)
      WRITE(IODBUG,901)
      WRITE(IODBUG,903)(IZZBUF(I),I=1,N)
      WRITE(IODBUG,901)
      WRITE(IODBUG,904)(ZZZBUF(I),I=1,N)
      RETURN
      END
