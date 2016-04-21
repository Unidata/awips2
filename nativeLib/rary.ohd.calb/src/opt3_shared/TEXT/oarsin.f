C     MEMBER OARSIN
C
      SUBROUTINE OARSIN
C
C.......................................
C     THIS SUBROUTINE READS AND PRINTS ADAPTIVE RANDOM SEARCH OPTIONS.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY
C            L. E. BRAZIL - HRL   FEB 1988   VERSION 1
C.......................................
C
      DIMENSION OLDOPN(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'ocommon/opschm'
      INCLUDE 'ocommon/opars'
      INCLUDE 'ocommon/odrop'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/oarsin.f,v $
     . $',                                                             '
     .$Id: oarsin.f,v 1.2 1996/07/11 20:46:01 dws Exp $
     . $' /
C    ===================================================================
C
C
C     TRACE LEVEL=1
      IF(ITRACE.GE.1) WRITE(IODBUG,1000)
 1000 FORMAT(1H0,17H** OARSIN ENTERED)
C
      CALL FSTWHR('OARSIN  ',0,OLDOPN,IOLDOP)
C
C  READ CARD FOR ADAPTIVE RANDOM SEARCH OPTIONS
C
      READ(IN,800) IF1,IF3,IF4,IF5,ISEED,IPFLAG
  800 FORMAT(6I5)
C
C  PRINT RANDOM SEARCH OPTIONS
C
  102 CONTINUE
  104 WRITE(IPR,910)
  910 FORMAT(1H0,//,38X,'ADAPTIVE RANDOM SEARCH OPTIMIZATION SCHEME',/,
     *38X,42(1H=),///,20X,'NO. RANGE',7X,'MAX TRIALS',7X,'NO. LOCAL',7X,
     *'NO. SUCCESSES',7X,'RANDOM',/,21X,'LEVELS',9X,'PER LEVEL',9X,
     *'TRIALS',11X,'TO STOP',12X,'SEED',/,20X,9(1H-),7X,10(1H-),7X,
     *9(1H-),7X,13(1H-),7X,6(1H-))
C
      PCENTA=PCENTO*100.
C
      WRITE(IPR,912) IF1,IF3,IF4,IF5,ISEED
C
  912 FORMAT(1H0,22X,I2,14X,I3,14X,I3,14X,I3,13X,I5)
C
      CALL FSTWHR(OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,1002)
 1002 FORMAT(1H0,14H** EXIT OARSIN)
C
      RETURN
      END
