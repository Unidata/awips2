C MEMBER SUSECS
C-----------------------------------------------------------------------
C
C  DESC  COMPUTE NUMBER OF SECONDS BASED ON HOURS, MINUTES AND SECONDS
C
      SUBROUTINE SUSECS (NHMS,NSECS)
C
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/susecs.f,v $
     . $',                                                             '
     .$Id: susecs.f,v 1.1 1995/09/17 19:03:02 dws Exp $
     . $' /
C    ===================================================================
C
C
      NHR=NHMS/10000
      NMIN=NHMS/100-NHR*100
      NSEC=NHMS-NMIN*100-NHR*10000
C
      NSECS=NHR*60*60+NMIN*60+NSEC
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,10) NHMS,NHR,NMIN,NSEC,NSECS
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
10    FORMAT (' EXIT SUSECS : NHMS=',I6,3X,'NHR=',I2,3X,'NMIN=',I2,3X,
     *   'NSEC=',I2,3X,'NSECS=',I6)
C
      RETURN
C
      END
