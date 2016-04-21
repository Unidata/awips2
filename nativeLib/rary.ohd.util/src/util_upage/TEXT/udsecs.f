C MEMBER UDSECS
C-----------------------------------------------------------------------
C
C  ROUTINE TO COMPUTE NUMBER OF SECONDS BASED ON HOURS, MINUTES AND
C  SECONDS.
C
      SUBROUTINE UDSECS (NHMS,NSECS)
C
      INCLUDE 'ucmdbx'
      INCLUDE 'upagex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_upage/RCS/udsecs.f,v $
     . $',                                                             '
     .$Id: udsecs.f,v 1.1 1995/09/17 19:05:10 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      NHR=NHMS/10000
      NMIN=NHMS/100-NHR*100
      NSEC=NHMS-NMIN*100-NHR*10000
C
      NSECS=NHR*60*60+NMIN*60+NSEC
C
      IF (ICMTRC.GT.0) THEN
         NPSNLN(ICMPRU)=NPSNLN(ICMPRU)+1
         WRITE (ICMPRU,10) NHMS,NHR,NMIN,NSEC,NSECS
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** EXIT UDSECS : NHMS=',I6,3X,'NHR=',I2,3X,'NMIN=',I2,
     *   3X,'NSEC=',I2,3X,'NSECS=',I6)
C
      END
