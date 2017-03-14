       SUBROUTINE PRC56(CO)
C..................................................................
C      CARRYOVER PRINT SUBROUTINE FOR THE 'GLACIER' OPERATION
C..................................................................
       DIMENSION CO(*)
C      COMMON BLOCKS
       INCLUDE 'common/ionum'
       INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc56.f,v $
     . $',                                                             '
     .$Id: prc56.f,v 1.1 1997/12/31 17:54:13 page Exp $
     . $' /
C    ===================================================================
C
C
C.................................................................
C      TRACE LEVEL=1, NO DEBUG
       IF(ITRACE.GE.1.) WRITE(IODBUG,900)
 900   FORMAT(1H0,'** PRC ENTERED')
C.................................................................
C      PRINT CARRYOVER
       WRITE(IPR,901)CO(1)
 901   FORMAT(1H0,10X,'PREVIOUS STORAGE= ',F6.1)
       WRITE(IPR,903)CO(2)
 903   FORMAT(1HO,10X,'PREVIOUS AFI= ',F6.1)
C.................................................................
       IF (ITRACE.GE.1) WRITE(IODBUG,902)
 902   FORMAT(1H0,'**EXIT PRC56')
       RETURN
       END
