C MEMBER PCICRM
C  (from old member PRDINCOR)
C-----------------------------------------------------------------------
C
      SUBROUTINE PCICRM (LENGTH,ISTAT)
C
C THIS ROUTINE CHECKS THE COUNTERS FOR INCORE TIME SERIES MANAGEMENT
C IT LOOKS FOR ROOM IN THE TS POINTER TABLE AND THE BUFFER ITSELF
C IF EITHER ONE IS FULL, IT WRITES A MESSAGE AND SETS STAT TO 1 OR 2
C
      INCLUDE 'uio'
      INCLUDE 'prdcommon/picptr'
      INCLUDE 'prdcommon/ptsicb'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/pcicrm.f,v $
     . $',                                                             '
     .$Id: pcicrm.f,v 1.1 1995/09/17 18:45:30 dws Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
      IF (NTSICP.LT.MTSICP) GO TO 10
      IF (ICBPRF.EQ.0) WRITE (LPE,2000) NTSICP
2000  FORMAT (' **NOTE** INCORE POINTER TABLE (TSICPT) FULL AT ',I4)
      ISTAT=1
10    IF (NTSICB+LENGTH.LE.MTSICB) GO TO 20
      IF (ICBPRF.EQ.0) WRITE (LPE,2001) LENGTH
2001  FORMAT (' **NOTE** INCORE BUFFER CANNOT FIT TIME SERIES OF ',I4,
     *  ' WORDS')
      ISTAT=2
20    CONTINUE
C
      RETURN
C
      END
