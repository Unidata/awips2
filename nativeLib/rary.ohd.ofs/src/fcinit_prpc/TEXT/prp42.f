C MEMBER PRP42
C  (from old member FCPRP42)
C
      SUBROUTINE PRP42(PO)
C.......................................
C    PARAMETER PRINT SUBROUTINE FOR THE 'RSNWELEV' OPERATION
C.......................................
C    INITIALLY WRITTEN BY ERIC ANDERSON - HRL DEC. 1991
C.......................................
      DIMENSION PO(1)
C    COMMON BLOCKS
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp42.f,v $
     . $',                                                             '
     .$Id: prp42.f,v 1.1 1995/09/17 18:50:21 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C    TRACE LEVEL=1 NO DEBUG
      IF (ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,'** PRP42 ENTERED')
C.......................................
C    CONTROL VARAIABLES
      IDT=PO(2)
      IZIN=PO(12)
C.......................................
C    PRINT TITLE
      WRITE(IPR,901) PO(8),PO(7)
  901 FORMAT(1H0,10X,'RAIN-SNOW ELEVATION OPERATION--PXTEMP=',
     -F4.1,1X,'DEGC',5X,'LAPSE RATE=',F5.2,1X,'DEGC/100 M')
      WRITE(IPR,902)
  902 FORMAT(1H0,20X,'TIME SERIES USED',//16X,'CONTENTS',14X,
     -'I.D.',5X,'TYPE',3X,'TIME INTERVAL',5X,'OTHER')
C.......................................
C    PRINT TIME SERIES
      WRITE(IPR,904) (PO(I),I=9,11),IDT
  904 FORMAT(1H0,10X,'RAIN-SNOW ELEVATION',6X,2A4,3X,A4,5X,I2,1X,
     -'HOURS')
      WRITE(IPR,903) (PO(I),I=3,5),IDT,PO(6)
  903 FORMAT(1H ,10X,'AIR TEMPERATURE',10X,2A4,3X,A4,5X,I2,1X,'HOURS',
     -5X,'ELEV=',F6.0,1X,'M')
      IF (IZIN.EQ.0) GO TO 190
      WRITE(IPR,905) (PO(I),I=13,15),IDT
  905 FORMAT(1H ,10X,'FREEZING LEVEL',11X,2A4,3X,A4,5X,I2,1X,'HOURS')
C.......................................
  190 IF (ITRACE.GE.1) WRITE(IODBUG,906)
  906 FORMAT(1H0,'** EXIT PRP42')
      RETURN
      END
