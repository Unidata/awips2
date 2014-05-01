C
      SUBROUTINE PRP56(PO)
C..........................................................
C     PARAMETER PRINT SUBROUTINE FOR THE GLACIER OPERATION
C..........................................................
      DIMENSION PO(*)
C     COMMON BLOCKS
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp56.f,v $
     . $',                                                             '
     .$Id: prp56.f,v 1.2 2003/08/15 13:17:28 gzhou Exp $
     . $' /
C    ===================================================================
C
C..........................................................
C     TRACE LEVEL=1 NO DEBUG
      IF (ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,'** PRP56 ENTERED')
C..........................................................
C     CONTROL VARIABLES
      IDT=PO(8)
      IVER=PO(1)
C     PRINT TITLE
      WRITE(IPR,901)IVER
  901 FORMAT(1H0,10X,'GLACIER OPERATION FROM MOORE(1993 VERSION-)',I2)
      WRITE(IPR,902)
  902 FORMAT(1H0,/10X,'TIME SERIES USED')
C     PRINT TIME SERIES
      WRITE(IPR,903) (PO(I),I=2,4),IDT
  903 FORMAT(1H0,10X,'RAIN+MELT',10X,2A4,3X,A4,5X,I2,1X,'HOURS')
      WRITE(IPR,904) (PO(I),I=5,7),IDT
  904 FORMAT(1H0,10X,'GLACIER OUT',10X,2A4,3X,A4,5X,I2,1X,'HOURS')
      WRITE(6,905)
      IF (IVER.EQ.2) WRITE(IPR,909) (PO(I),I=14,16),IDT
  909 FORMAT(1H0,10X,'FAFI OUT',10X,2A4,3X,A4,5X,I2,1X,'HOURS')
  905 FORMAT(1H0,/10X,'PARAMETERS')
      WRITE(IPR,906) (PO(I),I=9,13)
  906 FORMAT(1H0,10X,'CG1',2X,F6.1,2X,'CG2',2X,F4.2,2X,'CG3',2X,
     &F4.2,2X,'KG1',2X,F4.2,2X,'KG2',2X,F4.2)
      IF(ITRACE.GE.1) WRITE(IODBUG,908)
  908 FORMAT(1H0,/,'** EXIT PRP56')
      RETURN
      END
