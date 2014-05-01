      SUBROUTINE PUC56(PO,CO)
C............................................
C     PUNCH SUBROUTINE FOR GLACIER OPERATION
C
      DIMENSION PO(*),CO(*)
C     COMMON BLOCKS
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudflt'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc56.f,v $
     . $',                                                             '
     .$Id: puc56.f,v 1.3 2003/08/15 13:18:13 gzhou Exp $
     . $' /
C    ===================================================================
C
C............................................
C     TRACE LEVEL=1,NO DEBUG
      IF  (ITRACE.GE.1)WRITE(IODBUG,900)
 900  FORMAT(1H0,'** PUC56 ENTERED')
C............................................
C     CONTROL
      IVER=PO(1)
      IDT=PO(8)
C     PUNCH CARD 1
      IF(IVER.EQ.2) THEN
      WRITE(IPU,901) (PO(I),I=2,4),IDT,(PO(I),I=5,7),(PO(I),I=14,16)
      ELSE
      WRITE(IPU,902) (PO(I),I=2,4),IDT,(PO(I),I=5,7)
      ENDIF
 901  FORMAT(2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,2X,2A4,1X,A4)
 902  FORMAT(2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4)
C     PUNCH CARD 2
      WRITE(IPU,905) (PO(I),I=9,13)
 905  FORMAT(2X,F6.1,2X,F4.2,2X,F4.2,2X,F4.2,2X,F4.2)
C     PUNCH CARD 3
      IF(IPDFLT.EQ.1) GOTO 912
      WRITE(IPU,910) (CO(I),I=1,2)
 910  FORMAT(2X,F6.1,2X,F6.1)
C..............................................
 912  IF (ITRACE.GE.1) WRITE(IODBUG,915)
 915  FORMAT(1H0,'** EXIT PUC56')
      RETURN
      END



