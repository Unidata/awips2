C MEMBER STRC21
C  (from old member FCEX21)
C=======================================================================
      SUBROUTINE STRC21(PO,LRAT,YQD,QYQD,NUMRCP,KD,KDD,NRC)
      INCLUDE 'common/fratng'
      INCLUDE 'common/fdbug'
      COMMON/IONUM/IN,IPR,IPU
      DIMENSION PO(1),YQD(1),QYQD(1),NUMRCP(1),KD(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/strc21.f,v $
     . $',                                                             '
     .$Id: strc21.f,v 1.1 1996/01/17 21:49:55 page Exp $
     . $' /
C    ===================================================================
C
C
C     WRITE(IPR,777) IODBUG
C 777 FORMAT(//3X,25(1H=),' IODBUG=',I2,1X,25(1H=))
      KDD=KD(1)
      LORC=LRAT
      LYQD=0
      LQQD=0
C     WRITE(IODBUG,899) NRC,KDD,LORC
C 899 FORMAT(/5X,'NRC=',I3,2X,'KDD=',I1,2X,'LORC=',I5)
      DO 41 K=1,NRC
      CALL FGETRC(PO(LORC),ISW)
      NUMRCP(K)=NRCPTS
C     WRITE(IODBUG,900) K,NUMRCP(K)
C 900 FORMAT(/5X,'RATING CURVE NO. ',I3,3X,I3,' POINTS')
      IF(GZERO.LE.-999.) GZERO=0.
      DO 39 L=1,NRCPTS
      YQD(LYQD+L)=XRC(LOCH+L-1)+GZERO
      QYQD(LQQD+L)=XRC(LOCQ+L-1)
   39 CONTINUE
C     WRITE(IODBUG,902) (YQD(LYQD+L),L=1,NRCPTS)
C 902 FORMAT(3X,'YQD=',10F10.2)
C     WRITE(IODBUG,904) (QYQD(LQQD+L),L=1,NRCPTS)
C 904 FORMAT(3X,'QYQD=',10F10.2)
      LQQD=LQQD+NRCPTS
      LYQD=LYQD+NRCPTS
      LORC=LORC+2
   41 CONTINUE
      RETURN
      END
