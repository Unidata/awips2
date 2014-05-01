C MEMBER PUC15
C  (from old member FCPUC15)
C
      SUBROUTINE PUC15(PO)
C
C             THE FUNCTION OF THIS SUBROUTINE IS TO CARD PUNCH THE DATA
C              INPUT FOR THE THE WEIGH-TS OPERATION
C
C             THIS SUBROUTINE WAS WRITTEN BY:
C              JAN LEWIS        HRL     MARCH, 1980        VERSION NO. 1
C
      INCLUDE 'common/ionum'
C
      DIMENSION PO(1),SWITCH(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc15.f,v $
     . $',                                                             '
     .$Id: puc15.f,v 1.1 1995/09/17 18:50:38 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SWITCH/4H    ,4HAREA/
C
      NTS=PO(2)
      WTS=PO(3)
      IDT=PO(7)
C
      WRITE(IPU,100) NTS,WTS,(PO(K),K=4,6),IDT
      K=11
      DO 10 I=1,NTS
      IF(WTS.EQ.SWITCH(1)) WRITE(IPU,200) PO(K),PO(K+1),PO(K+2),PO(K+3)
      IF(WTS.EQ.SWITCH(2)) WRITE(IPU,300) PO(K),PO(K+1),PO(K+2),PO(K+4),
     1 PO(K+5)
      IF(WTS.EQ.SWITCH(1)) K=K+4
      IF(WTS.EQ.SWITCH(2)) K=K+6
   10 CONTINUE
  100 FORMAT(I5,1X,A4,2X,2A4,1X,A4,I5)
  200 FORMAT(2A4,2X,A4,1X,F10.3)
  300 FORMAT(2A4,2X,A4,1X,2A4)
      RETURN
      END
