C MEMBER PUC30
C  (FROM OLD MEMBER FCPUC30)
C-----------------------------------------------------------------------
C                             LAST UPDATE: 10/24/95.11:22:54 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE PUC30(PO)
C
C     THE FUNCTION OF THIS SUBROUTINE IS TO PUNCH THE DATA INPUT FOR
C     THE MERGE T.S. OPERATION.
C
C     THIS SUBROUTINE WAS WRITTEN BY:
C     ROBERT M. HARPER     HRL     NOVEMBER 1990     VERSION NO. 1
C
C     MODIFICATIONS BY:
C     BRYCE FINNERTY  HRL  MAY, 1995                 VERSION NO. 2
C
C**************************************
C
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
      DIMENSION PO(*),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc30.f,v $
     . $',                                                             '
     .$Id: puc30.f,v 1.2 1995/11/14 19:38:34 erb Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HPUC3,4H0   /
C
      NTS=PO(2)
      INTRVL=PO(6)
      ISWICH=PO(11)
C
      WRITE(IPU,1004) NTS,(PO(K),K=3,5),INTRVL,ISWICH
      K=13
      DO 10 I=1,NTS
        WRITE(IPU,1006) PO(K),PO(K+1),PO(K+2)
        K=K+4
   10 CONTINUE
 1004 FORMAT(I5,2X,2A4,1X,A4,I5,4X,I1)
 1006 FORMAT(2A4,3X,A4)
C
      IF(ITRACE .EQ. 1) WRITE(IODBUG,1007)SNAME
 1007 FORMAT(/,10X,'**',2A4,' EXITED',//)
      RETURN
      END
