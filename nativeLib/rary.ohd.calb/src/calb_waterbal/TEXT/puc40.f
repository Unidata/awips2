C MEMBER PUC40
C  (from old member MCPUC40)
C
      SUBROUTINE PUC40(PO)
C***************************************
C
C     THE FUNCTION OF THIS SUBROUTINE IS TO PUNCH THE DATA INPUT FOR THE
C     WATER BALANCE OPERATION.
C
C     THIS SUBROUTINE WAS INITIALLY WRITTEN BY:
C     ROBERT M. HARPER       HRL       MAY 1991
C***************************************
C
      DIMENSION PO(1),SNAME(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_waterbal/RCS/puc40.f,v $
     . $',                                                             '
     .$Id: puc40.f,v 1.3 2003/08/26 17:46:35 wkwock Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/'PUC3','0   '/
C***************************************
C
C     CONTROL VARIABLES
      NOSUB=PO(14)
C***************************************
C
      WRITE(IPU,910) (PO(J),J=2,6)
ckwz      WRITE(IPU,920) (PO(J),J=7,15),PO(17)
ckwz r24--po(14) is an integer, print a real with I5 will core in linux
      WRITE(IPU,920) (PO(J),J=7,13),NOSUB,PO(15),PO(17)
      DO 10 N=1,NOSUB
        I1=23+(N-1)*23
        I9=I1+9
        I11=I1+11
        I14=I1+14
        WRITE(IPU,930) (PO(J),J=I1,I9),(PO(J),J=I11,I14)
   10 CONTINUE
C***************************************
C
  910 FORMAT(5A4)
  920 FORMAT(2A4,2X,A4,2X,2A4,2X,A4,2X,F10.3,2X,I5,2X,A4,2X,A4)
  930 FORMAT(5A4,2X,F5.3,2X,2A4,2X,2A4,2X,2A4,2X,2A4)
C
      RETURN
      END
