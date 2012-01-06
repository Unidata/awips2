      SUBROUTINE CRTFLO55(I,J,II,II2,L1,L2,L3,L4,A,B,DB,D,C,QU,
     . K1,K2,K15)

      COMMON/IONUM/IN,IPR,IPU
      DIMENSION D(4,15),C(15),QU(K2,K1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/crtflo55.f,v $
     . $',                                                             '
     .$Id: crtflo55.f,v 1.1 1999/04/23 18:08:25 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/4HCRTF,4HLO55/
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      QCR=5.67*A**1.5/B**0.5
      DQCR=5.67/2.*(3.*(A*B)**0.5-DB*(A/B)**1.5)
      C(II)=-(QU(I,J)-QCR)
      D(L1,II)=-DQCR
      D(L2,II)=1.
      D(L3,II)=0.
      D(L4,II)=0.
      C(II2)=-(QU(I,J)-QU(I+1,J))
      D(L1,II2)=0.
      D(L2,II2)=1.
      D(L3,II2)=0.
      D(L4,II2)=-1.
      RETURN
      END
