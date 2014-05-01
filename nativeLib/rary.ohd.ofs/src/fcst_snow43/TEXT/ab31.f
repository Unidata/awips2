C     Module AB31
C
      SUBROUTINE AB31(PXI,GM,FRACS,rnsnwk,TA,SBCI,MF,NR,A,B,CONST)
C.......................................
C    FOR OPERATION 31 (SNOW-43)
C    This routine computes derivatives for non-rain melt,
C    rain melt, water and heat; computes the elements of the A and B 
C    matrices. Called only if P matrix is being propagated.
C        A(i,j) represents partial derivative of state space
C               equation for state i with respect to state j.
C        B(i,j) represents partial derivative of state space 
C               equation for state i with respect to input j.
C
C.......................................
C    ROUTINE INITIALLY WRITTEN BY...
C       Jay Day - RTi. 
C       Nalneesh Gaur - RTi. July 1995 
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C      PXI           I          Current Precipitation.
C      GM            I          Ground melt.
C      FRACS         I          Fraction of precip that is snow.
C      RNSNWK        I          Constant needed for the FRACS
C                               derivative.
C      TA            I          Temperature.
C      SBCI          I          Stefan Boltzman's constant adjusted 
C                               for timestep
C      MF            I          Melt factor
C      NR            I          Flag indicating RAIN or NON-RAIN 
C                               period. This flag is set by MELT31 
C      A             O          output A matrix
C      B             O          output B matrix
C      CONST         I          Constant (change in AESC/change in WE)
C                               used in case 1, 3 and 4 of A, B 
C                               calculations.
C.......................................
C
C
C     implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      real pxi, gm, fracs, rnsnwk, ta, sbci, mf, a, b, const
      integer nr
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/snpm31'
      include 'snow43/snco31'
      include 'snow43/cntl31'
      include 'snow43/czero31'
C
C----- D I M E N S I O N --------------------------------
      DIMENSION A(5,5),B(5,2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/ab31.f,v $
     . $',                                                             '
     .$Id: ab31.f,v 1.1 1996/05/07 10:58:33 page Exp $
     . $' /
C    ===================================================================
C
C
C   COMPUTE THE FRACS DERIVATIVE. ONLY THE DERIVATIVE WITH RESPECT 
C   TO TEMPERATURE IS NON-ZERO.
C
      DFSU2=-RNSNWK
C
      IF(NR.LE.0) GO TO 50
C
C   NON-RAIN MELT DERIV.
C
      DMX1=LZERO1
C
      DMX2=0.
C
      DMX3=0.
C
      DMX4=0.
C
      DMX5=(MF*(TA-MBASE)*MZERO1+.0125*PXI*(1.-FRACS)*TA*(1-KZERO1))*
     1 (1-LZERO1)-LZERO1*GM
C
      DMU1=.0125*(1.-FRACS)*TA*(1-KZERO1)*AESC*(1-LZERO1)+
     1 LZERO1*FRACS*SCF
C
      DMU2=(MF*MZERO1+.0125*PXI*(1.-FRACS)*(1-KZERO1)-.0125*TA*PXI*
     1    (1-KZERO1)*DFSU2)*AESC*(1-LZERO1)+LZERO1*PXI*SCF*DFSU2
C
      GO TO 100
C
C   RAIN MELT DERIV.
C
   50 DMX1=LZERO1
C
      DMX2=0.
C
      DMX3=0.
C
      DMX4=0.
C
      DMX5=(SBCI*(((TA+273.)*.01)**4-55.55)+(2.10291E9*EXP(-4278.63/
     1 (TA+242.792))-51.935)*UADJ+.004845*PA*UADJ*TA+.0125*PXI*(1.-
     2 FRACS)*TA*(1-KZERO1))*(1-LZERO1)-LZERO1*GM
C
      DMU1=.0125*(1.-FRACS)*TA*(1-KZERO1)*AESC*(1-LZERO1)+
     1 LZERO1*FRACS*SCF
C
      DMU2=(SBCI*.04*(.01*(TA+273))**3+8.9977574E12*EXP(-4278.63/(TA+
     1 242.792))/(TA+242.792)**2*UADJ+.004845*PA*UADJ+.0125*PXI*
     2 (1.-FRACS)*(1-KZERO1)-.0125*PXI*TA*(1-KZERO1)*DFSU2)
     3 *AESC*(1-LZERO1)+LZERO1*PXI*SCF*DFSU2
C
C   WATER DERIV.
C
  100 DWX1=DMX1
C
      DWX2=DMX2
C
      DWX3=DMX3
C
      DWX4=DMX4
C
      DWX5=DMX5+PXI*(1.-FRACS)
C
      DWU1=DMU1+(1.-FRACS)*AESC
C
      DWU2=DMU2-PXI*AESC*DFSU2
C
C   HEAT DERIV.
C
      DHX1=0.
C
      DHX2=-NZERO1
C
      DHX3=0.
C
      DHX4=MF/MFMAX*NMF*AESC*(1-NZERO1)*(1-HZERO1)
C
      DHX5=MF/MFMAX*NMF*(TINDEX*(1-HZERO1)+TA*(HZERO1-KZERO1))*
     1 (1-NZERO1)
C
      DHU1=-FRACS*SCF*TA*KZERO1/160.
C
      DHU2=MF/MFMAX*NMF*AESC*(1-NZERO1)*(HZERO1*KZERO1-KZERO1)-
     1     PXI*FRACS*SCF*KZERO1/160.-(PXI*SCF*TA*KZERO1/160.)*
     2     DFSU2
C
C   ELEMENTS OF THE A AND B MATRICES
C
      A(1,1)=-DMX1+(1-JZERO1)*DWX1+JZERO1*DHX1
C
      A(1,2)=-DMX2+(1-JZERO1)*DWX2+JZERO1*DHX2+JZERO1
C
      A(1,3)=-DMX3+(1-JZERO1)*DWX3+JZERO1*DHX3
C
      A(1,4)=-DMX4+(1-JZERO1)*DWX4+JZERO1*DHX4
C
      A(1,5)=-GM-DMX5+(1-JZERO1)*DWX5+JZERO1*DHX5
C
C
      B(1,1)=FRACS*SCF-DMU1+(1-JZERO1)*DWU1+JZERO1*DHU1
C
      B(1,2)=PXI*SCF*DFSU2-DMU2+(1-JZERO1)*DWU2+JZERO1*DHU2
C
C
      A(2,1)=(DHX1-DWX1)*(1-JZERO1)*(1-FZERO1)+FZERO1*0.33*(1.+A(1,1))
C
      A(2,2)=((DHX2-DWX2)*(1-JZERO1)-JZERO1)*(1-FZERO1)+FZERO1*(0.33*
     1 A(1,2)-1.)
C
      A(2,3)=(DHX3-DWX3)*(1-JZERO1)*(1-FZERO1)+FZERO1*0.33*A(1,3)
C
      A(2,4)=(DHX4-DWX4)*(1-JZERO1)*(1-FZERO1)+FZERO1*0.33*A(1,4)
C
      A(2,5)=(DHX5-DWX5)*(1-JZERO1)*(1-FZERO1)+FZERO1*0.33*A(1,5)
C
      B(2,1)=(DHU1-DWU1)*(1-JZERO1)*(1-FZERO1)+FZERO1*0.33*B(1,1)
C
      B(2,2)=(DHU2-DWU2)*(1-JZERO1)*(1-FZERO1)+FZERO1*0.33*B(1,2)
C
C
C
      A(3,1)=IZERO1*LIQW*GM*AESC*WE**(-2)+(DWX1-DHX1)*JZERO1*IZERO1+
     1 (1-IZERO1)*PLWHC*(1.+A(1,1))
C
      A(3,2)=JZERO1*IZERO1*(DWX2-1.-DHX2)+
     1 (1-IZERO1)*PLWHC*A(1,2)
C
      A(3,3)=-IZERO1/WE*GM*AESC+JZERO1*IZERO1*(DWX3-DHX3)+
     1 (1-IZERO1)*(PLWHC*A(1,3)-1.)
C
      A(3,4)=JZERO1*IZERO1*(DWX4-DHX4)+(1-IZERO1)*PLWHC*A(1,4)
C
      A(3,5)=-IZERO1*LIQW/WE*GM+JZERO1*IZERO1*(DWX5-DHX5)+
     1 (1-IZERO1)*PLWHC*A(1,5)
C
      B(3,1)=JZERO1*IZERO1*(DWU1-DHU1)+(1-IZERO1)*PLWHC*B(1,1)
C
      B(3,2)=JZERO1*IZERO1*(DWU2-DHU2)+(1-IZERO1)*PLWHC*B(1,2)
C
C
C
      A(4,1)=0.0
C
      A(4,2)=0.0
C
      A(4,3)=0.0
C
      A(4,4)=(-HZERO1+TIPM*(HZERO1-1))*(1-GZERO1)-GZERO1
C
      A(4,5)=0.0
C
      B(4,1)=0.0
C
      B(4,2)=(HZERO1*KZERO1-TIPM*(HZERO1-1))*(1-GZERO1)
C
C  TWE DERIVATIVES
C
      DTX1=1.+A(1,1)+A(3,1)
      DDWEX1=A(1,1)+A(3,1)
C
      DTX2=A(1,2)+A(3,2)
      DDWEX2=A(1,2)+A(3,2)
C
      DTX3=1.+A(1,3)+A(3,3)
      DDWEX3=A(1,3)+A(3,3)
C
      DTX4=A(1,4)+A(3,4)
      DDWEX4=A(1,4)+A(3,4)
C
      DTX5=A(1,5)+A(3,5)
      DDWEX5=A(1,5)+A(3,5)
C
      DTU1=B(1,1)+B(3,1)
      DDWEU1=B(1,1)+B(3,1)
C
      DTU2=B(1,2)+B(3,2)
      DDWEU2=B(1,2)+B(3,2)
C
      GO TO (150,200,250,300),ICASE
C
C   CASE 1 - MAX ACCUMULATION OR GT SI
C
C
  150 A(5,1)=0.0
C
      A(5,2)=0.0
C
      A(5,3)=0.0
C
      A(5,4)=0.0
C
      A(5,5)=-1.
C
      B(5,1)=0.0
C
      B(5,2)=0.0
C
      GO TO 400
C
C
C   CASE 2 - ON ADC CURVE
C
C
  200 CONTINUE
C
      A(5,1)=CONST*DDWEX1
C
      A(5,2)=CONST*DDWEX2
C
      A(5,3)=CONST*DDWEX3
C
      A(5,4)=CONST*DDWEX4
C
      A(5,5)=CONST*DDWEX5
C
      B(5,1)=CONST*DDWEU1
C
      B(5,2)=CONST*DDWEU2
C
      GO TO 400
C
C
C   CASE 3 - NEW SNOW, AESC=1
C
  250 CONTINUE
C
      A(5,1)=CONST*DDWEX1
C
      A(5,2)=CONST*DDWEX2
C
      A(5,3)=CONST*DDWEX3
C
      A(5,4)=CONST*DDWEX4
C
      A(5,5)=CONST*DDWEX5
C
      B(5,1)=CONST*DDWEU1
C
      B(5,2)=CONST*DDWEU2
C
C
      GO TO 400
C
C
C   CASE 4 - NEW SNOW LINE
C
C
  300 CONTINUE
C
      A(5,1)=CONST*DDWEX1
C
      A(5,2)=CONST*DDWEX2
C
      A(5,3)=CONST*DDWEX3
C
      A(5,4)=CONST*DDWEX4
C
      A(5,5)=CONST*DDWEX5
C
      B(5,1)=CONST*DDWEU1
C
      B(5,2)=CONST*DDWEU2
C
  400 CONTINUE
C
      RETURN
      END
