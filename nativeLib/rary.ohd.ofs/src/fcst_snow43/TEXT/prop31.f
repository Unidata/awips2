C     Module PROP31
C
      SUBROUTINE PROP31(PXI, GM, FRACS, TA, SBCI, MF, NR, CONST, P)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS ROUTINE COMPUTES THE PREDICTED ERROR COV MATRIX.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        Jay Day - RTi for the SNOW-43 operation (FROM ROUTINES 
C        BY L.E.BRAZIL, G.F.SMITH, AND J.C.SCHAAKE)
C        Modiified by Mark Woodbury and Nalneesh Gaur, 09/95
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C      PXI           I          Current Precipitation.
C      GM            I          Ground melt.
C      FRACS         I          Fraction of precip that is snow.
C      TA            I          Temperature.
C      SBCI          I          Stefan Boltzman's constant adjusted
C                               for timestep
C      MF            I          Melt factor
C      NR            I          Flag indicating RAIN or NON-RAIN
C                               period. This flag is set by MELT31
C      CONST         I          Constant (change in AESC/change in WE)
C      P             O          State Error Covariance matrix.
C.......................................
C
      implicit none
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/cupdt31'
      include 'snow43/tmx31'
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C----- D E C L A R A T I O N S --------------------------
      integer ifbug
C     --- F U N C T I O N  A R G U M E N T S ---
      real    pxi, gm, fracs, ta, sbci, mf, const
      integer nr
      real    p
C     --- L O C A L ---
      real    FMTB, FMTC, FMTD, FMTE, FMTF, FMT1 
      real    jbug
      real    f, a, g, tnl1
      integer i, j
      integer ipf, ipm, ipmc
C     --- C O M M O N  B L O C K  V A R S ---
      integer iodbug, itrace, idball, ndebug, idebug
C
C----- D I M E N S I O N --------------------------------
      DIMENSION P(nnfltr,nnfltr)
      DIMENSION F(5,5), G(5,2), A(5,5), tnl1(5,2)
      DIMENSION FMTB(4),FMTC(8),FMTD(4),FMTE(8),FMTF(4),   
     1          FMT1(5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/prop31.f,v $
     . $',                                                             '
     .$Id: prop31.f,v 1.1 1996/05/07 11:05:12 page Exp $
     . $' /
C    ===================================================================
C
C
C----- D A T A  S T A T E M E N T -----------------------
      DATA FMTB/4H(/' ,4HTNN1,4H=F*P,4H')  /                   
      DATA FMTC/4H(/' ,4HTNN2,4H=TNN,4H1*TR,4HANSP,4HOSE ,4HOF F,4H')  /
      DATA FMTD/4H(/' ,4HTNL1,4H=G*C,4HVU')/                        
      DATA FMTE/4H(/' ,4HTNN3,4H=TNR,4H1*TR,4HANSP,4HOSE ,4HOF G,4H')  / 
      DATA FMTF/4H(/' ,4HP1=T,4HNN1+,4HQ') /
      DATA FMT1/4H(/' ,4HTNN1,4H=TNN,4H2+TN,4HN3')/
      data jbug/4hMX31/
      data ipf, ipm, ipmc/0, 0, 0/
C
C......................................
C
C  PRINT INPUT INFORMATION
C
      if(ifbug(jbug) .eq. 1) then    
C
         WRITE(iodbug,610)
  610    FORMAT(//'STATE TRANSITION MATRIX (F)')
         CALL fmxprnt(F,nnfltr,nnfltr)
C
         WRITE(iodbug,612)
  612    FORMAT(//'INPUT WEIGHTING MATRIX (G)')
         CALL fmxprnt(G,nnfltr,llfltr)
C
         WRITE(iodbug,614)
  614    FORMAT(//'COVARIANCE MATRIX OF STATE ERROR (P)')
         CALL fmxprnt(P,nnfltr,nnfltr)
C
         WRITE(iodbug,616)
  616    FORMAT(//'SYSTEM NOISE COVARIANCE MATRIX (Q)')
         CALL fmxprnt(Q,nnfltr,nnfltr)
C
         WRITE(iodbug,618)
  618    FORMAT(//'MEASUREMENT WEIGHTING MATRIX (H)')
         CALL fmxprnt(H,mmfltr,nnfltr)
C
         WRITE(iodbug,620)
  620    FORMAT(//'INPUT ERROR COVARIANCE MATRIX (CVU)')
         CALL fmxprnt(CVU,llfltr,llfltr)
C
         WRITE(iodbug,622)
  622    FORMAT(//'MEASUREMENT ERROR COVARIANCE MATRIX (R)')
         CALL fmxprnt(R,mmfltr,mmfltr)
      endif
C
C                   
         CALL AB31(PXI,GM,FRACS,rnsnwk,TA,SBCI,MF,NR,A,G,CONST)
C
C        CALCULATE STATE TRANSITION MATRIX
C                      Identity TNN1
         CALL fmxidnt(TNN1,nnfltr)
C                      F (5 x 5) = TNN1 (5 x 5) + A (5 x 5)
         CALL fmxadd(TNN1,A,F,nnfltr,nnfltr,0,FMTB,1,0)
C
C
C
C  COMPUTE PREDICTED ERROR COVARIANCE MATRIX
C                      TNN1 (5 x 5) = F (5 x 5) * P (5 x 5)
      CALL fmxmult(F,P,TNN1,nnfltr,nnfltr,nnfltr,IPF,FMTB,4,IPM)
C                      TNN2 (5 x 5) = TNN1 (5 x 5) * Transpose{F(5 x 5)}
      CALL fmxtmult(TNN1,F,TNN2,nnfltr,nnfltr,nnfltr,IPF,FMTC,8,IPM)
C                      TNL1 (5 x 2) = G (5 x 2) * CVU (2 x 2)
      CALL fmxmult(G,CVU,TNL1,nnfltr,llfltr,llfltr,IPF,FMTD,4,IPM)
C                      TNN3 (5 x 5) = TNL1(5 x 2) * Transpose{ G(5 x 2) }
      CALL fmxtmult(TNL1,G,TNN3,nnfltr,llfltr,nnfltr,IPF,FMTE,8,IPM)
C                      TNN1 (5 x 5) = TNN2 (5 x 5) + TNN3 (5 x 5)
      CALL fmxadd(TNN2,TNN3,TNN1,nnfltr,nnfltr,IPF,FMT1,5,IPM)
C                      P1 (5 x 5) = TNN1 (5 x 5) + Q (5 x 5)
      CALL fmxadd(TNN1,Q,P1,nnfltr,nnfltr,IPF,FMTF,4,IPM)
C
      if(ifbug(jbug) .eq. 1) then    
         WRITE(iodbug,624)
  624    FORMAT(//'COVARIANCE MATRIX OF PREDICTED STATE ERROR (P1)')
         CALL fmxprnt(P1,nnfltr,nnfltr)
      endif
C
C        SET P=P1
C
      DO 22 I=1,nnfltr
      DO 22 J=1,nnfltr
      P(I,J)=P1(I,J)
   22 CONTINUE
C
  999 RETURN
      END
C
