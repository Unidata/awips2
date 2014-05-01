C     Module fltr31 
C
      SUBROUTINE FLTR31(x, x1, Z, p)
C.......................................
C     This routine computes the kalman gain matrix, and the 
C     updated error covariance matrix, and updates the states.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        Jay Day - RTi for the SNOW-43 operation (FROM ROUTINES 
C        BY L.E.BRAZIL, G.F.SMITH, AND J.C.SCHAAKE)
C        Modiefied by Mark Woodbury and Nalneesh Gaur 09/95
C
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C     X              O          Updated states matrix
C     X1             I          Predicted states matrix
C     Z              I          Observed Water Equivalent
C     P              O          State error covariance matrix
C
C.......................................
C
      implicit none
C
C----- D E C L A R A T I O N S --------------------------
      integer ifbug
C     --- F U N C T I O N  A R G U M E N T S ---
      real    x, x1, z, p
C     --- L O C A L --- 
      real    tnm1, tm1, tm2, tmm1, c
      real    jbug
      real    FMTG, FMTH, FMTI, FMTJ, FMTK, FMTL, FMTM,     
     1        FMTN, FMTO, FMTP, FMTQ, FMTR, FMTS, FMTT,   
     2        FMTU, FMT2
      integer ipf, ipm, ipmc
C     --- C O M M O N  B L O C K  V A R S ---
      integer iodbug, itrace, idball, ndebug, idebug
C
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/cupdt31'
      include 'snow43/tmx31'
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C----- D I M E N S I O N --------------------------------
      dimension TNM1(5,1), TM1(1), TM2(1), TMM1(1,1), C(5,1)
      DIMENSION FMTG(7),FMTH(5),FMTI(5),FMTJ(5),FMTK(4),FMTL(4),
     1          FMTM(4),FMTN(4),FMTO(4),FMTP(5),FMTQ(5),FMTR(9),
     2          FMTS(4),FMTT(8),FMTU(5),FMT2(7)
      dimension x(nnfltr), x1(nnfltr), z(mmfltr), p(nnfltr, nnfltr)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/fltr31.f,v $
     . $',                                                             '
     .$Id: fltr31.f,v 1.1 1996/05/07 11:01:23 page Exp $
     . $' /
C    ===================================================================
C
C
C----- D A T A  S T A T E M E N T -----------------------
      data ipf, ipm, ipmc/0, 0, 0/
      data jbug/4hMX31/
      DATA FMTG/4H(/' ,4HTNM1,4H=P1*,4HTRAN,4HSPOS,4HE OF,4H H')/       
      DATA FMTH/4H(/' ,4HTMM1,4H=H*T,4HNM1',4H)   /                     
      DATA FMTI/4H(/' ,4HTMM1,4H=TMM,4H1+R',4H)   /                     
      DATA FMTJ/4H(/' ,4HC=TN,4HM1*T,4HMM1',4H)   /                     
      DATA FMTK/4H(/' ,4H TM1,4H=H*X,4H1') /                            
      DATA FMTL/4H(/' ,4HTM2=,4HZ-TM,4H1') /                            
      DATA FMTM/4H(/' ,4HTNN1,4H=C*T,4HM2')/                            
      DATA FMTN/4H(/' ,4HX=X1,4H+TNN,4H1') /                            
      DATA FMTO/4H(/' ,4HTNN1,4H=C*H,4H')  /                            
      DATA FMTP/4H(/' ,4HTNN1,4H=TNN,4H2-TN,4HN1')/                     
      DATA FMTQ/4H(/' ,4HTNN2,4H=TNN,4H1*P1,4H')  /                     
      DATA FMTR/4H(/' ,4HTNN3,4H=TNN,4H2*TR,4HANSP,4HOSE ,4HOF T,4HNN1',
     1          4H)   /                                                 
      DATA FMTS/4H(/' ,4HTNM1,4H=C*R,4H')  /                            
      DATA FMTT/4H(/' ,4HTNN1,4H=TNM,4H1*TR,4HANSP,4HOSE ,4HOF C,4H')  /
      DATA FMTU/4H(/' ,4HP=TN,4HN3+T,4HNN1',4H)   /                     
      DATA FMT2/4H(/' ,4HTMM1,4H=INV,4HERSE,4H OF ,4HTMM1,4H')  /       
C
C
C  COMPUTE GAIN MATRIX
C                     TNM1 (5 x 1) = P1 (5 x 5) * Transpose{H (1 x 5)}
      CALL fmxtmult(P1,H,TNM1,nnfltr,nnfltr,mmfltr,IPF,FMTG,7,IPM)
C                     TMM1 (1 x 1) = H (1 x 5) * TNM1( 5 x 1)
      CALL fmxmult(H,TNM1,TMM1,mmfltr,nnfltr,mmfltr,IPF,FMTH,5,IPM)
C                     TMM1 (1 x 1) = TMM1 (1 x 1) * R (1 x 1)
      CALL fmxadd(TMM1,R,TMM1,mmfltr,mmfltr,IPF,FMTI,5,IPM)
C                     TMM1 (1 x 1) = Inverse {TMM1 ( 1 x 1 )}
      CALL fmxinv(mmfltr,TMM1,TMM1,TM1,TM2,IPF,FMT2,7,IPM)
C                     C (5 x 1) = TNM1 (5 x 1) * TMM1 ( 1 x 1 )
      CALL fmxmult(TNM1,TMM1,C,nnfltr,mmfltr,mmfltr,IPF,FMTJ,5,IPMC)
C
C---------------------------------------------------------------------
C  PROCESS THE OBSERVATIONS
C
C                     TM1 (1 x 1) = H (1 x 5) * X1 (5 x 1)
      CALL fmxmult(H,X1,TM1,mmfltr,nnfltr,1,IPF,FMTK,4,IPM)
C                     TM2 (1 x 1) = Z(1 x 1) * TM1 (1 x 1)               
      CALL fmxsub(Z,TM1,TM2,mmfltr,1,IPF,FMTL,4,IPM)
C                     TNN1 (5 x 1) = C (5 x 1) * TM2 (1 x 1)
      CALL fmxmult(C,TM2,TNN1,nnfltr,mmfltr,1,IPF,FMTM,4,IPM)
C                     X (5 x 1) = X1 (5 x 1) + TNN1 (5 x 1)
      CALL fmxadd(X1,TNN1,X,nnfltr,1,IPF,FMTN,4,IPMC)
C
C---------------------------------------------------------------------
C
C  COMPUTE NEW ERROR COVARIANCE MATRIX
C                     TNN1 (5 x 5) = C(5 x 1) * h (1 x 5)
      CALL fmxmult(C,H,TNN1,nnfltr,mmfltr,nnfltr,IPF,FMTO,4,IPM)     
C                     TNN2 (5 x 5) = 1                                                                   
      CALL fmxidnt(TNN2,nnfltr)                                    
C                     TNN1 (5 x 5) = TNN2 (5 x 5) - TNN1 (5 x 5)   
      CALL fmxsub(TNN2,TNN1,TNN1,nnfltr,nnfltr,IPF,FMTP,5,IPM)   
C                     TNN2 (5 x 5) = TNN1 (5 x 5) * P1 (5 x 5)                                         
      CALL fmxmult(TNN1,P1,TNN2,nnfltr,nnfltr,nnfltr,IPF,FMTQ,5,IPM) 
C                     TNN3 (5 x 5) = TNN2 (5 x 5) * TNN1 (5 x 5)                                             
      CALL fmxtmult(TNN2,TNN1,TNN3,nnfltr,nnfltr,nnfltr,IPF,FMTR,9,IPM)
C                     TNM1 (5 x 1) = C (5 x 1) * R (1 x 1)
      CALL fmxmult(C,R,TNM1,nnfltr,mmfltr,mmfltr,IPF,FMTS,4,IPM)     
C                     TNN1 (5 x 5) = TNM1 (5 x 1) * C (5 x 1) 
      CALL fmxtmult(TNM1,C,TNN1,nnfltr,mmfltr,nnfltr,IPF,FMTT,8,IPM) 
C                     P (5 x 5) = TNN3 (5 x 5) + TNN1 (5 x 5) 
      CALL fmxadd(TNN3,TNN1,P,nnfltr,nnfltr,IPF,FMTU,5,IPMC)       
C                                                                 
C
      if(ifbug(jbug) .eq. 0) GO TO 999
      WRITE(iodbug,604)
  604 FORMAT(//'FILTER GAIN MATRIX (C)')
      CALL fmxprnt(C,nnfltr,mmfltr)
      WRITE(iodbug,607)
  607 FORMAT(//' UPDATED STATE ERROR COVARIANCE MATRIX  (P)')
      CALL fmxprnt(P,nnfltr,nnfltr)
C
C
  999 RETURN
      END
