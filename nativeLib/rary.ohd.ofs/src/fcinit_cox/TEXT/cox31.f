C     MODULE COX31
C
      SUBROUTINE COX31(PSOLD,CSOLD,PSNEW,CSNEW)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS IS THE CARRYOVER TRANSFER ROUTINE FOR THE 'SNOW-43 '
C        OPERATION.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980 FOR THE SNOW-17 OPERATION
C     MODIFIED BY...
C        MARK WOODBURY, RTI AUG 1995 FOR USE IN THE SNOW-43 OPERATION
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C       psold        I          Old Parameter array
C       csold        I          Old Carryover array
C       psnew        I          New Parameter array
C       csnew        O          New Carryover array
C.......................................
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      real     psold, psnew, csold, csnew
C     --- L O C A L ---
      integer  i, j
      integer  ibug
      integer  ladc, iapflag, loc
      integer  itpxn, itpxo, ipropo, ipropn
      integer  npso, npsn, ncso, ncsn, no, nn,
     1         nhr, nlnew, nlold, n, lpm
      real     liqw, plwhc, wlos, ai, twe,
     1         si, r, sbaesc, accmax, sbws,
     2         fhr, fn, we, aesc, sb, aeadj
      real     sname, adc, xold, xnew, p
C     --- C O M M O N  B L O C K  V A R S ---
      integer  iodbug, itrace, idball, ndebug, idebug
C
C----- D I M E N S I O N --------------------------------
      DIMENSION PSOLD(*),CSOLD(*),PSNEW(*),CSNEW(*)
      DIMENSION SNAME(2),ADC(11)
      dimension xold(5), xnew(5), p(5,5)
C
C----- C O M M O N  B L O C K S -------------------------
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox31.f,v $
     . $',                                                             '
     .$Id: cox31.f,v 1.1 1996/05/07 10:48:58 page Exp $
     . $' /
C    ===================================================================
C
C
C----- D A T A  S T A T E M E N T -----------------------
      DATA SNAME/4HCOX3,4H1   /
C.......................................
C     TRACE LEVEL=1, DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,31,IBUG)
C.......................................
C     CONTROL VARIABLES
      ITPXO=PSOLD(10)
      ITPXN=PSNEW(10)
      NLOLD=5/ITPXO+2
      NLNEW=5/ITPXN+2
      LPM=PSNEW(25)
      ipropo = 0
      ipropn = 0
      if (psold(33) .ne. 0) ipropo = 1
      if (psnew(33) .ne. 0) ipropn = 1
C.......................................
C     CHECK FOR DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 100
C     PRINT OLD AND NEW PS AND CS ARRAYS.
      NPSO=PSOLD(16)
      NPSN=PSNEW(16)
      NCSO=11+NLOLD+25*ipropo
      NCSN=11+NLNEW+25*ipropn
      WRITE(IODBUG,900)
  900 FORMAT(1H0,48HSNOW-43 COX DEBUG--OLD AND NEW PS AND CS ARRAYS.)
      WRITE (IODBUG,901) (PSOLD(I),I=1,NPSO)
  901 FORMAT(1H0,15F8.3)
      WRITE(IODBUG,901) (CSOLD(I),I=1,NCSO)
      WRITE(IODBUG,901) (PSNEW(I),I=1,NPSN)
      WRITE(IODBUG,901) (CSNEW(I),I=1,NCSN)
C.......................................
C     BEGIN CARRYOVER TRANSFER.
  100 WE=CSOLD(1)
      CSNEW(1)=WE
      CSNEW(2)=CSOLD(2)
      LIQW=CSOLD(3)
      PLWHC=PSNEW(LPM+11)
      WLOS=0.0
      IF(LIQW.LE.PLWHC*WE) GO TO 101
      WLOS=LIQW-PLWHC*WE
      LIQW=PLWHC*WE
  101 CSNEW(3)=LIQW
      CSNEW(4)=CSOLD(4)
      ACCMAX=CSOLD(5)
      CSNEW(5)=ACCMAX
      SB=CSOLD(6)
      SB=SB-WLOS
      CSNEW(6)=SB
      AEADJ=CSOLD(10)
      SBAESC=CSOLD(7)
      SI=PSNEW(LPM+6)
      TWE=WE+LIQW
      AI=ACCMAX
      IF(AI.GT.SI) AI=SI
      IF (AEADJ.GT.0.0) AI=AEADJ
      IF(TWE.GE.AI) GO TO 105
      IF(SB.GT.TWE)SB=TWE
      LADC=PSNEW(26)
      ADC(1)=0.05
      DO 102 I=2,10
      J=LADC+I-2
  102 ADC(I)=PSNEW(J)
      ADC(11)=1.0
      R=(SB/AI)*10.0+1.0
      N=R
      FN=N
      R=R-FN
      SBAESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      IF(SBAESC.GT.1.0) SBAESC=1.0
      IF(SBAESC.LT.0.05) SBAESC=0.05
  105 CSNEW(7)=SBAESC
      SBWS=CSOLD(8)
      SBWS=SBWS-WLOS
      CSNEW(8)=SBWS
      CSNEW(9)=CSOLD(9)
      CSNEW(10)=AEADJ
C   
      aesc = sbaesc
      if (twe .gt. sb) then
         if (twe .lt. sbws) then
            AESC=SBAESC+((1.0-SBAESC)*((TWE-SB)/(SBWS-SB)))
         else
            aesc = 1.0
         endif
      endif
      csnew(11 + nlnew) = aesc
C
C     LAGGED EXCESS WATER.
      IF(ITPXN.NE.ITPXO) GO TO 110
      DO 111 N=1,NLNEW
  111 CSNEW(10+N)=CSOLD(10+N)
      GO TO 114
  110 NHR=NLOLD*ITPXO
      FN=ITPXO
      FHR=1.0/FN
      DO 113 N=1,NLNEW
  113 CSNEW(10+N)=0.0
      DO 112 N=1,NHR
      NO=(N-1)/ITPXO+1
      NN=(N-1)/ITPXN+1
      IF(NN.GT.NLNEW) GO TO 114
      CSNEW(10+NN)=CSNEW(10+NN)+FHR*CSOLD(10+NO)
  112 CONTINUE
C
C     IF P MATRIX IS BEING PROPAGATED, ADJUST FOR NEW STATES
C
C     
  114 if(ipropn .ne. 1) go to 250
      if(ipropo .ne. 1) then
C
C        New carryover, fill the array with zeros, then save
C
         do 215 i=1,5
            do 214 j=1,5
               p(i,j) = 0.
  214       continue
  215    continue
         go to 240
      else
C
C        Old carryover, retrieve the state error covariance matrix
C        (P) from the CSOLD array.
C
         loc = 11 + nlold + 1
         do 221 i = 1, 5
            do 220 j = 1, 5
               p(j,i) = csold(loc)
               loc = loc + 1
 220        continue
 221     continue
      end if
C
      iapflag = 0
      xold(1) = csold(1)
      xold(2) = csold(2)
      xold(3) = csold(3)
      xold(4) = csold(4)
      xold(5) = csold(11 + nlold)
C
      xnew(1) = csnew(1)
      xnew(2) = csnew(2)
      xnew(3) = csnew(3)
      xnew(4) = csnew(4)
      xnew(5) = csnew(11 + nlnew)
C
C     If xold different from xnew, adjust the P array
C
      do 223 i=1,5
         if (xold(i) .ne. xnew(i)) iapflag = 1
  223 continue
C
      if(iapflag .eq. 1) call adjp31(xold, xnew, p)
C
C     Load up the adjusted values from P into CSNEW
C
  240 loc = 11 + nlnew + 1
      do 246 i = 1, 5
         do 245 j = 1, 5
            csnew(loc) = p(j,i)
            loc = loc + 1
  245    continue
  246 continue
C
  250 continue
C
C     END OF CARRYOVER TRANSFER
C.......................................
C     CHECK FOR DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 999
      WRITE(IODBUG,901) (CSNEW(I),I=1,NCSN)
C.......................................
  999 CONTINUE
      RETURN
      END
