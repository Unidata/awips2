C     Module CKCO31
C
      SUBROUTINE CKCO31(IRDCO,PLWHC,SI,ADC,ITPX,iprop)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS ROUTINE CHECKS CARRYOVER VALUES FOR THE 'SNOW-43 '
C     OPERATION.
C.......................................
C     INITIALLY WRITTEN BY... 
C        ERIC ANDERSON - HRL   MAY |980 for the 'SNOW-17' operation.
c     Modified by...
C        Mark Woodbury - RTi, May, 1995, for use in the 'SNOW-43' 
C        operation.
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C       IRDCO        I          SWITCH GOVERNING AMOUNT OF CARRYOVER
C                               READ
C       PLWHC        I          PERCENT LIQUID WATER HOLDING CAPACITY
C       SI           I          THRESHOLD WE FOR AESC COMPUTATION
C       ADC          I          AREAL DEPLETION CURVE
C       ITPX         I          INPUT PRECIPITATION TIME INTERVAL
C       IPROP        I          SWITCH INDICATING PROPAGATION OF P MATRIX
C.......................................
C
C
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/snco31'
C
C----- D I M E N S I O N --------------------------------
      DIMENSION ADC(11)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/ckco31.f,v $
     . $',                                                             '
     .$Id: ckco31.f,v 1.1 1996/05/07 10:48:37 page Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK IF A SNOW COVER EXISTS.
      IF(IRDCO.EQ.0) GO TO 100
      IF(WE.EQ.0.0) GO TO 100
      GO TO 110
C.......................................
C     SET TO NO SNOW CONDITIONS.
  100 WE=0.0
      NEGHS=0.0
      LIQW=0.0
      TINDEX=0.0
      ACCMAX=0.0
      SB=0.0
      SBAESC=0.0
      SBWS=0.0
      aesc=0.0
  105 STORGE=0.0
      AEADJ=0.0
      DO 101 N=1,NEXLAG
  101 EXLAG(N)=0.0
      if(iprop .eq. 0) go to 120
      do 103 i=1,5
      do 102 j=1,5
      p(i,j)=0.0
  102 continue
  103 continue
      go to 120
C.......................................
C     SNOW COVER EXISTS -- MAKE CHECKS
  110 IF(NEGHS.LT.0.0) NEGHS=0.0
      IF(LIQW.GT.PLWHC*WE) LIQW=PLWHC*WE
      IF(TINDEX.GT.0.0) TINDEX=0.0
      IF(ACCMAX.LT.(WE+LIQW)) ACCMAX=WE+LIQW
      if (aesc .lt. 0.05) aesc = 0.05
      if (aesc .gt. 1.) aesc = 1.
      IF(IRDCO.EQ.2) GO TO 120
C
C     COMPUTE CARD 13 VALUES.
      TWE=WE+LIQW
      SBWS=TWE
      AI=ACCMAX
      IF(ACCMAX.GT.SI) AI=SI
      IF(TWE.LT.AI) GO TO 111
      SB=TWE
      SBAESC=1.0
      GO TO 105
  111 R=(TWE/AI)*10.0+1.0
      N=R
      FN=N
      R=R-FN
      SBAESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      IF(SBAESC.GT.1.0) SBAESC=1.0
      SB=TWE+0.2*ITPX
      GO TO 105
C
C     CARD 13 VALUES READ IN - NO CHECKS
C.......................................
c     final check for aesc
  120 if (aesc .lt. sbaesc) aesc = sbaesc
      RETURN
      END
