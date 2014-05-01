C     MEMBER CSAV31
C.......................................
      SUBROUTINE CSAV31(CS, iprop)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS ROUTINE STORES CARRYOVER VALUES IN THE SNCO31
C     COMMON BLOCK IN ARRAY CS.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980 for the 'SNOW-17' operation.  
C     Modified by...
C        Mark Woodbury - RTi., May 1995 for the 'SNOW-43' operation.
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C      CS            O          Carray over array
C      iprop         I          Location of Kalman Filtering
C                               Parameters in the P array
C.......................................
C
C
C
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      real    cs
      integer iprop
C     --- L O C A L ---
      integer loc, i, j
C----- D I M E N S I O N --------------------------------
      DIMENSION CS(*)
C
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/snco31'
      include 'snow43/cupdt31'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/csav31.f,v $
     . $',                                                             '
     .$Id: csav31.f,v 1.1 1996/05/07 11:00:41 page Exp $
     . $' /
C    ===================================================================
C
C
C.......................................
      CS(1)=WE
      CS(2)=NEGHS
      CS(3)=LIQW
      CS(4)=TINDEX
      CS(5)=ACCMAX
      CS(6)=SB
      CS(7)=SBAESC
      CS(8)=SBWS
      CS(9)=STORGE
      CS(10)=AEADJ
      DO 100 I=1,NEXLAG
  100 CS(10+I)=EXLAG(I)
c
      cs(11 + nexlag) = aesc
      if(iprop .eq. 0) go to 999
      loc = 12 + nexlag
      do 210 i = 1, nnfltr
         do 200 j = 1,nnfltr 
            cs(loc) = p(j, i)
            loc = loc + 1
  200    continue
  210 continue
c
C.......................................
  999 RETURN
      END
