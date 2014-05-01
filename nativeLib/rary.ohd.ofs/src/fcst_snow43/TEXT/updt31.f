C     MODULE UPDT31
C
      SUBROUTINE UPDT31 (iwemsng, iscmsng, owe, owev, osc, twe, cover,
     1 gain, freew)
C........................................................
C     FOR OPERATION 31 (SNOW-43)
C     This routine controls updating by calling the filter 
C     routines as well as allowing for updating outside the 
C     filter.
C........................................................
C     Subroutine Written by
C        Mark Woodbury and Nalneesh Gaur, RTi Aug 1995
C........................................................
C     Arguments:
C
C    Argument      I/O         Description
C     IWEMSNG       I          Flag for updating Water Equivalent
C     ISCMSNG       I          Flag for updating Snow Cover
C     OWE           I          Observed Water Equivalent  
C     OWEV          I          Variance of Observed Water Equivalent 
C     OSC           I          Observed Snow Cover
C     TWE          I/O         TOTAL Water Equivalent
C                              (I - simulated; O - updated)
C     COVER        I/O         Snow Cover (I - simulated; O - updated)
C     GAIN          I          Gain for updating water equiv. states
C     FREEW         I          Free water portion of TWE
C........................................................
C
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      real    owe, owev, osc, twe, cover, gain, freew
      integer iwemsng, iscmsng
C     --- L O C A L ---
      integer i, ibug, ifbug
      real    qual
      real    x, x1, xuf, a, b, c, f, z
      real    cv, pvar
      real    dum, sname, jbug
C     --- C O M M O N  B L O C K  V A R S ---
      integer in, ipr, ipu
      integer iodbug, itrace, idball, ndebug, idebug
C
C----- D I M E N S I O N --------------------------------
      DIMENSION X(5),X1(5),xuf(5),A(5,5),B(5,2),
     1          F(5,5),C(5,5),Z(5)
      dimension sname(2)
C
C----- D A T A  S T A T E M E N T -----------------------
      data sname/4hupdt,4h31  /
      data jbug/4hUX31/
C
C
C----- C O M M O N  B L O C K S -------------------------
      common/ionum/in,ipr,ipu
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      include 'snow43/snup31'
      include 'snow43/cntl31'
      include 'snow43/cupdt31'
      include 'snow43/snco31'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/updt31.f,v $
     . $',                                                             '
     .$Id: updt31.f,v 1.1 1996/05/07 11:10:07 page Exp $
     . $' /
C    ===================================================================
C
C....................................
C
      call fprbug(sname,1,31,ibug)
      IF(ITRACE.GE.1) then
           WRITE(IODBUG,601)
  601      FORMAT(1H0,17H** UPDT31 ENTERED)
      endif
c
C----------------------------------------------------
C   Copy the model states into the X array
C
      call swst31(x,1)
C
C   Put forecast states in X1 array
C
      DO 160 I=1,nnfltr
         X1(I) = X(I)
  160 CONTINUE
C
C----------------------------------------------------
C
C   PERFORM UPDATING using Kalman filter if variance
C   and observation are available.
C   
C----------------------------------------------------
      if( iwemsng .eq. 0) then
         if (x(1) .le. 0.1)  then
            write(ipr, 605)
            call warn
            iwemsng = 1
            gain = 1.0
            go to 170
         endif
         Z(1)= OWE - freew
         if(z(1) .lt. 0) z(1) = 0.0
         r(1, 1) = owev
C
         call fltr31(x,x1,z,p)
         call chkx31(x)
         if (iscfltr .eq. 1) then
C     Trigger the call to aeco31
            if (iscmsng .eq. 2) iscmsng = 0 
         endif
C
C     Update twe based on the new states
C
         twe = x(1) + x(3) + freew
C
         if(ifbug(jbug) .eq. 1) then
            WRITE(IODBUG,602) (X1(I),I=1,5)
            WRITE(IODBUG,603) (X(I),I=1,5)
            call fmxprnt(P,nnfltr,nnfltr)
 602        FORMAT(2X,'FORECAST',2X,5F10.4)
 603        FORMAT(2X,'UPDATED ',2X,5F10.4)
         endif
      endif
C
C------------------------------------------------------
C     Make a copy of the updated X array so we'll know
C     how and whether we need to adjust the state error
C     covariance matrix if there is any updating outside 
C     the filter
C------------------------------------------------------
C
  170 continue
      do 190 i=1,5
  190     xuf(i) = x(i)
C
c     Note: X1 now contains the predicted states and 
c     X and xuf contain the updated states.  When we 
c     finish, we'll know what updating took place (X, X1)
c     and we'll know how much of it took place outside
c     the filter (X, xuf)
c
c     End of Propagation and Kalman Filtering Section
c
C------------------------------------------------------
C     This section is for updating outside the filter
C     It requires an observation and a gain and implies
C     that water equivalent was not updated in the filter.
C------------------------------------------------------
C
  200 if(iwemsng .eq. 1) then
C        allocate observation between we and liqw
C
         if( owe .lt. 1.0) owe = 0.0
         if( abs( owe - twe) .le. (wetol*twe)) go to 210
         twe = gain*owe + (1-gain)*twe 
         if( twe .gt. 0.0) then
            qual = 1.0
            if( x(1) .gt. 0.0) qual = qual + (x(3)/x(1))
            x(1) = (twe - freew)/qual
            x(3) = ( qual - 1.0) * x(1)
            call chkx31(x)
         else
C     No snow remains
            x(1)  = 0.0
            call chkx31(x)
            call zero31
            twe = 0.0
            cover = 0.0
            go to 999
         endif
      endif
C
C
C......................................................
C
C     UPDATE AREAL EXTENT OF SNOW-COVER IF OBSERVED AESC AVAILABLE,
C     AND A SNOW COVER EXISTS (TOLERANCE IS HANDLED IN AECO31).
C
  210 if( iscmsng .eq. 1 .and. twe .gt. 1.0 ) then
         if( osc .le. 0.05) then
            write(IPR, 610)
            call warn
         else
            if( osc .gt. 1.0) osc = 1.0
            x(5) = osc
         endif
      endif
C
C
C-----------------------------------------------------
C     All updating is finished - adjust areal extent of
C     snow cover states to reflect changes in WE or 
C     LIQW, if any (iwemsng < 2 indicates WE updating).
C-----------------------------------------------------
C
      if( iwemsng .lt. 2 ) then
          call adjc31(x, x1, cover)
          if(ifbug(jbug) .eq. 1) then
             write(iodbug,604) cover
 604         format(2X,'COVER',2X,F8.4)
          endif
      endif
C
C-----------------------------------------------------
C     If AESC has been updated (iscmsng < 2 indicates 
C     AESC updating), adjust the AESC states to be 
C     consistent with the updated value.  
C-----------------------------------------------------
C
      if( iscmsng .lt. 2 ) then
         call aeco31(x, cover)
         cover = x(5)
      endif
C
C     Set AESC = cover via x(5).
C 
      x(5) = cover
C
C-----------------------------------------------------
C
C     Adjust P matrix for changes to model states outside
C     the filter.
C-----------------------------------------------------
C
      if ( iprop .eq. 1 ) then
         do 220 i = 1,5
            if(x(i) .ne. xuf(i)) go to 230
  220    continue
      endif
      go to 999
C
  230 call adjp31( xuf, x, p)
C
C
  605 format(1h0,10x,'**WARNING** THE SIMULATED FROZEN WATER EQUIVALENT        
     1IS LESS THAN 0.1, BUT AN OBSERVED VALUE EXISTS. UPDATING WILL BE D
     2ONE OUTSIDE THE FILTER.',/,23x,'AESC WILL BE SET TO 1.0. PROVIDE A
     3N OBSERVED SNOW COVER VALUE IF THIS IS INCORRECT.' )
C
  610 FORMAT(1H0,114H**WARNING** AREAL EXTENT OF SNOW IS NOT UPDATED FOR
     1 OBSERVED AREAL EXTENT VALUES IN THE RANGE GT 0.001 TO LE 0.05.)
C
      if( ibug .eq. 1) then
          WRITE(IODBUG,611) KMO31,KDA31,KYR31,KHR31
  611     FORMAT(2X,I2,1H/,I2,1H/,I2,2X,2HHR,1X,I2)
          WRITE(IODBUG,612) ACCMAX,SB,SBWS,SBAESC
  612     FORMAT(2X,'ACCMAX,SB,SBWS,SBAESC      ',2X,3F8.1,F8.2)
          WRITE(IODBUG,613) (X1(I),I=1,5),(P1(I,I),I=1,5)
  613     FORMAT(2X,'FORECAST',2X,5F10.2,2X,5F14.4)
          WRITE(IODBUG,614) (X(I),I=1,5),(P(I,I),I=1,5)
  614     FORMAT(2X,'UPDATED ',2X,5F10.2,2X,5f14.4)
      endif
C
  999 call swst31(x,0)
      IF(ITRACE.GE.1) WRITE(IODBUG,699)
  699 FORMAT(1H0,14H** EXIT UPDT31)
      return 
      end
