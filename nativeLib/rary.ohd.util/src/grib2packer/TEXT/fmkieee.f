      REAL FUNCTION FMKIEEE(A)
 !$$$ SUBPROGRAM DOCUMENTATION BLOCK
 ! . . . .
 ! SUBPROGRAM: mkieee 
 ! PRGMMR: Gilbert ORG: W/NP11 DATE: 2000-05-09
 !
 ! ABSTRACT: This subroutine stores a list of real values in 
 ! 32-bit IEEE floating point format.
 !
 ! PROGRAM HISTORY LOG:
 ! 2000-05-09 Gilbert
 !
 ! USAGE: CALL mkieee(a)
 ! INPUT ARGUMENT LIST:
 ! a - Input floating point value.
 !
 ! OUTPUT ARGUMENT LIST: None.
 !
 ! REMARKS: None
 !
 ! ATTRIBUTES:
 ! LANGUAGE: Fortran 90
 ! MACHINE: IBM SP
 !
 !$$$

      PARAMETER (two23=2.**23)
      PARAMETER (two126=2.**126)
C
      EQUIVALENCE(RTEMP,IEEE)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/fmkieee.f,v $
     . $',                                                             '
     .$Id: fmkieee.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
C
      alog2=alog(2.0)

      ieee=0

      if (a.ne.0.) then
 !
 ! Set Sign bit (bit 31 - leftmost bit)
 !
         if (a.lt.0.0) then
           ieee=ibset(ieee,31)
           atemp=abs(a)
         else
           ieee=ibclr(ieee,31)
           atemp=a
         endif
 !
 ! Determine exponent n with base 2
 !
         n=INT(flr(alog(atemp)/alog2))
         iexp=n+127
         if (n.gt.127) iexp=255 ! overflow
         if (n.lt.-127) iexp=0
         ! set exponent bits ( bits 30-23 )
         call mvbits(iexp,0,8,ieee,23)
 !
 ! Determine Mantissa
 ! 
         if (iexp.ne.255) then
           if (iexp.ne.0) then
             atemp=(atemp/(2.0**n))-1.0
           else
             atemp=atemp*two126
           endif
           imant=nint(atemp*two23)
         else
           imant=0
         endif
         ! set mantissa bits ( bits 22-0 )
         call mvbits(imant,0,23,ieee,0)
      endif
C
      FMKIEEE=RTEMP
      return
      end
