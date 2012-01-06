      REAL FUNCTION RDIEEE(RIEEE)
 !$$$ SUBPROGRAM DOCUMENTATION BLOCK
 ! . . . .
 ! SUBPROGRAM: rdi3e 
 ! PRGMMR: Gilbert ORG: W/NP11 DATE: 2000-05-09
 !
 ! ABSTRACT: This subroutine reads a list of real values in 
 ! 32-bit IEEE floating point format.
 !
 ! PROGRAM HISTORY LOG:
 ! 2000-05-09 Gilbert
 !
 ! USAGE: CALL rdieee(rieee,a,num)
 ! INPUT ARGUMENT LIST:
 ! rieee - Inputfloating point value in 32-bit IEEE format.
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

      PARAMETER (two23=2.**(-23))
      PARAMETER (two126=2.**(-126))
C
      EQUIVALENCE(RTEMP,IEEE)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/rdieee.f,v $
     . $',                                                             '
     .$Id: rdieee.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C
      RTEMP=RIEEE
 !
 ! Extract sign bit, exponent, and mantissa
 !
      isign=ibits(ieee,31,1)
      iexp=ibits(ieee,23,8)
      imant=ibits(ieee,0,23)
      sign=1.0
      if (isign.eq.1) sign=-1.0
      
      if ( (iexp.gt.0).and.(iexp.lt.255) ) then
        temp=2.0**(iexp-127)
        rdi3e=sign*temp*(1.0+(two23*real(imant)))

      elseif ( iexp.eq.0 ) then
        if ( imant.ne.0 ) then
          rdi3e=sign*two126*two23*real(imant)
        else
          rdi3e=sign*0.0
        endif

C     elseif ( iexp.eq.255 ) then
C       rdi3e=sign*huge(rdi3e)

      endif
C
      return
      end
