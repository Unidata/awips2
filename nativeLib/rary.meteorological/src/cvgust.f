      subroutine cvgust(dd7,ui,gstpot)
      implicit none
C###############################################################################
C Statement of purpose.
C ---------------------
C Compute the convective gust potential by locating the 700 mb dew point
C depression and the upper level stability index on the nomogram presented in
C Western Region Technical Attachment 76-14 (June, 1976). 
C
C History.
C --------                    
C D. Baker    25 Mar 87     Original version.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C dd7         real          700 mb dew point depression (C).
C ui          real          Upper level stability index (C).
C
C On output:
C ----------               
C gstpot      integer       Microburst potential index as follows:
C                            1 = gusts less than 30 kt (low level too moist)
C                            2 = gusts less than 30 kt (upper level too stable)
C                            3 = gusts 30 to 40 kt possible
C                            4 = gusts greater than 40 kt possible
C###############################################################################
C Input arguments.
      real dd7,ui
C Output arguments.
      integer gstpot
C Local variables.
      real eq1,eq2,eq3,x
C Statement functions - equations of the slanted lines on the nomogram.
      eq1(x)=(-2.)*x+20.
      eq2(x)=3.33*x+13.33
      eq3(x)=2.*x+15.
C Check for area 1 on nomogram.
      gstpot=0
      if (dd7.le.10.) then
       if (ui.le.5.) then
        gstpot=1
       else if (ui.gt.5.) then
        if (eq1(ui).ge.dd7) gstpot=1
       end if
      end if
      if (gstpot.ne.0) go to 9999
C Check for area 2 on nomogram.
      if (ui.ge.5.) then
       if (dd7.ge.10. .and. dd7.le.25.) then
        gstpot=2
       else if (dd7.lt.10.) then
        if (eq1(ui).le.dd7) gstpot=2
       else if (dd7.gt.25.) then
        if (eq3(ui).ge.dd7) gstpot=2
       end if
      end if
      if (gstpot.ne.0) go to 9999
C Check for area 3 on nomogram.
      if (dd7.ge.10. and. dd7.le.15.) then
       if (ui.le.5.) gstpot=3
      else if (ui.le.5. and. ui.ge.3.5) then
       if (dd7.ge.10. .and. dd7.le.25.) then
        gstpot=3
       else if (dd7.ge.25.) then
        if (eq2(ui).ge.dd7 .and. eq3(ui).le.dd7) gstpot=3
       end if
      else if (ui.ge.5.) then
       if (eq3(ui).le.dd7) gstpot=3
      end if
      if (gstpot.ne.0) go to 9999
C Check for area 4 on nomogram.
      if (dd7.ge.15. .and. dd7.le.25.) then
       if (ui.le.5.) then
        gstpot=4
       end if
      else if (dd7.ge.25.) then
       if (eq2(ui).le.dd7) then
        gstpot=4
       end if
      end if
C Return to calling program.
9999  return
      end
