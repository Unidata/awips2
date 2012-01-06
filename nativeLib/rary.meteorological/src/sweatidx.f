c
c
	subroutine sweatidx(tt,td8,u8,v8,u5,v5,mni,ni,nj,q)
c
c..............................................................................
c
c	Routine to calculate sweat index from the total totals, 
c	850 dewpoint, and wind components at 850 and 500..
c
c	Changes:
c               J. Ramer        06-12-03        Copied from spechum. 
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	   tt              RA         I    Total Totals
c	   td8             RA         I    850mb dewpoint (K)
c	   u8              RA         I    850mb u-component (K)
c	   v8              RA         I    850mb v-component (K)
c	   u5              RA         I    500mb u-component (K)
c	   v5              RA         I    500mb v-component (K)
c	   mni             I          I    First dimension of input array.
c	   ni,nj           I          I    Grid dimensions in i,j.
c	   q               RA         O    Sweat Index.
c	
c	
c	User Notes:
c
c	1.  No quality control is performed in this routine.
c
c..............................................................................
c
	implicit none
	integer mni, ni, nj, i, j
	real tt(mni,nj), td8(mni,nj)
	real u8(mni,nj), v8(mni,nj), u5(mni,nj), v5(mni,nj)
	real q(mni,nj)
        real s8, s5, tt49, sdir
        real flg,flag
        Data flg,flag/99998.0,1e37/

        Do 10 j=1,nj
        Do 10 i=1,ni
        If (tt(i,j).gt.flg .or. td8(i,j).gt.flg .or.
     &      u8(i,j).gt.flg .or. v8(i,j).gt.flg .or.
     &      u5(i,j).gt.flg .or. v5(i,j).gt.flg) Then
            q(i,j)=flag
        Else
            s8 = sqrt(u8(i,j)*u8(i,j)+v8(i,j)*v8(i,j))
            s5 = sqrt(u5(i,j)*u5(i,j)+v5(i,j)*v5(i,j))
            if (s8.eq.0 .or. s5.eq.0) then
                sdir = 0
            else
                sdir = (u5(i,j)*v8(i,j)-v5(i,j)*u8(i,j))/(s8*s5)
            End If
            tt49 = amax1(tt(i,j)-49.0,0.0)
            q(i,j) = 12*(td8(i,j)-273.15) + 20*tt49 +
     &               2*1.944*s8 + s5*1.944 + 125*(sdir+0.2)
        End If
10      Continue

	return
	end
