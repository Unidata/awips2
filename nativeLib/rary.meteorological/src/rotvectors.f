c
c
	subroutine rotVectors(aX,aY,angle,bX,bY,mni,ni,nj)
c
c..... 	Rotate a field of vectors "a", outputing the field of vectors "b".
c.....  An angle of +90 is the same as doing a "k cross" operation.

c.....	J Ramer Jun 95
c
	implicit none
	integer mni, ni, nj, i, j
	real flag, bad
	parameter(flag = 1.e37)
	real aX(mni,nj), aY(mni,nj), angle, bX(mni,nj), bY(mni,nj)
        real cosrot,sinrot
        real dgtord/0.01745329252/

        if (angle.eq.90.0) then
            do 2 j=1,nj
            do 2 i=1,ni
            bX(i,j) = -aY(i,j)
2           bY(i,j) = aX(i,j)
            return
        else if (angle.eq.-90.0) then
            do 3 j=1,nj
            do 3 i=1,ni
            bX(i,j) = aY(i,j)
3           bY(i,j) = -aX(i,j)
            return
        endif
c
	bad = 1e10
        cosrot = cos(dgtord*angle)
        sinrot = sin(dgtord*angle)
	do 1 j=1,nj
	do 1 i=1,ni
	  if(aX(i,j).gt.bad .or. bX(i,j).gt.bad) then
	     bX(i,j) = flag
	     bY(i,j) = flag
	  else
            bX(i,j) = cosrot*aX(i,j)-sinrot*aY(i,j)
            bY(i,j) = sinrot*aX(i,j)+cosrot*aY(i,j)
	  endif
1	continue
c
	return
	end
