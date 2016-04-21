      SUBROUTINE TOCHAR(XNUM,ANUM,ICOL)
      character*10 anum

cc  This subroutine converts numbers between 0 & 9999.999 to characters

	ichr=48
cc	iorg=339
      icol=1
c set the negative sign
      if(xnum.lt.0.) then
        anum(1:1)="-"
        icol=icol+1
      endif
      num=xnum
      i=iabs(num)

c find the number to left of decimal place
      if(i.ge.1000) then
        ival=i/1000
        anum(icol:icol)=char(ichr+ival)
        i=i-ival*1000
        icol=icol+1
cc      else
cc	   anum(icol:icol)="0"
	endif
	if(i.ge.100) then
	   ival=i/100
	   anum(icol:icol)=char(ichr+ival)
	   i=i-ival*100
 	   icol=icol+1
cc	else
cc	   anum(icol:icol)="0"
	endif
	if(i.ge.10) then
	   ival=i/10
	   anum(icol:icol)=char(ichr+ival)
	   i=i-ival*10
 	   icol=icol+1
cc	else
cc	   anum(3:3)="0"
	endif
	if(i.ge.1) then
	   ival=i+1
	   anum(icol:icol)=char(ichr+i)
	   icol=icol+1
	elseif(i.eq.0) then
	   anum(icol:icol)=char(ichr)
	   icol=icol+1
cc	else
cc	   a="null"
	endif

       anum(icol:icol)="."
       icol=icol+1

      rest=abs(xnum-float(num))
      i=anint(rest*1000)


cc      if(i.ge.1000) then
cc        ival=i/1000
cc        anum(icol:icol)=char(ichr+ival)
cc        i=i-ival*1000
cc        icol=icol+1
cc      else
cc        anum(icol:icol)="0"
cc      endif
	if(i.ge.100) then
	   ival=i/100
	   anum(icol:icol)=char(ichr+ival)
	   i=i-ival*100
 	   icol=icol+1
	else
	   anum(icol:icol)="0"
	endif
	if(i.ge.10) then
	   ival=i/10
	   anum(icol:icol)=char(ichr+ival)
	   i=i-ival*10
 	   icol=icol+1
	else
	   anum(icol:icol)="0"
          icol=icol+1
	endif
	if(i.ge.1) then
	   ival=i+1
	   anum(icol:icol)=char(ichr+i)
	   icol=icol+1
	elseif(i.eq.0) then
	   anum(icol:icol)=char(ichr)
	   icol=icol+1
cc	else
cc	   a="null"
	endif



cc	print 5, num,anum,icol
cc    5 format('num=',i5,'  anum=',a10,'  cols=',i1)
      return
	end

