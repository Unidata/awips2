c
c
	subroutine ReplInRange(a,testop,lo,hi,repl,result,mni,ni,nj)

c.....  The basic function of this routine is to copy values in the array
c.....  `a' to the array `result', unless a test is passed, in which case
c.....  values in `result' are replaced with values in the array `repl'.
c.....  The variable `tsttyp' controls how this routine functions.
c.....  If `tsttyp' is positive, then the test is passed when a>lo and
c.....  a<hi, otherwise the test is passed when a<lo or a>hi.
c.....  If the absolute value of `tsttyp' is two, then the entire
c.....  grid is tested point by point and if ANY test is passed then the
c.....  entire grid is replaced.  If the absolute value of `tsttyp' is
c.....  three or more, then the entire grid is tested point by point
c.....  and if EVERY test is passed then the entire grid is replaced.
c.....  Otherwise each individual corresponding grid point is tested
c.....  individually and replaced individually.

	implicit none
	integer testop, mni, ni, nj, i, j
	real a(mni,nj), lo(mni,nj), hi(mni,nj), repl(mni,nj)
	real result(mni,nj)
        integer tsttyp, optyp
        real Flag,Flg
        Data Flag,Flg/1e37,1e36/

        optyp = testop/1000
        tsttyp = testop-1000*optyp
        if (optyp.lt.0) optyp=-optyp

c        write (6,*) 'testop,optyp,tsttyp ',testop,optyp,tsttyp

        if (tsttyp.le.-3) Then
            do 993 j=1,nj
            do 993 i=1,ni
              if (lo(i,j).gt.Flg) Goto 993
              if (a(i,j).lt.lo(i,j)) Goto 993
              if (hi(i,j).gt.Flg) Goto 993
              if (a(i,j).gt.hi(i,j)) Goto 993
              Goto 44
993         continue
            Goto 55
        else if (tsttyp.ge.3) Then
            do 3 j=1,nj
            do 3 i=1,ni
              if (a(i,j).gt.Flg .or. lo(i,j).gt.Flg) Goto 3
              if (a(i,j).gt.lo(i,j) .and. a(i,j).lt.hi(i,j)) Goto 3
              Goto 44
3           continue
            Goto 55
        else if (tsttyp.le.-2) Then
            do 992 j=1,nj
            do 992 i=1,ni
              if (a(i,j).gt.Flg .or. lo(i,j).gt.Flg) Goto 992
              if (a(i,j).lt.lo(i,j) .or. a(i,j).gt.hi(i,j)) Goto 55
992         continue
        else if (tsttyp.ge.2) Then
            do 2 j=1,nj
            do 2 i=1,ni
              if (a(i,j).gt.Flg .or. lo(i,j).gt.Flg .or.
     &            hi(i,j).gt.Flg) Goto 2
              if (a(i,j).gt.lo(i,j) .and. a(i,j).lt.hi(i,j)) Goto 55
2           continue
        else if (tsttyp.eq.-1) Then
            if (optyp.eq.0) Then
                do 90 j=1,nj
                do 90 i=1,ni
                  if (a(i,j).lt.lo(i,j) .or. a(i,j).gt.hi(i,j)) then
                    result(i,j) = repl(i,j)
                  else
                    result(i,j) = a(i,j)
                  endif
90              continue
                return
            else if (optyp.eq.1) Then
                do 91 j=1,nj
                do 91 i=1,ni
                  if (a(i,j).ge.lo(i,j) .and. a(i,j).le.hi(i,j)) then
                    result(i,j) = a(i,j)
                  else if (a(i,j).gt.Flg .or. repl(i,j).gt.Flg) then
                    result(i,j) = Flag
                  else
                    result(i,j) = a(i,j)+repl(i,j)
                  endif
91              continue
                return
            else if (optyp.eq.2) Then
                do 92 j=1,nj
                do 92 i=1,ni
                  if (a(i,j).ge.lo(i,j) .and. a(i,j).le.hi(i,j)) then
                    result(i,j) = a(i,j)
                  else if (a(i,j).gt.Flg .or. repl(i,j).gt.Flg) then
                    result(i,j) = Flag
                  else
                    result(i,j) = a(i,j)-repl(i,j)
                  endif
92              continue
                return
            else if (optyp.eq.3) Then
                do 93 j=1,nj
                do 93 i=1,ni
                  if (a(i,j).ge.lo(i,j) .and. a(i,j).le.hi(i,j)) then
                    result(i,j) = a(i,j)
                  else if (a(i,j).gt.Flg .or. repl(i,j).gt.Flg) then
                    result(i,j) = Flag
                  else
                    result(i,j) = a(i,j)*repl(i,j)
                  endif
93              continue
                return
            else if (optyp.eq.4) Then
                do 94 j=1,nj
                do 94 i=1,ni
                  if (a(i,j).ge.lo(i,j) .and. a(i,j).le.hi(i,j)) then
                    result(i,j) = a(i,j)
                  else if (a(i,j).gt.Flg .or. repl(i,j).gt.Flg .or.
     &                     repl(i,j).eq.0) then
                    result(i,j) = Flag
                  else
                    result(i,j) = a(i,j)/repl(i,j)
                  endif
94              continue
                return
            endif
        else if (optyp.eq.0) Then
            do 10 j=1,nj
            do 10 i=1,ni
              if(a(i,j).gt.lo(i,j) .and. a(i,j).lt.hi(i,j)) then
                result(i,j) = repl(i,j)
              else
                result(i,j) = a(i,j)
              endif
10          continue
            return
        else if (optyp.eq.1) Then
            do 11 j=1,nj
            do 11 i=1,ni
              if(a(i,j).le.lo(i,j) .or. a(i,j).ge.hi(i,j)) then
                result(i,j) = a(i,j)
              else if (a(i,j).gt.Flg .or. repl(i,j).gt.Flg) then
                result(i,j) = Flag
              else
                result(i,j) = a(i,j)+repl(i,j)
              endif
11          continue
            return
        else if (optyp.eq.2) Then
            do 12 j=1,nj
            do 12 i=1,ni
              if(a(i,j).le.lo(i,j) .or. a(i,j).ge.hi(i,j)) then
                result(i,j) = a(i,j)
              else if (a(i,j).gt.Flg .or. repl(i,j).gt.Flg) then
                result(i,j) = Flag
              else
                result(i,j) = a(i,j)-repl(i,j)
              endif
12          continue
            return
        else if (optyp.eq.3) Then
            do 13 j=1,nj
            do 13 i=1,ni
              if(a(i,j).le.lo(i,j) .or. a(i,j).ge.hi(i,j)) then
                result(i,j) = a(i,j)
              else if (a(i,j).gt.Flg .or. repl(i,j).gt.Flg) then
                result(i,j) = Flag
              else
                result(i,j) = a(i,j)*repl(i,j)
              endif
13          continue
            return
        else if (optyp.eq.4) Then
            do 14 j=1,nj
            do 14 i=1,ni
              if(a(i,j).le.lo(i,j) .or. a(i,j).ge.hi(i,j)) then
                result(i,j) = a(i,j)
              else if (a(i,j).gt.Flg .or. repl(i,j).gt.Flg .or.
     &                 repl(i,j).eq.0) then
                result(i,j) = Flag
              else
                result(i,j) = a(i,j)/repl(i,j)
              endif
14          continue
            return
        endif

44	do 444 j=1,nj
        do 444 i=1,ni
444     result(i,j) = a(i,j)
        write (*,*) 'pass thru'
	return

55      If (optyp.eq.0) Then
            do 555 j=1,nj
            do 555 i=1,ni
555         result(i,j) = repl(i,j)
        Else If (optyp.eq.1) Then
            do 5551 j=1,nj
            do 5551 i=1,ni
            if (a(i,j).gt.Flg .or. repl(i,j).gt.Flg) Then
                result(i,j) = Flag
            else
                result(i,j) = a(i,j)+repl(i,j)
            endif
5551        continue
        Else If (optyp.eq.2) Then
            do 5552 j=1,nj
            do 5552 i=1,ni
            if (a(i,j).gt.Flg .or. repl(i,j).gt.Flg) Then
                result(i,j) = Flag
            else
                result(i,j) = a(i,j)-repl(i,j)
            endif
5552        continue
        Else If (optyp.eq.3) Then
            do 5553 j=1,nj
            do 5553 i=1,ni
            if (a(i,j).gt.Flg .or. repl(i,j).gt.Flg) Then
                result(i,j) = Flag
            else
                result(i,j) = a(i,j)*repl(i,j)
            endif
5553        continue
        Else If (optyp.eq.4) Then
            do 5554 j=1,nj
            do 5554 i=1,ni
            if (repl(i,j).eq.0 .or. a(i,j).gt.Flg .or.
     &          repl(i,j).gt.Flg) Then
                result(i,j) = Flag
            else
                result(i,j) = a(i,j)/repl(i,j)
            endif
5554        continue
        EndIf

C        write (6,*) 'replace'

	return
	end
