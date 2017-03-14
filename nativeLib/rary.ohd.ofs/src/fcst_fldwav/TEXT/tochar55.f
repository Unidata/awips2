      SUBROUTINE TOCHAR55(NUM,ANUM,ICOL)
      character*6 anum
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/tochar55.f,v $
     . $',                                                             '
     .$Id: tochar55.f,v 1.1 2004/02/02 22:08:28 jgofus Exp $
     . $' /
C    ===================================================================
C

cc  This subroutine converts integers between 0 & 9999 to characters

      anum="      "
      ichr=48
cc      iorg=339
      i=num
      icol=1
      if(i.ge.1000000) return
        if(i.ge.100000) then
          ival=i/100000
          anum(1:1)=char(ichr+ival)
          i=i-ival*100000
         icol=icol+1
        else
          anum(1:1)="0"
        endif
        if(i.ge.10000) then
          ival=i/10000
          anum(2:2)=char(ichr+ival)
          i=i-ival*10000
          icol=icol+1
        else
          anum(2:2)="0"
       endif
       if(i.ge.1000) then
         ival=i/1000
         anum(3:3)=char(ichr+ival)
         i=i-ival*1000
         icol=icol+1
       else
         anum(3:3)="0"
       endif
       if(i.ge.100) then
          ival=i/100
          anum(4:4)=char(ichr+ival)
          i=i-ival*100
           icol=icol+1
       else
          anum(4:4)="0"
       endif
       if(i.ge.10) then
          ival=i/10
          anum(5:5)=char(ichr+ival)
          i=i-ival*10
          icol=icol+1
       else
          anum(5:5)="0"
       endif
       if(i.ge.1) then
          ival=i+1
          anum(6:6)=char(ichr+i)
cc          icol=icol+1
       elseif(i.eq.0) then
          anum(6:6)=char(ichr)
       else
           anum="null"
       endif

cc       print 5, num,anum,icol
cc    5 format('num=',i5,'  anum=',a4,'  cols=',i1)
      return
       end

