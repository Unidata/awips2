c  =====================================================================
c  pgm:  putgro (ic)
c
c  i/o: ic     .... completion code
c  =====================================================================
c
      subroutine putgro (ic)
c
c..............................................................
c
c  Move gridded runoff to parameter array
c
c.......................................................................
c       Initially written by
c           Tim Sweeney, HRL                                 July 1995
c
c    Added checks for columns and rows within HRAP subset
c           Tim Sweeney, HRL                                 July 1999
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/uinfo'
      include 'ffg_inc/gridpm'
      include 'ffg_inc/rogrid'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/putgro.f,v $
     . $',                                                             '
     .$Id: putgro.f,v 1.3 2003/08/20 13:13:57 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('putgro',1,1,ibug)
c
      if (ic.gt.0) go to 90
c
      if (kgridf.eq.0) then
c     get all gridded runoffs from database
ccc         call getgrd (grotyp,idurt,mxdurg,nrow,ncol,mxd,mxr,mxc,
ccc     +                gval,tro,istat)
         kgridf = 1
         else if (kgridf.eq.1.or.kgridf.eq.3) then
            continue
         else
            go to 90
         endif
c
c  calculate local HRAP coordinates
      lcol = kol - mwcol + 1
      lrow = krow - msrow + 1
ccc      if (lcol.lt.1) lcol = 1
      if (lrow.lt.1.or.lrow.gt.nrow) go to 90
c
c  set array limits
      if (nok.eq.0) then
         ib = lcol
         ie = lcol
         else if (nok.ge.kol) then
            ib = lcol
            ie = lcol + nok - kol
         else if (nok.gt.0) then
            ib = lcol
            ie = lcol + nok - 1
         else
            ib = lcol + nok + 1
            ie = lcol
            if (ib.le.0) ib = 1
         endif
c
c  check if columns within HRAP subset
      if (ib.lt.1) then
         ib = 1
         if (ib.gt.ie) go to 90
         else if (ib.gt.ncol) then
            go to 90
         endif
      if (ie.gt.ncol) then
         ie = ncol
         if (ie.lt.ib) go to 90
         else if (ie.lt.1) then
            go to 90
         endif
c
      if (lrow.eq.62.and.ib.eq.151) then
ccc         ibug = 1
         if (ibug.eq.1) write (iud,*) ' in putgro - lrow=',lrow,
     +      ' ib=',ib
         endif
      if (ibug.eq.1) write (iud,10) tro(1,62,151)
10    format (' in putgro - (tro(1,62,151)=',f7.3)
c
      if (ibug.gt.0) then
         write (iud,*) 'in putgro - ldur=',ldur,' lrow=',lrow,
     +      ' ib=',ib,' ie=',ie
         write (iud,20) (tro(ldur,lrow,i),i=ib,ie)
20    format (' in putgro - (tro(ldur,lrow,k),k=ib,ie)=' / (1x,10f7.3))
         endif
c
c  transfer runoff values into parametric storage array tro
      do 30 i=ib,ie
         tro(ldur,lrow,i) = ro
30       continue
      if (ibug.eq.1) then
         write (iud,40) krow,kol,nok,ro,ldur,lrow,ib,ie
40    format (' in putgro - krow=',i4,' kol=',i4,' nok=',i4,
     +   ' ro=',f6.3,' ldur=',i4,' lrow=',i4,' ib=',i4,' ie=',i4)
         if (ib.eq.ie) then
            write (iud,50) tro(ldur,lrow,ib)
50    format (' in putgro - tro(ldur,lrow,ib)=',f7.3)
            else
               write (iud,20) (tro(ldur,lrow,k),k=ib,ie)
            endif
         endif
c
      if (ibug.ne.2) go to 90
c
c  extend row by 3 columns on left and right ends
      iext = 3
      do 70 k=1,2
         if (k.eq.1) then
            jb = ib - iext
            je = ib - 1
            ibh = jb
            else
               je = ie + iext
               jb = ie + 1
               ieh = je
            endif
         do 60 i=jb,je
            if (i.lt.1.or.i.gt.ncol) go to 60
            if (tro(ldur,lrow,i).le.0.) then
               tro(ldur,lrow,i) = ro
               else if (ro.lt.tro(ldur,lrow,i)) then
                  tro(ldur,lrow,i) = ro
               else
               endif
60          continue
70       continue
      write (iud,80) ibh,ieh,ro
80    format (' local extended columns: ',i4,' thru ',i4,
     +   ' filled with ',f6.3 )
c
90    return
c
      end

