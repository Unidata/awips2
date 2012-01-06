c  =====================================================================
c
      subroutine usrext (igetinf,icaller,iprint,icheck,ifound,istat)
c
c.......................................................................
c
c  Routine to print and/or check user control maximum and minimum
C  extremum for Gridded and Headwater FFG values.
c
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/count'
      include 'ffg_inc/iodev'
      include 'ffg_inc/gpo'
      include 'ffg_inc/uinfo'
c
      integer labl(25)
     +         / 1, 1, 2,11,12,
     +           3, 3, 4,13,14,
     +           6, 5, 6,15,16,
     +          12, 7, 8,17,18,
     +          24, 9,10,19,20/
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/usrext.f,v $
     . $',                                                             '
     .$Id: usrext.f,v 1.1 2004/01/30 17:53:12 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('usrext',1,1,ibug)
ccc      ibug = 1
c
      if (ibug.eq.1) write (iutw,*) 'in usrext - igetinf=',igetinf,
     +   ' icaller=',icaller,' iprint=',iprint,' icheck=',icheck
c
      ifound = 0
c
      if (igetinf.eq.1) then
c     get user information from file
         kod = 1
         call getinf (kod,mpo,po,iunit,istat)
         if (istat.gt.1) go to 190
         endif
c
      if (icaller.eq.1) go to 20
      if (icaller.eq.2) go to 130
      write (iutw,10) 'icaller',icaller
10    format (' ERROR: in usrext - invalid value of variable ',a,
     +   ' (',i2,').')
c
      go to 190
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c  guidance computation controls
c
20    if (iprint.eq.1) then
         write (iutw,30)
30    format (10x,'Maximum and minimum extremum for ',
     +   'Gridded and Headwater FFG values:')
         write (iutw,40)
40    format (10x,2x,7x,9x,'Gridded',15x,'Headwater' /
     +        10x,2x,'Duration',2('  Maximum    Minimum  '))
         write (iutw,50)
50    format (10x,2x,'--------',2('  ---------  ---------'))
         icmpg=1
         igeng=1
         icmph=1
         igenh=1
         do 70 i=1,5
            ii = (i-1)*5 + 1
            ij = ii + 1
            ik = labl(ij)
            il = ii + 2
            im = labl(il)
            in = ii + 3
            ji = labl(in)
            jj = ii + 4
            jk = labl(jj)
            write (iutw,60) labl(ii),
     +                      labl(ij),ext(ik),labl(il),ext(im),
     +                      labl(in),ext(ji),labl(jj),ext(jk)
60    format (10x,2x,i2,' hour',1x,
     +        2('  (',i2,')',f5.1),
     +        2('  (',i2,')',f5.1))
            if (ext(ik).eq.-1) icmpg=0
            if (ext(ji).eq.-1) icmph=0
            if (ext(im).eq.-1) igeng=0
            if (ext(jk).eq.-1) igenh=0
70          continue
         if (icmpg.eq.0.or.icmpg.eq.0) write (iutw,80)
80    format (10x,'NOTE: a maximum value of -1 indicates guidance ',
     +   'not to be computed.')
         if (igenh.eq.0.or.igeng.eq.0) write (iutw,90)
90    format (10x,'NOTE: a minimum value of -1 indicates products ',
     +   'not to be generated.')
         endif
c
      if (icheck.eq.1) then
         ifound = 0
         do 110 i=1,5
            ii = (i-1)*5 + 1
            ij = ii + 1
            ik = labl(ij)
            il = ii + 2
            im = labl(il)
            in = ii + 3
            ji = labl(in)
            jj = ii + 4
            jk = labl(jj)
            if (ibug.eq.1) write (iud,*) 'in ffmain - i=',i,' ik=',ik,
     +         ' ext(ik)=',ext(ik)
            if (ext(ik).eq.-1) then
               if (ifound.eq.0) then
                  write (iutw,*)
                  if (iupr.ne.iutw) write (iupr,*)
                  ifound = 1
                  endif
               write (iutw,100) 'Gridded',idurt(i)
               if (iupr.ne.iutw) write (iupr,100) 'Gridded',idurt(i)
100    format (' NOTE: computations for ',a,' FFG values will not ',
     +   'be done for a duration of ',i2,' hours.')
               endif
110         continue
         do 120 i=1,5
            ii = (i-1)*5 + 1
            ij = ii + 1
            ik = labl(ij)
            il = ii + 2
            im = labl(il)
            in = ii + 3
            ji = labl(in)
            jj = ii + 4
            jk = labl(jj)
            if (ibug.eq.1) write (iud,*) 'in ffmain - i=',i,' ji=',ji,
     +         ' ext(ji)=',ext(ji)
            if (ext(ji).eq.-1) then
               if (ifound.eq.0) then
                  write (iutw,*)
                  if (iupr.ne.iutw) write (iupr,*)
                  ifound = 1
                  endif
               write (iutw,100) 'Headwater',idurt(i)
               if (iupr.ne.iutw) write (iupr,100) 'Headwater',idurt(i)
               endif
120         continue
         endif
c
      go to 190
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c  product generation controls
c
130   if (iprint.eq.1) THEN
         write (iutw,30)
         write (iutw,40)
         write (iutw,50)
         icmpg=1
         igeng=1
         icmph=1
         igenh=1
         do 150 i=1,5
            ii = (i-1)*5 + 1
            ij = ii + 1
            ik = labl(ij)
            il = ii + 2
            im = labl(il)
            in = ii + 3
            ji = labl(in)
            jj = ii + 4
            jk = labl(jj)
            write (iutw,140) labl(ii),
     +                       ext(ik),ext(im),
     +                       ext(ji),ext(jk)
140   format (10x,2x,i2,' hour',1x,
     +        2('   ',2x,' ',f5.1),
     +        2('   ',2x,' ',f5.1))
            if (ext(ik).eq.-1) icmpg=0
            if (ext(ji).eq.-1) icmph=0
            if (ext(im).eq.-1) igeng=0
            if (ext(jk).eq.-1) igenh=0
150         continue
         if (icmpg.eq.0.or.icmpg.eq.0) write (iutw,80)
         if (igenh.eq.0.or.igeng.eq.0) write (iutw,90)
         endif
c
      if (icheck.eq.1) then
         ifound = 0
         do 170 i=1,5
            ii = (i-1)*5 + 1
            ij = ii + 1
            ik = labl(ij)
            il = ii + 2
            im = labl(il)
            in = ii + 3
            ji = labl(in)
            jj = ii + 4
            jk = labl(jj)
            if (ibug.eq.1) write (iud,*) 'in ffmain - i=',i,' im=',im,
     +         ' ext(im)=',ext(im)
            if (ext(im).eq.-1) then
               if (ifound.eq.0) then
                  write (iutw,*)
                  if (iupr.ne.iutw) write (iupr,*)
                  ifound = 1
                  endif
               write (iutw,160) 'Gridded',idurt(i)
               if (iupr.ne.iutw) write (iupr,160) 'Gridded',idurt(i)
160    format (' NOTE: products for ',a,' FFG values will not be ',
     +   'generated for a duration of ',i2,' hours.')
               endif
170         continue
         do 180 i=1,5
            ii = (i-1)*5 + 1
            ij = ii + 1
            ik = labl(ij)
            il = ii + 2
            im = labl(il)
            in = ii + 3
            ji = labl(in)
            jj = ii + 4
            jk = labl(jj)
            if (ibug.eq.1) write (iud,*) 'in ffmain - i=',i,' jk=',jk,
     +         ' ext(jk)=',ext(jk)
            if (ext(jk).eq.-1) then
               if (ifound.eq.0) then
                  write (iutw,*)
                  if (iupr.ne.iutw) write (iupr,*)
                  ifound = 1
                  endif
               write (iutw,160) 'Headwater',idurt(i)
               if (iupr.ne.iutw) write (iupr,160) 'Headwater',idurt(i)
               endif
180         continue
         endif
c
190   return
c
      end
