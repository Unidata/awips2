c  =====================================================================
c  pgm:  gpdotb (send,type)
c
c   in: send   .... sending office
c   in: type   .... type of data
c                      AFG, HFFG  - flash flood guidance
c                      CARY       - model carryover values
c                      WSUP       - water supply
c  =====================================================================
c
      subroutine gpdotb (send,type)
c
c.......................................................................
c
c  Generate SHEF .B header for a product.
c
c.......................................................................
c  Initially written by
c      Tim Sweeney, HRL                                    Apr 1992
c
c  Added year and century
c      Tim Sweeney, HRL                                    Mar 1998
c
c  Added time zone (default was Z).  Restructured routine.
c      Tim Sweeney, HRL                                    Mar 1999
c
c  Added optional century to date created field.
c      Tim Sweeney, HRL                                    Dec 1999
c.......................................................................
c
      parameter (mline=100)
      character*1 line(mline)
      character*4 send,type
      character*6 pel(12)
c
      dimension pmin(5)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/iodev'
      include 'ffg_inc/timez'
      include 'ffg_inc/uinfo'
      include 'prodgen_inc/pfparm'
      include 'prodgen_inc/poptns'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/gpdotb.f,v $
     . $',                                                             '
     .$Id: gpdotb.f,v 1.2 2004/01/30 17:56:40 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('gpdotb',1,1,ibug)
ccc      ibug = 1
c
      if (ibug.eq.1) write (iutw,*) 'in gpdotb - lcptim=',lcptim
c
c  need to use dot A format when more than 8 carryover values
      if (mxel.gt.8) go to 160
c
      ibend = 1
c
c  type determines which physical elements to use
      call shefpe (type,iffpe,irrtyp)
c
c  clear output array
      do 10 i=1,mline
         line(i) = ' '
10       continue
c
c  insert dot B
      ipos = 1
      mwid = 2
      num = 1
      loc = 1
      call ucw2c1 (line,ipos,mwid,num,loc,'.B')
c
c  insert sending office
      mwid = 4
      num = 1
      loc = 1
      call ucw2c1 (line,ipos,mwid,num,loc,send)
c
c  insert century
      ipos = ipos + 1
      if (icent.eq.4) then
         mwid = 1
         num = 2
         loc = 1
         call ucw2c1 (line,ipos,mwid,num,loc,lcptim)
         endif
c
c  insert yymmdd
      mwid = 1
      num = 8
      loc = 3
      call ucw2c1 (line,ipos,mwid,num,loc,lcptim)
c
c  insert time zone
      ipos = ipos + 1
      mwid = 1
      num = 1
      loc = 1
      call ucw2c1 (line,ipos,mwid,num,loc,'Z')
c
c  insert 'DH'
      ipos = ipos + 1
      mwid = 2
      num = 1
      loc = 1
      call ucw2c1 (line,ipos,mwid,num,loc,'DH')
c
c  insert DH hour
      mwid = 1
      num = 10
      loc = 9
      call ucw2c1 (line,ipos,mwid,num,loc,lcptim)
c
c  insert '/DC'
      mwid = 3
      num = 1
      loc = 1
      call ucw2c1 (line,ipos,mwid,num,loc,'/DC')
c
c  insert current Z time for Date Created
      mwid = 1
      num = 12
      loc = 1
      if (icent.eq.2) loc = 3
      call ucw2c1 (line,ipos,mwid,num,loc,nowz)
c
c  insert units code
      ipos = ipos + 1
      mwid = 1
      num = 1
      loc = 1
      call ucw2c1 (line,ipos,mwid,num,loc,'/')
c
      mwid = 3
      num = 1
      loc = 1
      call ucw2c1 (line,ipos,mwid,num,loc,peunit)
c
c  FFG physical elements
      if (type.eq.'affg'.or.type.eq.'AFFG') then
         do 20 i=1,ndur
            pmin(i) = pming(i)
20          continue
         go to 40
         else if (type.eq.'hffg'.or.type.eq.'HFFG') then
           do 30 i=1,ndur
               pmin(i) = pminh(i)
30            continue
           go to  40
         endif
      go to 70
c
40    num = 0
      if (iffpe.lt.2) then
         mwid = 4
         do 50 i=1,ndur
            if (pmin(i).ge.0.0) then
               num = num + 1
               pel(num) = '/'//pets(i)(1:3)
               endif
50          continue
         else
c        alternate pe for ffg (old method)
            mwid = 6
            do 60 i=1,ndur
               if (pmin(i).ge.0.0) then
                  num = num + 1
                  pel(num) = '/'//pets(i)
                  endif
60             continue
         endif
      loc = 1
      go to 130
c
70    if (type.eq.'cary'.or.type.eq.'CARY') then
c     carryover physical elements
         if (ibug.gt.0) write (iud,80) irrtyp,ncorr,(pets(i),i=1,ncorr)
80    format (' in gpdotb - irrtyp=',i4,' ncorr=',i4,' pets=',20(1x,a5))
         mwid = 6
         num = ncorr
         loc = 1
         do 90 i=1,ncorr
            pel(i) = '/'//pets(i)
90          continue
      else if (type.eq.'wsup' .or. type.eq.'WSUP') then
c     water supply physical elements
         mwid = 6
         nws = 6
         num = nws
         loc = 1
         do 100 i=1,nws
            pel(i) = '/'//pets(i)
100         continue
         if (iwats.eq.1) then
c        compress field for unit area
            do 110 i=1,6
                nwid(i) = 5
110             continue
            else if (iwats.eq.2) then
c           expand field for total area runoff
               do 120 i=1,6
                  ndec(i) = 0
120              continue
            endif
         endif
c
c  output physical elements
130   if (num.le.5) then
         call ucw2c1 (line,ipos,mwid,num,loc,pel)
         write (iuf,140) (line(i),i=1,ipos)
140   format (100a1)
         else
c        continuation required - shorten first line
            numa = 5
            call ucw2c1 (line,ipos,mwid,numa,loc,pel)
            write (iuf,140) (line(i),i=1,ipos)
            do 150 i=1,mline
               line(i) = ' '
150            continue
c        output '.B1 '
            ipos = 1
            mwidc = 4
            numc = 1
            loc = 1
            call ucw2c1 (line,ipos,mwidc,numc,loc,'.B1 ')
c        output rest of physical element codes
            loc = 6
            call ucw2c1 (line,ipos,mwid,num,loc,pel)
            write (iuf,140) (line(i),i=1,ipos)
         endif
c
160   return
c
      end
