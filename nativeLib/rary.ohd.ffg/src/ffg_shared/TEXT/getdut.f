c  =====================================================================
c  pgm:  getdut (oftel,dutyf)
c
c  out: oftel  .... office telephone
c  out: dutyf  .... duty forecaster and phone
c  =====================================================================
      subroutine getdut (oftel,dutyf)
c.......................................................................
c  routine used to select duty forecaster name and phone.
c
c.......................................................................
c  Initially written by
c        Tim Sweeney, HRL                        July 1995
c
c  Increased size of dutyf and oftel arrays
c        Tim Sweeney, HRL                        Nov 1995
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/uinfo'
c
      parameter (nd=14)
      character*4 dutyf(nd),oftel(nd)
      character*4 itl(2)
      character*8 sname
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/getdut.f,v $
     . $',                                                             '
     .$Id: getdut.f,v 1.3 2003/03/14 18:32:02 dws Exp $
     . $' /
C    ===================================================================
C
c
      data sname/ 'getdut  ' /
c
      call prbug (sname,1,1,ibug)
c
      do 10 i=1,nd
         oftel(i) = ' '
         dutyf(i) = ' '
10       continue
c
      if (mdf.eq.0) then
         write (iutw,15)
15    format (' WARNING: no Duty Forecasters are defined.')
         go to 130
         endif
c
      write (iutw,20)
20    format (/ 5x,'Duty Forecasters:' /)
c
      do 40 k=1,mdf
         ib = (k-1)*(nd+2) + 1
         ie = ib + nd + 1
         write (iutw,30) (fcstr(i),i=ib,ie)
30    format (5x,16a4)
40       continue
C
      write (iutw,50)
50    format (' Select (initials): ',$)
      read (iutr,'(2a4)',end=60) itl
      if (itl(1).eq.' ') go to 130
      if (itl(1).eq.'.') then
         call umemov (fcstr(1),itl,2)
         write (iutw,55) itl
55    format (' NOTE: Duty Forecaster set to ',2a4,'.')
         endif
      go to 80
60    write (iutw,70)
70    format (' ERROR: in getdut - unexpected end-of-file encountered.')
      go to 130
c
c  match initials to forecaster
80    do 120 k=1,mdf
         ib = (k-1)*(nd+2) + 1
         ie = ib - nd + 1
         if (igduty.eq.1) go to 100
            if (itl(1).eq.fcstr(ib).and.itl(2).eq.fcstr(ib+1)) then
               do 90 j=1,nd
                  m = ib + j + 1
                  dutyf(j) = fcstr(m)
90                continue
               if (ibug.gt.0) write (iud,'(15a)') ' in getdut - dutyf=',
     +            dutyf
               go to 120
               endif
c     get office phone
100      if (igduty.eq.2) go to 120
            if (fcstr(ib).eq.'offi'.and.fcstr(ib+1).eq.'ce  ') then
               do 110 j = 1,nd
                  m = ib + j + 1
                  oftel(j) = fcstr(m)
110               continue
               endif
120      continue
c
130   return
c
      end
