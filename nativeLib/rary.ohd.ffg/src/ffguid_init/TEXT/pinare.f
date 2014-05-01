c  =====================================================================
c  pgm:  pinare (id,istat)
c
c  out: ident  .... area identifier
c  i/o: istat  .... completion code:
c                     0 = successful
c                     1 = end of file or error
c                     2 = invalid data type
c                     3 = error getting values
c  =====================================================================
c
      subroutine pinare (ident,istat)
c
c.......................................................................
c
c  read data type 'affg' parameters
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL                                     Mar 1992
c
c  Changed to free format
c       Tim Sweeney, HRL                                     Feb 1997
c
c  Added area runoffs
c       Tim Sweeney, HRL                                     Dec 1997
c
c  Added option to use area runoffs as ffg.
c       Tim Sweeney, HRL                                     Mar 1999
c.......................................................................
c
      character*4 type
      character*8 ident,wid
      character*22 work
      character*80 line
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/arparm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/pinare.f,v $
     . $',                                                             '
     .$Id: pinare.f,v 1.6 2003/11/25 17:23:33 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('pinare',1,1,ibug)
c
      istat = 0
c
      artyp = 'affg'
      inp = iuu
      work = ' '
c
      lacpd = -1
      do 10 i=1,5
         affg(i) = -99.0
         aro(i) = -99.0
10       continue
c
c    -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
c
c   record 1
c
20    read (inp,'(a)',err=40,end=45) line
      if (line(1:1).eq.'#'.or.line(1:1).eq.'$') then
         write (iupr,30) line(1:lenstr(line))
30    format (' NOTE: in pinare - the following line is a comment:' /
     +   ' ',a)
         go to 20
         endif
      go to 70
40    write (iutw,50)
      if (iupr.ne.iutw) write (iupr,50)
50    format (' ERROR: in pinare - read error encountered reading ',
     +   'record.')
      istat = 3
      go to 160
45    istat = 1
      go to 160
c
c  data type
70    iptr = 1
      nwid = 4
      call uffch (line,iptr,nwid,type,nxt,istat)
      if (istat.gt.0) go to 160
      if (type.ne.'affg'.and.type.ne.'AFFG') then
         write (iutw,80) type,line(1:lenstr(line))
         if (iupr.ne.iutw) write (iupr,80) type,line(1:lenstr(line))
80    format (' ERROR: in pinare - data type (',a,') is not ',
     +   '''affg'' or ''AFFG'' on the following record:' /
     +   ' ',a)
         istat = 2
         go to 160
         endif
c
c  area identifier
      iptr = nxt
      nwid = 8
      call uffch (line,iptr,nwid,wid,nxt,istat)
      if (istat.gt.0) go to 140
      call umemov (wid,areaid,2)
      call umemov (areaid,ident,2)
c
c  description
      iptr = nxt
      nwid = 20
      call uffch (line,iptr,nwid,work,nxt,istat)
      if (istat.gt.0) go to 140
      call umemov (work,adesc,5)
c
c  computation control
      iptr = nxt
      nwid = 2
      call uffir (line,iptr,nwid,iropta,real,nxt,itype)
      if (istat.gt.0) go to 140
c
c  duration
      iptr = nxt
      nwid = 6
      call uffir (line,iptr,nwid,kadurf,real,nxt,itype)
c
c  basin boundary
      iptr = nxt
      nwid = 8
      call uffch (line,iptr,nwid,wid,nxt,istat)
      if (istat.gt.0) go to 140
      call umemov (wid,bbid,2,bbid)
      if (bbid(1).eq.' ') bbid(1) = 'none'
c
c  area runoffs
      nwid = 8
      do 100 i=1,5
         iptr = nxt
         call uffir (line,iptr,nwid,k,aro(i),nxt,itype)
100      continue
c
      write (iupr,110) type,areaid,adesc,iropta,kadurf,bbid,aro
110   format (' ',a4,1x,2a4,1x,5a4,1x,i1,1x,i1,1x,2a4,5f6.2)
c
c    -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
c
c   record 2
c
      if (bbid(1).eq.'box '.or.bbid(1).eq.'BOX ') then
c     input box to approximate boundary for area
         read (inp,'(a)') line
         iptr = 1
         nwid = 3
         do 120 i=1,6
            call uffir (line,iptr,nwid,ivar,real,nxt,itype)
            if (i.eq.1) then
               latd = ivar
               else if (i.eq.2) then
                  latm = ivar
               else if (i.eq.3) then
                  lond = ivar
               else if (i.eq.4) then
                  lonm = ivar
               else if (i.eq.5) then
                  lath = ivar
               else
                 lonh = ivar
               endif
120         iptr = nxt
c     convert centroid of box to single integer deg min
         lat = latd*100 + latm
         lon = lond*100 + lonm
c     convert to decimal degrees
         icv = 1
         call degcon (icv,lat,alat)
         call degcon (icv,lon,alon)
         alath = lath/60.
         alonh = lonh/60.
         write (iupr,130) latd,latm,lond,lonm,lath,lonh
130   format (6x, 2(1x,i2), 1x,i3, 3(1x,i2) )
         else
            latd = 0
            latm = 0
            lond = 0
            lonm = 0
            lath = 0
            lonh = 0
         endif
      go to 160
c
140   write (iutw,150) ident
      if (iupr.ne.iutw) write (iupr,150) ident
150   format (' ERROR: in pinare - errors encountered getting ',
     +   'parameters for area ',2a4,'.')
      istat = 3
c
160   return
c
      end
