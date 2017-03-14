c  =====================================================================
c  pgm:  pinar2 (ident,istat)
c
c  out: ident  .... area identifier
c  i/o: istat  .... completion code:
c                     0 = successful
c                     1 = end of file or error
c                     2 = invalid data type
c  =====================================================================
c
      subroutine pinar2 (ident,istat)
c
c.......................................................................
c
c  Routine to read area definition input file without computation 
c  control field and runoff values.
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - March 1992
c
c  Changed to free format
c       Tim Sweeney, HRL                                     Feb 1997
c.......................................................................
c
      character*4 ident(2),type
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
     .$Source: /fs/hseb/ob72/rfc/ffg/src/zgrid_sub/RCS/pinar2.f,v $
     . $',                                                             '
     .$Id: pinar2.f,v 1.2 2004/01/06 20:03:45 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('pinar2',1,1,ibug)
c
      istat = 0
c
      artyp = 'affg'
      inp = iuu
c
c  read first record
      read (inp,10,end=20,err=20) line
10    format (a)
      go to 30
c
c  end of file encountered
20    istat = 1
      go to 70
c
c  data type
30    iptr = 1
      nwid = 4
      call uffch (line,iptr,nwid,type,nxt,istat)
      if (istat.gt.0) go to 70
      if (type.ne.artyp.and.type.ne.'AFFG') then
         istat = 2
         go to 70
         endif
c
c  area id
      iptr = nxt
      nwid = 8
      call uffch (line,iptr,nwid,work,nxt,istat)
      if (istat.gt.0) go to 70
      call ucw2c4 (work,2,areaid)
      ident(1) = areaid(1)
      ident(2) = areaid(2)
c
c  description
      iptr = nxt
      nwid = 20
      call uffch (line,iptr,nwid,work,nxt,istat)
      if (istat.gt.0) go to 70
      call ucw2c4 (work,5,adesc)
c
c  duration
      iptr = nxt
      nwid = 4
      call uffir (line,iptr,nwid,kadurf,r,nxt,istat)
      if (istat.ne.0) go to 70
c
c  basin boundary
      iptr = nxt
      nwid = 8
      call uffch (line,iptr,nwid,work,nxt,istat)
      if (istat.gt.0) go to 70
      call ucw2c4 (work,2,bbid)
      if (bbid(1).eq.'    ') bbid(1) = 'none'
c
      write (iutw,40) type,areaid,adesc,kadurf,bbid
40    format (2x,a4,1x,2a4,1x,5a4,2x,i2,1x,2a4)
c
      if (bbid(1).eq.'    '.or.bbid(1).eq.'none') then
c     read second record - box to approximate boundary for area
         read (inp,10) line
         iptr = 1
         nwid = 3
         do 50 i=1,6
            call uffir (line,iptr,nwid,ivar,r,nxt,istat)
            if (istat.ne.0) go to 70
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
            iptr = nxt
50          continue
c     convert centroid of box to integer degrees and minutes
         lat = latd*100 + latm
         lon = lond*100 + lonm
c     convert to decimal degrees
         icv = 1
         call degcon (icv,lat,alat)
         call degcon (icv,lon,alon)
         alath = lath/60.
         alonh = lonh/60.
         write (iutw,60) latd,latm,lond,lonm,lath,lonh
60    format (6x,2(1x,i2),1x,i3,3(1x,i2))
         endif
c
70    return
c
      end
