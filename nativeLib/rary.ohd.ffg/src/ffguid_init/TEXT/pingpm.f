c  =====================================================================
c  pgm:  pingpm (id,istat)
c
c  out: ident  .... identifier
c  out: istat  .... completion code:
c                     0 = successful
c                     1 = end of input or error
c                     2 = invalid data type
c  =====================================================================
c
      subroutine pingpm (ident,istat)
c
c.......................................................................
c
c  read data type 'gdpm' parameters - runoff adjust for grids
c
c.......................................................................
c     Initially written by
c          Tim Sweeney, HRL                          March 1992
c
c  changed to free format
c          Tim Sweeney, HRL                            Feb 1997
c
c  expanded capabilities
c          Tim Sweeney, HRL                            Nov 1997
c
c  added option to skip an area while computing
c  gridded guidance
c          Tim Sweeney, HRL                            May 1999
c
c  added percent impervious area
c          Tim Sweeney, HL                             Jun 2001
c.......................................................................
c
      character*4 type
      character*8 ident
      character*20 work
      character*80 line
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/igparm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/pingpm.f,v $
     . $',                                                             '
     .$Id: pingpm.f,v 1.4 2003/08/20 13:12:44 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('pingpm',1,1,ibug)
c
      istat = 0
c
      inp = iuu
      nfld=0
c
c  record 1 - controls and grid parameters
c
      imore=0
c
5     read (inp,10,end=40,err=20) line
      if (ibug.eq.1) write (iud,*) 'in pingpm - ',
     *   'line=',line(1:lenstr(line))
10    format (a)
      if (line.eq.' ') go to 5
      write (iutw,15) line(1:lenstr(line))
15    format (' ',a)
      if (iupr.ne.iutw) write (iupr,15) line(1:lenstr(line))
      if (line(1:1).eq.'#') go to 5
      go to 60

20    write (iutw,30)
30    format (' ERROR: in pingpm - error encountered reading input ',
     *   'file.')
      istat = 2
      go to 150
c
40    if (imore.eq.1) write (iutw,50)
50    format (' ERROR: in pingpm - unexpected end of file encountered.')
      istat = 1
      go to 150
c
c  data type
60    iptr = 1
      nchar = 4
      nfld=nfld+1
      call uffch (line,iptr,nchar,type,nxt,istat)
      if (istat.gt.0) go to 150
      if (type.ne.'gdpm'.and.type.ne.'GDPM') then
         istat = 2
         go to 150
         endif
c
c  ffg area identifier
      iptr = nxt
      nchar = 8
      nfld=nfld+1
      call uffch (line,iptr,nchar,work,nxt,istat)
      if (istat.gt.0) go to 150
      call ucw2c4 (work,2,iffgid)
      call umemov (iffgid,ident,2)
c
c  high flow adjust option - values for iqoptg are:
c    0 = no adjust
c    1 = forecast flow at hours entered on record 2
c    2 = highest forecast flow over next hours entered on record 2
c    3 = highest forecast flow in time series
      iptr = nxt
      nchar = 4
      nfld=nfld+1
      call uffir (line,iptr,nchar,iqoptg,real,nxt,itype)
c
c  runoff adjust option - values for iroptg are:
c    0 = no adjust
c    1 = adjust runoff (record 3 required)
c    2 = use intensity as ffg (record 3 required)
c    3 = use runoff as ffg (record 3 not used)
c    9 = not a grid area (skip)
      iptr = nxt
      nchar = 4
      nfld=nfld+1
      call uffir (line,iptr,nchar,iroptg,real,nxt,itype)
c
c  overbank factor
      iptr = nxt
      nchar = 6
      nfld=nfld+1
      call uffir (line,iptr,nchar,integr,bank,nxt,itype)
c
c  check if at end of line
      lline=lenstr(line)
      if (nxt.gt.lline) then
         pcimpg=0.0
         go to 80
         endif
c
c  percent impervious area
      iptr = nxt
      nfld=nfld+1
      call uffir (line,iptr,nchar,integr,pcimpg,nxt,itype)
c
80    if (ibug.eq.1) then
         write (iutw,90) type,iffgid,iqoptg,iroptg,bank,pcimpg
90    format (1x,a4,1x,2a4,3x,i2,4x,i2,2f6.2)
         if (iupr.ne.iutw) write (iupr,90) type,iffgid,iqoptg,iroptg,
     +      bank,pcimpg
         endif
c
      if (iqoptg.eq.0) go to 120
c
c    -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
c
c  record 2 - forecast flow adjustment parameters
c
      imore=1
c
95    read (inp,10,end=40,err=20) line
      if (line.eq.' ') go to 95
      write (iutw,15) line(1:lenstr(line))
      if (iupr.ne.iutw) write (iupr,15) line(1:lenstr(line))
      if (line(1:1).eq.'#') go to 95
c
c  times to adjust
      nxt  = 1
      nchar = 4
      do 100 i=1,5
         iptr = nxt
         call uffir (line,iptr,nchar,integr,taqg(i),nxt,itype)
100      continue
c
c  id for forecast flow time series
      iptr  = nxt
      nchar = 8
      call uffch (line,iptr,nchar,work,nxt,istat)
      if (istat.gt.0) go to 150
      call ucw2c4 (work,2,qtsidg)
c
c  data type code for forecast flow time series
      iptr = nxt
      nchar = 4
      call uffch (line,iptr,nchar,work,nxt,istat)
      if (istat.gt.0) go to 150
      call ucw2c4 (work,1,dtcqg)
c
c  time interval of forecast flow time series
      iptr = nxt
      nchar = 4
      call uffir (line,iptr,nchar,intqg,real,nxt,itype)
c
      if (ibug.eq.1) then
         write (iutw,110) taqg,qtsidg,dtcqg,intqg
110   format (7x,5(2x,f4.0),1x,2a4,1x,a4,i4)
         if (iupr.ne.iutw) write (iupr,110) taqg,qtsidg,dtcqg,intqg
         endif
c
120   if (iroptg.ne.1.and.iroptg.ne.2.and.iroptg.ne.5) go to 150
c
c    -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
c
c  record 3 - runoff adjustment parameters
c
      imore=1
c
125   read (inp,10,end=40,err=20) line
      if (line.eq.' ') go to 125
      write (iutw,15) line(1:lenstr(line))
      if (iupr.ne.iutw) write (iupr,15) line(1:lenstr(line))
      if (line(1:1).eq.'#') go to 125
c
c  runoff values
      nxt = 1
      nchar = 6
      do 130 i=1,5
         iptr = nxt
         call uffir (line,iptr,nchar,k,rinten(i),nxt,itype)
130      continue
c
      if (ibug.eq.1) then
         write (iutw,140) rinten
140   format (7x,5f6.2)
         if (iupr.ne.iutw) write (iupr,140) rinten
         endif
c
c    -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
c
150   return
c
      end
