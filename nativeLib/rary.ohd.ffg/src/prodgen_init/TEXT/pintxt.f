c  =====================================================================
c  pgm:  pintxt (id,istat)
c
c  out: id       .... text identifier
c  out: istat    .... completion code:
c                       0 = successful
c                       1 = end of file or error
c                       2 = invalid data type
c  =====================================================================
c
      subroutine pintxt (id,istat)
c
c.......................................................................
c
c  input data type 'TEXT' parameters
c
c.......................................................................
c  Initially written by
c     Timothy L. Sweeney - Hydrologic Research Lab           Jan 1992
c
c  Changed to free format
c     Tim Sweeney                                            Feb 1997
c.......................................................................
c
      character*4 id(2),type
      character*4 etxt(2),letxt(2)
      character*8 work
      character*80 line
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'prodgen_inc/txtpar'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_init/RCS/pintxt.f,v $
     . $',                                                             '
     .$Id: pintxt.f,v 1.2 2004/01/30 17:57:59 scv Exp $
     . $' /
C    ===================================================================
C
      data etxt / 'ENDT','EXT ' /
      data letxt/ 'endt','ext '/
c
c
      call prbug ('pintxt',1,1,ibug)
c
      istat = 0
c
      txtyp = 'text'
      inp = iuu
c
      read (inp,10,end=20,err=20) line
10    format (a)
      go to 30
c
c  end of file encountered or i/o error
20    istat = 1
      go to 120
c
30    write (iupr,'(1x,a)') line(1:lenstr(line))
c
c  data type
      iptr = 1
      nwid = 4
      call uffch (line,iptr,nwid,type,nxt,istat)
      if (istat.gt.0) go to 120
      if (type.ne.txtyp.and.type.ne.'TEXT') then
         istat = 2
         go to 120
         endif
c
c  text id
      iptr = nxt
      nwid = 8
      call uffch (line,iptr,nwid,work,nxt,istat)
      if (istat.gt.0) go to 120
      call ucw2c4 (work,2,txtid)
      id(1) = txtid(1)
      id(2) = txtid(2)

      if (ibug.eq.1) write (iupr,*) 'type=',type,' txtid',txtid
c
c  text lines
      nlines = 0
      do 90 i=1,25
         read (inp,10,end=100,err=100) line
         write (iupr,'(1x,a)') line(1:lenstr(line))
         ib = (i-1)*18 + 1
         ie = 18*i
         read (line,60,end=100) (atext(j),j=ib,ie)
60       format (18a4)
         if (ibug.eq.1) write (iupr,70) ib,ie,(atext(j),j=ib,ie)
70    format (' ib=',i2,' ie=',i2,' (atext(j),j=ib,ie)=',18a4)
c     check for ENDTEXT command
         if (atext(ib).ne.etxt(1).and.atext(1).ne.letxt(1)) go to 80
         if (atext(ib+1).eq.etxt(2).or.
     +       atext(ib+1).eq.letxt(2)) go to 120
80       nlines = nlines + 1
90       continue
      go to 120
c
c  insert endtext
100   do 110 i=ib,ie
        atext(i) = ' '
110     continue
      atext(ib) = etxt(1)
      atext(ib+1) = etxt(2)
      nlines = nlines + 1
c
120   return
c
      end
