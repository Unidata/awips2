c  =====================================================================
c  pgm:  pinprd (id,istat)
c
c  out: id     .... product identifier
c  i/o: istat  .... completion code
c                     0 = successful
c                     1 = end of file or error
c                     2 = invalid data type
c  =====================================================================
c
      subroutine pinprd (id,istat)
c
c.......................................................................
c
c  input data type 'prod' parameters to define product contents.
c
c.......................................................................
c  Initially written by
c     Timothy L. Sweeney - Hydrologic Research Lab             Jan 1992
c.......................................................................
c
      character*4 cend(2),lcend(2),type
      character*4 id(2),wid(2),wtype,psid(8)
      character*12 work
      character*80 line
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'prodgen_inc/propar'
c
      dimension ldur(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_init/RCS/pinprd.f,v $
     . $',                                                             '
     .$Id: pinprd.f,v 1.3 2004/01/30 17:57:52 scv Exp $
     . $' /
C    ===================================================================
C
      data cend / 'ENDI','D   '/
      data lcend/ 'endi','d   '/
      data ldur/6,12,24/
      data psid / 'ISSU','LABL','SKIP','ENDI',
     +            'issu','labl','skip','endi'/
c
c
      call prbug ('pinprd',1,1,ibug)
c
      istat = 0
c
      protyp = 'prod'
      inp = iuu
      npsid = 8
c
c  read first record
      read (inp,10,err=20,end=20) line
10    format (a)
      go to 30
c
c  end of file or i/o error encountered
20    istat = 1
      go to 180

30    write (iupr,'(1x,a)') line(1:lenstr(line))
c
c  get data type
      iptr = 1
      nwid = 4
      call uffch (line,iptr,nwid,type,nxt,istat)
      if (istat.gt.0) go to 180
      if (type.ne.protyp.and.type.ne.'PROD') then
         istat = 2
         go to 180
         endif
c
c  get product id
      iptr = nxt
      nwid = 8
      call uffch (line,iptr,nwid,work,nxt,istat)
      if (istat.gt.0) go to 180
      call ucw2c4 (work,2,prodid)
      id(1) = prodid(1)
      id(2) = prodid(2)
c
c  get product format
      iptr = nxt
      nwid = 4
      call uffch (line,iptr,nwid,pform,nxt,istat)
      if (istat.gt.0) go to 180
c
c  get default data type
      iptr = nxt
      nwid = 4
      call uffch (line,iptr,nwid,dftyp,nxt,istat)
      if (istat.gt.0) go to 180
c
c  get comms product id
      iptr = nxt
      nwid = 12
      call uffch (line,iptr,nwid,work,nxt,istat)
      if (istat.gt.0) go to 180
      call ucw2c4 (work,3,pid)
c
c  get comms circuits
      iptr = nxt
      nwid = 4
      call uffch (line,iptr,nwid,cct,nxt,istat)
      if (istat.gt.0) go to 180
c
c  get wmo header
      iptr = nxt
      nwid = 8
      call uffch (line,iptr,nwid,work,nxt,istat)
      if (istat.gt.0) go to 180
      call ucw2c4 (work,2,wmo)
c
c  get sending office
      iptr = nxt
      nwid = 4
      call uffch (line,iptr,nwid,senof,nxt,istat)
      if (istat.gt.0) go to 180
c
c  get time zone
      iptr = nxt
      nwid = 4
      call uffch (line,iptr,nwid,tzone,nxt,istat)
      if (istat.gt.0) go to 180
c
c  get duration
      iptr = nxt
      nwid = 4
      call uffir (line,iptr,nwid,jdur,r,nxt,istat)
      if (istat.gt.0) go to 180
      do 40 i=1,3
         if (jdur.eq.ldur(i)) go to 50
40       continue
      jdur = ldur(1)
c
c  get description switch
50    iptr = nxt
      nwid = 4
      call uffir (line,iptr,nwid,jdes,r,nxt,istat)
      if (istat.gt.0) go to 180
c
      if (dftyp.eq.'GFFG'.or.dftyp.eq.'gffg') then
         jstr = 0
         go to 60
         endif
c
c  get stream name switch
      iptr = nxt
      nwid = 4
      call uffir (line,iptr,nwid,jstr,r,nxt,istat)
      if (istat.gt.0) go to 180
c
60    if (ibug.eq.1) write (iutw,70) type,prodid,pform,dftyp,pid,cct,
     +   wmo,senof,tzone,jdur,jdes,jstr
70    format (' ',a4,1x,2a4,2(1x,a4),1x,2a4,a1,1x,a3,1x,
     +   a4,a2,1x,a4,1x,a1,1x,3i4)

      if (dftyp.ne.'GFFG'.and.dftyp.ne.'gffg'.and.
     +    dftyp.ne.'SIII'.and.dftyp.ne.'siii') go to 100
c
c  read second record
      read (inp,10) line
      write (iupr,'(1x,a)') line(1:lenstr(line))
c
c  get local west most column
      iptr = 1
      nwid = 4
      call uffir (line,iptr,nwid,lwcol,r,nxt,istat)
      if (istat.gt.0) go to 180
c
c  get local number of columns
      iptr = nxt
      call uffir (line,iptr,nwid,lncol,r,nxt,istat)
      if (istat.gt.0) go to 180
c
c  get local south most row
      iptr = nxt
      call uffir (line,iptr,nwid,lsrow,r,nxt,istat)
      if (istat.gt.0) go to 180
c
c  get local number of rows
      iptr = nxt
      call uffir (line,iptr,nwid,lnrow,r,nxt,istat)
      if (istat.gt.0) go to 180
c
      if (ibug.eq.1) write (iutw,90) lwcol,lncol,lsrow,lnrow
90    format (' ',4i4)
      go to 180
c
c  get identifiers and type codes
100   k = 0
      kb = 0
      ke = 0
      ni = 0
125   read (inp,10,end=140) line
      write (iupr,'(1x,a)') line(1:lenstr(line))
      kb = ke + 1
      nxt = 1
      do 130 i=1,5
c     get identifier
         iptr = nxt
         nwid = 8
         call uffch (line,iptr,nwid,work,nxt,istat)
         if (istat.gt.0) go to 150
         call ucw2c4 (work,2,wid)
         if (wid(1).eq.' '.or.wid(1).eq.'none') go to 150
c        pseudo ids which do not use data types
            do 110 j=1,npsid
               if (wid(1).eq.psid(j)) then
                  wtype = ' '
                  nxt = nxt + 5
                  go to 120
                  endif
110            continue
c     get type code
         iptr = nxt
         nwid = 4
         call uffch (line,iptr,nwid,wtype,nxt,istat)
         if (istat.gt.0) go to 150
120      k = k + 1
         ke = ke + 1
         alidty(1,k) = wid(1)
         alidty(2,k) = wid(2)
         alidty(3,k) = wtype
         if (wid(1).eq.cend(1).or.wid(1).eq.lcend(1)) then
            if (wid(2).eq.cend(2).or.wid(2).eq.lcend(2)) then
               ni = 1
               istat = 0
               go to 150
               endif
            endif
130      continue
      go to 150
c
c  insert endid if missing
140   kb = ke + 1
      ke = kb
      alidty(1,kb) = 'endi'
      alidty(2,kb) = 'd   '
      alidty(3,kb) = 'none'
      ni = 1
c
150   if (ni.eq.0) go to 125
c
180   return
c
      end
