C$PRAGMA C (GBF_READ)
c  =====================================================================
c  pgm:  ungrib (iupr,ibug,mbuf,kbuf,mfld,fld,
c                mifld,ifld,kptr,kpds,kgds,kbms,
c                lwcol,lsrow,lncol,lnrow,ngrib,istat)
c
c   in: iupr   .... unit number of output
c   in: ibug   .... debug control
c   in: mbuf   .... number of words in kbuf
c   in: kbuf   .... array to hold packed message
c   in: mfld   .... number of words in fld
c  out: fld    .... real array of unpacked values
c   in: mifld  .... number of words in ifld
c  out: ifld   .... integer array of unpacked values
c  out: kptr   .... decoded GRIB pointer array
c  out: kpds   .... decoded Product Definition Section parameters
c  out: kgds   .... decoded Grid Definition Section parameters
c  out: kbms   .... decoded bitmap array
c  out: lwcol  .... decoded western most column
c  out: lsrow  .... decoded southern most row
c  out: lncol  .... decoded number of HRAP columns
c  out: lnrow  .... decoded number of HRAP rows
c  out: istat  .... completion code
c  =====================================================================
      subroutine ungrib_sub(iupr,ibug,mbuf,kbuf,mfld,fld,
     +                  mifld,ifld, kptr,kpds,kgds,kbms,
     +                  lwcol,lsrow,lncol,lnrow,ngrib,istat)
c.......................................................................
c  This routine unpacks a GRIB file.
c
c.......................................................................
c  Initially written by
c        Tim Sweeney, HRL                                   Jun 1999
c
c  Added units labels
c        Tim Sweeney, HRL                                   Oct 1999
c
c  Changed to read length of file from within GRIB, then read
c  entire GRIB array
c        Tim Sweeney, HRL                                   Apr 2000
c
c  Moved parameter print statements into new routine prgrib
c        Tim Sweeney, HL                                    Jan 2001
c.......................................................................
c
      logical*1 kbms(*)
      character*1 kbuf(mbuf)
c
      dimension fld(mfld)
      dimension ifld(mifld)
      dimension kptr(*)
      dimension kpds(*)
      dimension kgds(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/pproc/src/gribit_sub/RCS/ungrib.f,v $
     . $',                                                             '
     .$Id: ungrib.f,v 1.1 2006/10/19 16:06:04 dsa Exp $
     . $' /
C    ===================================================================
C
c
      if (ibug.gt.0) write (iupr,*) 'enter ungrib - istat=',istat
c
      if (istat.lt.0) istatt = 0
c
c  progess GRIB
10    istat = 0
      ipos = 1
      nb = 1
c
c  read to find 'GRIB' and to get length of file
20    call gbf_read (nb,kbuf(ipos),istat)
      if (ibug.gt.0) write (iupr,30) istat
30    format (' in ungrib - after gbf_read: istat=',i4)
      if (istat.ne.0) then
         if (ipos.eq.1.and.ngrib.eq.0) then
            write (iupr,40)
40    format (' ERROR: cannot read first byte of GRIB file. ',
     +        'File may not be a GRIB file.')
            istat = -20
            else
            if (ibug.gt.0) write (iupr,50) nb
50    format (' NOTE: in ungrib - file not read for ',i4,' bytes. ',
     +        'Probably end-of-file.')
            istat = 20
            endif
         go to 160
         endif
      if (nb.eq.1) istatt = istatt + 1
      if (istatt.gt.100.and.ibug.gt.0) then
         write (iupr,60)
60    format (' NOTE: in ungrib - another GRIB indicator not found ',
     +        'Possibly end-of-file')
         istat = 20
         go to 160
         endif
      if (kbuf(1).eq.'G'.and.nb.eq.1) then
         ipos = 2
         nb = 3
         else if (nb.eq.3) then
            istatt = 0
            if (kbuf(2).ne.'R') go to 10
            if (kbuf(3).ne.'I') go to 10
            if (kbuf(4).ne.'B') go to 10
            nb = 4
            ipos = 5
         else if (nb.eq.4) then
            len=ichar(kbuf(5))*256*256+ichar(kbuf(6))*256+ichar(kbuf(7))
            if (ibug.gt.2) then
               ned = ichar(kbuf(8))
               write (iupr,70) (kbuf(i),i=1,8),len,ned
70    format (' in ungrib - kbuf(1...8)=',8a1,' len=',i7,' ned=',i4)
               write (iupr,80) ichar(kbuf(5)),ichar(kbuf(6)),
     +                         ichar(kbuf(7))
80    format(' in ungrib - kbuf(5)=',i4,' kbuf(6)=',i4,' kbuf(7)=',i4)
               endif
            nb = len - 8
            ipos = 9
         else if (nb.gt.8) then
            if (ibug.gt.2) then
               write (iupr,90) (kbuf(i),i=1,60)
90    format (' in ungrib - kbuf(1...60)=',60a1)
               k = len - 60
               write (iupr,100) k,len,(kbuf(i),i=k,len)
100   format (' in ungrib - k=',i2,' len=',i6,
     +        ' kbuf(k...len)=',(1x,60a1 /))
               endif
            go to 110
         endif
      go to 20
c
c  initialize arrays
110   do 120 i=1,25
         kpds(i) = 0
120      continue
      do 130 i=1,20
         kptr(i) = 0
         kgds(i) = 0
130      continue
c
c  get GRIB field
      call w3fi63 (kbuf,kpds,kgds,kbms,fld,kptr,istat)
      if (istat.ne.0) then
         write (iupr,140) istat
140   format (' ERROR: in GRIB decoding - w3fi63 istat=',i2 /
     +   ' Values for istat:' /
     +   5x,' 1 = ''GRIB'' not found in first 100 characters' /
     +   5x,' 2 = ''7777'' not in correct location' /
     +   5x,' 3 = Unpacked field is larger than 65160' /
     +   5x,' 4 = GDS/ grid not one of currently accepted values' /
     +   5x,' 5 = Grid not currently available for center' /
     +   5x,' 7 = Edition indicated not included in decoder' /
     +   5x,' 8 = Temp GDS indicated, but GDS flag is off' /
     +   5x,' 9 = GDS indicates size mismatch with std grid' /
     +   5x,'10 = Incorrect center indicator' /
     +   5x,'11 = Binary data section (BDS) not completely processed' /
     +   5x,'     Program is not set to process flag combinations' /
     +   5x,'     shown in octets 4 and 14' /
     +   5x,'12 = Binary data section (BDS) not completely procesed.' /
     +   5x,'     Program is not set to process flag combinations')
ccc         go to 9999
         endif
      if (ibug.gt.0) write (iupr,150) (kbms(j),j=1,20)
150   format (' in ungrib - (kbms(j),j=1,20)=',20i2)
c
c  get grid coordinates of southwest corner of field
      rlat = kgds(4)/1000.
      rlon = -kgds(5)/1000.
      lncol = kgds(2)
      lnrow = kgds(3)
      if (kgds(1).eq.5) then
         illgd = 1
         npair = 1
         call cvllgd (rlon,rlat,npair,x,y,illgd,istat)
         call cvllgd (rlon,rlat,npair,x,y,illgd,istat)
         lwcol = int(x + 0.05)
         lsrow = int(y + 0.05)
         if (ibug.gt.0) write (iupr,*) 'in ungrib - lwcol=',lwcol,
     +      ' lsrow=',lsrow,' lncol=',lncol,' lnrow=',lnrow
         endif
c
      ngrib=ngrib+1
c
160   return
c
      end
