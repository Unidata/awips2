c  =====================================================================
c  pgm:  strgrd (type,idr,idurt,nurow,nucol,mxd,mxr,mxc,
c                msrow,mwcol,xver,usrnam,sdatim,vdatim,mxval,
c                ihfld,gar,istat)
c
c   in: type   .... data type
c   in: idr    .... duration time
c   in: idurt  .... allowable duration times
c   in: nurow  .... number of grid rows used
c   in: nucol  .... number of grid columns used
c   in: mxd    .... maximum number of durations
c   in: mxr    .... maximum number of grid rows
c   in: mxc    .... maximum number of grid columns
c   in: msrow  .... most south HRAP row
c   in: mwcol  .... most west HRAP column
c   in: xver   .... xmrg file format version number
c   in: usrnam .... user name
c   in: sdatim .... saved date time
c   in: vdatim .... valid date & time (computation)
c   in: mxval  .... maximum value of data
c   in: ihfld  .... parameter array for dimension only
c   in: gar    .... gridded values array
c  out: istat  .... status code
c  =====================================================================
c
      subroutine strgrd (type,idr,idurt,nurow,nucol,mxd,mxr,mxc,
     +                   msrow,mwcol,xver,usrnam,sdatim,vdatim,mxval,
     +                   ihfld,gar,istat)
c
c.......................................................................
c
c  This routine writes gridded values to xmrg files.
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - Oct 25, 1993
c
c  Changed filenames of gridded values to reflect national HRAP row
c  instead of locl HRAP row number.
c       Tim Sweeney, HRL                                    Nov 1995
c
c  Added gridded ffg files using same format as used by xmrg files
c  containing estimated rainfall from radars
c       Tim Sweeney, HRL                                    Dec 1997
c
c  Removed single record file--hrn  (n is duration hours)
c       Tim Sweeney, HRL                                    Aug 1998
c
c  Added duration time to process flag 'proces'
c       Tim Sweeney, HRL                                    Oct 1998
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/count'
c
      character*4 type
      character*4 accmode
      character*8 usrnam,ident
      character user*8,sdatim*20,proces*8,proc*8,cdur*4,vdatim*20
      character*50 strng
      character*128 pthnam
c
      integer*2 ihfld(*)
c
      dimension idurt(*),gar(mxd,mxr,mxc)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/strgrd.f,v $
     . $',                                                             '
     .$Id: strgrd.f,v 1.8 2005/03/18 14:39:19 wkwock Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('strgrd',1,1,ibug)
c
      if (type.eq.'grff'.or.type.eq.'grro') then
         if (type.eq.'grff') proc = 'FFG'
         if (type.eq.'grro') proc = 'FFR'
         else
            write (iutw,30) type
30    format (' ERROR - strgrd: ',a,' is invalid data type.')
            nerr = nerr + 1
            go to 110
         endif
c
c  set user name
      user=usrnam
c
      lproc=lenstr(proc)
c
      idur = idurt(idr)
      call ui2c4 (idur,cdur)
      lcdur = lenstr(cdur)
c
c  use 2 digits for duration
      proces=' '
      if (lcdur.eq.1) then
         proces = proc(1:lproc)//'0'//cdur(1:lcdur)
         else
            proces = proc(1:lproc)//cdur(1:lcdur)
         endif
c
c  open file
      n = 1
      call drname (idur,n,ident)
      accmode = 'rw'
      kod = 0
      call fixopn (ident,type,pthnam,accmode,kod,idev,istat)
      lpthnam=lenstr(pthnam)
      if (ibug.eq.1) write (iud,*) 'in strgrd - ',
     *   ' pthnam=',pthnam(1:lpthnam),' istat=',istat
      if (istat.ne.0) then
         if (istat.eq.-1) then
            write (iutw,35) type,ident
            if (iupr.ne.iutw) write (iupr,35) type,ident
35    format (' ERROR: directory not found for ',a,' file for ',a,'.')
            go to 110
            else
               write (iutw,40) ident
               if (iupr.ne.iutw) write (iupr,40) ident
40    format (' ERROR: cannot open file for ',a,'.')
               go to 110
            endif
         endif
c
      rewind (idev)
c
c  fill data array
      mxval = -1
      do 60 irow=1,nurow
         do 50 icol=1,nucol
            k = (irow-1)*nucol
            if (ibug.eq.1) write (iud,45) idr,irow,icol,
     +         gar(idr,irow,icol)
45    format (' in strgrd - idr=',i4,' irow=',i4,' icol=',i4,
     +   ' gar(idr,irow,icol)=',f7.3)
            pm = 25.4*gar(idr,irow,icol)
            if (pm.gt.mxval) mxval = pm
            ihfld(k+icol) = pm*100.0
50          continue
60       continue
c
c  write xmrg format file
      call wrxmrg (idev,iupr,mwcol,msrow,nucol,nurow,user,
     +            sdatim,proces,vdatim,mxval,ihfld,istat)
      if (ibug.eq.1) write (iud,*) 'in strgrd -',
     +   ' mwcol=',mwcol,' msrow=',msrow,
     *   ' nucol=',nucol,' nurow=',nurow,
     +   ' user=',user,' sdatim=',sdatim,' proces=',proces,
     +   ' vdatim=',vdatim,' mxval=',mxval,' istat=',istat
      if (istat.ne.0) then
         write (iutw,80) irow
80    format (' ERROR: writing gridded record for row ',i4,'.')
         nerr = nerr + 1
         go to 110
         endif
c
c  close file
      call upclos (idev,pthnam,istat)
c
      if (type.eq.'grro') then
         strng='Threshold Runoff'
         lstrng=lenstr(strng)
         write (iutw,90) strng(1:lstrng),
     +         pthnam(1:lpthnam) 
         if (iupr.ne.iutw) write (iupr,90) strng(1:lstrng),
     +      pthnam(1:lpthnam)
90    format (' NOTE: Gridded ',a,' written to xmrg file ',a,'.')
         else
            strng='Flash Flood Guidance'
            lstrng=lenstr(strng)
            write (iutw,90) strng(1:lstrng),
     +         pthnam(1:lpthnam)
            if (iupr.ne.iutw) write (iupr,90) strng(1:lstrng),
     +         pthnam(1:lpthnam)
         endif
c
110   return
c
      end
