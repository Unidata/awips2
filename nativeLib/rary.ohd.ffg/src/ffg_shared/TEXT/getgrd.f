c  =====================================================================
c  pgm:  getgrd (type,idurt,nudur,nurow,nucol,mxd,mxr,mxc,
c                msrow,mwcol,xver,usrnam,sdatim,aproc,vdatim,mxval,
c                ihfld,gar,ibline,istat)
c
c   in: type   .... data type
c   in: idurt  .... allowable duration times
c   in: nudur  .... number of durations used
c  i/o: nurow  .... number of grid rows used
c  i/o: nucol  .... number of grid columns used
c   in: mxd    .... maximum number of durations
c   in: mxr    .... maximum number of grid rows
c   in: mxc    .... maximum number of grid columns
c  i/o: msrow  .... most south HRAP row
c  i/o: mwcol  .... most west HRAP column
c  out: xver   .... version number of gridded file
c  out: usrnam .... username
c  out: sdatim .... date and time data saved
c  out: aproc  .... array of process flags
c  out: vdatim .... valid date and time (data computed)
c  out: mxval  .... maximum value in data array
c   in: ihfld  .... grid array from xmrg file
c  out: gar    .... grid array for all durations defined
c   in: ibline .... indicator if to print blank line
c  out: istat  .... status code
c  =====================================================================
c
      subroutine getgrd (type,idurt,nudur,nurow,nucol,mxd,mxr,mxc,
     +           msrow,mwcol,xver,usrnam,sdatim,aproc,vdatim,mxval,
     +           ihfld,gar,ibline,istat)
c
c.......................................................................
c
c  This routine reads gridded values from xmrg files.
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - Oct 25, 1993
c
c  Changed gridded filenames to national HRAP row numbers
c       Tim Sweeney, HRL                                    Nov 1995
c
c  File format changed to multiple records per file.
c       Tim Sweeney, HRL                                    Dec 1997
c
c  Removed single record file--the hrn files.
c       Tim Sweeney, HRL                                    Aug 1998
c.......................................................................
c
      character*2 bname
      character*4 type
      character*4 accmode
      character*8 usrnam,usrnamz,proces,aproc(5),ident
ckwz.10/13/04.user should not include os id
ckwz      character user*10,sdatim*20,vdatim*20
      character user*8,sdatim*20,vdatim*20
      character*50 strng
      character*128 pthnam
c
      integer*2 ihfld(*)
c
      dimension idurt(5),gar(mxd,mxr,mxc)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/count'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/getgrd.f,v $
     . $',                                                             '
     .$Id: getgrd.f,v 1.8 2005/03/18 14:36:27 wkwock Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('getgrd',1,1,ibug)
c
      if (type.eq.'grff'.or.type.eq.'grro') then
         else
            write (iutw,10) type
10    format (' ERROR - getgrd: ',a,' is invalid data type.')
            nerr = nerr + 1
            go to 120
         endif
c
c  read xmrg format file for each duration
      do 80 idr=1,nudur
         idur = idurt(idr)
c     set file name from duration
         n = 1
         call drname (idur,n,ident)
c     open file
         accmode = 'r'
         kod = 1
         call fixopn (ident,type,pthnam,accmode,kod,idev,istat)
         lpthnam=lenstr(pthnam)
         if (ibug.eq.1) write (iud,*) 'in getgrd -',
     *      ' pthnam=',pthnam(1:lpthnam),' istat=',istat
         if (istat.ne.0) then
c        file not found - initialize array to missing
            write (iutw,30) pthnam(1:lpthnam)
            if (iupr.ne.iutw) write (iupr,30) pthnam(1:lpthnam) 
30    format (' WARNING: file ',a,' not found. ',
     +   'Gridded values will be set to missing.')
            nwarn = nwarn + 1
            call inigrd (idr,nurow,nucol,mxd,mxr,mxc,gar,istat)
            go to 80
            endif
         if (idr.eq.1.and.ibline.eq.1) then
            write (iutw,*)
            if (iupr.ne.iutw) write (iupr,*)
            endif
c     read gridded values
         call rdxmrg (pthnam,idev,iutw,xver,kwcol,ksrow,kncol,knrow,
     +                user,sdatim,proces,vdatim,mxval,ihfld,istat)
         if (istat.ne.0) then
            nerr = nerr + 1
            go to 120
            endif
         if (ibug.eq.1) write (iud,*) 'in getgrd - user=',user
         if (user.ne.usrnam) then
            write (iutw,55) pthnam(1:lpthnam),
     +          user,usrnam
            if (iupr.ne.iutw) write (iupr,55) pthnam(1:lpthnam),
     +          user,usrnam
55    format (' WARNING in getgrd: user name in ',
     +   'xmrg file ',a,' (',a,') is not the same as in ',
     +   'FFG user information file (',a,').')
            nwarn = nwarn + 1
            endif
         aproc(idr) = proces
         do 60 irow=1,knrow
            do 50 icol=1,kncol
               k = (irow-1)*kncol
               gar(idr,irow,icol) = ihfld(k+icol)/(100.*25.4)
               if (ibug.eq.1) write (iud,*) 'in getgrd -',
     *            ' idr=',idr,' irow=',irow,' icol=',icol,
     *            ' gar(idr,irow,icol)=',gar(idr,irow,icol)
50             continue
60          continue
         if (type.eq.'grro') then
            strng='Threshold Runoff'
            lstrng=lenstr(strng)
            write (iutw,65) strng(1:lstrng),
     +         pthnam(1:lpthnam) 
            if (iupr.ne.iutw) write (iupr,65) strng(1:lstrng),
     +         pthnam(1:lpthnam)
65    format (' NOTE: Gridded ',a,' read from xmrg file ',a,'.')
            else
               strng='Flash Flood Guidance'
               lstrng=lenstr(strng)
               write (iutw,65) strng(1:lstrng),
     +            pthnam(1:lpthnam)
               if (iupr.ne.iutw) write (iupr,65) strng(1:lstrng),
     +            pthnam(1:lpthnam)
            endif
         bname=' '
         call upclos (idev,bname,istat)
         usrnamz = user(1:8)
c     check if FFG and xmgr file HRAP subsets match
         if (ksrow.ne.msrow.or.knrow.ne.nurow.or.
     +       kwcol.ne.mwcol.or.kncol.ne.nucol) then
            write (iutw,70) pthnam(1:lpthnam),
     +                      msrow,ksrow,nurow,knrow,
     +                      mwcol,kwcol,nucol,kncol
            if (iupr.ne.iutw) write (iupr,70) pthnam(1:lpthnam),
     +                      msrow,ksrow,nurow,knrow,
     +                      mwcol,kwcol,nucol,kncol
70    format (' WARNING: FFG HRAP subset does not match ',
     +      'that in xmrg file ',a,':' /
     +   20x,'FFG user  xmrg file' /
     +   20x,'--------  ---------' /
     +   10x,'south row  ',i5,3x,i5 /
     +   10x,'num rows   ',i5,3x,i5 /
     +   10x,'west col   ',i5,3x,i5 /
     +   10x,'num col    ',i5,3x,i5 /
     +   ' Resetting FFG HRAP subset to xmrg HRAP subset.')
            msrow = ksrow
            nurow = knrow
            mwcol = kwcol
            nucol = kncol
            endif
80       continue
c
      if (ibug.gt.0) then
         write (iud,90) kwcol,ksrow,kncol,knrow
90    format (' kwcol=',i4,' ksrow=',i4,' kncol=',i3,' knrow=',i3)
         write (iud,100) (aproc(i),i=1,nudur)
100      format (' aproc=',5(a8,1x))
         write (iud,110) usrnamz,sdatim,vdatim,mxval,xver
110   format (' usrnamz=',a,' sdatim=',a, 'vdatim=',a,
     +   ' mxval=',i4,' xver=',f5.2)
         endif
c
120   return
c
      end
