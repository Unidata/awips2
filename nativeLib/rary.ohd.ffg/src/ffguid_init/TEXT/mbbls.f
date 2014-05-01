c  =====================================================================
c  pgm:  mbbls (isl,mpo,po,ipo)
c
c   in: isl    .... edit mode (=1)
c   in: mpo    .... maximum size of array po
c   in: po     .... basin boundary parameters
c   in: ipo    .... working array
c  =====================================================================
c
      subroutine mbbls (isl,mpo,po,ipo)
c
c.......................................................................
c
c  define parameters for basin boundary line segments
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - March 1992
c.......................................................................
c
      character tent
      character*2 bname
      character*4 bbtyp,namtyp
      character*8 bbid
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
c
      dimension po(*),ipo(*)
      dimension lins(2000)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/mbbls.f,v $
     . $',                                                             '
     .$Id: mbbls.f,v 1.6 2004/09/13 14:23:51 scv Exp $
     . $' /
C    ===================================================================
C
      data bbtyp/ 'basn'/
      data namtyp/ 'anam'/
      data bname/ '  ' /
c
c
10    call prbug ('mbbls',1,1,ibug)
c
      call typent (tent)
      if (tent.eq.'m') go to 120
      if (tent.eq.'f') go to 10
c
c  enter basin boundary id
20    write (iutw,30)
30    format (' Enter basin boundary identifier: ',$)
      read (iutr,'(a)',err=20) bbid
c
c  get the file
      kod = 2
      call rppfil (bbid,bbtyp,kod,ipdv,mpo,po,npo,istat)
      if (istat.eq.0) then
         rewind (ipdv)
         nbpts = po(23)
         nsegs = po(24)
         ipnt = 25 +2*nbpts
c     transfer po to int*4 array
         call umemov (po,ipo,npo)
         do 50 i=1,nsegs
            j = (i-1)*3 + 1
            lins(j) = ipo(ipnt+i-1)
            k = ipnt + nsegs + (i-1)*2
            lins(j+1) = ipo(k)
            lins(j+2) = ipo(k+1)
50          continue
         if (ibug.gt.0) then
            write (iud,*) ' nbpts=',nbpts,' nsegs=',nsegs,
     +         ' ipnt=',ipnt,' npo=',npo
            k = ipnt + nsegs*3 - 1
            write (iud,60) (ipo(i),i=ipnt,k)
60    format (10i6)
            k = nsegs*3
            write (iud,70) (lins(i),i=1,k)
70    format (' lins=' / 70(/ 3i6))
            endif
         else if (istat.eq.1) then
            istat = 0
            isl = 1
            do 80 i=1,mpo
               po(i) = -999.0
80             continue
            nbpts = 30
         else
            go to 120
         endif
c
      inc = 3
      write (iutw,90) bbid
90    format (' Basin boundary line segments for ',a,':')
      call edviag (isl,iutw,lins,npo,inc,nsegs)
      if (isl.lt.3) then
         po(23) = nbpts
         po(24) = nsegs
         ipnt = 25 + 2*nbpts
c     transfer each int*4 row number
         nval = 1
         do 110 i=1,nsegs
            j = (i-1)*inc + 1
            call umemov (lins(j),po(ipnt+i-1),nval)
c        transfer begin and end column numbers
            np = ipnt + nsegs
            k = np + (i-1)*2
            call umemov (lins(j+1),po(k),nval)
            call umemov (lins(j+2),po(k+1),nval)
            if (ibug.ne.0) write (iud,100) lins(j),lins(j+1),lins(j+2),
     +         po(ipnt+i-1),po(k),po(k+1)
100   format (5x,3i6,4x,3f6.0)
110         continue
c     write to file
         ls = nsegs*3
         npo2 = ipnt + ls - 1
         call wppfil (ipdv,npo2,po,istat)
         call pstcod (istat,bbid,bbtyp,ipdv)
         endif
c
      call upclos (ipdv,bname,ic)
      go to 10
c
120   return
c
      end
