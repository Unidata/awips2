c  ===============================================================
c  pgm:  getbb (bbid,bbtyp,mbx,bx,ibx,mlseg,nlseg,nlrow,
c               nlbeg,nlend,istat)
c
c   in: bbid   .... basin boundary identifier
c   in: bbtyp  .... data type code
c   in: mbx    .... number of words in basin boundary array bx
c   in: bx     .... basin boundary array
c   in: ibx    .... basin boundary integer array
c   in: mlseg  .... maximum number of line segments
c  out: nlseg  .... number of line segments
c  out: nlrow  .... array of row numbers
c  out: nlbeg  .... array of beginning column numbers
c  out: nlend  .... array of ending column numbers
c  out: istat  .... completion code
c  ===============================================================
c
      subroutine getbb (bbid,bbtyp,mbx,bx,ibx,mlseg,nlseg,nlrow,
     +                  nlbeg,nlend,istat)
c
c.................................................................
c
c  Routine to read an OFS basin boundary parameter record
c
c.................................................................
C  Initially written
c       Tim Sweeney, HRL                            Feb 1998
c.................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/count'
c
      character*8 bbid
      character*4 bbtyp
c
      dimension bx(*),ibx(*),nlrow(*),nlbeg(*),nlend(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/getbb.f,v $
     . $',                                                             '
     .$Id: getbb.f,v 1.4 2004/01/30 17:48:45 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('getbb',1,1,ibug)
c
      nlseg = 0
c
c  get basin boundary (line segment definitions)
      iptbb = 0
      call rpprec (bbid,bbtyp,iptbb,mbx,bx,nbfill,irecnx,istat)
      if (ibug.gt.0) write (iupr,10) bbid,bbtyp,iptbb,nbfill,istat
10    format (' bbid=',a,' bbtyp=',a,' iptbb=',i6,' nbfill=',i6,
     +   ' istat=',i4)
      if (istat.ne.0) then
            write (iutw,15) bbtyp,bbid
            if (iupr.ne.iutw) write (iupr,15) bbtyp,bbid
15    format (' ERROR: ',a,' parameter record not found for ',
     +   'identifier ',a,'.')
            go to 90
            endif
         if (istat.eq.2) then
         if (istat.eq.3) then
            write (iutw,20) bbtyp,bbid,nbfill,mbx
            if (iupr.ne.iutw) write (iupr,20) bbtyp,bbid,nbfill,mbx
20    format (' ERROR: number of words needed for ',a,
     +   ' parameter record for identifier ',a,
     +   ' (',i5,') exceeds maximum (',i5,').')
            go to 90
            endif
         write (iutw,30) bbid,istat
         if (iupr.ne.iutw) write (iupr,30) bbid,istat
30    format (' ERROR: reading basin boundary for ',a,
     +   '. rpprec istat=',i4)
         go to 90
         endif
c
c  grid spacing factor
      igsf = ifix( bx(22) )
      if (igsf.lt.1) igsf = 1
      nbpts = bx(23)
      nsegs = bx(24)
      ipnt  = 25 + 2*nbpts
      if (ibug.gt.0) write (iud,40) igsf,nbpts,nsegs,ipnt
40    format (' igsf=',i2,' nbpts=',i5,' nsegs=',i5,' ipnt=',i5)
c
c  move integer values from real array to integer array
       call umemov (bx,ibx,mbx)
c
c  check max line segments allowed
      lsneed = nsegs*igsf - igsf + 1
      if (lsneed.gt.mlseg) then
         write (iutw,50) mlseg,lsneed,nsegs,igsf
         if (iupr.ne.iutw) write (iupr,50) mlseg,lsneed,nsegs,igsf
50    format (' ERROR: basin boundary work array used ',i4,' line ',
     +      'segments but needs',i4,'.' /
     +   t8,'defined line segments=',i3,3x,'grid spacing factor=',i2)
         istat = 9
         endif
c
C  calculations for each line segment within area
      do 80 lseg=1,nsegs
         np    = ipnt + (lseg-1)
         nrow  = ibx(np)
         ncbeg = ibx(np+nsegs)
         ncend = ibx(np+2*nsegs)
         do 70 ixr=1,igsf
            if (ixr.gt.1) then
               nrow = nrow - 1
               if (lseg.eq.nsegs) go to 80
               endif
            if (ibug.gt.2) write (iupr,60) lseg,np,nrow,ncbeg,ncend
60    format (' lseg=',i4,' np=',i5,' nrow=',i5,' ncbeg=',i5,
     +   ' ncend=',i5)
            nlseg = nlseg + 1
            nlrow(nlseg) = nrow
            nlbeg(nlseg) = ncbeg
            nlend(nlseg) = ncend
70          continue
80       continue
c
90    return
c
      end
