c  =====================================================================
c  pgm:  getidx (idxtyp,iunit,mcidx,po,ncidx,cidx,istat)
c
c   in: idxtyp .... index type
c   in: iunit  .... unit number
c   in: mcidx  .... maximum number of identifiers in array cidx
c   in: po     .... parameter array
c  out: ncidx  .... number of identifiers in cidx
c  out: cidx   .... array of identifiers
c  out: istat  .... status code
c  =====================================================================
c
      subroutine getidx (idxtyp,iunit,mcidx,po,ncidx,cidx,istat)
c
c.......................................................................
c
c  This routine reads index records from file and fills the character 
c  index array.
c
c.......................................................................
c        Initially written by
c            Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      character*4 idxtyp
      character*8 filtyp,cidx(*)
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/getidx.f,v $
     . $',                                                             '
     .$Id: getidx.f,v 1.3 2003/08/20 13:10:36 scv Exp $
     . $' /
C    ===================================================================
C
c
      ibug = 0
c
      if (ibug.gt.0) write (iud,*) 'in getidx : mcidx=',mcidx
c
c  read index file
      filtyp='index'
      kod = 0
      mpo = 0
      call rppfil (filtyp,idxtyp,kod,iunit,mpo,po,npo,istat)
      if (istat.eq.0) then
         rewind (iunit)
         ncidx = npo/2
         if (ncidx.gt.mcidx) then
            lfiltyp=lenstr(filtyp)
            lidxtyp=lenstr(idxtyp)
            write (iutw,10) ncidx,mcidx,
     +         filtyp(1:lfiltyp),idxtyp(1:lidxtyp)
            if (iupr.ne.iutw) write (iupr,10) ncidx,mcidx,
     +         filtyp(1:lfiltyp),idxtyp(1:lidxtyp)
10    format (' ERROR: in getidx - number of identifiers (',i4,
     +   ') exceeds maximum (',i4,') for ',a,' type ',a,'.')
            istat = 1
            ncidx = mcidx
            endif
         do 20 i=1,ncidx
            j = (i-1)*2 + 1
            call umemov (po(j),cidx(i),2)
20          continue
         else
c        new - initialize index array
            ncidx = 1
            cidx(1) = 'nonenone'
         endif
c
      if (ibug.gt.0) write (iud,*) 'in getidx : ncidx=',ncidx
c
      return
c
      end
