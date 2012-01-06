c  =====================================================================
c  pgm:  rppfil (ident,type,kod,iunit,mpo,po,npo,istat)
c
c   in: ident  .... identifier which becomes the filename
c   in: type   .... data type code
c   in: kod    .... code for handling file not found:
c                     1 =  error message
c                     2 =  prompt to create
c                     3 =  create file without prompt
c  out: iunit  .... unit number
c   in: mpo    .... maximum words in array po
c  out: po     .... parameter array
c  out: npo    .... number of words in array po
c  out: istat  .... status code
c
c  =====================================================================
c
      subroutine rppfil (ident,type,kod,iunit,mpo,po,npo,istat)
c
c  .....................................................................
c
c  This routine reads parameters from file
c
c  .....................................................................
c  Initially written by
c       Tim Sweeney, HRL - Apr 1992
c  .....................................................................
c
      character*4 type
      character*4 accmode
      character*(*) ident
      character*128 pthnam
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
c
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/rppfil.f,v $
     . $',                                                             '
     .$Id: rppfil.f,v 1.6 2004/01/30 17:52:22 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('rppfil',1,3,ibug)
c
      if (ibug.eq.1) write (iud,*) 'in rppfil - ident=',ident,
     +   ' type=',type,' kod=',kod
c
c  open file
      accmode = 'r'
      call fixopn (ident,type,pthnam,accmode,kod,iunit,istat)
      if (istat.ne.0) then
c     check if new file
         if (istat.eq.-1.and.kod.eq.2) then
            istat = 1
            else
               write (iutw,10) pthnam(1:lenstr(pthnam))
10    format (' WARNING: file ',a,' not found.')
            endif
         go to 80
         endif
c
c  read file
      read (iunit,end=50,err=60) npo,(po(i),i=1,npo)
      if (ibug.gt.0) then
         write (iud,*) 'in rppfil - npo=',npo
         write (iud,20) (po(i),i=1,npo)
20    format (' in rppfil - po in f8.2:' / (10(1x,f8.2)))
         write (iud,30) (po(i),i=1,npo)
30    format (' in rppfil - po in a4:' / (10(1x,a4)))
         endif
      if (mpo.gt.0.and.npo.gt.mpo) then
         write (iutw,40) npo,mpo,iunit
         if (iupr.ne.iutw) write (iupr,40) npo,mpo,iunit
40    format (' ERROR: in rppfil - number of words  (',i5,
     +   ') exceeds maximum (',i5,') for unit ',i3,'.')
            istat = 1
         endif
      go to 80
c
c  end of file
50    istat = 3
      if (npo.lt.5) istat = 1
      go to 70
c
c  read error
60    istat = 4
c
70    if (istat.ne.0) call pstcod (istat,ident,type,iunit)
c
80    return
c
      end

