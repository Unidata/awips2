c  =====================================================================
c  pgm:  wppfil (iunit,npo,po,istat)
c
c   in: iunit  .... device number
c   in: npo    .... number of words in array po
c   in: po     .... array containing data and/or parameters
c  out: istat  .... status code
c  =====================================================================
c
      subroutine wppfil (iunit,npo,po,istat)
c
c.......................................................................
c
c  This routine writes parameters to a file.
c
c.......................................................................
c  Initially written by
c         Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      include 'ffg_inc/count'
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
c
      character*4 type
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/wppfil.f,v $
     . $',                                                             '
     .$Id: wppfil.f,v 1.1 2003/08/20 13:16:20 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('wppfil',1,3,ibug)
c
      istat = 0
c
ccc      mpo2 = 16
      mpo2 = npo   
      if (ibug.eq.1) write (iud,10) iunit,npo,mpo2,(po(i),i=1,mpo2)
10    format (' in wppfil - iunit=',i2,' npo=',i6,' mpo2=',i6,
     +   ' (po(i),i=1,mpo2)=',(/1x,8f8.2))
c
c  write to file
      write (iunit,err=20) npo,(po(i),i=1,npo)
      go to 40
20    istat = 5
      write (iutw,30) npo,iunit,type
      if (iupr.ne.iutw) write (iupr,30) npo,iunit,type
30    format (' ERROR: writing ',i4,' words to unit ',i3,
     +   ' for type ',a,'.')
      nerr = nerr + 1
c
40    return
c
      end
