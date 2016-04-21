c=======================================================================
c
      subroutine edtext (ident,kpdv,is,mpo,po,istat)
c
c.......................................................................
c
c  Editor routine for text definitions
c
c.......................................................................
c  Initially written by
c          Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      character*8 ident
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'prodgen_inc/txtpar'
c
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_init/RCS/edtext.f,v $
     . $',                                                             '
     .$Id: edtext.f,v 1.4 2004/09/13 15:03:28 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('edtext',1,1,ibug)
c
      nperg = 18
c
10    write (iutw,20)
20    format (' Enter Text identifier: ',$)
      read (iutr,'(a)',err=10) ident
c
c  Open a TEXT file, read file
      kod = 2
      call rppfil (ident,txtyp,kod,kpdv,mpo,po,npo,istat)
      if (istat.eq.0) then
         rewind (kpdv)
         call gettxt (po)
         call umemov (ident,txtid,2)
      else if(istat.eq.1) then
c     set default values for new file
         istat = 0
         call umemov (ident,txtid,2)
         is = 1
         nlines = 1
         atext(1) = 'none'
         do 30 i=2,18
            atext(i) = ' '
30          continue
         npo = 18
      else
         go to 70
      endif
c
      write (iutw,50) txtid
50    format (15x,'Edit Text Parameters - ',2a4 /)
      write (iutw,60) nlines
60    format (5x,'Lines of text are: ',i2)
c
      iout = iutw
      mpt = nperg*nlines
      call edvcag (is,iutr,iout,mpt,atext,nperg,nlines)
c
70    return
c
      end
