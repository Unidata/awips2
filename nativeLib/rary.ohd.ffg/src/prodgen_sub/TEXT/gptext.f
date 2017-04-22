c  =====================================================================
c  pgm:  gptext (po)
c
c   in: po     .... array of text type parameters
c
c  =====================================================================
c
      subroutine gptext(po)
c
c.......................................................................
c
c  Generate text for a product as defined by parameter type TEXT.
c  Several keywords within the text provide special features.
c
c.......................................................................
c        Initially written by
c            Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'prodgen_inc/propar'
      include 'prodgen_inc/txtpar'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/gptext.f,v $
     . $',                                                             '
     .$Id: gptext.f,v 1.2 2004/01/30 17:57:25 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('gptext',1,1,ibug)
c
c  get parameters
      call gettxt (po)
c
c  write lines to output file
      do 30 i=1,nlines
         ib = (i-1)*18 + 1
         ie = 18*i
         j = ib + 1
c     check for ISSUED
         if (atext(ib).eq.'ISSU'.or.atext(ib).eq.'issu') then
            call gpissu
c     check for .B header
         else if (atext(ib).eq.'dotb'.or.atext(ib).eq.'DOTB') then
c        convert to individual digits
            if (atext(j+1).eq.' ') then
               call gpdotb (atext(j),dftyp)
               else
                  call gpdotb (atext(j),atext(j+1))
               endif
c     check for column label
         else if (atext(ib).eq.'labl'.or.atext(ib).eq.'LABL') then
            call gplabl (atext(j))
c     check for UGC
         else if (atext(ib).eq.'ugc '.or.atext(ib).eq.'UGC ') then
            call gpugc (j)
c     just text
         else
            if (ibug.eq.1) write (iutw,10) (atext(j),j=ib,ie)
10    format (' in gptext - (atext(j),j=ib,ie)=',18a4)
            write (iuf,20) (atext(j),j=ib,ie)
20    format (18a4)
            endif
30       continue
c
      return
c
      end
