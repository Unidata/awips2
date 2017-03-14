c=======================================================================
c
      subroutine edprod (ident,kpdv,is,mpo,po,istat)
c
c.......................................................................
c
c  Editor routine for product definitions
c
c.......................................................................
c  Initially written by
c           Tim Sweeney, HRL - Jan 1992
c.......................................................................
c
      character resp
      character*4 blnk,cnone
      character*8 ident
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'prodgen_inc/propar'
c
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_init/RCS/edprod.f,v $
     . $',                                                             '
     .$Id: edprod.f,v 1.5 2004/09/13 15:03:25 scv Exp $
     . $' /
C    ===================================================================
C
      data blnk/ '    '/
      data cnone/ 'none'/
c
c
      call prbug ('edprod',1,1,ibug)
c
c  set editor output device
      iout = iutw
c
10    write (iutw,20)
20    format (' Enter Product identifier: ',$)
      read (iutr,'(a)',err=10) ident
      if (ident.eq.' ') then
         istat=1
         go to 150
         endif
c
c  Open a Product contents file, get parameters from existing
c  product contents definition
      kod = 2
      call rppfil (ident,protyp,kod,kpdv,mpo,po,npo,istat)
      if (istat.eq.0) then
         rewind (kpdv)
         call getprd (po)
         call umemov (ident,prodid,2)
      else if (istat.eq.1) then
c     set default values for new file
         istat = 0
         call umemov (ident,prodid,2)
         pform = cnone
         pid(1) = cnone
         pid(2) = blnk
         pid(3) = blnk
         cct = cnone
         tzone = blnk
         wmo(1) = 'TTAA'
         wmo(2) = '00  '
         senof = cnone
         dftyp = 'HFFG'
         jdur = 6
         jdes = 0
         jstr = 0
         lwcol = 0
         lncol = 0
         lsrow = 0
         lnrow = 0
         nolid = 1
         alidty(1,1) = cnone
         alidty(2,1) = blnk
         alidty(3,1) = cnone
      else
         go to 150
      endif
c
30    write (iutw,40) prodid
40    format (20x,'Edit Product Parameters - ',2a4 /)
      write (iutw,50) pid,cct,wmo,senof,tzone,pform,dftyp,jdur
c
50    format (10x,'( 1) Product ID (MAX 9 CHAR):', 13x,2a4,a1 /
     +        10x,'( 2) Circuits (MAX 3 CHAR):               ',a4 /
     +        10x,'( 3) WMO ID (6 CHAR):                     ',2a4 /
     +        10x,'( 4) Sending office (4 CHAR):             ',a4 /
     +        10x,'( 5) Time zone (1 CHAR):                  ',a4 /
     +        10x,'( 6) Message format (SHEF or GRIB):       ',a4 /
     +        10x,'( 7) Default type (AFFG,GFFG,HFFG):       ',a4 /
     +        10x,'( 8) Longest duration (6, 12, or 24 hrs): ',i2)
c
      if (dftyp.ne.'GFFG'.and.dftyp.ne.'gffg' .and.
     1   dftyp.ne.'SIII'.and.dftyp.ne.'siii') go to 70
      write (iutw,60) jdes,lwcol,lncol,lsrow,lnrow
60    format (10x,'( 9) Pack GRIB bulletins ([0]-No 1-Yes):',2x,i2 /
     +        10x,'(10) West column:', 6x,  i4 /
     +        10x,'(11) Number of columns:',i4 /
     +        10x,'(12) South row:',   8x,  i4 /
     +        10x,'(13) Number of rows:',3x,i4  )
      go to 100
70    write (iutw,80) jdes,jstr,nolid
80    format (10x,'( 9) Description (0 - no, 1 - include):   ',i2 /
     +        10x,'(10) Stream name (0 - no, 1 - include):   ',i2 /
     +        10x,'(11) Number of Identifiers in product:   ',i3)
      n = nolid
ccc      if (n.ge.32) n = 32
      write (iutw,90) ((alidty(j,i),j=1,3),i=1,n)
90    format ((10x,5x,4(2a4,1x,a4,3x)))
c
100   write (iutw,110)
110   format (/ ' Select (number or <return>-exit): ',$)
      read (iutr,120,err=100) it,resp
120   format (i2,t1,a1)
c
      if (resp.eq.' ') go to 150
c
      if (it.eq.1) then
         call edvca (is,iutr,iout,3,pid)
      else if (it.eq.2) then
         call edvca (is,iutr,iout,1,cct)
      else if (it.eq.3) then
         call edvca (is,iutr,iout,2,wmo)
      else if (it.eq.4) then
         call edvca (is,iutr,iout,1,senof)
      else if (it.eq.5) then
         call edvca (is,iutr,iout,1,tzone)
      else if (it.eq.6) then
         call edvca (is,iutr,iout,1,pform)
      else if (it.eq.7) then
         call edvca (is,iutr,iout,1,dftyp)
      else if (it.eq.8) then
         call edvi (is,iutr,iout,jdur)
      else
         go to 130
      endif
      go to 30
c
130   if (dftyp.eq.'GFFG'.or.dftyp.eq.'gffg'.or.
     +    dftyp.eq.'SIII'.or.dftyp.eq.'siii') then
         if (it.eq.9) then
            call edvi (is,iutr,iout,jdes)
         else if (it.eq.10) then
            call edvi (is,iutr,iout,lwcol)
         else if (it.eq.11) then
            call edvi (is,iutr,iout,lncol)
         else if (it.eq.12) then
            call edvi (is,iutr,iout,lsrow)
         else if (it.eq.13) then
            call edvi (is,iutr,iout,lnrow)
         else
            go to 150
         endif
         go to 30
      else
         if (it.eq.9) then
            call edvi (is,iutr,iout,jdes)
         else if (it.eq.10) then
            call edvi (is,iutr,iout,jstr)
         else if (it.eq.11) then
c        location ids
            ib = 1
            idel = 99
            igrp = 3
140         call dispid (resp,ib,idel,igrp,nolid,alidty)
            if (resp.eq.' ') go to 30
            call edvid (iutr,iutw,resp,ib,igrp,nolid,alidty)
            go to 140
         else
            go to 150
         endif
         go to 30
      endif
c
150   return
c
      end
