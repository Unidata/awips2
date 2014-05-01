c  =====================================================================
c  pgm:  gplabl (ltype)
c
c   in: ltype  .... label type
c  =====================================================================
c
      subroutine gplabl (ltype)
c
c.......................................................................
c
c  Generate column labels for flash flood guidance product.
c
c.......................................................................
c        Initially written by
c            Tim Sweeney, HRL - Apr 1992
c
c  Added capability to designate unique label for location based on
c  ltype
c            Tim Sweeney, HRL                          Oct 1995
c.......................................................................
c
      character*4 ltype
      character*6 caray(26),lbl(26)
      character*16 lzone,lcnty,lhead,lresv
c
      dimension pmin(5)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/uinfo'
      include 'prodgen_inc/poptns'
      include 'prodgen_inc/propar'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/gplabl.f,v $
     . $',                                                             '
     .$Id: gplabl.f,v 1.2 2004/01/30 17:57:02 scv Exp $
     . $' /
C    ===================================================================
C
      data lbl/ 'IDENT ','   1HR','   3HR','   6HR','  12HR',
     +          '  24HR','  NAME','      ','      ','      ',
     +          'STREAM','      ','      ',
     +          '======','= ====','  ====','  ====','  ====',
     +          '  ====','  ====','======','======','====  ',
     +          '======','======','======'  /
      data lzone/'  ZONE NAME'/
      data lcnty/'  COUNTY NAME'/
      data lhead/'  HEADWATER NAME'/
      data lresv/'  RESERVOIR NAME'/
c
c
      call prbug ('gplabl',1,1,ibug)
c
c  select customized label for location based on ltype
      if (ltype.eq.'zone'.or.ltype.eq.'ZONE') then
         call umemov (lzone,lbl(7),len(lzone)/4)
      else if (ltype.eq.'cnty'.or.ltype.eq.'CNTY') then
         call umemov (lcnty,lbl(7),len(lcnty)/4)
      else if (ltype.eq.'head'.or.ltype.eq.'HEAD') then
         call umemov (lhead,lbl(7),len(lhead)/4)
      else if (ltype.eq.'resv'.or.ltype.eq.'RESV') then
         call umemov (lresv,lbl(7),len(lresv)/4)
      else if (ltype.eq.'wsup'.or.ltype.eq.'WSUP') then
         go to 60
      endif
c
      if (dftyp.eq.'affg'.or.dftyp.eq.'AFFG') then
c     use grid minimum when default type is AFFG for zone/county
         do 10 i=1,ndur
            pmin(i) = pming(i)
10          continue
         else if (dftyp.eq.'hffg'.or.dftyp.eq.'HFFG') then
c        use headwater minimum when default type is HFFG for headwaters
c        and when headwater definition used for zone/county
            do 20 i=1,ndur
               pmin(i) = pminh(i)
20             continue
         endif
c
c  flash flood guidance label
      loc = 1
      caray(loc) = lbl(1)
      caray(loc+13) = lbl(14)
c  duration labels
      do 30 i=1,ndur
         if (pmin(i).ge.0.0) then
            loc = loc + 1
            caray(loc) = lbl(i+1)
            caray(loc+13) = lbl(i+14)
            endif
30       continue
c
      if (ndes.eq.1) then
c     desc and strnam labels
         if (nstr.eq.1) then
            k = 7
            else
               k = 4
            endif
         do 40 j=1,k
            caray(loc+j)    = lbl(j+6)
            caray(loc+j+13) = lbl(j+19)
40          continue
         loc = loc + k
         endif
c
      write (iuf,50) (caray(j),j=1,loc)
50    format (':',13a6)
      k = loc + 13
      write (iuf,50) (caray(j),j=14,k)
      go to 90
c
c  water supply guidance label
60    if (iwats.lt.2) then
         write (iuf,70) rnfl
70    format (':',17x,'RAINFALL' /
     +        ':','IDENT ',6f6.1,'  LOCATION  ' /
     +        ':','======= ==== ===== ===== =====',
     +            ' ===== =====  ================' )
         else
            write (iuf,80) rnfl
80    format (':',17x,'RAINFALL'  /
     +        ':','IDENT' ,4f7.1,2f8.1,1x,'  LOCATION  ' /
     +        ':','======= ===== ====== ======',
     +            ' ======',
     +            ' ======= =======  ================' )
         endif
c
90    return
c
      end
