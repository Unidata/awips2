c  =====================================================================
c  pgm:  stridx (iunit,mpo,po,ncidx,cidx,dtype)
c
c   in: iunit  .... unit number
c   in: mpo    .... number of words in array po
c   in: po     .... parameter array
c   in: ncidx  .... number of identifiers in cidx
c   in: cidx   .... array of identifiers
c   in: dtype .... index type
c  =====================================================================
c
      subroutine stridx (iunit,mpo,po,ncidx,cidx,dtype)
c
c.......................................................................
c
c  This routine copies the character index array to real array and
c  then writes it to a file.
c
c.......................................................................
c       Initially written by
c            Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      character*2 bname
      character*4 cidx(2,1),dtype
      character*8 filtyp
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/stridx.f,v $
     . $',                                                             '
     .$Id: stridx.f,v 1.5 2004/01/30 17:52:39 scv Exp $
     . $' /
C    ===================================================================
C
c
      ibug=0
c
      filtyp='index'
c
      if (ibug.eq.1) write (iud,*) 'in stridx - filtyp=',filtyp
      lfiltyp=lenstr(filtyp)
      if (ibug.eq.1) write (iud,*) 'in stridx - lfiltyp=',lfiltyp
      if (ibug.eq.1) write (iud,*) 'in stridx - dtype=',dtype
      ldtype=lenstr(dtype)
      if (ibug.eq.1) write (iud,*) 'in stridx - ldtype=',ldtype
c
c  copy to array
      do 20 i=1,ncidx
         j = (i-1)*2 + 1
         if (j.gt.mpo) then
            write (iutw,10) j,mpo,
     +         filtyp(1:lfiltyp),dtype(1:ldtype)
            if (iupr.ne.iutw) write (iupr,10) j,mpo,
     +         filtyp(1:lfiltyp),dtype(1:ldtype)
10    format (' ERROR: in stridx - number of words  (',i4,
     +   ') exceeds maximum (',i4,') for ',a,' type ',a,'.')
            istat = 1
            go to 30
            endif
         call umemov (cidx(1,i),po(j),2)
20       continue
c
c  write to file
      np = ncidx*2
      call wppfil (iunit,np,po,istat)
      call pstcod (istat,filtyp,dtype,iunit)
      bname = ' '
      call upclos (iunit,bname,istat)
c
      write (iutw,25) filtyp(1:lfiltyp),
     +   dtype(1:ldtype)
25    format (' NOTE: ',a,' file written for parameter type ',a,'.')
      if (iupr.ne.iutw) write (iupr,25) filtyp(1:lfiltyp),
     +   dtype(1:ldtype)    
c
30    return
c
      end
