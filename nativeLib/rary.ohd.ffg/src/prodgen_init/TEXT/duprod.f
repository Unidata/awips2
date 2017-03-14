c  =====================================================================
c  pgm:  duprod (po,iout)
c
c   in: po     .... parameter array
c   in: iout   .... output device
c  =====================================================================
c
      subroutine duprod (po,iout)
c
c.......................................................................
c
c  This routine dumps parameters that define a product--heading and
c  content--(data type PROD) to a file in the same format that can be
c  used to define a product.
c
c.......................................................................
c  Initially written by
c           Tim Sweeney, HRL - Aug 1992
c.......................................................................
c
      parameter (npsid=6)
      character*4 psid(npsid)
c
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'prodgen_inc/propar'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_init/RCS/duprod.f,v $
     . $',                                                             '
     .$Id: duprod.f,v 1.2 2004/01/30 17:55:56 scv Exp $
     . $' /
C    ===================================================================
C
      data psid / 'ISSU','LABL','SKIP','issu','labl','skip'/
c
c
      call prbug ('duprod',1,1,ibug)
c
c  get parameters
      call getprd (po)
c
      write (iout,10) protyp,prodid,pform,dftyp,pid,cct,
     +   wmo,senof,tzone,jdur,jdes,jstr
10    format (a4,1x,2a4,1x,a4,1x,a4,1x,2a4,a1,1x,a3,1x,
     +   a4,a2,1x,a4,1x,a1,3(1x,i3))
c
      if (dftyp.eq.'GFFG'.or.dftyp.eq.'gffg'.or.
     +    dftyp.eq.'SIII'.or.dftyp.eq.'siii') then
         write (iout,20) lwcol,lncol,lsrow,lnrow
20    format (4x,4i4)
         else
c        add 'ENDID' as last id before listing product ids
            k = nolid + 1
            alidty(1,k) = 'ENDI'
            alidty(2,k) = 'D   '
            alidty(3,k) = ' '
c        check that data type is specified as needed
            do 40 i=1,nolid
               if (alidty(3,i).eq.'    ') then
c              keywords do not use data type but need to fill field
                  do 30 j=1,npsid
                     if (alidty(1,i).eq.psid(j)) then
                        alidty(3,i) = 'none'
                        go to 40
                        endif
30                    continue
c              otherwise insert default data type
                  alidty(3,i) = dftyp
                  endif
40             continue
            write (iout,50) ((alidty(j,i),j=1,3),i=1,k)
50    format ( (4x,5(1x,2a4,1x,a4) ) )
            endif
c
      return
c
      end
