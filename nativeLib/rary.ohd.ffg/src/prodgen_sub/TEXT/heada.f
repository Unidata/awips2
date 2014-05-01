c=======================================================================
      subroutine heada(idev)
c***********************************************************************
c  This routine ouputs the comms heading for AFOS
c
c***********************************************************************
c        Initially written by
c            Tim Sweeney, HRL - Aug 1992
c***********************************************************************
c
      include 'prodgen_inc/propar'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/heada.f,v $
     . $',                                                             '
     .$Id: heada.f,v 1.1 2001/08/16 17:43:06 dws Exp $
     . $' /
C    ===================================================================
C
c
c  First line:
      write(idev,20) pid,cct
c
c  2nd line:  WMO header and date/time
      if (wmo(1).eq.'TTAA') then
         write(idev,30) wmo,senof
      else
         write(idev,40) wmo,senof,(idt(i),i=5,10)
      end if
c
      return
c
   20 format('ZCZC ',2a4,a1,1x,a3)
   30 format(a4,a3,a4,1x,'DDHHMM')
   40 format(a4,a3,a4,1x,6i1)
c
      end
