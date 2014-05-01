c  =====================================================================
c  pgm:  ui2c4 (ival,id2)
c
c   in: ival   .... number consisting of 1 to 4 digits
c  out: id2    .... number changed to characters
c  =====================================================================
      subroutine ui2c4 (ival,id2)
c.......................................................................
c
c  this routine converts a number to character*4 word
c
c.......................................................................
c  initially written by
c       Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      character idc(4)
      character*4 id2
      character*8 sname
c
      include 'ffg_inc/gdebug'
c
      dimension id(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_util/RCS/ui2c4.f,v $
     . $',                                                             '
     .$Id: ui2c4.f,v 1.1 2001/08/16 17:42:50 dws Exp $
     . $' /
C    ===================================================================
C
c
      data sname/ 'ui2c4   '/
c
      call prbug (sname,1,4,ibug)
c
      ipos = 1
      iwidth = 4
      num = 1
      call ffi2a (idc,ipos,iwidth,num,ival)
c
      if (ival.lt.10) then
        id2 = idc(4)
      else if (ival.lt.100) then
        id2 = idc(3)//idc(4)
      else if (ival.lt.1000) then
        id2 = idc(2)//idc(3)//idc(4)
      else
        id2 = idc(1)//idc(2)//idc(3)//idc(4)
      end if
      if(ibug.gt.1) write(iud,7020) ival,id2
 7020 format(' ui2c4:  ival=',i4,3x,'id2=',a4)
      return
      end
