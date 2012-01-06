c  ======================================================================
c  pgm:  gbparm (iupr,ibug,line,nw1,nw2,nw3,nfld1,fld2,fld3,ic)
c
c   in: iupr   ... unit number of output
c   in: ibug   ... debug output control
c   in: line   ... input character array
c   in: nw1    ... width of field nfld1
c   in: nw2    ... width of field fld2
c   in: nw3    ... width of field fld3
c  out: nfld1  ... integer value
c  out: fld2   ... character word 1
c  out: fld3   ... character word 2
c  out: ic     ... completion code
c  
c  ======================================================================
c
      subroutine gbparm (iupr,ibug,line,nw1,nw2,nw3,nfld1,fld2,fld3,ic)
c
c........................................................................
c
c  Routine parses grib parameter tables
c
c........................................................................
c  Initially written by
c     Tim Sweeney, HL                                       April 2000
c........................................................................
      character*(*) line,fld2,fld3
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/gbparm.f,v $
     . $',                                                             '
     .$Id: gbparm.f,v 1.1 2006/05/03 13:43:58 gsood Exp $
     . $' /
C    ===================================================================
C
c
c  get field 1 - parameter number in table
      iptr = 1
      call uffir (line,iptr,nw1,nfld1,r,nxt,ic)
c
c  get field 2 - parameter value
      iptr = nxt
      call uffch (line,iptr,nw2,fld2,nxt,ic)
c
      if (nw3.le.0) go to 10
c
c  get field 3
      iptr = nxt
      call uffch (line,iptr,nw3,fld3,nxt,ic)
c
10    return
c
      end
      
