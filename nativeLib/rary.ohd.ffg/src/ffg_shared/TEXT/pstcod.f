c  =====================================================================
c  pgm:  pstcod (istat,ident,type,iunit)
c
c  =====================================================================
c
      subroutine pstcod (istat,ident,type,iunit)
c
      character*4 type
      character*(*) ident
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/pstcod.f,v $
     . $',                                                             '
     .$Id: pstcod.f,v 1.4 2003/03/14 18:28:29 dws Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('pstcod  ',1,4,ibug)
c
      if (istat.le.0) then
         go to 60
         else if (istat.eq.1) then
ccc         write (iutw,*) 'WARNING: file not found.'
         else if (istat.eq.2) then
            write (iutw,10)
10    format (' ERROR: directory not found.')
         else if (istat.eq.3) then
            write (iutw,20) ident,type,iunit
20    format (' WARNING: end of file encountered for file=',a,
     +   ' type=',a,' unit=',i2)
         else if (istat.eq.4) then
            write (iutw,30) ident,type,iunit
30    format (' ERROR: while reading file=',a,
     +   ' type=',a,' unit=',i2)
         else if (istat.eq.5) then
            write (iutw,40) ident,type,iunit
40    format (' ERROR: attempting to write file=',a,
     +   ' type=',a,' unit=',i2)
         else if (istat.eq.6) then
            write (iutw,50) type(1:lenstr(type))
50    format (' ERROR: ',a,' is an invalid data type.')
         else
         endif
c
60    return
c
      end
