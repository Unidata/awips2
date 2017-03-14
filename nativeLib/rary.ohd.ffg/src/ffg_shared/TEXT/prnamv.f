c  =====================================================================
c  pgm:  prnamv (iform,idev)
c
c   in: iform  .... output format:
c                     1 = print title, program name, program version on
c                         multiple lines
c                     2 = print program name and version
c                     3 = print title, program name, program version on
c                         single line
c   in: idev   .... output device
c  =====================================================================
c
      subroutine prnamv (iform,idev)
c
c.......................................................................
c
c  Routine to print title, program name, program version number and
c  program version date.
c
c.......................................................................
c  Initially written by
c        Tim Sweeney, HRL                                     Feb 1998
c.......................................................................
c
      include 'upvrsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/prnamv.f,v $
     . $',                                                             '
     .$Id: prnamv.f,v 1.4 2004/01/30 17:52:12 scv Exp $
     . $' /
C    ===================================================================
C
c
      if (iform.eq.1) write (idev,10)
   10 format (
     +  10x,5x,'   N A T I O N A L   W E A T H E R   S E R V I C E'   //
     +  10x,5x,'F L A S H   F L O O D   G U I D A N C E   S Y S T E M')
C
      if (iform.le.2) write (idev,20) pgmnam(1:lenstr(pgmnam)),
     +                                pgmvrn(1:lenstr(pgmvrn)),
     +                                pgmvrd(1:lenstr(pgmvrd))
   20 format (/ 10x,5x,3x,'PROGRAM ',a,
     +        ' (VERSION: ',a,' - ',a,')')
c
      if (iform.eq.3) write (idev,30) pgmnam(1:lenstr(pgmnam)),
     +                                pgmvrn(1:lenstr(pgmvrn)),
     +                                pgmvrd(1:lenstr(pgmvrd))
   30 format (' NWS FLASH FLOOD GUIDANCE SYSTEM - PROGRAM ',a,
     +        ' (VERSION: ',a,' - ',a,')')
c
      return
c
      end
