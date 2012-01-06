c  =====================================================================
c  pgm:  btype
c
c  =====================================================================
      subroutine btype
c***********************************************************************
c  Routine fills common blocks with data types
c
c***********************************************************************
c  Initially written by
c     Tim Sweeney - HRL                             Sept 1995
c***********************************************************************
c
      include 'ffg_inc/arparm'
      include 'ffg_inc/hwparm'
      include 'ffg_inc/igparm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/btype.f,v $
     . $',                                                             '
     .$Id: btype.f,v 1.1 2001/08/16 17:42:38 dws Exp $
     . $' /
C    ===================================================================
C
c
      artyp  = 'affg'
      hdwtyp = 'hffg'
      igptyp = 'gdpm'
c
      return
      end
      
