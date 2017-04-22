c  =====================================================================
c  block data blocbb
c
c  block data to set dimensions for basin boundary arrays in FFGS
c
c    current sizes
c          array    size
c          -----    ----
c          bx       5000
c          ibx      5000
c          nlrow    150
c          nlbeg    150
c          nlend    150
c  =====================================================================
      block data blocbb
c.......................................................................
c  dimensions set by Tim Sweeney, HRL                    Feb 1998
c
c  increased line segment work arrays from 70 to 100
c  arrays nlrow, nlbeg, nlend
c      Tim Sweeney, HRL                                  May 1998
c
c  increased line segment work arrays from 100 to 150
c  arrays nlrow, nlbeg, nlend
c      Tim Sweeney, HL                                   Mar 2001
c.......................................................................
c
      include 'ffg_inc/gbx'
      include 'ffg_inc/linseg'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSBLOCBB        / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/block/RCS/blocbb.f,v $
     . $',                                                             '
     .$Id: blocbb.f,v 1.1 2001/08/16 17:42:37 dws Exp $
     . $' /
C    ===================================================================
C
c
c.......................................................................
c
      data mbx /5000/
      data mlseg /150/
c
      end
      
