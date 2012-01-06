C MEMBER OPUGAJ
C  (from old member FCSTDUMY)
C
      SUBROUTINE OPUGAJ (H,V,XIN,OUT,W,N,ISTAT)
      DIMENSION XIN(1),OUT(1)
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/opugaj.f,v $
     . $',                                                             '
     .$Id: opugaj.f,v 1.1 1995/09/17 19:04:42 dws Exp $
     . $' /
C    ===================================================================
C
      WRITE(IPR,600)H,V,N
  600 FORMAT(1H0,10X,'**WARNING** IN UHGADJ MOD - YOU ARE USING ',
     1 'A DUMMY SUBROUTINE OPUGAJ.'/11X,
     2 'NO ADJUSTMENTS WILL BE MADE TO THE UNIT-HYDROGRAPH.'/11X,
     3 'HADJ=',F9.2,', VADJ=',F9.2,', NORDS=',I4)
      CALL WARN
      DO 10 I=1,N
   10 OUT(I)=XIN(I)
      ISTAT=0
      RETURN
      END
