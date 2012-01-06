C MEMBER FRCUN
C  (from old member FCFRCUN)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/08/94.08:27:33 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE FRCUN (FACLEN,FACVOL,FACMKM,IERSUM)
C
C SUBROUTINE TO DETERMINE CONVERSION FACTORS FOR DEFRC.
C AUTHOR - ED VANBLARGAN - 6/83
C
C ARGUMENT LIST:
C FACLEN - OUTPUT - LENGTH  CONVERSION (I.E., METERS TO FEET)
C FACVOL -   "    - VOLUME     "        "     CMS    TO CFS)
C FACMKM -   "    - DISTANCE   "        "     KM     TO MILES)
C IERSUM -   "    - ERROR FLAG =0, O.K.   =1, ERROR
C
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/frcun.f,v $
     . $',                                                             '
     .$Id: frcun.f,v 1.1 1995/09/17 18:55:05 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA UM,UCMS,UKM/4HM   ,4HCMS ,4HKM  /,DL3,DL/4HL3  ,4HL   /
C GET CONVERSIONS
      CALL FCONVT(UM,DL,STDE,FACLEN,SUMADD,IER)
      IERSUM=IERSUM+IER
      CALL FCONVT(UCMS,DL3,STDE,FACVOL,ADD,IER)
      IERSUM=IERSUM+IER
      SUMADD=SUMADD+ADD
      CALL FCONVT(UKM,DL,STDE,FACMKM,ADD,IER)
      IERSUM=IERSUM+IER
      SUMADD=SUMADD+ADD
C DONE WITH FCONVT
      IF(SUMADD.LT.0.001 .AND. SUMADD.GT.-0.001) GO TO 999
      WRITE(IPR,100)
      CALL ERROR
      IERSUM=IERSUM+1
C DONE
999   RETURN
100   FORMAT(1H0,10X,37H**ERROR** ADDITION CONSTANT FOR UNITS,
     $ ' CONVERSION NOT ZERO.')
      END
