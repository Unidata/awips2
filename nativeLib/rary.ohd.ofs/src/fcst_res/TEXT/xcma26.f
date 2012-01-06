C MEMBER XCMA26
C  (from old member FCXCMA26)
C
C DESC COMPUTE AREAS FROM ELEVATION VS STORAGE CURVE
C----------------------------------------------------------------
      SUBROUTINE XCMA26(VAL,NVAL,ELEV,AREA)
C-----------------------------------------------------------------
C  THIS SUBROUTINE WILL COMPUTE AREAS FROM AN ELEVATION VS STORAGE CURVE
C---------------------------------------------------------------------
C  JTOSTROWSKI - HRL - MARCH 1983
C----------------------------------------------------------------
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/resv26'
C
      DIMENSION VAL(1),ELEV(1),AREA(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xcma26.f,v $
     . $',                                                             '
     .$Id: xcma26.f,v 1.1 1995/09/17 19:06:24 dws Exp $
     . $' /
C    ===================================================================
C
C
      NHALF = NVAL
      NSEC = NHALF+1
C
      AREA(1) = VAL(NSEC)
      ELEV(1) = VAL(1)
C
      NEND = NHALF-1
C
C  THE METHOD USED TO COMPUTE AREAS IS THE AVERAGE AREA BETWEEN TWO
C  CONSECUTIVE ELEVATIONS IS THE DIFFERENCE BETWEEN THE VOLUMES DIVIDED
C  BY THE DIFFERENCE IN THE ELEVATIONS. THE RESULTING AREA IS ASSIGNED
C  TO THE MIDPOINT BETWEEN THE ELEVATIONS.
C   _
C   A = DVOL/DELEV
C
      DO 20 I=1,NEND
      J = NHALF + I
      H2 = VAL(I+1) - VAL(1)
      H1 = VAL(I) - VAL(1)
      DH = H2 - H1
      V2 = VAL(J+1)
      V1 = VAL(J)
      DV = V2 - V1
C
C  UNITS OF AREA ARE CMSD*24/MINODT / M OR M2/S/D*24/MINODT. THESE UNITS
C  ALLOW AREA TO BE MULTIPLIED BY DEPTH (IN M) TO RESULT IN THE SAME
C  UNITS AS THOSE USED FOR STORAGE CONTENTS.
C
      ABAR = DV/DH
      AREA(I+1) = ABAR
      ELEV(I+1) = VAL(I) + DH/2.0
   20 CONTINUE
C
      IF (IBUG.GE.2) WRITE(IODBUG,1610) (ELEV(I),I=1,NHALF)
      IF (IBUG.GE.2) WRITE(IODBUG,1611) (AREA(I),I=1,NHALF)
 1610 FORMAT(/'   ** ELEVATION VS. AREA CURVE :'//,(6F12.3))
 1611 FORMAT((6F12.3))
      RETURN
      END
