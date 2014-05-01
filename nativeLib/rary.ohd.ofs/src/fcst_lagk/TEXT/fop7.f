C MODLUE FOP7
C-----------------------------------------------------------------------
C
      LOGICAL FUNCTION FOP7 (X,Y)
C
C.......................................................................
C
C  THIS FUNCTION CHECKS IF THE LAG AND/OR K FUNCTIONS ARE TURNED ON.
C  RETURNS TRUE IF ON, FALSE IF OFF.
C.......................................................................
C
C  FUNCTION ORIGINALLY PROGRAMMED BY
C           GEORGE F. SMITH - HRL  DECEMBER 1979
C.......................................................................
C
C  VARIABLES IN ARGUMENT LIST
C
C    TO CHECK FOR LAG OPERATION:
C       1. X - P(19)
C       2. Y - P(20)
C
C    TO CHECK FOR K OPERATION:
C       1. X - P(P(18))
C       2. Y - P(P(18)+1)
C.......................................................................
C
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_lagk/RCS/fop7.f,v $
     . $',                                                             '
     .$Id: fop7.f,v 1.2 2000/03/13 20:47:34 page Exp $
     . $' /
C    ===================================================================
C
C
      FOP7=.TRUE.
C
      IF (X.LE.0.005.OR.X.GE.0.015) RETURN
      IF (Y.LE.-0.005.OR.Y.GE.0.005) RETURN
C
      FOP7=.FALSE.
C
CCC     WRITE (IODBUG,*) 'FOP7=',FOP7
C
      RETURN
C
      END
