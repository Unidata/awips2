C$PRAGMA C (SBPTNPLY)
C$PRAGMA C (SBPTNEDG)
C MODULE SBBLDSEG
C Written by Hank Herr on Jan 13, 2005
C This routine determines the segments within a basin given basin
C boundaries.  See below for argument list.
C----------------------------------------------------------------------
C
      SUBROUTINE SBBLDSEG (X,Y,NBPTS,IY,IXB,IXE,MSEGS,IYP,LFACTR,ISTAT)
C
C  The routine performs a simple algorithm.  It bounds the X and Y axes,
C  finds all integer values within the bounds, looks through all pairs
C  of X and Y resulting from these values, determines if they are either
C  in the basin polygon or on its edge and, if so, adds them to the 
C  list of segments.
C
C  This uses the lower left corner to determine if a grid is in a basin.
C  To use center, see the "NOTE:" section below.
C

      DIMENSION X(NBPTS),Y(NBPTS)
      DIMENSION IY(MSEGS),IXB(MSEGS),IXE(MSEGS)

C     Stores the integer bounds on X and Y.
      INTEGER MINY
      INTEGER MAXY
      INTEGER MINX
      INTEGER MAXX
      
C     Working space for IY, IXB, and IXE arrays.
      INTEGER IYTMP(1000)
      INTEGER IXBTMP(1000)
      INTEGER IXETMP(1000)
      INTEGER NTEMP
      INTEGER IYPTMP
      
C     Loop counters.  ORDX is real version of LOOPX, same for Y.
      INTEGER LOOPX
      INTEGER LOOPY
      REAL    ORDX
      REAL    ORDY
      REAL    USEDORDX
      REAL    USEDORDY
      
C     Stores return values from poly and edge routines.
      INTEGER RETVAL1
      INTEGER RETVAL2
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbbldseg.f,v $
     . $',                                                             '
     .$Id: sbbldseg.f,v 1.2 2005/08/25 14:56:50 hank Exp $
     . $' /
C    ===================================================================
C
      
C     Find integers that bound the X and Y coordinates.  
      MINY=Y(1)
      MAXY=(Y(1) + 1.00001)
      MINX=X(1)
      MAXX=(X(1) + 1.00001)
      DO 20 I=1,NBPTS-1
        IF(Y(I).LT.MINY) MINY=Y(I)
        IF(Y(I).GT.MAXY) MAXY=(Y(I) + 1.00001)
        IF(X(I).LT.MINX) MINX=X(I)
        IF(X(I).GT.MAXX) MAXX=(X(I) + 1.00001)
20    CONTINUE
      
      NTEMP=0
      IYPTMP=1
      IYP=1

C     Loop through every integer Y value within the bounds for Y
C     counting down from MAXY.
      LOOPY=MAXY
      DO 50
      
        NTEMP=0
        IYPTMP=1
        ORDY=LOOPY
      
C       For every integer X value within the bounds of X, counting
C       down from MAXX.
        LOOPX=MAXX
        DO 30
          ORDX=LOOPX
          
C         NOTE: If we ever decide to use the middle of an HRAP grid as
C         opposed to the lower left hand corner, the easiest way to do
C         it is to uncomment the following two lines:
          USEDORDX=ORDX+0.5
          USEDORDY=ORDY+0.5

C         If the combination of (ORDX,ORDY) is within the basin polygon
C         or on its edte, add it to the temporary version of IY,
C         IXB, and IXE.
          CALL SBPTNEDG(NBPTS,X,Y,USEDORDX,USEDORDY,RETVAL1)
          CALL SBPTNPLY(NBPTS,X,Y,USEDORDX,USEDORDY,RETVAL2)
          IF (RETVAL1.EQ.1.OR.RETVAL2.EQ.1) THEN
            NTEMP=NTEMP+1
            IYPTMP=IYPTMP+1
            IYTMP(NTEMP)=LOOPY
            IXBTMP(NTEMP)=LOOPX
            IXETMP(NTEMP)=LOOPX
          ENDIF
          
          LOOPX=LOOPX-1
          IF(LOOPX.LT.MINX) GO TO 35
30      CONTINUE

C       If some points were found for this value of Y, then filter
C       the points, combining segments together, and add the 
C       filtered results to the permanent IY, IXB, and IXE arrays.
35      IF (NTEMP.GT.0) THEN
          CALL SBFILTER(IYTMP,IXBTMP,IXETMP,NTEMP,IYPTMP,IERR)
          DO 40 J=1,NTEMP
            IY(IYP)=IYTMP(J)
            IXB(IYP)=IXBTMP(J)
            IXE(IYP)=IXETMP(J)
            IYP=IYP+1
40        CONTINUE
        ENDIF
      
        LOOPY=LOOPY-1
        IF(LOOPY.LT.MINY) GO TO 60
50    CONTINUE
 
C           
C
60    RETURN
     
      END

