C MODULE SFBDRV
C----------------------------------------------------------------------
C
      SUBROUTINE SFBDRV (X,Y,FLAT,FLON,IY,IXB,IXE,MSEGS,NBPTS,LFACTR,
     *   AREA,UAREA,CAREA,XC,YC,UNITS,NSEGS,ISTAT)
C
C  THIS IS THE DRIVER ROUTINE FOR THE COMPUTATION OF HRAP GRID
C  SEGMENTS IN A BASIN.
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
      CHARACTER*4 UNITS
      DIMENSION X(NBPTS),Y(NBPTS),FLAT(NBPTS),FLON(NBPTS)
      DIMENSION XX(NBPTS+1),YY(NBPTS+1)
      DIMENSION IY(MSEGS),IXB(MSEGS),IXE(MSEGS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfbdrv.f,v $
     . $',                                                             '
     .$Id: sfbdrv.f,v 1.5 2005/01/14 20:21:05 hank Exp $
     . $' /
C    ===================================================================
C
C
      
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,80)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('BASN')
C
      ISTAT=0
C
      ILLGD=1
      CALL SBLLGD (FLON,FLAT,NBPTS,X,Y,ILLGD,IERR)
      CALL SBPTCK (X,Y,FLAT,FLON,NBPTS,MSEGS,IERR)
      IF (IERR.EQ.1) GO TO 60
C
C  COMPUTE GRID DENSITY FACTOR.
      CALL SBFMIN (FLAT,NBPTS,YMIN,IERR)
      CALL SBFMAX (FLAT,NBPTS,YMAX,IERR)
      CALL SBCNTR (YMIN,YMAX,YCENTR,IERR)
      CALL SBGRDL (YCENTR,GRIDL,IERR)
      CALL SBAREA (X,Y,NBPTS,GRIDL,NGRID,CAREA,IERR)
      IF (IERR.EQ.1) GO TO 60
C
CCC      CALL SBFCTR (NGRID,LFACTR,IERR)
      LFACTR=1
      
      IF (AREA.LT.-996.9.AND.AREA.GT.-997.1) GO TO 40
C
C  COMPARE INPUT AND COMPUTED AREAS
      UAREA=AREA
      IF (UNITS.EQ.'ENGL') THEN
         CALL UDUCNV ('MI2 ','KM2 ',1,1,AREA,UAREA,IERR)
         ENDIF
      DIF=ABS((UAREA-CAREA)/UAREA)
      IF (DIF.LT..05) GO TO 40
         IDIF=DIF*100+.99
         IF (UNITS.EQ.'ENGL') THEN
            CALL UDUCNV ('KM2 ','MI2 ',1,1,CAREA,ECAREA,IERR)
            WRITE (LP,90) ECAREA,'MI2',AREA,'MI2',IDIF
            CALL SUWRNS (LP,2,NUMERR)
            ELSE
               WRITE (LP,90) CAREA,'KM2',UAREA,'KM2',IDIF
               CALL SUWRNS (LP,2,NUMERR)
            ENDIF
C
40    DO 50 I=1,MSEGS
         IY(I)=9999
         IXB(I)=9999
         IXE(I)=9999
50       CONTINUE
C

CHH Uses new code to fix bug r25-29 (Hank 2005-01-11).
CHH First I must make sure X and Y begin and end at the same
CHH point.  So, I'll create new arrays and add one more point if
CHH needed.
      DO 55 I=1,NBPTS
        XX(I)=X(I)
        YY(I)=Y(I)
55    CONTINUE
      NNBPTS=NBPTS
C     Here is the point adding section...
      IF (X(1).NE.X(NBPTS).OR.Y(1).NE.Y(NBPTS)) THEN
        NNBPTS=NNBPTS+1
        XX(NNBPTS)=X(1)
        YY(NNBPTS)=Y(1)
      ENDIF

CHH Call SBBLDSEG to acquire the grid segments in the basin.
      IYP=1
      CALL SBBLDSEG(XX,YY,NNBPTS,IY,IXB,IXE,MSEGS,IYP,LFACTR,IERR)
      IF (IERR.GT.0) GO TO 60
      NSEGS=IYP-1
      
CHH This code comes from sbpair.f.  If no segments are in the basin,
CHH it will find the closest one and use it.  Otherwise, I need to run
CHH the filtering algorithm to merge segments if possible.
160   IF (NSEGS.LE.0) THEN
         WRITE (LP,260)
         CALL SUWRNS (LP,2,-1)
         NNBPTS=1
         ILLGD=1
         CALL SBLLGD (FLON,FLAT,NNBPTS,XGRID,YGRID,ILLGD,ISTAT)
         IY(1)=YGRID
         IXB(1)=XGRID
         IXE(1)=XGRID
         IYP=2
         NSEGS=IYP-1
      ENDIF
      
C
C  COMPUTE BASIN CENTROID
      CALL SBCNTD (IY,IXB,IXE,NSEGS,XC,YC,LFACTR,IERR)
C
60    IF (IERR.GT.0) ISTAT=1
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,100)
         CALL SULINE (IOSDBG,1)
         ENDIF 
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER SFBDRV')
90    FORMAT ('0*** WARNING - THE COMPUTED  AREA (',F7.1,' ',A,
     *   ') DIFFERS FROM THE SPECIFIED AREA (',F7.1,' ',A,') BY ',I5,
     *   ' PERCENT.')
100   FORMAT (' *** EXIT SFBDRV')
260   FORMAT ('0*** WARNING - NO GRID POINTS FALL WITHIN THE BASIN ',
     *   'BOUNDARY. THE CLOSEST GRID POINT WILL BE STORED.')
C
      END
