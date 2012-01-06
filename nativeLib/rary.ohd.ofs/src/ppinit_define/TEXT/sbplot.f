C MODULE SBPLOT
C-----------------------------------------------------------------------
C
      SUBROUTINE SBPLOT (IPTYPE,ICOL,IY,IXB,IXE,NSEGS,X,Y,JX,JY,NBPTS,
     *   LFACTR,XC,YC,JN,ISTAT)
C
C  THIS ROUTINE PLOTS THE GRID POINTS FOR A BASIN BOUNDARY.
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
      CHARACTER*132 IPLT
      DIMENSION IY(NSEGS),IXB(NSEGS),IXE(NSEGS)
      DIMENSION X(NBPTS),Y(NBPTS),JX(NBPTS),JY(NBPTS),JN(NBPTS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbplot.f,v $
     . $',                                                             '
     .$Id: sbplot.f,v 1.2 1999/07/06 11:43:04 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBPLOT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NSEGS=',NSEGS
         CALL SULINE (IOSDBG,1)
         DO 10 I=1,NSEGS
            WRITE (IOSDBG,'(1X,A,I4,4(1X,A,I4))')
     *         'I=',I,'IY(I)=',IY(I),'IXB(I)=',IXB(I),'IXE(I)=',IXE(I)
            CALL SULINE (IOSDBG,1)
10          CONTINUE
         ENDIF
C
      DO 20 I=1,NBPTS
         JN(I)=I
20       CONTINUE
C
      IF (IPTYPE.NE.1) GO TO 40
C
C  ROUND USER SPECIFIED BOUNDARY POINT TO NEAREST GRID POINT
      DO 30 I=1,NBPTS
         JX(I)=X(I)+0.5
         JY(I)=Y(I)+0.5
30       CONTINUE
C
C  SORT POINTS INTO DESCENDING ORDER
40    CALL SBSORT (JY,JX,JN,NBPTS,ISTAT)
C
C  COMPUTE FIRST COLUMN OF PLOT
      CALL SBIMIN (IXB,NSEGS,IXBMIN,ISTAT)
      CALL SBIMAX (IXE,NSEGS,IXEMAX,ISTAT)
      CALL SBIMIN (JX,NBPTS,JXMIN,ISTAT)
      CALL SBIMAX (JX,NBPTS,JXMAX,ISTAT)
C
      IXMIN=IXBMIN
      IF (JXMIN.LT.IXBMIN) IXMIN=JXMIN
      IXMAX=IXEMAX
      IF (JXMAX.GT.IXEMAX) IXMAX=JXMAX
      IXDIF=IXMAX-IXMIN
C
      IDIF=(ICOL-IXDIF)/2
      IF (IDIF.LT.0) IDIF=0
      JDIF=IXMIN-IDIF
C
      IXC=XC+0.5
      IYC=YC+0.5
C
      WRITE (LP,160)
      CALL SULINE (LP,1)
C
      NXY=1
      IJ=1
C
C  SET INITIAL ROW COUNTER
      IROW=IY(1)
      IF (JY(1).GT.IROW) IROW=JY(1)
C
50    IPLT=' '
C
C  CHECK FOR GRID POINTS ON THIS ROW
70    IF (IJ.GT.NSEGS) GO TO 100
      IF (IY(IJ).NE.IROW) GO TO 100
      LB=IXB(IJ)-JDIF
      LE=IXE(IJ)-JDIF
C
      IF (LB.GE.1.AND.LE.LE.ICOL) GO TO 80
         WRITE (LP,170)
         CALL SUWRNS (LP,2,-1)
         IF (LB.LT.1) LB=1
         IF (LB.GT.ICOL) LB=ICOL
         IF (LE.LT.1) LB=1
         IF (LE.GT.ICOL) LE=ICOL
C
80    DO 90 L=LB,LE,LFACTR
         IPLT(L:L)='*'
90       CONTINUE
      IJ=IJ+1
      GO TO 70
C
C  CHECK FOR CENTROID ON THIS ROW
100   IF (IYC.NE.IROW) GO TO 120
      IXCJ=IXC-JDIF
      IF (IXCJ.GE.1.AND.IXCJ.LE.ICOL) GO TO 110
         WRITE (LP,170)
         CALL SUWRNS (LP,2,-1)
      IF (IXCJ.LT.1) IXCJ=1
      IF (IXCJ.GT.ICOL) IXCJ=ICOL
C
110   IPLT(IXCJ:IXCJ)='@'
C
C  CHECK FOR USER SPECIFIED POINTS OR STATION WEIGHTS
120   IF (NXY.GT.NBPTS) GO TO 130
      IF (JY(NXY).NE.IROW) GO TO 130
      LLP=JX(NXY)-JDIF
C
      CALL SBLETR (IPTYPE,ICOL,JN(NXY),IPLT,LLP,ISTAT)
      NXY=NXY+1
      GO TO 120
C
130   WRITE (LP,'(1X,A)') IPLT(1:ICOL)
      CALL SULINE (LP,1)
C
      IF (IJ.GT.NSEGS.AND.NXY.GT.NBPTS.AND.IYC.GE.IROW) GO TO 140
         IROW=IROW-1
         GO TO 50
C
140   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBPLOT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
160   FORMAT (' ')
170   FORMAT ('0*** WARNING - THE FOLLOWING ROW CONTAINS AT LEAST ',
     *   'ONE POINT WHICH DOES NOT PLOT ON THE PAGE.')
C
      END
