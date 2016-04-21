C MODULE SBPTCK
C-----------------------------------------------------------------------
C
      SUBROUTINE SBPTCK (X,Y,FLAT,FLON,NBPTS,MBPTS,ISTAT)
C
C  THIS ROUTINE CHECKS THE BASIN BOUNDARY POINTS TO SEE IF ANY LINES
C  INTERSECT.
C
      DIMENSION X(MBPTS),Y(MBPTS)
      DIMENSION FLAT(NBPTS),FLON(NBPTS)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbptck.f,v $
     . $',                                                             '
     .$Id: sbptck.f,v 1.6 2001/06/13 12:43:03 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBPTCK'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NBPTS=',NBPTS
         CALL SULINE (IOSDBG,1)
         NVAL=NBPTS
         NPER=10
         NLINE=NVAL/NPER
         IF (NVAL/NPER*NPER.NE.NVAL) NLINE=NLINE+1
         NLINE=NLINE+1
         WRITE (IOSDBG,'(1X,A,I2,A/(10(1X,F7.2)))')
     *      'Y(1...',NVAL,')=',(Y(I),I=1,NVAL)
         CALL SULINE (IOSDBG,NLINE)
         WRITE (IOSDBG,'(1X,A,I2,A/(10(1X,F7.2)))')
     *      'X(1...',NVAL,')=',(X(I),I=1,NVAL)
         CALL SULINE (IOSDBG,NLINE)
         ENDIF
C
      ISTAT=0
C
      NXY=NBPTS
C
C  CHECK LAST POINT
      IF (ABS(X(1)-X(NBPTS)).LT.0.00001.AND.
     *    ABS(Y(1)-Y(NBPTS)).LT.0.00001)
     *   GO TO 10
C
      IF (NBPTS+1.GT.MBPTS) THEN
         WRITE (LP,190) MBPTS
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 90
         ENDIF
C
      NXY=NBPTS+1
      X(NXY)=X(1)
      Y(NXY)=Y(1)
C
C  CHECK FOR IDENTICAL POINTS
10    ISAME=0
      DO 20 I=1,NBPTS-1
         J=I+1
         DIFF=0.00001
         IF (ABS(X(I)-X(J)).GT.DIFF.OR.ABS(Y(I)-Y(J)).GT.DIFF) GO TO 20
            WRITE (LP,210) I,J
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            ISAME=1
20       CONTINUE
C
      IF (ISAME.GT.0) GO TO 140
C
C  PERFORM LOOP FOR EACH LINE SEGMENT
      NXY2=NXY-2
      IF (NXY2.LT.1) GO TO 90
C
      DO 80 I=1,NXY2
         J=I+1
         Y1=Y(I)
         Y2=Y(J)
         X1=X(I)
         X2=X(J)
         YA=Y1
         YB=Y2
         XA=X1
         XB=X2
C     COMPUTE SLOPE AND INTERCEPT FOR BASIS LINE - save original values
c     make check compatible with check for vertical lines - use same 
C     diff
         diff = 0.001
         IF (ABS(X1-X2).LE.diff) GO TO 30
            S1=(Y2-Y1)/(X2-X1)
            B1=Y1-(((Y2-Y1)/(X2-X1))*X1)
            sa = s1
            ba = b1
C     PERFORM COMPARISONS
30       M=J+1
         NXY1=NXY-1
         IF (I.EQ.1) NXY1=NXY-2
         IF (NXY1.LT.M) GO TO 90
         DO 70 K=M,NXY1
            L=K+1
            Y1=YA
            Y2=YB
            X1=XA
            X2=XB
            Y3=Y(K)
            Y4=Y(L)
            X3=X(K)
            X4=X(L)
            s1 = sa
            b1 = ba
C        CHECK FOR LINES THAT ARE TOO CLOSE TO VERTICAL
            DIFF=0.001
            IF (ABS(X1-X2).GT.DIFF.AND.ABS(X3-X4).GT.DIFF) GO TO 40
               CALL SBRTAT (X1,X2,X3,X4,Y1,Y2,Y3,Y4,DIFF,ISTAT)
               IF (ISTAT.GT.0) THEN
                  WRITE (LP,180)
     *               I,FLAT(I),FLON(I),J,FLAT(J),FLON(J),
     *               K,FLAT(K),FLON(K),L,FLAT(L),FLON(L)
                  CALL SUERRS (LP,2,-1)
                  ISTAT=1
                  GO TO 140
                  ENDIF
c        recompute slope and intercept after the axis rotation                  
               S1=(Y2-Y1)/(X2-X1)
               B1=Y1-(((Y2-Y1)/(X2-X1))*X1)               
C        COMPARE SLOPE AND INTERCEPT FOR COMPARISON LINE
40          S2=(Y4-Y3)/(X4-X3)
            B2=Y3-(((Y4-Y3)/(X4-X3))*X3)
C        CHECK FOR PARALLEL SEGMENTS
            IF (ABS(S2-S1).GT.0.0) GO TO 50
            IF (ABS(B1-B2).GT.0.0) GO TO 70
            IF ((X1.GE.X3.AND.X1.LE.X4).OR.(X1.LE.X3.AND.X1.GE.X4))
     *         GO TO 60
            IF ((X2.GE.X3.AND.X2.LE.X4).OR.(X2.LE.X3.AND.X2.GE.X4))
     *         GO TO 60
            IF ((X3.GE.X1.AND.X3.LE.X2).OR.(X3.LE.X1.AND.X3.GE.X2))
     *         GO TO 60
            IF ((X4.GE.X1.AND.X4.LE.X2).OR.(X4.LE.X1.AND.X4.GE.X2))
     *         GO TO 60
            GO TO 70
C        COMPUTE INTERSECTION
50          XINT=(B1-B2)/(S2-S1)
            IF (LDEBUG.GT.1) THEN
               WRITE (IOSDBG,170) I,K,XINT,B1,B2,S1,S2
               CALL SULINE (LP,1)
               ENDIF
C        CHECK FOR INTERSECTION WITHIN SEGMENT
            X1MIN=AMIN1(X1,X2)
            X1MAX=AMAX1(X1,X2)
            X2MIN=AMIN1(X3,X4)
            X2MAX=AMAX1(X3,X4)
            XMIN=AMAX1(X1MIN,X2MIN)
            XMAX=AMIN1(X1MAX,X2MAX)
            IF (XINT.LT.XMIN.OR.XINT.GT.XMAX) GO TO 70
60             WRITE (LP,200)
     *            I,FLAT(I),FLON(I),J,FLAT(J),FLON(J),
     *            K,FLAT(K),FLON(K),L,FLAT(L),FLON(L)
               CALL SUERRS (LP,3,-1)
               ISTAT=1
70          CONTINUE
80       CONTINUE
C
C  CHECK MIN AND MAX POINTS TO SEE IF THEY FALL EXACTLY ON A GRID ROW
90    YMIN=Y(1)
      NMIN=1
      YMAX=Y(1)
      NMAX=1
      IMOVE=0
      DO 110 I=2,NBPTS
         IF (Y(I).GE.YMIN) GO TO 100
            NMIN=I
            YMIN=Y(I)
100      IF (Y(I).LE.YMAX) GO TO 110
            NMAX=I
            YMAX=Y(I)
110      CONTINUE
      IMIN=YMIN
      IF (YMIN-IMIN.GE..001) GO TO 120
         Y(NMIN)=YMIN+.001
         IMOVE=1
120   IMAX=YMAX
      IF (YMAX-IMAX.GE..001) GO TO 130
         Y(NMAX)=YMAX+.001
         IMOVE=1
130   IF (IMOVE.EQ.1) GO TO 90
C
140   IF (ISTRCE.GT.0) THEN
         WRITE (LP,*) 'EXIT SBPTCK'
         CALL SULINE (LP,2)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
190   FORMAT ('0*** ERROR - IN SBPTCK - THE DIMENSION OF THE ',
     *   'LINE SEGMENT ARRAYS (',I5,') IS TOO SMALL ',
     *   'TO CHECK LINE SEGMENTS.')
210   FORMAT ('0*** ERROR - IN SBPTCK - BASIN BOUNDARY POINTS ',I4,
     *   ' AND ',I4,' ARE THE SAME.')
180   FORMAT ('0*** ERROR - IN SBPTCK - THE LINE CONNECTING PAIR ',
     *      I3,' (',F8.4,',',F9.4,') AND PAIR ',
     *      I3,' (',F8.4,',',F9.4,') AND' /
     *   T26,'THE LINE CONNECTING PAIR ',
     *      I3,' (',F8.4,',',F9.4,') AND PAIR ',
     *      I3,' (',F8.4,',',F9.4,') ',
     *      'CANNOT BE ROTATED.')
170   FORMAT (' I=',I3,3X,'K=',I3,3X,
     *   'XINT=',F9.3,3X,
     *   'B1=',F10.3,3X,'B2=',F10.3,3X,
     *   'S1=',F7.3,3X,'S2=',F7.3,3X)
200   FORMAT ('0*** ERROR - IN SBPTCK - THE LINE CONNECTING PAIR ',
     *      I3,' (',F8.4,',',F9.4,') AND PAIR ',
     *      I3,' (',F8.4,',',F9.4,') INTERSECTS ' /
     *   T26,'THE LINE CONNECTING PAIR ',
     *      I3,' (',F8.4,',',F9.4,') AND PAIR ',
     *      I3,' (',F8.4,',',F9.4,').')
C
      END
