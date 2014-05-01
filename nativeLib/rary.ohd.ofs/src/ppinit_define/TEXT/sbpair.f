C MODULE SBPAIR
C-----------------------------------------------------------------------
C
      SUBROUTINE SBPAIR (FLAT,FLON,IY,IXB,IXE,NSEGS,ISTAT)
C
C  THIS ROUTINE COMBINES THE BASIN DEFINITION END POINTS INTO MATCHED
C  PAIRS.
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION IY(NSEGS),IXB(NSEGS),IXE(NSEGS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbpair.f,v $
     . $',                                                             '
     .$Id: sbpair.f,v 1.4 2001/06/13 12:42:55 dws Exp $
     . $' /
C    ===================================================================
C
C    
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBPAIR'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         DO 10 I=1,NSEGS
            WRITE (IOSDBG,'(1X,A,I4,4(1X,A,I4))')
     *         'I=',I,'IY(I)=',IY(I),'IXB(I)=',IXB(I),'IXE(I)=',IXE(I)
            CALL SULINE (IOSDBG,1)
10          CONTINUE
         ENDIF
C
      ISTAT=0
C
      M=1
C
C  SORT SEGMENT END POINTS INTO DESCENDING ORDER
30    L=1
      MM=M+1
      DO 40 I=MM,NSEGS
         IF (IY(I).NE.IY(M)) GO TO 50
         L=L+1
40       CONTINUE
C
cew check to see if there is more than one point on an HRAP
cew row.  If there is just one per row, set end point and
cew beginning point to be the same.  Setting is done below.
 50   if (l.eq.1) goto 95
C
      L2=L/2
      CALL SBSORT (IXB(M),IY(M),IXE(M),L,ISTAT)
      CALL SBSORT (IXE(M),IY(M),IXB(M),L2,ISTAT)
C
C  MATCH BEGINNING POINTS TO ENDING POINTS
      N=M+L2-1
      IF (N.LT.M) GO TO 70
C
      DO 60 I=M,N
         IXB(I)=IXB(I+L2)
60       CONTINUE
C
70    LL=M+L2
      NSEGS=NSEGS-L2
C
C  MOVE REMAINING POINTS INTO UNUSED POSITIONS
      IF (NSEGS.LT.LL) GO TO 90
      DO 80 I=LL,NSEGS
         IY(I)=IY(I+L2)
         IXB(I)=IXB(I+L2)
         IXE(I)=IXE(I+L2)
80       CONTINUE
C
cew this sets the end point and the beginning point
cew to be the same for those points which are singles.
95    if(ixb(m).eq.9999) then
        ixb(m)=ixe(m)
        ll=m+1
      elseif(ixe(m).eq.9999) then
        ixe(m)=ixb(m)
        ll=m+1
      endif

90    M=LL
C
      IF (M.GT.NSEGS) GO TO 100
C
C  REPEAT PROCESS UNTIL ALL POINTS ARE MATCHED
      GO TO 30
C
C  CHECK TO SEE IF ANY PAIRS ARE IN REVERSE ORDER
100   JK=0
      IJ=1
C
110   IF (NSEGS.LT.IJ) GO TO 160
      DO 120 I=IJ,NSEGS
         JK=JK+1
         IF (IXB(I).GT.IXE(I)) GO TO 130
120      CONTINUE
C
      GO TO 160
C
130   KM1=NSEGS-1
      IF (KM1.LT.JK) GO TO 150
      DO 140 I=JK,KM1
         IY(I)=IY(I+1)
         IXB(I)=IXB(I+1)
         IXE(I)=IXE(I+1)
140      CONTINUE
C
150   NSEGS=NSEGS-1
      IJ=JK
      JK=JK-1
      GO TO 110
C
C  CHECK IF ANY GRID POINTS FALL WITH THE BASIN
160   IF (NSEGS.LE.0) THEN
         WRITE (LP,260)
         CALL SUWRNS (LP,2,-1)
         NBPTS=1
         ILLGD=1
         CALL SBLLGD (FLON,FLAT,NBPTS,X,Y,ILLGD,ISTAT)
         IY(1)=Y
         IXB(1)=X
         IXE(1)=X
         NSEGS=1
         GO TO 200
         ENDIF
C
C  CHECK FOR ANY REMAINING UNMATCHED POINTS
      DO 170 I=1,NSEGS
         IF (IXB(I).EQ.9999.OR.IXE(I).EQ.9999) GO TO 180
170      CONTINUE
         GO TO 200
180   WRITE (LP,250)
      CALL SUERRS (LP,2,-1)
      ISTAT=1
      GO TO 220
C
200   IF (LDEBUG.GT.0) THEN
         DO 210 I=1,NSEGS
            WRITE (IOSDBG,'(1X,A,I4,4(1X,A,I4))')
     *         'I=',I,'IY(I)=',IY(I),'IXB(I)=',IXB(I),'IXE(I)=',IXE(I)
            CALL SULINE (IOSDBG,1)
210         CONTINUE
         ENDIF
C
220   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBPAIR'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
250   FORMAT ('0*** ERROR - IN SBPAIR - THE BASIN GRID POINTS COULD ',
     *   'NOT BE DEFINED. CHECK THAT LATITUDE/LONGITUDE PAIRS ARE IN ',
     *   'CLOCKWISE ORDER.')
260   FORMAT ('0*** WARNING - NO GRID POINTS FALL WITHIN THE BASIN ',
     *   'BOUNDARY. THE CLOSEST GRID POINT WILL BE STORED.')
C
      END
