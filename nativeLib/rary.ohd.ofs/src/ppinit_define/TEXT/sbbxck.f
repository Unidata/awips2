C MODULE SBBXCK
C-----------------------------------------------------------------------
C
      SUBROUTINE SBBXCK (MX,MY,IY,IXB,IXE,NSEGS,LFACTR,NPTS,ISTAT)
C
C  THIS ROUTINE COMPUTES THE NUMBER OF STORED BASIN POINTS THAT FALL
C  WITHIN A SPECIFIED MDR BOX.
C
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION IY(NSEGS),IXB(NSEGS),IXE(NSEGS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbbxck.f,v $
     . $',                                                             '
     .$Id: sbbxck.f,v 1.2 1999/07/07 14:11:08 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBBXCK'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MX=',MX,' MY=',MY
         CALL SULINE (IOSDBG,1)
         DO 30 I=1,NSEGS
            WRITE (IOSDBG,'(1X,A,I4,4(1X,A,I4))')
     *         'I=',I,'IY(I)=',IY(I),'IXB(I)=',IXB(I),'IXE(I)=',IXE(I)
            CALL SULINE (IOSDBG,1)
30          CONTINUE
         WRITE (IOSDBG,*) 'LFACTR=',LFACTR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      NPTS=0
C
C  COMPUTE MIN AND MAX GRID POINTS FOR MDR BOX.
      IXMN=((MX-1)*10)+1
      IXMX=IXMN+10
      IYMN=((MY-1)*10)+1
      IYMX=IYMN+10
C
C  COMPUTE NUMBER OF POINTS WITHIN BOX.
      DO 130 I=1,NSEGS
         IF (IY(I).GT.IYMX) GO TO 130
         IF (IY(I).LT.IYMN) GO TO 140
         IF (IXE(I).LT.IXMN.OR.IXB(I).GT.IXMX) GO TO 130
         IXXB=IXB(I)
         IXXE=IXE(I)
         DO 70 J=IXXB,IXXE,LFACTR
            LB=J
            IF (LB.GE.IXMN) GO TO 80
70          CONTINUE
80       IF (IXB(I).EQ.IXE(I)) GO TO 110
         DO 90 J=LB,IXXE,LFACTR
            LE=J
            IF (LE.GT.IXMX) GO TO 100
90          CONTINUE
         GO TO 120
100      LE=LE-LFACTR
         GO TO 120
110      LE=LB
120      NPTS=NPTS+((LE-LB)/LFACTR)+1
130      CONTINUE
C
140   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NPTS=',NPTS
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBBXCK'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
