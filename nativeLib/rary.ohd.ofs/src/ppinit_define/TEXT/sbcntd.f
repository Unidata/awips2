C MODULE SBCNTD
C-----------------------------------------------------------------------
C
      SUBROUTINE SBCNTD (IY,IXB,IXE,NSEGS,XC,YC,LFACTR,ISTAT)
C
C  THIS ROUTINE COMPUTES THE CENTROID OF A BASIN BASED ON THE 
C  GRID POINT DEFINITION.
C
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION IY(NSEGS),IXB(NSEGS),IXE(NSEGS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbcntd.f,v $
     . $',                                                             '
     .$Id: sbcntd.f,v 1.2 1999/07/06 11:39:40 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBCNTD'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NSEGS=',NSEGS,' LFACTR',LFACTR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      NUM=0
      IXSUM=0
      IYSUM=0
C
C  COMPUTE SUMS OF GRID POINTS
      DO 20 I=1,NSEGS
         IXXB=IXB(I)
         IXXE=IXE(I)
         DO 10 J=IXXB,IXXE,LFACTR
            IXSUM=IXSUM+J
            NUM=NUM+1
10          CONTINUE
         IYSUM=IYSUM+(((IXXE-IXXB)/LFACTR)+1)*IY(I)
20       CONTINUE
C
C  COMPUTE CENTROID
      XC=FLOAT(IXSUM)/FLOAT(NUM)
      YC=FLOAT(IYSUM)/FLOAT(NUM)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'XC=',XC,' YC=',YC
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBCNTD'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
