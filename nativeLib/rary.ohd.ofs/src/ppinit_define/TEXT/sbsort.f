C MODULE SBSORT
C-----------------------------------------------------------------------
C
      SUBROUTINE SBSORT (IY,IXB,IXE,NSEGS,ISTAT)
C
C  THIS ROUTINE SORTS ARRAYS INTO DESCENDING ORDER.
C
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION IY(NSEGS),IXB(NSEGS),IXE(NSEGS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbsort.f,v $
     . $',                                                             '
     .$Id: sbsort.f,v 1.2 1999/07/06 11:43:35 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBSORT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('GRID')
C
      ISTAT=0
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NSEGS=',NSEGS
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (NSEGS.LT.2) GO TO 50
C
      L=NSEGS-1
C
      DO 20 M=1,L
         IND=0
         DO 10 N=1,L
            IF (IY(N).GT.IY(N+1)) GO TO 10
            ITEMP=IY(N+1)
            IY(N+1)=IY(N)
            IY(N)=ITEMP
C       SORT ARRAYS IXB() AND IXE() SO THAT THEY CORRESPOND TO IY()
            ITEMP=IXB(N+1)
            IXB(N+1)=IXB(N)
            IXB(N)=ITEMP
            ITEMP=IXE(N+1)
            IXE(N+1)=IXE(N)
            IXE(N)=ITEMP
            IND=1
10          CONTINUE
         IF (IND.EQ.0) GO TO 30
20       CONTINUE
C
30    IF (LDEBUG.GT.0) THEN
         DO 40 I=1,NSEGS
            WRITE (IOSDBG,'(1X,A,I4,4(1X,A,I4))')
     *         'I=',I,'IY(I)=',IY(I),'IXB(I)=',IXB(I),'IXE(I)=',IXE(I)
            CALL SULINE (IOSDBG,1)
40          CONTINUE
         ENDIF
C
50    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBSORT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C
      END
