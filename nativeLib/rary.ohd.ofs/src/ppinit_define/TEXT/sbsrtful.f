C MODULE SBSRTFUL
C-----------------------------------------------------------------------
C
      SUBROUTINE SBSRTFUL (IY,IXB,IXE,NSEGS,ISTAT)
C
C  This routine sorts IY, IXB, and IXE by (in order) IY, IXB, and IXE
C  into DESCENDING order!  This was done for bug r25-29 to force it to
C  produce output in the same order as it did before the fix.
C
C  Editted from sbsort.f by Hank Herr on 1/12/05 for bug r25-29.
C
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION IY(NSEGS),IXB(NSEGS),IXE(NSEGS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbsrtful.f,v $
     . $',                                                             '
     .$Id: sbsrtful.f,v 1.1 2005/01/14 20:20:24 hank Exp $
     . $' /
C    ===================================================================
C
C
C
C Debug statements, if desired.
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBSORT'
         CALL SULINE (IOSDBG,1)
      ENDIF
      LDEBUG=ISBUG('GRID')
      ISTAT=0
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NSEGS=',NSEGS
         CALL SULINE (IOSDBG,1)
      ENDIF
C
C No sorting necessary if only one segment to sort.
      IF (NSEGS.LT.2) GO TO 50
C
      L=NSEGS-1
C
C This is a bubble sort.
      DO 20 M=1,L
         IND=0
         DO 10 N=1,L

C This complex IF conditions triggers a swap when...
C (1) IY(N) < IY(N+1)
C (2) If IY(N) = IY(N+1), then swap if IXB(N) < IXB(N+1)
C (3) IF both of the above equal, then swap if IXE(N) < IXE(N+1)
            IF (IY(N).LT.IY(N+1)) GO TO 5
            IF (IY(N).EQ.IY(N+1)) THEN
              IF (IXB(N).LT.IXB(N+1)) GO TO 5
              IF (IXB(N).EQ.IXB(N+1)) THEN
                IF (IXE(N).LT.IXE(N+1)) GO TO 5
              ENDIF
            ENDIF
            GO TO 10
            
C If the Nth element is an end pt (IXE) then it can be swapped even
C if the N+1 element is also an end pt (in this case the swap does
C nothing since all numbers to swap will be equal).  
            
C This code swaps the two entries!  First IY, then IXB and IXE.
5           ITEMP=IY(N+1)
            IY(N+1)=IY(N)
            IY(N)=ITEMP
            ITEMP=IXB(N+1)
            IXB(N+1)=IXB(N)
            IXB(N)=ITEMP
            ITEMP=IXE(N+1)
            IXE(N+1)=IXE(N)
            IXE(N)=ITEMP
            
C Indicate that a swap occurred.  
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
