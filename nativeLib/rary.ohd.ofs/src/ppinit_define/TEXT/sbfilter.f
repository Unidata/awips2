C MODULE SBFILTER
C written by Hank Herr
C Jan 12, 2005
C
C Arguments: IY, IXB, and IXE are integer arrays defined in calling
C routines.  NSEGS is the size of the arrays.  IYP is a pointer
C to where the next grid point would go if one were added.
C ISTAT is the return status.  All are edited by this routine.
C----------------------------------------------------------------------
C
      SUBROUTINE SBFILTER (IY,IXB,IXE,NSEGS,IYP,ISTAT) 
C
C  NSEGS is the number of segments with results in them.  Typically, 
C  IYP-1 when passed into here.  IYP is still a pointer to place to
C  hold NEXT grid segment.
C
C  This routine passes through the IY, IXB, and IXE arrays and combines
C  segments when possible.  It will sort the segments and then do the
C  comparisons. 
C
      DIMENSION IY(NSEGS),IXB(NSEGS),IXE(NSEGS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbfilter.f,v $
     . $',                                                             '
     .$Id: sbfilter.f,v 1.1 2005/01/14 20:19:37 hank Exp $
     . $' /
C    ===================================================================
C

C  Sort the IY, IXB, and IYB arrays into descending order by IY, then
C  IXB, and finally IXE.
      CALL SBSRTFUL(IY, IXE, IXB, NSEGS, ISTAT)
      
C  Loop through all lines in IY...
      I=2
      IF (I.GT.NSEGS) GO TO 50
      
      DO 40
               
C  If the item in IY(I) and IY(I-1) have the same rows and the ending
C  point in I-1 equals or exceeds IXB(I)-1, then merge
C  the two and reset all the remaining ones in order to remove the
C  now superfluous one.  IYP then has one removed from it.
C
C  If IXE(I-1) equals IXB(I)-1, then the two are directly adjacent and
C    can be merged.
C  If IXE(I-1) exceeds IXB(I)-1, then the two overlap and can be
C    merged.
C
        IF (IY(I).EQ.IY(I-1).AND.IXE(I).GE.(IXB(I-1)-1)) THEN
          IXB(I-1)=IXB(I)
          IYP=IYP-1
          NSEGS=NSEGS-1
          DO 20 J=I,NSEGS
            IY(J)=IY(J+1)
            IXB(J)=IXB(J+1)
            IXE(J)=IXE(J+1)
20        CONTINUE

C         Clearing the one at the end that is no longer used...
          IY(NSEGS+1)=9999
          IXB(NSEGS+1)=9999
          IXE(NSEGS+1)=9999
          
        ELSE 
C         Increment I and check for break condition, but only if the 
C         list was not altered due to merging.
          I=I+1
          IF (I.GT.NSEGS) GO TO 50
        ENDIF
          
40    CONTINUE

50    RETURN
C
C
      END
        
