C MEMBER POSN26
C  (from old member FCPOSN26)
C
C DESC POSITION UNIT TO PROPER LINE FOR SUBSEQUENT READING
C
C---------------------------------------------------------------------
C
      SUBROUTINE POSN26(IUNIT,LINE)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/posn26.f,v $
     . $',                                                             '
     .$Id: posn26.f,v 1.1 1995/09/17 18:52:25 dws Exp $
     . $' /
C    ===================================================================
C
C
C---------------------------------------------------------------------
C
C  ARGS:
C
C   IUNIT - UNIT NUMBER OF DATA SET TO BE POSITIONED
C    LINE - LINE NUMBER TO BE POSITIONED TO. IUNIT IS ACTUALLY READ
C           TO THE PREVIOUS LINE SO THAT THE NEXT LINE READ IS 'LINE'
C
C----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
C  JTOSTROWSKI - HRL - MARCH 1983
C----------------------------------------------------------------
      LINETO = LINE-1
C
      REWIND IUNIT
C
      IF (LINETO .LE. 0) GO TO 99
C
      DO 10 I=1,LINETO
           READ(IUNIT,5)DUMMY
5          FORMAT(A4)
   10 CONTINUE
C
   99 CONTINUE
      RETURN
      END
