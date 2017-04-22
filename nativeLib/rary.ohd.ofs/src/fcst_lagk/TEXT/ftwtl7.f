C MODULE FTWTL7
C-----------------------------------------------------------------------
C
      SUBROUTINE FTWTL7 (IBUG,TLRC,QBNTL,QLI,QPREV,QK,Q)
C
C.......................................................................
C
C  THIS SUBROUTINE PERFORMS THE FORT WORTH FLOW TRANSMISSION
C  LOSS COMPUTATIONS.
C.......................................................................
C
C  SUBROUTINE ADDED JUNE 1989 - GEORGE SMITH - HRL
C  DEBUG PRINT ADDED JANUARY 1990 - GEORGE SMITH - HRL
C.......................................................................
C
C  VARIABLES IN ARGUMENT LIST
C
C  -- INPUT VARIABLES --
C     1. IBUG  - PRINT DEBUG SWITCH
C     2. TLRC  - TRANSMISSION LOSS RECESSION COEFFICIENT
C                  (RANGE GT 0 AND LT 1)
C     3. QBNTL - FLOW BELOW WHICH NO LOSS OCCURS
C     4. QLI   - CURRENT LAGGED INFLOW
C     5. QPREV - PREVIOUS ROUTED (ATTENUATED) FLOW
C     6. QK    - CURRENT ROUTED FLOW (BEFORE LOSS COMPUTATIONS)
C  -- OUTPUT VARIABLE --
C     7. Q     - CURRENT ROUTED FLOW (INCLUDING LOSS, IF ANY)
C
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_lagk/RCS/ftwtl7.f,v $
     . $',                                                             '
     .$Id: ftwtl7.f,v 1.2 2000/03/13 20:52:09 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG.EQ.1) WRITE (IODBUG,600) TLRC,QBNTL,QLI,QPREV,QK
 600  FORMAT (' ENTER FTWTL7: TLRC=',F5.2,' QBNTL=',F12.1,
     1 ' QLI=',F12.1,' QPREV=',F12.1,' QK=',F12.1)
C
      QTL=-999.
      Q=QK
C
C  SEE IF WE ARE ON RECESSION PORTION OF HYDROGRAPH
C
      IF(QLI.GE.QPREV)GO TO 999
C
C  YES - COMPUTE FLOW INCLUDING TRANSMISSION LOSS (QTL) AND
C        CHECK FOR RANGE TO APPLY LOSS
C
      QTL=QPREV*TLRC
C
      IF(QTL.GE.QK .OR. QTL.LE.QBNTL)GO TO 999
C
      Q=QTL
C
 999  IF (IBUG.GT.0) WRITE (IODBUG,601) Q,QTL
 601  FORMAT (' EXIT FTWTL7: Q=',F12.1,' QTL=',F14.3)
C
      RETURN
      END
