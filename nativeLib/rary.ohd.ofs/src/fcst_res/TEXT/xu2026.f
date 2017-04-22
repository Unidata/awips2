C MEMBER XU2026
C  (from old member FCXU2026)
C
      SUBROUTINE XU2026(SUNUM,PO,W,LOCOWS,ELEVT,QMAXT)
C---------------------------------------------------------------------
C  SUBROUTINE TO EXECUTE UTILITY #20 - 'MAXQ'
C  IT COMPUTES THE ELEV VS MAX DISCHARGE (EITHER TOTAL OR GENERATION)
C  CURVE HOWEVER IT HAS BEEN ENTERED AND COMPUTES THE MAX VALUE AT
C  THE TEST ELEVATION, ELEVT.
C---------------------------------------------------------------------
C  WRITTEN BY - JOE OSTROWSKI - HRL - AUGUST 1983
C---------------------------------------------------------------------
C
      INCLUDE 'common/resv26'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/exg26'
C
      DIMENSION PO(1),W(1),LOCOWS(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xu2026.f,v $
     . $',                                                             '
     .$Id: xu2026.f,v 1.1 1995/09/17 19:07:16 dws Exp $
     . $' /
C    ===================================================================
C
C
C-------------------------
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600)
 1600 FORMAT('   *** ENTER XU2026 ***')
C
C  GET POINTER INFORMATION FOR THIS UTILITY
C
      CALL XPTR26(SUNUM,PO,IORD,IBASE,LEVEL,LOCPM,LOCTS,LOCCO)
C
C  COMPUTE ELEVATION VS. DISCHARGE CURVE
C
      CALL XFMQ26(SUNUM,PO,LOCPM,W,LOCOWS,IMQTYP,LOCMQ,LOKELV,NDAMQ)
C
C  CALL THE INTERPOLATION ROUTINE WITH THE PROPER ADDRESS FOR THE MAXQ
C  CURVE.
C
      NTRP = 0
      LOCMQ2 = LOCMQ + NDAMQ
      IF (IMQTYP.EQ.1) CALL NTER26(ELEVT,QMAXT,PO(LOCMQ),PO(LOCMQ2),
     . NDAMQ,IFLAG,NTRP,IBUG)
      IF (IMQTYP.EQ.2) CALL NTER26(ELEVT,QMAXT,PO(LOKELV),W(LOCMQ),
     . NDAMQ,IFLAG,NTRP,IBUG)
C
      IF (IBUG.GE.2) WRITE(IODBUG,1690) ELEVT,QMAXT
 1690 FORMAT('     * MAXIMUM DISCHARGE AT ELEVATION ',F7.2,' IS ',F10.3)
C
C----------------------------------------------
C  THAT'S ALL THERE IS
C
      IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XU2026 ***')
      RETURN
      END
