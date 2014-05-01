C MEMBER PRC44
C  (from old member FCPRC44)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/23/95.16:29:40 BY $WC30RE
C
C @PROCESS LVL(77)
C
      SUBROUTINE PRC44 (P,C)

C     THIS IS THE PRINT CARRYOVER ROUTINE FOR SSARR ROUTING.

C     THIS ROUTINE ORIGINALLY WRITTEN BY
C        RAY FUKUNAGA - NWRFC   MAY 1994

C     POSITION     CONTENTS OF P ARRAY
C      1           VERSION NUMBER OF OPERATION
C      2-19        GENERAL NAME OR TITLE

C     USED TO CONSERVE INFLOW HYDROGRAPH VOLUME
C     20-21        START INFLOW TIME SERIES IDENTIFIER
C     22           START INFLOW DATA TYPE CODE 

C     INFLOW HYDROGRAPH
C     23-24        END INFLOW TIME SERIES IDENTIFIER
C     25           END INFLOW DATA TYPE CODE 

C     USED TO CONSERVE ROUTED HYDROGRAPH VOLUME
C     26-27        START OUTFLOW TIME SERIES IDENTIFIER
C     28           START OUTFLOW DATA TYPE CODE 

C     ROUTED OUTFLOW HYDROGRAPH
C     29-30        END OUTFLOW TIME SERIES IDENTIFIER
C     31           END OUTFLOW DATA TYPE CODE 

C     32           1 OR 2 INFLOW TIME SERIES SPECIFIED FLAG
C                  = 1, ONLY THE END INFLOW TIME SERIES IS SPECIFIED
C                  = 2, BOTH THE START AND END INFLOW TIME SERIES ARE
C                       SPECIFIED
C     33           1 OR 2 OUTFLOW TIME SERIES SPECIFIED FLAG
C                  = 1, ONLY THE END OUTFLOW TIME SERIES IS SPECIFIED
C                  = 2, BOTH THE START AND END OUTFLOW TIME SERIES ARE
C                       SPECIFIED
C     34           NUMBER OF ROUTING PHASES (MUST BE > 0)
C     35           N VALUE OF KTS/Q**N COMPUTATION
C                  IF N=0, THE TIME OF STORAGE IS EXTRACTED FROM THE
C                           DISCHARGE-TIME OF STORAGE TABLE
C     36           KTS VALUE IN HOURS IF N IS NONZERO
C     37           THE NUMBER OF POINTS ON THE DISCHARGE-TIME OF
C                  STORAGE TABLE IF N=0 (=0 IF N IS NONZERO)
C     38           COMPUTATIONAL TIME INTERVAL (HOURS)
C     39+          THE POINTS OF THE DISCHARGE-TIME OF STORAGE TABLE
C
C
C     THEREFORE THE NUMBER OF ELEMENTS REQUIRED IN THE P ARRAY IS
C        38 +
C         2 * NUMBER OF POINTS OF THE DISCHARGE-TIME OF STORAGE TABLE

C     POSITION     CONTENTS OF C ARRAY
C      1           INITIAL START INFLOW, IF ONLY END INFLOW TIME SERIES
C                  IS SPECIFIED
C      2+          PHASE FLOW VALUES FROM REACH


      DIMENSION P(*),C(*)

C     COMMON BLOCKS

      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FCONIT/IVALUE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc44.f,v $
     . $',                                                             '
     .$Id: prc44.f,v 1.2 1996/03/21 15:21:08 page Exp $
     . $' /
C    ===================================================================
C

      CALL FPRBUG ('PRC44   ',1,44,IBUG)

      NPS = NINT(P(34))
      NTS = NINT(P(32))
      IF (NTS.EQ.1) THEN
         NCO = NPS + 1
         NSO = 2
      ELSE
         NCO = NPS
         NSO = 1
      ENDIF
      IF (IBUG.EQ.1) WRITE(IODBUG,60) NPS,NTS,NCO
  60  FORMAT('PRC44: NPS,NTS,NCO: ', 3I6)

      IF (NTS.EQ.1) THEN
         WRITE(IPR,510) C(1)
 510     FORMAT(/10X,'INITIAL START INFLOW TO REACH',/,
     +           10X,'-----------------------------',/,
     +           10X,'          ',F9.1)
      ELSE
         WRITE(IPR,511)
 511     FORMAT(/10X,'INITIAL START INFLOW TO REACH',/,
     +           10X,'FROM START INFLOW TIME SERIES')
      ENDIF

      WRITE(IPR,500)
 500  FORMAT(/10X,'CARRYOVER PHASE FLOW VALUES',/,
     +           10X,'CURRENT TO FUTURE PHASE FLOW VALUES',/,
     +           10X,'-----------------------------------',/,
     +           10X,'                        FLOW',/,
     +           10X,'PHASE                   (CFS)',/,
     +           10X,'-----                   -----')
      J=0
      DO 100 I=NSO,NCO
         J=J+1
 100  WRITE(IPR,501) J,C(I)
 501  FORMAT(10X,I3,'                ',F9.1)

      IF (ITRACE.GE.1) WRITE(IODBUG,90)
 90   FORMAT('PRC44:  EXITED:')

      RETURN
      END
