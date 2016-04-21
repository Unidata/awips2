C MEMBER GLSTV
C  (from old member PPGLSTV)
C
      SUBROUTINE GLSTV(W6, GP6, PPVR, PVPTR, NVST, ITINT)
C
C.....THIS SUBROUTINE IS A DRIVER FOR THE LISTING ROUTINES FOR LESS
C.....THAN 24-HOUR PRECIPITATION.  AS OF THE FEB. 1988...THE ONLY
C.....LESS THAN 24-HOUR TIME DURATION PERMITTED IN MARO IS 6 HOURS.
C
C.....GLSTV HAS THE FOLLOWING ARGUMENTS:
C
C.....W6     - ARRAY OF SIX-HOUR DISTRIBUTION PERCENTAGES.
C.....GP6    - GRID POINT ADDRESS-DEPENDENT POINGER TO W6 ARRAY.
C.....PPVR   - THE ARRAY OF LESS THAN 24-HOUR PRECIPITATION.
C.....PVPTR  - THE POINTER ARRAY FOR PPVR.
C.....NVST   - THE NUMBER OF STATIONS IN PPVR.
C.....ITINT  - THE TIME INTERVAL.
C
C.....ORIGINALLY WRITTEN BY:
C
C.....JERRY M. NUNN     WGRFC, FT. WORTH, TEXAS     FEBRUARY 24, 1988
C
      DIMENSION SNAME(2)
      INTEGER*2 PPVR(1), PVPTR(1), W6(1), GP6(1)
C
      INCLUDE 'common/where'
      INCLUDE 'common/pudbug'
      INCLUDE 'common/errdat'
      INCLUDE 'gcommon/gsize'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/glstv.f,v $
     . $',                                                             '
     .$Id: glstv.f,v 1.1 1995/09/17 19:01:54 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME /4hGLST, 4hV   /
C
C
  900 FORMAT(1H0, '*** GLSTV ENTERED ***')
  901 FORMAT(1H0, '*** EXIT GLSTV ***')
  902 FORMAT(1H0, '*** WARNING ***  YOU HAVE SPECIFIED A TIME INTERVAL O
     *F ', I4, ' HOURS.', /, 5X, 'THE ONLY LESS THAN 24-HOUR TIME INTERV
     *AL PERMITTED IN MARO IS ', I4, ' HOURS.', /, 5X, 'THE TIME INTERVA
     *L IS BEING CHANGED TO ', I4, ' HOURS.')
C
      INCLUDE 'gcommon/setwhere'
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,900)
C
C.....TEST THE TIME INTERVAL. AS OF NOW (FEB. 1988) THE ONLY TIME
C.....INTERVAL PERMITTED IN MARO IS 6 HOURS.
C
      IF(ITINT .EQ. 6) GOTO 100
      WRITE(IPR,902) ITINT, NSTEP1, NSTEP1
      CALL WARN
      ITINT = NSTEP1
C
  100 CALL GLST6(W6, GP6, PPVR, PVPTR, NVST, ITINT)
C
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,901)
      RETURN
      END
