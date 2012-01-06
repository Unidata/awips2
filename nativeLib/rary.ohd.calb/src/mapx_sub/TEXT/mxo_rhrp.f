C MODULE MXO_RHRP
C  =====================================================================
C  pgm: MXO_RHRP .. Read ofs files to get hrap data
C  =====================================================================
      SUBROUTINE MXO_RHRP(LARR,KARR,IARR,RARR,BASINS,NBASN,NXBAS,NUMBA,
     $                    IY,IXB,IXE,NUMLL,AREA,UNIT,IPTR,IERR)

      CHARACTER*8     BASINS(*),NXBAS,BOUNID
      CHARACTER*4     UNIT
      INTEGER         NBASN,NUMLL,IERR,NBPTS,NSEGS,NUMBA,JERR
      REAL            AREA,AREAM
 
      CHARACTER*(*)   KARR
      INTEGER         IARR(*)
      REAL            RARR(*)
      INTEGER         LARR

      INTEGER         IY(*),IXB(*),IXE(*)
      CHARACTER*4     PARMTP
      CHARACTER*8     PARMID
      INTEGER         NUM,IPTR,IPTRNX,ISTAT,II,JJ1,JJ2,JJ3,INEX,IFOU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxo_rhrp.f,v $
     . $',                                                             '
     .$Id: mxo_rhrp.f,v 1.1 2001/06/13 09:24:12 mgm Exp $
     . $' /
C    ===================================================================
C

        NUMLL = 0
        AREA  = 0.0
        UNIT  = ' '
        NXBAS = ' '

        PARMTP = 'BASN'

        IERR = -1
        IFOU = 0
   20   IF (IPTR.LT.0 .OR. IERR.GT.0 .OR. IFOU.NE.0) GOTO 50

          IF (NBASN .GT. 0) THEN
            PARMID = BASINS(NUMBA)
          ELSE
            PARMID = ' '
          ENDIF

          CALL RPPREC (PARMID,PARMTP,IPTR,LARR,RARR,NUM,IPTRNX,ISTAT)
          IF (IPTRNX .LE. 0) IERR = -1
          IPTR = IPTRNX
          IF (IPTR .LE. 0) IPTR = -1
          IF (ISTAT .NE. 0) THEN
            IERR = 1
          ELSE
            BOUNID = KARR(5:12)

C                   Check if this basin is wanted

            IF (BOUNID .NE. ' ') THEN
              IF (NBASN .LE. 0) THEN
                IFOU = 9999
              ELSE
                INEX = NBASN
                IFOU = 0
   42           IF (INEX .LE. IFOU) GOTO 44
                  IF (BOUNID .EQ. BASINS(INEX)) IFOU = INEX
                  INEX = INEX-1
                  GOTO 42
   44           CONTINUE
              ENDIF
            ENDIF

            IF (IFOU .GT. 0) THEN
              NXBAS = BOUNID
              AREAM = RARR(10)

              NBPTS = RARR(23)
              NSEGS = RARR(24)

C    AREAM is in metric from the database, convert to 'ENGL'

              CALL UDUCNV ('KM2 ','MI2 ',1,1,AREAM,AREA,JERR)
              IF (JERR .NE. 0) IERR = 4
              UNIT  = 'ENGL'

              IF (NSEGS .GT. LARR) THEN
                IERR = 2
              ELSEIF (NSEGS .LE. 0) THEN
                IERR = 3
              ELSE
                IERR = 0
                JJ1 = 24 + 2*NBPTS
                DO 22 II=1,NSEGS
                  JJ1 = JJ1+1
                  JJ2 = JJ1+NSEGS
                  JJ3 = JJ2+NSEGS
                  IY(II)  = IARR(JJ1)
                  IXB(II) = IARR(JJ2)
                  IXE(II) = IARR(JJ3)
   22           CONTINUE
                NUMLL= NSEGS
              ENDIF
            ENDIF

          ENDIF
          GOTO 20
   50   CONTINUE

      RETURN
      END
