C MODULE MXO_MXCO
C  =====================================================================
C  pgm: MXO_MXCO .. Get mapx basin names
C  =====================================================================
      SUBROUTINE MXO_MXCO(LARR,KARR,IARR,RARR,NAREA,BASINS,MAXBSN,ISTAT)

      CHARACTER*8     BASINS(*)
      INTEGER         NAREA,MAXBSN,IERR

      CHARACTER*(*)   KARR
      INTEGER         IARR(*)
      REAL            RARR(*)
      INTEGER         LARR

      CHARACTER*4     PARMTP
      CHARACTER*8     PARMID,BAS
      CHARACTER*200   LIN
      CHARACTER*40    MSG1
      INTEGER         NUM,IPTR,IPTRNX,ISTAT,JSTAT,NXA,II,JJ,KK,LL,JE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxo_mxco.f,v $
     . $',                                                             '
     .$Id: mxo_mxco.f,v 1.2 2005/06/09 19:27:45 dws Exp $
     . $' /
C    ===================================================================
C

      DATA  MSG1 / '  Mapx stations found using command MXCO' /

        IERR = 0

        IF (NAREA .EQ. 1) THEN
          BAS = BASINS(1)
          CALL KKCAPS(BAS)
          IF (BAS .EQ. '_MXCO') THEN

            CALL MXO_DBOP('O',IERR)
            IF (IERR .EQ. 0) THEN
              IPTR   = 0
              PARMTP = 'MXCO'
              PARMID = ' '

              CALL RPPREC(PARMID,PARMTP,IPTR,LARR,RARR,NUM,IPTRNX,JSTAT)

              IF (JSTAT .EQ. 0) THEN
                NXA = RARR(2)
                IF (NXA .GT. MAXBSN) THEN
                  IERR = 32
                  CALL WLIN('B',' ')
                  CALL WLIN('E','Number of MXCO basins exceeds limit!')
                  LIN = ' '
                  WRITE(LIN,'(''found:'',I5,'', but limit is:'',I4)',
     $                  IOSTAT=JE) NXA,MAXBSN
                  IF(JE.EQ.0) CALL WLIN('S',LIN)
                  CALL WLIN('B',' ')
                ELSE
                  IF (NXA .GT. 0) THEN
                    JJ = 4
                    DO 120 II=1,NXA
                      KK = 4*JJ + 1
                      LL = JJ + 3
                      JJ = JJ + 3
                      BASINS(II) = KARR(KK:KK+7)
  120               CONTINUE
                    NAREA = NXA

                    CALL WLIN('B',' ')
                    CALL WLIN('M',MSG1)
                    DO 124 II=1,NXA
                      LIN = ' '
                      WRITE(LIN,'(4X,I4,2X,A)',IOSTAT=JE) II, BASINS(II)
                      IF(JE.EQ.0) CALL WLIN('M',LIN)
  124               CONTINUE
                    CALL WLIN('B',' ')

                  ENDIF
                ENDIF
              ELSE
                IERR = 16
                CALL WLIN('B',' ')
                CALL WLIN('E','bad access to PPP db for mxco')
                CALL WLIN('B',' ')

              ENDIF
            ENDIF

          ENDIF
        ENDIF

        IF (IERR .NE. 0) ISTAT = ISTAT+IERR

      RETURN
      END
