C MODULE UPRIMC
C  =====================================================================
C  pgm: UPRIMC .. Close and end programs.
C  =====================================================================
      SUBROUTINE UPRIMC

      EXTERNAL       UPCLOS

      INCLUDE 'uiox'
      INCLUDE 'upvrsx'
      COMMON/LUNS/ LCHN,JCHN,KCHN,MCHN,MREC,ICHER

      INTEGER  ISTAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/uprimc.f,v $
     . $',                                                             '
     .$Id: uprimc.f,v 1.3 2001/06/13 09:58:09 mgm Exp $
     . $' /
C    ===================================================================
C

  100   IF( PGMNAM .NE. 'FILESIZE' ) GO TO 110
          IF( ICD    .NE. 1 ) CALL UPCLOS(ICD,'  ',ISTAT)
          IF( LP     .NE. 1 ) CALL UPCLOS(LP,'  ',ISTAT)
          IF( ICDPUN .NE. 1 ) CALL UPCLOS(ICDPUN,'  ',ISTAT)
            GO TO 900
  110   IF( PGMNAM .NE. 'FILECRAT' ) GO TO 120
          IF( ICD    .NE. 1 ) CALL UPCLOS(ICD,'  ',ISTAT)
          IF( LP     .NE. 1 ) CALL UPCLOS(LP,'  ',ISTAT)
            GO TO 900
  120   IF( PGMNAM .NE. 'PRDUTIL ' ) GO TO 130
          IF( ICD    .NE. 1 ) CALL UPCLOS(ICD,'  ',ISTAT)
          IF( LP     .NE. 1 ) CALL UPCLOS(LP,'  ',ISTAT)
            GO TO 900
  130   IF( PGMNAM .NE. 'PPDUTIL ' ) GO TO 140
          IF( ICD    .NE. 1 ) CALL UPCLOS(ICD,'  ',ISTAT)
          IF( LP     .NE. 1 ) CALL UPCLOS(LP,'  ',ISTAT)
          IF( ICDPUN .NE. 1 ) CALL UPCLOS(ICDPUN,'  ',ISTAT)
            GO TO 900
  140   IF( PGMNAM .NE. 'PPINIT  ' ) GO TO 150
                              CALL UPCLOS(ICDTMP,'  ',ISTAT)
          IF( ICD    .NE. 1 ) CALL UPCLOS(ICD,'  ',ISTAT)
          IF( LP     .NE. 1 ) CALL UPCLOS(LP,'  ',ISTAT)
          IF( ICDPUN .NE. 1 ) CALL UPCLOS(ICDPUN,'  ',ISTAT)
            GO TO 900
  150   IF( PGMNAM .NE. 'LOOKNSET' ) GO TO 160
          IF( ICD    .NE. 1 ) CALL UPCLOS(ICD,'  ',ISTAT)
          IF( LP     .NE. 1 ) CALL UPCLOS(LP,'  ',ISTAT)
          IF( ICDPUN .NE. 1 ) CALL UPCLOS(ICDPUN,'  ',ISTAT)
            GO TO 900
  160   IF( PGMNAM .NE. 'FCINIT') GO TO 170
          IF( ICD    .NE. 1 ) CALL UPCLOS(ICD,'  ',ISTAT)
          IF( LP     .NE. 1 ) CALL UPCLOS(LP,'  ',ISTAT)
          IF( ICDPUN .NE. 1 ) CALL UPCLOS(ICDPUN,'  ',ISTAT)
                              CALL UPCLOS(89,'  ',ISTAT)
            GO TO 900
  170   IF( PGMNAM .NE. 'FCST' ) GO TO 180
          IF( ICD    .NE. 1 ) CALL UPCLOS(ICD,'  ',ISTAT)
          IF( LP     .NE. 1 ) CALL UPCLOS(LP,'  ',ISTAT)
                              CALL UPCLOS(8,'  ',ISTAT)
                              CALL UPCLOS(9,'  ',ISTAT)
                              CALL UPCLOS(97,'  ',ISTAT)
                              CALL UPCLOS(70,'  ',ISTAT)
                              CALL UPCLOS(71,'  ',ISTAT)
                              CALL UPCLOS(72,'  ',ISTAT)
                              CALL UPCLOS(73,'  ',ISTAT)
                              CALL UPCLOS(74,'  ',ISTAT)
                              CALL UPCLOS(75,'  ',ISTAT)
                              CALL UPCLOS(76,'  ',ISTAT)
                              CALL UPCLOS(77,'  ',ISTAT)
                              CALL UPCLOS(78,'  ',ISTAT)
                              CALL UPCLOS(79,'  ',ISTAT)
                              CALL UPCLOS(88,'  ',ISTAT)
            GO TO 900
  180   IF( PGMNAM .NE. 'SHEFUTIL' ) GO TO 190
          IF( ICD    .NE. 1 ) CALL UPCLOS(ICD,'  ',ISTAT)
          IF( LP     .NE. 1 ) CALL UPCLOS(LP,'  ',ISTAT)
            GO TO 900
  190   IF( PGMNAM .NE. 'SHEFPARS' ) GO TO 200
          IF( ICD    .NE. 1 ) CALL UPCLOS(ICD,'  ',ISTAT)
          IF( LP     .NE. 1 ) CALL UPCLOS(LP,'  ',ISTAT)
          IF( LCHN   .NE. 1 ) CALL UPCLOS(LCHN,'  ',ISTAT)
          IF( JCHN   .NE. 1 ) CALL UPCLOS(JCHN,'  ',ISTAT)
            GO TO 900
  200   IF( PGMNAM .NE. 'SHEFPOST' ) GO TO 210
          IF( ICD    .NE. 1 ) CALL UPCLOS(ICD,'  ',ISTAT)
          IF( LP     .NE. 1 ) CALL UPCLOS(LP,'  ',ISTAT)
          IF( LPE    .NE. 1 ) CALL UPCLOS(LPE,'  ',ISTAT)
          IF( JCHN   .NE. 1 ) CALL UPCLOS(JCHN,'  ',ISTAT)
            GO TO 900
  210   IF( PGMNAM .NE. 'GOESDB' ) GO TO 220
          IF( ICD    .NE. 1 ) CALL UPCLOS(ICD,'  ',ISTAT)
          IF( LP     .NE. 1 ) CALL UPCLOS(LP,'  ',ISTAT)
            GO TO 900
  220   IF( PGMNAM .NE. 'SASMDB' ) GO TO 230
          IF( ICD    .NE. 1 ) CALL UPCLOS(ICD,'  ',ISTAT)
          IF( LP     .NE. 1 ) CALL UPCLOS(LP,'  ',ISTAT)
            GO TO 900
  230   IF( PGMNAM .NE. 'REORDER' ) GO TO 240
          IF( ICD    .NE. 1 ) CALL UPCLOS(ICD,'  ',ISTAT)
          IF( LP     .NE. 1 ) CALL UPCLOS(LP,'  ',ISTAT)
                              CALL UPCLOS(9,'  ',ISTAT)
            GO TO 900
  240   CONTINUE
          IF( ICD    .NE. 1 ) CALL UPCLOS(ICD,'  ',ISTAT)
          IF( LP     .NE. 1 ) CALL UPCLOS(LP,'  ',ISTAT)
          IF( ICDPUN .NE. 1 ) CALL UPCLOS(ICDPUN,'  ',ISTAT)

  900   CONTINUE

C  Release the OFS lock

        CALL FREE_OFS_LOCK(KOND)

CCC     CALL UPACT(0,PGMNAM,INTS(8))
CCC     CALL UPACT(1,'  finished',INTS(10))
CCC     CALL UPACT(2,'  ',INTS(0))

      RETURN
      END
