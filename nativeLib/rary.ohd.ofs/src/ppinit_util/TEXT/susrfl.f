C MODLUE SUSRFL
C-----------------------------------------------------------------------
C
C  ROUTINE SUSRFL ALLOCATES THE FILES FOR THE SPECIFIED USER.
C
      SUBROUTINE SUSRFL (USERN,TYPMSG,ISTAT)
C
      CHARACTER*8 USERN,TYPMSG
      CHARACTER*44 QLFNEW
      CHARACTER*150 PATHN
C
      INCLUDE 'uio'
      INCLUDE 'uunits'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suddsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/susrfl.f,v $
     . $',                                                             '
     .$Id: susrfl.f,v 1.4 2000/07/21 20:01:52 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,10)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('SYS ')
C
      ISTAT=0
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'IDBALC=',IDBALC
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      CALL USRNEW (USERN,PATHN,IERR)
C
      IPRQLF=0
      QLFNEW=' '
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C    
C  ALLOCATE USER PARAMETER DATASET
C
      IPOS=2
      IF (IDBALC(IPOS).EQ.1) THEN
         CALL UALCDB ('UPRM',USERN,IPRQLF,QLFNEW,TYPMSG,IERR)
         IF (IERR.EQ.0) THEN
            CALL SUDOPN (1,'UPRM',IERR)
            ELSE
               IDBALC(IPOS)=-1
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  ALLOCATE HCL LOCAL DATASETS
C
      IPOS=3
      IF (IDBALC(IPOS).EQ.1) THEN
         CALL UALCDB ('LHCL',USERN,IPRQLF,QLFNEW,TYPMSG,IERR)
         IF (IERR.EQ.0) THEN
            CALL SUDOPN (1,'HCL ',IERR)
            ELSE
               IDBALC(IPOS)=-1
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  ALLOCATE PREPROCESSOR DATASETS
C
      IPOS=4
      IF (IDBALC(IPOS).EQ.1) THEN
         CALL UALCDB ('PPD',USERN,IPRQLF,QLFNEW,TYPMSG,IERR)
         IF (IERR.EQ.0) THEN
            CALL SUDOPN (1,'PPD ',IERR)
            ELSE
               IDBALC(IPOS)=-1
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  ALLOCATE PREPROCESSOR PARAMETRIC DATASETS
C
      IPOS=5
      IF (IDBALC(IPOS).EQ.1) THEN
         CALL UALCDB ('PPP',USERN,IPRQLF,QLFNEW,TYPMSG,IERR)
         IF (IERR.EQ.0) THEN
            CALL SUDOPN (1,'PPP ',IERR)
            ELSE
               IDBALC(IPOS)=-1
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  ALLOCATE PROCESSED DATASETS
C
      IPOS=6
      IF (IDBALC(IPOS).EQ.1) THEN
         CALL UALCDB ('PRD',USERN,IPRQLF,QLFNEW,TYPMSG,IERR)
         IF (IERR.EQ.0) THEN
            CALL SUDOPN (1,'PRD ',IERR)
            ELSE
               IDBALC(IPOS)=-1
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  ALLOCATE FORECAST COMPONENT DATASETS
C
      IPOS=7
      IF (IDBALC(IPOS).EQ.1) THEN
         CALL UALCDB ('FC',USERN,IPRQLF,QLFNEW,TYPMSG,IERR)
         IF (IERR.EQ.0) THEN
            CALL SUDOPN (1,'FC  ',IERR)
            ELSE
               IDBALC(IPOS)=-1
            ENDIF
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'IDBALC=',IDBALC
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,20)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER SUSRFL')
20    FORMAT (' *** EXIT SUSRFL')
C
      END
