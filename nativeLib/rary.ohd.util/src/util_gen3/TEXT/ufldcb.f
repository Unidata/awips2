C MEMBER UFLDCB
C-----------------------------------------------------------------------
C
C  ROUTINE TO GET DATASET DCB INFORMATION
C
      SUBROUTINE UFLDCB (DDN,MEMBR,FILE,LRECL,LBLK,ISTAT)
C
      CHARACTER*(*) FILE
      CHARACTER*8 DDN,MEMBR,TMEMBR
      CHARACTER*8 BLNK8/' '/
      CHARACTER*80 ARRAY
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
      INCLUDE 'ufldcx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/ufldcb.f,v $
     . $',                                                             '
     .$Id: ufldcb.f,v 1.1 1995/09/17 19:03:45 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) WRITE (ICMPRU,90)
C
      IF (ICMDBG.GT.0) WRITE (ICMPRU,100) DDN,MEMBR
C
      ISTAT=0
      LRECL=0
      LBLK=0
C
C  CHECK IF COMMON BLOCK ALEADY FILLED
      IF (IFLDCX.GT.0) GO TO 60
C
      LARRAY=80
      NFILE=0
C
      IPRERR=1
C
C  CHECK IF DDNAME IS ALLOCATED
      NUNIT=0
      CALL UDDNST (DDN,NUNIT,IPRERR,IERR)
      IF (IERR.EQ.0) GO TO 10
         ISTAT=1
         GO TO 80
10    TMEMBR=MEMBR
      NUMREC=0
C
C  READ RECORDS FROM MEMBER OF PDS
20    CALL URDPDS (DDN,TMEMBR,IPRERR,ARRAY,LARRAY,NUMREC,IFLAG,IERR)
      IF (IERR.EQ.2) GO TO 50
      IF (IERR.EQ.0) GO TO 30
         ISTAT=2
         GO TO 80
30    IF (ICMDBG.GT.0) WRITE (ICMPRU,150) ARRAY
C
C  CHECK FOR COMMENT CARD
      IF (ARRAY(1:1).EQ.'*') GO TO 20
C
C  CHECK FOR UNIT CARD
      IF (ARRAY(1:4).NE.'UNIT') GO TO 40
         CALL SUBSTR (ARRAY,6,4,DUNITX,1)
         IF (ICMDBG.GT.0) WRITE (ICMPRU,160) DUNITX
         GO TO 20
C
C  STORE DATASET ATTRIBUTES
40    NFILE=NFILE+1
      CALL SUBSTR (ARRAY,1,8,FILEX(NFILE),1)
      CALL FFA2I (ARRAY,10,5,1,LRECLX(NFILE),IERR)
      CALL FFA2I (ARRAY,16,5,1,LBLKX(NFILE),IERR)
      IF (ICMDBG.GT.0)
     *    WRITE (ICMPRU,110) NFILE,FILEX(NFILE),LRECLX(NFILE),
     *        LBLKX(NFILE)
      GO TO 20
C
C  CLOSE FILE
50    CALL URDPDS (DDN,BLNK8,IPRERR,ARRAY,LARRAY,NUMREC,IFLAG,IERR)
      IF (ICMDBG.GT.0) WRITE (ICMPRU,140) TMEMBR
      IFLDCX=1
C
60    IF (FILE.EQ.BLNK8) GO TO 80
C
C  CHECK FOR FILE NAME
      DO 70 I=1,NFILE
         IF (FILE.NE.FILEX(I)) GO TO 70
            LRECL=LRECLX(I)
            LBLK=LBLKX(I)
            WRITE (LP,130) LRECL,LBLK,FILE
            GO TO 80
70       CONTINUE
      WRITE (LP,120) FILE
      ISTAT=3
C
80    IF (ICMTRC.GT.0) WRITE (ICMPRU,170)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    FORMAT (' *** ENTER UFLDCB')
100   FORMAT (' DDN=',A8,3X,'MEMBER=',A8)
110   FORMAT (' NFILE=',I2,3X,'FILEX=',A8,3X,'LRECLX=',I5,3X,
     *   'LBLKX=',I5)
120   FORMAT ('0*** ERROR - IN UFLDCB - FILE ',A8,' NOT RECOGNIZED.')
130   FORMAT ('0LOGICAL RECORD LENGTH IS ',I5,' AND BLOCK SIZE IS ',I5,
     *   ' FOR FILE ',A8,'.')
140   FORMAT (' MEMBER ',A8,' SUCCESSFULLY PROCESSED')
150   FORMAT (' ARRAY=',A)
160   FORMAT (' DUNITX=',A4)
170   FORMAT (' *** EXIT UFLDCB')
C
      END
