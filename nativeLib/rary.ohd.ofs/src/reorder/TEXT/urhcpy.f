C MODULE URHCPY
C-----------------------------------------------------------------------
C
      SUBROUTINE URHCPY (LWORK,WORK,NOVPRT,ISTAT)
C
C  ROUTINE TO COPY HYDROLOGIC COMMAND LANQUAGE LOCAL DATASETS.
C
      CHARACTER*8 DDN/'FTXXF001'/
C
      DIMENSION WORK(LWORK)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hcuuno'
CCC      INCLUDE 'urcommon/urhunt'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urhcpy.f,v $
     . $',                                                             '
     .$Id: urhcpy.f,v 1.3 2003/08/21 07:54:35 scv Exp $
     . $' /
C    ===================================================================
C
C
      IF (IUTLTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*) 'ENTER URHCPY'
         ENDIF
C
      ISTAT=0
C
      CALL SULINE (LP,2)
      WRITE (LP,60)
      CALL SULINE (LP,2)
      WRITE (LP,60)
      DO 10 I=1,NOVPRT
         CALL SULINE (LP,0)
         WRITE (LP,70)
10       CONTINUE
      CALL SULINE (LP,2)
      WRITE (LP,60)
C
      IPRERR=0
C
C  CHECK IF INDEX FILE ALLOCATED
      CALL UFXDDN (DDN,KLOXUI,IERR)
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.GT.0) GO TO 20
      CALL UFXDDN (DDN,KLOXUO,IERR)
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.GT.0) GO TO 20
C
C  COPY INDEX FILE
      LPRINT=0
      NTDSN=0
      NTRIN=1
      NTROUT=0
      NUNIT=1
      NREC1=1
      NRECL=-2
      CALL UDACPY (LPRINT,NUNIT,KLOXUI,KLOXUO,NREC1,NRECL,
     *   LWORK,WORK,NTDSN,NTRIN,NTROUT,IERR)
C
C  CHECK IF DEFINITION FILE ALLOCATED
20    CALL UFXDDN (DDN,KLOFUI,IERR)
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.GT.0) GO TO 30
      CALL UFXDDN (DDN,KLOFUO,IERR)
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.GT.0) GO TO 30
C
C  COPY DEFINITION FILE
      LPRINT=0
      NTDSN=0
      NTRIN=1
      NTROUT=0
      NUNIT=1
      NREC1=1
      NRECL=-2
      CALL UDACPY (LPRINT,NUNIT,KLOFUI,KLOFUO,NREC1,NRECL,
     *   LWORK,WORK,NTDSN,NTRIN,NTROUT,IERR)
C
C  CHECK IF DEFAULT FILE ALLOCATED
30    CALL UFXDDN (DDN,KLODUI,IERR)
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.GT.0) GO TO 40
      CALL UFXDDN (DDN,KLODUO,IERR)
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.GT.0) GO TO 40
C
C  COPY DEFAULT FILE
      LPRINT=0
      NTDSN=0
      NTRIN=1
      NTROUT=0
      NUNIT=1
      NREC1=1
      NRECL=-2
      CALL UDACPY (LPRINT,NUNIT,KLODUI,KLODUO,NREC1,NRECL,
     *   LWORK,WORK,NTDSN,NTRIN,NTROUT,IERR)
C
40    CALL SULINE (LP,2)
      WRITE (LP,60)
      CALL SULINE (LP,2)
      WRITE (LP,60)
      DO 45 I=1,NOVPRT
         CALL SULINE (LP,0)
         WRITE (LP,80)
45       CONTINUE
      CALL SULINE (LP,2)
      WRITE (LP,60)
C
      IF (IUTLTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*) 'EXIT URHCPY'
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT ('0')
70    FORMAT ('+',4('***  BEGIN LHCL FILE COPY  '),'***')
80    FORMAT ('+',4('***  END LHCL FILE COPY  '),'***')
C
      END
