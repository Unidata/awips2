C MODULE URECPY
C-----------------------------------------------------------------------
C
      SUBROUTINE URECPY (IOESPP,LWORK,WORK,NOVPRT,ISTAT)
C
C  ROUTINE TO COPY ESP PARAMETER AND/OR TIME SERIES FILES.
C
      CHARACTER*8 DDN/'FTXXF001'/
C
      DIMENSION WORK(LWORK)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'urcommon/urunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urecpy.f,v $
     . $',                                                             '
     .$Id: urecpy.f,v 1.3 2003/08/21 07:45:47 scv Exp $
     . $' /
C    ===================================================================
C
C
      IF (IUTLTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*) 'ENTER URECPY'
         ENDIF
C
      ISTAT=0
C
      CALL SULINE (LP,2)
      WRITE (LP,80) '0'
      DO 10 I=1,NOVPRT
         CALL SULINE (LP,0)
         WRITE (LP,80) '0'
10       CONTINUE
C
      IPRERR=1
C
C  CHECK IF PARAMETER FILE HAS BEEN REORDERED
      IF (IOESPP.EQ.1) THEN
         CALL SULINE (LP,2)
         WRITE (LP,90)
         GO TO 30
         ENDIF
C
C  CHECK IF PARAMETER FILE ALLOCATED
      CALL UFXDDN (DDN,KUESPP,IERR)
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.GT.0) GO TO 30
      CALL UFXDDN (DDN,LUESPP,IERR)
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.GT.0) GO TO 30
C
C  COPY PARAMETER FILE
      LPRINT=0
      NTDSN=0
      NTRIN=1
      NTROUT=0
      NUNIT=1
      NREC1=1
      NRECL=-2
      CALL UDACPY (LPRINT,NUNIT,KUESPP,LUESPP,NREC1,NRECL,
     *   LWORK,WORK,NTDSN,NTRIN,NTROUT,IERR)
C
C  COPY TIME SERIES FILES
30    DO 40 IUNIT=1,5
C     CHECK IF TIME SERIES FILE ALLOCATED
         CALL UFXDDN (DDN,KUESPT(IUNIT),IERR)
         CALL UDDST (DDN,IPRERR,IERR)
         IF (IERR.GT.0) GO TO 40
         CALL UFXDDN (DDN,LUESPT(IUNIT),IERR)
         CALL UDDST (DDN,IPRERR,IERR)
         IF (IERR.GT.0) GO TO 40
C     READ CONTROL RECORD FROM NEW FILE
         IREC=1
         CALL UREADT (LUESPT(IUNIT),IREC,WORK,IERR)
         MXREC1=WORK(1)
C     COPY RECORDS
         LPRINT=0
         NTDSN=0
         NTRIN=1
         NTROUT=0
         NUNIT=1
         NREC1=1
         NRECL=-2
         CALL UDACPY (LPRINT,NUNIT,KUESPT(IUNIT),LUESPT(IUNIT),
     *      NREC1,NRECL,LWORK,WORK,NTDSN,NTRIN,NTROUT,IERR)
C     READ CONTROL RECORD FROM NEW FILE
         IREC=1
         CALL UREADT (LUESPT(IUNIT),IREC,WORK,IERR)
         MXREC2=WORK(1)
         IF (IUTLTR.GT.0) THEN
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,*) 'MXREC1=',MXREC1,
     *         'MXREC2=',MXREC2
            ENDIF
         IF (MXREC1.NE.MXREC2) THEN
            WRITE (LP,100) MXREC2,MXREC1
            CALL SULINE (LP,2)
C        UPDATE CONTROL RECORD IN NEW FILE
            WORK(1)=MXREC1+.01
            CALL SULINE (LP,2)
            CALL UWRITT (LUESPT(IUNIT),IREC,WORK,IERR)
            ENDIF
40       CONTINUE
C
      CALL SULINE (LP,2)
      WRITE (LP,110) '0'
      DO 50 I=1,NOVPRT
         CALL SULINE (LP,0)
         WRITE (LP,110) '+'
50       CONTINUE
C
      IF (IUTLTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*) 'EXIT URECPY'
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (A,4('***  BEGIN ESP FILE COPY  '),'***')
90    FORMAT ('0*** NOTE - ESP PARAMETER FILE HAS BEEN ',
     *   'REORDERED AND WILL NOT BE COPIED.')
100   FORMAT ('0*** NOTE - MAXIMUM NUMBER OF RECORDS IN FILE WILL ',
     *   'BE CHANGED FROM ',I5,' TO ',I5,'.')
110   FORMAT (A,4('***  END ESP FILE COPY  '),'***')
C
      END
