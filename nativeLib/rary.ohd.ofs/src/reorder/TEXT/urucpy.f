C MODULE URUCPY
C-----------------------------------------------------------------------
C
      SUBROUTINE URUCPY (LWORK,WORK,NOVPRT,ISTAT)
C
C  ROUTINE TO COPY THE USER PARAMETER FILE.
C
      CHARACTER*8 DDN/'FTXXF001'/
C
      DIMENSION WORK(LWORK)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'uunits'
      INCLUDE 'urcommon/urunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urucpy.f,v $
     . $',                                                             '
     .$Id: urucpy.f,v 1.3 2003/08/21 07:48:29 scv Exp $
     . $' /
C    ===================================================================
C
C
      IF (IUTLTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*) 'ENTER URUCPY'
         ENDIF
C
      ISTAT=0
C
      CALL SULINE (LP,2)
      WRITE (LP,50) '0'
      DO 10 I=1,NOVPRT
         CALL SULINE (LP,0)
         WRITE (LP,50) '+'
10       CONTINUE
C
      IPRERR=0
C
C  CHECK IF FILE ALLOCATED
      CALL UFXDDN (DDN,KUPARM,IERR)
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.GT.0) GO TO 20
      CALL UFXDDN (DDN,KURPRM,IERR)
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.GT.0) GO TO 20
C
C  COPY FILE
      LPRINT=0
      NTDSN=0
      NTRIN=1
      NTROUT=0
      NUNIT=1
      NREC1=1
      NRECL=-2
      CALL UDACPY (LPRINT,NUNIT,KUPARM,KURPRM,NREC1,NRECL,
     *   LWORK,WORK,NTDSN,NTRIN,NTROUT,IERR)
C
20    CALL SULINE (LP,2)
      WRITE (LP,60) '0'
      DO 25 I=1,NOVPRT
         CALL SULINE (LP,0)
         WRITE (LP,60) '+'
25       CONTINUE
C
      IF (IUTLTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*) 'EXIT URUCPY'
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (A,4('***  BEGIN UPRM FILE COPY  '),'***')
60    FORMAT (A,4('***  END UPRM FILE COPY  '),'***')
C
      END
