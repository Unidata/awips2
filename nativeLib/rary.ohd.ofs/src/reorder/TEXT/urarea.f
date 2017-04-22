C MODULE URAREA
C-----------------------------------------------------------------------
C
       SUBROUTINE URAREA (LPARRY,IPARRY,LWORK,IWORK,ISTAT)
C
C  THIS ROUTINE WILL REORDER THE MAP, MAPS, MAT AND MAPE PARAMETER
C  RECORDS IN THE SAME ORDER AS THE TIME SERIES OF THAT TYPE IN THE 
C  NEW SET OF FILES.
C
C  ARGUMENT LIST:
C       NAME    TYPE  I/O  DIM     DESCRIPTION
C       ------  ----  ---  ------  -----------
C       LPARRY    I    I     1     LENGTH OF WORK ARRAY
C       IPARRY    I   I/O  LPARRY  WORK ARRAY FOR PARM RECORD
C       LWORK     I    I    1      LENGTH OF WORK ARRAY
C       IWORK     I   I/O  LWORK   WORK ARRAY FOR MAPS RECORD
C       ISTAT     I    O      1    STATUS CODE:
C                                     0=NORMAL RETURN
C                                     OTHER=ERROR
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'prdcommon/pdatas'
      INCLUDE 'urcommon/urppdt'
      INCLUDE 'urcommon/urxctl'
      INCLUDE 'urcommon/urftbl'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urcdta'
C
      CHARACTER*4 XPARM      
      PARAMETER (NPARM=4)
      CHARACTER*4 PARMTP(NPARM)/'MAPS','MAP ','MAT ','MAPE'/
      CHARACTER*4 KPARM(NPARM)/'MAP ','MAP ','MAT ','MAPE'/
      CHARACTER*8 PARMID,TYPMSG
C
      DIMENSION IPARRY(LPARRY),IWORK(LWORK)
      DIMENSION IWORK2(16)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urarea.f,v $
     . $',                                                             '
     .$Id: urarea.f,v 1.3 1998/07/06 13:20:23 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPPTR.GT.0) THEN
         CALL SULINE (IODBG,1)
         WRITE (IOGDB,50)
         ENDIF
C
      IF (IPPDB.GT.0) THEN
         CALL SULINE (IODBG,1)
         WRITE (IOGDB,*)
     *      'LPARRY=',LPARRY,
     *      'LWORK=',LWORK,
     *      ' '
         ENDIF
C
      ISTAT=0
C
C  PROCESSED EACH TIME SERIES TYPE
      DO 30 IPARM=1,NPARM
         CALL SULINE (LP,2)
         WRITE (LP,60) PARMTP(IPARM)
         NREC=0
C     FIND TYPE IN PREPROCESSOR DATA BASE DATA DIRECTORY
         IAMORD=1
         CALL PFDTYP (KPARM(IPARM),INDXD)
         IF (INDXD.EQ.0) THEN
            CALL SULINE (LP,2)
            WRITE (LP,70) PARMTP(IPARM)
            GO TO 30
            ENDIF
C     SET RECORD NUMBER OF FIRST TIME SERIES OF TYPE
         ISTRT=IDATFL(8,INDXD)
         IF (ISTRT.EQ.0) THEN
            CALL SULINE (LP,2)
            WRITE (LP,80) KPARM(IPARM)
            GO TO 30
            ENDIF
C     SET UNIT NUMBER
         IUNIT=IDATFL(2,INDXD)-KUPRDO
C     READ TIME SERIES RECORD
10       CALL RVLRCD (IUNIT,ISTRT,1,IWORK2,LRECLT,ISTAT)
         IF (ISTAT.NE.0) THEN
            WRITE (LP,90) ISTAT
            CALL SUERRS (LP,2,-1)
            IWURFL=1
            GO TO 30
            ENDIF
         IF (IPPDB.GT.0) THEN
            CALL SULINE (IODBG,1)
            WRITE (IOGDB,100) (IWORK2(J),J=4,6),IWORK2(13)
            ENDIF
C     GET AND CHECK TYPE CODE
         CALL UMEMOV (IWORK2(6),XPARM,1)
         IF (XPARM.NE.KPARM(IPARM)) GO TO 30
C     GET IDENTIFIER
         CALL UMEMOV (IWORK2(4),PARMID,2)
C     GET THE PARAMETER RECORD
         IAMORD=0
         IPTR=0
         CALL RPPREC (PARMID,PARMTP(IPARM),IPTR,LPARRY,IPARRY,NUMFIL,
     *      IPTRNX,ISTAT)
         IF (ISTAT.GT.0) THEN
            IF (ISTAT.EQ.2) THEN
               TYPMSG='WARNING'
               WRITE (LP,110) TYPMSG(1:LENSTR(TYPMSG)),
     *            PARMTP(IPARM),PARMID
               CALL SUWRNS (LP,2,-1)
               GO TO 20
               ENDIF
            CALL SRPPST (PARMID,PARMTP(IPARM),IPTR,LPARRY,NUMFIL,IPTRNX,
     *         IPTRNX,ISTAT)
            IWURFL=1
            GO TO 30
            ENDIF
C     FIND TYPE IN DIRECTORY
         IDX=IPCKDT(PARMTP(IPARM))
         IF (IDX.EQ.0) THEN
            WRITE (LP,120) PARMTP(IPARM)
            CALL SUERRS (LP,2,-1)
            IWURFL=1
            GO TO 30
            ENDIF
         IF (IPARM.EQ.2) THEN
C        READ MAPS PARAMEGER RECORD
            IAMORD=1
            IPMAPS=0
            CALL RPPREC (PARMID,PARMTP(1),IPMAPS,LWORK,IWORK,NUMFLM,
     *         IPTRNX,ISTAT)
            IF (ISTAT.GT.0) THEN
               CALL SRPPST (PARMID,PARMTP(1),IPMAPS,LWORK,NUMFLM,IPTRNX,
     *            ISTAT)
               WRITE (LP,130) PARMTP(1),PARMID,ISTAT
               CALL SUERRS (LP,2,-1)
               ENDIF
            ENDIF
C     WRITE MAP PARAMETER RECORD
         IRECNW=0
         IAMORD=1
         CALL WPPREC (PARMID,PARMTP(IPARM),NUMFIL,IPARRY,IRECNW,ISTAT)
         IF (ISTAT.GT.0) THEN
            CALL SWPPST (PARMID,PARMTP(IPARM),NUMFIL,IRECNW,ISTAT)
            IWURFL=1
            GO TO 30
            ENDIF
        IF (IPARM.EQ.2) THEN
C       WRITE MAPS PARAMETER RECORD
            REAL=IRECNW
            REAL=REAL+0.01
            CALL UMEMOV (REAL,IWORK(7),1)
            IF (IPPDB.GT.0) CALL SULINE (IODBG,1)
            IF (IPPDB.GT.0) WRITE (IOGDB,140) PARMID,IRECNW,REAL
            IAMORD=1
            CALL WPPREC (PARMID,PARMTP(1),NUMFLM,IWORK,IPMAPS,ISTAT)
            IF (ISTAT.GT.0) THEN
               CALL SWPPST (PARMID,PARMTP(1),NUMFIL,IPMAPS,ISTAT)
               IWURFL=1
               GO TO 30
               ENDIF
            ENDIF
20       NREC=NREC+1
C     CHECK IF LAST TIME SERIES RECORD PROCESSED
         IF (IWORK2(13).EQ.0) THEN
            CALL SULINE (LP,2)
            WRITE (LP,150) NREC,PARMTP(IPARM)
            ELSE
               ISTRT=IWORK2(13)
               GO TO 10
            ENDIF
30       CONTINUE
C
40    IF (IPPTR.GT.0) THEN
         CALL SULINE (IODBG,1)
         WRITE (IOGDB,160)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER URAREA')
60    FORMAT ('0*** NOTE - BEGIN TO REORDER  << ',A,' PARAMETER >>  ',
     *   'RECORDS.')
70    FORMAT ('0*** NOTE - DATA TYPE ',A,' NOT FOUND IN THE ',
     *   'PROCESSED DATA BASE.')
80    FORMAT ('0*** NOTE - NO TIME SERIES DEFINED FOR DATA TYPE ',A,
     *   ' IN THE PROCESSED DATA BASE.')
90    FORMAT ('0*** ERROR - IN URAREA - ERROR READING RECORD. ',
     *   'RVLRCD STATUS= ',I2)
100   FORMAT (' TS=',3A4,' NEXT=',I8)
110   FORMAT ('0*** ',A,' - IN URAREA - ',A,' PARAMETER RECORD FOR ',
     *   'IDENTIFIER ',A,' NOT FOUND.')
120   FORMAT ('0*** ERROR - IN URAREA - PARAMETER TYPE ',A,
     *   ' IN PREPROCESSOR DATA BASE.')
130   FORMAT ('0*** ERROR - IN URAREA - READING ',A,' PARAMETERS ',
     *   'FOR AREA ',A,' TO SET POINTER TO MAP. ISTAT=',I3)
140   FORMAT (' PARMID=',A,3X,'IRECNW=',I6,3X,'REAL=',F9.2)
150   FORMAT ('0*** NOTE - ',I6,' ',A,' PARAMETER RECORDS HAVE ',
     *   'BEEN SUCCESSFULLY REORDERED.')
160   FORMAT (' *** EXIT URAREA : STATUS=',I2)
C
      END
