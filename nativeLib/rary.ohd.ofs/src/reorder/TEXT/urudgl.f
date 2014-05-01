C MODULE URUDGL
C-----------------------------------------------------------------------
C
      SUBROUTINE URUDGL (PARMID,PTYPE,IRECNW,LARRAY,IARRAY,ISTAT)
C
C    THIS ROUTINE UPDATES THE PCPN, TEMP, PE AND RRS RECORD NUMBERS
C    IN THE GENL PARAMETER RECORD. THEY HAVE BEEN PREVIOUSLY SET
C    TO -9999 WHEN THE GENL PARAMETER RECORDS WERE COPIED TO THE
C    NEW FILES. ANY RECORD NUMBERS NOT SET TO -9999 ARE INVALID.
C
C    ARGUMENT LIST:
C
C       NAME     TYPE   I/O  DIM     DESCRIPTION
C       ------   ----   ---  ------  -----------
C       PARMID    A8     I     2     STATION IDENTIFIER
C       PTYPE     A4     I     1     PARAMETER TYPE
C       IRECNW    I*4    I     1     NEW RECORD LOCATION NUMBER
C       LARRAY    I*4    I     1     LENGTH OF IARRAY
C       IARRAY    I*4   I/O  LARRAY  GENERAL PARAM RECORD WORK ARRAY
C       ISTAT     I*4    O     1     STATUS CODE:
C                                      0=NORMAL RETURN
C                                      1=SYSTEM ERROR
C                                      2=TYPE NOT FOUND IN RECORD
C                                      3=RECORD NUMBER UNDEFINED
C
      CHARACTER*4 PTYPE,XPTYPE
      CHARACTER*8 PARMID
C
      DIMENSION IARRAY(LARRAY)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'urcommon/urcdta'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urudgl.f,v $
     . $',                                                             '
     .$Id: urudgl.f,v 1.4 2001/06/13 14:09:42 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPPTR.GT.0.OR.IPDTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*) 'ENTER URUDGL'
         ENDIF
C
C  READ GENL PARAMETER RECORD
      IPTR=0
      IAMORD=1
      CALL RPPREC (PARMID,'GENL',IPTR,LARRAY,IARRAY,NFILL,IPTRNX,ISTAT)
      IAMORD=0
      IF (ISTAT.NE.0) THEN
         CALL SRPPST (PARMID,'GENL',IPTR,LARRAY,NFILL,IPTRNX,ISTAT)
         IWURFL=1
         GO TO 30
         ENDIF
      IF (IPPDB.GT.0.OR.IPDDB.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,50) PARMID,IPTR,IPTRNX
         ENDIF
C
      IF (PARMID.EQ.'CANC2') THEN
         IFOUND=1
         ENDIF
C
C  FIND TYPE IN ARRAY
      IPOS=18
      CALL UMEMOV (IARRAY(IPOS),REAL,1)
      NUMGPS=REAL
      IPOS=IPOS+1
      DO 10 I=1,NUMGPS
         IF (IPPDB.GT.0.OR.IPDDB.GT.0) THEN
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,60) NUMGPS,IARRAY(IPOS)
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,70) (IARRAY(N),N=18,27)
            ENDIF
         CALL UMEMOV (IARRAY(IPOS),XPTYPE,1)
         IF (XPTYPE.EQ.PTYPE) GO TO 20
         IPOS=IPOS+1
10       CONTINUE
C
C  TYPE NOT FOUND IN PARAMETER RECORD
      WRITE (LP,80) PTYPE
      CALL SUERRS (LP,2,-1)
      ISTAT=2
      GO TO 30
C
C  CHECK RECORD NUMBER
20    IPOS=IPOS+NUMGPS
      IF (IPPDB.GT.0.OR.IPDDB.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,90) PARMID,IPOS,IARRAY(IPOS)
         ENDIF
      ICKVAL=-9999
      IF (IARRAY(IPOS).NE.ICKVAL) THEN
         WRITE (LP,100) PTYPE,PARMID,ICKVAL
         CALL SUERRS (LP,2,-1)
         ISTAT=3
         GO TO 30
         ENDIF
C
C  UPDATE RECORD NUMBER
      RECNW=IRECNW
      RECNW=RECNW+0.01
      IF (IPPDB.GT.0.OR.IPDDB.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,110) PARMID,PTYPE,IARRAY(IPOS),RECNW
         ENDIF
      CALL UMEMOV (RECNW,IARRAY(IPOS),1)
C
C  WRITE PARAMETER RECORD
      IAMORD=1
      CALL WPPREC (PARMID,'GENL',NFILL,IARRAY,IPTR,ISTAT)
      IAMORD=0
      IF (ISTAT.NE.0) THEN
         CALL SWPPST (PARMID,'GENL',NFILL,IPTR,ISTAT)
         IWURFL=1
         GO TO 30
         ENDIF
      IF (IPPDB.GT.0.OR.IPDDB.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,120) PARMID,PTYPE,RECNW
         ENDIF
C
30    IF (IPPTR.GT.0.OR.IPDTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*) 'EXIT URUDGL'
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' PARMID=',A,3X,'IPTR=',I6,3X,'IPTRNX=',I6)
60    FORMAT (' NUMGPS=',I4,3X,'IARRAY(IPOS)=',A4)
70    FORMAT (' IARRAY= ',F5.2,A,7F9.2)
80    FORMAT ('0*** ERROR - IN URUDGL - TYPE ',A,
     *   ' NOT FOUND IN GENL PARAMETER RECORD.')
90    FORMAT (' PARMID=',A,3X,'IPOS=',I3,3X,'IARRAY(IPOS)=',F9.2)
100   FORMAT ('0*** ERROR - IN URUDGL - RECORD LOCATION FOR ',A,
     *   ' PARAMETERS IN GENL PARAMETERS FOR STATION ',A,
     *   ' IS NOT ',I6,'.')
110   FORMAT (' PARMID=',A,3X,'PTYPE=',A,3X,'IARRAY(IPOS)=',I6,3X,
     *   'RECNW=',F9.2)
120   FORMAT (' PARMID=',A,3X,'PTYPE ',A,3X,'RECNW=',F9.2)
C
      END
