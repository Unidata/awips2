C MODULE WPPDEL
C-----------------------------------------------------------------------
C
       SUBROUTINE WPPDEL (PARMID,PARMTP,ISTAT)
C
C  THIS ROUTINE DELETES A PARAMETER RECORD FROM THE PREPROCESSOR
C  PARAMETRIC DATA BASE.
C
C  THE INDEX RECORD IS SET TO -1 AND THE ID IN THE PARAMETER ARRAY
C  IS SET TO 'DELETED'.
C
C  ARGUMENT LIST:
C
C    NAME    TYPE  I/O  DIM  DESCRIPTION
C    ------  ----  ---  ---  -----------
C    PARMID   A8    I    2   IDENTIFIER
C    PARMTP   A4    I    1   PARAMETER TYPE
C    ISTAT    I     O    1   STATUS:
C                              0=OK
C                              1=RECORD NOT FOUND
C                              2=PARAMETER TYPE NOT IN DIRECTORY
C                              3=SYSTEM ERROR ACCESSING PARAMETER FILE
C                              4=SYSTEM ERROR ACCESSING INDEX FILE
C                              5=IDENTIFIER AND TYPE IN INDEX DO NOT
C                                 MATCH THAT FOUND IN PARAMETER RECORD
C
      CHARACTER*4 PARMTP,PARMCK
      CHARACTER*8 PARMID
      DIMENSION IXBUF(4),IWORK(16)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pppcommon/ppmctl'
      INCLUDE 'pppcommon/ppunts'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'urcommon/urppdt'
      INCLUDE 'urcommon/urunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_ppprw/RCS/wppdel.f,v $
     . $',                                                             '
     .$Id: wppdel.f,v 1.2 2001/06/13 13:16:02 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPPTR.GT.0) WRITE (LP,*) 'ENTER WPPDEL - ',
     *   ' PARMID=',PARMID,'PARMTP=',PARMTP
C
      ISTAT=0
C
C  CHECK IF PARMETER TYPE IS IN DIRECTORY
      IDXDAT=IPCKDT(PARMTP)
      IF (IDXDAT.EQ.0) THEN
         IF (IPPDB.GT.0) WRITE (LP,50) PARMTP
         ISTAT=2
         GO TO 40
         ENDIF
C
C  SET THE UNIT NUMBERS FOR INDEX AND PARAMETER FILE
      IF (IAMORD.EQ.0) THEN
         LUINDX=KPPIDX
         LUPRMF=KPPRMU(IPDTDR(2,IDXDAT))
         ENDIF
      IF (IAMORD.EQ.1) THEN
         LUINDX=KURIDX
         LUPRMF=KUPRMI(JPDTDR(2,IDXDAT))
         ENDIF
C
C  FIND THE PARM RECORD AND ALSO THE INDEX RECORD
      IFIND=1
      CALL PPFNDR (PARMID,PARMTP,IFIND,IXBUF,IFREE,ISTAT)
      IXREC=IFIND
      IF (IXREC.EQ.0) THEN
         ISTAT=1
         GO TO 40
         ENDIF
C
C  GOT IT IN THE INDEX, NOW READ THE FIRST PARM RECORD.
      CALL UREADT (LUPRMF,IXBUF(4),IWORK,ISTAT)
      IF (ISTAT.NE.0) GO TO 20
C
C  CHECK IDENTIFIER
      CALL UCMPAR (IWORK(2),PARMID,2,IMATCH)
      IF (IMATCH.NE.0) GO TO 30
C
C  CHECK TYPE
      CALL UMEMOV (IWORK(4),PARMCK,1)
      IF (PARMCK.NE.PARMTP) GO TO 30
C
C  SET ID TO DELETED
      CALL UMEMOV ('DELETED ',IWORK(2),2)
C
C  WRITE PARAMETER ARRAY
      CALL UWRITT (LUPRMF,IXBUF(4),IWORK,ISTAT)
      IF (ISTAT.NE.0) GO TO 10
C
C  UPDATE INDEX
      IXBUF(1)=-1
      IXBUF(2)=0
      CALL UWRITT (LUINDX,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 20
C
C  DECREMENT NUMBER OF RECORDS FOR THIS TYPE IN DIRECTORY
      IF (IAMORD.EQ.0) IPDTDR(5,IDXDAT)=IPDTDR(5,IDXDAT)-1
      IF (IAMORD.EQ.1) JPDTDR(5,IDXDAT)=JPDTDR(5,IDXDAT)-1
      GO TO 40
C
C  READ ERROR
10    IF (IPPDB.GT.0) WRITE (LP,60) LUPRMF,ISTAT
      ISTAT=3
      GO TO 40
C
20    IF (IPPDB.GT.0) WRITE (LP,70) ISTAT
      GO TO 40
C
30    IF (IPPDB.GT.0) WRITE (LP,80) ISTAT
      ISTAT=4
C
40    IF (IPPTR.GT.0) WRITE (IOGDB,90) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT ('0**ERROR** IN WPPDEL - DATA TYPE ',A,' NOT IN',
     *   ' PARAMETER TYPE DIRECTORY')
60    FORMAT ('0**ERROR** IN WPPDEL - READ OR WRITE ERROR. FILE=',I4,
     *   ' STATUS=',I2)
70    FORMAT ('0**ERROR** IN WPPDEL - READ OR WRITE ERROR FROM INDEX.',
     *   ' STATUS=',I2)
80    FORMAT ('0**ERROR** IN WPPDEL - IDENTIFIER AND TYPE IN INDEX DO ',
     *   'NOT MATCH THAT IN PARAMETER ARRAY. STATUS=',I2)
90    FORMAT (' EXIT WPPDEL : ISTAT=',I2)
C
      END
