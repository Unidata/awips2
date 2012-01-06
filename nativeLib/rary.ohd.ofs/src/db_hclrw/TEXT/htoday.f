C MODULE HTODAY
C-----------------------------------------------------------------------
C
      SUBROUTINE HTODAY (ICARDR)
C
C  ROUTINE TO SET THE CURRENT HYDRLOGIC DAY USING THE USER SPECIFIED
C  VALUE.
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'ufreei'
      INCLUDE 'common/where'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'hclcommon/hcomnd'
C
      CHARACTER*8 OPNOLD
      DIMENSION ITBUF(7)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/htoday.f,v $
     . $',                                                             '
     .$Id: htoday.f,v 1.4 2005/10/31 18:13:12 xfan Exp $
     . $' /
C    ===================================================================
C
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'ENTER HTODAY'
C
      IOPOLD=IOPNUM
      CALL UMEMOV (OPNAME,OPNOLD,2)
      IOPNUM=0
      CALL UREPET (' ',OPNAME,8)
C
      IF (ICARDR.EQ.1) THEN
C     READ CARD
         NNCARD=1
         CALL HCARDR (NNCARD,ISTAT)
         IF (ISTAT.NE.0) THEN
            WRITE (LP,10)
10    FORMAT ('0**ERROR** IN HTODAY - SYSTEM ERROR.')
            CALL ERROR
            GO TO 60
            ENDIF
         ENDIF
C
C  CHECK NUMBER OF FIELDS ON CARD
      IF (NFIELD.NE.2) THEN
         WRITE (LP,20)
20    FORMAT ('0**ERROR** IN HTODAY - INVALID NUMBER OF FIELDS.')
         CALL ERROR
         GO TO 60
         ENDIF
C
C  GET SECOND FIELD
      NFLD=2
      K=IFSTRT(NFLD)
      N=IFSTOP(NFLD)
      IF (IBUF(K).EQ.IASTR) THEN
         WRITE (LP,30) IASTR
30    FORMAT ('0**ERROR** IN HTODAY - ''',A1,
     *   ''' NOT ALLOWED IN DATE FIELD.')
         CALL ERROR
         GO TO 60
         ENDIF
      IF (IBUF(K).EQ.IPOUND) THEN
         WRITE (LP,30) IPOUND
         CALL ERROR
         GO TO 60
         ENDIF
      IF (IBUF(K).EQ.IPRCNT) THEN
         WRITE (LP,30) IPRCNT
         CALL ERROR
         GO TO 60
         ENDIF
C
C  CHECK FOR VALID DATE
      CALL HCKDAT (K,N,ITBUF,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,40)
40    FORMAT ('0**ERROR** IN HTODAY - INVALID DATE SPECIFIED.')
         CALL ERROR
         GO TO 60
         ENDIF
C
C  MOVE DATE INTO TDATES ARRAY
50    CALL UMEMOV (ITBUF(2),TDATES(2),3)
C
60    IF (IHCLDB.GT.1) WRITE (IOGDB,70) TDATES
70    FORMAT (' IN HTODAY - TDATES=',
     .        I7,2(1X,I2),1X,I4,1X,I2,1X,A4,1X,I2)
C
      IOPNUM=IOPOLD
      CALL UMEMOV (OPNOLD,OPNAME,2)
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'EXIT HTODAY'
C
      RETURN
C
      END
