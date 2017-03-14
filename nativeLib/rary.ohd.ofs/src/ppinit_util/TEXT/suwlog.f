C MODULE SUWLOG
C-----------------------------------------------------------------------
C
C  ROUTINE TO WRITE INFORMATION TO PROGRAM LOG.
C
      SUBROUTINE SUWLOG (TYPE,XNAME,SEQNUM,INTEGR,LOGTYP,ISTAT)
C
      CHARACTER*4 TYPE
      CHARACTER*4 XCMND/'CMND'/
      CHARACTER*4 XOPTN/'OPTN'/
      CHARACTER*8 DDN/'FT??F001'/
      CHARACTER*8 XNAME,SEQNUM,XTYPE
      CHARACTER*8 XCOMND/'COMMAND '/
      CHARACTER*8 XOPTON/'OPTION  '/
C
      INCLUDE 'uiox'
      INCLUDE 'ufreex'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'scommon/supagx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/suwlog.f,v $
     . $',                                                             '
     .$Id: suwlog.f,v 1.3 2001/06/13 14:05:52 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SUWLOG'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('UTIL')
C
      ISTAT=0
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,80) IOPCLG,IOPOLG,TYPE,XNAME,
     *      SEQNUM,INTEGR,LOGTYP
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK FOR VALID VALUE OF LOG TYPE
      IF (LOGTYP.GE.-2.AND.LOGTYP.LE.1) GO TO 10
         WRITE (LP,90) LOGTYP
         CALL SUERRS (LP,2,-1)
         GO TO 60
C
10    IF (LOGTYP.GE.0) GO TO 30
C
C  CHECK IF COMMAND LOG ENTRY TO BE PRINTED
      IF (IOPCLG(1).EQ.0) GO TO 60
C
C  CHECK FOR VALID VALUE OF COMMAND LOG TYPE
      IF (LOGTYP.GE.-2.AND.LOGTYP.LE.-1) GO TO 15
         WRITE (LP,90) LOGTYP
         CALL SUERRS (LP,2,-1)
         GO TO 60
C
15    NUNIT=IOPCLG(2)
C
      ICMCDE=INTEGR
      IF (LOGTYP.EQ.-2) GO TO 20
C
      WRITE (NUNIT,110) XNAME
      CALL SULINE (NUNIT,1)
      GO TO 60
C
20    WRITE (NUNIT,120) XNAME,SEQNUM,ICMCDE
      CALL SULINE (NUNIT,1)
      INTEGR=0
      GO TO 60
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF OPTION LOG ENTRY TO BE PRINTED
30    IF (IOPOLG(1).EQ.0) GO TO 60
C
C  CHECK FOR VALID VALUE OF OPTION LOG TYPE
      IF (LOGTYP.GE.0.AND.LOGTYP.LE.1) GO TO 40
         WRITE (LP,90) LOGTYP
         CALL SUERRS (LP,2,-1)
         GO TO 60
C
C  STORE CARD SEQUENCE NUMBER
40    IF (SEQNUM.EQ.' ') CALL SUSEQN (ICDBUF(73:73),NRDCRD,SEQNUM)
C
C  CHECK LOG TYPE
      IF (TYPE.EQ.XCMND) XTYPE=XCOMND
      IF (TYPE.EQ.XOPTN) XTYPE=XOPTON
C
C  SET OUTPUT UNIT
      NUNIT=IOPOLG(2)
C
C  CHECK IF UNIT ALLOCATED
      CALL UFXDDN (DDN,NUNIT,IERR)
      IPDDST=0
      CALL UDDST (DDN,IPDDST,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,100) DDN
         CALL SUWRNS (LP,2,-1)
         IOPOLG(1)=0
         GO TO 60
         ENDIF
C
C  SET PAGE NUMBER
      NPAGE=INTEGR
      IF (NPAGE.EQ.0) IPAGE=NPSPAG
      IF (NPAGE.GT.0) IPAGE=NPAGE
      IF (XTYPE.EQ.XCOMND.AND.LOGTYP.EQ.1) THEN
         WRITE (NUNIT,130)
         CALL SULINE (NUNIT,1)
         ENDIF
      IF (LOGTYP.EQ.1) THEN
         WRITE (NUNIT,140) XTYPE,XNAME,SEQNUM,IPAGE
         CALL SULINE (NUNIT,1)
         ENDIF
      IF (LOGTYP.EQ.0) THEN
         WRITE (NUNIT,150) XTYPE,XNAME
         CALL SULINE (NUNIT,1)
         ENDIF
C
60    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SUWLOG'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' IOPCLG(1...2)=',2(I2,1X),3X,
     *   ' IOPOLG(1...2)=',2(I2,1X),3X,
     *   'TYPE=',A4,3X,'XNAME=',A8,3X,
     *   'SEQNUM=',A8,3X,'INTEGR=',I4,3X,'LOGTYP=',I2)
90    FORMAT ('0*** ERROR - IN SUWLOG - ',I3,' IS AN INVALID ',
     *   'VALUE FOR LOG TYPE.')
100   FORMAT ('0*** WARNING - IN SUWLOG - DDNAME ',A8,' IS NOT ',
     *   'ALLOCATED. LOG OPTION CANCELLED.')
110   FORMAT (' COMMAND ',A8,' BEING EXECUTED.')
120   FORMAT (' COMMAND ',A8,' FOUND AT INPUT LINE ',A8,' EXECUTED. ',
     *   'CONDITION CODE = ',I2)
130   FORMAT (' ')
140   FORMAT (' *** NOTE - EXECUTION OF ',A7,1X,A8,
     *   ' FOUND AT INPUT LINE  ',A8,' (PAGE ',I4,')  STARTED.')
150   FORMAT (' *** NOTE - EXECUTION OF ',A7,1X,A8,
     *   ' COMPLETED.')
C
      END
