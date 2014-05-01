C MODULE URFORD
C-----------------------------------------------------------------------
C
C  ROUTINE TO REORDER THE FORECAST COMPONENT DATA FILES.
C
      SUBROUTINE URFORD (IINIT,
     *   MC,C,MD,D,MP,P,MT,T,MTS,TS,
     *   DCBDDN,DCBMBR,
     *   INDSTA,NOVPRT,TXTLOG,LULOG,
     *   MFUNIT,KFUNIT,LFUNIT,KFUNITO,ISTAT)
C
      CHARACTER*(*) DCBDDN,DCBMBR
      CHARACTER*4 DSKUNT
      CHARACTER*8 FUNNAM
      CHARACTER*8 USER
      CHARACTER*(*) TXTLOG
C
      DIMENSION C(MC),D(MD),P(MP),T(MT),TS(MTS)
      DIMENSION KFUNIT(MFUNIT),LFUNIT(MFUNIT),KFUNITO(MFUNIT)
C     PARAMETER (MLSTRC=1000)
C  HSD-bug r25-55 the number of rating curve cab be exceeded 1000
C     change 1000 to 2000
      PARAMETER (MLSTRC=2000) 
      PARAMETER (MLSTCK=MLSTRC)
      DIMENSION LSTRC(2,MLSTRC)
      INTEGER LSTCK(MLSTCK)/MLSTCK*0/
C
      INCLUDE 'uiox'
      INCLUDE 'uunits'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fccgd'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/fginfo'
      INCLUDE 'common/frcptr'
      INCLUDE 'scommon/suddsx'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'urcommon/urunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urford.f,v $
     . $',                                                             '
     .$Id: urford.f,v 1.9 2005/01/12 17:44:13 xfan Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C
      LDEBUG=0
C
      CALL SULINE (LP,1)
      WRITE (LP,*)
      CALL SULINE (LP,2)
      WRITE (LP,150) '0'
      DO 10 I=1,NOVPRT
         CALL SULINE (LP,0)
         WRITE (LP,150) '+'
10       CONTINUE
      CALL SULINE (LP,1)
      WRITE (LP,*)
C
C  READ HCL CONTROL INFORMATION
      FUNNAM='FCEXEC'
      CALL HLDCBS (FUNNAM,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,160) FUNNAM
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 130
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF NEW FILES TO BE INITIALIZED
      IF (IINIT.EQ.0) GO TO 40
C
      CALL SUPAGE
      TXTLOG='FILE INITIALIZATION'
      CALL URWLOG (' ',TXTLOG,'STARTED  ',' ',LULOG)
C
      CALL SULINE (LP,1)
      WRITE (LP,170) '0'
      DO 20 I=1,NOVPRT
         CALL SULINE (LP,0)
         WRITE (LP,170) '+'
20       CONTINUE
      CALL SULINE (LP,1)
      WRITE (LP,*)
C
C  CHANGE UNIT NUMBERS TO POINT TO NEW FILES
      CALL UMEMOV (KFUNIT,KFUNITO,MFUNIT)
      CALL UMEMOV (LFUNIT,KFUNIT,MFUNIT)
C
C  READ FORECAST COMPONENT DATA BASE CONTROL INFORMATION
      CALL INSTRT
C
C  CHANGE UNIT NUMBERS TO POINT TO OLD FILES
      CALL UMEMOV (KFUNITO,KFUNIT,MFUNIT)
C
C  INITIALIZE NEW FILES
      IXINIT=IINIT
      IF (IXINIT.EQ.0) IXINIT=-1
      USER=' '
      DSKUNT=' '
      IAMORD=1
      CALL FCINT (USER,DCBDDN,DCBMBR,DSKUNT,IXINIT,LDEBUG,IERR)
      IAMORD=0
      IF (IERR.GT.0) THEN
         ISTAT=1
         GO TO 130
         ENDIF
C
      CALL URWLOG (' ',TXTLOG,'COMPLETED',' ',LULOG)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  INITIALIZE COMMON BLOCKS
C
40    CALL URFC0 (IERR)
      IF (IERR.GT.0)  THEN
         ISTAT=1
         GO TO 120
         ENDIF
C
      CALL URDBUG
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IDUMP=1
C
C  CHECK IF STATUS INFORMATION TO BE PRINTED BEFORE REORDERING
      IF (INDSTA.EQ.1.OR.INDSTA.EQ.3) THEN
         ELSE
            GO TO 60
         ENDIF
C
      CALL SULINE (LP,1)
      WRITE (LP,*)
      TXTLOG='FILE STATUS BEFORE REORDERING'
      CALL URWLOG (' ',TXTLOG,'STARTED  ',' ',LULOG)
      CALL SULINE (LP,2)
      WRITE (LP,190) '0'
      DO 50 I=1,NOVPRT
         CALL SULINE (LP,0)
         WRITE (LP,190) '+'
50       CONTINUE
      CALL SULINE (LP,1)
      WRITE (LP,*)
C
C  PRINT STATUS
      CALL FSTATS (IDUMP)
C
      CALL URWLOG (' ',TXTLOG,'COMPLETED',' ',LULOG)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  REORDER DATA BASE
C
60    CALL SUPAGE
      TXTLOG='FILE REORDERING'
      CALL URWLOG (' ',TXTLOG,'STARTED  ',' ',LULOG)
      CALL SULINE (LP,2)
      WRITE (LP,200) '0'
      DO 70 I=1,NOVPRT
         CALL SULINE (LP,0)
         WRITE (LP,200) '+'
70       CONTINUE
      CALL SULINE (LP,1)
      WRITE (LP,*)
C
C  PROCESS CARRYOVER GROUP DEFINITIONS
      CALL URFC1
C
      CALL URDBUG
C
C  PROCESS FORECAST GROUP DEFINITIONS
      CALL URFCB (IERR)
      IF (IERR.GT.0) THEN
         ISTAT=1
         GO TO 120
         ENDIF
C
      CALL URDBUG
C
C  PROCESS SEGMENT DEFINITIONS
      NLSTRC=0
      CALL URFCC (MLSTRC,LSTRC,NLSTRC,
     *   MC,C,MD,D,MP,P,MT,T,MTS,TS,IERR)
      IF (IERR.GT.0)  THEN
         ISTAT=1
         GO TO 120
         ENDIF
C
      CALL URDBUG
C
C  COPY RATING CURVE DEFINITIONS
      CALL URFCRC (LSTRC,NLSTRC,MLSTCK,LSTCK,IERR)
      IF (IERR.GT.0)  THEN
         ISTAT=1
         GO TO 120
         ENDIF
C
      CALL URDBUG
C
      CALL SULINE (LP,2)
      WRITE (LP,210)
C
      CALL SULINE (LP,1)
      WRITE (LP,*)
      CALL SULINE (LP,2)
      WRITE (LP,220) '0'
      DO 80 I=1,NOVPRT
         CALL SULINE (LP,0)
         WRITE (LP,220) '+'
80       CONTINUE
C
      CALL URWLOG (' ',TXTLOG,'COMPLETED',' ',LULOG)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF STATUS TO BE PRINTED
      IF (INDSTA.EQ.2.OR.INDSTA.EQ.3) THEN
         ELSE
            GO TO 120
         ENDIF
C
C  RELOAD COMMONS FCCGD, FCFGS, FCSEGP, FRCPTR AND FGINFO TO GET 
C  THE STATUS OF THE NEW FILES
      CALL UREADT (LFCGD,1,NSLOTS,ISTAT)
      CALL UREADT (LFFGST,1,FGID,ISTAT)
      NFGREC=IDUMYG
C
C  GET THE MAXIMUM NUMBER OF FORECAST GROUPS FROM NEW FCFGSTAT FILE
      IREC=2
      CALL UREADT (LFFGST,IREC,FGID,ISTAT)
      MAXFG=IDUMYG
C
C  READ THROUGH NEW FCFGSTAT FILE AND COMPUTE THE LAST USED RECORD
C  IN THE NEW FCFGLIST FILE
      LUFGL=0
      IF (NFGREC.EQ.0) GO TO 100
      DO 90 I=1,NFGREC
         CALL UREADT (LFFGST,I,FGID,ISTAT)
         IF (IREC+NSEG-1.GT.LUFGL) LUFGL=IREC+NSEG-1
90       CONTINUE
C
100   CALL UREADT (LFSGPT,1,NS,ISTAT)
      CALL UREADT (LFSGPT,2,NRP,ISTAT)
      CALL UREADT (LFRCPT,1,NRC,ISTAT)
C
C  CHANGE UNIT NUMBERS TO POINT TO NEW FILES
      CALL UMEMOV (KFUNIT,KFUNITO,MFUNIT)
      CALL UMEMOV (LFUNIT,KFUNIT,MFUNIT)
C
      CALL URDBUG
C
      CALL SULINE (LP,1)
      WRITE (LP,*)
      TXTLOG='FILE STATUS AFTER REORDERING'
      CALL URWLOG (' ',TXTLOG,'STARTED  ',' ',LULOG)
      CALL SULINE (LP,2)
      WRITE (LP,230) '0'
      DO 110 I=1,NOVPRT
         CALL SULINE (LP,0)
         WRITE (LP,230) '+'
110      CONTINUE
      CALL SULINE (LP,1)
      WRITE (LP,*)
C
      CALL FSTATS (IDUMP)
C
      CALL URWLOG (' ',TXTLOG,'COMPLETED',' ',LULOG)
C
      CALL URDBUG
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  RESET INDICATORS TO CHECK IF DATA BASE ALLOCATED
120   IDBALC(4)=0
      IDBALC(5)=0
      IDBALC(6)=0
C
130   CALL SULINE (LP,1)
      WRITE (LP,*)
      CALL SULINE (LP,2)
      WRITE (LP,240) '0'
      DO 140 I=1,NOVPRT
         CALL SULINE (LP,0)
         WRITE (LP,240) '+'
140      CONTINUE
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
150   FORMAT (A,4('***  BEGIN FC REORDER  '),'***')
160   FORMAT ('0*** ERROR - IN URFORD - FUNCTION NAME ',A,
     *   ' NOT FOUND.')
170   FORMAT (A,4('***  INITIALIZE NEW FC FILES  '),'***')
190   FORMAT (A,3('***  FC STATUS BEFORE REORDERING  '),'***')
200   FORMAT (A,4('***  BEGIN FC REORDERING  '),'***')
210   FORMAT ('0*** NOTE - FORECAST COMPONENT FILES SUCCESSFULLY ',
     *   'REORDERED.')
220   FORMAT (A,4('***  END FC REORDERING  '),'***')
230   FORMAT (A,3('***  FC STATUS AFTER REORDERING  '),'***')
240   FORMAT (A,4('***  END FC REORDER  '),'***')
C
      END
