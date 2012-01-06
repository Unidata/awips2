C MEMBER CZROR2
C  (from old member RWCZRORD)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE CZROR2 (IBEG,IEND,ISTAT)
C
C  ROUTINE  CZROR2  INITIALIZES ALL THE VARIABLES FOR A RANGE OF SLOTS
C  IN THE BOOKKEEPING SYSTEM FOR READING TIME SERIES DATA.
C
      INCLUDE 'uio'
      INCLUDE 'clbcommon/cdbugx'
      INCLUDE 'clbcommon/crwdef'
      INCLUDE 'clbcommon/crdts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/rdwt3/RCS/czror2.f,v $
     . $',                                                             '
     .$Id: czror2.f,v 1.1 1996/05/24 16:34:18 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICRWTR.GT.0) THEN
         CALL ULINE (IOCDBG,1)
         WRITE (IOCDBG,*) '*** EXIT CZROR2 - ',
     *      'IBEG=',IBEG,
     *      'IEND=',IEND
         ENDIF
C
      ISTAT=0
C
      IF (IBEG.LE.0.OR.IBEG.GT.MAXRD) ISTAT=1
      IF (IEND.LE.0.OR.IEND.GT.MAXRD) ISTAT=1
      IF (IBEG.GT.IEND) ISTAT=1
C
      IF (ISTAT.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,40) IBEG,IEND
         GO TO 30
         ENDIF
C
      DO 20 I=IBEG,IEND
         NREF(I)=0
         NXR(I)=0
         INC(I)=0
         C1(I)=0.0
         C2(I)=0.0
         NV(I)=0
         LREAD(I)=0
         NMON1(I)=0
         BNDS(1,I)=0.
         BNDS(2,I)=0.
         INT(I)=0
         LTSH(I)=0
20       CONTINUE
C
      IF (ICRWTR.GT.0) THEN
         CALL ULINE (IOCDBG,1)
         WRITE (IOCDBG,*) '*** EXIT CZROR2 - ',
     *      'ISTAT=',ISTAT
         ENDIF
30    RETURN
C
40    FORMAT ('+*** ERROR - IN CZROR2 - VALUES FOR ',
     *   'FIRST (',I3,') ',
     *   'AND/OR ',
     *   'LAST (',I3,') ',
     *   'POSITIONS TO BE INITIALIZED ARE INVALID.')
C
      END
