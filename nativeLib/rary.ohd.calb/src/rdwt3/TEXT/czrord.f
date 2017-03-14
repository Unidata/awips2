C MEMBER CZRORD
C  (from old member RWCZRORD)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/01/94.14:35:39 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE CZRORD
C
C  ROUTINE  CZRORD  INITIALIZES ALL THE VARIABLES FOR ALL SLOTS
C  IN THE BOOKKEEPING SYSTEM FOR READING TIME SERIES DATA.
C
      INCLUDE 'clbcommon/cdbugx'
      INCLUDE 'clbcommon/crwdef'
      INCLUDE 'clbcommon/crwctl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/rdwt3/RCS/czrord.f,v $
     . $',                                                             '
     .$Id: czrord.f,v 1.1 1996/05/24 16:34:19 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICRWTR.GT.0) THEN
         CALL ULINE (IOCDBG,1)
         WRITE (IOCDBG,*) '*** ENTER CZRORD'
         ENDIF
C
      IBEG=1
      IEND=MAXRD
C
      CALL CZROR2 (IBEG,IEND,IERR)
      IF (IERR.GT.0) IERROR=1
C
      IF (ICRWTR.GT.0) THEN
         CALL ULINE (IOCDBG,1)
         WRITE (IOCDBG,*) '*** EXIT CZRORD'
         ENDIF
C
      RETURN
C
      END
