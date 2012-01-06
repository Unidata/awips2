C MEMBER SUCMD2
C  (from old member SUCMDS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/11/95.14:35:57 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE SUCMD2 (FTXX,DDNAME,IUCLOG,IPRERR,IOPCLG,IERR)
C
      INCLUDE 'uio'
C
      CHARACTER*8 FTXX,DDNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sucmd2.f,v $
     . $',                                                             '
     .$Id: sucmd2.f,v 1.1 1995/09/17 19:15:17 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET DDNAME
      DDNAME=FTXX
      CALL UFXDDN (DDNAME,IUCLOG,IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (LP,40) IERR
         CALL SUWRNS (LP,2,-1)
         GO TO 20
C
C  CHECK IF DDNAME ALLOCATED
10    CALL UDDST (DDNAME,IPRERR,IERR)
      IF (IERR.EQ.0) GO TO 20
         WRITE (LP,30) DDNAME
         CALL SUWRNS (LP,2,-1)
         IOPCLG=0
C
20    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT ('0*** WARNING - DDNAME ',A,' IS NOT ALLOCATED. ',
     *   'COMMAND LOG OPTION CANCELLED.')
40    FORMAT ('0*** WARNING - UFXDDN STATUS CODE=',I3)
C
      END
