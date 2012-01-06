C MODULE EDELSG
C----------------------------------------------------------------------
C
      SUBROUTINE EDELSG (IOPT,ISGREC,IDSEG,ISEGST)
C
C   THIS SUBROUTINE MARKS A SEGMENT ON THE ESPFILE AS 'OBSOLETE'.
C
C       IOPT=0    USE RECORD - COMMON ESPSEG CONTAINS SEG INFO
C       IOPT=1    USE ID TO FIND RECORD - COMMON ESPSEG FILLED BY
C                 READING RECORD
C       ISEGST=0  DO NOT WRITE TO SEGMENT STATUS FILE
C       ISEGST=1  WRITE TO SEGMENT STATUS FILE
C
C   THIS SUBROUTINE WAS ORIGINALLY WRITTEN BY GERALD N. DAY.
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/espseg'
      INCLUDE 'common/esprec'
      INCLUDE 'common/eunit'
      INCLUDE 'common/ets'
      INCLUDE 'common/esp'
      INCLUDE 'common/ep'
C
      CHARACTER*8 IDSEG,OLDOPN
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/edelsg.f,v $
     . $',                                                             '
     .$Id: edelsg.f,v 1.5 2002/02/11 20:14:53 dws Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=0
      CALL FSTWHR ('EDELSG  ',IOPNUM,OLDOPN,IOLDOP)
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER EDELSG'
C
      IREC=ISGREC
      IF (IOPT.EQ.0) GO TO 10
C
C  FIND SEGMENT OF FORECAST COMPONENT DATA BASE
      CALL EFNDSG (IDSEG,IREC,IER)
      IF (IER.NE.0) GO TO 40
C
C  GET ESP SEGMENT DEFINITION
      ICODE=0
      ICHK=1
      CALL ESPRDF (ICODE,ICHK,IREC,TSESP,MTSESP,PESP,MPESP,SPESP,MSPESP,
     *   IER)
      IF (IER.NE.0) GO TO 40
C
10    CALL UMEMOV ('OBSOLETE',ESPDAT(1),2)
      ESPDAT(3)=NSREC+.01
      CALL UWRITT (KEPARM,IREC,ESPDAT,IERR)
      IEREC=0
C
      IF (ISEGST.EQ.0) GO TO 20
      CALL UWRITT (KFSGST,IRSEG,IDSEGN,IERR)
C
20    WRITE (IPR,30) IDSEG,IREC
30    FORMAT ('0**NOTE** ESP SEGMENT ',A,' FOUND IN FILE ESPPARM ',
     *   'AT RECORD ',I5,' SUCCESSFULLY DELETED.')
C
40    CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
      END
