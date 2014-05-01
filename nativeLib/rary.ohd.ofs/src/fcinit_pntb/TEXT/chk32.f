C MODULE CHK32
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK IF AN FFG AREA IDENTIFIER IS USED BY ANOTHER FFG
C  OPERATION.
C
      SUBROUTINE CHK32 (SEGID,FFGAID,IER)
C
      CHARACTER*8 SEGID,SEGIDO,FFGAID
      CHARACTER*8 RTNNAM,OPNOLD
      CHARACTER*8 SEGID2,FFGAID2
      CHARACTER*8 OPID,OPNAM
C
      DIMENSION IBUF(3)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/flarys'
      INCLUDE 'common/oldp'
      INCLUDE 'common/oldt'
      INCLUDE 'common/oldts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/chk32.f,v $
     . $',                                                             '
     .$Id: chk32.f,v 1.2 2002/02/13 15:40:34 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      RTNNAM='CHK32'
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER ',RTNNAM
C
      IBUG=IFBUG('FFG')
C
      IF (IBUG.GT.0) WRITE (IODBUG,*) ' FFGAID=',FFGAID
C
      IER=0

C  GET OPERATION NUMBER FOR OPERATION NAME
      OPID='FFG'
      CALL FOPCDE (OPID,NUMOP)
      IF (IBUG.GT.0) WRITE (IODBUG,*) 'FOPCDE CALLED : OPID=',OPID,
     *   ' NUMOP=',NUMOP
C
C  SAVE OLD VALUES IN COMMON BLOCKS
      LTSO=LTS
      LPO=LP
      LCO=LC
      LTO=LT
      LDO=LD
C
C  PROCESS EACH SEGMENT
      DO 40 I=1,NS
         IREC=I+2
         CALL UREADT (KFSGPT,IREC,IBUF,IERR)
         CALL UMEMOV (IBUF(1),SEGID2,2)
         IF (SEGID2.EQ.SEGID) GO TO 40
         IRSEGO=IBUF(3)
         SEGIDO=SEGID
C     GET SEGMENT DEFINITION
         ISRCH=0
         NOPARM=0
         NRSEG=-IRSEGO
         CALL FGETSG (SEGID2,NRSEG,MOLDP,OLDP,MOLDT,OLDT,MOLDTS,OLDTS,
     *      ISRCH,NOPARM,IERR)
         IF (IBUG.EQ.1) WRITE (IODBUG,*) 'SEGID2=',SEGID2,
     *      ' IRSEGO=',IRSEGO
         SEGID=SEGIDO
         IF (SEGID2.EQ.'OBSOLETE') GO TO 40
C         SEGID=SEGIDO >> THIS IS THE OLD LINE, MOVED UP TWO LINES!
         IF (IERR.NE.0) THEN
            WRITE (IPR,10) IERR,IRSEGO
10    FORMAT ('0**WARNING** STATUS CODE ',I2,' FROM ROUTINE FGETSG ',
     *   'READING SEGMENT DEFINITION AT RECORD ',I6,'.')
            CALL WARN
            GO TO 40
            ENDIF
C     CHECK IF OPERATION NUMBER FOUND
         OPNAM=' '
         LOCP=1
20       CALL FSERCH (NUMOP,OPNAM,LOCP,OLDP,MOLDP)
         IF (IBUG.EQ.1) WRITE (IODBUG,*) 'SEGID=',SEGID,
     *      ' SEGID2=',SEGID2,' NUMOP=',NUMOP,
     *      ' LOCP=',LOCP,' OPNAM=',OPNAM
         IF (LOCP.GT.0) THEN
            CALL UMEMOV (OLDP(LOCP+1),FFGAID2,2)
            IF (IBUG.EQ.1) WRITE (IODBUG,*) 'SEGID=',SEGID,
     *         ' SEGID2=',SEGID2,' LOCP=',LOCP,' FFGAID2=',FFGAID2
            IF (FFGAID2.EQ.FFGAID) THEN
               WRITE (IPR,30) SEGID,FFGAID,SEGID2
30    FORMAT ('0**WARNING** FFG AREA IDENTIFIER SPECIFIED FOR SEGMENT ',
     *   A,' (',A,') IS USED BY SEGMENT ',A,'.')
               CALL WARN
               IER=1
               ENDIF
            GO TO 20
            ENDIF
40       CONTINUE
C
C  RESET VALUES IN COMMON BLOCKS
      LTS=LTSO
      LP=LPO
      LC=LCO
      LT=LTO
      LD=LDO
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'EXIT ',RTNNAM
C
      RETURN
C
      END
