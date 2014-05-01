C MEMBER HGETP2
C  (from old member HCLGETPM)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/28/95.13:42:08 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HGETP2 (ISTAT)
C
C          ROUTINE:  HGETP2
C
C             VERSION:  1.0.0
C
C                DATE:  4-23-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C  THIS ROUTINE READS RECORD 2 OF FILE USERPARM
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ISTAT      I     O     1    STATUS INDICATOR
C                                        0=RECORD READ
C                                        OTHER=ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'uunits'
      INCLUDE 'hclcommon/huprm2'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hgetp2.f,v $
     . $',                                                             '
     .$Id: hgetp2.f,v 1.1 1995/09/17 18:42:21 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
C  READ RECORD
      NREC=2
      CALL UREADT (KUPARM,NREC,IFLSTA,ISTAT)
C
      IF (IHCLDB.GT.0) WRITE (IOGDB,*) 'IN HGETP2 - ISTAT=',ISTAT
C
      RETURN
C
      END
