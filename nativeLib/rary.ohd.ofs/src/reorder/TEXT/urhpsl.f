C MEMBER URHPSL
C  (from old member URHCKP)
C***********************************************************************
C                                                                      *
C         MEMBER URHPSL
C                                                                      *
C***********************************************************************
C
       SUBROUTINE URHPSL( IPASS , ISTCOD )
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  URHPSL
C                                                                      *
C    7/18/86   NEW MEMBER FROM TASK 282
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  06/20/82                                       *
C                                                                      *
C              AUTHOR: JONATHAN D. GERSHAN
C                      DATA SCIENCES INC                              *
C                      8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C      THIS ROUTINE CHECKS THE VALIDITY OF A 4 LETTER LOCAL PASSWORD   *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'hclcommon/hcuuno'
      INCLUDE 'udsi'
      INCLUDE 'ufreei'
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
C                                                                      *
C                                                                      *
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
C                                                                      *
C                                                                      *
C                                                                      *
C***********************************************************************
C
C
C ********** THIS ROUTINE CHECKS A PASSWORD FOR THE COMPRESS ROUTINES
C ********** A STATUS CODE IS RETURNED  AS FOLLOWS:
C ********** 0 = OK , 1 = NO PASSWORD , 2 = BAD PASSWORD
C
C
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hcntrl'
C
       INTEGER JBUF(16)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urhpsl.f,v $
     . $',                                                             '
     .$Id: urhpsl.f,v 1.1 1995/09/17 19:17:51 dws Exp $
     . $' /
C    ===================================================================
C
C
       ISTCOD=0
       CALL UREADT(KLOFUI,1,JBUF,ISTAT)
       IF(IPASS.EQ.JBUF(5)) GO TO 999
       ISTCOD = 2
C
C
999     RETURN
        END
