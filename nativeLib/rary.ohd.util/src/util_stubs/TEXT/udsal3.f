C MEMBER UDSAL3
C-----------------------------------------------------------------------
C
      SUBROUTINE UDSAL3 (NUNIT,DDNAME,DSNAME,TYPALC,ICKDDN,
     *   IPRERR,TYPMSG,IPRMSG,NBLINE,ISTAT)
C
C  ROUTINE TO ALLOCATE A DATASET TO A DDNAME OR UNALLOCATE A DATASET
C  FROM A DDNAME. THE DATASET MUST BE CATALOGED.
C
C     ARGUMENT   TYPE   I/O   DIM   CONTENTS
C     --------   ----   ---   ---   --------
C     NUNIT      I*4    I     1     UNIT NUMBER
C                                     0=DDNAME WILL BE USED
C                                    >0=DDNAME WILL BE CREATED
C                                       USING UNIT NUMBER
C     DDNAME     A8     I     1     DD NAME
C     DSNAME     A44    I     1     DATASET NAME
C     TYPALC     (*)    I     1     TYPE OF ALLOCATION
C                                     'ALLOCATE'=ALLOCATE DATASET
C                                     'UNALLOC'=UNALLOCATE DATASET
C     ICKDDN     I*4    I     1     OPTION TO CHECK IF DDNAME IS
C                                   ALLOCATED
C                                     0=NO
C                                    >0=YES
C     IPRERR     I*4    I     1     OPTION TO PRINT ERRORS
C                                     0=NO
C                                    >0=YES
C     TYPMSG     (*)    I     1     TYPE OF MESSAGE
C                                     'NOTE'=PRINT AS NOTE
C                                     'WARNING'=PRINT AS WARNING
C                                     'ERROR'=PRINT AS ERROR
C     IPRMSG     I*4    I     1     OPTION TO PRINT ALLOCATION MESSAGE
C                                     0=NO
C                                    >0=YES
C     NBLINE     I*4    I     1     NUMBER OF BLANK LINES TO PRINT
C                                   BEFORE ALLOCATION MESSAGE
C     ISTAT      I*4    I     1     STATUS CODE
C                                     0=NORMAL RETURN
C                                     1=INVALID UNIT NUMBER
C                                     2=DDNAME NOT ALLOCATED
C                                     3=INVALID ALLOCATION TYPE
C                                     4=UNSUCCESSFUL ALLOCATION
C
C
      CHARACTER*(*) TYPALC,TYPMSG
      CHARACTER*8 DDNAME
      CHARACTER*44 DSNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_stubs/RCS/udsal3.f,v $
     . $',                                                             '
     .$Id: udsal3.f,v 1.1 1995/09/17 19:04:32 dws Exp $
     . $' /
C    ===================================================================
C

        ISTAT = 0

      RETURN
      END
