C MODULE PDTRRS
C***********************************************************************
C    FILL COMMON BLOCKS WITH THE RRS DATA TYPES AND ATTRIBUTES.
C***********************************************************************
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ISTAT      I     O     1    STATUS CODE
C***********************************************************************
      SUBROUTINE PDTRRS (ISTAT)
C
      INCLUDE 'pdbcommon/pdtrrx'
C
      CHARACTER*4  UNTOUT(1),DISTRB(1),XNONE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdtrrs.f,v $
     . $',                                                             '
     .$Id: pdtrrs.f,v 1.2 1999/07/06 15:05:17 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA    XNONE / 'NONE' /
C
        ISTAT = 0
C
C  INITIALIZE VARIABLES TO BE FILLED
        NVALPO(1) = 0
        IUMISS(1) = 0
        IRSUNT(1) = 0
        LFIELD(1) = 0
        NUMDEC(1) = 0
        CHKMAX(1) = 0.0
        CHKMIN(1) = 0.0
C
C  INITIALIZE VARIABLES NOT TO BE FILLED
        ICLASS = -99
        UNTOUT(1) = XNONE
        DISTRB(1) = XNONE
C
C  FILL COMMON BLOCKS
        CALL UDTRRS (MTYPE,NTYPE,IRTYPE,ICLASS,NVALPO,IUMISS,
     *     IRSUNT,UNTOUT,DISTRB,LFIELD,NUMDEC,CHKMIN,CHKMAX,ISTAT)
C
      RETURN
      END
