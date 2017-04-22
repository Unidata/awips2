C MEMBER UPNTRK
C-----------------------------------------------------------------------
C
      SUBROUTINE UPNTRK (IPUNIT,NDSTRK,IDSTRK,NABSTR)
C
C  ROUTINE UPNTRK OUTPUTS DATASET SPACE PARAMETER IN CARD IMAGE FORM.
C
      CHARACTER*4   XFMT
      CHARACTER*8   XCHAR
      CHARACTER*80  CARD
      INTEGER       LENX
C
      DIMENSION IDSTRK(NDSTRK)
C
      INCLUDE 'uiox'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/upntrk.f,v $
     . $',                                                             '
     .$Id: upntrk.f,v 1.2 1998/04/07 14:22:44 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA    LENX,XFMT,XCHAR,CARD / 8,'(I8)',' ',' ' /
C
      NSPACE=0
      NCHECK=0
      ITYPE=-1
C
C  OUTPUT AS '//     F??=??,'
      DO 10 IUNIT=1,NDSTRK
         IF (IDSTRK(IUNIT).EQ.0) GO TO 10
         NPOS=1
         CALL UTOCRD (IPUNIT,NPOS,'//     F0',9,NSPACE,CARD,ITYPE,
     *      NCHECK,LNUM,IERR)
         WRITE(XCHAR(1:LENX),XFMT) IUNIT
         CALL ULEFTC (XCHAR,LENX,LENTH)
         IF (LENTH.EQ.2) NPOS=NPOS-1
         CALL UTOCRD (IPUNIT,NPOS,XCHAR,LENX,NSPACE,CARD,ITYPE,
     *      NCHECK,LNUM,IERR)
         CALL UTOCRD (IPUNIT,NPOS,'=',1,NSPACE,CARD,ITYPE,NCHECK,LNUM,
     *      IERR)
         WRITE(XCHAR(1:LENX),XFMT) IDSTRK(IUNIT)
         CALL UTOCRD (IPUNIT,NPOS,XCHAR,LENX,NSPACE,CARD,ITYPE,
     *      NCHECK,LNUM,IERR)
         CALL UTOCRD (IPUNIT,NPOS,',',1,NSPACE,CARD,ITYPE,NCHECK,LNUM,
     *      IERR)
         CALL UPNCRD (IPUNIT,CARD)
10       CONTINUE
C
C  OUTPUT AS '//FS5FCRT.FT??F001  DD  SPACE=(ABSTR,(?,????))'
      DO 20 IUNIT=1,NDSTRK
         IF (IDSTRK(IUNIT).EQ.0) GO TO 20
         NPOS=1
         CALL UTOCRD (IPUNIT,NPOS,'//FS5FCRT.FT0',13,NSPACE,CARD,ITYPE,
     *      NCHECK,LNUM,IERR)
         WRITE(XCHAR(1:LENX),XFMT) IUNIT
         CALL ULEFTC (XCHAR,LENX,LENTH)
         IF (LENTH.EQ.2) NPOS=NPOS-1
         CALL UTOCRD (IPUNIT,NPOS,XCHAR,LENX,NSPACE,CARD,ITYPE,
     *      NCHECK,LNUM,IERR)
         CALL UTOCRD (IPUNIT,NPOS,'F001 DD SPACE=',14,NSPACE,CARD,ITYPE,
     *      NCHECK,LNUM,IERR)
         CALL UTOCRD (IPUNIT,NPOS,'(ABSTR,(',8,NSPACE,CARD,ITYPE,
     *      NCHECK,LNUM,IERR)
         WRITE(XCHAR(1:LENX),XFMT) IDSTRK(IUNIT)
         CALL UTOCRD (IPUNIT,NPOS,XCHAR,LENX,NSPACE,CARD,ITYPE,
     *      NCHECK,LNUM,IERR)
         CALL UTOCRD (IPUNIT,NPOS,',',1,NSPACE,CARD,ITYPE,NCHECK,LNUM,
     *      IERR)
         WRITE(XCHAR(1:LENX),XFMT) NABSTR
         CALL UTOCRD (IPUNIT,NPOS,XCHAR,LENX,NSPACE,CARD,ITYPE,
     *      NCHECK,LNUM,IERR)
         CALL UTOCRD (IPUNIT,NPOS,'))',2,NSPACE,CARD,ITYPE,NCHECK,LNUM,
     *      IERR)
         NABSTR=NABSTR+IDSTRK(IUNIT)
         CALL UPNCRD (IPUNIT,CARD)
20       CONTINUE
C
      RETURN
C
      END
