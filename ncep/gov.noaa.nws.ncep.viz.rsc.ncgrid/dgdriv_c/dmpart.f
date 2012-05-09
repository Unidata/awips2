	SUBROUTINE DM_PART  ( iflno, prtnam, lenhdr, ityprt, nparms,
     +			      prmnam, iscale, ioffst, nbits, iret )
C************************************************************************
C* DM_PART								*
C*									*
C* This subroutine returns information for a specific part.		*
C*									*
C* DM_PART  ( IFLNO, PRTNAM, LENHDR, ITYPRT, NPARMS, PRMNAM, ISCALE,	*
C*            IOFFST, NBITS, IRET )					*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	PRTNAM		CHAR*4		Part name			*
C*									*
C* Output parameters:							*
C*	LENHDR		INTEGER		Length of data header		*
C*	ITYPRT		INTEGER		Data type			*
C*	NPARMS		INTEGER		Number of parameters		*
C*	PRMNAM (NPARMS)	CHAR*4		Parameter names			*
C*	ISCALE (NPARMS)	INTEGER		Scaling term			*
C*	IOFFST (NPARMS)	INTEGER		Offset				*
C*	NBITS  (NPARMS)	INTEGER		Number of bits			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					-10 = invalid part name		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
	INCLUDE		'dbcmn.cmn'
C
	CHARACTER*(*)	prtnam, prmnam (*)
	INTEGER		iscale (*), ioffst (*), nbits (*)
C
        CHARACTER*4     mandpp (6), sigtpp (3), sigwpp (3), troppp (5),
     +                  maxwpp (3)
        DATA            mandpp  / 'PRES', 'TEMP', 'DWPT',
     +                            'DRCT', 'SPED', 'HGHT' /
        DATA            sigtpp  / 'PRES', 'TEMP', 'DWPT' /
        DATA            sigwpp  / 'HGHT', 'DRCT', 'SPED' /
        DATA            troppp  / 'PRES', 'TEMP', 'DWPT',
     +                            'DRCT', 'SPED' /
        DATA            maxwpp  / 'PRES', 'DRCT', 'SPED' /
C-----------------------------------------------------------------------
C*	Check that the file is open.
C
        IF ( dbread ) THEN
C
C*         Get parameter list from packing file 
C
           IF ( INDEX(dbdatasrc,'metar') .gt. 0 .or.
     +          INDEX(dbdatasrc,'synop') .gt. 0 ) THEN 
              lenhdr = 1
              ityprt = 4
              CALL IN_PRMF  ( dbprmfile, nparms, prmnam, iscale,
     +                        ioffst, nbits, pkflg, ier )
              IF ( ier .eq. 0 ) THEN 
                 iret = 0
              ELSE 
                 iret = ier
                 RETURN
              END IF
           ELSE IF ( INDEX(dbdatasrc,'bufrua') .gt. 0 .and.
     +               prtnam .eq. 'SNDT' ) THEN 
              iret = -4
           ELSE IF ( INDEX(dbdatasrc,'bufrua') .gt. 0 .and. 
     +               prtnam .ne. 'SNDT' ) THEN 
              lenhdr = 1
              ityprt = 4
              IF ( prtnam .eq. 'TTCC' .or. prtnam .eq. 'TTAA' ) THEN
                 nparms = 6
                 DO iparm = 1, nparms
                    prmnam (iparm) = mandpp (iparm)
                 END DO
              ELSE IF (prtnam .eq. 'TRPC' .or. prtnam .eq. 'TRPA' ) THEN
                 nparms = 5
                 DO iparm = 1, nparms
                    prmnam (iparm) = troppp (iparm)
                 END DO
              ELSE IF (prtnam .eq. 'MXWC' .or. prtnam .eq. 'MXWA' ) THEN
                 nparms = 3
                 DO iparm = 1, nparms
                    prmnam (iparm) = maxwpp (iparm)
                 END DO
              ELSE IF (prtnam .eq. 'PPCC' .or. prtnam .eq. 'PPCA' ) THEN
                 DO  i = 1, 3
                    ii = i + 2
                    IF ( i . eq. 1 ) ii = i
                    prmnam (i) = mandpp (ii)
                 END DO
                 nparms = i
              ELSE IF (prtnam .eq. 'TTDD' .or. prtnam .eq. 'TTBB' ) THEN
                 nparms = 3
                 DO iparm = 1, nparms
                    prmnam (iparm) = sigtpp (iparm)
                 END DO
              ELSE IF (prtnam .eq. 'PPDD' .or. prtnam .eq. 'PPBB' ) THEN
                 nparms = 3
                 DO iparm = 1, nparms
                    prmnam (iparm) = sigwpp (iparm)
                 END DO
              END IF
              iret = 0
           ELSE IF ( INDEX(dbdatasrc,'grid') .gt. 0 ) THEN 
              lenhdr = 128
              ityprt = 5
              nparms = 1
              prmnam(1) = 'GRID'
              gridtmdb = .true.
              igdtim = 1
              DO ii = 1, 200
                 dbtimes(ii) = ''
              END DO
           END IF
           RETURN
        END IF
	CALL DM_CHKF  ( iflno, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Find this part name.
C
	knt = 0
	DO  i = 1, kprt ( iflno )
	    IF  ( kprtnm ( i, iflno ) .eq. prtnam )  knt = i
	END DO
C
C*	Return if part name was not found.
C
	IF  ( knt .eq. 0 )  THEN
	    iret = -10
	    RETURN
	END IF
C
C*	Retrieve part information from common area.
C
	lenhdr = klnhdr ( knt, iflno )
	ityprt = ktyprt ( knt, iflno )
	nparms = kparms ( knt, iflno )
	DO  i = 1, nparms
	    prmnam ( i ) = kprmnm ( i, knt, iflno )
	    iscale ( i ) = kscale ( i, knt, iflno )
	    ioffst ( i ) = koffst ( i, knt, iflno )
	    nbits  ( i ) = kbits  ( i, knt, iflno )
	END DO
C*
	RETURN
	END
