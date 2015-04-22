	SUBROUTINE DM_SRCH ( iflno, type, nkey, keyloc, keyval,
     +			     irwcl, iret )
C************************************************************************
C* DM_SRCH								*
C*									*
C* This subroutine searches a DM file for rows or columns which		*
C* match the given input values.					*
C*									*
C* DM_SRCH  ( IFLNO, TYPE, NKEY, KEYLOC, KEYVAL, IRWCL, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	TYPE		CHAR*		Dimension type : ROW or COL	*
C*	NKEY		INTEGER		Number of keys to search	*
C*	KEYLOC (NKEY)	INTEGER		Key locations			*
C*	KEYVAL (NKEY)	INTEGER		Key values			*
C*									*
C* Output parameters:							*
C*	IRWCL		INTEGER		Search location			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file is not open		*
C*					-17 = search criteria not met	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* J. Whistler/NSSFC	 3/95	Changed the search to be more efficient	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
	INCLUDE		'dbcmn.cmn'
C
	CHARACTER*(*)	type
	INTEGER		keyloc (*), keyval (*)
	LOGICAL		done
        INTEGER         intdtf (3), level (2)
        CHARACTER       dttim*20, qtype*8,garea*30,stinfo*25,
     +                  astnstr(4)*10, gdattm (2)*22, parm*14,
     +                  vcord*5, src*21, datauri*254, amodel*30,
     +                  anevent*30
c        CHARACTER       dbstn*4
        CHARACTER       message*720, funcnm*8, logdebug*6, logerror*6,
     +                  loginfo*6
C------------------------------------------------------------------------
        loginfo  = "info"
        logdebug = "debug"
        logerror = "error"
        funcnm="DM_SRCH"
        CALL ST_NULL ( funcnm,   funcnm,   lenq, ier )
        CALL ST_NULL ( loginfo, loginfo, lenq, ier )
        CALL ST_NULL ( logdebug, logdebug, lenq, ier )
        CALL ST_NULL ( logerror, logerror, lenq, ier )
	irwcl = 0
c        print *, "======= DM_SRCH - IN"
c        print *, "type=", type 
c        print *, "nkey=", nkey 
c        print *, "keyloc=",(keyloc(iii), iii=1, nkey)
c        print *, "keyval=",(keyval(iii), iii=1, nkey)
        IF ( dbread ) THEN
           qtype = " "
c           print *, "faking for db in dm_srch for ", dbdatasrc
           message = "faking for db in dm_srch for " // dbdatasrc
           CALL ST_NULL ( message,  message,  lenq, ier )
           CALL DB_MSGCAVE ( funcnm, logdebug, message, ier )
           irwcl = 1
           iret = 0
           IF ( type .eq. 'ROW' ) THEN
              CALL TI_CDTM ( keyval(1), keyval(2), dttim, ier)
              IF ( ier .eq. 0 ) dbdttm = dttim
c              print *, "set dbdttm=", dbdttm
              message = "set dbdttm=" // dbdttm
              CALL ST_NULL ( message,  message,  lenq, ier )
              CALL DB_MSGCAVE ( funcnm, logdebug, message, ier )
            ELSE IF ( type .eq. 'COL' ) THEN
              IF ( INDEX(dbdatasrc,'grid')  .gt. 0 ) THEN
                 qtype = "dataURI"
c                 print *, "need to search for the keyvals"
C
C*               Change the two integer time into a three integer 
C*               time and then into a character GEMPAK grid time.
C
                 CALL TG_FTOI  ( keyval, intdtf, ier )
                 CALL TG_ITOC  ( intdtf, gdattm (1), ier )
C
C*               Change the second GEMPAK time.
C
                 CALL TG_FTOI  ( keyval (3), intdtf, ier )
                 CALL TG_ITOC  ( intdtf, gdattm (2), ier )
C
C*               Move the levels and the vertical coordinate.
C
c                 print *, "checking the missing keylocs and keyvals"
                 IF ( nkey .ne. 10 ) THEN
                    ilevel = 0
                    level(1) = 0
                    level(2) = -1
                    ivcord = 0
                    iparm = 5
                 ELSE
                    level (1) = keyval (5)
                    level (2) = keyval (6)
                    ivcord    = keyval (7)
                    ilevel = level(1)
                    iparm = 8
                 END IF

C
C*               Change the last three integers into the parameter name.
C
                 CALL ST_ITOS  ( keyval (iparm), 3, nchar, parm, ier )
                 CALL LV_CCRD  ( ivcord, vcord, ier )
                 CALL DB_GETEVTNAME ( anevent, ier )
                 IF ( ier .ne. 0 ) THEN
c                    iret = -17
c                    print *,"after DB_GETEVTNAME event name not set"
                    message = "after DB_GETEVTNAME event name not set"
                    CALL DB_MSGCAVE ( funcnm, loginfo, message, ier )
c                    RETURN
                 END IF
                 
                 WRITE (message, 1001 ) ilevel 
 1001            FORMAT ("calling DB_GETDURI with LEVEL:",I6)
                 CALL ST_LSTR ( message, lmess, iret )
                 message=message(:lmess)
     +              // " GDATTM 1:" // gdattm(1) // "2:" // gdattm(2)
     +              // " VCORD:"  // vcord
     +              // " PARM:"   // parm
     +              // " qtype:"  // qtype
     +              // " src:"    // src
                  CALL ST_NULL ( message,  message,  lenq, ier )
                  CALL DB_MSGCAVE ( funcnm, logdebug, message, ier )
c                 print *, "calling DB_GETDURI with:"
c                 print *, "GDATTM: ", gdattm
c                 print *, "LEVEL:  ", ilevel
c                 print *, "VCORD:  ", vcord
c                 print *, "PARM:   ", parm
c                 print *, "qtype:  ", qtype
c                 print *, "src  :  ", src
                 CALL ST_LCUC ( dbdatasrc, src, ier )
                 CALL ST_NULL ( qtype,  qtype,  lstr, ier ) 
                 CALL ST_NULL ( src,    src,    lstr, ier )
                 CALL ST_NULL ( gdattm, gdattm, lstr, ier )
                 CALL ST_NULL ( vcord,  vcord,  lstr, ier )
                 CALL ST_NULL ( parm,   parm,   lstr, ier )
                 CALL ST_NULL ( dbmodel, amodel,   lstr, ier )
c                 CALL ST_NULL ( evtname, evtname,   lstr, ier )
                 CALL ST_NULL ( anevent, anevent,   lstr, ier )
                 CALL DB_GETDURI ( qtype, src, amodel, gdattm,
     +                             vcord, parm, anevent, ilevel, 
     +                             level(2), datauri, ldt, ier)
                 IF ( ier .ne. 0 ) THEN 
c                    print *,"after DB_GETDURI dburi not set"
                    message = "after DB_GETDURI datauri not set"
                    CALL DB_MSGCAVE ( funcnm, loginfo, message, ier )
c                    IF ( isensnames ) THEN 
c                       CALL DB_ENSM1 ( ier) 
c                    END IF
                    iret = -17
                    RETURN
                 END IF
                 CALL ST_RNUL ( datauri, dburi, ldt, ier)
c                 print *,"after DB_GETDURI set dburi to:",dburi(:ldt)
                 message = "after DB_GETDURI datauri set to" // 
     +                     dburi(:ldt)
                 CALL DB_MSGCAVE ( funcnm, logdebug, message, ier )
                 iret = 0
                 RETURN
              END IF
              IF ( INDEX(dbdatasrc,'metar')  .gt. 0 ) qtype = "stidqry"
              IF ( INDEX(dbdatasrc,'bufrua') .gt. 0 ) qtype = "stnmqry"
              IF ( INDEX(dbdatasrc,'synop')  .gt. 0 ) qtype = "stnmqry"
              
c              print *, "DM_SRCH qtype=", qtype
              IF ( qtype .eq. " " ) RETURN
              CALL ST_NULL ( qtype, qtype, lqtype, ier ) 
              CALL DB_GETGAREA (nkey, type, keyval, keyval, garea, ier)
c              print *, "before DB_GETSTINFO garea=", garea
              CALL ST_NULL ( garea, garea, lgarea, ier)
              CALL DB_GETSTINFO (qtype,garea,stinfo,lstinfo,ier)
              IF ( ier .ne. 0 ) THEN
                 iret = -14
                 RETURN
              END IF
c              print *, "after DB_GETSTINFO stinfo=", stinfo(:lstinfo)
c              print *, "after DB_GETSTINFO stinfo length=", lstinfo
              CALL ST_CLST ( stinfo(:lstinfo), ';', ' ', 4,
     +                       astnstr, iparts, iret)
c               print *, "after ST_CLST:"
c               do iii =1,4
c                 print *, astnstr(iii)
c               enddo
               stnindx = 1
               dbstid = astnstr(1)
c               print *, "            id:", dbstid
               CALL ST_NUMB ( astnstr(2), dbstlt, ier)
c               print *, "           lat:", dbstlt
               CALL ST_NUMB ( astnstr(3), dbstln, ier)
c               print *, "           lon:", dbstln
               CALL ST_NUMB ( astnstr(4), dbstel, ier)
c               print *, "          elev:", dbstel
c              CALL ST_ITOS (keyval, nkey, ncar, dbstn, ier)
c              CALL ST_LSTR ( dbstn, ldbstr, ier )
c              IF ( ldbstr .le. 3 ) THEN
c                  dbstid="K"//dbstn(:ldbstr)
c              ELSE 
c                  dbstid=dbstn(:ldbstr)
c              END IF
c              print *, "dbstid=", dbstid

           END IF
c        print *, "======= DM_SRCH - OUT"
           RETURN
        END IF
C
C*	Check that the file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Find headers to search.
C
	IF  ( type .eq. 'ROW' )  THEN
	    istart = 1
	    istop  = klstrw ( iflno )
	  ELSE IF  ( type .eq. 'COL' )  THEN
	    istart = krow ( iflno ) + 1
	    istop  = istart + klstcl ( iflno ) - 1
	  ELSE
	    iret = -17
	    RETURN
	END IF
C
C*	Loop through all headers looking for match.
C
	done  = .false.
	i     = istart
	DO WHILE (( .not. done ) .and. ( i .le. istop ) )
	    done = .true.
	    j = 1
	    DO WHILE (  ( j .le. nkey ) .and. ( done ) ) 
		IF  ( kheadr ( keyloc (j), i, iflno ) .ne. keyval (j) ) 
     +						done = .false.
		j = j + 1
	    END DO
	    IF  ( done ) irwcl = i
	    i = i + 1
	END DO
C
C*	Correct location when using columns.
C
	IF  ( ( irwcl .ne. 0 ) .and. ( type .eq. 'COL' ) )
     +			irwcl = irwcl - krow ( iflno )
	IF (irwcl .eq. 0) iret = -17
C*
c        print *, "======= DM_SRCH - OUT"
	RETURN
	END
