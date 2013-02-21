        SUBROUTINE DB_INIT  ( iret )
C************************************************************************
C* DB_INIT                                                              *
C*                                                                      *
C* This subroutine dumps common.                             		*
C*                                                                      *
C* DB_INIT  ( IRET )  	                                         	*
C*                                                                      *
C* Input parameters:                                                    *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/CWS	07/09                                           *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'dbcmn.cmn'
C*
C------------------------------------------------------------------------
C*
        iensmem = 0
        igdtim    = 1
        dimx      = 0
        dimy      = 0
        dbread    = .false.
        firstdb   = .false.
        gridtmdb  = .false.
        isnavtime = .false.
        isevtname = .false.
        isensnames = .false.
        dburi     = ''
        dbtime    = ''
        navtime   = ''
        dbdttm    = ''
        dbmodel   = ''
        dbstid    = ''
        dbdatasrc = ''
        dbparms   = ''
        dbprmfile = ''
        evtname   = ''
        RETURN
        END
