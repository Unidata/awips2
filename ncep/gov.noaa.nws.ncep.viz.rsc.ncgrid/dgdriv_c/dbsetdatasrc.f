        SUBROUTINE DB_SETDATASRC  ( afile, iftype, ifsrc, iret )
C************************************************************************
C* DB_SETDATASRC                                                        *
C*                                                                      *
C* This subroutine puts DB info into common.                            *
C*                                                                      *
C* DB_SETDATASRC  ( AFILE, IFTYPE, IFSRC, IRET )                        *
C*                                                                      *
C* Input parameters:                                                    *
C*      AFILE           CHAR*           a file                          *
C*                                                                      *
C* Output parameters:                                                   *
C*      IFTYPE          INTEGER         File type                       *
C*      IFSRCE          INTEGER         File source                     *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                       -1 = not a A2DB access file    *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/CWS  05/11                                           *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'dbcmn.cmn'
C*
        CHARACTER*(*)   afile
        CHARACTER       carr1(25)*50, carr2(6)*25,ens*120
        CHARACTER       message*720, funcnm*20, loglevel*6
C*
C------------------------------------------------------------------------
C*
        loglevel = "debug"
        funcnm="DB_SETDATASRC"
        CALL ST_NULL ( funcnm,   funcnm,   lenq, ier )
        CALL ST_NULL ( loglevel, loglevel, lenq, ier )
        dbdatasrc = ' '
        dbparms   = ' '
        dbprmfile = ' '
c        print *, "DB_SETDATASRC dbread=", dbread
        IF ( dbread ) THEN 
           IF ( INDEX(afile,'metar') .gt. 0 )  THEN
                iftype    = MFSF
                ifsrc    = 102
                dbdatasrc = "metar"
                dbprmfile = 'metar.pack'
             ELSE IF ( INDEX(afile,'bufrua') .gt. 0 ) THEN
                iftype = MFSN
                ifsrc = 102
                dbdatasrc = "bufrua"
                dbprmfile = 'snmerg.pack'
             ELSE IF ( INDEX(afile,'GRID') .gt. 0 ) THEN
                iftype = MFGD
                ifsrc = 6
                dbdatasrc = "grid"
                dbparms   = 'ncgrib' 
C
C*              Get the grid name from the file name
C
                CALL ST_CLST ( afile, '/', ' ', 25, carr1,
     +                         num, ier )
                IF ( ier .eq. 0 ) THEN
c                   CALL ST_RPST ( carr1(num), 'db','db',
c     +                                ipos, carr1(num), ier)
c                   IF ( ier .ne. 0 ) THEN
c                       iret = -1
c                       RETURN
c                   ELSE
c                       dbmodel= carr1(num)(:ipos-2)
c                   END IF

                   CALL ST_CLST ( carr1(num), '_', ' ', 6, carr2,
     +                            num2, ier )
                   IF ( num2 .eq. 2 ) THEN
C
C*                    this is grid file
C

                      CALL ST_LSTR ( carr2(1), icarr2, ier )
                      dbmodel= carr2(1)(:icarr2)
                      message = "DB_SETDATASRC set dbmodel=" // dbmodel
                      CALL ST_NULL ( message,  message,  lenq, ier )
                      CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
                   ELSE IF ( num2 .eq. 4 ) THEN
C
C*                    this is ensemble file
C
                      CALL ST_LSTR ( carr2(1), icarr2, ier )
                      dbmodel= carr2(1)(:icarr2)
                      CALL ST_LSTR ( carr2(3), icarr2, ier )
                      CALL DB_SETENSMBRS ( carr2(3)(:icarr2), ier )
                      CALL ST_LSTR ( carr2(4), icarr2, ier )
                      CALL DB_SETNAVTIME ( carr2(4)(:icarr2),ier )
                      message = "DB_SETDATASRC set dbmodel=" // dbmodel
     +                       // " set ens member =" // carr2(3)(:icarr2)
     +                       // " set nav time=" // carr2(4)(:icarr2)
                      CALL ST_NULL ( message,  message,  lenq, ier )
                      CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
                    ELSE IF ( num2 .gt. 4 ) THEN
                        CALL ST_LSTR ( carr2(1), icarr2, ier )
                        dbmodel= carr2(1)(:icarr2)
                        CALL ST_LSTR ( carr2(3), icarr2, ier )
                        ens = carr2(3)(:icarr2)
                        
                        DO ii = 4 , num2 - 1
                            CALL ST_LSTR ( carr2(ii), icarr2, ier )
                            CALL ST_LSTR ( ens,lenq, ier )
                            ens = ens(1:lenq)//'_'//carr2(ii)(:icarr2)
                        ENDDO
                        CALL DB_SETENSMBRS ( ens, ier )
                      CALL ST_LSTR ( carr2(num2), icarr2, ier )
                      CALL DB_SETNAVTIME ( carr2(num2)(:icarr2),ier )
                      message = "DB_SETDATASRC set dbmodel=" // dbmodel
     +                       // " set ens member =" // ens
     +                       // " set nav time=" // carr2(num2)(:icarr2)
                      CALL ST_NULL ( message,  message,  lenq, ier )
                      CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )

                    ELSE  
C
C*                   TODO - improve the error handling
C
                      iret = -1
                      RETURN
                   END IF
                ELSE 
C
C*                 TODO - improve the error handling
C  
                   iret = -1
                   RETURN
                END IF
             ELSE 
               iret = -1
             END IF
         ELSE 
           iret = -1
        END IF
c        print *, "parms set in DB_SETDATASRC:", 
c     +            dbdatasrc, dbparms, dbprmfile
        RETURN
        END
