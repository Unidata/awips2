C*     L. Hinson/AWC Corrected type definition for sindex
	SUBROUTINE Sort_Mtbl(nstn,stid,thres)
	INTEGER nstn
	CHARACTER*(*) stid (*)
	INTEGER       thres (*)
	INTEGER	      sindex
	
	DO i=1,nstn-1
	  sindex=i
	  DO j=i+1,nstn
	    IF (stid(j) .gt. stid(sindex)) THEN
	      sindex=j
	    END IF
	  END DO
	  IF (i .ne. sindex) THEN
	    CALL Swap_Items(stid,thres,i,sindex)
	  END IF
	END DO
	RETURN
	END
	
	SUBROUTINE Swap_Items(stid,thres,i,j)
        CHARACTER*(*) stid (*)
        INTEGER       thres (*)
	INTEGER       i,j
	CHARACTER tstid*4
	INTEGER   tthres
	tstid=stid(i)
	stid(i)=stid(j)
	stid(j)=tstid
	tthres=thres(i)
	thres(i)=thres(j)
	thres(j)=tthres
	RETURN
	END
	
	  
