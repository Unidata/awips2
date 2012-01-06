/*
File:		rating_util.c
Date:		December 1999 
Author:		Russell Erb and Sung Vo

Purpose:	
*/

#include <stdio.h>

#include "BinarySearch.h"
#include "get_limits.h"
#include "RatingShift.h"
#include "rating_util.h"

/*************************************************************************/

float stage2discharge(char *lid, float stage)
{

 static char	prev_lid[9]="\0";
	char	where[BUFSIZ];
 static double  shift_amount = 0 ;
	float   discharge=0.0,
		diff_discharge=0.0,
		diff_stage=0.0;
	int	count, ctr, need_to_find_shift_amount = 0 ;
 static	Rating	*rateHead=NULL;
	Rating	*ratePtrLower=NULL, *ratePtrHigher=NULL, *rateTail=NULL;
        RatingShift * ratingShiftHead = NULL ;
        RatingShift * ratingShiftNode = NULL ;

        /* Check to determine if the stage value is missing. 
           If it is then return a flow value of missing. */
        if ( stage == MISSING_VAL )
        {
           return RATING_CONVERT_FAILED ;
        }

	/*
	 if the lid passed in is NOT the same as the previous lid
	 then copy lid passed in to previous
	 check if the previous head is NOT NULL then free that
	 memory and set it to NULL
	 then create a where clause and get the rating curve for this lid
	*/
	if (strcmp(lid, prev_lid) != 0)
	{
	   strcpy(prev_lid, lid);

	   if (rateHead != NULL)
	   {
	      FreeRating (rateHead);
	      rateHead=NULL;
	   }

	   sprintf ( where, " WHERE lid = '%s' AND discharge IS NOT"
                            " NULL ORDER BY stage ASC ", lid);
	   rateHead = GetRating(where);

		if ( rateHead == NULL )
			printf("No Rating table available for LID=%s\n", lid);

       need_to_find_shift_amount = 1 ;
	}

	/*
	 if the pointer to the head is NULL then that means there
	 is NO rating curve for that location
	*/	
	if ( rateHead == NULL )
		return RATING_CONVERT_FAILED ;

	/*
	 if there is less than 2 points (ie. one) then that means there
	 is NO usable rating curve for that location
	*/
	count = ListCount(&rateHead->list);
	if (count < 2)
	{
		printf("Rating table has less than 2 points for LID=%s\n", lid);
		return RATING_CONVERT_FAILED ;
	}

        /*
         Determine if there is a shift factor for this rating curve.
         If there is, then shift each point in the rating curve by this
         factor. */
        if ( need_to_find_shift_amount == 1 )
        {
	   sprintf ( where, " WHERE lid = '%s' "
                            " AND active = 'T' "
                            " ORDER BY date DESC " , lid ) ;
           ratingShiftHead = GetRatingShift ( where ) ;

           if ( ratingShiftHead != NULL )
           {
              ratingShiftNode = ( RatingShift * ) 
                                ListFirst ( & ratingShiftHead->list ) ;
              shift_amount = ratingShiftNode->shift_amount ; 
              FreeRatingShift ( ratingShiftHead ) ; 
              ratingShiftHead = NULL ;
	      ratePtrHigher =  rateHead ;

              while ( ratePtrHigher != NULL )
              {
                 ratePtrHigher->stage += shift_amount ;    
	         ratePtrHigher = ( Rating * )
                                  ListNext ( &ratePtrHigher->node ) ;
              }
           }

        }
	
	/*
	 calculate the last point in the linked list
	*/
	rateTail = (Rating *) ListLast(&rateHead->list);
	
	/*
	 if the stage value passed in is less then the lowest stage in the
	 rating table then extrapolate the discharge
	*/
	if (stage < rateHead->stage)
	{
		ratePtrHigher = (Rating *) ListNext(&rateHead->node);

		diff_discharge = ratePtrHigher->discharge - rateHead->discharge;
		diff_stage = ratePtrHigher->stage - rateHead->stage;
		if (diff_stage == 0)
		   discharge = rateHead->discharge;
		else
		   discharge = rateHead->discharge - 
                               ((diff_discharge/diff_stage) * 
                               (rateHead->stage - stage));
	}
	
	/*
	 if the stage value passed in is greater then the highest stage in the
	 rating table then extrapolate the discharge
	*/
	if (stage > rateTail->stage)
	{
		ratePtrLower = (Rating *) ListPrev(&rateTail->node);

		diff_discharge = rateTail->discharge - ratePtrLower->discharge;
		diff_stage = rateTail->stage - ratePtrLower->stage;

		if (diff_stage == 0)
		   discharge = rateTail->discharge;
		else
		   discharge = rateTail->discharge + 
                               ((diff_discharge/diff_stage) * 
                               (stage - rateTail->stage));
	}

	/*
	 if the stage value passed in is between the lowest and highest stage
	 in the rating table then interpolate the discharge
	*/
	if ((stage >= rateHead->stage) && (stage <= rateTail->stage))
	{
		ratePtrLower = (Rating *) ListFirst(&rateHead->list);

		count = ListCount(&rateHead->list);
		for (ctr=0; ctr < count-1; ctr++)
		{
			ratePtrHigher = (Rating *) ListNext(&ratePtrLower->node);
			if (stage >= ratePtrLower->stage && 
                            stage <= ratePtrHigher->stage)
			{
				diff_discharge = ratePtrHigher->discharge - 
                                                 ratePtrLower->discharge;
				diff_stage = ratePtrHigher->stage - 
                                             ratePtrLower->stage;

				if (diff_stage == 0)
				   discharge = ratePtrLower->discharge;
				else
				   discharge = ratePtrLower->discharge + 
                                               ((diff_discharge/diff_stage) * 
                                               (stage - ratePtrLower->stage));

				break;
			}
			ratePtrLower = (Rating *) ListNext(&ratePtrLower->node);
		}
	}

	/*
	 for some reason the discharge is less than zero then return missing. 
	*/
	if (discharge < 0.0)
	   return RATING_CONVERT_FAILED ;
	else
	   return discharge ;
}

/*************************************************************************/

float discharge2stage(char *lid, float discharge)
{
 static char	prev_lid[9]="\0";
 static double  shift_amount = 0 ;
 static	Rating	*rateHead=NULL;
	Rating	*ratePtrLower=NULL, *ratePtrHigher=NULL, *rateTail=NULL;
        RatingShift * ratingShiftHead = NULL ;
        RatingShift * ratingShiftNode = NULL ;
	char	where[BUFSIZ];
	int	count, ctr, need_to_find_shift_amount = 0 ;
	float   stage=0.0,
		diff_discharge=0.0,
		diff_stage=0.0;

        /* Check to see if the discharge value is bad, i.e. missing.
           If it is bad, then return a stage value of missing. */
        if ( discharge < 0 )
        {
           return RATING_CONVERT_FAILED ;
        }

	/*
	 if the lid passed in is NOT the same as the previous lid
	 then copy lid passed in to previous
	 check if the previous head is NOT NULL then free that
	 memory and set it to NULL
	 then create a where clause and get the rating curve for this lid
	*/
	if (strcmp(lid, prev_lid) != 0)
	{
	   strcpy(prev_lid, lid);
	   if (rateHead != NULL)
	   {
	      FreeRating (rateHead);
	      rateHead=NULL;
	   }
	   sprintf ( where, " WHERE lid = '%s' AND discharge IS NOT NULL"
                            " ORDER BY discharge ASC ", lid);
	   rateHead = GetRating(where);

		if ( rateHead == NULL )
			printf("No Rating table available for LID=%s\n", lid);

        need_to_find_shift_amount = 1 ;
	}

	/*
	 if the pointer to the head is NULL then that means there
	 is NO rating curve for that location
	*/	
	if ( rateHead == NULL )
		return RATING_CONVERT_FAILED ;

	/*
	 if there is less than 2 points (ie. one) then that means there
	 is NO usable rating curve for that location
	*/
	count = ListCount(&rateHead->list);

	if (count < 2)
	{
		printf("Rating table has less than 2 points for LID=%s\n", lid);
		return RATING_CONVERT_FAILED ;
	}

        /*
         Determine if there is a shift factor for this rating curve.
         If there is, then shift each point in the rating curve by this
         factor. */
        if ( need_to_find_shift_amount == 1 )
        {
	   sprintf ( where, " WHERE lid = '%s' "
                            " AND active = 'T' "
                            " ORDER BY date DESC " , lid ) ;
           ratingShiftHead = GetRatingShift ( where ) ;

           if ( ratingShiftHead != NULL )
           {
              ratingShiftNode = ( RatingShift * ) 
                                ListFirst ( & ratingShiftHead->list ) ;
              shift_amount = ratingShiftNode->shift_amount ; 
              FreeRatingShift ( ratingShiftHead ) ; 
              ratingShiftHead = NULL ;
           }
        }
	
	/*
	 calculate the last point in the linked list
	*/
	rateTail = (Rating *) ListLast(&rateHead->list);
	
	/*
	 if the discharge value passed in is less then the lowest discharge in
	 the rating table then extrapolate the stage
	*/
	if (discharge < rateHead->discharge)
	{
		ratePtrHigher = (Rating *) ListNext(&rateHead->node);

		diff_discharge = ratePtrHigher->discharge - rateHead->discharge;
		diff_stage = ratePtrHigher->stage - rateHead->stage;
		if (diff_discharge == 0)
		   stage = rateHead->stage;
		else
		   stage = rateHead->stage - ((diff_stage/diff_discharge) * 
                           (rateHead->discharge - discharge));
	}
	
	/*
	 if the discharge value passed in is greater then the highest discharge
	 in the rating table then extrapolate the stage
	*/
	if (discharge > rateTail->discharge)
	{
		ratePtrLower = (Rating *) ListPrev(&rateTail->node);

		diff_discharge = rateTail->discharge - ratePtrLower->discharge;
		diff_stage = rateTail->stage - ratePtrLower->stage;

		if (diff_discharge == 0)
		   stage = rateTail->stage;
		else
		   stage = rateTail->stage + 
                           ((diff_stage/diff_discharge) * 
                           (discharge - rateTail->discharge));
	}

	/*
	 if the discharge value passed in is between the lowest and highest 
         discharge in the rating table then interpolate the stage
	*/
	if ((discharge >= rateHead->discharge) && 
            (discharge <= rateTail->discharge))
	{
		ratePtrLower = (Rating *) ListFirst(&rateHead->list);

		count = ListCount(&rateHead->list);
		for (ctr=0; ctr < count-1; ctr++)
		{
			ratePtrHigher = (Rating *) ListNext(&ratePtrLower->node);
			if (discharge >= ratePtrLower->discharge && 
                            discharge <= ratePtrHigher->discharge)
			{
				diff_discharge = ratePtrHigher->discharge - 
                                                 ratePtrLower->discharge;
				diff_stage = ratePtrHigher->stage - 
                                             ratePtrLower->stage;

				if (diff_discharge == 0)
				   stage = ratePtrLower->stage;
				else
				   stage = ratePtrLower->stage + 
                                           ((diff_stage/diff_discharge) * 
                                         (discharge - ratePtrLower->discharge));

				break;
			}
			ratePtrLower = (Rating *) ListNext(&ratePtrLower->node);
		}
	}

        stage += ( float ) shift_amount ;
	return stage ;

}

/*************************************************************************/

void setRejectedDataObs(Observation *obs, RejectedData *rejectedData,
                        float old_value)
{
     char       *userid=NULL;
     time_t     currentTime = 0;
     dtime_t	postingTime;
     
     extern char *getlogin( void );


     strcpy(rejectedData->lid, obs->lid);

     strcpy(rejectedData->pe, obs->pe);

     rejectedData->dur = obs->dur;

     strcpy(rejectedData->ts, obs->ts);

     strcpy(rejectedData->extremum, obs->extremum);

     /* set probability to -1 for observed data */
     rejectedData->probability = -1.0;

     /* set validtime and basistime to obstime for observed data */
     rejectedData->validtime = obs->obstime;
     rejectedData->basistime = obs->obstime;

     /* set postingtime to current time */
     time(&currentTime);
     timet_to_yearsec_dt(currentTime, &postingTime);
     rejectedData->postingtime = postingTime;

     rejectedData->value = old_value;

     rejectedData->revision = obs->revision;

     strcpy(rejectedData->shef_qual_code, obs->shef_qual_code);

     strcpy(rejectedData->product_id, obs->product_id);

     rejectedData->producttime = obs->producttime;

     rejectedData->quality_code = obs->quality_code;

     /* set reject_type to M for Manual */
     strcpy(rejectedData->reject_type, "M");

     /* copy userid to rejectedData structure */
     if ((userid = getlogin()) != NULL)
          strcpy(rejectedData->userid, userid);
     else
          strcpy(rejectedData->userid, "");

     return;
}

/*************************************************************************/
void setRejectedDataFcst(Forecast *fcst, RejectedData *rejectedData,
                         float old_value)
{
     char       *userid=NULL;
     time_t     currentTime = 0;
     dtime_t	postingTime;
     
     extern char *getlogin( void );


     strcpy(rejectedData->lid, fcst->lid);

     strcpy(rejectedData->pe, fcst->pe);

     rejectedData->dur = fcst->dur;

     strcpy(rejectedData->ts, fcst->ts);

     strcpy(rejectedData->extremum, fcst->extremum);

     rejectedData->probability = fcst->probability;

     rejectedData->validtime = fcst->validtime;
     
     rejectedData->basistime = fcst->basistime;

     /* set postingtime to current time */
     time(&currentTime);
     timet_to_yearsec_dt(currentTime, &postingTime);
     rejectedData->postingtime = postingTime;

     rejectedData->value = old_value;

     rejectedData->revision = fcst->revision;

     strcpy(rejectedData->shef_qual_code, fcst->shef_qual_code);

     strcpy(rejectedData->product_id, fcst->product_id);

     rejectedData->producttime = fcst->producttime;

     rejectedData->quality_code = fcst->quality_code;

     /* set reject_type to M for Manual */
     strcpy(rejectedData->reject_type, "M");

     /* copy userid to rejectedData structure */
     if ((userid = getlogin()) != NULL)
          strcpy(rejectedData->userid, userid);
     else
          strcpy(rejectedData->userid, "");

     return;
}

/*************************************************************************/

void	createUpdDelWhereObs(char *where, Observation *obsPtr)
{
     
     char obstimeString[ANSI_TIME_LEN];
     
     
     yearsec_dt_to_ansi(obsPtr->obstime, obstimeString);
     
     /*
     Used for updating and deleting.
     */	
     sprintf(where, " where lid = '%s' and pe = '%s' "
	     "and obstime = '%s' "
	     "and dur = %d and ts = '%s'  and extremum = '%s' ",
	     obsPtr->lid, obsPtr->pe,
	     obstimeString,
	     obsPtr->dur, obsPtr->ts, obsPtr->extremum);

     return;  
}   

/*************************************************************************/

void	createUpdDelWhereFcst(char *fcst_where, Forecast *fcstPtr)
{
     
     char validtimeString[ANSI_TIME_LEN];
     char basistimeString[ANSI_TIME_LEN];
     
     
     yearsec_dt_to_ansi(fcstPtr->validtime, validtimeString);
     yearsec_dt_to_ansi(fcstPtr->basistime, basistimeString);
     
     /*
     Used for updating and deleting.
     */	
     sprintf(fcst_where, " where lid = '%s' and pe = '%s' "
	     "and validtime = '%s' and basistime = '%s' "
	     "and dur = %d and ts = '%s'  and extremum = '%s' ",
	     fcstPtr->lid, fcstPtr->pe,
	     validtimeString, basistimeString,
	     fcstPtr->dur, fcstPtr->ts, fcstPtr->extremum);

     return;  
}   

/*************************************************************************/

float stage2discharge_buff(char *lid, float stage)
{

 static char	prev_lid[9]="\0";
	float   discharge=0.0,
		diff_discharge=0.0,
		diff_stage=0.0;
	int	count , ctr ;
 static	Rating	*rateHead=NULL;
	Rating	*ratePtrLower=NULL, *ratePtrHigher=NULL, *rateTail=NULL;

       /* Check to determine if the stage value passed into this routine
          is missing. If it is, then return a flow value of missing. */
       if ( stage == MISSING_VAL )
       {
          return RATING_CONVERT_FAILED ;
       }
 
       /*
         if the lid passed in is NOT the same as the previous lid
         then copy lid passed in to previous
         check if the previous head is NOT NULL then free that
         memory and set it to NULL
         then create a where clause and get the rating curve for this lid
        */
        if (strcmp(lid, prev_lid) != 0)
        {
           strcpy(prev_lid, lid);

           rateHead = load_rating_curve ( lid ) ;

			if ( rateHead == NULL )
				printf("No Rating table available for LID=%s\n", lid);
        }

        /* If the pointer to the rating curve is NULL,
           it means that there is no rating curve information
           for that location. */
 
	if ( rateHead == NULL )
		return RATING_CONVERT_FAILED ;

	/*
	 if there is less than 2 points (ie. one) then that means there
	 is NO usable rating curve for that location
	*/
	count = ListCount(&rateHead->list);
	if (count < 2)
	{
		printf("Rating table has less than 2 points for LID=%s\n", lid);
		return RATING_CONVERT_FAILED ;
	}

	/*
	 calculate the last point in the linked list
	*/
	rateTail = (Rating *) ListLast(&rateHead->list);
	
	/*
	 if the stage value passed in is less then the lowest stage in the
	 rating table then extrapolate the discharge
	*/
	if (stage < rateHead->stage)
	{
		ratePtrHigher = (Rating *) ListNext(&rateHead->node);

		diff_discharge = ratePtrHigher->discharge - rateHead->discharge;
		diff_stage = ratePtrHigher->stage - rateHead->stage;
		if (diff_stage == 0)
		   discharge = rateHead->discharge;
		else
		   discharge = rateHead->discharge - 
                               ((diff_discharge/diff_stage) * 
                               (rateHead->stage - stage));
	}
	
	/*
	 if the stage value passed in is greater then the highest stage in the
	 rating table then extrapolate the discharge
	*/
	if (stage > rateTail->stage)
	{
		ratePtrLower = (Rating *) ListPrev(&rateTail->node);

		diff_discharge = rateTail->discharge - ratePtrLower->discharge;
		diff_stage = rateTail->stage - ratePtrLower->stage;

		if (diff_stage == 0)
		   discharge = rateTail->discharge;
		else
		   discharge = rateTail->discharge + 
                               ((diff_discharge/diff_stage) * 
                               (stage - rateTail->stage));
	}

	/*
	 if the stage value passed in is between the lowest and highest stage
	 in the rating table then interpolate the discharge
	*/
	if ((stage >= rateHead->stage) && (stage <= rateTail->stage))
	{
		ratePtrLower = (Rating *) ListFirst(&rateHead->list);

		count = ListCount(&rateHead->list);
		for (ctr=0; ctr < count-1; ctr++)
		{
			ratePtrHigher = (Rating *) ListNext(&ratePtrLower->node);
			if (stage >= ratePtrLower->stage && 
                            stage <= ratePtrHigher->stage)
			{
				diff_discharge = ratePtrHigher->discharge - 
                                                 ratePtrLower->discharge;
				diff_stage = ratePtrHigher->stage - 
                                             ratePtrLower->stage;

				if (diff_stage == 0)
				   discharge = ratePtrLower->discharge;
				else
				   discharge = ratePtrLower->discharge + 
                                               ((diff_discharge/diff_stage) * 
                                               (stage - ratePtrLower->stage));

				break;
			}
			ratePtrLower = (Rating *) ListNext(&ratePtrLower->node);
		}
	}

	/*
	 for some reason the discharge is less than zero then return zero
	*/
	if ( discharge < 0.0 )
	   return RATING_CONVERT_FAILED ;
	else
	   return discharge ;
}

/*************************************************************************/

float discharge2stage_buff(char *lid, float discharge)
{

 static char	prev_lid[9]="\0";
	float   stage=0.0,
		diff_discharge=0.0,
		diff_stage=0.0;
int	count , ctr ;
 static	Rating	*rateHead=NULL;
	Rating	*ratePtrLower=NULL, *ratePtrHigher=NULL, *rateTail=NULL;

       /* Check to see if the discharge is a missing value.  If it is
          then return a missing value for the stage. */
       if ( discharge < 0 )
       {
          return RATING_CONVERT_FAILED ;
       }

       /*
         if the lid passed in is NOT the same as the previous lid
         then copy lid passed in to previous
         check if the previous head is NOT NULL then free that
         memory and set it to NULL
         then create a where clause and get the rating curve for this lid
        */
        if (strcmp(lid, prev_lid) != 0)
        {
           strcpy(prev_lid, lid);
           rateHead = load_rating_curve ( lid ) ;

			if ( rateHead == NULL )
				printf("No Rating table available for LID=%s\n", lid);
        }

	/*
	 if the pointer to the head is NULL then this means there
	 is NO rating curve for that location
	*/	
	if ( rateHead == NULL )
		return RATING_CONVERT_FAILED ;

	/*
	 if there is less than 2 points (ie. one) then that means there
	 is NO usable rating curve for that location
	*/
	count = ListCount(&rateHead->list);

	if (count < 2)
	{
		printf("Rating table has less than 2 points for LID=%s\n", lid);
		return RATING_CONVERT_FAILED ;
	}

	/*
	 calculate the last point in the linked list
	*/
	rateTail = (Rating *) ListLast(&rateHead->list);
	
	/*
	 if the discharge value passed in is less then the lowest discharge in
	 the rating table then extrapolate the stage
	*/
	if (discharge < rateHead->discharge)
	{
		ratePtrHigher = (Rating *) ListNext(&rateHead->node);

		diff_discharge = ratePtrHigher->discharge - rateHead->discharge;
		diff_stage = ratePtrHigher->stage - rateHead->stage;
		if (diff_discharge == 0)
		   stage = rateHead->stage;
		else
		   stage = rateHead->stage - ((diff_stage/diff_discharge) * 
                           (rateHead->discharge - discharge));
	}
	
	/*
	 if the discharge value passed in is greater then the highest discharge
	 in the rating table then extrapolate the stage
	*/
	if (discharge > rateTail->discharge)
	{
		ratePtrLower = (Rating *) ListPrev(&rateTail->node);

		diff_discharge = rateTail->discharge - ratePtrLower->discharge;
		diff_stage = rateTail->stage - ratePtrLower->stage;

		if (diff_discharge == 0)
		   stage = rateTail->stage;
		else
		   stage = rateTail->stage + 
                           ((diff_stage/diff_discharge) * 
                           (discharge - rateTail->discharge));
	}

	/*
	 if the discharge value passed in is between the lowest and highest 
         discharge in the rating table then interpolate the stage
	*/
	if ((discharge >= rateHead->discharge) && 
            (discharge <= rateTail->discharge))
	{
		ratePtrLower = (Rating *) ListFirst(&rateHead->list);

		count = ListCount(&rateHead->list);
		for (ctr=0; ctr < count-1; ctr++)
		{
			ratePtrHigher = (Rating *) ListNext(&ratePtrLower->node);
			if (discharge >= ratePtrLower->discharge && 
                            discharge <= ratePtrHigher->discharge)
			{
				diff_discharge = ratePtrHigher->discharge - 
                                                 ratePtrLower->discharge;
				diff_stage = ratePtrHigher->stage - 
                                             ratePtrLower->stage;

				if (diff_discharge == 0)
				   stage = ratePtrLower->stage;
				else
				   stage = ratePtrLower->stage + 
                                           ((diff_stage/diff_discharge) * 
                                         (discharge - ratePtrLower->discharge));

				break;
			}
			ratePtrLower = (Rating *) ListNext(&ratePtrLower->node);
		}
	}

	return stage ;

}

/*************************************************************************/
static int compare_rating_info ( void * search_value , void * array_value )
{
   char * pSearchValue = ( char * ) search_value ;
   RatingInfo * pArrayValue = ( RatingInfo * ) array_value ;

   return ( strcmp ( pSearchValue , pArrayValue->lid ) ) ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:   load_rating_curve
* PURPOSE:       This routine returns a rating curve for a user-specified
*                lid.  This rating curve is returned as a linked list of 
*                of Rating structures.  If no rating curve is available 
*                for the specified lid, then a value of NULL is returned.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME         DESCRIPTION/UNITS
*   Input  char *      lid          The name of the station to retrieve 
*                                   data for.
*
* RETURNS:
*   DATA TYPE                       DESCRIPTION
*   Rating *                        A pointer to the rating curve which
*                                   is applicable for the user-supplied 
*                                   station name.
*
* APIs UTILIZED:
*   NAME               HEADER FILE     DESCRIPTION
*   binary_search      BinarySearch.h  Performs a binary search on an array
*                                      of ordered data elements.
*   free_rating_curve  rating_util.h    Frees the memory allocated for the
*                                      array of RatingInfo structures.  Also,
*                                      frees the memory used by the linked
*                                      list of Rating structures referenced
*                                      by the RatingInfo array.
*   FreeRatingShift    RatingShift.h   Frees the dynamic memory used by the
*                                      the linked list of rating shift data.
*   GetRating          Rating.h        Retrieves a linked list of Rating
*                                      structures containing rating.
*                                      The exact nature of the data returned in
*                                      this linked list depends upon the
*                                      "where" clause supplied by the user.
*   GetRatingShift     RatingShift.h   This routine returns a linked list 
*                                      of RatingShift structures which contains
*   ListAdd            List.h
*   ListFirst          List.h
*   ListInit           List.h
*   ListNext           List.h
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

/* This variable is used only by the load_rating_curve and
   free_rating_curve routines.  It is static so that its visibility
   and accessibility is limited to routines subsequent to this point
   in the file. */
static RatingInfo * pRatingCurve = NULL ;
static Rating * pRatingHead = NULL ;
static int num_rating_curves = 0 ;

Rating * load_rating_curve ( char * lid )
{
   char * station_name = NULL ;
   double shift_amount ;
   static int first = 1 ;
   int i ;
   int num_rating_points ;
   int value ;
   Rating * pRatingNode = NULL ;
   Rating * pRatingTemp = NULL ;
   Rating * pRatingPrev = NULL ;
   RatingInfo * pRatingInfoHead = NULL ;
   RatingInfo * pRatingInfoNode = NULL ;
   RatingInfo * pRatingInfoTemp = NULL ;
   RatingShift * pRatingShiftHead = NULL ;
   RatingShift * pRatingShiftNode = NULL ;

   /* Check to determine if the rating curve information has
      been loaded yet. */
   if ( first == 1 )
   {
      first = 0 ;

      /*  Retrieve all of the rating curves.  These will be ordered by lid
          and stage in ascending order. */
      pRatingHead = GetRating ( " ORDER BY lid, stage ASC " ) ;

      if ( pRatingHead == NULL )
      {
         return NULL ;
      }

      pRatingNode = ( Rating * ) ListFirst ( & pRatingHead->list ) ;

      /* Load the valid rating shift values and the lids they pertain to. */ 
      pRatingShiftHead = GetRatingShift ( " WHERE active = 'T' ORDER BY"
                                          " lid ASC, date DESC " ) ;
           
      if ( pRatingShiftHead != NULL )
      {
         pRatingShiftNode = ( RatingShift * ) ListFirst ( 
                                              & pRatingShiftHead->list ) ; 
      }

      while ( pRatingNode != NULL )
      {
         /* set shift amount to zero.
            Increment the unique station count.
            Set the individual rating curve point count = 1 */
         shift_amount = 0.0 ;
         ++ num_rating_curves ;
         num_rating_points = 1 ;

         /* Temporarily point to the head pointer of this station's data.
            From the first node get the station name. */
         pRatingTemp = pRatingNode ;
         station_name = pRatingNode->lid ;

         /* Check to determine if there is a rating shift for this
            station. */
         while ( pRatingShiftNode != NULL )
         {
            value = strcmp ( station_name , pRatingShiftNode->lid ) ;

            if ( value == 0 )
            {
               /* There is a rating curve for this station. */
               shift_amount = pRatingShiftNode->shift_amount ;
               break ;
            }
            else if ( value < 0 )
            {
               /* There is no rating shift amount for this station. */ 
               break ;
            }
            else 
            {
                pRatingShiftNode = ( RatingShift * ) ListNext (
                                                    & pRatingShiftNode->node ) ;
            }
         }

         if ( shift_amount != 0.0 )
         {
            pRatingNode->stage += shift_amount ;
            pRatingNode = ( Rating * ) ListNext ( & pRatingNode->node ) ;  

            while ( ( pRatingNode != NULL ) &&
                    ( strcmp ( station_name , pRatingNode->lid ) == 0 ) ) 
            {
               ++ num_rating_points ;
               pRatingNode->stage += shift_amount ;
               pRatingPrev = pRatingNode ;
               pRatingNode = ( Rating * ) ListNext ( & pRatingNode->node ) ;
            }
      
         }
         else
         {
            pRatingNode = ( Rating * ) ListNext ( & pRatingNode->node ) ;  

            while ( ( pRatingNode != NULL ) &&
                    ( strcmp ( station_name , pRatingNode->lid ) == 0 ) ) 
            {
               ++ num_rating_points ;
               pRatingPrev = pRatingNode ;
               pRatingNode = ( Rating * ) ListNext ( & pRatingNode->node ) ;
            }
       
         }

         pRatingInfoNode = ( RatingInfo * ) malloc ( sizeof ( RatingInfo ) ) ;

         if ( pRatingInfoNode == NULL )
         {
            fprintf ( stderr , "\nIn routine 'load_rating_curve':\n"
                               "Could not allocate %d bytes for a\n"
                               "RatingInfo structure. Rating curve\n"
                               "information will not be loaded.\n" , 
                               sizeof ( RatingInfo ) ) ;

            /* Free whatever memory has been loaded. */
            if ( pRatingInfoHead != NULL )
            {
               pRatingInfoNode = ( RatingInfo * ) 
                                 ListFirst ( & pRatingInfoHead->list ) ;

               while ( pRatingInfoNode != NULL )
               {
                  pRatingInfoTemp = ( RatingInfo * ) ListNext ( 
                                                     & pRatingInfoNode->node ) ;
                  free ( pRatingInfoNode ) ;
                  pRatingInfoNode = pRatingInfoTemp ;
               }

               pRatingInfoHead = NULL ;
            }

            free_rating_curve ( ) ;

            num_rating_curves = 0 ;
            return NULL ;
         }

         strcpy ( pRatingInfoNode->lid , station_name ) ;
         pRatingInfoNode->count = num_rating_points ;
         pRatingInfoNode->pRatingHead = pRatingTemp ;

         /* Copy the information necessary to make the program
            think that pRatingTemp is a minature linked list. */
         pRatingTemp->list.first = ( Node * ) pRatingTemp ;
         pRatingTemp->list.last = ( Node * ) pRatingPrev ; 
         pRatingTemp->list.count = num_rating_points ;

         if ( pRatingInfoHead == NULL )
         {
            pRatingInfoHead = pRatingInfoNode ;
            ListInit ( & pRatingInfoHead->list ) ;
         }

         ListAdd ( & pRatingInfoHead->list , & pRatingInfoNode->node ) ;
      }

      /* Free the linked list of Rating Shift information. */
      if ( pRatingShiftHead != NULL )
      {
         FreeRatingShift ( pRatingShiftHead ) ; 
         pRatingShiftHead = NULL ;
      }

      pRatingCurve = ( RatingInfo * ) malloc ( num_rating_curves *
                                              sizeof ( RatingInfo ) ) ;
      
      if ( pRatingCurve == NULL )
      {
         fprintf ( stderr , "\nIn routine 'load_rating_curve':\n"
                            "Could not allocate %d bytes for an array of\n"
                            "%d RatingInfo structures. Rating curve\n"
                            "information will not be loaded.\n" , 
                            sizeof ( RatingInfo ) * num_rating_curves ,
                            num_rating_curves ) ;

      /* Free whatever memory has been allocated up to this point
         in this routine. */
         if ( pRatingInfoHead != NULL )
         {
            pRatingInfoNode = ( RatingInfo * ) 
                                      ListFirst ( & pRatingInfoHead->list ) ;

            while ( pRatingInfoNode != NULL )
            {
               pRatingInfoTemp = ( RatingInfo * ) ListNext ( 
                                                  & pRatingInfoNode->node ) ;
               free ( pRatingInfoNode ) ;
               pRatingInfoNode = pRatingInfoTemp ;
            }

            pRatingInfoHead = NULL ;
         }

         free_rating_curve ( ) ;

         num_rating_curves = 0 ;
         return NULL ;
      }

      pRatingInfoNode = ( RatingInfo * ) ListFirst ( 
                                                 & pRatingInfoHead->list ) ;

      for ( i = 0 ; i < num_rating_curves ; ++ i )
      {
         pRatingCurve [ i ] = * pRatingInfoNode ;
         pRatingInfoNode = ( RatingInfo * ) ListNext 
                                            ( & pRatingInfoNode->node ) ;
      }

      if ( pRatingInfoHead != NULL )
      {
         pRatingInfoNode = ( RatingInfo * ) ListFirst ( 
                                                    & pRatingInfoHead->list ) ;

         while ( pRatingInfoNode != NULL )
         {
            pRatingInfoTemp = ( RatingInfo * ) ListNext ( 
                                               & pRatingInfoNode->node ) ;
            free ( pRatingInfoNode ) ;
            pRatingInfoNode = pRatingInfoTemp ;
         }

         pRatingInfoHead = NULL ;
      }

   }
   
   /* Search for the lid and return the linked list of rating 
      information. */
   if ( ( num_rating_curves > 0 ) && ( pRatingCurve != NULL ) &&
        ( lid != NULL ) )
   {
      pRatingInfoNode = ( RatingInfo * ) 
                         binary_search ( pRatingCurve ,
                                         lid ,
                                         num_rating_curves ,
                                         sizeof ( RatingInfo ) ,
                                         compare_rating_info ) ;

      if ( pRatingInfoNode != NULL )
      {
         return ( pRatingInfoNode->pRatingHead ) ;
      }

   }

   return NULL ;
}

/*************************************************************************/

void free_rating_curve ( )
{
   /* Check to see if there is rating curve data.  If so,
      deallocate the memory that was used to create it. */

   if ( pRatingCurve != NULL )
   {
      free ( pRatingCurve ) ;
      pRatingCurve = NULL ;
   }

   if ( pRatingHead != NULL ) 
   {
      FreeRating ( pRatingHead ) ;
      pRatingHead = NULL ;
   }

}
