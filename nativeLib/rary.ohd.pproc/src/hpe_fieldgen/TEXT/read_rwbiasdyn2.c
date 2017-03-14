#include <stdio.h>
#include <string.h>
#include "empe_fieldgen.h"
#include "time_convert.h"
#include "RWBiasDyn.h"
#include "RadarLoc.h"

#define HPE_RFC_BIAS_LAG 2    // default number of bias lag hours
RadarLoc * pRadarLocHead = NULL;
extern int dualpol_on_flag;

void retrieveOfficeIDByRadarID(const char * radarID, char * officeID,
        long int * status);

void read_rwbiasdyn2(const char *radar_id, const char *office_id,
        const char * str, const int lag_cut, double *num_pairs, double *sumgag,
        double *sumrad, double *bias, int *lag, char sstr1[19], long int *irc);

void readRWBiasDyn(const char *radar_id, const char *site_id,
        const char *datehour, const int lag_cut, double *num_pairs,
        double *sumgag, double *sumrad, double *bias, int *lag,
        char datetime1[19], int dualpol_data_avail, long int *irc)

{
    const char * HPE_RFC_BIAS_LAG_TOKEN = "hpe_rfc_bias_lag";
    const char * HPE_BIAS_SOURCE_TOKEN = "hpe_bias_source";

    static int first = 1;
    static int rfc_bias_lag = HPE_RFC_BIAS_LAG;
    static char bias_source[6] = "rfc"; // RFC or LOCAL, default to RFC
    char officeID[WFO_LEN + 1] = { '\0' };
    char strTokenValue[6] = { '\0' };

    /*
     * load and store the token values:
     *   HPE_RFC_BIAS_LAG_TOKEN
     *   HPE_BIAS_SOURCE_TOKEN
     */

    if (first == 1) {
        if ((hpe_fieldgen_getAppsDefaults(HPE_RFC_BIAS_LAG_TOKEN, strTokenValue)
                != -1) && (hpe_fieldgen_isDigits(strTokenValue) == 1)) {
            int value = atoi(strTokenValue);

            if (value > 0) {
                rfc_bias_lag = value;
                sprintf(message,
                        "STATUS:in readRWBiasDyn - token value for \"%s\" is: %d",
                        HPE_RFC_BIAS_LAG_TOKEN, rfc_bias_lag);
                hpe_fieldgen_printMessage(message);
            } else {
                sprintf(message, "ERROR:in readRWBiasDyn - Invalid token value"
                        " for token \"%s\". Default value is set to: %d",
                        HPE_RFC_BIAS_LAG_TOKEN, HPE_RFC_BIAS_LAG);
                hpe_fieldgen_printMessage(message);
            }
        } else {
            sprintf(message, "ERROR:in readRWBiasDyn - Invalid token value"
                    " for token \"%s\". Default value is set to: %d",
                    HPE_RFC_BIAS_LAG_TOKEN, HPE_RFC_BIAS_LAG);
            hpe_fieldgen_printMessage(message);
        }

        if (hpe_fieldgen_getAppsDefaults(HPE_BIAS_SOURCE_TOKEN, strTokenValue)
                != -1) {
            if (strcmp(hpe_fieldgen_toLowerCase(strTokenValue), "local") == 0) {
                strcpy(bias_source, "local");

                sprintf(message,
                        "STATUS:in readRWBiasDyn - token value for \"%s\" is: %s",
                        HPE_BIAS_SOURCE_TOKEN, bias_source);
                hpe_fieldgen_printMessage(message);
            } else if (strcmp(hpe_fieldgen_toLowerCase(strTokenValue), "rfc")
                    == 0) {
                strcpy(bias_source, "rfc");

                sprintf(message,
                        "STATUS:in readRWBiasDyn - token value for \"%s\" is: %s",
                        HPE_BIAS_SOURCE_TOKEN, bias_source);
                hpe_fieldgen_printMessage(message);
            }
        } else {
            sprintf(message, "ERROR:in readRWBiasDyn - Invalid token value"
                    " for token \"%s\". Default value is set to: rfc",
                    HPE_BIAS_SOURCE_TOKEN);
            hpe_fieldgen_printMessage(message);
        }

        first = 0;
    }

    /*
     * if the bias source is LOCAL,
     * pick up bias value based on the FXA_LOCAL_SITE
     */

    if (strcmp(bias_source, "local") == 0) /*for LOCAL MFB */
    {
        if (dualpol_on_flag == 0) {
            sprintf(message,
                    " STATUS:in readRWBiasDyn - Loading SP LOCAL MFB bias from RWBiasDyn "
                            "table for siteID (token fxa_local_site) = %s, lag time = %d",
                    site_id, lag_cut);
            hpe_fieldgen_printMessage(message);

            read_rwbiasdyn2(radar_id, site_id, datehour, lag_cut, num_pairs,
                    sumgag, sumrad, bias, lag, datetime1, irc);
        } else {
            sprintf(message,
                    "STATUS:in readRWBiasDyn - Loading DP LOCAL MFB bias from DAABiasDyn "
                            "table for siteID (token fxa_local_site) = %s, lag time = %d",
                    site_id, lag_cut);
            hpe_fieldgen_printMessage(message);

            read_daabiasdyn(radar_id, site_id, datehour, lag_cut, num_pairs,
                    sumgag, sumrad, bias, lag, datetime1, irc);
        }
    } else /* for RFC MFB */
    {
        /*
         * retrieve the office ID from RadarLoc table
         */

        retrieveOfficeIDByRadarID(radar_id, officeID, irc);

        if (*irc == -1) {
            sprintf(message,
                    "ERROR:in readRWBiasDyn - could not find office ID for radar: %s",
                    radar_id);
            shutdown(message);
        }

        sprintf(message, "STATUS:in readRWBiasDyn - officeID = %s, siteID = %s",
                officeID, site_id);
        hpe_fieldgen_printMessage(message);

        /*
         * Pick up the bias value based on the radar's office ID.
         * If there is no record found, check if the office ID is
         * the same as the FXA_LOCAL_SITE, if not, then use the
         * FXA_LOCAL_SITE as office ID to pick up the bias value again.
         */

        if (strcmp(officeID, site_id) != 0) {
            /*
             * if the office ID != FXA_LOCAL_SITE,
             * then use the HPE_RFC_BIAS_LAG_TOKEN value
             * to pick up the bias value.
             */
            if (dualpol_on_flag == 0) {
                sprintf(message,
                        "STATUS:in readRWBiasDyn - start to load SP RFC MFB from RWBiasDyn table"
                                " for officeID = %s, lag time = %d", officeID,
                        rfc_bias_lag);
                hpe_fieldgen_printMessage(message);

                read_rwbiasdyn2(radar_id, officeID, datehour, rfc_bias_lag,
                        num_pairs, sumgag, sumrad, bias, lag, datetime1, irc);
                if (*irc != 0) {
                    sprintf(message,
                            "STATUS:in readRWBiasDyn - can not load SP RFC MFB bias from RWBiasDyn"
                                    " table for officeID = %s. Loading SP LOCAL MFB for siteID = %s, lag time = %d",
                            officeID, site_id, lag_cut);
                    hpe_fieldgen_printMessage(message);

                    read_rwbiasdyn2(radar_id, site_id, datehour, lag_cut,
                            num_pairs, sumgag, sumrad, bias, lag, datetime1,
                            irc);
                } else {
                    sprintf(message,
                            "STATUS:in readRWBiasDyn - Loading SP RFC MFB bias from RWBiasDyn "
                                    "table for officeID = %s, lag time = %d",
                            officeID, rfc_bias_lag);
                    hpe_fieldgen_printMessage(message);
                }
            } else {
                sprintf(message,
                        "STATUS:in readRWBiasDyn - start to load DP RFC MFB from DAABiasDyn table"
                                " for officeID = %s, lag time = %d", officeID,
                        rfc_bias_lag);
                hpe_fieldgen_printMessage(message);

                read_daabiasdyn(radar_id, officeID, datehour, rfc_bias_lag,
                        num_pairs, sumgag, sumrad, bias, lag, datetime1, irc);
                if (*irc != 0) {
                    sprintf(message,
                            "STATUS:in readRWBiasDyn - can not load DP RFC MFB from DAABiasDyn table"
                                    " for officeID = %s. Loading DP LOCAL MFB for siteID = %s, lag time = %d",
                            officeID, site_id, lag_cut);
                    hpe_fieldgen_printMessage(message);

                    read_daabiasdyn(radar_id, site_id, datehour, lag_cut,
                            num_pairs, sumgag, sumrad, bias, lag, datetime1,
                            irc);
                } else {
                    sprintf(message,
                            "STATUS:in readRWBiasDyn - Loading DP RFC MFB bias from DAABiasDyn table"
                                    " for officeID = %s, lag time = %d",
                            officeID, rfc_bias_lag);
                    hpe_fieldgen_printMessage(message);
                }
            }
        } else {
            if (dualpol_on_flag == 0) {
                read_rwbiasdyn2(radar_id, officeID, datehour, lag_cut,
                        num_pairs, sumgag, sumrad, bias, lag, datetime1, irc);

                sprintf(message,
                        "STATUS:in readRWBiasDyn - Loading SP RFC MFB bias from RWBiasDyn table"
                                " for officeID(siteID) = %s, lag time = %d",
                        officeID, lag_cut);
                hpe_fieldgen_printMessage(message);
            } else {
                read_daabiasdyn(radar_id, officeID, datehour, lag_cut,
                        num_pairs, sumgag, sumrad, bias, lag, datetime1, irc);

                sprintf(message,
                        "STATUS:in readRWBiasDyn - Loading DP RFC MFB bias from DAABiasDyn table "
                                "for officeID(siteID) = %s, lag time = %d",
                        officeID, lag_cut);
                hpe_fieldgen_printMessage(message);
            }
        }
    }
}

void read_rwbiasdyn2(const char *radar_id, const char *office_id,
        const char * str, const int lag_cut, double *num_pairs, double *sumgag,
        double *sumrad, double *bias, int *lag, char sstr1[19], long int *irc)

/*
 this function reads records from the RWBiasDyn table for previous hours
 these records contain the state variables used in the bias calculations
 each previous hour up to lag_cut hours is searched to find records in the
 table if no records is found, then state variables are reinitialized

 calling function: mfb_subr
 */

{
    int j, irec, ctr;
    RWBiasDyn *bdHead = NULL;
    RWBiasDyn *bdPtr = NULL;
    char rrad[4] = { '\0' };
    char strp[22] = { '\0' };
    char dude[22] = { '\0' };
    char where[BUFSIZ] = { '\0' };
    char obstime_ANSI[ANSI_TIME_LEN + 1] = { '\0' };
    time_t firstTime = 0;

    strncpy(rrad, radar_id, 3);
    strncpy(dude, str, 19);

    yearsec_ansi_to_timet(dude, &firstTime);

    /*------------------------------------------------*/
    /*   subtract 1 hour and attempt to select record */
    /*------------------------------------------------*/

    *irc = -1;
    for (j = 0; j < lag_cut; j++) {
        firstTime -= SECONDS_PER_HOUR;
        timet_to_yearsec_ansi(firstTime, strp);
        strncpy(sstr1, strp, 19);

        irec = 0;
        *lag = j + 1;

        /*------------------------------------------*/
        /*   select records for all mem_span values */
        /*------------------------------------------*/

        timet_to_yearsec_ansi(firstTime, obstime_ANSI);
        sprintf(where, " WHERE radid='%s' AND obstime='%s' AND "
                " office_id = '%s'", rrad, obstime_ANSI, office_id);

        bdHead = GetRWBiasDyn(where);
        if (bdHead) {
            bdPtr = (RWBiasDyn *) ListFirst(&bdHead->list);
            ctr = 0;
            while (bdPtr) {
                num_pairs[ctr] = (double) bdPtr->numpairs;
                sumgag[ctr] = (double) bdPtr->sumgag;
                sumrad[ctr] = (double) bdPtr->sumrad;
                bias[ctr] = (double) bdPtr->bias;

                ctr++;
                bdPtr = (RWBiasDyn *) ListNext(&bdPtr->node);
            }

            if (bdHead != NULL) {
                FreeRWBiasDyn(bdHead);
                bdHead = NULL;
            }

            if (ctr > 0) {
                *irc = 0;
                break;
            }
        } else {
            *irc = 100;
        }
    } /*  end for (j=0  ...  */

    return;

} /*  end read_rwbiasdyn2 function  */

void retrieveOfficeIDByRadarID(const char * radarID, char * officeID,
        long int * status) {
    char where_clause[BUFSIZ] = { '\0' };
    RadarLoc * pRadarLocNode = NULL;

    *status = 0;
    strcpy(officeID, "");

    sprintf(where_clause, " ");

    if (pRadarLocHead == NULL) {
        pRadarLocHead = GetRadarLoc(where_clause);
    }

    if (pRadarLocHead != NULL) {
        pRadarLocNode = (RadarLoc *) ListFirst(&pRadarLocHead->list);

        while (pRadarLocNode != NULL) {
            if (strcmp(pRadarLocNode->radid, radarID) == 0) {
                strcpy(officeID, pRadarLocNode->office_id);

                sprintf(message,
                        "STATUS:in retrieveOfficeIDByRadarID - From RadarLoc table, Radar ID: %s, Office ID: %s",
                        radarID, officeID);
                hpe_fieldgen_printMessage(message);

                break;
            } else {
                pRadarLocNode = (RadarLoc *) ListNext(&pRadarLocNode->node);
            }
        }
    } else {
        *status = -1;
    }

    return;
}

void freeRadarLocMemory() {
    /* Deallocate the RadarLoc linked list. */

    if (pRadarLocHead != NULL) {
        FreeRadarLoc(pRadarLocHead);
        pRadarLocHead = NULL;
    }
}

