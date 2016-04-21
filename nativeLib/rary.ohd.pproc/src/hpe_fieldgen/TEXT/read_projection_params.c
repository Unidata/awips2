/*******************************************************************************
 * FILENAME:            read_projection_params.c
 *
 * Purpose:
 * This function loads static data from projection constant file.
 * parameters are plugged into projection_params_struct.
 *
 * calling function: run_nowcast
 * functions called: NULL
 *
 * ORIGINAL AUTHOR:       Guoxian Zhou
 * CREATION DATE:         January, 2008
 * ORGANIZATION:          HSEB / OHD
 * MACHINE:               HP-UX / Dell-Redhat Linux
 * MODIFICATION HISTORY:
 *   DATE         PROGRAMMER        DESCRIPTION/REASON
 *   January 2008 Guoxian Zhou      first version 
 ********************************************************************************
 */

#include "empe_fieldgen.h"

void readProjectionParams(projection_params_struct * pProjectionParams)
{
    const char * INPUT_DIR_TOKEN = "hpe_input_dir";
    const char * PROJECTION_FILENAME = "projection.con";

    char inputDir[PATH_LEN] = { '\0' };
    char file_path[PATH_LEN] = { '\0' };
    char buf[MESSAGE_LEN] = { '\0' };

    FILE * fp = NULL;

    if (hpe_fieldgen_getAppsDefaults(INPUT_DIR_TOKEN, inputDir) == -1)
    {
        sprintf(message, "ERROR: Invalid token value"
            " for token \"%s\"."
            "\n\tProgram exit.", INPUT_DIR_TOKEN) ;
        shutdown(message);
    }

    sprintf(file_path, "%s/%s", inputDir, PROJECTION_FILENAME);

    if ((fp = fopen(file_path, "r")) == NULL)
    {
        sprintf(message, "ERROR:in readProjectionParams function, "
            "cannot open input file: %s"
            "\n\tProgram exit.", file_path) ;
        shutdown(message);
    }
    fgets(buf, MESSAGE_LEN, fp);
    sscanf(buf, "%lf", &pProjectionParams->resetTime);

    fgets(buf, MESSAGE_LEN, fp);
    sscanf(buf, "%lf", &pProjectionParams->projectionInterval);

    fgets(buf, MESSAGE_LEN, fp);
    sscanf(buf, "%lf", &pProjectionParams->normalGridSize);

    fgets(buf, MESSAGE_LEN, fp);
    sscanf(buf, "%lf", &pProjectionParams->errorProportionFactor);

    fgets(buf, MESSAGE_LEN, fp);
    sscanf(buf, "%lf", &pProjectionParams->nominalScanInterval);

    fgets(buf, MESSAGE_LEN, fp);
    sscanf(buf, "%lf", &pProjectionParams->minThresholdPrecipRate);

    fgets(buf, MESSAGE_LEN, fp);
    sscanf(buf, "%d", &pProjectionParams->minSampleNumber);

    fgets(buf, MESSAGE_LEN, fp);
    sscanf(buf, "%lf", &pProjectionParams->maxMissingperiodOfRadar);

    fgets(buf, MESSAGE_LEN, fp);
    sscanf(buf, "%lf", &pProjectionParams->minPDFAreaThreshold);

    fgets(buf, MESSAGE_LEN, fp);
    sscanf(buf, "%lf", &pProjectionParams->minPDFRainrateThreshold);

    fgets(buf, MESSAGE_LEN, fp);
    char growthflag = '\0';
    sscanf(buf, "%c", &growthflag);
    if ((growthflag == 'y') || (growthflag == 'Y'))
    {
        pProjectionParams->isGrowth = 1;
    } else
    {
        pProjectionParams->isGrowth = 0;
    }

    //fgets(buf, MESSAGE_LEN, fp);
    //sscanf(buf, "%d", &pProjectionParams->smoothingMethod);

    fgets(buf, MESSAGE_LEN, fp);
    sscanf(buf, "%d", &pProjectionParams->stormMotionVector);

    double param;
    fgets(buf, MESSAGE_LEN, fp);
    sscanf(buf, "%lf", &param);
    if ( (param >= 0.7) && (param <= 0.8))
    {
        pProjectionParams->lamda = param;
    } else
    {
        pProjectionParams->lamda = 0.8;
    }

    fgets(buf, MESSAGE_LEN, fp);
    sscanf(buf, "%lf", &param);
    if ( (param >= 1.0) && (param <= 1.3))
    {
        pProjectionParams->kappa = param;
    } else
    {
        pProjectionParams->kappa = 1.2;
    }

    fclose(fp);
    fp = NULL;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/read_projection_params.c,v $";
 static char rcs_id2[] = "$Id: read_projection_params.c,v 1.2 2008/05/14 18:58:59 gzhou Exp $";}
/*  ===================================================  */

}
