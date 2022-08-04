/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.mpe.util;

import javax.measure.UnitConverter;

import systems.uom.common.USCustomary;

/**
 * Reads the monthly temperature prism data from XMRG files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- -----------  --------------------------
 * Feb 25, 2009            snaples      Initial creation
 * Apr 16, 2012            mgamazaychik DR9602 - changed how max and min 
 *                         temperature data are read from PRISM
 * Oct 03, 2017 6407       bkowal       Cleanup. Updated to extend {@link AbstractPrismDataReader}.
 * Sep 27, 2018  7482      smanoj       Fix the issue with data conversion
 * 
 * </pre>
 * 
 * @author snaples
 */

public class MeanMonthlyTemp extends AbstractPrismDataReader {

    private static MaxMin maxmin;
    
    private static final float convFactor = 10.0f;
    
    public MeanMonthlyTemp() {
        super(USCustomary.FAHRENHEIT, USCustomary.FAHRENHEIT.divide(10));
    }

    /*
     * TODO: Would it not be great if this method actually returned what it read
     * so that it could be passed along as needed instead of populating static
     * variables?
     */
    public MPEPrismDataLoadFailed read_mean_monthly_temp(String mpe_prism_dir,
            String mpe_rfc_name, int smonth, int emonth) {
        if (dqc.init_maxmin) {
            return null;
        }

        statusHandler.info("Loading temperature prism data ...");

        maxmin = new MaxMin();
        maxmin.maxvalue = new int[mon_name.length][MaxY][MaxX];
        maxmin.minvalue = new int[mon_name.length][MaxY][MaxX];

        /* Read in the PRISM files. */
        /* j increments latitude i increments longitude */
        for (int k = 0; k < mon_name.length; k++) {
            if (!isWithinRange(k, smonth, emonth)) {
                continue;
            }

            final String monAbbr = mon_name[k];
            MPEPrismDataLoadFailed mpePrismDataLoadFailed = readPrismAndPopulateDestination(
                    mpe_prism_dir, mpe_rfc_name, "prism_max_temp_", k, monAbbr,
                    maxmin.maxvalue, dataToImage);
            if (mpePrismDataLoadFailed != null) {
                return mpePrismDataLoadFailed;
            }
            mpePrismDataLoadFailed = readPrismAndPopulateDestination(
                    mpe_prism_dir, mpe_rfc_name, "prism_min_temp_", k, monAbbr,
                    maxmin.minvalue, dataToImage);
            if (mpePrismDataLoadFailed != null) {
                return mpePrismDataLoadFailed;
            }
        }

        dqc.init_maxmin = true;
        dqc.maxmin_used = true;
        statusHandler.info("Successfully loaded temperature prism data.");
        return null;
    }

    public static class MaxMin {

        public int maxvalue[][][];

        public int minvalue[][][];

    }

    /**
     * @return the maxmin
     */
    public MaxMin getMaxmin() {
        return maxmin;
    }

    /**
     * @param maxmin
     *            the maxmin to set
     */
    public void setMaxmin(MaxMin maxmin) {
        MeanMonthlyTemp.maxmin = maxmin;
    }

    @Override
    protected float handleNegativeValue(UnitConverter dataToImage,
            short value) {
        if (value == -9999 || value == -999 || value == -8888
                || value == -899) {
            return value;
        } else {
            return (float) dataToImage.convert(value);
        }
    }
    
    @Override
    protected float getconvFactor(){
        return convFactor;
    }
}