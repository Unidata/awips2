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

import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.unit.MetricPrefix;

/**
 * Reads the monthly precipitation prism data from XMRG files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2009            snaples     Initial creation
 * April , 2012  8672      lbousaidi   fixed the reading of the PRISM data.
 * Feb 3,  2015  16993     snaples     fixed color scale data conversion issue.
 * Mar 2,  2015  15660     snaples     Fixed problem with color scale using wrong values. Causing grids to be all zeros.
 * Oct 03, 2017  6407      bkowal      Cleanup. Updated to extend {@link AbstractPrismDataReader}.
 * Sep 27, 2018  7482      smanoj      Fix the issue with data conversion
 * 
 * </pre>
 * 
 * @author snaples
 */

public class MeanMonthlyPrecip extends AbstractPrismDataReader {

    private final String VERSION = "111511";

    private static Isoh isoh;
    
    private static final float convFactor = 25.4f;
    
    public MeanMonthlyPrecip() {
        super(USCustomary.INCH, MetricPrefix.MILLI(SI.METRE));
    }

    /*
     * TODO: Would it not be great if this method actually returned what it read
     * so that it could be passed along as needed instead of populating static
     * variables?
     */
    public MPEPrismDataLoadFailed read_mean_monthly_precip(String mpe_prism_dir,
            String mpe_rfc_name, int smonth, int emonth) {
        isoh = new Isoh();
        isoh.value = new int[mon_name.length][MaxY][MaxX];

        statusHandler.info("Loading precipitation prism data (Version = "
                + VERSION + ") ...");
        /* Read in the PRISM files. */
        /* j increments latitude i increments longitude */

        for (int k = 0; k < mon_name.length; k++) {
            /*
             * Loop over the months. Determine for which months PRISM data is
             * needed.
             */
            if (!isWithinRange(k, smonth, emonth)) {
                continue;
            }

            final String monAbbr = mon_name[k];
            MPEPrismDataLoadFailed mpePrismDataLoadFailed = readPrismAndPopulateDestination(
                    mpe_prism_dir, mpe_rfc_name, "prism_mean_precip_", k,
                    monAbbr, isoh.value, dataToImage);
            if (mpePrismDataLoadFailed != null) {
                return mpePrismDataLoadFailed;
            }
        }

        dqc.isohyets_used = true;
        statusHandler.info("Successfully loaded precipitation prism data.");
        return null;
    }

    public static class Isoh {

        public int value[][][];

    }

    /**
     * @return the isoh
     */
    public Isoh getIsoh() {
        return isoh;
    }

    /**
     * @param isoh
     *            the isoh to set
     */
    public void setIsoh(Isoh isoh) {
        MeanMonthlyPrecip.isoh = isoh;
    }

    @Override
    protected float handleNegativeValue(UnitConverter dataToImage,
            short value) {
        return 0;
    }
    
    @Override
    protected float getconvFactor(){
        return convFactor;
    }
}