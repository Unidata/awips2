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

import java.io.File;

import javax.measure.UnitConverter;
import javax.measure.Unit;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.viz.mpe.core.MPEDataManager;

/**
 * Abstraction of the Prism data reading capability.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2017 6407       bkowal      Initial creation
 * Sep 26, 2018 7482       smanoj      Fix the issue with data conversion
 * 
 * </pre>
 *
 * @author bkowal
 */

public abstract class AbstractPrismDataReader {

    protected final int MaxX;

    protected final int MaxY;

    protected final UnitConverter dataToImage;

    protected final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    protected final String mon_name[] = { "jan", "feb", "mar", "apr", "may",
            "jun", "jul", "aug", "sep", "oct", "nov", "dec" };
    
    protected final DailyQcUtils dqc = DailyQcUtils.getInstance();

    public AbstractPrismDataReader(final Unit<?> displayUnit,
            final Unit<?> dataUnit) {
        MaxX = MPEDataManager.getInstance().getHRAPExtent().width;
        MaxY = MPEDataManager.getInstance().getHRAPExtent().height;
        dataToImage = MPEConversionUtils.constructConverter(dataUnit,
                displayUnit);
    }

    public boolean isWithinRange(int k, int smonth, int emonth) {
        if ((smonth <= emonth) && (k >= smonth) && (k <= emonth)) {
            return true;
        }
        if (smonth > emonth) {
            if (k <= emonth) {
                return true;
            }

            if (k >= smonth) {
                return true;
            }
        }
        return false;
    }

    protected MPEPrismDataLoadFailed readPrismAndPopulateDestination(
            final String mpe_prism_dir, final String mpe_rfc_name,
            final String prismFilePrefix, final int monthIndex,
            final String monAbbr, final int[][][] destination,
            final UnitConverter dataToImage) {
        final String prismFilePath = mpe_prism_dir + File.separator
                + prismFilePrefix + mpe_rfc_name + "_" + monAbbr;
        final File prismFile = new File(prismFilePath);
        if (!prismFile.exists()) {
            return new MPEPrismDataLoadFailed(
                    "Unable to find prism file: " + prismFilePath + ".");
        }
        XmrgFile xmrgFile = new XmrgFile(prismFile);
        try {
            xmrgFile.load();
        } catch (Exception e) {
            return new MPEPrismDataLoadFailed(
                    "Failed to load xmrg prism file: " + prismFilePath + ".",
                    e);
        }
        /*
         * Verify that the hrap extents have been specified in the xmrg file.
         */
        if (xmrgFile.getHrapExtent() == null) {
            return new MPEPrismDataLoadFailed("Xmrg prism file: "
                    + prismFilePath + " is missing the HRAP Extents.");
        }

        /*
         * Verify that the data set contained within the xmrg file is the
         * expected size.
         */
        if (xmrgFile.getData().length != (MaxX * MaxY)) {
            return new MPEPrismDataLoadFailed(
                    "Xmrg prism file: " + prismFilePath
                            + " does not contain sufficient data. Expected data length = "
                            + (MaxX * MaxY) + "; actual data length = "
                            + xmrgFile.getData().length + ".");
        } 
             
        final short[] data = xmrgFile.getData();
        int index = 0;
        for (int i = MaxY - 1; i >= 0; i--) {
            for (int j = 0; j < MaxX; j++) {
                float f = 0;
                short s = data[index];
                ++index;
                if (s < 0) {
                    f = handleNegativeValue(dataToImage, s);
                } else {
                    f = (float) dataToImage.convert(s);
                }
                
                float aa = (float) (Math.floor((f * getconvFactor())));
                int bb = (int) aa;
                destination[monthIndex][i][j] = bb;
            }
        }

        return null;
    }
    
    protected abstract float getconvFactor();
    	
    protected abstract float handleNegativeValue(
            final UnitConverter dataToImage, final short value);
    
    
}