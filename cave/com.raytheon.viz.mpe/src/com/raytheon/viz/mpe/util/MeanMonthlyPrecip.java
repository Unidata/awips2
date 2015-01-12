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
import java.io.IOException;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.mpe.core.MPEDataManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2009            snaples     Initial creation
 * April , 2012  8672      lbousaidi  fixed the reading of the PRISM data.
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class MeanMonthlyPrecip {
    int MaxX;

    int MinX;

    int MinY;

    int MaxY;

    private final String VERSION = "111511";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MeanMonthlyPrecip.class);

    private static Isoh isoh;

    ColorMapParameters cmc = new ColorMapParameters();

    String mon_name[] = { "jan", "feb", "mar", "apr", "may", "jun", "jul",
            "aug", "sep", "oct", "nov", "dec" };

    public MeanMonthlyPrecip() {
        // empty constructor
    }

    public boolean read_mean_monthly_precip(String mpe_prism_dir,
            String mpe_rfc_name, int smonth, int emonth) {
        isoh = new Isoh();
        MinX = (int) MPEDataManager.getInstance().getHRAPExtent().getMinX();
        MaxX = MPEDataManager.getInstance().getHRAPExtent().width;
        MinY = (int) MPEDataManager.getInstance().getHRAPExtent().getMinY();
        MaxY = MPEDataManager.getInstance().getHRAPExtent().height;
        Unit<?> displayUnit = Unit.ONE;
        Unit<?> dataUnit = Unit.ONE;

        displayUnit = NonSI.INCH;
        dataUnit = SI.MILLIMETER;
        cmc.setDisplayUnit(displayUnit);
        cmc.setDataUnit(dataUnit);
        UnitConverter dataToImage = cmc.getDataToImageConverter();

        /*
         * Loop over the months. Determine for which months PRISM data are
         * needed.
         */
        isoh.value = new int[12][MaxY][MaxX];

        System.out
                .println(" *** MeanMonthlyPrecip: Reading Precip from PRISM data. Version "
                        + VERSION + " *** ");

        /* Read in the PRISM files. */
        /* j increments latitude i increments longitude */

        for (int k = 0; k < 12; k++) {
            int ier = is_good(k, smonth, emonth);

            if (ier == -1) {
                continue;
            }

            String mon = mon_name[k];

            /* Create the PRISM filename. */
            String pfile = DailyQcUtils.mpe_prism_dir + "/prism_mean_precip_"
                    + DailyQcUtils.mpe_rfc_name + "_" + mon;

            /* read in data file */
            /* i is longitude j is latitude */
            File pf = new File(pfile);
            if (pf.exists() != true) {
                return false;
            }
            XmrgFile xmfile = new XmrgFile(pfile);
            short[] pdata = new short[(int) xmfile.getFile().length()];
            try {
                xmfile.load();
            } catch (IOException e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Could not read PRISM information.  Check if PRISM Precip file exists.",
                                e);
            }
            if (xmfile.getHrapExtent() == null) {
                return false;
            }
            pdata = xmfile.getData();
            if (pdata.length == 0) {
                System.out.println("Error reading " + pfile);
                return false;
            }
            for (int i = MaxY - 1; i >= 0; i--) {    
                for (int j = 0; j < MaxX; j++) {
                    float f = 0;
                    short s = pdata[j + MaxX * (MaxY - i -1)];                    
                    if (s < 0) {
                        f = 0;
                    } else {
                        f = (float) dataToImage.convert(s);
                    }
                    float aa = (float) ((Math.floor((int) (f * 100))) / 100.0);
                    isoh.value[k][i][j] = (int) aa;
                }
            }

        }

        DailyQcUtils.isohyets_used = 1;
        return true;
    }

    public static class Isoh {
        Icoord coord[];

        int value[][][];

        int maxi;

        int maxj;

        float max_lat;

        float max_lon;

        float total_lat;

        float total_lon;

        float delta_lat;

        float delta_lon;
    }

    public static class maxmin {

        Icoord coord[];

        int maxvalue[][][];

        int minvalue[][][];

        int maxi;

        int maxj;

        float max_lat;

        float max_lon;

        float total_lat;

        float total_lon;

        float delta_lat;

        float delta_lon;

    }

    public class Icoord {
        int x;

        int y;
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

    public int is_good(int k, int smonth, int emonth) {
        if ((smonth <= emonth) && (k >= smonth) && (k <= emonth)) {
            return (1);
        }
        if (smonth > emonth) {
            if (k <= emonth) {
                return (1);
            }

            if (k >= smonth) {
                return (1);
            }
        }
        return (-1);
    }

}
