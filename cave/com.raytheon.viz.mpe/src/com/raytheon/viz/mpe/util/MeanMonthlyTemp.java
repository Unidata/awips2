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

import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.viz.mpe.core.MPEDataManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    	Description
 * ------------ ---------- ----------- 	--------------------------
 * Feb 25, 2009            snaples     	Initial creation
 * Apr 16, 2012			   mgamazaychik	DR9602 - changed how max and min 
 * 										temperature data are read from PRISM  
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class MeanMonthlyTemp {
    int MaxX;

    int MaxY;

    int MinX;

    int MinY;

    ColorMapParameters cmc = new ColorMapParameters();

    private static MaxMin maxmin;

    String mon_name[] = { "jan", "feb", "mar", "apr", "may", "jun", "jul",
            "aug", "sep", "oct", "nov", "dec" };

    public MeanMonthlyTemp() {
        // empty constructor
    }

    public boolean read_mean_monthly_temp(String mpe_prism_dir,
            String mpe_rfc_name, int smonth, int emonth) {
        System.out.println("Starting mmt: ");
        if (DailyQcUtils.init_maxmin == -1) {
            maxmin = new MaxMin();
            MaxX = MPEDataManager.getInstance().getHRAPExtent().width;
            MaxY = MPEDataManager.getInstance().getHRAPExtent().height;
            MinX = (int) MPEDataManager.getInstance().getHRAPExtent().getMinX();
            MinY = (int) MPEDataManager.getInstance().getHRAPExtent().getMinY();

            maxmin.maxi = -1;
            maxmin.maxj = -1;
            maxmin.max_lat = -1;
            maxmin.max_lon = -1;
            maxmin.total_lat = -1;
            maxmin.total_lon = -1;
            maxmin.delta_lat = -1;
            maxmin.delta_lon = -1;

            maxmin.maxvalue = new int[12][MaxY][MaxX];
            maxmin.minvalue = new int[12][MaxY][MaxX];

            Unit<?> displayUnit = Unit.ONE;
            Unit<?> dataUnit = Unit.ONE;

            displayUnit = NonSI.FAHRENHEIT;
            dataUnit = NonSI.FAHRENHEIT.divide(10);
            cmc.setDisplayUnit(displayUnit);
            cmc.setDataUnit(dataUnit);

            /* Read in the PRISM files. */
            /* j increments latitude i increments longitude */

            for (int k = 0; k < 12; k++) {
                int ier = is_good(k, smonth, emonth);

                if (ier == -1) {
                    continue;
                }

                String mon = mon_name[k];

                /* Create the max temp PRISM filename. */
                String pfile = DailyQcUtils.mpe_prism_dir + "/prism_max_temp_"
                        + DailyQcUtils.mpe_rfc_name + "_" + mon;

                /* read in data file */
                /* i is longitude j is latitude */
                File pf = new File(pfile);
                if (pf.exists() != true) {
                    return false;
                }
                XmrgFile xmfile = new XmrgFile(pfile);
                try {
                    xmfile.load();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                short[] pdata = new short[(int) xmfile.getFile().length()];
                if (xmfile.getHrapExtent() == null) {
                    return false;
                }
                pdata = xmfile.getData();
                if (pdata.length == 0) {
                    System.out.println("Error reading " + pfile);
                    return false;
                }
                /*
                 * DR9602 - added to read max temps from PRISM properly
                 */
                int index=0;
                for (int i = MaxY - 1; i >= 0; i--) {
                    for (int j = 0; j < MaxX; j++) {
                        float f = 0;                        
                        short s = pdata[index];
                        index++;
                        if (s < 0) {
                            if (s == -9999 || s == -999) {
                                f = s;
                            } else if (s == -8888 || s == -899) {
                                f = s;
                            } else {
                                f = (float) cmc.getDataToDisplayConverter()
                                        .convert(s);
                            }
                        } else {
                            f = (float) cmc.getDataToDisplayConverter()
                                    .convert(s);
                        }
                        float aa = (float) (Math.floor((f * 10)));
                        int bb = (int) aa;
                        maxmin.maxvalue[k][i][j] = bb;
                    }
                }
            }
            for (int k = 0; k < 12; k++) {
                int ier = is_good(k, smonth, emonth);

                if (ier == -1) {
                    continue;
                }

                String mon = mon_name[k];

                /* Create the min temp PRISM filename. */
                String pfile = DailyQcUtils.mpe_prism_dir + "/prism_min_temp_"
                        + DailyQcUtils.mpe_rfc_name + "_" + mon;

                /* read in data file */
                /* i is longitude j is latitude */
                File pf = new File(pfile);
                if (pf.exists() != true) {
                    return false;
                }
                XmrgFile xmfile = new XmrgFile(pfile);
                short[] pdata2 = new short[(int) xmfile.getFile().length()];
                try {
                    xmfile.load();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                if (xmfile.getHrapExtent() == null) {
                    return false;
                }
                pdata2 = xmfile.getData();
                if (pdata2.length == 0) {
                    System.out.println("Error reading " + pfile);
                    return false;
                }
                /*
                 * DR9602 - added to read min temps from PRISM properly
                 */
                int index=0;
                for (int i = MaxY - 1; i >= 0; i--) {
                    for (int j = 0; j < MaxX; j++) {
                        float f = 0;                        
                        short s = pdata2[index];
                        index++;
                        if (s < 0) {
                            if (s == -9999 || s == -999) {
                                f = s;
                            } else if (s == -8888 || s == -899) {
                                f = s;
                            } else {
                                f = (float) cmc.getDataToDisplayConverter()
                                        .convert(s);
                            }
                        } else {
                            f = (float) cmc.getDataToDisplayConverter()
                                    .convert(s);
                        }
                        float aa = (float) (Math.floor((f * 10)));
                        int bb = (int) aa;
                        maxmin.minvalue[k][i][j] = bb;
                    }
                }
            }

            DailyQcUtils.init_maxmin = 1;
        }
        DailyQcUtils.maxmin_used = 1;
        System.out.println("Finished mmt: ");
        return true;
    }

    public static class MaxMin {

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
