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

import java.awt.Rectangle;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 26, 2011            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class GenerateAccumGrids {
    AppsDefaults appsDefaults = AppsDefaults.getInstance();

    String qpe_files_dir = appsDefaults.getToken("rfcwide_xmrg_dir");

    String date_form = appsDefaults.getToken("st3_date_form");

    Date[] obsdate;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GenerateAccumGrids.class);

    private static final int SECONDS_PER_DAY = 86400;

    private static final SimpleDateFormat MMddyyyyHH = new SimpleDateFormat(
            "MMddyyyyHH");

    private static final SimpleDateFormat yyyyMMddHH = new SimpleDateFormat(
            "yyyyMMddHH");

    Hrap_Grid hrap_grid = DailyQcUtils.getHrap_grid();

    int grid_xor = hrap_grid.hrap_minx;

    int grid_yor = hrap_grid.hrap_miny;

    int grid_maxx = hrap_grid.maxi;

    int grid_maxy = hrap_grid.maxj;

    // int maxx = grid_maxx - grid_xor;

    // int maxy = grid_maxy - grid_yor;
    static {
        yyyyMMddHH.setTimeZone(TimeZone.getTimeZone("GMT"));
        MMddyyyyHH.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * @param qcDays
     */
    public void generate_accum_grids(int qcDays) {

        // Get grid coordinates
        int hr = 0;
        int hour = 0;
        int jj = 0;
        Calendar kal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        Date endTimeTemp = new Date(DailyQcUtils.btime.getTimeInMillis());
        obsdate = new Date[qcDays + 1];
        ColorMapParameters cmc = new ColorMapParameters();
        // if not empty clear them
        // if (DailyQcUtils.QPEaccum24hr != null) {
        DailyQcUtils.QPEaccum24hr = new double[qcDays][grid_maxy][grid_maxx];
        // }
        Unit<?> displayUnit = Unit.ONE;
        Unit<?> dataUnit = Unit.ONE;
        displayUnit = NonSI.INCH;
        dataUnit = SI.MILLIMETER.divide(100);
        cmc.setDisplayUnit(displayUnit);
        cmc.setDataUnit(dataUnit);
        // if not initialized, clear and initialize array
        // if (DailyQcUtils.QPEgrid1hr != null) {
        DailyQcUtils.QPEgrid1hr = new short[grid_maxy][grid_maxx];
        // }
        for (int i = 0; i < grid_maxy; i++) {
            for (int j = 0; j < grid_maxx; j++) {
                DailyQcUtils.QPEgrid1hr[i][j] = (short) DailyQcUtils.MOSAIC_DEFAULT;
            }
        }
        for (int k = 0; k < qcDays; k++) {
            for (int i = 0; i < grid_maxy; i++) {
                for (int j = 0; j < grid_maxx; j++) {
                    DailyQcUtils.QPEaccum24hr[k][i][j] = DailyQcUtils.MOSAIC_DEFAULT;
                }
            }
        }
        kal.setTime(endTimeTemp);
        for (int k = 0; k < qcDays + 1; k++) {
            kal.add(Calendar.SECOND, -SECONDS_PER_DAY * k);
            obsdate[k] = kal.getTime();
        }

        for (int j = 0; j < qcDays; j++) {
            for (hr = 0; hr < 24; hr++) {
                /*------------------------*/
                /* read QPE grid (xmrg format) */
                /* if unable to read file, return 1 */
                /* else return 0 */
                /*------------------------*/

                hour = 12 - hr;
                jj = j;
                if (hour < 0) {
                    hour = 24 + hour;
                    jj = j + 1;
                }

                if (Read1hrQPEGrid(jj, hour) == 0) {

                    /* accumulate 1hr grids */

                    for (int i = 0; i < grid_maxy; i++) {

                        for (int k = 0; k < grid_maxx; k++) {
                            if (DailyQcUtils.QPEaccum24hr[j][i][k] == DailyQcUtils.MOSAIC_DEFAULT) {
                                DailyQcUtils.QPEaccum24hr[j][i][k] = 0.0;
                            }

                            float f = 0;
                            short s = DailyQcUtils.QPEgrid1hr[i][k];
                            if (s < 0) {
                                if (s == -9999 || s == -999 || s == -99
                                        || (s == -9)) {
                                    f = (float) DailyQcUtils.MOSAIC_DEFAULT;
                                } else if (s == -8888 || s == -899) {
                                    f = s;
                                } else {
                                    f = (float) cmc.getDataToDisplayConverter()
                                            .convert(s);
                                }
                            } else {
                                if (s < 30 && s > 24) {
                                    s = 26;
                                } else if (s > 0 && s <= 24) {
                                    s = 0;
                                }
                                f = (float) cmc.getDataToDisplayConverter()
                                        .convert(s);
                            }
                            float aa = (float) ((Math.floor((int) (f * 100))) / 100.0);
                            if (DailyQcUtils.QPEaccum24hr[j][i][k] < 0
                                    && s >= 0) {
                                DailyQcUtils.QPEaccum24hr[j][i][k] = aa;
                            } else if (DailyQcUtils.QPEaccum24hr[j][i][k] >= 0
                                    && s > 0) {
                                DailyQcUtils.QPEaccum24hr[j][i][k] += aa;
                            }
                            // DailyQcUtils.QPEaccum24hr[j][i][k] =
                            // DailyQcUtils.QPEaccum24hr[j][i][k]
                            // + aa;
                        }
                    }

                } /* end if */
            }
        }
    }

    /**
     * @param jj
     * @param hour
     * @return
     */
    private int Read1hrQPEGrid(int jj, int hour) {
        /*------------------------------------------------*/
        /* function to read QPE grids for a 6hr or 24 hr period */
        /* Input: */
        /* j = day number */
        /* maxx,maxy = max hrap x/y coord */
        /* qpe_files_dir = dir containing QPE grids */
        /* date_form = value of st3_date_form token */
        /* - format of date in filename */
        /* miss = missing grid indicator */
        /* - 0 -- file read 1 -- file not read */
        /*                                                */
        /* Output: */
        /* 1) QPE grid values stored in QPEgrid1hrs array */
        /* 2) miss = missing grid flag */
        /* = 0 -- grid read properly */
        /* = 1 -- grid not read */
        /*------------------------------------------------*/

        /*----------------------------------------------*/
        /* create file name */
        /* filenames are of the form: */
        /* xmrgmmddyyyyhhz for st3_date_form = mdY */
        /* xmrgyyyymmddhhz for st3_date_form = Ymd */
        /*----------------------------------------------*/
        Calendar kal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        kal.setTime(obsdate[jj]);
        kal.set(Calendar.HOUR_OF_DAY, hour);
        String qpe_filename;
        Date ts = kal.getTime();
        if (date_form.charAt(0) == 'm') {
            qpe_filename = FileUtil.join(qpe_files_dir + "/", "xmrg"
                    + MMddyyyyHH.format(ts) + "z");

            // qpe_filename = String.format("%s/xmrg%s%s%s%02dz", qpe_files_dir,
            // MMddyyyyHH.format(ts));
        } else {
            qpe_filename = String.format("%s/xmrg%s%s%s%02dz", qpe_files_dir,
                    yyyyMMddHH.format(ts));
        }

        System.out.println("GenerateAccumGrids.Read1hrQPEGrid(): qpe_filename: " + qpe_filename);
        
        XmrgFile xmrg = new XmrgFile(qpe_filename);
        try {
            long fileLength = xmrg.getFile().length();
            if (fileLength > 0)
            {
                xmrg.load();
                Rectangle extent = xmrg.getHrapExtent();
                DailyQcUtils.QPEgrid1hr = xmrg.getData(extent);
            }
        } catch (FileNotFoundException e) {
            System.out.println("xmrg file not found: " + qpe_filename);

        } catch (IOException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return 1;
        }

        return 0;
    }
}
