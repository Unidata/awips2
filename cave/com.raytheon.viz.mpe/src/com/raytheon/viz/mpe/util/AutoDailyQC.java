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
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
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
 * Jul 07, 2011            snaples     Initial creation
 * May 02, 2013 15956      wkwock      Fix incorrect contents in precip_LLL_grid_yyyymmdd.nc file
 * May 21, 2018  7131      mduff       Updated to accommodate changes to other classes.
 * 
 * </pre>
 * 
 * @author snaples
 */

public class AutoDailyQC {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AutoDailyQC.class);

    // initialize variables
    private static final int SIX_HOURS_OF_SECONDS = TimeUtil.SECONDS_PER_HOUR
            * 6;

    private static final int TWELVE_HOURS_OF_SECONDS = TimeUtil.SECONDS_PER_HOUR
            * 12;

    private static final int EIGHTTEEN_HOURS_OF_SECONDS = TimeUtil.SECONDS_PER_HOUR
            * 18;

    private Calendar current_hydrologic_time;

    private Calendar end_hydrologic_time;

    private boolean precip_flag = true;

    private boolean temperature_flag = false;

    private boolean freezingl_flag = false;

    private boolean PP_flag = true;

    private int numDays = 1;

    private static final String GMT = "GMT";

    private static SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd");

    private String db_name;

    private String mpe_site_id;

    private DailyQcUtils dqcu = DailyQcUtils.getInstance();

    private GridAttributes ga;

    static {
        sdf.setTimeZone(TimeZone.getTimeZone(GMT));
    }

    public AutoDailyQC() {

    }

    public void runAutoDailyQC(String[] args) throws Exception {

        Date current_time = new Date();
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(current_time);
        current_hydrologic_time = Calendar
                .getInstance(TimeZone.getTimeZone("GMT"));
        current_hydrologic_time.setTime(current_time);
        current_hydrologic_time.add(Calendar.SECOND, SIX_HOURS_OF_SECONDS);
        end_hydrologic_time = current_hydrologic_time;
        ga = new GridAttributes();

        parseArgs(args);

        statusHandler
                .info("STATUS: Start running autoDailyQC -- " + cal.getTime());

        int curHrMinSec = DailyQcUtils
                .getCurrentHrMinSec(end_hydrologic_time.getTime());

        if (curHrMinSec != -1) {
            if (curHrMinSec >= 0 && curHrMinSec < SIX_HOURS_OF_SECONDS) {
                dqcu.curHr00_06 = 1;
            } else if (curHrMinSec >= SIX_HOURS_OF_SECONDS
                    && curHrMinSec < TWELVE_HOURS_OF_SECONDS) {
                dqcu.curHr06_12 = 1;
            } else if (curHrMinSec >= TWELVE_HOURS_OF_SECONDS
                    && curHrMinSec < EIGHTTEEN_HOURS_OF_SECONDS) {
                dqcu.curHr12_18 = 1;
            } else {
                dqcu.curHr18_00 = 1;
            }
        } else {
            dqcu.curHr00_06 = -1;
            dqcu.curHr06_12 = -1;
            dqcu.curHr12_18 = -1;
            dqcu.curHr18_00 = -1;
        }

        loadAutoQcTokens();

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.debug(String.format("\tDatabase: %s\n", db_name));
            statusHandler
                    .debug(String.format("\tmpe_site_id: %s\n", mpe_site_id));
            statusHandler.debug(String
                    .format("\tThe number of running days: %d\n", numDays));
            statusHandler.debug(
                    String.format("\tThe end hydrologic date is: %d-%d-%d\n",
                            end_hydrologic_time.get(Calendar.YEAR),
                            end_hydrologic_time.get(Calendar.MONTH) + 1,
                            end_hydrologic_time.get(Calendar.DAY_OF_MONTH)));
        }
        String datatype_str = "";
        if (precip_flag) {
            datatype_str = "PP";
        }
        if (temperature_flag) {
            datatype_str = "TA";
        }
        if (freezingl_flag) {
            datatype_str = "FZ";
        }
        statusHandler.debug(
                String.format("\tThe data type mode: %s\n", datatype_str));

        /*
         * re-use MPE/DailyQC to define structures and load point level2 file
         * for whole domain, this data will be kept in pdata, zdata and tdata
         * structures
         */
        dqcu.qcDataHasChanged(end_hydrologic_time.getTime(),
                end_hydrologic_time.getTime(), mpe_site_id, numDays, true);

        /*
         * if run DQC at the time frames such as curHr18_00 or curHr00_06 or
         * curHr06_12, for precipitation, do not display the 24 hr precipiation
         * if the pcpn_day=0
         */
        /*
         * 0 represents the time frame 12 - 18, 1 represents time frame 18-00, 2
         * represents time frame 00-06Z, 3 represents time frame 06-12z
         */
        /*
         * if (precip_flag == true) { if (DailyQcUtils.curHr18_00 == 1) {
         * DailyQcUtils.pdata[0].used[1] = 0; DailyQcUtils.pdata[0].used[2] = 0;
         * DailyQcUtils.pdata[0].used[3] = 0; DailyQcUtils.pdata[0].used[4] = 0;
         * } else if (DailyQcUtils.curHr00_06 == 1) {
         * DailyQcUtils.pdata[0].used[2] = 0; DailyQcUtils.pdata[0].used[3] = 0;
         * DailyQcUtils.pdata[0].used[4] = 0; } else if (DailyQcUtils.curHr06_12
         * == 1) { DailyQcUtils.pdata[0].used[3] = 0;
         * DailyQcUtils.pdata[0].used[4] = 0; } } else
         */ if (freezingl_flag) {
            if (dqcu.curHr18_00 == 1) {
                DailyQcUtils.zdata[0].used[1] = 0;
                DailyQcUtils.zdata[0].used[2] = 0;
                DailyQcUtils.zdata[0].used[3] = 0;
            } else if (dqcu.curHr00_06 == 1) {
                DailyQcUtils.zdata[0].used[2] = 0;
                DailyQcUtils.zdata[0].used[3] = 0;
            } else if (dqcu.curHr06_12 == 1) {
                DailyQcUtils.zdata[0].used[3] = 0;
            }

        } else if (temperature_flag) {

            if (dqcu.curHr18_00 == 1) {
                DailyQcUtils.tdata[0].used[1] = 0;
                DailyQcUtils.tdata[0].used[2] = 0;
                DailyQcUtils.tdata[0].used[3] = 0;

            } else if (dqcu.curHr00_06 == 1) {
                DailyQcUtils.tdata[0].used[2] = 0;
                DailyQcUtils.tdata[0].used[3] = 0;

            } else if (dqcu.curHr06_12 == 1) {
                DailyQcUtils.tdata[0].used[3] = 0;

            }

        }

        /*
         * render grids and output
         */
        autoDailyQCRenderGrids();
        return;
    }

    /**
     * autodailyqc_render_grids()
     * 
     * Generate grids for precipitation, temperature and freezing level used in
     * auto_dailyqc, output the grids.
     */
    private void autoDailyQCRenderGrids() {
        Hrap_Grid hrap_grid = DailyQcUtils.getHrap_grid();
        int[] precip_level2_flag = new int[numDays];
        int[] temperature_level2_flag = new int[numDays];
        int[] freezingl_level2_flag = new int[numDays];
        Calendar otime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        String fname_nc = "";
        int num_period_qc = 0;
        float[][] datavals = new float[hrap_grid.maxi][hrap_grid.maxj];
        if (dqcu.mpe_dqc_save_grib || dqcu.mpe_dqc_save_netcdf) {
            for (int y = 0; y < hrap_grid.maxj; y++) {
                for (int x = 0; x < hrap_grid.maxi; x++) {
                    datavals[x][y] = (DailyQcUtils.pcp.value[x][y] / 100.f);
                }
            }
        }

        statusHandler.debug("\nStart to generate grids. ");

        for (int i = 0; i < numDays; i++) {
            if (precip_flag) {
                /* check if the level2 point file exist */
                otime.setTime(DailyQcUtils.pdata[i].data_time);

                String precip_level2_file = String.format("%s%04d%02d%02d",
                        dqcu.proc_pcpn_file, otime.get(Calendar.YEAR),
                        otime.get(Calendar.MONTH) + 1,
                        otime.get(Calendar.DAY_OF_MONTH));
                File pl2 = new File(precip_level2_file);

                // dr16046:file not exist then don't generate
                if (!pl2.exists()) {
                    precip_level2_flag[i] = 0;
                    statusHandler.debug(String.format(
                            "ERROR: Do not generate grids and MAP for hydrologic date %04d%02d%02d since %s does not exist.\n",
                            otime.get(Calendar.YEAR),
                            otime.get(Calendar.MONTH) + 1,
                            otime.get(Calendar.DAY_OF_MONTH),
                            precip_level2_file));

                } else {
                    precip_level2_flag[i] = 1;
                    if (dqcu.mpe_dqc_save_netcdf || dqcu.mpe_dqc_save_grib) {
                        /* create name of netCDF file */
                        fname_nc = String.format("%s%04d%02d%02d.nc",
                                dqcu.grid_file, otime.get(Calendar.YEAR),
                                otime.get(Calendar.MONTH) + 1,
                                otime.get(Calendar.DAY_OF_MONTH));

                        /* define attributes and store in structure */
                        num_period_qc = ga.define_grid_attributes(1, i,
                                num_period_qc);
                    }

                    if (dqcu.mpe_dqc_save_grib) {

                        if (!dqcu.mpe_dqc_save_netcdf) {
                            num_period_qc = ga.define_grid_attributes(1, i,
                                    num_period_qc);
                        }
                    }

                    num_period_qc = 5;
                    // for the 6 hours periods
                    for (int l = 0; l < 5; l++) {
                        if (DailyQcUtils.pdata[i].used[l] == 0) {
                            continue;
                        }

                        if (l < 2) {
                            otime.setTime(DailyQcUtils.pdata[i].data_time);
                            otime.add(Calendar.SECOND, -86_400);
                        } else {
                            otime.setTime(DailyQcUtils.pdata[i].data_time);
                        }

                        int ll;
                        if (l < 4) {
                            ll = 0;
                        } else {
                            ll = 1;
                        }
                        RenderPcp rp = new RenderPcp();
                        rp.render_pcp(i, l, ll,
                                DailyQcUtils.precip_stations.size(),
                                DailyQcUtils.precip_stations,
                                DailyQcUtils.getHrap_grid(), DailyQcUtils.pdata,
                                DailyQcUtils.pcp_in_use);

                        /* output grid to file in Ascii format */
                        String dbuf = String.format("%s%s_%04d%02d%02d",
                                dqcu.grid_file, DailyQcUtils.TIME_FILE[2][l],
                                otime.get(Calendar.YEAR),
                                otime.get(Calendar.MONTH) + 1,
                                otime.get(Calendar.DAY_OF_MONTH));
                        WriteQPFGrids wq = new WriteQPFGrids();
                        wq.write_qpf_grids(dbuf);

                        // copy data from DailyQcUtils.pcp.value to datavals
                        for (int y = 0; y < hrap_grid.maxj; y++) {
                            for (int x = 0; x < hrap_grid.maxi; x++) {
                                datavals[x][y] = (DailyQcUtils.pcp.value[x][y]
                                        / 100.f);
                            }
                        }

                        /* output grid to file in grib format */

                        // create netCDF file from data, write it out then call
                        // nc2grib against it making a grib file, when done
                        // remove the unneeded netCDF file.
                        if (dqcu.mpe_dqc_save_grib) {
                            WriteDQCNetCDFGrids wng = new WriteDQCNetCDFGrids();
                            String ncfile = String.format("%s.nc", dbuf);
                            wng.write_dqc_netcdf_grids(ncfile, 0, 1, 1,
                                    ga.getCommonGridAttributes(), datavals);
                            WriteDQCGribGrids wgg = new WriteDQCGribGrids();
                            String fname_grib = String.format("%s.grb", dbuf);
                            int status = wgg.write_dqc_grib_grids(ncfile,
                                    fname_grib, 1);
                            File nfile = new File(ncfile);
                            nfile.delete();
                            nfile = null;
                            if (status != 0) {
                                statusHandler.warn(String.format(
                                        "\n problem with writing GRIB file in write_dqc_grib_grids. status=%d\n",
                                        status));
                            }

                        }

                        int num;
                        if (l < 4) {
                            num = i * 4 + 3 - l;
                        } else {
                            num = i + 40;
                        }

                        /* create the MAP */
                        CreateMap cm = new CreateMap();
                        cm.create_map(num);
                    }

                    // Here's the 12 to 12 whole day nc file
                    // for (int l = 0; l < 5; l++) {
                    for (int l = 0; l < num_period_qc; l++) {
                        if (DailyQcUtils.pdata[i].used[l] == 0) {
                            continue;
                        }

                        if (l < 2) {
                            otime.setTime(DailyQcUtils.pdata[i].data_time);
                            otime.add(Calendar.SECOND, -86_400);
                        } else {
                            otime.setTime(DailyQcUtils.pdata[i].data_time);
                        }

                        int ll;
                        if (l < 4) {
                            ll = 0;
                        } else {
                            ll = 1;
                        }
                        RenderPcp rp = new RenderPcp();
                        rp.render_pcp(i, l, ll,
                                DailyQcUtils.precip_stations.size(),
                                DailyQcUtils.precip_stations,
                                DailyQcUtils.getHrap_grid(), DailyQcUtils.pdata,
                                DailyQcUtils.pcp_in_use);

                        // copy data from DailyQcUtils.pcp.value to datavals
                        for (int y = 0; y < hrap_grid.maxj; y++) {
                            for (int x = 0; x < hrap_grid.maxi; x++) {
                                datavals[x][y] = (DailyQcUtils.pcp.value[x][y]
                                        / 100.f);
                            }
                        }

                        /* output grid to file in Ascii format */
                        String dbuf = String.format("%s%s_%04d%02d%02d",
                                dqcu.grid_file, DailyQcUtils.TIME_FILE[2][l],
                                otime.get(Calendar.YEAR),
                                otime.get(Calendar.MONTH) + 1,
                                otime.get(Calendar.DAY_OF_MONTH));
                        WriteQPFGrids wq = new WriteQPFGrids();
                        wq.write_qpf_grids(dbuf);

                        /* output grid to file in NetCDF format */
                        if (dqcu.mpe_dqc_save_netcdf) {
                            WriteDQCNetCDFGrids wng = new WriteDQCNetCDFGrids();
                            wng.write_dqc_netcdf_grids(fname_nc, l,
                                    num_period_qc, 1,
                                    ga.getCommonGridAttributes(), datavals);
                        }

                    }
                }

            }
            if (temperature_flag) {
                /* check if the level2 point file exist */
                otime.setTime(DailyQcUtils.tdata[i].data_time);

                String temperature_level2_file = String.format("%s%04d%02d%02d",
                        dqcu.tpoint2_file, otime.get(Calendar.YEAR),
                        otime.get(Calendar.MONTH) + 1,
                        otime.get(Calendar.DAY_OF_MONTH));
                File pl2 = new File(temperature_level2_file);

                if (pl2.exists()) {
                    precip_level2_flag[i] = 0;
                    statusHandler.error(String.format(
                            "ERROR: Do not generate grids and MAT for hydrologic date %04d%02d%02d since %s does not exist.\n",
                            otime.get(Calendar.YEAR),
                            otime.get(Calendar.MONTH) + 1,
                            otime.get(Calendar.DAY_OF_MONTH),
                            temperature_level2_file));

                } else {
                    temperature_level2_flag[i] = 1;

                    if (dqcu.mpe_dqc_save_netcdf) {
                        /* create name of netCDF file */
                        fname_nc = String.format("%s%04d%02d%02d.nc",
                                dqcu.tgrid_file, otime.get(Calendar.YEAR),
                                otime.get(Calendar.MONTH) + 1,
                                otime.get(Calendar.DAY_OF_MONTH));
                        statusHandler.debug(String
                                .format("netcdf file name = %s\n", fname_nc));

                        /* define attributes and store in structure */
                        num_period_qc = ga.define_grid_attributes(2, i,
                                num_period_qc);
                    }

                    // if (DailyQcUtils.mpe_dqc_save_grib == true) {
                    if (true) {
                        if (!dqcu.mpe_dqc_save_netcdf) {
                            ga.define_grid_attributes(2, i, num_period_qc);
                        }
                    }

                    /*
                     * need to calculate the .a parameter in the tdata structure
                     */
                    /*
                     * this parameter is calculated in the
                     * estimate_daily_tstations routine
                     */

                    EstDailyTStations edt = new EstDailyTStations();
                    edt.estimate_daily_tstations(i,
                            DailyQcUtils.temperature_stations,
                            DailyQcUtils.temperature_stations.size());

                    /*
                     * loop on 6 time periods (max, min, four 6hr time periods)
                     */

                    for (int l = 5; l >= 0; l--) {
                        if (DailyQcUtils.tdata[i].used[l] == 0) {
                            continue;
                        }

                        if (l < 1) {
                            otime.setTime(DailyQcUtils.pdata[i].data_time);
                            otime.add(Calendar.SECOND, -86_400);
                        } else {
                            otime.setTime(DailyQcUtils.pdata[i].data_time);
                        }

                        statusHandler.debug(
                                String.format("Temperature %02d-%02d-%02d\n",
                                        otime.get(Calendar.MONTH) + 1,
                                        otime.get(Calendar.DAY_OF_MONTH),
                                        otime.get(Calendar.YEAR)));

                        if (l == 5) {
                            RenderT rt = new RenderT();

                            rt.render_t(i, l, 2,
                                    DailyQcUtils.temperature_stations.size(),
                                    DailyQcUtils.temperature_stations,
                                    DailyQcUtils.getHrap_grid(),
                                    DailyQcUtils.tdata,
                                    DailyQcUtils.pcp_in_use);
                        } else if (l == 4) {
                            RenderT rt = new RenderT();
                            rt.render_t(i, l, 1,
                                    DailyQcUtils.temperature_stations.size(),
                                    DailyQcUtils.temperature_stations,
                                    DailyQcUtils.getHrap_grid(),
                                    DailyQcUtils.tdata,
                                    DailyQcUtils.pcp_in_use);
                        } else {
                            statusHandler.debug(String.format(
                                    "i = %d l = %d max_tstations = %d\n", i, l,
                                    DailyQcUtils.temperature_stations.size()));
                            RenderT6 rt6 = new RenderT6();
                            rt6.render_t6(i, l, 0,
                                    DailyQcUtils.temperature_stations.size(),
                                    DailyQcUtils.temperature_stations,
                                    DailyQcUtils.getHrap_grid(),
                                    DailyQcUtils.tdata,
                                    DailyQcUtils.pcp_in_use);
                        }

                        /* output grid to file in Ascii format */

                        String dbuf = String.format("%s%s_%04d%02d%02d",
                                dqcu.tgrid_file,
                                DailyQcUtils.T_TIME_FILE[dqcu.dqcTimeStringIndex][l],
                                otime.get(Calendar.YEAR),
                                otime.get(Calendar.MONTH) + 1,
                                otime.get(Calendar.DAY_OF_MONTH));

                        WriteQPFGrids wq = new WriteQPFGrids();
                        wq.write_qpf_grids(dbuf);

                        /* output grid to file in netCDF format */
                        if (dqcu.mpe_dqc_save_netcdf) {
                            WriteDQCNetCDFGrids wcg = new WriteDQCNetCDFGrids();
                            wcg.write_dqc_netcdf_grids(fname_nc, l,
                                    num_period_qc, 2,
                                    ga.getCommonGridAttributes(), datavals);
                        }

                        /* output grid to file in grib format */
                        if (dqcu.mpe_dqc_save_grib) {
                            WriteDQCNetCDFGrids wng = new WriteDQCNetCDFGrids();
                            String ncfile = String.format("%s.nc", dbuf);
                            wng.write_dqc_netcdf_grids(ncfile, 0, 1, 2,
                                    ga.getCommonGridAttributes(), datavals);
                            WriteDQCGribGrids wgg = new WriteDQCGribGrids();
                            String fname_grib = String.format("%s.grb", dbuf);
                            int status = wgg.write_dqc_grib_grids(ncfile,
                                    fname_grib, 2);
                            File nfile = new File(ncfile);
                            nfile.delete();
                            nfile = null;
                            if (status != 0) {
                                statusHandler.warn(String.format(
                                        "\n problem with writing GRIB file in write_dqc_grib_grids. status=%d\n",
                                        status));
                            }

                        }

                        int num = 150 + i * 4 + 3 - l;

                        if (l < 4) {
                            MakeMat mmt = new MakeMat();
                            mmt.make_mat(num, num - 150);
                        }
                    }

                }
            }

            if (freezingl_flag) {
                /* check if the level2 point file exist */
                otime.setTime(DailyQcUtils.tdata[i].data_time);

                String freezingl_level2_file = String.format("%s%04d%02d%02d",
                        dqcu.zpoint2_file, otime.get(Calendar.YEAR),
                        otime.get(Calendar.MONTH) + 1,
                        otime.get(Calendar.DAY_OF_MONTH));
                File pl2 = new File(freezingl_level2_file);

                if (pl2.exists()) {
                    precip_level2_flag[i] = 0;
                    statusHandler.error(String.format(
                            "ERROR: Do not generate grids and MAZ for hydrologic date %04d%02d%02d since %s does not exist.\n",
                            otime.get(Calendar.YEAR),
                            otime.get(Calendar.MONTH) + 1,
                            otime.get(Calendar.DAY_OF_MONTH),
                            freezingl_level2_file));

                } else {
                    freezingl_level2_flag[i] = 1;

                    if (dqcu.mpe_dqc_save_netcdf) {
                        /* create name of netCDF file */
                        fname_nc = String.format("%s%04d%02d%02d.nc",
                                dqcu.zgrid_file, otime.get(Calendar.YEAR),
                                otime.get(Calendar.MONTH) + 1,
                                otime.get(Calendar.DAY_OF_MONTH));

                        statusHandler.debug(
                                String.format("netcdf file = %s\n", fname_nc));

                        /* define attributes and store in structure */
                        num_period_qc = ga.define_grid_attributes(3, i,
                                num_period_qc);

                    }
                    if (dqcu.mpe_dqc_save_grib) {
                        if (!dqcu.mpe_dqc_save_netcdf) {
                            ga.define_grid_attributes(3, i, num_period_qc);
                        }
                    }

                    for (int l = 0; l < 4; l++) {
                        if (DailyQcUtils.zdata[i].used[l] == 0) {
                            continue;
                        }

                        if (l < 2) {
                            otime.setTime(DailyQcUtils.pdata[i].data_time);
                            otime.add(Calendar.SECOND, -86_400);
                        } else {
                            otime.setTime(DailyQcUtils.pdata[i].data_time);
                        }

                        statusHandler.debug(
                                String.format("Freezing level %02d-%02d-%02d\n",
                                        otime.get(Calendar.MONTH) + 1,
                                        otime.get(Calendar.DAY_OF_MONTH),
                                        otime.get(Calendar.YEAR)));

                        RenderZ rz = new RenderZ();
                        rz.render_z(i, l, 0,
                                DailyQcUtils.freezing_stations.size(),
                                DailyQcUtils.freezing_stations,
                                DailyQcUtils.getHrap_grid(), DailyQcUtils.zdata,
                                DailyQcUtils.pcp_in_use);

                        /* output grid to file in Ascii format */

                        String dbuf = String.format("%s%s_%04d%02d%02d",
                                dqcu.zgrid_file,
                                DailyQcUtils.Z_TIME_FILE[dqcu.dqcTimeStringIndex][l],
                                otime.get(Calendar.YEAR),
                                otime.get(Calendar.MONTH) + 1,
                                otime.get(Calendar.DAY_OF_MONTH));

                        WriteQPFGrids wq = new WriteQPFGrids();
                        wq.write_qpf_grids(dbuf);

                        /* output grid to file in netCDF format */
                        if (l + 1 <= num_period_qc) {
                            if (dqcu.mpe_dqc_save_netcdf) {
                                WriteDQCNetCDFGrids wc = new WriteDQCNetCDFGrids();
                                wc.write_dqc_netcdf_grids(fname_nc, l,
                                        num_period_qc, 3,
                                        ga.getCommonGridAttributes(), datavals);
                            }
                        }

                        /* output grid to file in grib format */

                        if (dqcu.mpe_dqc_save_grib) {
                            WriteDQCGribGrids wg = new WriteDQCGribGrids();
                            WriteDQCNetCDFGrids wng = new WriteDQCNetCDFGrids();
                            String ncfile = String.format("%s.nc", dbuf);
                            wng.write_dqc_netcdf_grids(ncfile, 0, 1, 3,
                                    ga.getCommonGridAttributes(), datavals);
                            String fname_grib = String.format("%s.grb", dbuf);
                            int status = wg.write_dqc_grib_grids(ncfile,
                                    fname_grib, 3);
                            File nfile = new File(ncfile);
                            nfile.delete();
                            nfile = null;
                            if (status != 0) {
                                statusHandler.error(String.format(
                                        "\n problem with writing GRIB file in write_dqc_grib_grids. status=%d\n",
                                        status));
                            }

                        }

                        int num = 100 + i * 4 + 3 - l;
                        MakeRsel mr = new MakeRsel();
                        mr.make_rsel(num, num - 100);
                    }
                }
            }
        }
    }

    /**
     * Parses command line arguments
     * 
     * @param args
     *            [] String array of arguments to pass to program
     */
    private void parseArgs(String[] args) {

        /* Create the Options object */
        Options opts = new Options();

        /* Add options to the container */
        opts.addOption("d", true, "number of days");
        opts.addOption("t", true, "ending hydrologic date (YYYYMMDD)");
        opts.addOption("m", true, "data type[PP TA FZ...]");

        try {
            PosixParser parser = new PosixParser();
            CommandLine line = parser.parse(opts, args);

            int cliOpts = line.getOptions().length;
            if (cliOpts == 0) {
                // automatically generate the help statement
                HelpFormatter formatter = new HelpFormatter();
                formatter.printHelp("autodqc ", opts);
                statusHandler.warn("WARNING: Using default values. \n"
                        + " where default values are: \n"
                        + " number of days: 1\n "
                        + " ending hydrologic date (YYYYMMDD):  current hydrologic day\n"
                        + " data type: PP\n");
                numDays = 1;
                end_hydrologic_time = current_hydrologic_time;
                precip_flag = true;
                return;
            }
            if (line.hasOption("d")) {
                numDays = Integer.parseInt(line.getOptionValue("d").trim());
            } else {
                numDays = 1;
                statusHandler
                        .warn("WARNING: Use 1 as default number of days.\n");
            }
            if (line.hasOption("t")) {
                try {
                    end_hydrologic_time.setTime(
                            sdf.parse(line.getOptionValue("t").trim()));
                } catch (ParseException e) {
                    statusHandler.error(
                            "Error Parsing Hydro date, format is YYYYMMDD \n");
                    statusHandler
                            .error("Using default -- Current Hydrologic Day");
                    end_hydrologic_time = current_hydrologic_time;
                }
            } else {
                end_hydrologic_time = current_hydrologic_time;
                statusHandler.warn(
                        "WARNING: Use current hydrologic day as default.\n");
            }
            if (line.hasOption("m")) {
                if (line.getOptionValue("m").trim().equals("PP")) {
                    PP_flag = true;
                } else if (line.getOptionValue("m").trim().equals("TA")) {
                    temperature_flag = true;
                } else if (line.getOptionValue("m").trim().equals("FZ")) {
                    freezingl_flag = true;
                }
                if (PP_flag && (temperature_flag || freezingl_flag)) {
                    precip_flag = false;
                } else {
                    precip_flag = true;
                }
            } else {
                precip_flag = true;
                statusHandler.warn("WARNING: Use PP as default data type.\n");
            }

        } catch (org.apache.commons.cli.ParseException e) {
            statusHandler.error("Error parsing data.", e);
        }
    }

    /**
     * 
     */
    private void loadAutoQcTokens() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        db_name = appsDefaults.getToken("db_name");
        mpe_site_id = appsDefaults.getToken("mpe_site_id");
    }

}
