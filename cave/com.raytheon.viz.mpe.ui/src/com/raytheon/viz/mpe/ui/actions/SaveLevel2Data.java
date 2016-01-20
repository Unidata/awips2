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
package com.raytheon.viz.mpe.ui.actions;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.RunProcess;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.QcFreezeOptionsDialog;
import com.raytheon.viz.mpe.ui.dialogs.QcPrecipOptionsDialog;
import com.raytheon.viz.mpe.ui.dialogs.QcTempOptionsDialog;
import com.raytheon.viz.mpe.ui.dialogs.SendtoDatabaseDialog;
import com.raytheon.viz.mpe.util.BadTValues;
import com.raytheon.viz.mpe.util.BadValues;
import com.raytheon.viz.mpe.util.CheckConsistency;
import com.raytheon.viz.mpe.util.CreateMap;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.raytheon.viz.mpe.util.DailyQcUtils.Maps;
import com.raytheon.viz.mpe.util.Disagg6Hr;
import com.raytheon.viz.mpe.util.EstDailyStations;
import com.raytheon.viz.mpe.util.EstDailyTStations;
import com.raytheon.viz.mpe.util.EstMissingStations;
import com.raytheon.viz.mpe.util.EstMissingTStations;
import com.raytheon.viz.mpe.util.EstPartStations;
import com.raytheon.viz.mpe.util.GridAttributes;
import com.raytheon.viz.mpe.util.MakeMat;
import com.raytheon.viz.mpe.util.MakeRsel;
import com.raytheon.viz.mpe.util.QCStations;
import com.raytheon.viz.mpe.util.QCTStations;
import com.raytheon.viz.mpe.util.RenderPcp;
import com.raytheon.viz.mpe.util.RenderT;
import com.raytheon.viz.mpe.util.RenderT6;
import com.raytheon.viz.mpe.util.RenderZ;
import com.raytheon.viz.mpe.util.WriteDQCGribGrids;
import com.raytheon.viz.mpe.util.WriteDQCNetCDFGrids;
import com.raytheon.viz.mpe.util.WriteQPFGrids;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 24, 2009            snaples     Initial creation
 * Sep 19, 2011 10955      rferrel     Use RunProcess
 * Nov 06, 2012 15481      wkwock      Fix save 6 hours precipitation files
 * May 02, 2013 15956      wkwock      Fix incorrect contents in precip_LLL_grid_yyyymmdd.nc file
 * Mar 10, 2015 14575      snaples     Added status check to make sure that we close everything before exiting.
 * Jun 25, 2015 17462      snaples     Fixed loop of basins for temp and freezing.
 * Jan 15, 2016 5054       randerso    Added proper constructor with parent shell parameter
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class SaveLevel2Data {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SaveLevel2Data.class);

    static final int MAX_GAGEQC_DAYS = 10;

    public static final int NUM_QCTYPE = 3;

    private static final String ON = "ON";

    MPEDisplayManager mpd = MPEDisplayManager.getCurrent();

    AppsDefaults apps_defaults = AppsDefaults.getInstance();

    private Date currntDate = null;

    private String qarea = "";

    private int qdays = 0;

    public static int[] qctype_flag = new int[NUM_QCTYPE];

    private final Shell shell;

    DailyQcUtils dqc = DailyQcUtils.getInstance();

    /**
     * @param shell
     *            parent shell for dialogs
     */
    public SaveLevel2Data(Shell shell) {
        this.shell = shell;
    }

    public int check_new_area(Date curDate, String area, int days) {
        int j, m;
        currntDate = curDate;
        qarea = area;
        qdays = days;

        if (dqc.pdata == null || dqc.pdata.length < 10) {
            return 0;
        }
        /* Check if any of the GageQC datasets have not been QC'd. */
        /* Check the precipitation datasets. */
        for (j = 0; j < MAX_GAGEQC_DAYS; j++) {
            for (m = 0; m < 5; m++) {
                /* not databased */

                if (dqc.pdata[j] == null) {
                    return 0;
                }
                if (dqc.pdata[j].used[m] != 1 || dqc.pdata[j].level != 1) {
                    continue;
                }

                /*
                 * Found a dataset which has not been QC'd. List all of the
                 * datasets that have not been QC'd and ask user if he wants to
                 * continue.
                 */
                check_gageqc_status();
                return 2;

            }
        }

        /* Check the freezing level datasets. */
        for (j = 0; j < MAX_GAGEQC_DAYS; j++) {
            for (m = 0; m < 5; m++) {
                /* not databased */
                if (dqc.zdata[j].used[m] != 1 || dqc.zdata[j].level[m] != 1) {
                    continue;
                }

                check_gageqc_status();
                return 2;

            }
        }

        /* Check the temperature datasets. */
        for (j = 0; j < MAX_GAGEQC_DAYS; j++) {
            for (m = 0; m < 5; m++) {
                /* not databased */
                if (dqc.tdata[j].used[m] != 1 || dqc.tdata[j].level[m] != 1) {
                    continue;
                }

                check_gageqc_status();
                return 2;

            }
        }

        /* Each of the GageQC datasets has been QC'd. */
        /* Check if any of the datasets need to be saved to the database. */
        check_saved_datasets();
        return 2;
    }

    public void check_gageqc_status() {

        StringBuilder ttext = new StringBuilder();
        String tbuf = "";
        int j, m;
        Calendar ltime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

        for (j = 0; j < MAX_GAGEQC_DAYS; j++) {

            /*
             * if processing level a data is not available do not write out
             * processing level b data. In addition, do not write MAPs if no
             * processing level a data
             */

            for (m = 0; m < 5; m++) {

                if (dqc.pdata[j].used[m] != 1 || dqc.pdata[j].level != 1) {
                    continue;
                }

                ltime.setTime(dqc.pdata[j].data_time);

                tbuf = String.format("Precipitation %02d-%02d-%02d\n",
                        ltime.get(Calendar.MONTH) + 1,
                        ltime.get(Calendar.DAY_OF_MONTH),
                        ltime.get(Calendar.YEAR));

                ttext.append(tbuf);

                break;

            }

        }

        for (j = 0; j < MAX_GAGEQC_DAYS; j++) {

            /*
             * if processing level a data is not available do not write out
             * processing level b data. In addition, do not write MAPs if no
             * processing level a data
             */

            for (m = 0; m < 5; m++) {

                if (dqc.zdata[j].used[m] != 1 || dqc.zdata[j].level[m] != 1) {
                    continue;
                }

                ltime.setTime(dqc.pdata[j].data_time);

                tbuf = String.format("Freezing level %02d-%02d-%02d\n",
                        ltime.get(Calendar.MONTH) + 1,
                        ltime.get(Calendar.DAY_OF_MONTH),
                        ltime.get(Calendar.YEAR));

                ttext.append(tbuf);
                break;

            }

        }

        for (j = 0; j < MAX_GAGEQC_DAYS; j++) {

            /*
             * if processing level a data is not available do not write out
             * processing level b data. In addition, do not write MAPs if no
             * processing level a data
             */

            for (m = 0; m < 6; m++) {

                if (dqc.tdata[j].used[m] != 1 || dqc.tdata[j].level[m] != 1) {
                    continue;
                }

                ltime.setTime(dqc.pdata[j].data_time);

                tbuf = String.format("Temperature %02d-%02d-%02d\n",
                        ltime.get(Calendar.MONTH) + 1,
                        ltime.get(Calendar.DAY_OF_MONTH),
                        ltime.get(Calendar.YEAR));

                ttext.append(tbuf);

                break;

            }

        }
    }

    private void check_saved_datasets() {

        int j, m;
        for (j = 0; j < MAX_GAGEQC_DAYS; j++) {

            for (m = 0; m < 5; m++) {

                /* not databased */
                if (dqc.pdata[j].used[m] != 3 && dqc.pdata[j].used[m] != 2) {
                    continue;
                }

                send_dbase_new_area();

                return;

            }

        }

        /* The datasets have all been QC'd and saved to the database. */
        /* Go ahead and load the new DailyQC dataset. */

        if (MPEDisplayManager.getCurrent().isQpf() == true) {
            MPEDisplayManager.getCurrent().setQpf(false);
        }

        if (MPEDisplayManager.getCurrent().isMaxmin() == true) {
            MPEDisplayManager.getCurrent().setMaxmin(false);
        }

        if (MPEDisplayManager.getCurrent().isZflag() == true) {
            MPEDisplayManager.getCurrent().setZflag(false);
        }

        /* Free the data from the previous run. */
        dqc.qcDataReload(currntDate, qarea, qdays, false);

        /* Launch the qc gui function. */
    }

    public void send_dbase_new_area() {
        StringBuilder ttext = new StringBuilder();
        String tbuf = "";
        int j, m;
        Calendar ltime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

        /* initialize the three qc types status */
        for (int i = 0; i < SaveLevel2Data.NUM_QCTYPE; i++) {
            SaveLevel2Data.qctype_flag[i] = -1;
        }

        if (dqc.pdata == null) {
            return;
        }
        if (dqc.pdata.length == 0 && dqc.tdata.length == 0
                && dqc.zdata.length == 0) {
            return;
        }
        for (j = 0; j < MAX_GAGEQC_DAYS; j++) {
            /*
             * if processing level a data is not available do not write out
             * processing level b data. In addition, do not write MAPs if no
             * processing level a data
             */

            for (m = 0; m < 5; m++) {

                if (dqc.pdata[j].used[m] != 3 && dqc.pdata[j].used[m] != 2) {

                    if (dqc.zdata[j].used[m] != 3 && dqc.zdata[j].used[m] != 2) {

                        if (dqc.tdata[j].used[m] != 3
                                && dqc.tdata[j].used[m] != 2) {
                            continue;
                        }

                    }

                }

                ltime.setTime(dqc.pdata[j].data_time);

                tbuf = String.format("%02d-%02d-%04d\n",
                        ltime.get(Calendar.MONTH) + 1,
                        ltime.get(Calendar.DAY_OF_MONTH),
                        ltime.get(Calendar.YEAR));

                ttext.append(tbuf);

                break;

            }

        }

        int mode = 1;
        if (ttext.length() < 1) {
            return;
        }

        SendtoDatabaseDialog dlg = new SendtoDatabaseDialog(shell,
                ttext.toString(), mode);
        int rt = (Integer) dlg.open();
        if (rt == 1) {
            save_dbase();
        } else {
            cancel_dbase();
        }
    }

    /* get the token value of token mpe_map_one_zone */
    private int get_token_value_mpe_map_one_zone() {

        int token_of_mpe_map_one_zone = 0;
        int first = 0;

        if (first == 0) {
            String tokenName = "mpe_map_one_zone";
            String tokenValue = apps_defaults.getToken(tokenName);

            if (tokenValue != null && tokenValue.length() > 0) {
                /* we use the token ON and OFF */
                if (tokenValue.equalsIgnoreCase(ON)) {
                    token_of_mpe_map_one_zone = 1;
                }
            }

            first = 1;
        }

        return token_of_mpe_map_one_zone;
    }

    private void save_dbase() {

        int j, k, m, l, ll, num;
        int dqcEndingObsTime = dqc.getEnding6HourObsTime();
        int dqcTimeStringIndex = (dqcEndingObsTime / 6) + 1;
        Hrap_Grid hrap_grid = dqc.getHrap_grid();
        int numzones;
        float temp = 0.0f;
        Date old_time;
        int[] pcp_in_use = dqc.pcp_in_use;
        int time_pos = 0;
        int archive_flag[] = new int[10];
        Maps[] mean_areal_precip_global = dqc.mean_areal_precip_global;
        int max_stations = dqc.precip_stations.size();
        Calendar gm = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        String pcpn_bad_file = dqc.pcpn_bad_file;
        String map_file = dqc.map_file;
        String[][] timefile = dqc.timefile;
        String grid_file = dqc.grid_file;
        String proc_pcpn_file = dqc.proc_pcpn_file;
        StringBuilder buf = new StringBuilder();
        StringBuilder mbuf = new StringBuilder();
        StringBuilder pbuf = new StringBuilder();
        StringBuilder xbuf = new StringBuilder();
        String type = "2";
        BufferedWriter fp = null;
        int first = 0;
        int save_isom = dqc.isom;
        int one_zone_flag = get_token_value_mpe_map_one_zone();
        GridAttributes ga = new GridAttributes();
        int num_period_qc = 0;
        String fname_nc = "";
        float[][] datavals = new float[hrap_grid.maxi][hrap_grid.maxj];
        if (dqc.mpe_dqc_save_grib == true || dqc.mpe_dqc_save_netcdf == true) {
            for (int h = 0; h < hrap_grid.maxj; h++) {
                for (int i = 0; i < hrap_grid.maxi; i++) {
                    datavals[i][h] = (dqc.pcp.value[i][h] / 100.f);
                }
            }
        }

        /*
         * Generally the type in PEDSTEP in level2 file is "2", NWRFC requestes
         * the flexibility to have type as "R" or other number/letter. The
         * defaults type is "2". Retrieve value from token mpe_level2_type_value
         */

        if (first == 0) {
            String l2Type = apps_defaults.getToken("mpe_level2_type_value");

            if (l2Type != null && l2Type.length() > 0) {
                type = l2Type.trim();
            } else {
                type = "2";
            }

            // logMessage(
            // "\nSTATUS: 1) type value in type/source in level2 file is ##%s##\n",
            // type);

            first = 1;
        }

        for (k = 0; k < 10; k++) {
            archive_flag[k] = -1;
        }
        // logMessage("\nSTATUS: 2) type value in type/source in level2 file is ##%s##\n",type);
        save_isom = dqc.isom;

        // MPEDisplayManager mpd = MPEDisplayManager.getCurrent();

        if ((DailyQcUtils.qpf_flag == true || DailyQcUtils.maxmin_flag == true || DailyQcUtils.z_flag == true)
                && dqc.pcpn_day == 0
                && (dqc.curHr18_00 == 1 || dqc.curHr00_06 == 1 || dqc.curHr06_12 == 1)) {
            /*
             * if run DQC at the time frames such as curHr18_00 or curHr00_06 or
             * curHr06_12, for precipitation, do not display the 24 hr
             * precipiation if the pcpn_day=0
             */
            /*
             * 0 represents the time frame 12 - 18, 1 represents time frame
             * 18-00, 2 represents time frame 00-06Z, 3 represents time frame
             * 06-12z
             */

            if (DailyQcUtils.qpf_flag == true) {
                if (dqc.curHr18_00 == 1) {
                    dqc.pdata[dqc.pcpn_day].used[1] = 0;
                    dqc.pdata[dqc.pcpn_day].used[2] = 0;
                    dqc.pdata[dqc.pcpn_day].used[3] = 0;
                    dqc.pdata[dqc.pcpn_day].used[4] = 0;
                } else if (dqc.curHr00_06 == 1) {
                    dqc.pdata[dqc.pcpn_day].used[2] = 0;
                    dqc.pdata[dqc.pcpn_day].used[3] = 0;
                    dqc.pdata[dqc.pcpn_day].used[4] = 0;
                } else if (dqc.curHr06_12 == 1) {
                    dqc.pdata[dqc.pcpn_day].used[3] = 0;
                    dqc.pdata[dqc.pcpn_day].used[4] = 0;
                }
            }

            else if (DailyQcUtils.z_flag == true) {
                if (dqc.curHr18_00 == 1) {
                    dqc.zdata[dqc.pcpn_day].used[1] = 0;
                    dqc.zdata[dqc.pcpn_day].used[2] = 0;
                    dqc.zdata[dqc.pcpn_day].used[3] = 0;

                } else if (dqc.curHr00_06 == 1) {
                    dqc.zdata[dqc.pcpn_day].used[2] = 0;
                    dqc.zdata[dqc.pcpn_day].used[3] = 0;

                } else if (dqc.curHr06_12 == 1) {
                    dqc.zdata[dqc.pcpn_day].used[3] = 0;

                }

            }

            else if (DailyQcUtils.maxmin_flag == true) {
                if (dqc.curHr18_00 == 1) {
                    dqc.tdata[dqc.pcpn_day].used[1] = 0;
                    dqc.tdata[dqc.pcpn_day].used[2] = 0;
                    dqc.tdata[dqc.pcpn_day].used[3] = 0;

                } else if (dqc.curHr00_06 == 1) {
                    dqc.tdata[dqc.pcpn_day].used[2] = 0;
                    dqc.tdata[dqc.pcpn_day].used[3] = 0;
                } else if (dqc.curHr06_12 == 1) {
                    dqc.tdata[dqc.pcpn_day].used[3] = 0;

                }

            }
        }

        for (j = 0; j < MAX_GAGEQC_DAYS; j++) {

            /*
             * if processing level a data is not available do not write out
             * processing level b data. In addition, do not write MAPs if no
             * processing level a data
             */

            // Sanity check
            if (dqc.pdata == null || dqc.pdata.length <= 0) {
                return;
            }

            System.out
                    .println("******   SAVE LEVEL 2 DATA - Version 11092011   ******");

            for (m = 0; m < 5; m++) {
                /*
                 * New for 9.2. The user has to indicate via a GUI that they
                 * wish to save a QC'd precip hydro day. The qctype_flag must be
                 * positive (checked in the GUI) in order for level 2 to be
                 * saved.
                 */

                if ((dqc.pdata[j].used[m] == 3 || dqc.pdata[j].used[m] == 2)
                        && qctype_flag[0] == 1) {
                    break;
                }
            }

            if (m == 5) {
                continue;
            }

            /* re-create all pcpn and maps if necessary */
            /* next three lines are new */

            /*
             * do not estimate daily and partial point precipitation from each
             * other if run DQC on partial time frame and j=0
             */

            if (j == 0
                    && (dqc.curHr00_06 == 1 || dqc.curHr06_12 == 1 || dqc.curHr18_00 == 1)) {
            } else {
                EstDailyStations eds = new EstDailyStations();
                eds.estimate_daily_stations(j, dqc.precip_stations,
                        max_stations);
                EstPartStations eps = new EstPartStations();
                eps.estimate_partial_stations(j, dqc.precip_stations,
                        max_stations);
            }

            QCStations qcs = new QCStations();
            qcs.quality_control_stations(j, dqc.precip_stations, max_stations);
            CheckConsistency cc = new CheckConsistency();
            cc.check_consistency(j, dqc.precip_stations, max_stations);

            gm.setTime(dqc.pdata[j].data_time);

            String fbuf = String.format("%s%04d%02d%02d", pcpn_bad_file,
                    gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                    gm.get(Calendar.DAY_OF_MONTH));

            dqc.isom = gm.get(Calendar.MONTH);

            BadValues bv = new BadValues();
            bv.write_bad_values(fbuf, j);

            if (dqc.mpe_dqc_save_netcdf == true) {

                /* create name of netCDF file */
                fname_nc = String.format("%s%04d%02d%02d.nc", grid_file,
                        gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                        gm.get(Calendar.DAY_OF_MONTH));

                /* define attributes and store in structure */
                num_period_qc = ga.define_grid_attributes(1, j, num_period_qc);
            }

            if (dqc.mpe_dqc_save_grib == true) {

                if (dqc.mpe_dqc_save_netcdf == false) {
                    ga.define_grid_attributes(1, j, num_period_qc);
                }
            }

            // save for each 6 hour periods
            for (l = 0; l < 5; l++) {

                if (dqc.pdata[j].used[l] == 0 || qctype_flag[0] == -1) {
                    continue;
                }

                /*
                 * this needs to be fixed to ensure that the gridded temperature
                 * files are named correctly.
                 */
                if (l < 2) {
                    old_time = dqc.pdata[j].data_time;
                    Calendar od = Calendar.getInstance();
                    od.setTime(old_time);
                    od.add(Calendar.SECOND, -86400);
                    old_time = od.getTime();
                } else {
                    old_time = dqc.pdata[j].data_time;
                }

                gm.setTime(old_time);

                if (l < 4) {
                    ll = 0;
                } else {
                    ll = 1;
                }

                RenderPcp rcp = new RenderPcp();
                rcp.render_pcp(j, l, ll, max_stations, dqc.precip_stations,
                        hrap_grid, dqc.pdata, dqc.pcp_in_use);

                String dbuf = String.format("%s%s_%04d%02d%02d", grid_file,
                        timefile[2][l], gm.get(Calendar.YEAR),
                        gm.get(Calendar.MONTH) + 1,
                        gm.get(Calendar.DAY_OF_MONTH));

                // copy dqc.pcp.value to datavals
                for (int h = 0; h < hrap_grid.maxj; h++) {
                    for (int i = 0; i < hrap_grid.maxi; i++) {
                        datavals[i][h] = (dqc.pcp.value[i][h] / 100.f);
                    }
                }

                /* output grid to file in ascii xmrg format */
                WriteQPFGrids wqg = new WriteQPFGrids();
                wqg.write_qpf_grids(dbuf);

                if (dqc.mpe_dqc_save_grib == true) {
                    WriteDQCNetCDFGrids wng = new WriteDQCNetCDFGrids();
                    String ncfile = String.format("%s.nc", dbuf);
                    wng.write_dqc_netcdf_grids(ncfile, l, 1, 1,
                            ga.getCommonGridAttributes(), datavals);
                    WriteDQCGribGrids wgg = new WriteDQCGribGrids();
                    String fname_grib = String.format("%s.grb", dbuf);
                    int status = wgg
                            .write_dqc_grib_grids(ncfile, fname_grib, 1);
                    if (status != 0) {
                        statusHandler
                                .handle(Priority.WARN,
                                        String.format(
                                                "problem with writing GRIB file in write_dqc_grib_grids. status=%d\n",
                                                status));
                    }

                }

                if (l < 4) {
                    num = j * 4 + 3 - l;
                } else {
                    num = 40 + j;
                }

                /* Create the MAP. */
                CreateMap cm = new CreateMap();
                cm.create_map(num);

            }

            // save the whole days
            for (l = 0; l < 5; l++) {

                if (dqc.pdata[j].used[l] == 0 || qctype_flag[0] == -1) {
                    continue;
                }

                /*
                 * this needs to be fixed to ensure that the gridded temperature
                 * files are named correctly.
                 */
                if (l < 2) {
                    old_time = dqc.pdata[j].data_time;
                    Calendar od = Calendar.getInstance();
                    od.setTime(old_time);
                    od.add(Calendar.SECOND, -86400);
                    old_time = od.getTime();
                } else {
                    old_time = dqc.pdata[j].data_time;
                }

                gm.setTime(old_time);

                if (l < 4) {
                    ll = 0;
                } else {
                    ll = 1;
                }

                RenderPcp rcp = new RenderPcp();
                rcp.render_pcp(j, l, ll, max_stations, dqc.precip_stations,
                        hrap_grid, dqc.pdata, dqc.pcp_in_use);

                // copy dqc.pcp.value to datavals
                for (int h = 0; h < hrap_grid.maxj; h++) {
                    for (int i = 0; i < hrap_grid.maxi; i++) {
                        datavals[i][h] = (dqc.pcp.value[i][h] / 100.f);
                    }
                }

                String dbuf = String.format("%s%s_%04d%02d%02d", grid_file,
                        timefile[2][l], gm.get(Calendar.YEAR),
                        gm.get(Calendar.MONTH) + 1,
                        gm.get(Calendar.DAY_OF_MONTH));

                /* output grid to file in ascii xmrg format */
                WriteQPFGrids wqg = new WriteQPFGrids();
                wqg.write_qpf_grids(dbuf);

                /* output grid to file in netCDF format */
                if (dqc.mpe_dqc_save_netcdf == true) {
                    WriteDQCNetCDFGrids wng = new WriteDQCNetCDFGrids();
                    wng.write_dqc_netcdf_grids(fname_nc, l, num_period_qc, 1,
                            ga.getCommonGridAttributes(), datavals);
                }

                if (l < 4) {
                    num = j * 4 + 3 - l;
                } else {
                    num = 40 + j;
                }

                /* Create the MAP. */
                CreateMap cm = new CreateMap();
                cm.create_map(num);

            }

            EstMissingStations ems = new EstMissingStations();
            ems.estimate_missing_stations(j, dqc.precip_stations, max_stations,
                    dqc.pdata);

            old_time = dqc.pdata[j].data_time;

            gm.setTime(old_time);

            /* create the point precipitation level 2 data file */
            fbuf = String.format("%s%04d%02d%02d", proc_pcpn_file,
                    gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                    gm.get(Calendar.DAY_OF_MONTH));
            System.out.println("Writing Level 2 Point Precip data. ");
            try {
                if (fp == null) {
                    File fo = new File(fbuf.toString());
                    fo.setReadable(true, false);
                    fo.setWritable(true, false);
                    fp = new BufferedWriter(new FileWriter(fo));
                }
                for (m = 0; m < max_stations; m++) {

                    buf.setLength(0);
                    pbuf.setLength(0);
                    mbuf.setLength(0);
                    gm.setTime(dqc.pdata[j].data_time);
                    pbuf.append(String.format("PPD%s%s", type,
                            dqc.precip_stations.get(m).parm.substring(4)));
                    buf.append(String.format(".AR %s %02d%02d%02d DH12/%s ",
                            dqc.precip_stations.get(m).hb5,
                            gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                            gm.get(Calendar.DAY_OF_MONTH), pbuf.toString()));

                    /* write out the 24 hr point precipitation */
                    if (dqc.pdata[j].stn[m].frain[4].data < 0) {
                        mbuf.append("   M  ");
                    } else {
                        mbuf.append(String.format("%5.2f",
                                dqc.pdata[j].stn[m].frain[4].data));

                        if (dqc.pdata[j].stn[m].sflag[4] != 1) {
                            if (dqc.pdata[j].stn[m].frain[4].qual == 0) {
                                mbuf.append("S");

                            }
                            if (dqc.pdata[j].stn[m].frain[4].qual == 1) {
                                mbuf.append("F");

                            }
                            if (dqc.pdata[j].stn[m].frain[4].qual == 2) {
                                mbuf.append("W");

                            }
                            if (dqc.pdata[j].stn[m].frain[4].qual == 3) {
                                mbuf.append("Q");

                            }
                            if (dqc.pdata[j].stn[m].frain[4].qual == 4) {
                                mbuf.append("D");

                            }
                            if (dqc.pdata[j].stn[m].frain[4].qual == 8) {
                                mbuf.append("V");

                            }
                            if (dqc.pdata[j].stn[m].frain[4].qual == 5) {
                                mbuf.append("E");

                            }
                            if (dqc.pdata[j].stn[m].frain[4].qual == 6) {
                                mbuf.append("L");

                            }

                        } else if (dqc.pdata[j].stn[m].sflag[4] == 1) {
                            if (dqc.pdata[j].stn[m].frain[k].qual == 8) {
                                mbuf.append("A");

                            }
                            if (dqc.pdata[j].stn[m].frain[k].qual == 0) {
                                mbuf.append("B");

                            }
                            if (dqc.pdata[j].stn[m].frain[k].qual == 3) {
                                mbuf.append("C");

                            }

                        }

                    }
                    buf.append(mbuf.toString());
                    fp.write(buf.toString());
                    fp.newLine();

                    /*
                     * write out the four 6hr time frame point precipitation
                     * level 2 data
                     */
                    old_time = dqc.pdata[j].data_time;
                    gm.setTime(old_time);
                    gm.add(Calendar.SECOND, -86400);
                    pbuf.setLength(0);
                    buf.setLength(0);
                    mbuf.setLength(0);
                    pbuf.append(String.format("PPQ%s%s", type,
                            dqc.precip_stations.get(m).parm.substring(4)));
                    buf.append(String.format(
                            ".ER %s %02d%02d%02d DH18/%s/DIH+6/",
                            dqc.precip_stations.get(m).hb5,
                            gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                            gm.get(Calendar.DAY_OF_MONTH), pbuf.toString()));

                    for (k = 0; k < 4; k++) {
                        mbuf.setLength(0);
                        if (dqc.pdata[j].used[k] == 0) {
                            mbuf.append("   ");
                            if (k != 3) {
                                mbuf.append("/");

                            }

                        } else if (dqc.pdata[j].stn[m].frain[k].data < 0) {
                            mbuf = new StringBuilder();
                            mbuf.append("   M  ");

                            if (k != 3) {
                                mbuf.append("/");

                            }

                        } else {
                            mbuf.append(String.format("%5.2f",
                                    dqc.pdata[j].stn[m].frain[k].data));

                            if (dqc.pdata[j].stn[m].frain[k].qual == 8) {
                                mbuf.append("V");

                            }
                            if (dqc.pdata[j].stn[m].frain[k].qual == 1) {
                                mbuf.append("F");

                            }
                            if (dqc.pdata[j].stn[m].frain[k].qual == 2) {
                                mbuf.append("W");

                            }
                            if (dqc.pdata[j].stn[m].frain[k].qual == 3) {
                                mbuf.append("Q");

                            }
                            if (dqc.pdata[j].stn[m].frain[k].qual == 4) {
                                mbuf.append("D");

                            }
                            if (dqc.pdata[j].stn[m].frain[k].qual == 0) {
                                mbuf.append("S");

                            }
                            if (dqc.pdata[j].stn[m].frain[k].qual == 5) {
                                mbuf.append("E");

                            }
                            if (dqc.pdata[j].stn[m].frain[k].qual == 6) {
                                mbuf.append("L");

                            }
                            if (k != 3) {
                                mbuf.append("/");

                            }

                        }

                        buf.append(mbuf.toString());

                    }

                    fp.write(buf.toString());
                    fp.newLine();

                }

                fp.close();
                fp = null;
                System.out
                        .println("Finished writing out Level 2 Point precip file. ");
            } catch (IOException e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "ERROR: There was a problem creating/writing out the Level 2 Point precip file "
                                        + fbuf + " . " + e.getMessage(), e);
                return;
            } finally {
                try {
                    if (fp != null) {
                        fp.close();
                        fp = null;
                    }

                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            /* write_qc_file(type); */
            for (l = 0; l < 4; l++) {

                int jj = j * 4 + 3 - l;

                if (pcp_in_use[jj] != 1) {
                    continue;
                }

                if (l < 2) {
                    old_time = dqc.pdata[j].data_time;
                    gm.setTime(old_time);
                    gm.add(Calendar.SECOND, -86400);
                } else {
                    old_time = dqc.pdata[j].data_time;
                    gm.setTime(old_time);
                }

            }

            /* build map file */
            System.out.println("Building MAP .");
            found: for (m = 0; mean_areal_precip_global[m] != null; m++) {
                for (k = 0; k < 4; k++) {

                    num = j * 4 + 3 - k;

                    if (mean_areal_precip_global[m].maps_done != null) {
                        if (mean_areal_precip_global[m].maps_done[num] == 1) {
                            break found;
                        }
                    }

                }

            }

            if (mean_areal_precip_global[m] != null) {
                if (mean_areal_precip_global[m].hb5 == "") {
                    System.out.println("MAP.hb5 is empty, continuing. ");
                    continue;
                }
            }

            gm.setTime(dqc.pdata[j].data_time);

            fbuf = "";
            fbuf = String.format("%s%04d%02d%02d", map_file,
                    gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                    gm.get(Calendar.DAY_OF_MONTH));
            System.out.println("Writing out MAP file: " + fbuf);

            try {
                if (fp == null) {
                    File fo = new File(fbuf.toString());
                    fo.setReadable(true, false);
                    fo.setWritable(true, false);
                    fp = new BufferedWriter(new FileWriter(fo));
                    System.out.println("Creating new MAP file. ");
                    System.out.println("One zone flag is set to: "
                            + one_zone_flag);
                }
                /*
                 * at least processing level a data is available but no
                 * guarantee that MAPs have been rendered
                 */
                old_time = dqc.pdata[j].data_time;
                gm.setTime(old_time);
                gm.add(Calendar.SECOND, -86400);

                /* loop through and write maps to file */
                for (m = 0; m < dqc.getMax_basins(); m++) {
                    numzones = 0;

                    for (l = 0; l < 4; l++) {

                        if (mean_areal_precip_global[m].zones[l] < 0) {
                            continue;
                        }

                        numzones++;

                    }

                    for (l = 0; l < 4; l++) {

                        if (mean_areal_precip_global[m].zones[l] < 0) {
                            continue;
                        }

                        xbuf.setLength(0);
                        xbuf.append(".ER ");
                        xbuf.append(mean_areal_precip_global[m].hb5);

                        String tb = xbuf.toString().toUpperCase();
                        xbuf.setLength(0);
                        xbuf.append(tb);

                        /* fix for polygon basins */
                        if (dqc.mpe_rfc_name.equalsIgnoreCase("cbrfc")
                                && mean_areal_precip_global[m].hb5.length() == 7) {

                            // logMessage ("polygon write\n");
                            xbuf.append(mean_areal_precip_global[m].bchar
                                    .charAt(0));

                        }

                        else {

                            if (l == 0 && numzones != 1 && one_zone_flag != 1) {
                                xbuf.append("L");
                                xbuf.append(mean_areal_precip_global[m].bchar
                                        .charAt(0));

                            }

                            else if (l == 0 && numzones == 1
                                    && one_zone_flag != 1) {
                                xbuf.append("O");
                                xbuf.append(mean_areal_precip_global[m].bchar
                                        .charAt(0));

                            }

                            else if (l == 1) {
                                xbuf.append("M");
                                xbuf.append(mean_areal_precip_global[m].bchar
                                        .charAt(1));
                            }

                            else if (l == 2) {
                                xbuf.append("U");
                                xbuf.append(mean_areal_precip_global[m].bchar
                                        .charAt(2));
                            }

                            else if (l == 3) {
                                xbuf.append("G");
                                xbuf.append(mean_areal_precip_global[m].bchar
                                        .charAt(3));
                            }

                        }

                        mbuf.setLength(0);
                        mbuf.append(String.format(" %02d%02d%02d",
                                gm.get(Calendar.YEAR),
                                gm.get(Calendar.MONTH) + 1,
                                gm.get(Calendar.DAY_OF_MONTH)));
                        xbuf.append(mbuf.toString());

                        xbuf.append(" DH18/PPQPBZZ/DIH+6");

                        for (k = 0; k < 4; k++) {

                            int h = j * 4 + (3 - k);

                            /* fix for polygon basins */

                            if (dqc.mpe_rfc_name.equalsIgnoreCase("cbrfc")
                                    && mean_areal_precip_global[m].hb5.length() == 7) {
                                temp = mean_areal_precip_global[m].lz[h];
                            } else if (l == 0) {
                                temp = mean_areal_precip_global[m].lz[h];
                            } else if (l == 1) {
                                temp = mean_areal_precip_global[m].mz[h];
                            } else if (l == 2) {
                                temp = mean_areal_precip_global[m].uz[h];
                            } else if (l == 3) {
                                temp = mean_areal_precip_global[m].gz[h];
                            }

                            if (mean_areal_precip_global[m].maps_done[h] != 1) {
                                mbuf = new StringBuilder();
                                mbuf.append("   ");
                            } else {
                                mbuf = new StringBuilder();
                                mbuf.append(String.format("%5.2f ", temp));
                            }

                            if (k < 4) {
                                xbuf.append("/");
                            }

                            xbuf.append(mbuf.toString());

                        }
                        if (one_zone_flag == 0) {
                            fp.write(xbuf.toString());
                            fp.newLine();
                        } else if (one_zone_flag == 1 && l == 0) {
                            fp.write(xbuf.toString());
                            System.out
                                    .println("Writing out record to One Zone MAP file: "
                                            + xbuf.toString());
                            fp.newLine();
                        } else {
                            continue;
                        }
                    }
                }

                fp.close();
                System.out.println("Finished writing out MAP file. ");
                fp = null;

            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "ERROR: There was a problem creating/writing out the MAP file "
                                + fbuf + " . Possible file permissions issue "
                                + e.getMessage(), e);
                return;
            } finally {
                try {
                    if (fp != null) {
                        fp.close();
                        fp = null;
                    }

                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            for (m = 0; m < 5; m++) {

                if (dqc.pdata[j].used[m] != 0) {
                    dqc.pdata[j].used[m] = 4;
                }

            }
            bv.restore_bad_values(j, dqc.precip_stations, max_stations);

            /* write stddev file */
            gm.setTime(dqc.pdata[j].data_time);

            String ebuf = String.format("%s%04d%02d%02d", dqc.pcpn_dev_file,
                    gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                    gm.get(Calendar.DAY_OF_MONTH));
            System.out.println("Writing out Precip Dev file. ");

            try {
                if (fp == null) {
                    File fo = new File(ebuf.toString());
                    fo.setReadable(true, false);
                    fo.setWritable(true, false);
                    fp = new BufferedWriter(new FileWriter(fo));
                }
                if (fp != null) {
                    mbuf.setLength(0);
                    mbuf.append(String.format("%f", dqc.pdata[j].stddev));
                    fp.write(mbuf.toString());
                    fp.newLine();
                    fp.close();
                    fp = null;

                }
                System.out.println("Finished writing Precip Dev file. ");

            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "ERROR: There was a problem creating/writing out the file "
                                + ebuf + ". Possible file permissions issue "
                                + e.getMessage(), e);
                return;
            } finally {
                try {
                    if (fp != null) {
                        fp.close();
                        fp = null;
                    }

                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            ebuf = String.format("%s%04d%02d%02d", dqc.temp_dev_file,
                    gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                    gm.get(Calendar.DAY_OF_MONTH));

            try {
                if (fp == null) {
                    File fo = new File(ebuf.toString());
                    fo.setReadable(true, false);
                    fo.setWritable(true, false);
                    fp = new BufferedWriter(new FileWriter(fo));
                }

                if (fp != null) {
                    mbuf.setLength(0);
                    mbuf.append(String.format("%f", dqc.tdata[j].stddev));
                    fp.write(mbuf.toString());
                    fp.newLine();
                    fp.close();
                    fp = null;

                }

            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "ERROR: There was a problem creating/writing out the file "
                                + ebuf + ". Possible file permissions issue "
                                + e.getMessage(), e);
                return;

            } finally {
                try {
                    if (fp != null) {
                        fp.close();
                        fp = null;

                    }

                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            archive_flag[j] = 1;

        }

        /* Process the freezing level data if they exist. */
        int max_zstations = dqc.freezing_stations.size();
        String[][] ztimefile = dqc.ztimefile;

        for (j = 0; j < MAX_GAGEQC_DAYS; j++) {

            /*
             * if processing level a data is not available do not write out
             * processing level b data. In addition, do not write MAPs if no
             * processing level a data
             */
            for (m = 0; m < 4; m++) {
                /*
                 * this rule will be different for freezing level since it's
                 * model data and may not really be QC'd by the user. So let the
                 * level 1 pass through here to the save data question
                 */

                if ((dqc.zdata[j].used[m] == 3 || dqc.zdata[j].used[m] == 2 || dqc.zdata[j].used[m] == 1)
                        && qctype_flag[2] == 1) {
                    break;
                }

            }

            if (m == 4) {
                continue;
            }

            // logMessage ("create maz\n");

            if (dqc.mpe_dqc_save_netcdf == true) {

                /* create name of netCDF file */
                fname_nc = String.format("%s%04d%02d%02d.nc", dqc.zgrid_file,
                        gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                        gm.get(Calendar.DAY_OF_MONTH));

                // logMessage ("netcdf file = %s\n", fname_nc);
                /* define attributes and store in structure */
                ga.define_grid_attributes(3, j, num_period_qc);
            }
            if (dqc.mpe_dqc_save_grib == true) {

                if (dqc.mpe_dqc_save_netcdf != true) {
                    ga.define_grid_attributes(3, j, num_period_qc);
                }
            }

            /* re-create all pcpn and maps if necessary */

            /* next three lines are new */

            // gm.setTime(dqc.zdata[j].data_time);

            for (l = 0; l < 4; l++) {

                if (dqc.zdata[j].used[l] == 0 || qctype_flag[2] == -1) {
                    continue;
                }
                /*
                 * dqcEndingObsTime is read from the dqc_ending_6hour_obstime
                 * token. It should only be 06 or 12 and the default is 06.
                 * However, have to account for it being set to 12 and original
                 * code didn't account for this.
                 */
                if (dqcEndingObsTime == 12) {
                    if (l < 1) {
                        old_time = dqc.zdata[j].data_time;
                        gm.setTime(old_time);
                        gm.add(Calendar.SECOND, -86400);
                    } else {
                        old_time = dqc.zdata[j].data_time;
                        gm.setTime(old_time);
                    }
                } else {
                    /*
                     * dqcEndingObsTime == 6 but since only two values are
                     * valid, can just use an else here until code needs to
                     * accommodate the others
                     */
                    if (l < 2) {
                        old_time = dqc.zdata[j].data_time;
                        gm.setTime(old_time);
                        gm.add(Calendar.SECOND, -86400);
                    } else {
                        old_time = dqc.zdata[j].data_time;
                        gm.setTime(old_time);
                    }
                }
                // logMessage ("Freezing level %02d-%02d-%02d", gm->tm_mon + 1,
                // gm->tm_mday, gm->tm_year + 1900);

                RenderZ rz = new RenderZ();
                rz.render_z(j, l, 0, max_zstations, dqc.freezing_stations,
                        hrap_grid, dqc.zdata, dqc.pcp_in_use);

                String dbuf = String.format("%s%s_%04d%02d%02d",
                        dqc.zgrid_file, ztimefile[dqcTimeStringIndex][l],
                        gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                        gm.get(Calendar.DAY_OF_MONTH));

                WriteQPFGrids wqg = new WriteQPFGrids();
                wqg.write_qpf_grids(dbuf);

                /* output grid to file in netCDF format */
                if (l + 1 <= num_period_qc) {
                    if (dqc.mpe_dqc_save_netcdf == true) {
                        WriteDQCNetCDFGrids wng = new WriteDQCNetCDFGrids();
                        wng.write_dqc_netcdf_grids(fname_nc, l, num_period_qc,
                                3, ga.getCommonGridAttributes(), datavals);
                    }
                }
                if (dqc.mpe_dqc_save_grib == true) {
                    WriteDQCNetCDFGrids wng = new WriteDQCNetCDFGrids();
                    String ncfile = String.format("%s.nc", dbuf);
                    wng.write_dqc_netcdf_grids(ncfile, l, 1, 3,
                            ga.getCommonGridAttributes(), datavals);
                    WriteDQCGribGrids wgg = new WriteDQCGribGrids();
                    String fname_grib = String.format("%s.grb", dbuf);
                    int status = wgg
                            .write_dqc_grib_grids(ncfile, fname_grib, 3);

                    if (status != 0) {
                        statusHandler
                                .handle(Priority.WARN,
                                        String.format(
                                                "problem with writing GRIB file in write_dqc_grib_grids. status=%d\n",
                                                status));
                    }
                }

                num = 100 + j * 4 + 3 - l;

                MakeRsel mr = new MakeRsel();
                mr.make_rsel(num, num - 100);

            }

            old_time = dqc.zdata[j].data_time;
            gm.setTime(old_time);

            String fbuf = String.format("%s%04d%02d%02d", dqc.zpoint2_file,
                    gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                    gm.get(Calendar.DAY_OF_MONTH));

            try {
                if (fp == null) {
                    File fo = new File(fbuf.toString());
                    fo.setReadable(true, false);
                    fo.setWritable(true, false);
                    fp = new BufferedWriter(new FileWriter(fo));
                }

                for (m = 0; m < max_zstations; m++) {
                    old_time = dqc.zdata[j].data_time;
                    gm.setTime(old_time);
                    gm.add(Calendar.SECOND, -86400);

                    pbuf = new StringBuilder();
                    pbuf.append(String.format("HZI%s%s", type,
                            dqc.freezing_stations.get(m).parm.substring(4)));

                    if (dqcEndingObsTime == 12) {
                        buf.setLength(0);
                        buf.append(String.format(
                                ".ER %s %02d%02d%02d DH18/%s/DIH+6/ ",
                                dqc.freezing_stations.get(m).hb5,
                                gm.get(Calendar.YEAR),
                                gm.get(Calendar.MONTH) + 1,
                                gm.get(Calendar.DAY_OF_MONTH), pbuf.toString()));
                    } else {
                        buf = new StringBuilder();
                        buf.append(String.format(
                                ".ER %s %02d%02d%02d DH12/%s/DIH+6/ ",
                                dqc.freezing_stations.get(m).hb5,
                                gm.get(Calendar.YEAR),
                                gm.get(Calendar.MONTH) + 1,
                                gm.get(Calendar.DAY_OF_MONTH), pbuf.toString()));
                    }

                    for (k = 0; k < 4; k++) {
                        if (dqc.zdata[j].used[k] == 0) {
                            mbuf.setLength(0);
                            mbuf.append("    ");
                            if (k != 3) {
                                mbuf.append("/");
                            }
                        }

                        else if (dqc.zdata[j].zstn[m].zlevel2[k].data < 0) {
                            mbuf.setLength(0);
                            mbuf.append("   M ");
                            if (k != 3) {
                                mbuf.append("/");
                            }
                        } else {
                            mbuf.setLength(0);
                            mbuf.append(String.format("%4.1f",
                                    dqc.zdata[j].zstn[m].zlevel2[k].data));
                            if (dqc.zdata[j].zstn[m].zlevel2[k].qual == 8) {
                                mbuf.append("S");
                            }
                            if (dqc.zdata[j].zstn[m].zlevel2[k].qual == 1) {
                                mbuf.append("F");
                            }

                            if (dqc.zdata[j].zstn[m].zlevel2[k].qual == 2) {
                                mbuf.append("W");
                            }

                            if (dqc.zdata[j].zstn[m].zlevel2[k].qual == 5) {
                                mbuf.append("E");
                            }

                            if (k != 3) {
                                mbuf.append("/");
                            }

                        }
                        buf.append(mbuf.toString());
                    }
                    fp.write(buf.toString());
                    fp.newLine();
                }
                fp.close();
                fp = null;
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "ERROR: There was a problem creating/writing out the file "
                                + fbuf + ". Possible file permissions issue "
                                + e.getMessage(), e);
                return;
            } finally {
                try {
                    if (fp != null) {
                        fp.close();
                    }

                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            /* build map file */
            foundz: for (m = 0; mean_areal_precip_global[m].hb5 != ""; m++) {

                for (k = 0; k < 4; k++) {

                    num = j * 4 + 3 - k;

                    if (mean_areal_precip_global[m].zmaps_done[num] == 1) {
                        break foundz;
                    }

                }

            }

            if (mean_areal_precip_global[m].hb5 == "") {
                continue;
            }
            gm.setTime(dqc.zdata[j].data_time);

            fbuf = String.format("%s%04d%02d%02d", dqc.rsel_file,
                    gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                    gm.get(Calendar.DAY_OF_MONTH));

            try {
                if (fp == null) {
                    File fo = new File(fbuf.toString());
                    fo.setReadable(true, false);
                    fo.setWritable(true, false);
                    fp = new BufferedWriter(new FileWriter(fo));
                }
                /*
                 * At least processing level a data is available but no
                 * guarantee that MAPs have been rendered
                 */

                old_time = dqc.zdata[j].data_time;
                gm.setTime(old_time);
                gm.add(Calendar.SECOND, -86400);

                /* loop through and write maps to file */
                for (m = 0; m < dqc.getMax_basins(); m++) {
                    numzones = 0;

                    for (l = 0; l < 4; l++) {

                        if (mean_areal_precip_global[m].zones[l] < 0) {
                            continue;
                        }

                        numzones++;

                    }

                    for (l = 0; l < 4; l++) {

                        if (mean_areal_precip_global[m].zones[l] < 0) {
                            continue;
                        }
                        xbuf.setLength(0);
                        xbuf.append(".ER ");
                        xbuf.append(mean_areal_precip_global[m].hb5);

                        String tb = xbuf.toString().toUpperCase();
                        xbuf.setLength(0);
                        xbuf.append(tb);

                        /* fix for polygon basins */
                        if (dqc.mpe_rfc_name.equalsIgnoreCase("cbrfc")
                                && mean_areal_precip_global[m].hb5.length() == 7) {

                            // logMessage ("polygon write\n");
                            xbuf.append(mean_areal_precip_global[m].bchar
                                    .charAt(0));

                        }

                        else {

                            if (l == 0 && numzones != 1) {

                                xbuf.append("L");
                                xbuf.append(mean_areal_precip_global[m].bchar
                                        .charAt(0));

                            }

                            else if (l == 0 && numzones == 1) {

                                xbuf.append("O");
                                xbuf.append(mean_areal_precip_global[m].bchar
                                        .charAt(0));

                            }

                            else if (l == 1) {

                                xbuf.append("M");
                                xbuf.append(mean_areal_precip_global[m].bchar
                                        .charAt(1));

                            }

                            else if (l == 2) {

                                xbuf.append("U");
                                xbuf.append(mean_areal_precip_global[m].bchar
                                        .charAt(2));

                            }

                            else if (l == 3) {

                                xbuf.append("G");
                                xbuf.append(mean_areal_precip_global[m].bchar
                                        .charAt(3));

                            }

                        }

                        mbuf.setLength(0);
                        mbuf.append(String.format(" %02d%02d%02d",
                                gm.get(Calendar.YEAR),
                                gm.get(Calendar.MONTH) + 1,
                                gm.get(Calendar.DAY_OF_MONTH)));
                        xbuf.append(mbuf.toString());

                        if (dqcEndingObsTime == 12) {
                            mbuf.setLength(0);
                            mbuf.append(" DH18/HZIPBZZ/DIH+6");
                        } else {
                            mbuf.setLength(0);
                            mbuf.append(" DH12/HZIPBZZ/DIH+6");
                        }

                        xbuf.append(mbuf.toString());

                        for (k = 0; k < 4; k++) {

                            int h = j * 4 + (3 - k);

                            if (dqc.mpe_rfc_name.equalsIgnoreCase("cbrfc")
                                    && mean_areal_precip_global[m].hb5.length() == 7) {
                                temp = mean_areal_precip_global[m].zlz[h];

                            } else if (l == 0) {
                                temp = mean_areal_precip_global[m].zlz[h];
                            } else if (l == 1) {
                                temp = mean_areal_precip_global[m].zmz[h];
                            } else if (l == 2) {
                                temp = mean_areal_precip_global[m].zuz[h];
                            } else if (l == 3) {
                                temp = mean_areal_precip_global[m].zgz[h];
                            }

                            if (mean_areal_precip_global[m].zmaps_done[h] != 1) {
                                mbuf.setLength(0);
                                mbuf.append("    ");
                            } else {
                                mbuf.setLength(0);
                                mbuf.append(String.format("%4.1f ", temp));
                            }

                            if (k < 4) {
                                xbuf.append("/");
                            }
                            xbuf.append(mbuf.toString());

                        }

                        fp.write(xbuf.toString());
                        fp.newLine();

                    }

                }

                fp.close();
                fp = null;
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "ERROR: There was a problem creating/writing out the file "
                                + fbuf + ". Possible file permissions issue "
                                + e.getMessage(), e);
                return;
            } finally {
                try {
                    if (fp != null) {
                        fp.close();
                    }

                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            for (m = 0; m < 4; m++) {

                if (dqc.zdata[j].used[m] != 0) {
                    dqc.zdata[j].used[m] = 4;
                }

            }

            archive_flag[j] = 1;

        }

        WriteQc2File wq = new WriteQc2File();
        wq.writeQcFile(type);

        /*************** Process the temperature data. ****************/
        int max_tstations = dqc.temperature_stations.size();

        for (j = 0; j < MAX_GAGEQC_DAYS; j++) {

            /*
             * if processing level a data is not available do not write out
             * processing level b data. In addition, do not write MAPs if not
             * processing level a data
             */

            for (m = 0; m < 6; m++) {

                if ((dqc.tdata[j].used[m] == 3 || dqc.tdata[j].used[m] == 2)
                        && qctype_flag[1] == 1) {
                    break;
                }

            }

            if (m == 6) {
                continue;
            }

            /* re-create all pcpn and maps if necessary */

            if (j == 0
                    && (dqc.curHr00_06 == 1 || dqc.curHr06_12 == 1 || dqc.curHr18_00 == 1)) {
                // do not estimate
            } else {

                EstDailyTStations edt = new EstDailyTStations();
                edt.estimate_daily_tstations(j, dqc.temperature_stations,
                        max_tstations);
            }

            QCTStations qct = new QCTStations();
            qct.quality_control_tstations(j, dqc.temperature_stations,
                    max_tstations);

            gm.setTime(dqc.tdata[j].data_time);

            String fbuf = String.format("%s%04d%02d%02d", dqc.temp_bad_file,
                    gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                    gm.get(Calendar.DAY_OF_MONTH));

            BadTValues btv = new BadTValues();
            btv.write_bad_tvalues(fbuf, j);

            if (dqc.mpe_dqc_save_netcdf == true) {

                /* create name of netCDF file */
                fname_nc = String.format("%s%04d%02d%02d.nc", dqc.tgrid_file,
                        gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                        gm.get(Calendar.DAY_OF_MONTH));

                // logMessage ("netcdf file = %s\n", fname_nc);
                /* define attributes and store in structure */
                ga.define_grid_attributes(2, j, num_period_qc);
            }
            if (dqc.mpe_dqc_save_grib == true) {

                if (dqc.mpe_dqc_save_netcdf != true) {
                    ga.define_grid_attributes(2, j, num_period_qc);
                }
            }

            for (l = 5; l >= 0; l--) {

                if (dqc.tdata[j].used[l] == 0 || qctype_flag[1] == -1) {
                    continue;
                }

                /*
                 * dqcEndingObsTime is by the dqc_ending_6hour_obstime token. It
                 * should only be 06 or 12 and the default is 06. However, have
                 * to account for it being set to 12 and original code didn't
                 * account for this.
                 */
                if (dqcEndingObsTime == 12) {
                    if (l < 1) {
                        old_time = dqc.tdata[j].data_time;
                        gm.setTime(old_time);
                        gm.add(Calendar.SECOND, -86400);
                    } else {
                        old_time = dqc.tdata[j].data_time;
                        gm.setTime(old_time);
                    }
                } else {
                    /*
                     * dqcEndingObsTime == 6 but since only two values are
                     * valid, can use just an else here until code needs to
                     * accommodate the others
                     */
                    if (l < 2) {
                        old_time = dqc.tdata[j].data_time;
                        gm.setTime(old_time);
                        gm.add(Calendar.SECOND, -86400);
                    } else {
                        old_time = dqc.tdata[j].data_time;
                        gm.setTime(old_time);
                    }
                }

                // logMessage ("Temperature %02d-%02d-%02d", gm->tm_mon + 1,
                // gm->tm_mday, gm->tm_year + 1900);

                RenderT rt = new RenderT();
                if (l == 5) {
                    rt.render_t(j, l, 2, max_tstations,
                            dqc.temperature_stations, hrap_grid, dqc.tdata,
                            dqc.pcp_in_use);
                } else if (l == 4) {
                    rt.render_t(j, l, 1, max_tstations,
                            dqc.temperature_stations, hrap_grid, dqc.tdata,
                            dqc.pcp_in_use);
                } else {
                    RenderT6 rt6 = new RenderT6();
                    rt6.render_t6(j, l, 0, max_tstations,
                            dqc.temperature_stations, hrap_grid, dqc.tdata,
                            dqc.pcp_in_use);
                }

                String dbuf = String.format("%s%s_%04d%02d%02d",
                        dqc.tgrid_file, dqc.ttimefile[dqcTimeStringIndex][l],
                        gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                        gm.get(Calendar.DAY_OF_MONTH));

                WriteQPFGrids wqg = new WriteQPFGrids();
                wqg.write_qpf_grids(dbuf);

                /* output grid to file in netCDF format */
                if (l + 1 <= num_period_qc) {
                    if (dqc.mpe_dqc_save_netcdf == true) {
                        WriteDQCNetCDFGrids wng = new WriteDQCNetCDFGrids();
                        wng.write_dqc_netcdf_grids(fname_nc, l, num_period_qc,
                                2, ga.getCommonGridAttributes(), datavals);
                    }
                }
                if (dqc.mpe_dqc_save_grib == true) {
                    WriteDQCNetCDFGrids wng = new WriteDQCNetCDFGrids();
                    String ncfile = String.format("%s.nc", dbuf);
                    wng.write_dqc_netcdf_grids(ncfile, l, 1, 2,
                            ga.getCommonGridAttributes(), datavals);
                    WriteDQCGribGrids wgg = new WriteDQCGribGrids();
                    String fname_grib = String.format("%s.grb", dbuf);
                    int status = wgg
                            .write_dqc_grib_grids(ncfile, fname_grib, 2);
                    File nfile = new File(ncfile);
                    nfile.delete();
                    nfile = null;
                    if (status != 0) {
                        statusHandler
                                .handle(Priority.WARN,
                                        String.format(
                                                "problem with writing GRIB file in write_dqc_grib_grids. status=%d\n",
                                                status));
                    }
                }

                num = 150 + j * 4 + 3 - l;

                if (l < 4) {
                    MakeMat mmt = new MakeMat();
                    mmt.make_mat(num, num - 150);
                }
            }

            EstMissingTStations emt = new EstMissingTStations();
            emt.estimate_missing_tstations(j, dqc.temperature_stations,
                    max_tstations, dqc.tdata);

            old_time = dqc.tdata[j].data_time;
            gm.setTime(old_time);

            fbuf = String.format("%s%04d%02d%02d", dqc.tpoint2_file,
                    gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                    gm.get(Calendar.DAY_OF_MONTH));

            try {
                if (fp == null) {
                    File fo = new File(fbuf.toString());
                    fo.setReadable(true, false);
                    fo.setWritable(true, false);
                    fp = new BufferedWriter(new FileWriter(fo));
                }
                for (m = 0; m < max_tstations; m++) {

                    old_time = dqc.tdata[j].data_time;
                    gm.setTime(old_time);
                    gm.add(Calendar.SECOND, -86400);
                    pbuf.setLength(0);
                    pbuf.append(String.format("TAI%s%cZZ", type,
                            dqc.temperature_stations.get(m).parm.charAt(4)));

                    if (dqcEndingObsTime == 12) {
                        buf.setLength(0);
                        buf.append(String.format(
                                ".ER %s %02d%02d%02d DH18/%s/DIH+6/",
                                dqc.temperature_stations.get(m).hb5,
                                gm.get(Calendar.YEAR),
                                gm.get(Calendar.MONTH) + 1,
                                gm.get(Calendar.DAY_OF_MONTH), pbuf.toString()));
                    } else {
                        buf.setLength(0);
                        buf.append(String.format(
                                ".ER %s %02d%02d%02d DH12/%s/DIH+6/",
                                dqc.temperature_stations.get(m).hb5,
                                gm.get(Calendar.YEAR),
                                gm.get(Calendar.MONTH) + 1,
                                gm.get(Calendar.DAY_OF_MONTH), pbuf.toString()));
                    }
                    for (k = 0; k < 4; k++) {
                        if (dqc.tdata[j].used[k] == 0) {
                            mbuf.setLength(0);
                            mbuf.append("    ");
                            if (k != 3) {
                                mbuf.append("/");
                            }
                        } else if (dqc.tdata[j].tstn[m].tlevel2[k].data == -99) {
                            mbuf.setLength(0);
                            mbuf.append("   M ");
                            if (k != 3) {
                                mbuf.append("/");
                            }
                        } else {
                            mbuf.setLength(0);
                            mbuf.append(String.format("%3d",
                                    (int) dqc.tdata[j].tstn[m].tlevel2[k].data));
                            if (dqc.tdata[j].tstn[m].tlevel2[k].qual == 8) {
                                mbuf.append("V");
                            }

                            if (dqc.tdata[j].tstn[m].tlevel2[k].qual == 1) {
                                mbuf.append("F");
                            }

                            if (dqc.tdata[j].tstn[m].tlevel2[k].qual == 3) {
                                mbuf.append("Q");
                            }

                            if (dqc.tdata[j].tstn[m].tlevel2[k].qual == 0) {
                                mbuf.append("S");
                            }

                            if (dqc.tdata[j].tstn[m].tlevel2[k].qual == 5) {
                                mbuf.append("E");
                            }

                            if (dqc.tdata[j].tstn[m].tlevel2[k].qual == 6) {
                                mbuf.append("L");
                            }

                            if (k != 3) {
                                mbuf.append("/");
                            }

                        }

                        buf.append(mbuf.toString());

                    }

                    fp.write(buf.toString());
                    fp.newLine();

                    old_time = dqc.tdata[j].data_time;
                    gm.setTime(old_time);

                    pbuf.setLength(0);
                    pbuf.append(String.format("TAI%s%cXZ", type,
                            dqc.temperature_stations.get(m).parm.charAt(4)));

                    buf.setLength(0);
                    buf.append(String.format(".AR %s %02d%02d%02d DH12/%s ",
                            dqc.temperature_stations.get(m).hb5,
                            gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                            gm.get(Calendar.DAY_OF_MONTH), pbuf.toString()));

                    if (dqc.tdata[j].used[4] == 0) {
                        mbuf.setLength(0);
                        mbuf.append("    ");
                    } else if (dqc.tdata[j].tstn[m].tlevel2[4].data == -99) {
                        mbuf.setLength(0);
                        mbuf.append("   M ");
                    } else {
                        mbuf.setLength(0);
                        mbuf.append(String.format("%3d",
                                (int) dqc.tdata[j].tstn[m].tlevel2[4].data));

                        if (dqc.tdata[j].tstn[m].tlevel2[4].qual == 8) {
                            mbuf.append("V");
                        }

                        if (dqc.tdata[j].tstn[m].tlevel2[4].qual == 1) {
                            mbuf.append("F");
                        }

                        if (dqc.tdata[j].tstn[m].tlevel2[4].qual == 3) {
                            mbuf.append("Q");
                        }

                        if (dqc.tdata[j].tstn[m].tlevel2[4].qual == 0) {
                            mbuf.append("S");
                        }

                        if (dqc.tdata[j].tstn[m].tlevel2[4].qual == 5) {
                            mbuf.append("E");
                        }

                        if (dqc.tdata[j].tstn[m].tlevel2[4].qual == 6) {
                            mbuf.append("L");
                        }

                    }

                    buf.append(mbuf);
                    fp.write(buf.toString());
                    fp.newLine();

                    old_time = dqc.tdata[j].data_time;
                    gm.setTime(old_time);

                    pbuf.setLength(0);
                    mbuf.setLength(0);
                    buf.setLength(0);

                    pbuf.append(String.format("TAI%s%cNZ", type,
                            dqc.temperature_stations.get(m).parm.charAt(4)));

                    buf.append(String.format(".AR %s %02d%02d%02d DH12/%s ",
                            dqc.temperature_stations.get(m).hb5,
                            gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                            gm.get(Calendar.DAY_OF_MONTH), pbuf.toString()));

                    if (dqc.tdata[j].used[5] == 0) {
                        mbuf.append("    ");
                    } else if (dqc.tdata[j].tstn[m].tlevel2[5].data == -99) {
                        mbuf.append("   M ");
                    } else {
                        mbuf.setLength(0);
                        mbuf.append(String.format("%3d",
                                (int) dqc.tdata[j].tstn[m].tlevel2[5].data));
                        if (dqc.tdata[j].tstn[m].tlevel2[5].qual == 8) {
                            mbuf.append("V");
                        }

                        if (dqc.tdata[j].tstn[m].tlevel2[5].qual == 1) {
                            mbuf.append("F");
                        }

                        if (dqc.tdata[j].tstn[m].tlevel2[5].qual == 3) {
                            mbuf.append("Q");
                        }

                        if (dqc.tdata[j].tstn[m].tlevel2[5].qual == 0) {
                            mbuf.append("S");
                        }

                        if (dqc.tdata[j].tstn[m].tlevel2[5].qual == 5) {
                            mbuf.append("E");
                        }

                        if (dqc.tdata[j].tstn[m].tlevel2[5].qual == 6) {
                            mbuf.append("L");
                        }

                    }

                    buf.append(mbuf);
                    fp.write(buf.toString());
                    fp.newLine();

                }

                fp.close();
                fp = null;
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "ERROR: There was a problem creating/writing out the file "
                                + fbuf + ". Possible file permissions issue "
                                + e.getMessage(), e);
                return;
            } finally {
                try {
                    if (fp != null) {
                        fp.close();
                    }

                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            /* build map file */
            foundt: for (m = 0; mean_areal_precip_global[m].hb5 != ""; m++) {

                for (k = 0; k < 4; k++) {

                    num = j * 4 + 3 - k;

                    if (mean_areal_precip_global[m].tmaps_done[num] == 1) {
                        break foundt;
                    }

                }

            }

            if (mean_areal_precip_global[m].hb5 == "") {
                continue;
            }

            gm.setTime(dqc.tdata[j].data_time);

            fbuf = String.format("%s%04d%02d%02d", dqc.mat_file,
                    gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                    gm.get(Calendar.DAY_OF_MONTH));

            try {
                if (fp == null) {
                    File fo = new File(fbuf.toString());
                    fo.setReadable(true, false);
                    fo.setWritable(true, false);
                    fp = new BufferedWriter(new FileWriter(fo));
                }
                /*
                 * at least processing level a data is available but no
                 * guarantee that MAPs have been rendered
                 */
                old_time = dqc.tdata[j].data_time;
                gm.setTime(old_time);
                gm.add(Calendar.SECOND, -86400);

                /* loop through and write maps to file */
                for (m = 0; m < dqc.getMax_basins(); m++) {
                    numzones = 0;

                    for (l = 0; l < 4; l++) {

                        if (mean_areal_precip_global[m].zones[l] < 0) {
                            continue;
                        }

                        numzones++;

                    }

                    for (l = 0; l < 4; l++) {

                        if (mean_areal_precip_global[m].zones[l] < 0) {
                            continue;
                        }

                        xbuf.setLength(0);
                        xbuf.append(".ER ");
                        xbuf.append(mean_areal_precip_global[m].hb5);

                        String tb = xbuf.toString().toUpperCase();
                        xbuf.setLength(0);
                        xbuf.append(tb);

                        /* fix for polygon basins */

                        if (dqc.mpe_rfc_name.equalsIgnoreCase("cbrfc")
                                && mean_areal_precip_global[m].hb5.length() == 7) {
                            // logMessage ("polygon write\n");
                            xbuf.append(mean_areal_precip_global[m].bchar
                                    .charAt(0));

                        }

                        else {
                            if (l == 0 && numzones != 1) {
                                xbuf.append("L");
                                xbuf.append(mean_areal_precip_global[m].bchar
                                        .charAt(0));

                            } else if (l == 0 && numzones == 1) {
                                xbuf.append("O");
                                xbuf.append(mean_areal_precip_global[m].bchar
                                        .charAt(0));

                            } else if (l == 1) {
                                xbuf.append("M");
                                xbuf.append(mean_areal_precip_global[m].bchar
                                        .charAt(1));

                            } else if (l == 2) {
                                xbuf.append("U");
                                xbuf.append(mean_areal_precip_global[m].bchar
                                        .charAt(2));

                            } else if (l == 3) {
                                xbuf.append("G");
                                xbuf.append(mean_areal_precip_global[m].bchar
                                        .charAt(3));

                            }

                        }

                        mbuf.setLength(0);
                        mbuf.append(String.format(" %02d%02d%02d",
                                gm.get(Calendar.YEAR),
                                gm.get(Calendar.MONTH) + 1,
                                gm.get(Calendar.DAY_OF_MONTH)));

                        xbuf.append(mbuf.toString());

                        if (dqcEndingObsTime == 12) {
                            mbuf.setLength(0);
                            mbuf.append(" DH18/TAIPBZZ/DIH+6");

                        } else {
                            mbuf.setLength(0);
                            mbuf.append(" DH12/TAIPBZZ/DIH+6");

                        }

                        xbuf.append(mbuf.toString());

                        for (k = 0; k < 4; k++) {

                            int h = j * 4 + (3 - k);

                            if (dqc.mpe_rfc_name.equalsIgnoreCase("cbrfc")
                                    && mean_areal_precip_global[m].hb5.length() == 7) {

                                temp = mean_areal_precip_global[m].tlz[h];

                            } else if (l == 0) {
                                temp = mean_areal_precip_global[m].tlz[h];

                            } else if (l == 1) {
                                temp = mean_areal_precip_global[m].tmz[h];

                            } else if (l == 2) {
                                temp = mean_areal_precip_global[m].tuz[h];

                            } else if (l == 3) {
                                temp = mean_areal_precip_global[m].tgz[h];

                            }

                            if (mean_areal_precip_global[m].tmaps_done[h] != 1) {
                                mbuf.setLength(0);
                                mbuf.append("    ");

                            } else {
                                mbuf.setLength(0);
                                mbuf.append(String.format("%4.1f ", temp));

                            }

                            if (k < 4) {
                                xbuf.append("/");

                            }

                            xbuf.append(mbuf.toString());

                        }

                        fp.write(xbuf.toString());
                        fp.newLine();
                    }
                }

                fp.close();
                fp = null;
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "ERROR: There was a problem creating/writing out the file "
                                + fbuf + ". Possible file permissions issue "
                                + e.getMessage(), e);
                return;
            } finally {
                try {
                    if (fp != null) {
                        fp.close();
                    }

                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            for (m = 0; m < 6; m++) {

                if (dqc.tdata[j].used[m] != 0) {
                    dqc.tdata[j].used[m] = 4;

                }

            }

            btv.restore_bad_tvalues(j, dqc.temperature_stations, max_tstations);

            archive_flag[j] = 1;

        }

        // Execute script, added CHPS support.
        executeScript();

        int pcpn_time_step = MPEDisplayManager.pcpn_time_step;
        int pcp_flag = dqc.pcp_flag;

        int pcpn_day = dqc.pcpn_day;

        if (DailyQcUtils.qpf_flag == true || DailyQcUtils.z_flag == true
                || DailyQcUtils.maxmin_flag == true) {

            if (DailyQcUtils.qpf_flag == true) {
                QcPrecipOptionsDialog.dataSet.clear();
                QcPrecipOptionsDialog.dataSet
                        .addAll(QcPrecipOptionsDialog.dataType);

                if (pcpn_time_step == 0) {
                    time_pos = pcp_flag;
                } else {
                    time_pos = 40 + pcpn_day;
                }

            }

            else if (DailyQcUtils.z_flag == true) {
                QcFreezeOptionsDialog.dataSet.clear();
                QcFreezeOptionsDialog.dataSet
                        .addAll(QcPrecipOptionsDialog.dataType);

                time_pos = 100 + pcp_flag;
            } else if (DailyQcUtils.maxmin_flag == true) {
                QcTempOptionsDialog.dataSet.clear();
                QcTempOptionsDialog.dataSet
                        .addAll(QcPrecipOptionsDialog.dataType);

                if (pcpn_time_step == 1) {
                    time_pos = 190 + pcpn_day;
                }

                else if (pcpn_time_step == 2) {
                    time_pos = 200 + pcpn_day;
                }

                else {
                    time_pos = 150 + pcp_flag;
                }

            }

            int points_flag = dqc.points_flag;
            int grids_flag = dqc.grids_flag;
            int map_flag = dqc.map_flag;

            if (points_flag == 1 && pcp_in_use[time_pos] == -1) {
                k = 0;
            }

            else if (points_flag == 1 && grids_flag == -1 && map_flag == -1) {
                k = 0;
            }

            else if (points_flag == -1 && grids_flag == 1 && map_flag == -1) {
                k = 1;
            }

            else if (points_flag == -1 && grids_flag == -1 && map_flag == 1) {
                k = 2;
            }

            else if (points_flag == 1 && grids_flag == 1 && map_flag == -1) {
                k = 3;
            }

            else if (points_flag == 1 && grids_flag == -1 && map_flag == 1) {
                k = 4;
            }

            else if (points_flag == -1 && grids_flag == -1 && map_flag == -1) {
                k = 5;
            }

            if (DailyQcUtils.qpf_flag == true
                    && QcPrecipOptionsDialog.isOpen == true) {
                QcPrecipOptionsDialog.selectDataSetVal(k);
            }
        }

        Disagg6Hr d6h = new Disagg6Hr();
        try {
            d6h.disagg6hr();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        /* Free DailyQC resources. */

        // if (MPEDisplayManager.getCurrent().isQpf() == true) {
        // MPEDisplayManager.getCurrent().setQpf(false);
        // }
        //
        // if (MPEDisplayManager.getCurrent().isMaxmin() == true) {
        // MPEDisplayManager.getCurrent().setMaxmin(false);
        // }
        //
        // if (MPEDisplayManager.getCurrent().isZflag() == true) {
        // MPEDisplayManager.getCurrent().setZflag(false);
        // }

        // FreeDQCData dqd = new FreeDQCData();
        // dqd.free_dqc_data();

        // if (QcPrecipOptionsDialog.isOpen == true) {
        // QcPrecipOptionsDialog.destroy(false);
        // }
        dqc.isom = save_isom;
    }

    /**
     * 
     */
    public void executeScript() {
        String mpeExecuteScript = apps_defaults
                .getToken("mpe_dqc_execute_internal_script");
        String pproc_local_bin = apps_defaults.getToken("pproc_local_bin");
        String script = String
                .format("%s/mpe_internal_script", pproc_local_bin);

        if (mpeExecuteScript.equalsIgnoreCase(ON)) {
            try {
                // DR#10955
                RunProcess.getRunProcess().exec(script);
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }

        } else {
            return;
        }
    }

    private void cancel_dbase() {
        // String qcarea = dqc.currentQcArea;
        qdays = dqc.qcDays;
        currntDate = MPEDisplayManager.getCurrent().getCurrentEditDate();

        /* The datasets have all been QC'd and saved to the database. */
        /* Go ahead and load the new DailyQC dataset. */
        // if (MPEDisplayManager.getCurrent().isQpf() == true) {
        // MPEDisplayManager.getCurrent().setQpf(false);
        // }
        //
        // if (MPEDisplayManager.getCurrent().isMaxmin() == true) {
        // MPEDisplayManager.getCurrent().setMaxmin(false);
        // }
        //
        // if (MPEDisplayManager.getCurrent().isZflag() == true) {
        // MPEDisplayManager.getCurrent().setZflag(false);
        // }

        // /* Free the data from the previous run. */
        // DailyQcUtils dqc = new DailyQcUtils();
        // dqc.qcDataReload(currntDate, qcarea, qdays);

        // FreeDQCData dqd = new FreeDQCData();
        // dqd.free_dqc_data();
    }

    // private void ok_dbase() {
    // Date prevDate = ChooseDataPeriodDialog.prevDate;
    // DailyQcUtils dqc = new DailyQcUtils();
    // dqc.qcDataReload(prevDate, qarea, qdays);
    // }
}
