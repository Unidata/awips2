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
package com.raytheon.viz.mpe.core;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.apache.commons.lang.StringUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.shef.tables.Pseudogageval;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.whfslib.GagePPOptions;
import com.raytheon.viz.hydrocommon.whfslib.GagePPOptions.shef_dup;
import com.raytheon.viz.hydrocommon.whfslib.GagePPWrite;
import com.raytheon.viz.mpe.core.MPEDataManager.MPEGageData;

/**
 * FUNCTION: Regenerate Hour Field calculations
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2008            snaples     Initial creation
 * Aug 08, 2012  15271     snaples     Updated hourly slot
 * Jan 02, 2013  15565     snaples     Fixed problem with wrong time being sent to mpe_fieldgen
 * Mar 14, 2013   1457     mpduff      Fixed memory leak.
 * Jun 18, 2013  16053     snaples     Removed check for Radar Edit flag
 * Apr 24, 2014  16308     lbousaidi   added the ability to send RFC Bias across the WAN
 *                                     after Mpe fieldgen run.
 * Feb 29, 2016   2347     skorolev    Removed extra conversion in the regenFields.
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class RegenHrFlds {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegenHrFlds.class);

    private static final char manual_qc_code = 'M';

    private static final char zero_offset_code = '0';

    private static final String PSSTR = "PSEUDO";

    private static final float MISSING_PRECIP = -9999f;

    final int positive_offset_base = 'A';

    final int negative_offset_base = 'a';

    private static final int NUM_6HOURLY_SLOTS = 4;

    private GagePPOptions options = new GagePPOptions();

    private int pp_1hr_dur = 1001;

    private Map<String, MPEGageData> gages;

    private static final SimpleDateFormat sdf = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss");

    private static RegenHrFlds instance;

    private List<Pseudogageval> pseudoList;

    public RegenHrFlds() {
        // empty constructor
    }

    /**
     * Singleton constructor.
     * 
     * @return the regenHrFlds instance variable.
     */
    public static synchronized RegenHrFlds getInstance() {
        if (instance == null) {
            instance = new RegenHrFlds();
        }

        return instance;
    }

    /**
     * Check gages.
     * 
     * @param obsdate
     *            observation date
     */
    public void checkGages(String obsdate) {
        /* Test if any gages have been edited */
        int num_gage_edit = 0;
        gages = MPEDataManager.getInstance().readEditGages();

        for (MPEGageData gData : gages.values()) {
            if (!gData.edit.equals("")) {
                num_gage_edit++;
            }
        }
        String where = " WHERE obstime='" + obsdate + "' ";
        pseudoList = MPEDataManager.getInstance().getPseudoGageVal(where);
        int pCount = pseudoList.size();
        num_gage_edit += pCount;
    }

    /**
     * Regenerate fields.
     * 
     * @param dt
     *            observation date
     * @throws VizException
     */
    public void regenFields(Date dt) throws VizException {

        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        String obsdate = sdf.format(dt);

        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        this.checkGages(obsdate);
        /* Store any gage edits into the HourlyPP or PseudoGageVal table. */
        options.shef_duplicate = shef_dup.USE_REVCODE;

        for (MPEGageData gData : gages.values()) {
            if (!gData.edit.equals("")) {
                double new_hourly_value = MISSING_PRECIP;
                double pp_value = MISSING_PRECIP;
                float new_pseudo_value = MISSING_PRECIP;
                if (gData.id.contains(PSSTR)) {
                    /*
                     * The values are stored differently for pseudo gages.
                     */
                    if (!gData.edit.equalsIgnoreCase("m")) {
                        try {
                            new_pseudo_value = Float.parseFloat(gData.edit);
                        } catch (NumberFormatException e) {
                            statusHandler
                                    .handle(Priority.PROBLEM,
                                            "Error getting pseudo value: "
                                                    + gData.edit, e);
                        }
                    }
                    this.update_pseudo_gages(gData.id, dt, new_pseudo_value);
                } else {
                    if (gData.edit.equalsIgnoreCase("b")) {
                        continue;
                    } else if (!gData.edit.equalsIgnoreCase("m")) {
                        try {
                            pp_value = Double.parseDouble(gData.edit);
                        } catch (NumberFormatException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Error getting PP value: " + pp_value, e);
                        }
                        double hourly_value = pp_value * 100.0;
                        hourly_value = java.lang.Math.round(hourly_value);
                        pp_value = java.lang.Math.round(pp_value);
                        new_hourly_value = (int) hourly_value;
                    }
                    GagePPWrite.gage_pp_init(dt, gData.id, gData.ts,
                            new_hourly_value, obsdate, zero_offset_code,
                            manual_qc_code);

                    short[] revision_6hour = new short[NUM_6HOURLY_SLOTS];
                    short revision = 1;

                    GagePPWrite.gage_pp_write_rec("PP", gData.id, gData.ts, dt,
                            options, revision, revision_6hour,
                            new_hourly_value, pp_value);
                    /*
                     * Create/update a PP record in the RawPP table for this
                     * report.
                     */
                    this.update_rawPP(dt, gData.id, gData.ts, manual_qc_code,
                            pp_1hr_dur, pp_value);
                }
            }
        }

        /*-------------------------------------------------------------------------*/
        /* Read Gage Data and store in structure */
        /*-------------------------------------------------------------------------*/
        Calendar cl = TimeUtil.newGmtCalendar(dt);
        cl.setTimeInMillis(dt.getTime());
        String hh = Integer.toString(cl.get(Calendar.HOUR_OF_DAY));
        String hour = StringUtils.leftPad(hh, 2, '0');
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        SimpleDateFormat dr = new SimpleDateFormat("MMddyyyy");
        dr.setTimeZone(TimeZone.getTimeZone("GMT"));
        String drr = "1 " + hour + " " + dr.format(dt);
        statusHandler.info("Regen args are " + drr);
        try {
            MpeFieldGenJob regen = new MpeFieldGenJob(drr);
            shell.setCursor(shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
            regen.schedule();
            try {
                regen.join();
            } catch (InterruptedException e) {
                statusHandler
                        .handle(Priority.PROBLEM, "MPE job " + regen.getName()
                                + " has been interrupted.", e);
            }
            /*-----------------------------------------------------------------------*
             * send RFC Bias across the WAN if the tokens TRANSMIT_BIAS_ON_RERUN and
             * MPE_TRANSMIT_BIAS are both ON. The script rerun_mpe_fieldgen only run
             * transmit_rfc_bias but it doesn't rerun mpe_fieldgen
             * ----------------------------------------------------------------------*/
            AppsDefaults appsDefaults = AppsDefaults.getInstance();
            String scriptDir = appsDefaults.getToken("pproc_bin");
            String scriptName = "rerun_mpe_fieldgen";
            String transmitRun = dr.format(dt);
            String script = scriptDir + File.separator + scriptName;
            ProcessBuilder pb = new ProcessBuilder(script, hour, transmitRun);
            try {
                pb.start();
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "MPE: Error to start script: " + script, e);
            }
        } finally {
            MPEDataManager.getInstance().clearEditGages();
            shell.setCursor(null);
        }
    }

    /**
     * Update Pseudo gage values in db.
     * 
     * @param id
     *            Gage Id
     * @param datetime
     *            Current date time for update
     * @param new_pseudo_value
     *            New value for pseudo gage
     * @throws VizException
     */
    private void update_pseudo_gages(String id, Date datetime,
            float new_pseudo_value) throws VizException {
        String where = "WHERE pseudo_gage_id='" + id + "' AND obstime='"
                + sdf.format(datetime) + "'";
        if (!MPEDataManager.getInstance().getPseudoGageVal(where).isEmpty()) {
            where = "set gage_Value=" + new_pseudo_value + ", man_Edited="
                    + "'T'" + " WHERE pseudo_gage_id='" + id
                    + "' AND obstime='" + sdf.format(datetime) + "'";
            MPEDataManager.getInstance().updatePseudoGageVal(where);
        } else {
            throw new VizException(
                    "Could not retrieve record from PseudoGageVal"
                            + " for Gage Id:" + id + "and obstime:"
                            + sdf.format(datetime));
        }
    }

    /**
     * Create or Update existing record in the RawPP table.
     * 
     * @param datetime
     *            Current date time of record update.
     * @param id
     *            Gage Id
     * @param ts
     *            Type source
     * @param manual_qc_code
     *            Manual Qc code
     * @param pp_1hr_dur
     *            duration
     * @param pp_value
     *            precip value
     */
    private void update_rawPP(Date datetime, String id, String ts,
            char manual_qc_code, int pp_1hr_dur, double pp_value) {

        Date dto = new Date(datetime.getTime());
        Calendar dt = TimeUtil.newGmtCalendar(dto);
        int hr = dt.get(Calendar.HOUR_OF_DAY);
        if (hr == 0) {
            hr = 24;
            dt.add(Calendar.DAY_OF_MONTH, -1);
        }

        String where = "WHERE lid='" + id + "' AND pe='PP'" + " AND dur="
                + pp_1hr_dur + " AND ts='" + ts + "' AND extremum='Z'"
                + " AND obstime='" + sdf.format(dt.getTime()) + "'";
        List<Rawpp> rawpp_rec = MPEDataManager.getInstance().getRawPP(where);

        if (rawpp_rec.isEmpty()) {
            where = "(lid, pe, dur, ts, extremum, obstime, producttime, shef_Qual_Code, value, revision, product_Id, quality_Code)"
                    + " values ('"
                    + id
                    + "','PP'"
                    + ","
                    + pp_1hr_dur
                    + ",'"
                    + ts
                    + "','Z'"
                    + ",'"
                    + sdf.format(dt.getTime())
                    + "','"
                    + sdf.format(dt.getTime())
                    + "','"
                    + manual_qc_code
                    + "',"
                    + pp_value
                    + ",1"
                    + ",'MPEUPDATE'"
                    + ","
                    + ShefConstants.QC_MANUAL_PASSED + ")";
            MPEDataManager.getInstance().insertRawPP(where);

        } else {

            where = "set value=" + pp_value + ",shef_Qual_Code='"
                    + manual_qc_code + "',revision=1" + ",quality_Code="
                    + ShefConstants.QC_MANUAL_PASSED + ",producttime='"
                    + sdf.format(dt.getTime()) + "',postingtime='"
                    + sdf.format(dt.getTime()) + "' WHERE lid='" + id
                    + "' AND pe='PP'" + " AND dur=" + pp_1hr_dur + " AND ts='"
                    + ts + "' AND extremum='Z'" + " AND obstime='"
                    + sdf.format(dt.getTime()) + "'";
            MPEDataManager.getInstance().updateRawPP(where);
        }
    }

}
