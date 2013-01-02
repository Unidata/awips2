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

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.Pseudogageval;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
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
 * Aug 8, 2012   15271	   snaples     Updated hourly slot
 * Jan 02, 2013	 15565     snaples     Fixed problem with wrong time being sent to mpe_fieldgen
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class RegenHrFlds {

    private String obsdate;

    private Date datetime;

    private static final char manual_qc_code = 'M';

    private static final char zero_offset_code = '0';

    private static final String psstr = "PSEUDO";

    private static final float MISSING_PRECIP = -9999f;

    final int positive_offset_base = 'A';

    final int negative_offset_base = 'a';

    double hourly_value;

    float new_pseudo_value;

    double pp_value;

    private static final int NUM_HOURLY_SLOTS = 24;

    private static final int NUM_6HOURLY_SLOTS = 4;

    GagePPOptions options = new GagePPOptions();

    Hourlypp hourlyPP;

    int hour_slot;

    int i;

    int len;

    int j;

    int pp_1hr_dur = 1001;

    int split;

    int status;

    int num_gage_edit;

    long irc;

    long num_update;

    double new_hourly_value;

    short revision;

    short[] revision_6hour = new short[4];

    Map<String, MPEGageData> gages;

    MPEDataManager.MPEGageData gData;

    Hourlypp ppData;

    private static SimpleDateFormat sdf;

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

    public void checkGages() {

        /* Test if any gages have been edited */
        num_gage_edit = 0;
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

    public double round(final double x) {
        double tmp = x + 0.5;
        tmp = java.lang.Math.floor(tmp);
        return tmp;
    }

    public void regenFields(Date dt) throws VizException {

        datetime = dt;
        sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        obsdate = sdf.format(datetime);

        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        Cursor prev = shell.getCursor();
        Cursor wait = new Cursor(Display.getDefault(), SWT.CURSOR_WAIT);
        this.checkGages();
        boolean ref = MPEDataManager.getInstance().isRadarEditFlag();
        /* Store any gage edits into the HourlyPP or PseudoGageVal table. */
        if (num_gage_edit > 0 || ref == true) {
            options.shef_duplicate = shef_dup.USE_REVCODE;

            for (MPEGageData gData : gages.values()) {

                if (!gData.edit.equals("")) {
                    if (gData.id.contains(psstr)) {
                        /*
                         * The values are stored differently for pseudo gages.
                         */
                        if (gData.edit.equalsIgnoreCase("m")) {
                            new_pseudo_value = -999f;
                        } else {
                            UnitConverter conv = NonSI.INCH
                                    .getConverterTo(SI.MILLIMETER);
                            new_pseudo_value = (float) conv.convert(Double
                                    .parseDouble(gData.edit));
                        }

                        try {
                            this.update_pseudo_gages(gData.id, datetime,
                                    new_pseudo_value);
                        }

                        catch (Exception e) {
                            throw new VizException("In class RegenHrFlds : "
                                    + "Could not update record in "
                                    + "PseudoGageVal table. " + "Gage id: "
                                    + gData.id + " obstime "
                                    + sdf.format(datetime) + " value "
                                    + new_pseudo_value);
                        }
                    } else {
                        if (gData.edit.equalsIgnoreCase("m")) {
                            new_hourly_value = MISSING_PRECIP;
                            pp_value = MISSING_PRECIP;
                        } else if (gData.edit.equalsIgnoreCase("b")) {
                            continue;
                        } else {
                            pp_value = Double.parseDouble(gData.edit);
                            hourly_value = pp_value * 100.0;
                            hourly_value = round(hourly_value);
                            pp_value = round(pp_value);
                            new_hourly_value = (int) hourly_value;
                        }

                        /*
                         * Update records in HourlyPP and/or PseudoGageRadarVal
                         * tables.
                         */
                        hour_slot = GagePPWrite.gage_pp_init(datetime,
                                gData.id, gData.ts, new_hourly_value, obsdate,
                                zero_offset_code, manual_qc_code);

                        for (j = 0; j < NUM_6HOURLY_SLOTS; ++j) {
                            revision_6hour[j] = 0;
                        }

                        revision = 1;

                        try {
                            GagePPWrite.gage_pp_write_rec("PP", gData.id,
                                    gData.ts, datetime, options, revision,
                                    revision_6hour, new_hourly_value, pp_value);
                        } catch (Exception e) {
                            throw new VizException("In class RegenHrFlds: "
                                    + "The call to 'gage_pp_write_rec' "
                                    + "failed for the following reason: " + e);
                        }

                        /*
                         * Create/update a PP record in the RawPP table for this
                         * report.
                         */
                        try {
                            this.update_rawPP(datetime, gData.id, gData.ts,
                                    manual_qc_code, pp_1hr_dur, pp_value);
                        } catch (Exception e) {
                            throw new VizException("In class RegenHrFlds: "
                                    + "the call to 'update_RawPP' "
                                    + "failed for the following for the gage: "
                                    + gData.id);
                        }
                    }
                }
            }

            MPEDataManager.getInstance().setRadarEditFlag(false);
            /*-------------------------------------------------------------------------*/
            /* Read Gage Data and store in structure */
            /*-------------------------------------------------------------------------*/
            Calendar cl = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            cl.setTimeInMillis(datetime.getTime());
            int hh = cl.get(Calendar.HOUR_OF_DAY);
            String hour = "" + hh;
            if (hh < 10) {
                hour = "0" + hh;
            }
            sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
            SimpleDateFormat dr = new SimpleDateFormat("MMddyyyy");
            dr.setTimeZone(TimeZone.getTimeZone("GMT"));
            String drr = "1 " + hour + " " + dr.format(datetime);
            System.out.println("Regen args are " + drr);
            MpeFieldGenJob regen = new MpeFieldGenJob(drr);
            shell.setCursor(wait);
            regen.schedule();
            try {
                regen.join();
            } catch (InterruptedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

            /* Clear gage edits */
            MPEDataManager.getInstance().clearEditGages();
            shell.setCursor(prev);
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
        Calendar dt = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        dt.setTime(dto);
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        int hr = dt.get(Calendar.HOUR_OF_DAY);

        //
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
