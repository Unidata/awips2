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
package com.raytheon.viz.hydrocommon.whfslib;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.HourlypcId;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.HourlyppId;
import com.raytheon.uf.common.dataplugin.shef.tables.IHourlyTS;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.whfslib.GagePPOptions.shef_dup;
import com.raytheon.viz.hydrocommon.whfslib.GagePPOptions.upd_action;

/**
 * TODO
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 05, 2008   1649     snaples     Initial creation
 * Aug 08, 2012   15271    snaples     Updated hourly slot
 * May 27, 2014   3133     njensen     Removed dead code
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public final class GagePPWrite {

    private static IHourlyTS hourly_rec;

    private static Date datetime;

    private static final float MISSING_PRECIP = -9999f;

    private static final int MINUTES_PER_HOUR = 60;

    private static int status;

    private static final char zero_offset = '0';

    private static final int positive_offset_base = 'A';

    private static final int negative_offset_base = 'a';

    private static final int NUM_HOURLY_SLOTS = 24;

    private static final int NUM_6HOURLY_SLOTS = 4;

    private static char hourly_qc[] = new char[NUM_HOURLY_SLOTS];

    private static char minute_offset[] = new char[NUM_HOURLY_SLOTS];

    private static char sixhr_qc[] = new char[NUM_6HOURLY_SLOTS];

    private static char sixhr_offset[] = new char[NUM_6HOURLY_SLOTS];

    private static List<Hourlypc> hour_PC_old = new ArrayList<Hourlypc>();

    private static List<Hourlypp> hour_PP_old = new ArrayList<Hourlypp>();

    private static Hourlypp hpp = null;

    /**
     * Empty constructor.
     */
    public GagePPWrite() {
    }

    /**
     * Creates a new Hourlypp record or Updates existing record based on data
     * passed to it.
     * 
     * @param id
     *            gage id
     * @param ts
     *            type source
     * @param obsdate
     *            observation date
     * @param options
     *            GagePP Options
     * @param revision
     *            hourly revision code
     * @param revision_6hour
     *            six hour revision code
     * @param new_hourly_value
     *            new hourly precip value
     * @param pp_value
     *            new six hour precip value
     */
    public static void gage_pp_write_rec(String pe, String id, String ts,
            Date obsdate, GagePPOptions options, short revision,
            short revision_6hour[], double new_hourly_value, double pp_value) {

        int is_pc = 0;
        int six = 0;
        Date dto = new Date(obsdate.getTime());

        Calendar dt = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        dt.setTime(dto);
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        GagePPOptions opts = options;
        int hr = dt.get(Calendar.HOUR_OF_DAY);
        int min = dt.get(Calendar.MINUTE);

        //
        if (hr == 0) {
            hr = 24;
            dt.add(Calendar.DAY_OF_MONTH, -1);
            dto = dt.getTime();
        }

        String obstime = sdf.format(dto);

        char sixhroffset = get_offset_code(min);
        char sixhrqc = 'M';
        char minoff = sixhroffset;
        char qcc = sixhrqc;
        hourly_rec = null;

        //
        String where = "WHERE lid='" + id + "' AND ts='" + ts
                + "' AND obsdate ='" + obstime + "'";

        if (hr >= 0 && hr <= 6) {
            six = 0;
        } else if (hr > 6 && hr <= 12) {
            six = 1;
        } else if (hr > 12 && hr <= 18) {
            six = 2;
        } else {
            six = 3;
        }

        if (pe.equalsIgnoreCase("PC")) {
            is_pc = 1;
        }
        if (is_pc == 1) {
            hour_PC_old = null;
            hour_PC_old = IHFSDbGenerated.GetHourlyPC(where);
            if (hour_PC_old.size() >= 1) {
                hourly_rec = hour_PC_old.get(0);
            }
        } else {
            hour_PP_old = null;
            hour_PP_old = IHFSDbGenerated.GetHourlyPP(where);
            if (hour_PP_old.size() >= 1) {
                hourly_rec = hour_PP_old.get(0);
            }
        }

        if (hourly_rec == null) {
            setMinOffset(minute_offset, hr, minoff);
            setHourlyQC(hourly_qc, hr, qcc);
            sixhr_offset[six] = sixhroffset;
            sixhr_qc[six] = sixhrqc;

            if (is_pc == 1) {
                Hourlypc pHourpc = new Hourlypc();
                HourlypcId pHid = new HourlypcId();
                pHid.setLid(id);
                pHid.setTs(ts);
                pHid.setObsdate(dto);
                pHourpc.setId(pHid);
                pHourpc.setMinuteOffset(String.valueOf(minute_offset));
                pHourpc.setHourlyQc(String.valueOf(hourly_qc));
                set_hour_slot_value(pHourpc, hr, new_hourly_value);
                update_gage_rec(pHourpc);

            } else {
                Hourlypp pHourpp = new Hourlypp();
                HourlyppId pHid = new HourlyppId();
                pHid.setLid(id);
                pHid.setTs(ts);
                pHid.setObsdate(dto);
                pHourpp.setId(pHid);
                pHourpp.setMinuteOffset(String.valueOf(minute_offset));
                pHourpp.setHourlyQc(String.valueOf(hourly_qc));
                pHourpp.setSixhroffset(String.valueOf(sixhr_offset));
                pHourpp.setSixhrqc(String.valueOf(sixhr_qc));
                set_hour_slot_value(pHourpp, hr, new_hourly_value);
                set_6hour_slot_value(pHourpp, six, pp_value);
                update_gage_rec(pHourpp);
            }

        } else {

            double old_hr_value = -7700;
            char offset[] = new char[NUM_HOURLY_SLOTS];
            offset = minute_offset;
            char old_offset[] = new char[NUM_HOURLY_SLOTS];
            char qc[] = new char[NUM_HOURLY_SLOTS];
            qc = hourly_qc;
            char old_qc[] = new char[NUM_HOURLY_SLOTS];
            double hr_value = -7700;
            char prev_offset = ' ';
            char prev_qc = ' ';
            double six_hr_slot_val = -7700;
            double old_six_hr_val = -7700;
            char old_sixhroffset[] = new char[NUM_6HOURLY_SLOTS];
            char six_hr_qc[] = new char[NUM_6HOURLY_SLOTS];
            six_hr_qc = sixhr_qc;
            char old_six_qc[] = new char[NUM_6HOURLY_SLOTS];
            Arrays.fill(old_six_qc, '-');
            Arrays.fill(old_sixhroffset, '-');
            char prev_sixhroff = ' ';
            char prev_sixqc = ' ';

            if (hourly_rec != null) {
                // ppData = hourly_rec;
            } else {
                System.out.println("Hourly Rec is null.");
            }
            old_offset = hourly_rec.getMinuteOffset().toCharArray();
            int slot = getOffset(old_offset, hr);
            slot = hr - 1;
            prev_offset = old_offset[slot];
            old_qc = hourly_rec.getHourlyQc().toCharArray();
            int qcslot = getOffset(old_qc, hr);
            qcslot = hr - 1;
            prev_qc = old_qc[qcslot];

            int use_value = 1;

            if (get_hour_slot_value(hourly_rec, hr) != null) {
                old_hr_value = get_hour_slot_value(hourly_rec, hr);
                use_value = use_precip_value(new_hourly_value, old_hr_value,
                        qcc, prev_qc, minute_offset[slot], prev_offset,
                        opts.shef_duplicate.name(), revision);
            }

            if (use_value == 1) {
                hr_value = new_hourly_value;
                offset = old_offset;
                offset[slot] = minoff;
                qc = old_qc;
                qc[qcslot] = qcc;
            } else {
                hr_value = old_hr_value;
                offset = old_offset;
                qc = old_qc;
            }

            if (use_value == 1) {
                if (is_pc == 0) {
                    if (hourly_rec instanceof Hourlypp) {
                        hpp = (Hourlypp) hourly_rec;
                    }
                    if (hpp != null) {
                        if (hpp.getSixhroffset() != null) {
                            old_sixhroffset = hpp.getSixhroffset()
                                    .toCharArray();
                        }
                        if (hpp.getSixhrqc() != null) {
                            old_six_qc = hpp.getSixhrqc().toCharArray();
                        }
                        Short sixval = get_6hour_slot_value(hpp, six);
                        prev_sixhroff = old_sixhroffset[six];
                        prev_sixqc = old_six_qc[six];

                        if (sixval != null) {
                            if ((String.valueOf(prev_sixqc)
                                    .equalsIgnoreCase("M"))
                                    && !(String.valueOf(sixhrqc)
                                            .equalsIgnoreCase("M"))) {
                                use_value = 0;
                            } else {
                                status = compare_offset_codes(
                                        sixhr_offset[six], prev_sixhroff);

                                if (status > 0) {
                                    use_value = 0;
                                } else if ((status == 0)
                                        && (sixhroffset == prev_sixhroff)) {
                                    boolean rev = true;
                                    if (revision_6hour[six] == 0) {
                                        rev = false;
                                    }
                                    String update_action = determine_update_action(
                                            options.shef_duplicate.name(), rev);

                                    if ((update_action == upd_action.DONT_UPDATE_ACTION
                                            .name())
                                            || ((update_action == upd_action.IF_DIFFERENT_UPDATE_ACTION
                                                    .name()) && (pp_value == old_six_hr_val))) {
                                        use_value = 0;
                                    }
                                }
                            }
                        }
                        if (use_value == 1) {
                            if (pp_value == -7700) {
                                pp_value = 0;
                            }
                            six_hr_slot_val = pp_value;
                            sixhr_offset = old_sixhroffset;
                            sixhr_offset[six] = sixhroffset;
                            six_hr_qc = old_six_qc;
                            six_hr_qc[six] = sixhrqc;

                        } else {
                            if (old_six_hr_val == -7700) {
                                old_six_hr_val = 0;
                            }
                            six_hr_slot_val = old_six_hr_val;
                            sixhr_offset = old_sixhroffset;
                            six_hr_qc = old_six_qc;
                        }
                    }
                }

                if (is_pc == 1) {
                    Hourlypc pHourpc = (Hourlypc) hourly_rec;
                    HourlypcId pHid = new HourlypcId();
                    pHid.setLid(pHourpc.getLid());
                    pHid.setTs(pHourpc.getTs());
                    pHid.setObsdate(dto);
                    pHourpc.setId(pHid);
                    pHourpc.setMinuteOffset(String.valueOf(offset));
                    pHourpc.setHourlyQc(String.valueOf(qc));
                    set_hour_slot_value(pHourpc, hr, hr_value);
                    update_gage_rec(pHourpc);

                } else {

                    Hourlypp pHourpp = hpp;
                    HourlyppId pHid = new HourlyppId();
                    pHid.setLid(pHourpp.getLid());
                    pHid.setTs(pHourpp.getTs());
                    pHid.setObsdate(dto);
                    pHourpp.setId(pHid);
                    pHourpp.setMinuteOffset(String.valueOf(offset));
                    pHourpp.setHourlyQc(String.valueOf(qc));
                    pHourpp.setSixhroffset(String.valueOf(sixhr_offset));
                    pHourpp.setSixhrqc(String.valueOf(six_hr_qc));
                    set_hour_slot_value(pHourpp, hr, hr_value);
                    set_6hour_slot_value(pHourpp, six, six_hr_slot_val);
                    update_gage_rec(pHourpp);
                }
            }

        }

    }

    /**
     * Returns a new hour slot based on current data. This is used to insert or
     * update an existing Hourlypp record.
     * 
     * @param datetime
     * @param id
     * @param ts
     * @param new_hourly_value
     * @param obsdate
     * @param zero_offset_code
     * @param manual_qc_code
     * @return int hour_slot numeric value of the hour slot for current value.
     */
    public static int gage_pp_init(Date dtime, String id, String ts,
            double new_hourly_value, String obsdate, char zero_offset_code,
            char manual_qc_code) {

        hourly_rec = new Hourlypp();
        datetime = dtime;
        Calendar dt = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        dt.setTime(datetime);
        int hour_slot = dt.get(Calendar.HOUR_OF_DAY);
        Arrays.fill(hourly_qc, '-');
        Arrays.fill(minute_offset, '-');
        Arrays.fill(sixhr_qc, '-');
        Arrays.fill(sixhr_offset, '-');

        // if (hour_slot == 0) {
        // hour_slot = 24;
        // dt.add(Calendar.HOUR_OF_DAY, -1);
        // }
        minute_offset[hour_slot] = zero_offset_code;
        hourly_qc[hour_slot] = manual_qc_code;
        set_hour_slot_value(hourly_rec, hour_slot, new_hourly_value);
        return hour_slot;
    }

    /**
     * Updates record in the Hourlypp table with new value.
     * 
     * @param where
     *            String where clause of sql statement for update.
     * 
     */
    public static void update_gage_rec(PersistableDataObject obj) {
        try {
            DirectDbQuery.saveOrUpdate(obj, "ihfs");
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    /**
     * 
     * @param qc
     * @param hour
     * @param value
     */
    public static final void setMinOffset(char[] minOffset, int hour, char value) {
        if (hour == 0) {
            hour = 23;
        } else {
            hour--;
        }
        minOffset[hour] = value;
    }

    // get the correct offset slot in array based on hour
    public static final int getOffset(char[] minOffset, int hour) {
        int slot = 0;
        if (hour == 0) {
            slot = 23;
        } else {
            slot = hour--;
        }
        return slot;
    }

    /**
     * 
     * @param qc
     * @param hour
     * @param value
     */
    public static final void setHourlyQC(char[] qc, int hour, char value) {
        if (hour == 0) {
            hour = 23;
        } else {
            hour--;
        }
        qc[hour] = value;
    }

    /**
     * Returns the offset code based on minute of the hour.
     * 
     * @param int minute
     * @return String value of offset code
     */
    public static char get_offset_code(int minute) {

        char code;
        int offset;

        /*
         * You should be able to have a base value for positive and negative
         * offsets to allow you to calculate the absolute offset when decoding.
         */
        if (minute == 0) {
            code = zero_offset;
        } else if (minute > 30) {
            offset = MINUTES_PER_HOUR - minute - 1;
            code = (char) (negative_offset_base + offset);
        } else {
            offset = minute - 1;
            code = (char) (positive_offset_base + offset);
        }
        return code;
    }

    /**
     * Determines which precipitation value to use based on Qc codes and
     * shef_duplicate value.
     * 
     * @param value_new
     *            new precip value
     * @param value_old
     *            old precip value
     * @param value_new_qc
     *            new qc code
     * @param value_old_qc
     *            old qc code
     * @param value_new_offset
     *            new offset code
     * @param value_old_offset
     *            old offset code
     * @param shef_duplicate
     *            shef_duplicate value
     * @param revision
     *            current revision code
     * @return int use_value
     */
    public static int use_precip_value(double value_new, double value_old,
            char value_new_qc, char value_old_qc, char value_new_offset,
            char value_old_offset, String shef_duplicate, short revision) {
        int status;
        String update_action;
        int use_value = 1;

        /*
         * There is a value in the new hour slot. There is also a value in the
         * old hour slot. If the value in the old hour slot has been manually
         * edited, DO NOT overwrite it unless the new value is a manual edited.
         */
        if ((String.valueOf(value_old_qc).equalsIgnoreCase("M"))
                && !(String.valueOf(value_new_qc).equalsIgnoreCase("M"))) {
            use_value = 0;
        } else {
            /*
             * Check to see which is closest to the top of the hour. Check to
             * see if they are identical.
             */

            /*
             * Compare the number of minutes each of these reports is offset
             * from the top of the hour.
             */
            status = compare_offset_codes(value_new_offset, value_old_offset);

            if (status > 0) {
                use_value = 0;
            } else if ((status == 0) && (value_new_offset == value_old_offset)) {
                /*
                 * This is either a duplicate report or a revision. Check the
                 * revision flag to determine how to deal with this report.
                 */
                boolean tf = true;
                if (revision == 0) {
                    tf = false;
                }
                update_action = determine_update_action(shef_duplicate, tf);

                if ((update_action == upd_action.DONT_UPDATE_ACTION.name())
                        || ((update_action == upd_action.IF_DIFFERENT_UPDATE_ACTION
                                .name()) && (value_new == value_old))) {
                    use_value = 0;
                }
            }
        }

        return use_value;
    }

    /**
     * Compare offset codes to determine which one is greater.
     * 
     * @param minute_offset1
     *            first offset code
     * @param minute_offset2
     *            second offset code
     * @return int status
     */
    public static int compare_offset_codes(char minute_offset1,
            char minute_offset2) {
        int offset1;
        int offset2;
        int status;

        if (minute_offset1 >= negative_offset_base) {
            offset1 = (minute_offset1 - negative_offset_base) + 1;
        } else if (minute_offset1 >= positive_offset_base) {
            offset1 = (minute_offset1 - positive_offset_base) + 1;
        } else {
            offset1 = 0;
        }

        if (minute_offset2 >= negative_offset_base) {
            offset2 = (minute_offset2 - negative_offset_base) + 1;
        } else if (minute_offset2 >= positive_offset_base) {
            offset2 = (minute_offset2 - positive_offset_base) + 1;
        } else {
            offset2 = 0;
        }

        status = 0;

        if (offset1 < offset2) {
            status = -1;
        } else if (offset1 > offset2) {
            status = 1;
        }

        return status;
    }

    /**
     * Check if the existing value should be overwritten.
     * 
     * @param options_duplicate
     *            option to determine what to do if duplicate
     * @param shefrec_rev
     *            revision code
     * @return int up_action
     */
    public static String determine_update_action(String options_duplicate,
            boolean rev) {
        int shefrec_rev = 0;
        if (rev == true) {
            shefrec_rev = 1;
        }
        String up_action = upd_action.DONT_UPDATE_ACTION.name();

        /*
         * Check if the existing value should be overwritten. This occurs under
         * any of the following conditions:
         * 
         * 1)if shef_duplicate = always_overwrite ie., options_duplicate =
         * ALWAYS_OVERWRITE 2)if shef_duplicate = use_revcode and revisions flag
         * is set in the SHEF record ie., options_duplicate = USE_REVCODE 3)if
         * shef_duplicate = if_different and the value is different ie.,
         * options_duplicate = IF_DIFFERENT 4)if shef_duplicate =
         * if_different_or_revcode and the revision flag is set in SHEF record
         * or if value is different ie., options_duplicate =
         * IF_DIFFERENT_UPDATE_ACTION 5)if shef_duplicate =
         * if_different_and_revcode and the revision flag is set in SHEF record
         * and value is different ie., options_duplicate =
         * IF_DIFFERENT_UPDATE_ACTION
         */

        if (options_duplicate == shef_dup.ALWAYS_OVERWRITE.name()) {
            up_action = upd_action.UPDATE_ACTION.name();
        } else if (options_duplicate == shef_dup.USE_REVCODE.name()) {
            if (shefrec_rev == 1) {
                up_action = upd_action.UPDATE_ACTION.name();
            } else {
                up_action = upd_action.DONT_UPDATE_ACTION.name();
            }
        } else if (options_duplicate == shef_dup.IF_DIFFERENT_OR_REVCODE.name()
                && shefrec_rev == 1) {
            up_action = upd_action.UPDATE_ACTION.name();
        } else if (options_duplicate == shef_dup.IF_DIFFERENT_AND_REVCODE
                .name() && shefrec_rev == 1) {
            up_action = upd_action.IF_DIFFERENT_UPDATE_ACTION.name();
        } else if (options_duplicate != shef_dup.IF_DIFFERENT_AND_REVCODE
                .name()) {
            /*
             * This address the case where options_duplicate == IF_DIFFERENT or
             * options_duplicate == IF_DIFFERENT_OR_REVCODE and shefrec_rev == 0
             */
            up_action = upd_action.IF_DIFFERENT_UPDATE_ACTION.name();
        }

        return up_action;
    }

    /**
     * Returns the precipitation value stored in the appropriate six hour slot
     * based on hour.
     * 
     * @param pHourlyPP
     *            Current Hourlypp record
     * @param hour
     *            int value of hour
     * @return short precip_value
     */
    public static Short get_6hour_slot_value(Hourlypp pHourlyPP, int hour) {
        Short precip_value = null;

        /*
         * Depending on the hour, select the value in the correct hour slot in
         * the HourPC structure.
         */
        switch (hour) {
        case 0: /* 00z - 06z */

            precip_value = pHourlyPP.getSixhr06();
            break;

        case 1: /* 06z - 12z */

            precip_value = pHourlyPP.getSixhr12();
            break;

        case 2: /* 12z - 18z */

            precip_value = pHourlyPP.getSixhr18();
            break;

        case 3: /* 18z - 00z */

            precip_value = pHourlyPP.getSixhr24();
            break;

        default:
            break;
        }

        if (precip_value == null) {
            precip_value = new Short((short) MISSING_PRECIP);
        }

        return precip_value;
    }

    public static Short set_6hour_slot_value(Hourlypp pHourlyPP, int hour,
            double val) {
        Short precip_value = new Short((short) MISSING_PRECIP);

        /*
         * Depending on the hour, select the value in the correct hour slot in
         * the HourPC structure.
         */
        switch (hour) {
        case 0: /* 00z - 06z */

            precip_value = new Short((short) val);
            pHourlyPP.setSixhr06(precip_value);
            break;

        case 1: /* 06z - 12z */

            precip_value = new Short((short) val);
            pHourlyPP.setSixhr12(precip_value);
            break;

        case 2: /* 12z - 18z */

            precip_value = new Short((short) val);
            pHourlyPP.setSixhr18(precip_value);
            break;

        case 3: /* 18z - 00z */

            precip_value = new Short((short) val);
            pHourlyPP.setSixhr24(precip_value);
            break;

        default:
            break;
        }

        return precip_value;
    }

    public static Short get_hour_slot_value(IHourlyTS pHourlyPP, int hour) {
        Short precip_value = null;

        /*
         * Depending on the hour, select the value in the correct hour slot in
         * the HourPC structure.
         */
        switch (hour) {
        case 1:
            precip_value = pHourlyPP.getHour1();
            break;

        case 2:
            precip_value = pHourlyPP.getHour2();
            break;

        case 3:
            precip_value = pHourlyPP.getHour3();
            break;

        case 4:
            precip_value = pHourlyPP.getHour4();
            break;

        case 5:
            precip_value = pHourlyPP.getHour5();
            break;

        case 6:
            precip_value = pHourlyPP.getHour6();
            break;

        case 7:
            precip_value = pHourlyPP.getHour7();
            break;

        case 8:
            precip_value = pHourlyPP.getHour8();
            break;

        case 9:
            precip_value = pHourlyPP.getHour9();
            break;

        case 10:
            precip_value = pHourlyPP.getHour10();
            break;

        case 11:
            precip_value = pHourlyPP.getHour11();
            break;

        case 12:
            precip_value = pHourlyPP.getHour12();
            break;

        case 13:
            precip_value = pHourlyPP.getHour13();
            break;

        case 14:
            precip_value = pHourlyPP.getHour14();
            break;

        case 15:
            precip_value = pHourlyPP.getHour15();
            break;

        case 16:
            precip_value = pHourlyPP.getHour16();
            break;

        case 17:
            precip_value = pHourlyPP.getHour17();
            break;

        case 18:
            precip_value = pHourlyPP.getHour18();
            break;

        case 19:
            precip_value = pHourlyPP.getHour19();
            break;

        case 20:
            precip_value = pHourlyPP.getHour20();
            break;

        case 21:
            precip_value = pHourlyPP.getHour21();
            break;

        case 22:
            precip_value = pHourlyPP.getHour22();
            break;

        case 23:
            precip_value = pHourlyPP.getHour23();
            break;

        case 24:
            precip_value = pHourlyPP.getHour24();
            break;

        default:
            break;
        }

        if (precip_value == null) {
            precip_value = new Short((short) MISSING_PRECIP);

        }

        return precip_value;
    }

    public static Short set_hour_slot_value(IHourlyTS pHourly, int hour,
            double val) {
        Short precip_value = new Short((short) MISSING_PRECIP);

        /*
         * Depending on the hour, select the value in the correct hour slot in
         * the HourPC structure.
         */
        switch (hour) {
        case 1:
            precip_value = new Short((short) val);
            pHourly.setHour1(precip_value);
            break;

        case 2:
            precip_value = new Short((short) val);
            pHourly.setHour2(precip_value);
            break;

        case 3:
            precip_value = new Short((short) val);
            pHourly.setHour3(precip_value);
            break;

        case 4:
            precip_value = new Short((short) val);
            pHourly.setHour4(precip_value);
            break;

        case 5:
            precip_value = new Short((short) val);
            pHourly.setHour5(precip_value);
            break;

        case 6:
            precip_value = new Short((short) val);
            pHourly.setHour6(precip_value);
            break;

        case 7:
            precip_value = new Short((short) val);
            pHourly.setHour7(precip_value);
            break;

        case 8:
            precip_value = new Short((short) val);
            pHourly.setHour8(precip_value);
            break;

        case 9:
            precip_value = new Short((short) val);
            pHourly.setHour9(precip_value);
            break;

        case 10:
            precip_value = new Short((short) val);
            pHourly.setHour10(precip_value);
            break;

        case 11:
            precip_value = new Short((short) val);
            pHourly.setHour11(precip_value);
            break;

        case 12:
            precip_value = new Short((short) val);
            pHourly.setHour12(precip_value);
            break;

        case 13:
            precip_value = new Short((short) val);
            pHourly.setHour13(precip_value);
            break;

        case 14:
            precip_value = new Short((short) val);
            pHourly.setHour14(precip_value);
            break;

        case 15:
            precip_value = new Short((short) val);
            pHourly.setHour15(precip_value);
            break;

        case 16:
            precip_value = new Short((short) val);
            pHourly.setHour16(precip_value);
            break;

        case 17:
            precip_value = new Short((short) val);
            pHourly.setHour17(precip_value);
            break;

        case 18:
            precip_value = new Short((short) val);
            pHourly.setHour18(precip_value);
            break;

        case 19:
            precip_value = new Short((short) val);
            pHourly.setHour19(precip_value);
            break;

        case 20:
            precip_value = new Short((short) val);
            pHourly.setHour20(precip_value);
            break;

        case 21:
            precip_value = new Short((short) val);
            pHourly.setHour21(precip_value);
            break;

        case 22:
            precip_value = new Short((short) val);
            pHourly.setHour22(precip_value);
            break;

        case 23:
            precip_value = new Short((short) val);
            pHourly.setHour23(precip_value);
            break;

        case 24:
            precip_value = new Short((short) val);
            pHourly.setHour24(precip_value);
            break;

        default:
            precip_value = new Short((short) MISSING_PRECIP);
            break;
        }

        return precip_value;
    }
}
