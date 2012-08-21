package com.raytheon.edex.plugin.shef.ohdlib;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import com.raytheon.edex.plugin.shef.data.precip.PrecipRecord;
import com.raytheon.edex.plugin.shef.ohdlib.GagePPOptions.shef_dup;
import com.raytheon.edex.plugin.shef.ohdlib.GagePPOptions.upd_action;
import com.raytheon.edex.plugin.shef.util.SHEFDate;
import com.raytheon.uf.common.dataplugin.shef.tables.Dailypp;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.IHourlyTS;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElement;
import com.raytheon.uf.common.dataplugin.shef.util.SHEFTimezone;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

public abstract class PrecipUtils {

    private static final float MISSING_PRECIP = -9999f;

    private static final int POSITIVE_OFFSET_BASE = 'A';

    private static final int NEGATIVE_OFFSET_BASE = 'a';

    private static final int MINUTES_PER_HOUR = 60;

    private static final int SECONDS_PER_MINUTE = 60;

    private static final int SECONDS_PER_HOUR = MINUTES_PER_HOUR * SECONDS_PER_MINUTE;

    private static final int SECONDS_IN_6HOUR_PERIOD = 6 * SECONDS_PER_HOUR;

    private static final int NUM_MINUTES_PER_6HOUR_OFFSET = 10;

    private static final int NUM_SECONDS_PER_6HOUR_OFFSET = (NUM_MINUTES_PER_6HOUR_OFFSET * SECONDS_PER_MINUTE);

    private static final int SECONDS_PER_DAY = 86400;

    private static final char ZERO_OFFSET = '0';
    
    public static final int NUM_HOURLY_SLOTS = 24;

    public static final int NUM_6HOURLY_SLOTS = 4;


    
    /**
     * 
     * @param pDailyPP
     * @param msgstr
     * @param pPrecipRecord
     */
    public static void initialize_dailypp(Dailypp pDailyPP, final PrecipRecord pPrecipRecord) {

        pDailyPP.setPostingtime(pPrecipRecord.getPostingTime());
        pDailyPP.setQc("-");
        pDailyPP.setValue(0.0);
    }

    /**
     * 
     * @param options
     */
    public static void get_precip_window(GagePPOptions options) {
        AppsDefaults APPS_DEFAULTS = AppsDefaults.getInstance();
        options.setIntpc(APPS_DEFAULTS.getInt("intpc",10));
        options.setIntlppp(APPS_DEFAULTS.getInt("intlppp",10));
        options.setIntuppp(APPS_DEFAULTS.getInt("intuppp",10));
    }
    
    
    /**
     * 
     * @param pe
     * @param options
     * @return
     */
    public static boolean checkPrecipWindow(SHEFDate obsTime, PhysicalElement pe, GagePPOptions options) {
        boolean withinWindow = false;
        
        
        int pcWindow = options.getIntpc();
        int ppBefore = options.getIntlppp();
        int ppAfter = options.getIntuppp();
        
        int minutes = obsTime.getMinute();
        
        if(PhysicalElement.PRECIPITATION_ACCUMULATOR.equals(pe)) {
            withinWindow = (minutes >= (60 - pcWindow) || (minutes <= pcWindow));
        } else {
            withinWindow = ((minutes >= (60 - ppBefore)) || (minutes <= ppAfter));
        }
        
        return withinWindow;
    }
    
    
    /**
     * 
     * @param ppq_window
     * @return
     */
    public static float get_6hour_precip_window() {
        return AppsDefaults.getInstance().getFloat("intppq", 2.0f);
    }
    
    /**
     * 
     * @param hour
     * @return
     */
    public static int getSixHourSelector(int hour) {
        int selector = 3;

        if (hour >= 0 && hour <= 6) {
            selector = 0;
        } else if (hour > 6 && hour <= 12) {
            selector = 1;
        } else if (hour > 12 && hour <= 18) {
            selector = 2;
        }
        return selector;
    }

    /**
     * 
     */
    public static Short set_hour_slot_value(IHourlyTS pHourly, int hour,
            Short precip_value) {

        /*
         * Depending on the hour, select the value in the correct hour slot in
         * the HourPC structure.
         */
        switch (hour) {
        case 1:
            pHourly.setHour1(precip_value);
            break;

        case 2:
            pHourly.setHour2(precip_value);
            break;

        case 3:
            pHourly.setHour3(precip_value);
            break;

        case 4:
            pHourly.setHour4(precip_value);
            break;

        case 5:
            pHourly.setHour5(precip_value);
            break;

        case 6:
            pHourly.setHour6(precip_value);
            break;

        case 7:
            pHourly.setHour7(precip_value);
            break;

        case 8:
            pHourly.setHour8(precip_value);
            break;

        case 9:
            pHourly.setHour9(precip_value);
            break;

        case 10:
            pHourly.setHour10(precip_value);
            break;

        case 11:
            pHourly.setHour11(precip_value);
            break;

        case 12:
            pHourly.setHour12(precip_value);
            break;

        case 13:
            pHourly.setHour13(precip_value);
            break;

        case 14:
            pHourly.setHour14(precip_value);
            break;

        case 15:
            pHourly.setHour15(precip_value);
            break;

        case 16:
            pHourly.setHour16(precip_value);
            break;

        case 17:
            pHourly.setHour17(precip_value);
            break;

        case 18:
            pHourly.setHour18(precip_value);
            break;

        case 19:
            pHourly.setHour19(precip_value);
            break;

        case 20:
            pHourly.setHour20(precip_value);
            break;

        case 21:
            pHourly.setHour21(precip_value);
            break;

        case 22:
            pHourly.setHour22(precip_value);
            break;

        case 23:
            pHourly.setHour23(precip_value);
            break;

        case 0:
            pHourly.setHour24(precip_value);
            break;
        default:
            precip_value = new Short((short) MISSING_PRECIP);
            break;
        }
        return precip_value;
    }

    /**
     * 
     */
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
        case 0:
            precip_value = pHourlyPP.getHour24();
            break;
        default:
            break;
        }

        return precip_value;
    }

    /**
     * Returns the precipitation value stored in the appropriate six hour slot
     * based on hour.
     * 
     * @param pHourlyPP
     *            Current Hourlypp record
     * @param hour
     *            int Hour of the day [00..23]
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

//        if (precip_value == null) {
//            precip_value = new Short((short) MISSING_PRECIP);
//        }

        return precip_value;
    }

    /**
     * 
     * @param pHourlyPP
     * @param hour Hour of the day [00..23]
     * @param precip_value
     * @return
     */
    public static Short set_6hour_slot_value(Hourlypp pHourlyPP, int hour,
            Short precip_value) {

        /*
         * Depending on the hour, select the value in the correct hour slot in
         * the HourPC structure.
         */
        switch (hour) {
        case 0: /* 00z - 06z */
            pHourlyPP.setSixhr06(precip_value);
            break;

        case 1: /* 06z - 12z */
            pHourlyPP.setSixhr12(precip_value);
            break;

        case 2: /* 12z - 18z */
            pHourlyPP.setSixhr18(precip_value);
            break;

        case 3: /* 18z - 00z */
            pHourlyPP.setSixhr24(precip_value);
            break;

        default:
            precip_value = new Short((short) MISSING_PRECIP);
            break;
        }
        return precip_value;
    }

    /**
     * Returns the character code which represents the minute offset from the
     * top of the hour. The coding scheme works as follows:
     * 
     * <pre>
     *    Char Code   Positive Offset / Negative Offset (minutes)   
     *    0    0             0                  0
     *    A    a             1                 -1 
     *    B    b             2                 -2
     *    C    c             3                 -3
     *    D    d             4                 -4
     *    E    e             5                 -5
     *    F    f             6                 -6
     *    G    g             7                 -7
     *    H    h             8                 -8
     *    I    i             9                 -9
     *    J    j            10                 -10
     *    K    k            11                 -11
     *    L    l            12                 -12
     *    M    m            13                 -13
     *    N    n            14                 -14
     *    O    o            15                 -15
     *    P    p            16                 -16
     *    Q    q            17                 -17
     *    R    r            18                 -18
     *    S    s            19                 -19
     *    T    t            20                 -20
     *    U    u            21                 -21
     *    V    v            22                 -22
     *    W    w            23                 -23
     *    X    x            24                 -24
     *    Y    y            25                 -25
     *    Z    z            26                 -26
     *    [    {            27                 -27
     *    \    |            28                 -28
     *    ]    }            29                 -29
     *    ^                 30                 
     * 
     *  These are divided into two sets.  One set represents positive
     *  offsets.  These are offsets from 0 (the top of the hour) to
     *  30 minutes past the hour.
     * 
     *  The other set represents the negative offsets.  These are
     *  offsets.  These are offsets from 1 minute before the top of
     *  the hour to 29 minutes before the top of the hour.
     * 
     *  If the minute supplied to this routine ranges from 31 to 59,
     *  then the negative offset (before the top of the hour) list
     *  is used and the appropriate character is used.
     * 
     *  If the minute supplies to this routine ranges from 0 to 30,
     *  then the positive offset (after the top of the hour) list is
     *  used and the appropriate character is used.
     * 
     *  The characters have been chosen is such a way that they are
     *  sequential ASCII characters this makes it easy to convert them
     *  back into actual durations.
     * </pre>
     * 
     * @param int minute
     * @return String value of offset code
     */
    public static char get_offset_code(int minute) {
        char code = ZERO_OFFSET;
        /*
         * You should be able to have a base value for positive and negative
         * offsets to allow you to calculate the absolute offset when decoding.
         */
        if (minute > 30) {
            int offset = MINUTES_PER_HOUR - minute - 1;
            code = (char) (NEGATIVE_OFFSET_BASE + offset);
        } else if (minute != 0) {
            int offset = minute - 1;
            code = (char) (POSITIVE_OFFSET_BASE + offset);
        }
        return code;
    }

    /**
     * This routine compares two offset character codes. It returns a value
     * indicating how the absolute offset of the first character code compares
     * to the absolute offset of the second character code. A return value of 0
     * means the durations are equal. A return value of -1 means the duration of
     * the first code is smaller than the duration of the second code. A return
     * value of 1 means the duration of the first code is larger than the
     * duration of the second code.
     * 
     * @param minute_offset1
     *            The actual offset represented by the first character code.
     * @param minute_offset2
     *            The actual offset represented by the second character code.
     * @return int status A value indicating how the offset represented by the
     *         first character code compares to the offset represented by the
     *         second character code.
     * 
     *         <pre>
     *  -1   The duration of the first character code is smaller than the
     *       duration of the second character code.
     *   0   The durations of the two character codes are the same.
     *   1   The durations of the second character code is greater than
     *       the duration of the first character code.
     * </pre>
     */
    public static int compare_offset_codes(char minute_offset1,
            char minute_offset2) {
        int offset1 = 0;
        int offset2 = 0;

        if (minute_offset1 >= NEGATIVE_OFFSET_BASE) {
            offset1 = (minute_offset1 - NEGATIVE_OFFSET_BASE) + 1;
        } else if (minute_offset1 >= POSITIVE_OFFSET_BASE) {
            offset1 = (minute_offset1 - POSITIVE_OFFSET_BASE) + 1;
        }

        if (minute_offset2 >= NEGATIVE_OFFSET_BASE) {
            offset2 = (minute_offset2 - NEGATIVE_OFFSET_BASE) + 1;
        } else if (minute_offset2 >= POSITIVE_OFFSET_BASE) {
            offset2 = (minute_offset2 - POSITIVE_OFFSET_BASE) + 1;
        }

        int status = 0;
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

        if (shef_dup.ALWAYS_OVERWRITE.equalsOption(options_duplicate)) {
            up_action = upd_action.UPDATE_ACTION.name();
        } else if (shef_dup.USE_REVCODE.equalsOption(options_duplicate)) {
            if (rev) {
                up_action = upd_action.UPDATE_ACTION.name();
            } else {
                up_action = upd_action.DONT_UPDATE_ACTION.name();
            }
        } else if (shef_dup.IF_DIFFERENT_OR_REVCODE
                .equalsOption(options_duplicate) && rev) {
            up_action = upd_action.UPDATE_ACTION.name();
        } else if (shef_dup.IF_DIFFERENT_AND_REVCODE
                .equalsOption(options_duplicate) && rev) {
            up_action = upd_action.IF_DIFFERENT_UPDATE_ACTION.name();
        } else if (!shef_dup.IF_DIFFERENT_AND_REVCODE
                .equalsOption(options_duplicate)) {
            /*
             * This address the case where options_duplicate == IF_DIFFERENT or
             * options_duplicate == IF_DIFFERENT_OR_REVCODE and shefrec_rev == 0
             */
            up_action = upd_action.IF_DIFFERENT_UPDATE_ACTION.name();
        }

        return up_action;
    }

    // private static char pOffsetCode = '0';


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

        int use_value = 1;
        
        /*
         * There is a value in the new hour slot. There is also a value in the
         * old hour slot. If the value in the old hour slot has been manually
         * edited, DO NOT overwrite it unless the new value is a manual edited.
         */
        if (isManualEdit(value_old_qc) && !isManualEdit(value_new_qc)) {
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
            int status = compare_offset_codes(value_new_offset,
                    value_old_offset);

            if (status > 0) {
                use_value = 0;
            } else if ((status == 0) && (value_new_offset == value_old_offset)) {
                /*
                 * This is either a duplicate report or a revision. Check the
                 * revision flag to determine how to deal with this report.
                 */
                boolean tf = (revision != 0);

                String update_action = determine_update_action(shef_duplicate,
                        tf);

                if ((upd_action.DONT_UPDATE_ACTION.isAction(update_action))
                        || ((upd_action.IF_DIFFERENT_UPDATE_ACTION
                                .isAction(update_action)) && (value_new == value_old))) {
                    use_value = 0;
                }
            }
        }

        return use_value;
    }


    /**
     * 
     * @param pOptions
     * @param pPrecipRecord
     * @param pHourlyPP
     * @param offset_code
     * @param hour
     * @return
     */
    public static int use_6_hour_value(final GagePPOptions pOptions,
            final PrecipRecord pPrecipRecord, final Hourlypp pHourlyPP,
            char offset_code, short hour) {

        int useValue = 1;
        
        Short precip_val = PrecipUtils.get_6hour_slot_value(pHourlyPP, hour);
        if(precip_val != null) {
            short rev = (short)((pPrecipRecord.revision) ? 1 : 0);

            useValue = PrecipUtils.use_precip_value(pPrecipRecord.getValue(),
                    precip_val, String.valueOf(pPrecipRecord.getQualifier()).charAt(
                            hour), pHourlyPP.getSixhrqc().charAt(hour),
                    offset_code, pHourlyPP.getSixhroffset().charAt(hour),
                    pOptions.getShef_duplicate().name(), rev);
        }
        return useValue;
    }
    
    /**
     * 
     * @param pOptions
     * @param pPrecipRecord
     * @param pDailyPP
     * @return
     */
    public static int use_24_hour_value(final GagePPOptions pOptions,
            final PrecipRecord pPrecipRecord, final Dailypp pDailyPP) {
        double precip_val = 0;
        String update_action = null;
        int use_value = 1;

        if (pDailyPP.getValue() != null) {
            update_action = PrecipUtils.determine_update_action(
                    pOptions.getShef_duplicate().toString(), pPrecipRecord.revision);

            if ((upd_action.DONT_UPDATE_ACTION.isAction(update_action))
                    || ((upd_action.IF_DIFFERENT_UPDATE_ACTION
                            .isAction(update_action)) && (precip_val == pPrecipRecord.getValue()))) {
                use_value = 0;
            }
        }
        return use_value;
    }
    
    /**
     * 
     * @param dur
     * @param time12z
     * @param dataDuration
     * @return
     */
    public static int shefdur_to_time(PrecipRecord precip, Date time12z) {
        int status = 0;
        int retval = 0;

        int dur = precip.getShefDuration().getValue();
        
        /* 000x values are in minutes */
        if (dur < 1000) {
            retval = dur * SECONDS_PER_MINUTE;
        } else if (dur >= 1000 && dur < 2000) {
            retval = ((dur % 1000) * SECONDS_PER_HOUR);
        } else if (dur >= 2000 && dur < 3000) {
            retval = ((dur % 2000) * SECONDS_PER_DAY);
        } else if (dur >= 3000 && dur < 4000) {
            retval = ((dur % 2000) * SECONDS_PER_DAY);
        } else if (dur >= 4000 && dur < 5000) {
            status = -1;
            retval = 0;
        } else if (dur == 5004) {
        /*
         * 500x values are specially defined. only consider 5004; its duration
         * is defined as the period ending at the observation time and beginning
         * at the most recent 7AM local
         */
            /*
             * find the time_t for 7AM local. use the data time. subtract a day
             * from it so the modulo approach can be used to determine the
             * duration. this is needed to easily handle the cases where the
             * ending time is between midnight and 7 AM.
             */

            /* Get the search window around local 7 AM. */
            int local_7am_window = AppsDefaults.getInstance().getInt("ppp_ppd_local_7am_window", 3);

            Calendar c = new GregorianCalendar(SHEFTimezone.GMT_TIMEZONE);
            c.setTime(time12z);

            Calendar lc = new GregorianCalendar(SHEFTimezone.GMT_TIMEZONE);
            lc.setTimeInMillis(c.getTimeInMillis() + getLocalOffset());
            
            // Calendar with the default timezone set.
            Calendar ccc = new GregorianCalendar();
            c.set(Calendar.YEAR, lc.get(Calendar.YEAR));
            c.set(Calendar.MONTH, lc.get(Calendar.MONTH));
            c.set(Calendar.DAY_OF_MONTH, lc.get(Calendar.DAY_OF_MONTH));
            ccc.set(Calendar.HOUR_OF_DAY, 7);
            ccc.set(Calendar.MINUTE, 0);
            ccc.set(Calendar.SECOND, 0);
            ccc.set(Calendar.MILLISECOND, 0);
            // difference in seconds
            int diff = (int) Math.abs((c.getTimeInMillis() - ccc.getTimeInMillis()) / 1000L);
            
            if(diff < (3600 * local_7am_window)) {
                retval = SECONDS_PER_DAY;
            } else if (c.getTimeInMillis() < ccc.getTimeInMillis()) {
                retval = SECONDS_PER_DAY - diff;
            } else {
                retval = diff;
            }
        } else {
            /* then there are some undefined values */
            status = -1;
            retval = 0;
        }

        precip.setDataDuration(retval);
        
        return status;
    }

    /**
     * 
     * @param where
     * @return
     */
    public static List<Hourlypp> getHourlyPP(String where) {
        StringBuilder query = new StringBuilder("FROM ");
        query.append(Hourlypp.class.getName());
        query.append(" ");
        query.append(where);

        List<Hourlypp> retVal = new ArrayList<Hourlypp>();
        CoreDao dao = new CoreDao(DaoConfig.forDatabase("ihfs"));
        QueryResult qu = dao.executeHQLQuery(query.toString());
        List<Object[]> results = new ArrayList<Object[]>();
        Object[] obj = new Object[qu.getColumnCount()];
        for (int i = 0; i < qu.getResultCount(); i++) {
            obj[0] = (qu.getRowColumnValue(i, 0));
            results.add(obj);
        }

        for (Object[] item : results) {
            retVal.add((Hourlypp) item[0]);
        }

        return retVal;
    }

    /**
     * 
     * @param where
     * @return
     */
    public static ArrayList<Hourlypc> getHourlyPC(String where) {
        StringBuilder query = new StringBuilder("FROM ");
        query.append(Hourlypc.class.getName());
        query.append(" ");
        query.append(where);

        ArrayList<Hourlypc> retVal = new ArrayList<Hourlypc>();
        CoreDao dao = new CoreDao(DaoConfig.forDatabase("ihfs"));
        QueryResult qu = dao.executeHQLQuery(query.toString());
        List<Object[]> results = new ArrayList<Object[]>();
        Object[] obj = new Object[qu.getColumnCount()];
        for (int i = 0; i < qu.getResultCount(); i++) {
            obj[0] = (qu.getRowColumnValue(i, 0));
            results.add(obj);
        }

        retVal.ensureCapacity(results.size());
        for (Object[] item : results) {
            retVal.add((Hourlypc) item[0]);
        }

        return retVal;
    }

    /**
     * 
     * @param where
     * @return
     */
    public static List<Dailypp> getDailyPP(String where) {
        StringBuilder query = new StringBuilder("FROM ");
        query.append(Dailypp.class.getName());
        query.append(" ");
        query.append(where);

        List<Dailypp> retVal = new ArrayList<Dailypp>();
        CoreDao dao = new CoreDao(DaoConfig.forDatabase("ihfs"));
        QueryResult qu = dao.executeHQLQuery(query.toString());
        
        List<Object[]> results = new ArrayList<Object[]>();
        Object[] obj = new Object[qu.getColumnCount()];
        for (int i = 0; i < qu.getResultCount(); i++) {
            obj[0] = (qu.getRowColumnValue(i, 0));
            results.add(obj);
        }

        for (Object[] item : results) {
            retVal.add((Dailypp) item[0]);
        }
        return retVal;
    }
    
    public static final boolean isManualEdit(String s) {
        boolean isManual = false;
        if(s != null && s.length() == 1) {
            isManual = isManualEdit(s.charAt(0));
        }
        return isManual;
    }

    public static final boolean isManualEdit(char c) {
        return ('M' == Character.toUpperCase(c));
    }

    /**
     * Return the local offset in seconds.
     * @return
     */
    public static final long getLocalOffset() {
        Calendar c = new GregorianCalendar();
        return c.getTimeZone().getOffset(c.getTimeInMillis()) / 1000;
    }
    
    /**
     * 
     * @param qc
     * @param hour
     * @param value
     */
    public static final void setMinOffset(char [] minOffset, int hour, char value) {
        if(hour == 0) {
            hour = 23;
        } else {
            hour--;
        }
        minOffset[hour] = value;
    }
    
    /**
     * 
     * @param qc
     * @param hour
     * @param value
     */
    public static final void setHourlyQC(char [] qc, int hour, char value) {
        if(hour == 0) {
            hour = 23;
        } else {
            hour--;
        }
        qc[hour] = value;
    }
   
    
    public static final void main(String [] args) {
        
//        public static int use_precip_value(double value_new, double value_old,
//                char value_new_qc, char value_old_qc, char value_new_offset,
//                char value_old_offset, String shef_duplicate, short revision) {

        short rev = 0;
        
        int stat = use_precip_value(0.0, 0.0, '-', 'G', '-', 'b', "ALWAYS_OVERWRITE", rev);
        
        System.out.println(stat);
        
        
        
    }
    
    
    
    
    
}
