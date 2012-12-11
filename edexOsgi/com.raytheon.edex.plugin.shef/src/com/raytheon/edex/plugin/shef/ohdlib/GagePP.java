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
package com.raytheon.edex.plugin.shef.ohdlib;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.shef.data.precip.PrecipRecord;
import com.raytheon.uf.common.dataplugin.shef.tables.Dailypp;
import com.raytheon.uf.common.dataplugin.shef.tables.DailyppId;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.HourlyppId;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.Duration;
import com.raytheon.uf.common.dataplugin.shef.util.SHEFTimezone;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date           Ticket#    Engineer    Description
 * ------------   ---------- ----------- --------------------------
 * 9/22           #1553      mnash       Initial implementation of GagePP
 * 6/1/09         #2432      jsanchez    Updated value magnitude for hourlypp/pc.
 * 26 Nov 2012    #15554     lbousaidi	 used obstime instead of system time in isNear12Z
 * 										 routine.
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class GagePP {

    public static final int GPP_OK = 0;

    private static final int GPP_ERROR = 1;

    private static final int GPP_SKIP_RECORD = 4;

    private static final int GPP_BADRECORD = 3;
    
    private static final int DEFAULT_WINDOW_HOURS = 3;

    public static final int MINIMUM_GPP_SLEEP_DURATION = 10;

    private static final int SECONDS_PER_HOUR = 3600;

    private static final int MINUTES_PER_HOUR = 60;

    private static final int SECONDS_PER_DAY = 86400;

    private static final int SECONDS_PER_MINUTE = 60;

    private static final int SECONDS_IN_6HOUR_PERIOD = 21600;

    private static final int NUM_MINUTES_PER_6HOUR_OFFSET = 10;

    private static final int NUM_SECONDS_PER_6HOUR_OFFSET = (NUM_MINUTES_PER_6HOUR_OFFSET * SECONDS_PER_MINUTE);

    private static final short SHRT_MAX = 32767;

    private static final AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private static final float MISSING_PRECIP = -9999;

    private static Log logger = LogFactory.getLog(GagePP.class);

    private GagePPWrite gagePPWrite = null;

    private char pOffsetCode;

    private short pHourSlot;

    private short p6HourSlot;
    
    public GagePP() {
    }

    /**
     * 
     * @param record
     * @param pOptions
     * @return
     */
    public int gage_pp_process_file(final PrecipRecord record,
            final GagePPOptions pOptions) {

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(SHEFTimezone.GMT_TIMEZONE);

        PrecipRecord rec = record;

        String obstime_ytd = sdf.format(rec.getObsTime());
        String obstime_prev = "";

        char[] offset_code = new char[] { '0', };
        String prev_pe = "";
        String msgstr = "";
        String cur_key = "";


        Dailypp dailyPP = new Dailypp();

        Hourlypp hourlyPP = new Hourlypp();
        // double elapsed_time;

        int prev_dur = 0;
        int use_value;
        short precip_val;
        short hour = 1;

        short rev_code = 0;
        short[] rev_6hour_code = new short[PrecipUtils.NUM_6HOURLY_SLOTS];
        boolean rev_24hour_code = false;

        WriteInfo write_info = new WriteInfo();

        Date btime = new Date();

        int status = GPP_OK;
        
        logger.info("Processing records at " + btime);
        
        Calendar dt = TimeTools.newCalendar(rec.getObsTime().getTime());

        HourlyppId id = new HourlyppId(rec.getLocationId(), rec.getTypeSource()
                .getCode(), btime);
        hourlyPP.setId(id);
        DailyppId did = new DailyppId(rec.getLocationId(), rec.getTypeSource()
                .getCode(), btime);
        dailyPP.setId(did);
        rev_24hour_code = false;

        Date time12z = rec.getObsTime();

        Calendar dp = Calendar.getInstance(SHEFTimezone.GMT_TIMEZONE);
        dp.setTime(time12z);

        if (rec.getPhysicalElement().getCode().charAt(1) == 'C') {
            rec.setShefDuration(Duration.INSTANTENOUS);
        }

        switch (rec.getShefDuration().getValue()) {

        case 0:
            if (rec.getPhysicalElement().getCode().charAt(1) == 'P') {
                msgstr = String.format(
                        "%s %s %d %s %s cannot have an instantaneous duration",
                        rec.getLocationId(),
                        rec.getPhysicalElement().getCode(), rec
                                .getShefDuration().getValue(), rec
                                .getTypeSource().getCode(), rec.getObsTime());
                logger.info(msgstr);
                status = GPP_SKIP_RECORD;
            } else {
                rec.setDataDuration(0);
                gage_pp_1hour_slot(pOptions, rec.getPhysicalElement()
                        .toString(), rec);
                hour = pHourSlot;
            }
            break;

        case 60: /* 60 minute precipitation report. */
        case 1001: {
            /* PPH : This is a one hour PP report. */
            rec.setDataDuration(1);

            /*
             * Determine which 1 hour slot to place the precipitation value in.
             */
            gage_pp_1hour_slot(pOptions, rec.getPhysicalElement().toString(),
                    rec);
            hour = pHourSlot;
            break;
        }
        case 1003: { // Skip 3 hourly data.
            status = GPP_SKIP_RECORD;
            break;
        }
        case 1006: /* PPQ */
            /* This is a 6 hour PP report. */
            rec.setDataDuration(6);

            /*
             * Determine which 6 hour slot to place the data value in.
             */
            
            short [] sHour = new short [] { p6HourSlot, };
            status = gage_pp_6hour_slot(pOptions, dt, obstime_ytd, sHour,
                    offset_code);
            
            if(!dt.getTime().equals(rec.getObsTime())) {
                rec.setObsTime(dt.getTime());
                dp = TimeTools.copy(dt);
            }
            
            p6HourSlot = sHour[0];
            break;

        case 5004: {
            /* PPP */

            /*
             * This MAY be a 24 hour PP report. It depends on how the obstime of
             * the report compares with 7 am local time.
             */

            status = PrecipUtils.shefdur_to_time(rec, time12z);

            if (rec.getDataDuration() != SECONDS_PER_DAY) {
                msgstr = String.format(
                        "%s %s %d %s %s does not have 24 hour duration", rec
                                .getLocationId(), rec.getPhysicalElement()
                                .getCode(), rec.getShefDuration().getValue(),
                        rec.getTypeSource().getCode(), rec.getObsTime());
                status = GPP_SKIP_RECORD;
                logger.info(msgstr);
            }

            rec.setDataDuration(24);

            /*
             * Check to make sure that this report falls within a specified time
             * window around 12z. If it doesn't, then do not use it. Otherwise,
             * force the obstime of this station to be 12z.
             */
            if (isNear12Z(rec.getObsTime())) {
                dp.set(Calendar.HOUR_OF_DAY, 12);
                dp.set(Calendar.MINUTE, 0);
                dp.set(Calendar.SECOND, 0);
                rec.setObsTime(dp.getTime());
            } else {
                status = GPP_SKIP_RECORD;
            }
            break;
        }
        case 2001: {
            /* PPD */

            /* This is a 24 hour PP report. */
            rec.setDataDuration(24);

            /*
             * Check to make sure that this report falls within a specified time
             * window around 12z. If it doesn't, then do not use it. Otherwise,
             * force the obstime to be 12z.
             */

            if (isNear12Z(rec.getObsTime())) {
                dp.set(Calendar.HOUR_OF_DAY, 12);
                dp.set(Calendar.MINUTE, 0);
                dp.set(Calendar.SECOND, 0);
                rec.setObsTime(dp.getTime());
            } else {
                status = GPP_SKIP_RECORD;
            }

            break;
        }
        default:

            /* The duration of this report was not recognized. */
            msgstr = String.format("%s %s %d %s %s has unsupported duration",
                    rec.getLocationId(), rec.getPhysicalElement().getCode(),
                    rec.getShefDuration().getValue(), rec.getTypeSource()
                            .getCode(), rec.getObsTime());

            logger.info(msgstr);
            status = GPP_SKIP_RECORD;
            break;
        } // switch duration

        if (status == GPP_OK) {

            if ((rec.getValue() != MISSING_PRECIP)
                    && rec.getDataDuration() != 24) {
                float value = rec.getValue() * 100;

                if (value > SHRT_MAX) {
                    logger.info("Precip value exceeds capacity "
                            + "of short integer. Setting value " + "to "
                            + MISSING_PRECIP);
                    value = MISSING_PRECIP;
                }

                /*
                 * Make certain that the value is properly rounded. This will
                 * prevent problems when downcasting to a short.
                 */
                rec.setValue(Math.round((value)));
            }
            
            if ((Float) (rec.getValue()) != null && rec.getLocationId() != null) {
                prev_dur = rec.getShefDuration().getValue();
                status = GPP_OK;
                cur_key = rec.getLocationId() + " " + rec.getPhysicalElement()
                        + " " + rec.getDataDuration() + " "
                        + rec.getTypeSource().getCode() + " ";
                if (status == GPP_OK) {
                    if (rec.getDataDuration() != 24) {
                        cur_key = cur_key + btime.getTime();
                    } else {
                        cur_key = cur_key + rec.getObsTime();
                    }

                    switch (rec.getDataDuration()) {
                    case 0:
                    case 1:
                    case 6:
                        gagePPWrite = new GagePPWrite();
                        gagePPWrite.gage_pp_init(dt.getTime(), rec
                                .getLocationId(),
                                rec.getTypeSource().getCode(), rec.getValue(),
                                obstime_ytd, '0', 'M');
                        initialize_hourlypp(hourlyPP, rev_6hour_code,
                                msgstr, rec, rec.getObsTime());
                        obstime_prev = obstime_ytd;
                        break;

                    case 24:
                        PrecipUtils.initialize_dailypp(dailyPP, rec);
                        obstime_prev = sdf.format(rec.getObsTime());
                        break;

                    default:
                        break;
                    }

                    use_value = 1;

                    switch (rec.getDataDuration()) {
                    case 0:
                    case 1:
                        use_value = use_1_hour_value(pOptions, rec, hourlyPP,
                                offset_code, hour);
                        break;

                    case 6:
                        use_value = PrecipUtils.use_6_hour_value(pOptions, rec,
                                hourlyPP, offset_code[0], p6HourSlot);
                        break;

                    case 24:
                        use_value = PrecipUtils.use_24_hour_value(pOptions,
                                rec, dailyPP);
                        break;

                    default:
                        break;
                    }

                    if (use_value == 1) {
                        switch (rec.getDataDuration()) {
                        case 0:
                        case 1:
                            precip_val = (short) rec.getValue();
                            PrecipUtils.set_hour_slot_value(hourlyPP, hour,
                                    precip_val);
                            hourlyPP.setHourlyQc(String.valueOf(rec
                                    .getQualCode()));
                            hourlyPP.setMinuteOffset(String
                                    .valueOf(offset_code[0]));
                            
                            rev_code = (short) ((rec.revision) ? 1 : 0);
                            break;
                        case 6:
                            precip_val = (short) rec.getValue();
                            PrecipUtils.set_6hour_slot_value(hourlyPP, p6HourSlot,
                                    precip_val);
                            hourlyPP.setSixhrqc(String.valueOf(rec
                                    .getQualCode()));
                            hourlyPP.setSixhroffset(String.valueOf(offset_code[0]));
                            
                            rev_6hour_code[p6HourSlot] = (short)((rec.revision) ? 1 : 0);
                            break;
                        case 24:
                            dailyPP.setValue(Double.valueOf(rec.getValue()));
                            dailyPP.setQc(String.valueOf(rec.getQualCode()));

                            rev_24hour_code = rec.revision;
                            break;
                        default:
                            break;
                        }
                    }
                    /*
                     * Increment the count of total values. This should always
                     * be equal to the sum of the ignored value count and the
                     * used value count.
                     */
                    // Record the current duration as the previous duration
                    prev_dur = rec.getShefDuration().getValue();
                }
            }

            write_precip_data(hourlyPP, dailyPP, write_info, pOptions,
                    rev_code, rev_6hour_code, rev_24hour_code, obstime_prev, 1,
                    0, msgstr, prev_pe, prev_dur, rec);

            status = 1;
        } else if (status == GPP_BADRECORD) {
            logger.debug("Incomplete record ignored");
        } else if (status == GPP_SKIP_RECORD) {
            logger.error(" " + rec.getLocationId() + " "
                    + rec.getPhysicalElement() + " " + rec.getShefDuration()
                    + " " + rec.getTypeSource().getCode() + " "
                    + rec.getObsTime() + ": Record skipped");
        }


        return status;
    }

    /**
     * 
     * @param pHourlyPP
     * @param pDailyPP
     * @param pWriteInfo
     * @param pOptions
     * @param rev_code
     * @param rev_6hour_code
     * @param rev_24hour_code
     * @param obstime
     * @param used_value_count
     * @param ignored_value_count
     * @param msgstr
     * @param prev_pe
     * @param duration
     * @param rec
     */
    public void write_precip_data(final Hourlypp pHourlyPP,
            final Dailypp pDailyPP, WriteInfo pWriteInfo,
            final GagePPOptions pOptions, final short rev_code,
            final short[] rev_6hour_code, boolean rev_24hour_code,
            final String obstime, int used_value_count,
            int ignored_value_count, String msgstr, final String prev_pe,
            int duration, PrecipRecord rec) {
        final String[] db_action_strings = { "Inserted record",
                "Updated record", "Ignored record" };
        int status;

        switch (duration) {
        case 0:
        case 60:
        case 1001:
        case 1006:
            status = GPP_OK;
//            PrecipUtils.set_6hour_slot_value(pHourlyPP, pHourSlot,
//                    rec.getValue());
            try {
                Short oneHourlyVal = PrecipUtils.get_hour_slot_value(pHourlyPP, pHourSlot);
                Short sixHourlyVal = PrecipUtils.get_6hour_slot_value(pHourlyPP, p6HourSlot);
                
                gagePPWrite
                        .gage_pp_write_rec(pHourlyPP, rec.getPhysicalElement()
                                .getCode(), rec.getObsTime(), pOptions,
                                rev_code, rev_6hour_code, oneHourlyVal,
                                sixHourlyVal, rec.getQualCode(), rec.getQualifier());
            } catch (Exception e) {
                logger.error("Unable to write to the database : " + e);
                status = GPP_ERROR;
            }
            if (status == GPP_OK) {
                logger.info(" " + pHourlyPP.getId().getLid() + " " + prev_pe
                        + " " + duration + " " + pHourlyPP.getTs() + " "
                        + rec.getObsTime().toString() + ": "
                        + db_action_strings[pWriteInfo.db_action]);

                pWriteInfo.num_hr_ignored += ignored_value_count;
                msgstr = gage_pp_write_info(msgstr, pWriteInfo);
                logger.info(msgstr);
            } else {
                logger.info(msgstr);
            }

            break;

        case 2001:
        case 5004:

            status = GagePPWrite.gage_pp_write_daily_rec(pDailyPP, pOptions,
                    rec.getObsTime(), rev_24hour_code, rec.getQualCode(), rec.getQualifier());
            if (status == GPP_OK) {
                msgstr = String.format("   %s %s %d %s %s: %s; ",
                        rec.getLocationId(), "PP", duration,
                        rec.getTypeSource(), rec.getObsTime(),
                        db_action_strings[pWriteInfo.db_action]);

                /* Don't need to update the number of values ignored here. */
                gage_pp_write_info(msgstr, pWriteInfo);
                logger.info(msgstr);
            } else {
                logger.info(msgstr);
            }

            break;

        default:

            break;

        }
    }

    /**
     * 
     * @param pOptions
     * @param pPE
     * @param rec
     */
    private void gage_pp_1hour_slot(final GagePPOptions pOptions,
            final String pPE, PrecipRecord rec) {
        Calendar dt = Calendar.getInstance(SHEFTimezone.GMT_TIMEZONE);
        dt.setTime(rec.getObsTime());

        int hour = dt.get(Calendar.HOUR_OF_DAY);

        int minute = dt.get(Calendar.MINUTE);
        
        if (((pPE.charAt(1) == 'P' && minute >= MINUTES_PER_HOUR
                - pOptions.getIntlppp()))
                || (pPE.charAt(1) == 'C' && minute >= MINUTES_PER_HOUR
                        - pOptions.getIntpc())) {
            hour++;
            dt.add(Calendar.HOUR_OF_DAY, 1);
            rec.setObsTime(dt.getTime());
        }
        if (hour == 0) {
            // hour = 24;
            dt.add(Calendar.DAY_OF_YEAR, -1);
            rec.setObsTime(dt.getTime());
        }
        
        pOffsetCode = PrecipUtils.get_offset_code(minute);
        pHourSlot = (short) hour;
    }

    /**
     * 
     */
    private int use_1_hour_value(final GagePPOptions pOptions,
            final PrecipRecord pPrecipRecord, final Hourlypp pHourlyPP,
            char[] offset_code, short hour) {

        int useValue = 1;

        Short precip_val = PrecipUtils.get_hour_slot_value(pHourlyPP, hour);
        if (precip_val != null) {

            short rev = (short) ((pPrecipRecord.revision) ? 1 : 0);

            offset_code[0] = pOffsetCode;
            
            char qc = 'M';
            if (pHourlyPP.getHourlyQc() == null) {
                pHourlyPP.setHourlyQc("------------------------");
            }
            useValue = PrecipUtils.use_precip_value(pPrecipRecord.getValue(),
                    precip_val, qc, pHourlyPP.getHourlyQc().charAt(hour - 1),
                    '-', pOffsetCode, pOptions.getShef_duplicate()
                            .name(), rev);
        }
        return useValue;
    }
    
    private void initialize_hourlypp(Hourlypp pHourlyPP,
            short rev_6hour_code[], String msgstr,
            final PrecipRecord pPrecipRecord, Date obstime_ytd) {

        /* initialize the 1 hour slots */
        pHourlyPP.setMinuteOffset("------------------------");
        pHourlyPP.setHourlyQc("------------------------");
        pHourlyPP.initHourlyValues(null);
        pHourlyPP.setSixhrqc("----");
        pHourlyPP.setSixhroffset("----");
        pHourlyPP.init6HourlyValues(null);

        Arrays.fill(rev_6hour_code, (short) 0);
    }

    // *********************************************************
    // These static methods are ok
    // *********************************************************

    /**
     * 
     * @param yearsec_ansi
     * @param pTimet12z
     * @return
     */
    private static boolean isNear12Z(final Date yearsec_ansi) {
        int ppp_ppd_window = appsDefaults.getInt("ppp_ppd_local_7am_window", DEFAULT_WINDOW_HOURS);
        ppp_ppd_window *= SECONDS_PER_HOUR * 1000L;

        // Observation time as a calendar
        Calendar timeTObs = TimeTools.getSystemCalendar();
        timeTObs.setTime(yearsec_ansi);

        // Create a 12Z object
        Calendar pStructTm = TimeTools.newCalendar(yearsec_ansi.getTime());
        pStructTm.set(Calendar.HOUR_OF_DAY, 12);
        pStructTm.set(Calendar.MINUTE, 0);
        pStructTm.set(Calendar.SECOND, 0);

        // -----------------------------------------------------------------

        Calendar upper_bound = Calendar.getInstance(SHEFTimezone.GMT_TIMEZONE);
        upper_bound.setTimeInMillis(pStructTm.getTimeInMillis()
                + ppp_ppd_window);
        Calendar lower_bound = Calendar.getInstance(SHEFTimezone.GMT_TIMEZONE);
        lower_bound.setTimeInMillis(pStructTm.getTimeInMillis()
                - ppp_ppd_window);

        return ((timeTObs.getTimeInMillis() >= lower_bound.getTimeInMillis())
                && timeTObs.getTimeInMillis() <= upper_bound.getTimeInMillis());
    }

    /**
     * 
     */
    private static int gage_pp_6hour_slot(final GagePPOptions pOptions,
            final Calendar ansi_obstime_year_sec, String ansi_date_year_day,
            short [] p6HourSlot, char[] p6HourOffsetCode) {

        float ppq_window = pOptions.getIntppq();
        ppq_window *= SECONDS_PER_HOUR;
        
        int bottom_6_hour_period;
        int diff1;
        int diff2;
        int hour = ansi_obstime_year_sec.get(Calendar.HOUR_OF_DAY);
        int minute = ansi_obstime_year_sec.get(Calendar.MINUTE);
        int num_periods;
        int remainder;
        int top_6_hour_period;
//        Calendar num_seconds_since_00z = Calendar
//                .getInstance(SHEFTimezone.GMT_TIMEZONE);

        int num_seconds_since_00z = (hour * SECONDS_PER_HOUR)
                + (minute * SECONDS_PER_MINUTE);

        bottom_6_hour_period = num_seconds_since_00z / SECONDS_IN_6HOUR_PERIOD;
        top_6_hour_period = bottom_6_hour_period + 1;

        diff1 = num_seconds_since_00z
                - (bottom_6_hour_period * SECONDS_IN_6HOUR_PERIOD);
        diff2 = (top_6_hour_period * SECONDS_IN_6HOUR_PERIOD)
                - num_seconds_since_00z;

        if (diff1 < diff2) {
            /*
             * the report is closest to the bottom 6 hour period. Check if the
             * report falls within the acceptable time windows.
             */
            if (diff1 <= ppq_window) {
                /*
                 * The report falls within the allotted time window. Determine
                 * the offset code.
                 */
                num_periods = diff1 / NUM_SECONDS_PER_6HOUR_OFFSET;
                remainder = diff1 % NUM_SECONDS_PER_6HOUR_OFFSET;

                if (remainder != 0) {
                    num_periods++;
                }

                if (bottom_6_hour_period == 0) {
                    ansi_obstime_year_sec.add(Calendar.DAY_OF_YEAR, -1);
                    // Set the hour slot to be the fourth period
                    p6HourSlot[0] = 4;
                } else {
                    p6HourSlot[0] = (short) bottom_6_hour_period;
                }
            } else {
                /*
                 * This report cannot be used. It is outside the window of
                 * acceptable time. Skip this report.
                 */
                return GPP_SKIP_RECORD;
            }
        } else {
            /*
             * The report is closest to the top 6 hour period. Check if the
             * report falls within the acceptable time window.
             */

            if (diff2 <= ppq_window) {
                num_periods = diff2 / NUM_SECONDS_PER_6HOUR_OFFSET;
                remainder = diff2 % NUM_SECONDS_PER_6HOUR_OFFSET;

                if (remainder != 0) {
                    num_periods++;
                }

                num_periods = MINUTES_PER_HOUR - num_periods;
                p6HourSlot[0] = (short) top_6_hour_period;
            } else {
                return GPP_SKIP_RECORD;
            }
        }
        /*
         * Using the number of NUM_MINUTES_PER_6HOUR_OFFSET periods, get the
         * offset code.
         */
        
        p6HourSlot[0]--;
        
        p6HourOffsetCode[0] = PrecipUtils.get_offset_code(num_periods);
        return GPP_OK;
    }

    /**
     * @param msgstr
     * @param writeInfo
     */
    private static String gage_pp_write_info(String msgstr, WriteInfo writeInfo) {
        String temp = "";
        int num_ignored = writeInfo.num_hr_ignored;
        int num_inserts = writeInfo.num_hr_inserts;
        int num_updates = writeInfo.num_hr_updates;

        if (num_inserts != 0) {
            temp.concat("Inserted " + num_inserts + ", ");
        }
        if (num_updates != 0) {
            temp.concat("Replaced " + num_updates + ", ");
        }
        if (num_ignored != 0) {
            temp.concat("Ignored " + num_ignored + ", ");
        }

        return temp;
    }

    
    
    public static final void main(String [] args) {
        
        GagePPOptions pOptions = new GagePPOptions();
        pOptions.setIntlppp(2);
        pOptions.setIntpc(10);
        pOptions.setIntppq(2);
        pOptions.setIntuppp(2);
        
        Calendar obstime = Calendar.getInstance(TimeZone.getTimeZone("Zulu"));
        obstime.set(Calendar.YEAR, 2011);
        obstime.set(Calendar.MONTH, 4);
        obstime.set(Calendar.DAY_OF_MONTH, 16);
        obstime.set(Calendar.HOUR_OF_DAY, 0);
        obstime.set(Calendar.MINUTE, 0);
        obstime.set(Calendar.SECOND, 0);
        obstime.set(Calendar.MILLISECOND, 0);
        
        
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(SHEFTimezone.GMT_TIMEZONE);

        String time = sdf.format(obstime.getTime());
        
        short [] p6HourSlot = { 0 };
        char[] p6HourOffsetCode = { ' ' };

        gage_pp_6hour_slot(pOptions, obstime, time, p6HourSlot, p6HourOffsetCode);
        
        System.out.println(p6HourSlot[0]);
        System.out.println(p6HourOffsetCode[0]);
    }
}
