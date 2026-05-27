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
package com.raytheon.uf.edex.activetable;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import java.util.concurrent.TimeUnit;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.OperationalActiveTableRecord;
import com.raytheon.uf.common.activetable.PracticeActiveTableRecord;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Service for the VTEC active table. Determines if the VTEC product corresponds
 * to a site we want in the active table, and if so, updates the active table.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 17, 2009           njensen   Initial creation
 * Jul 14, 2009  2950     njensen   Multiple site support
 * Dec 21, 2009  4055     njensen   No site filtering
 * Jun 17, 2014  3296     randerso  Added performance logging
 * Dec 09, 2014  3885     dgilling  Handle offset time from camel route headers.
 * Nov 03, 2016  5934     randerso  Moved transformFromWarnings method to
 *                                  ActiveTableSrv
 * Sep 25, 2017  6449     randerso  Changed to convert operational warnings to
 *                                  active table records before sending to
 *                                  vtecArrived. Made sure all active table
 *                                  records share a single instance of the
 *                                  rawMessage.
 *
 * </pre>
 *
 * @author njensen
 */

public class ActiveTableSrv {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ActiveTableSrv.class);

    private static ThreadLocal<ActiveTable> threadLocalActiveTable = new ThreadLocal<ActiveTable>() {

        /*
         * (non-Javadoc)
         *
         * @see java.lang.ThreadLocal#initialValue()
         */
        @Override
        protected ActiveTable initialValue() {
            return new ActiveTable();
        }

    };

    /**
     * Merge VTEC info from new warning records into the active table
     *
     * @param records
     */
    public void vtecArrived(List<ActiveTableRecord> records) {
        ITimer timer = TimeUtil.getTimer();
        timer.start();
        try {
            ActiveTable activeTable = threadLocalActiveTable.get();
            if ((records != null) && (records.size() > 0)) {
                activeTable.merge(records);
            }
        } catch (Throwable t) {
            statusHandler.handle(Priority.PROBLEM, "Error merging active table",
                    t);
        }
        timer.stop();
        PerformanceStatus.getHandler("ActiveTable").logDuration(
                "Total time to process " + records.size() + " records",
                timer.getElapsedTime());
    }

    /**
     * Merge new warning records into the practice active table
     *
     * @param records
     * @param headers
     */
    public void practiceVtecArrived(List<AbstractWarningRecord> records,
            Headers headers) {
        int offsetSeconds = getOffsetTime((String) headers.get("drtstring"));
        if ((records != null) && (records.size() > 0)) {
            ActiveTable activeTable = threadLocalActiveTable.get();
            try {
                activeTable.merge(transformFromWarnings(records,
                        ActiveTableMode.PRACTICE), offsetSeconds);
            } catch (Throwable t) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error processing new VTEC products for PRACTICE active table",
                        t);
            }
        }
    }

    private int getOffsetTime(String drtTimeString) {
        if (drtTimeString != null) {
            DateFormat drtParse = new SimpleDateFormat("yyyyMMdd_HHmm");
            drtParse.setTimeZone(TimeZone.getTimeZone("GMT"));

            try {
                Date drtTime = drtParse.parse(drtTimeString);
                Date currentTime = new Date();
                long diffInMillis = drtTime.getTime() - currentTime.getTime();

                return (int) TimeUnit.SECONDS.convert(diffInMillis,
                        TimeUnit.MILLISECONDS);
            } catch (ParseException e) {
                statusHandler.error(
                        "Could not parse DRT time string: " + drtTimeString, e);
            }
        }

        return 0;
    }

    /**
     * Transform warning records to operational active table records
     *
     * @param warnings
     * @return list of active table records
     */
    public List<ActiveTableRecord> transformFromOperationalWarnings(
            List<AbstractWarningRecord> warnings) {
        return transformFromWarnings(warnings, ActiveTableMode.OPERATIONAL);
    }

    private List<ActiveTableRecord> transformFromWarnings(
            List<AbstractWarningRecord> warnings, ActiveTableMode mode) {
        List<ActiveTableRecord> list = new ArrayList<>();
        String rawMessage = null;
        /*
         * All warning records in the list will have the same rawMessage since
         * they were all decoded from the same product.
         *
         * Save off the value from the first record here and use it to populate
         * the active table records so they all share a single Java String
         * instance.
         */
        if (!warnings.isEmpty()) {
            rawMessage = warnings.get(0).getRawmessage();
        }
        for (AbstractWarningRecord wr : warnings) {
            ActiveTableRecord atr = null;
            if (mode.equals(ActiveTableMode.OPERATIONAL)) {
                atr = new OperationalActiveTableRecord();
            } else {
                atr = new PracticeActiveTableRecord();
            }
            atr.setAct(wr.getAct());
            atr.setCountyheader(wr.getCountyheader());
            atr.setEndTime(calendarToDate(wr.getEndTime()));
            atr.setEtn(wr.getEtn());
            atr.setFloodBegin(calendarToDate(wr.getFloodBegin()));
            atr.setFloodCrest(calendarToDate(wr.getFloodCrest()));
            atr.setFloodEnd(calendarToDate(wr.getFloodEnd()));
            atr.setFloodRecordStatus(wr.getFloodRecordStatus());
            atr.setFloodSeverity(wr.getFloodSeverity());
            atr.setForecaster(wr.getForecaster());
            atr.setImmediateCause(wr.getImmediateCause());
            atr.setIssueTime(calendarToDate(wr.getIssueTime()));
            atr.setLoc(wr.getLoc());
            atr.setLocationID(wr.getLocationID());
            atr.setMotdir(wr.getMotdir());
            atr.setMotspd(wr.getMotspd());
            atr.setOfficeid(wr.getOfficeid());
            atr.setOverviewText(wr.getOverviewText());
            atr.setPhen(wr.getPhen());
            atr.setPhensig(wr.getPhensig());
            atr.setPil(wr.getPil());
            atr.setProductClass(wr.getProductClass());
            atr.setPurgeTime(calendarToDate(wr.getPurgeTime()));
            atr.setRawmessage(rawMessage);
            atr.setRegion(wr.getRegion());
            atr.setSeg(wr.getSeg());
            atr.setSegText(wr.getSegText());
            atr.setSig(wr.getSig());
            atr.setStartTime(calendarToDate(wr.getStartTime()));
            atr.setUfn(wr.isUfn());
            atr.setVtecstr(wr.getVtecstr());
            atr.setWmoid(wr.getWmoid());
            atr.setXxxid(wr.getXxxid());

            for (String ugc : wr.getUgcZones()) {
                ActiveTableRecord ugcRecord = (ActiveTableRecord) atr.clone();
                ugcRecord.setUgcZone(ugc);
                list.add(ugcRecord);
            }
        }

        return list;
    }

    private static Date calendarToDate(Calendar calendar) {
        Date date = null;
        if (calendar != null) {
            date = calendar.getTime();
        }
        return date;
    }
}
