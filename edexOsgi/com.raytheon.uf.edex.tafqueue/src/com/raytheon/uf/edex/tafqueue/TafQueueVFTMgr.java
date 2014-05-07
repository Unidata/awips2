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

package com.raytheon.uf.edex.tafqueue;

import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.tafqueue.TafQueueRecord;
import com.raytheon.uf.common.tafqueue.TafQueueVftConfigMgr;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * This class is used to create AvnFPS verification (VFT) products.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 07, 2013 15375      zhao        Initial creation
 * May 07, 2014 3091       rferrel     fcstid now a String.
 * 
 * </pre>
 * 
 * @author zhao
 * 
 */
public class TafQueueVFTMgr {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TafQueueVFTMgr.class);

    private static TafQueueVFTMgr instance = null;

    private TafQueueVftConfigMgr config = null;

    /** The wmo in config file. */
    private String wmoid;

    /** The siteid in config file. */
    private String siteid;

    /** The stationid in config file. */
    private String stationid;

    /** The forecasterid for VFT in config file. */
    private String fcstid;

    /**
     * Number of hours; default period of VFT product creation, to be replaced
     * by period in config file.
     */
    private int period;

    /** Default BBB field for a VFT product. */
    private String bbb;

    private Date lastVftTime = null;

    private Date nextVftTime = null;

    private TafQueueVFTMgr() {
        init();
        statusHandler.handle(Priority.INFO, "Tafqueue VFT manager created.");
    }

    /**
     * Create an AvnFPS VFT product, and updates time for next VFT product,
     */
    public void makeVftProduct() {
        TafQueueDao dao = new TafQueueDao();
        Calendar cal = Calendar.getInstance();
        cal.setTimeZone(TimeZone.getTimeZone("GMT"));
        nextVftTime = cal.getTime();
        try {
            List<TafQueueRecord> records = dao.getRecordsForVFT(lastVftTime,
                    fcstid);
            if (records != null) {
                if (records.size() > 0) {
                    StringBuilder sb = new StringBuilder();
                    for (TafQueueRecord record : records) {
                        sb.append(formatVftTafRecord(record));
                        sb.append("\n");
                    }
                    String vftText = sb.toString();
                    vftText = vftText.substring(0, vftText.length() - 1);
                    TafQueueRecord vftRecord = new TafQueueRecord(fcstid,
                            nextVftTime, vftText, bbb, siteid, wmoid,
                            stationid, nextVftTime);
                    dao.create(vftRecord);
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Tafqueue VFT manager: error occurred while making a VFT product.\n"
                            + e.getLocalizedMessage(), e);
        }
        lastVftTime = nextVftTime;
        statusHandler.handle(
                Priority.INFO,
                "Tafqueue VFT manager: Last VFT xmit time = "
                        + lastVftTime.toString());
        cal.add(Calendar.HOUR_OF_DAY, period);
        nextVftTime = cal.getTime();
        statusHandler.handle(
                Priority.INFO,
                "Tafqueue VFT manager: Next VFT xmit time = "
                        + nextVftTime.toString());
    }

    private String formatVftTafRecord(TafQueueRecord record) {
        String tafText = record.getTafText();
        int indexOfTafPeriod = tafText.indexOf('/');
        String tafPeriod = tafText.substring(indexOfTafPeriod - 4,
                indexOfTafPeriod + 5);
        String tafBbb = record.getBbb();
        if (tafBbb.equals("") || tafBbb.equals("   ")) {
            tafBbb = "___";
        }
        return String.format("%1$s %2$s %3$ty%3$tm%3$td%3$tH%4$s "
                + "%5$s %6$s %7$ty%7$tm%7$td%7$tH%7$tm%8$s %9$s %10$s",
                record.getWmoId(), record.getStationId(),
                record.getHeaderTime(), "00", tafBbb, record.getStationId()
                        .charAt(0) + record.getSiteId(), record.getXmitTime(),
                "Z", tafPeriod, record.getForecasterId());
    }

    private void init() {
        config = TafQueueVftConfigMgr.getInstance();
        wmoid = config.getWmoid();
        siteid = config.getSiteid();
        stationid = config.getStationid();
        fcstid = config.getFcstid();
        period = config.getPeriod();
        bbb = config.getBbb();

        // determine lastVftTime and nextVftTime
        TafQueueDao dao = new TafQueueDao();
        try {
            lastVftTime = dao.getLastXmitTimeByForecasterId(fcstid);
        } catch (DataAccessLayerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Tafqueue VFT manager: error occurred while querying for last VFT xmit time.\n"
                            + e.getLocalizedMessage(), e);
        }
        Calendar cal = Calendar.getInstance();
        cal.setTimeZone(TimeZone.getTimeZone("GMT"));
        if (lastVftTime == null) {
            nextVftTime = cal.getTime();
            cal.add(Calendar.HOUR_OF_DAY, -period);
            lastVftTime = cal.getTime();
        } else {
            cal.setTime(lastVftTime);
            cal.add(Calendar.HOUR_OF_DAY, period);
            nextVftTime = cal.getTime();
        }
        statusHandler.handle(
                Priority.INFO,
                "Tafqueue VFT manager: Last VFT xmit time = "
                        + lastVftTime.toString());
        statusHandler.handle(
                Priority.INFO,
                "Tafqueue VFT manager: Next VFT xmit time = "
                        + nextVftTime.toString());
    }

    public static synchronized TafQueueVFTMgr getInstance() {
        if (instance == null) {
            instance = new TafQueueVFTMgr();
        }
        return instance;
    }

    public void setWmoid(String wmoid) {
        this.wmoid = wmoid;
    }

    public String getWmoid() {
        return wmoid;
    }

    public void setPeriod(int period) {
        this.period = period;
    }

    public int getPeriod() {
        return period;
    }

    public void setSiteid(String siteid) {
        this.siteid = siteid;
    }

    public String getSiteid() {
        return siteid;
    }

    public void setStationid(String stationid) {
        this.stationid = stationid;
    }

    public String getStationid() {
        return stationid;
    }

    public void setFcstid(String fcstid) {
        this.fcstid = fcstid;
    }

    public String getFcstid() {
        return fcstid;
    }

    public Date getNextVftTime() {
        return nextVftTime;
    }

}
