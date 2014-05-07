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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.tafqueue.TafQueueRecord;
import com.raytheon.uf.common.tafqueue.TafQueueRecord.TafQueueState;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * This class is used to maintain and obtain data in the taf_queue table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 1, 2012  14715      rferrel     Initial creation
 * Mar 21, 2013 15375      zhao        Added methods for handling VFT product
 * May 07, 2014 3091       rferrel     forecasterId now a string.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class TafQueueDao extends CoreDao {

    public TafQueueDao() {
        super(DaoConfig.forClass(TafQueueRecord.class));
    }

    /**
     * Remove old records no longer needed for the logs.
     * 
     * @return number of records purged
     * @throws PluginException
     * @throws DataAccessLayerException
     */
    public int purgeExpiredData() throws PluginException,
            DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(TafQueueRecord.class.getName());
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.DAY_OF_MONTH, -7);
        query.addQueryParam("xmitTime", cal.getTime(), QueryOperand.LESSTHAN);
        return deleteByCriteria(query);
    }

    /**
     * Generate a query by state common components.
     * 
     * @param state
     * @return query
     */
    private DatabaseQuery createByStateQuery(TafQueueState state) {
        DatabaseQuery query = new DatabaseQuery(TafQueueRecord.class.getName());
        query.addQueryParam("state", state.toString(), QueryOperand.EQUALS);
        query.addQueryParam("display", Boolean.TRUE, QueryOperand.EQUALS);
        query.addOrder("xmitTime", true);
        return query;
    }

    /**
     * Obtain all the records with the given TafQueueState in order by xmitTime.
     * 
     * @param state
     * @return records
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public List<TafQueueRecord> getRecordsByState(
            TafQueueRecord.TafQueueState state) throws DataAccessLayerException {
        DatabaseQuery query = createByStateQuery(state);
        List<TafQueueRecord> records = (List<TafQueueRecord>) queryByCriteria(query);
        return records;
    }

    /**
     * Get the time to transmit the next pending record.
     * 
     * @return xmitTime - Transmit time of next pending record or null if none.
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public Date nextXmitTime() throws DataAccessLayerException {
        Date xmitTime = null;
        DatabaseQuery query = createByStateQuery(TafQueueState.PENDING);
        query.setMaxResults(1);
        List<TafQueueRecord> records = (List<TafQueueRecord>) queryByCriteria(query);
        if (records.size() > 0) {
            xmitTime = records.get(0).getXmitTime();
        }
        return xmitTime;
    }

    /**
     * For list of ids obtain the records ordered by xmitTime.
     * 
     * @param idList
     * @return records
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public List<TafQueueRecord> getRecordsById(List<String> idList)
            throws DataAccessLayerException {
        List<Integer> idIntList = new ArrayList<Integer>();
        for (String id : idList) {
            idIntList.add(Integer.parseInt(id));
        }
        DatabaseQuery query = new DatabaseQuery(TafQueueRecord.class.getName());
        query.addQueryParam("id", idIntList, QueryOperand.IN);
        query.addOrder("xmitTime", true);
        List<TafQueueRecord> records = (List<TafQueueRecord>) queryByCriteria(query);
        return records;
    }

    /**
     * Obtain records that need to be transmitted ordered by xmitTime.
     * 
     * @return records
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public List<TafQueueRecord> getRecordsToSend()
            throws DataAccessLayerException {
        DatabaseQuery query = createByStateQuery(TafQueueState.PENDING);
        Calendar cal = Calendar.getInstance();
        cal.setTimeZone(TimeZone.getTimeZone("GMT"));
        query.addQueryParam("xmitTime", cal.getTime(),
                QueryOperand.LESSTHANEQUALS);
        List<TafQueueRecord> records = (List<TafQueueRecord>) queryByCriteria(query);
        return records;
    }

    /**
     * Change the state of records and if the state is SENT or ERROR update the
     * status message of all records.
     * 
     * @param records
     * @param state
     */
    public int updateState(List<TafQueueRecord> records,
            TafQueueRecord.TafQueueState state) {
        if (records == null) {
            return -1;
        } else if (records.size() == 0) {
            return 0;
        }
        for (TafQueueRecord record : records) {
            record.setState(state);
        }
        persistAll(records);
        return records.size();
    }

    /**
     * Get the log messages for all records in the SENT or BAD state.
     * 
     * @param startTime
     *            Start time
     * @param endTime
     *            End time
     * @return
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public String getLogMessages(List<Date> dateList)
            throws DataAccessLayerException {
        Date[] dates = new Date[dateList.size()];
        dateList.toArray(dates);
        DatabaseQuery query = new DatabaseQuery(TafQueueRecord.class.getName());
        query.addQueryParam("state", TafQueueRecord.TafQueueState.PENDING,
                QueryOperand.NOTEQUALS);
        query.addQueryParam("xmitTime", dates, QueryOperand.BETWEEN);
        query.addOrder("xmitTime", true);
        List<TafQueueRecord> obs = (List<TafQueueRecord>) queryByCriteria(query);
        StringBuilder sb = new StringBuilder();
        for (TafQueueRecord ob : obs) {
            if (ob.getState() == TafQueueState.SENT) {
                sb.append("SUCCESS ");
            } else {
                sb.append("FAILURE ");
            }
            sb.append(ob.getInfo()).append(" ").append(ob.getStatusMessage())
                    .append("\n");
        }
        return sb.toString();
    }

    /**
     * Remove list of ids from lists.
     * 
     * @param idList
     *            - List of ids to remove
     * @param state
     *            - The state of list idList came from.
     * @return numRecords - number of records affected
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public int removeSelected(List<String> idList,
            TafQueueRecord.TafQueueState state) throws DataAccessLayerException {
        List<Integer> ids = new ArrayList<Integer>();
        for (String id : idList) {
            ids.add(new Integer(id));
        }
        DatabaseQuery query = new DatabaseQuery(TafQueueRecord.class.getName());
        query.addQueryParam("id", ids, QueryOperand.IN);

        int numRecords = -999;

        if (state == TafQueueState.PENDING) {
            numRecords = deleteByCriteria(query);
        } else {
            // SENT and BAD need to be kept around for the logs
            query.addQueryParam("state", state.toString(), QueryOperand.EQUALS);
            List<TafQueueRecord> records = (List<TafQueueRecord>) queryByCriteria(query);
            for (TafQueueRecord record : records) {
                record.setDisplay(false);
            }
            persistAll(records);
            numRecords = records.size();
        }
        return numRecords;
    }

    /**
     * Retransmit entries in the list of records.
     * 
     * @param idList
     * @return
     * @throws DataAccessLayerException
     */
    public int retransmit(List<String> idList) throws DataAccessLayerException {
        List<TafQueueRecord> records = getRecordsById(idList);

        Calendar cal = Calendar.getInstance();
        cal.setTimeZone(TimeZone.getTimeZone("GMT"));
        Date xmitTime = cal.getTime();
        // Make copies so the both send/error messages will appear in the log.
        for (TafQueueRecord record : records) {
            record.setId(0); // force creation of new record.
            record.setDisplay(true);
            record.setStatusMessage("");
            record.setState(TafQueueState.PENDING);
            record.setXmitTime(xmitTime);
            create(record);
        }
        return records.size();
    }

    /**
     * (for DR15375) Get last xmit time for a forecaster id (for VFT purpose)
     * 
     * @param forecasterid
     * @return last xmittime; return null if no record exists
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public Date getLastXmitTimeByForecasterId(String forecasterid)
            throws DataAccessLayerException {
        Date lastXmittime = null;
        DatabaseQuery query = new DatabaseQuery(TafQueueRecord.class.getName());
        query.addQueryParam("forecasterId", forecasterid, QueryOperand.EQUALS);
        query.addOrder("xmitTime", false);
        List<TafQueueRecord> records = (List<TafQueueRecord>) queryByCriteria(query);
        if (records.size() > 0) {
            lastXmittime = records.get(0).getXmitTime();
        }
        return lastXmittime;
    }

    /**
     * (for DR15375) Retrieves a list of TAF records sent since the last VFT
     * product was created
     * 
     * @param lastVftTime
     *            (last VFT creation time)
     * @param forecasterid
     *            (forecaster ID for VFT)
     * @return a list of TAF records sent since lastVftTime or null when no such
     *         records exit
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public List<TafQueueRecord> getRecordsForVFT(Date lastVftTime,
            String forecasterid) throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(TafQueueRecord.class.getName());
        query.addQueryParam("xmitTime", lastVftTime,
                QueryOperand.GREATERTHANEQUALS);
        query.addQueryParam("forecasterId", forecasterid,
                QueryOperand.NOTEQUALS);
        query.addQueryParam("state", TafQueueState.SENT, QueryOperand.EQUALS);
        query.addOrder("xmitTime", true);
        List<TafQueueRecord> records = (List<TafQueueRecord>) queryByCriteria(query);
        return records;
    }
}
