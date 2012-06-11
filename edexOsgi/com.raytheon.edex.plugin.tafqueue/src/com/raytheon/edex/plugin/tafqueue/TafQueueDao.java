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
package com.raytheon.edex.plugin.tafqueue;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import java.util.concurrent.locks.Lock;

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
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class TafQueueDao extends CoreDao {

    public TafQueueDao() {
        super(DaoConfig.forClass(Lock.class));
    }

    /**
     * Remove old records no longer needed for the logs.
     * 
     * @return number of records purged
     * @throws PluginException
     */
    public int purgeExpiredData() throws PluginException {
        String sql = "DELETE FROM taf_queue where xmitTime < (CURRENT_TIMESTAMP - (interval '7 days'));";
        return executeSQLUpdate(sql);
    }

    /**
     * Obtain all the records with the given TafQueueState in order by xmitTime.
     * 
     * @param state
     * @return obs
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public List<TafQueueRecord> getRecordsByState(
            TafQueueRecord.TafQueueState state) throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(TafQueueRecord.class.getName());
        query.addQueryParam("state", state.toString(), QueryOperand.EQUALS);
        query.addQueryParam("display", Boolean.TRUE, QueryOperand.EQUALS);
        query.addOrder("xmitTime", true);
        List<TafQueueRecord> obs = (List<TafQueueRecord>) queryByCriteria(query);
        return obs;
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
     * Obtain list of records in the PENDING state ordered by xmitTime.
     * 
     * @return obs
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public List<TafQueueRecord> getRecordsToSend()
            throws DataAccessLayerException {
        Calendar cal = Calendar.getInstance();
        cal.setTimeZone(TimeZone.getTimeZone("GMT"));
        DatabaseQuery query = new DatabaseQuery(TafQueueRecord.class.getName());
        query.addQueryParam("state",
                TafQueueRecord.TafQueueState.PENDING.toString());
        query.addQueryParam("display", Boolean.TRUE, QueryOperand.EQUALS);
        query.addQueryParam("xmitTime", cal.getTime(),
                QueryOperand.LESSTHANEQUALS);
        List<TafQueueRecord> obs = (List<TafQueueRecord>) queryByCriteria(query);
        return obs;
    }

    /**
     * Change the state of records and if the state is SENT or ERROR update the
     * status message of all obs.
     * 
     * @param obs
     * @param state
     */
    public int updateState(List<TafQueueRecord> obs,
            TafQueueRecord.TafQueueState state) {
        if (obs == null) {
            return -1;
        } else if (obs.size() == 0) {
            return 0;
        }

        StringBuilder sb = new StringBuilder("UPDATE taf_queue SET state = '")
                .append(state.toString()).append("' where id IN (");
        String prefix = "";
        for (TafQueueRecord ob : obs) {
            sb.append(prefix).append(ob.getId());
            ob.setState(state);
            prefix = ", ";
        }
        sb.append(");");
        if (state == TafQueueState.SENT || state == TafQueueState.BAD) {
            // update the messages
            for (TafQueueRecord ob : obs) {
                sb.append("\nUPDATE taf_queue SET statusMessage = '")
                        .append(ob.getStatusMessage()).append("' where id = ")
                        .append(ob.getId()).append(";");
            }
        }
        return executeSQLUpdate(sb.toString());
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
    public String getLogMessages(Date startTime, Date endTime)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(TafQueueRecord.class.getName());
        query.addQueryParam("state", TafQueueRecord.TafQueueState.PENDING,
                QueryOperand.NOTEQUALS);
        query.addQueryParam("xmitTime", new Object[] { startTime, endTime },
                QueryOperand.BETWEEN);
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
     *            - List of ids tor remove
     * @param state
     *            - The state of list idList came from.
     * @return
     */
    public int removeSelected(List<String> idList,
            TafQueueRecord.TafQueueState state) {
        StringBuilder sb = new StringBuilder();

        if (state == TafQueueState.PENDING) {
            // No need to keep around for the logs so really remove the data
            sb.append("DELETE from taf_queue WHERE id IN (");
            String prefix = "";
            for (String id : idList) {
                sb.append(prefix).append(id);
                prefix = ", ";
            }
            sb.append(") and state = 'PENDING';");
        } else {
            // SENT and BAD need to be kept around for the logs
            sb.append("UPDATE taf_queue set display = FALSE where id in (");
            String prefix = "";
            for (String id : idList) {
                sb.append(prefix).append(id);
                prefix = ", ";
            }
            sb.append(") and state = '").append(state).append("';");
        }
        return executeSQLUpdate(sb.toString());
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
        return idList.size();
    }
}
