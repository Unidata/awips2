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
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.tafqueue.ServerResponse;
import com.raytheon.uf.common.tafqueue.TafQueueRecord;
import com.raytheon.uf.common.tafqueue.TafQueueRecord.TafQueueState;
import com.raytheon.uf.common.tafqueue.TafQueueRequest;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * This handles the CAVE requests to the taf_queue table.
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
public class TafQueueRequestHandler implements IRequestHandler<TafQueueRequest> {
    private ServerResponse<?> response;

    private TafQueueDao dao;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object handleRequest(TafQueueRequest request) {
        List<String> idList = null;
        Map<String, Object> arguments = request.getArguments();
        TafQueueState state = request.getState();
        try {
            dao = new TafQueueDao();
            switch (request.getType()) {
            case CREATE:
                response = new ServerResponse<String>();
                for (TafQueueRecord record : request.getRecords()) {
                    dao.create(record);
                    response.addMessage(record.getInfo());
                }
                break;
            case GET_LIST:
                response = new ServerResponse<List<String>>();
                makeList(request.getState());
                break;
            case GET_LOG:
                response = new ServerResponse<String>();
                Date startTime = (Date) arguments.get("starttime");
                Date endTime = (Date) arguments.get("endtime");
                String log = dao.getLogMessages(startTime, endTime);
                ((ServerResponse<String>) response).setPayload(log);
                break;
            case GET_TAFS:
                response = new ServerResponse<String>();
                List<TafQueueRecord> records = (List<TafQueueRecord>) dao
                        .getRecordsById((List<String>) arguments.get("idlist"));
                makeTafs(records);
                break;
            case REMOVE_SELECTED:
                response = new ServerResponse<List<String>>();
                idList = (List<String>) arguments.get("idlist");
                int numRemoved = dao.removeSelected(idList, state);
                if (idList.size() != numRemoved) {
                    response.setError(true);
                    response.addMessage((idList.size() - numRemoved)
                            + " forecast(s) not in "
                            + state.toString().toLowerCase() + " state.");
                } else {
                    response.addMessage(numRemoved + " "
                            + state.toString().toLowerCase()
                            + " forecast(s) removed.");
                }
                makeList(state);
                break;
            case RETRANSMIT:
                response = new ServerResponse<List<String>>();
                idList = (List<String>) arguments.get("idlist");
                int retransNum = dao.retransmit(idList);
                if (retransNum == idList.size()) {
                    response.addMessage("Forecast(s) queued for immediate transmission.");
                } else {
                    response.setError(true);
                    response.addMessage("Unable queue all forecast(s) for immediate transmission.");
                }
                makeList(request.getState());
                break;
            default:
                response = new ServerResponse<String>();
                response.addMessage("Unknown type: " + request.getType());
                response.setError(true);
            }
        } catch (DataAccessLayerException e) {
            response.addMessage(e.getMessage());
            response.setError(true);
        }
        return response;
    }

    /**
     * Place in the response payload a list of the records in the desired state.
     * Each entry in the list is a string in the format "record_id,record_info".
     * 
     * @param state
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    private void makeList(TafQueueRecord.TafQueueState state)
            throws DataAccessLayerException {
        List<TafQueueRecord> records = dao.getRecordsByState(state);
        List<String> recordsList = new ArrayList<String>();
        StringBuilder sb = new StringBuilder();
        for (TafQueueRecord record : records) {
            sb.setLength(0);
            sb.append(record.getId()).append(",").append(record.getInfo());
            recordsList.add(sb.toString());
        }
        ((ServerResponse<List<String>>) response).setPayload(recordsList);
    }

    /**
     * Place in the response payload a string displayin all the TAFs in the
     * records.
     * 
     * @param records
     */
    @SuppressWarnings("unchecked")
    private void makeTafs(List<TafQueueRecord> records) {
        StringBuilder sb = new StringBuilder();
        String prefix = "";
        for (TafQueueRecord record : records) {
            sb.append(prefix).append(record.getTafText());
            prefix = "\n\n";
        }
        ((ServerResponse<String>) response).setPayload(sb.toString());
    }
}
