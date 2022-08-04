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
package com.raytheon.uf.edex.aviation;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.tafqueue.ServerResponse;
import com.raytheon.uf.common.tafqueue.TafQueueRecord;
import com.raytheon.uf.common.tafqueue.TafQueueRecord.TafQueueState;
import com.raytheon.uf.common.tafqueue.TafQueueRequest;
import com.raytheon.uf.common.tafqueue.TafQueueRequest.Type;
import com.raytheon.uf.edex.auth.AuthManagerFactory;
import com.raytheon.uf.edex.auth.IPermissionsManager;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * This handles the CAVE requests to the taf_queue table.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 01, 2012  14715    rferrel   Initial creation
 * May 08, 2013  1814     rjpeter   Added time to live to topic
 * Jun 07, 2013  1981     mpduff    TafQueueRequest is now protected.
 * May 08, 2014  3091     rferrel   Added CHECK_AUTHORIZED.
 * May 28, 2014  3211     njensen   Use IAuthorizer instead of IRoleStorage
 * Nov 06, 2015  5108     rferrel   Get list of TAFs based on state and
 *                                  xmittime.
 * Feb 03, 2017  6065     tgurney   Also generate XML product
 * Mar 22, 2017  6065     tgurney   Set the XML flag on taf queue record
 * Apr 18, 2017  6065     tgurney   Add getLastTafText method
 * Jul 17, 2017  6288     randerso  Changed to use new Roles/Permissions
 *                                  framework
 * Dec 12, 2017  6549     tgurney   Wrap TAF XMLs in a WMO bulletin
 * Dec 13, 2017  6550     tgurney   Fix XML previous report valid period
 * May 15, 2019 20693 mgamazaychikov Removed code associated with creating TAF XML
 * Jan 14, 2022  8736     jsebahar  Added additional logging to verify requests were received
 *
 * </pre>
 *
 * @author rferrel
 */
public class TafQueueRequestHandler
        extends AbstractPrivilegedRequestHandler<TafQueueRequest> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TafQueueRequestHandler.class);

    @SuppressWarnings("unchecked")
    @Override
    public ServerResponse<?> handleRequest(TafQueueRequest request) {
        List<String> idList = null;
        TafQueueState state = request.getState();
        TafQueueDao dao = null;
        ServerResponse<?> response = null;

        try {
            statusHandler.handle(Priority.INFO,
                    "HandleRequest type - " + request.getType());

            dao = new TafQueueDao();
            switch (request.getType()) {
            case CREATE:
                response = new ServerResponse<>();
                for (TafQueueRecord record : request.getRecords()) {
                    dao.create(record);
                    response.addMessage(record.getInfo());
                }
                sendNotification(Type.CREATE);
                break;
            case GET_LIST:
                response = new ServerResponse<>();
                makeList(request.getState(), dao, response);
                break;
            case GET_LOG:
                response = new ServerResponse<>();
                List<Date> dateList = (List<Date>) request.getArgument();
                String log = dao.getLogMessages(dateList);
                ((ServerResponse<String>) response).setPayload(log);
                break;
            case GET_TAFS:
                idList = (List<String>) request.getArgument();
                List<TafQueueRecord> records = null;
                if (idList != null) {
                    response = new ServerResponse<>();
                    records = dao.getRecordsById(idList);
                    makeTafs(records, response);
                } else {
                    response = new ServerResponse<>();
                    records = dao.getRecordsByXmittime(request.getXmitTime(),
                            request.getState());
                    makeList(response, records);
                }
                break;
            case REMOVE_SELECTED:
                response = new ServerResponse<>();
                idList = (List<String>) request.getArgument();
                int numRemoved = dao.removeSelected(idList, state);
                if (idList.size() != numRemoved) {
                    response.setError(true);
                    response.addMessage(idList.size() - numRemoved
                            + " forecast(s) not in "
                            + state.toString().toLowerCase() + " state.");
                } else {
                    response.addMessage(
                            numRemoved + " " + state.toString().toLowerCase()
                                    + " forecast(s) removed.");
                }
                makeList(state, dao, response);
                if (state == TafQueueState.PENDING && numRemoved > 0) {
                    sendNotification(Type.REMOVE_SELECTED);
                }
                break;
            case RETRANSMIT:
                response = new ServerResponse<>();
                idList = (List<String>) request.getArgument();
                int retransNum = dao.retransmit(idList);
                if (retransNum == idList.size()) {
                    response.addMessage(
                            "Forecast(s) queued for immediate transmission.");
                } else {
                    response.setError(true);
                    response.addMessage(
                            "Unable queue all forecast(s) for immediate transmission.");
                }
                makeList(request.getState(), dao, response);

                if (retransNum > 0) {
                    sendNotification(Type.RETRANSMIT);
                }
                break;
            case CHECK_AUTHORIZED:
                response = new ServerResponse<>();
                response.addMessage("User is authorized.");
                break;
            default:
                response = new ServerResponse<>();
                response.addMessage("Unknown type: " + request.getType());
                response.setError(true);
                break;
            }
        } catch (EdexException | SerializationException e) {
            statusHandler.error(e.getMessage(), e);
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
    private void makeList(TafQueueRecord.TafQueueState state, TafQueueDao dao,
            ServerResponse<?> response) throws DataAccessLayerException {
        List<TafQueueRecord> records = dao.getRecordsByState(state);
        makeList(response, records);
    }

    /**
     * Convert list of records into a list of strings in the format
     * "record_id,record_info" and place in response's payload.
     */
    @SuppressWarnings("unchecked")
    private void makeList(ServerResponse<?> response,
            List<TafQueueRecord> records) {
        List<String> recordsList = new ArrayList<>();
        StringBuilder sb = new StringBuilder();
        for (TafQueueRecord record : records) {
            sb.setLength(0);
            sb.append(record.getId()).append(",").append(record.getInfo());
            recordsList.add(sb.toString());
        }
        ((ServerResponse<List<String>>) response).setPayload(recordsList);
    }

    /**
     * Place in the response payload a string displaying all the TAFs in the
     * records.
     *
     * @param records
     */
    @SuppressWarnings("unchecked")
    private void makeTafs(List<TafQueueRecord> records,
            ServerResponse<?> response) {
        StringBuilder sb = new StringBuilder();
        String prefix = "";
        for (TafQueueRecord record : records) {
            sb.append(prefix).append(record.getTafText());
            prefix = "\n\n";
        }
        ((ServerResponse<String>) response).setPayload(sb.toString());
    }

    private void sendNotification(TafQueueRequest.Type type)
            throws SerializationException, EdexException {
        byte[] message = SerializationUtil.transformToThrift(type.toString());
        EDEXUtil.getMessageProducer().sendAsyncUri(
                "jms-generic:topic:tafQueueChanged?timeToLive=60000", message);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AuthorizationResponse authorized(TafQueueRequest request)
            throws AuthorizationException {
        IPermissionsManager manager = AuthManagerFactory.getInstance()
                .getPermissionsManager();

        boolean authorized = manager.isPermitted(request.getRoleId());

        if (authorized) {
            return new AuthorizationResponse(authorized);
        } else {
            return new AuthorizationResponse(request.getNotAuthorizedMessage());
        }
    }
}
