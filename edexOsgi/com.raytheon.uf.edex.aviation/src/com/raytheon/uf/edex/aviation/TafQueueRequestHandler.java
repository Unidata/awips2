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

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.dataplugin.taf.TafRecord;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
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
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.common.wmo.WMOXMLProductCreator;
import com.raytheon.uf.edex.auth.AuthManagerFactory;
import com.raytheon.uf.edex.auth.IPermissionsManager;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.plugin.text.AfosToAwipsLookup;

import jep.JepConfig;
import jep.JepException;

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
            dao = new TafQueueDao();
            switch (request.getType()) {
            case CREATE:
                response = new ServerResponse<>();
                for (TafQueueRecord record : request.getRecords()) {
                    dao.create(record);
                    response.addMessage(record.getInfo());
                    TafQueueRecord xmlRecord = makeXmlRecord(record);
                    if (xmlRecord != null) {
                        dao.create(xmlRecord);
                    }
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
     * @return XML version of taf record. Return null if failed to convert to
     *         XML or to look up the correct WMO header for the XML product
     */
    private static TafQueueRecord makeXmlRecord(TafQueueRecord record) {
        if (record.isXml()) {
            return record;
        }
        TafQueueRecord rval = null;
        try {
            TafQueueRecord xmlRecord = new TafQueueRecord(record);
            xmlRecord.setXml(true);
            String awipsWanPil = xmlRecord.getAwipsWanPil();
            String cccc = awipsWanPil.substring(0, 4);
            String nnn = awipsWanPil.substring(4, 7);
            String xxx = awipsWanPil.substring(7);
            if (awipsWanPil.length() >= 10) {
                xxx = awipsWanPil.substring(7, 10);
            }
            List<AfosToAwips> list = AfosToAwipsLookup
                    .lookupAfosId(cccc, nnn, xxx).getIdList();
            if (list.size() == 1) {
                xmlRecord.setWmoId(list.get(0).getWmottaaii());
                String xmlTafText = textToXML(record.getTafText());
                xmlRecord.setTafText(WMOXMLProductCreator.createBulletin(
                        xmlRecord.getWmoId(), cccc, xmlRecord.getXmitTime(),
                        new String[] { xmlTafText }));
                rval = xmlRecord;
            } else {
                statusHandler
                        .warn("Failed to look up WMO header for TAF XML product "
                                + "with cccc " + cccc + " nnn " + nnn + " xxx "
                                + xxx);
            }
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to convert TAC to XML format", e);
        }
        return rval;
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
     * Convert TAF in plain text format to IWXXM 2.0 XML format
     *
     * @param tafText
     *            The plain text TAF
     * @return TAF as XML string
     */
    public static String textToXML(String tafText) throws JepException {
        // Set up the TAC-to-XML script and associated data files
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext baseCtx = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File scriptFile = pm.getFile(baseCtx,
                LocalizationUtil.join("aviation", "TAF.py"));
        File scriptDir = pm.getFile(baseCtx, "aviation");
        File wwCodesFile = pm.getFile(baseCtx,
                LocalizationUtil.join("aviation", "data", "ww.xml"));
        File tafStationInfoFile = pm.getFile(baseCtx, LocalizationUtil
                .join("aviation", "data", "metarStationInfo.txt"));
        String pythonDir = pm.getFile(baseCtx, "python").getPath();
        String includePath = PyUtil.buildJepIncludePath(pythonDir,
                scriptDir.getPath());
        JepConfig config = new JepConfig().setIncludePath(includePath)
                .setClassLoader(TafQueueManager.class.getClassLoader());

        // Run it
        try (PythonScript python = new PythonScript(config,
                scriptFile.getPath())) {
            Map<String, Object> instArgs = new HashMap<>();
            instArgs.put("wwCodesFile", wwCodesFile.getAbsolutePath());
            instArgs.put("tafStationInfoFile",
                    tafStationInfoFile.getAbsolutePath());
            python.instantiatePythonClass("encoder", "TACtoXML", instArgs);
            Map<String, Object> encoderArgs = new HashMap<>();
            encoderArgs.put("tac", tafText);
            if (tafText.contains("TAF AMD") || tafText.contains("TAF COR")) {
                String fourLetterId = tafText.substring(
                        tafText.indexOf('\n') + 1,
                        tafText.indexOf(' ', tafText.indexOf('\n')));
                Pair<Date, Date> validPeriod = getLastTafValidPeriod(
                        fourLetterId);
                if (validPeriod != null) {
                    encoderArgs.put("lastTafRangeStart",
                            validPeriod.getFirst().getTime()
                                    / TimeUtil.MILLIS_PER_SECOND);
                    encoderArgs.put("lastTafRangeEnd",
                            validPeriod.getSecond().getTime()
                                    / TimeUtil.MILLIS_PER_SECOND);
                }
            }
            String xml = (String) python.execute("__call__", "encoder",
                    encoderArgs);

            return xml;
        }
    }

    /**
     * @param stationId
     * @return Valid period of last TAF issued for the specified station. null
     *         if it is not available
     */
    private static Pair<Date, Date> getLastTafValidPeriod(String stationId) {
        Pair<Date, Date> rval = null;
        CoreDao dao = new CoreDao(DaoConfig.forClass(TafRecord.class));
        Map<String, Object> paramMap = new HashMap<>(1, 1);
        paramMap.put("stationId", stationId);
        QueryResult qresult = dao.executeHQLQuery(
                "select dataTime.validPeriod.start, dataTime.validPeriod.end from TafRecord t1 where t1.issue_time = ("
                        + " select max(issue_time) from TafRecord"
                        + " where stationId = :stationId group by stationId)"
                        + " and t1.stationId = :stationId",
                paramMap);
        QueryResultRow[] rows = qresult.getRows();
        if (rows != null && rows.length > 0) {
            Object rangestart = rows[0].getColumn(0);
            Object rangeend = rows[0].getColumn(1);
            if (rangestart != null && rangeend != null) {
                rval = new Pair<>((Date) rangestart, (Date) rangeend);
            }
        }
        return rval;
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
