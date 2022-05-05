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
package com.raytheon.uf.edex.activetable.handler;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.OperationalActiveTableRecord;
import com.raytheon.uf.common.activetable.PracticeActiveTableRecord;
import com.raytheon.uf.common.activetable.request.MergeActiveTableRequest;
import com.raytheon.uf.common.activetable.response.ActiveTableSharingResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.activetable.ActiveTable;

import jep.JepException;

/**
 * Handler for <code>MergeActiveTableRequest<code>s. This is the server-side
 * portion of the ingestAT/MergeVTEC applications.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2010            wldougher     Initial creation
 * Aug 20, 2012  #1084     dgilling      Properly zero pad incoming
 *                                       ETN values.
 * Feb 26, 2013  #1447     dgilling      Rewrite based on MergeActiveTableRequest
 *                                       and use MergeVTEC.py to perform merge.
 * Jul 23, 2013  #2212     dgilling      Fix ClassCastExceptions on flood
 *                                       fields.
 * Apr 28, 2015  #4027     randerso      Expunged Calendar from ActiveTableRecord
 * Jun 27, 2016  #5707     nabowle       Removed geometry from ActiveTableRecord.
 * Nov 02, 2016  #5979     njensen       Cast to Number where applicable
 * Apr 14, 2017  #5979     njensen       Safer null check after cast
 * 
 * </pre>
 * 
 * @author wldougher
 */

public class MergeActiveTableHandler implements
        IRequestHandler<MergeActiveTableRequest> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MergeActiveTableHandler.class);

    /**
     * Handle a request to update the active table with a list of records. The
     * mode of the update will depend on the mode of the records. Operational
     * and practice records should never be sent together in the same request.
     *
     * @see com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest(com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public ActiveTableSharingResponse handleRequest(
            MergeActiveTableRequest request) throws Exception {
        Map<String, Object>[] activeTableMap = request.getIncomingRecords();
        ActiveTableMode mode = request.getTableName();

        statusHandler.handle(Priority.INFO,
                "Received UpdateActiveTable request containing "
                        + activeTableMap.length + " maps.");

        List<ActiveTableRecord> records;
        try {
            records = mapToRecords(activeTableMap, mode);
        } catch (Exception e) {
            // already logged this below, just returning failed status
            return new ActiveTableSharingResponse(false,
                    "Error executing converting incoming records: "
                            + e.getLocalizedMessage());
        }

        String xmlSource = request.isFromIngestAT() ? request.getXmlSource()
                : null;
        try {
            ActiveTable.mergeRemoteTable(request.getSite(),
                    request.getTableName(), records, request.getTimeOffset(),
                    request.isMakeBackups(), request.isFromIngestAT(),
                    xmlSource);
        } catch (JepException e) {
            // already logged this in ActiveTable, just returning failed status
            return new ActiveTableSharingResponse(false,
                    "Error performing merge: " + e.getLocalizedMessage());
        }

        return new ActiveTableSharingResponse(true, null);
    }

    /**
     * This is a Java version of transformActiveTableToThrift(), from
     * ActiveTableVtec.py. I wrote it so I didn't have to mess with the include
     * path for a PythonScript. We may use a PythonScript later.
     *
     * @param activeTableMap
     * @param mode
     * @return
     * @throws Exception
     */
    protected List<ActiveTableRecord> mapToRecords(
            Map<String, Object>[] activeTableMap, ActiveTableMode mode)
            throws Exception {
        List<ActiveTableRecord> records = new ArrayList<>(
                activeTableMap.length);
        if (activeTableMap != null) {
            for (Map<String, Object> template : activeTableMap) {
                ActiveTableRecord atr = null;
                if (ActiveTableMode.OPERATIONAL.equals(mode)) {
                    atr = new OperationalActiveTableRecord();
                } else {
                    atr = new PracticeActiveTableRecord();
                }

                try {
                    atr.setVtecstr(template.get("vtecstr").toString());
                    Integer incomingEtn = ((Number) template.get("etn"))
                            .intValue();
                    DecimalFormat formatter = new DecimalFormat("0000");
                    String paddedEtn = formatter.format(incomingEtn);
                    atr.setEtn(paddedEtn);
                    atr.setSig(template.get("sig").toString());
                    atr.setPhen(template.get("phen").toString());
                    if (template.containsKey("segText")) {
                        atr.setSegText(template.get("segText").toString());
                    }
                    if (template.containsKey("segText")) {
                        atr.setSegText(template.get("segText").toString());
                    }
                    if (template.containsKey("overviewText")) {
                        atr.setOverviewText(template.get("overviewText")
                                .toString());
                    }
                    atr.setPhensig(template.get("phensig").toString());
                    atr.setAct(template.get("act").toString());
                    atr.setSeg(((Number) template.get("seg")).intValue());
                    Date start = new Date(
                            ((Number) template.get("startTime")).longValue() * 1000L);
                    atr.setStartTime(start);
                    Date end = new Date(
                            ((Number) template.get("endTime")).longValue() * 1000L);
                    atr.setEndTime(end);
                    Date purge = new Date(
                            ((Number) template.get("purgeTime")).longValue() * 1000L);
                    atr.setPurgeTime(purge);
                    Date issue = new Date(
                            ((Number) template.get("issueTime")).longValue() * 1000L);
                    atr.setIssueTime(issue);
                    atr.setUfn((Boolean) template.get("ufn"));
                    atr.setOfficeid(template.get("officeid").toString());
                    atr.setXxxid(template.get("xxxid").toString());
                    atr.setPil(template.get("pil").toString());
                    atr.setProductClass(template.get("productClass").toString());
                    atr.setUgcZone(template.get("id").toString());
                    atr.setRawmessage(template.get("rawMessage").toString());
                    atr.setCountyheader((String) template.get("countyheader"));

                    Number floodBeginTime = (Number) template.get("floodBegin");
                    if (floodBeginTime != null) {
                        Date floodBegin = new Date(
                                floodBeginTime.longValue() * 1000L);
                        atr.setFloodBegin(floodBegin);
                    }
                    Number floodCrestTime = (Number) template.get("floodCrest");
                    if (floodCrestTime != null) {
                        Date floodCrest = new Date(
                                floodCrestTime.longValue() * 1000L);
                        atr.setFloodCrest(floodCrest);
                    }
                    Number floodEndTime = (Number) template.get("floodEnd");
                    if (floodEndTime != null) {
                        Date floodEnd = new Date(
                                floodEndTime.longValue() * 1000L);
                        atr.setFloodEnd(floodEnd);
                    }
                    atr.setFloodRecordStatus((String) template
                            .get("floodRecordStatus"));
                    atr.setFloodSeverity((String) template.get("floodseverity"));

                    atr.setImmediateCause((String) template
                            .get("immediateCause"));
                    atr.setLoc((String) template.get("loc"));
                    atr.setLocationID((String) template.get("locationid"));
                    Number motDir = (Number) template.get("motdir");
                    if (motDir != null) {
                        atr.setMotdir(motDir.intValue());
                    }
                    Number motSpd = (Number) template.get("motspd");
                    if (motSpd != null) {
                        atr.setMotspd(motSpd.intValue());
                    }
                    atr.setWmoid((String) template.get("wmoid"));
                    records.add(atr);
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                    throw e;
                }
            }
        }

        return records;
    }
}
