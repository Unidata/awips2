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
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;

import jep.JepException;

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
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKTReader;

/**
 * Handler for
 * <code>MergeActiveTableRequest<code>s. This is the server-side portion of the
 * ingestAT/MergeVTEC applications.
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
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
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
        List<ActiveTableRecord> records = new ArrayList<ActiveTableRecord>(
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
                    Integer incomingEtn = (Integer) template.get("etn");
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
                    atr.setSeg((Integer) template.get("seg"));
                    Calendar start = GregorianCalendar.getInstance();
                    start.setTimeInMillis(((Number) template.get("startTime"))
                            .longValue() * 1000L);
                    atr.setStartTime(start);
                    Calendar end = GregorianCalendar.getInstance();
                    end.setTimeInMillis(((Number) template.get("endTime"))
                            .longValue() * 1000L);
                    atr.setEndTime(end);
                    Calendar purge = GregorianCalendar.getInstance();
                    purge.setTimeInMillis(((Number) template.get("purgeTime"))
                            .longValue() * 1000L);
                    atr.setPurgeTime(purge);
                    Calendar issue = GregorianCalendar.getInstance();
                    issue.setTimeInMillis(((Number) template.get("issueTime"))
                            .longValue() * 1000L);
                    atr.setIssueTime(issue);
                    atr.setUfn((Boolean) template.get("ufn"));
                    atr.setOfficeid(template.get("officeid").toString());
                    atr.setXxxid(template.get("xxxid").toString());
                    atr.setPil(template.get("pil").toString());
                    atr.setProductClass(template.get("productClass").toString());
                    atr.setUgcZone(template.get("id").toString());
                    atr.setRawmessage(template.get("rawMessage").toString());

                    atr.setCountyheader((String) template.get("countyheader"));
                    Integer floodBeginInt = (Integer) template
                            .get("floodBegin");
                    if (floodBeginInt != null) {
                        Calendar floodBegin = GregorianCalendar.getInstance();
                        floodBegin.setTimeInMillis(floodBeginInt * 1000L);
                        atr.setFloodBegin(floodBegin);
                    }
                    Integer floodCrestInt = (Integer) template
                            .get("floodCrest");
                    if (floodCrestInt != null) {
                        Calendar floodCrest = GregorianCalendar.getInstance();
                        floodCrest.setTimeInMillis(floodCrestInt * 1000L);
                        atr.setFloodCrest(floodCrest);
                    }
                    Integer floodEndInt = (Integer) template.get("floodEnd");
                    if (floodEndInt != null) {
                        Calendar floodEnd = GregorianCalendar.getInstance();
                        floodEnd.setTimeInMillis(floodEndInt * 1000L);
                        atr.setFloodEnd(floodEnd);
                    }
                    atr.setFloodRecordStatus((String) template
                            .get("floodRecordStatus"));
                    atr.setFloodSeverity((String) template.get("floodseverity"));
                    atr.setImmediateCause((String) template
                            .get("immediateCause"));
                    atr.setLoc((String) template.get("loc"));
                    atr.setLocationID((String) template.get("locationid"));
                    atr.setMotdir((Integer) template.get("motdir"));
                    atr.setMotspd((Integer) template.get("motspd"));
                    atr.setWmoid((String) template.get("wmoid"));
                    String geomString = (String) template.get("geometry");
                    if (geomString != null) {
                        Geometry geometry = buildGeometry(geomString);
                        atr.setGeometry(geometry);
                    }
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

    /**
     * This method creates a Geometry object for storage in the database which
     * defines the polygon represented by a warning. Adapted from the method of
     * the same name in PythonDecoder.
     * 
     * @param tempPoly
     *            The polygon string from the warning
     * @return The geometry generated from tempPoly, or null if the polygon
     *         could not be built.
     */
    private Geometry buildGeometry(String tempPoly) {
        Geometry geo = null;

        StringBuffer buf = new StringBuffer();
        buf.append("POLYGON((");

        // Parse and scale latitude and longitude and swap order, so
        // longitude is x and latitude is y in the geometry.
        double lat = Double.NaN;
        double lon = Double.NaN;
        boolean latCoord = true;
        String sep = "";
        String firstPt = null;
        String[] coords = tempPoly.split("[\\r\\n ]+");
        try {
            for (String coord : coords) {
                if (latCoord) {
                    lat = Double.parseDouble(coord) / 100.0;
                } else {
                    lon = Double.parseDouble(coord) / -100.0;
                    buf.append(sep).append(lon).append(" ").append(lat);
                    if (firstPt == null) {
                        firstPt = buf.substring(9); // 9=="POLYGON((".length()
                    }
                    sep = ", ";
                }
                latCoord = !latCoord;
            }

            // Make sure the polygon is a closed loop
            if (!buf.substring(buf.length() - firstPt.length()).equals(firstPt)) {
                buf.append(sep).append(firstPt);
            }

            buf.append("))");

            geo = new WKTReader().read(buf.toString());

        } catch (ParseException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (NumberFormatException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        return geo;
    }
}
