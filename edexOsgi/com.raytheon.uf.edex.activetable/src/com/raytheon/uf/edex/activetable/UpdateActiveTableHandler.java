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

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.OperationalActiveTableRecord;
import com.raytheon.uf.common.activetable.PracticeActiveTableRecord;
import com.raytheon.uf.common.activetable.UpdateActiveTableRequest;
import com.raytheon.uf.common.activetable.UpdateActiveTableResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKTReader;

/**
 * Handler for UpdateActiveTableRequests. This is the server-side portion of the
 * ingestAT application.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2010            wldougher     Initial creation
 * Aug 20, 2012  #1084     dgilling      Properly zero pad incoming
 *                                       ETN values.
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class UpdateActiveTableHandler implements
        IRequestHandler<UpdateActiveTableRequest> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(UpdateActiveTableHandler.class);

    /**
     * Handle a request to update the active table with a list of records. The
     * mode of the update will depend on the mode of the records. Operational
     * and practice records should never be sent together in the same request.
     * 
     * @see com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest(com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Object handleRequest(UpdateActiveTableRequest request)
            throws Exception {
        StringBuilder sb = new StringBuilder();
        Float timeOffset = request.getTimeOffset();

        Map<String, Object>[] activeTableMap = request.getActiveTable();
        ActiveTableMode mode = request.getMode();

        statusHandler.handle(Priority.INFO,
                "Received UpdateActiveTable request containing "
                        + activeTableMap.length + " maps.");

        List<ActiveTableRecord> records = null;
        try {
            records = mapToRecords(activeTableMap, mode);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "The mapToRecords() call failed.", e);
            sb.append(e.getLocalizedMessage());
            for (StackTraceElement ste : e.getStackTrace()) {
                sb.append(ste.toString() + "\n");
            }
        }

        if (sb.length() == 0) {

            // Records may have entries for more than one site ID (officeid).
            // However, the merge() method of ActiveTable assumes it will only
            // get records for a single site. So some sorting and slicing of the
            // list has to be done.
            class OidComparator implements Comparator<ActiveTableRecord> {
                /**
                 * Comparator for sorting ActiveTableRecords by office id.
                 * 
                 * @see java.util.Comparator#compare(java.lang.Object,
                 *      java.lang.Object)
                 */
                @Override
                public int compare(ActiveTableRecord o1, ActiveTableRecord o2) {
                    return o1.getOfficeid().compareTo(o2.getOfficeid());
                }
            }
            OidComparator comparator = new OidComparator();
            Collections.sort(records, comparator);

            // Create an active table instance to perform the merge
            ActiveTable table = new ActiveTable();
            // A sublist of records, all for the same site
            List<ActiveTableRecord> siteRecords = null;
            // A dummy record for use with binarySearch
            ActiveTableRecord key = new PracticeActiveTableRecord();
            // The index of the first record with a different site ID
            int nextIdx = -1;

            while (records.size() > 0) {
                // Get a sublist with identical site IDs
                key.setOfficeid(records.get(0).getOfficeid() + " ");
                nextIdx = Collections.binarySearch(records, key, comparator);
                if (nextIdx < 0) {
                    nextIdx = -(nextIdx + 1);
                }
                siteRecords = records.subList(0, nextIdx);
                // Merge all the records for the site
                Exception exc = table.merge(siteRecords, timeOffset);
                if (exc != null) {
                    statusHandler.handle(Priority.PROBLEM,
                            "table.merge() returned an error.", exc);
                    sb.append(exc.getLocalizedMessage());
                    for (StackTraceElement ste : exc.getStackTrace()) {
                        sb.append(ste.toString() + "\n");
                    }
                    break;
                }
                // Remove the processed records
                records = records.subList(nextIdx, records.size());
            }

        }

        String xmlSource = request.getXmlSource();
        List<String> sourceInfo = null;
        if (xmlSource != null && !("".equals(xmlSource.trim()))) {
            ServerInfoExtractor infoExtractor = new ServerInfoExtractor();
            sourceInfo = infoExtractor.extract(xmlSource);
        }

        // Tell the user we finished processing.
        // The merge() method handles its own error reporting, we get no status
        UpdateActiveTableResponse response = new UpdateActiveTableResponse();
        response.setSourceInfo(sourceInfo);
        if (sb.length() > 0) {
            response.setMessage(sb.toString());
        }
        return response;
    }

    /**
     * This is a Java version of transformActiveTableToThrift(), from
     * ActiveTableVtec.py. I wrote it so I didn't have to mess with the include
     * path for a PythonScript. We may use a PythonScript later.
     * 
     * @param activeTableMap
     * @param mode
     * @return
     */
    protected List<ActiveTableRecord> mapToRecords(
            Map<String, Object>[] activeTableMap, ActiveTableMode mode) {
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
                    start.setTimeInMillis(((Integer) template.get("startTime")) * 1000L);
                    atr.setStartTime(start);
                    Calendar end = GregorianCalendar.getInstance();
                    end.setTimeInMillis(((Integer) template.get("endTime")) * 1000L);
                    atr.setEndTime(end);
                    Calendar purge = GregorianCalendar.getInstance();
                    purge.setTimeInMillis(((Integer) template.get("purgeTime")) * 1000L);
                    atr.setPurgeTime(purge);
                    Calendar issue = GregorianCalendar.getInstance();
                    issue.setTimeInMillis(((Integer) template.get("issueTime")) * 1000L);
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
