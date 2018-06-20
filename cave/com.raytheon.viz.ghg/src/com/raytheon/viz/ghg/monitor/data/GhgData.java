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
package com.raytheon.viz.ghg.monitor.data;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.StringTokenizer;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.AlertsEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.SelectionEnum;

/**
 * This class containing the GHG data displayed in the table.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 MAR 2008  N/A        lvenable    Initial creation
 * 30 JUL 2010  6721       mpduff      WFO now from officeid column.
 * 28 APR 2015  4027       randerso    Expunged Calendar from ActiveTableRecord
 * 05 JAN 2016  5184       dgilling    Refactor constructor.
 * 05 FEB 2016  5316       randerso    Removed unnecessary exception handler
 * 02 AUG 2016  5790       dgilling    Use MultiMap to save segment data.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class GhgData implements Comparable<GhgData> {

    /**
     * Minimum number of consecutive zones to cause ">" group notation. This
     * should always be at least 2.
     */
    public static final int MIN_GROUP_ZONES = 2;

    /**
     * A string array containing the string values in each cell.
     */
    private String[] cellDataStrings = null;

    /**
     * Action string.
     */
    private String action;

    /**
     * VTEC string.
     */
    private String vtecString;

    /**
     * ETN string.
     */
    private String etn;

    /**
     * Hazard string.
     */
    private String hazard;

    /**
     * Phen string.
     */
    private String phen;

    /**
     * Sig string.
     */
    private String sig;

    /**
     * Start date.
     */
    private Date startDate;

    /**
     * End date.
     */
    private Date endDate;

    /**
     * Purge date.
     */
    private Date purgeDate;

    /**
     * Issue time.
     */
    private Date issueTime;

    /**
     * Pil string.
     */
    private String pil;

    /**
     * Hazard has no set end time--runs until further notice
     */
    private boolean ufn;

    /**
     * WFO string.
     */
    private String wfo;

    /**
     * Geo ID string.
     */
    private String geoId;

    /**
     * Raw Message.
     */
    private String rawMessage;

    /**
     * Maps segment number to zones. If multiple zones share the same segment
     * text the segment number will be the lowest of the segment numbers.
     */
    private final Multimap<Integer, String> segmentZoneMap = HashMultimap
            .create();

    /**
     * Maps segment numbers to segment text.
     */
    private final Map<Integer, String> segmentNumMap = new HashMap<>();

    /**
     * Flag to determine if this data object is a combined object
     */
    private boolean combined = false;

    /**
     * The original GhgData records combined to make this record. Contains
     */
    private List<GhgData> combinedList = new ArrayList<>();

    private GhgConfigData.AlertsEnum alert;

    private GhgConfigData.SelectionEnum selection;

    private String productClass;

    private String overviewText;

    private boolean alertLevel1 = false;

    private boolean alertLevel2 = false;

    private boolean alertExpired = false;

    /**
     * Formatted UTC.
     */
    SimpleDateFormat dateFormatter = new SimpleDateFormat("HH:mm'Z' dd-MMM-yy");

    /**
     * Constructor. Uses the warning to create the data record.
     * 
     * @param warning
     *            the warning to convert
     * @param hazardDesc
     *            Headline-like description of the warning
     * 
     */
    public GhgData(ActiveTableRecord warning, String hazardDesc) {
        if (warning != null) {
            action = warning.getAct();
            vtecString = warning.getVtecstr();
            etn = warning.getEtn();
            phen = warning.getPhen();
            sig = warning.getSig();
            hazard = hazardDesc;
            startDate = warning.getStartTime();
            endDate = warning.getEndTime();
            purgeDate = warning.getPurgeTime();
            issueTime = warning.getIssueTime();
            pil = warning.getPil();
            wfo = warning.getOfficeid();
            ufn = warning.isUfn();
            geoId = warning.getUgcZone();
            alert = AlertsEnum.NoAlerts;
            selection = SelectionEnum.NoSelection;
            productClass = warning.getProductClass();
            overviewText = warning.getOverviewText();
            // An active table record has only one action, segment, and geoid.
            setSegmentText(geoId, warning.getSeg(), warning.getSegText());

            dateFormatter.setTimeZone(TimeZone.getTimeZone("UTC"));
            getCombinedList().add(this);
        }
    }

    /**
     * Copy constructor.
     * 
     * @param src
     *            The GhgData to copy.
     */
    public GhgData(GhgData src) {
        action = src.action;
        vtecString = src.vtecString;
        etn = src.etn;
        phen = src.phen;
        sig = src.sig;
        hazard = src.hazard;
        startDate = (Date) src.startDate.clone();
        endDate = (Date) src.endDate.clone();
        purgeDate = (Date) src.purgeDate.clone();
        issueTime = (Date) src.issueTime.clone();
        pil = src.pil;
        wfo = src.wfo;
        ufn = src.ufn;
        geoId = src.geoId;
        alert = src.alert;
        selection = src.selection;
        productClass = src.productClass;
        overviewText = src.overviewText;
        rawMessage = src.rawMessage;
        segmentNumMap.putAll(src.segmentNumMap);
        segmentZoneMap.putAll(src.segmentZoneMap);
        dateFormatter.setTimeZone(src.dateFormatter.getTimeZone());
        combined = src.combined;
        getCombinedList().addAll(src.getCombinedList());
        alert = src.alert;
        alertLevel1 = src.alertLevel1;
        alertLevel2 = src.alertLevel2;
        alertExpired = src.alertExpired;
    }

    /**
     * Parses the county header, converting the string to a comma separated list
     * of counties.
     * 
     * @param header
     *            the header to parse
     * 
     * @return the converted list
     */
    public static String parseCountyHeader(String header) {
        Pattern p = Pattern.compile("^([A-Z]{3})(.+)$");
        StringBuffer sb = new StringBuffer();
        if (header != null) {

            StringTokenizer tok = new StringTokenizer(header, "-");
            String geo = "";
            boolean first = true;
            while (tok.hasMoreTokens()) {
                sb.append(first ? "" : ",");
                String token = tok.nextToken().toUpperCase();
                Matcher m = p.matcher(token);
                if (m.matches()) {
                    geo = m.group(1);
                    sb.append(geo).append(m.group(2));
                } else {
                    sb.append(geo).append(token);
                }
                first = false;
            }
        }
        return sb.toString();
    }

    /**
     * Get a string array of the data.
     * 
     * @return String array of data.
     */
    public String[] getDataCellNames() {
        if (cellDataStrings == null) {
            cellDataStrings = new String[GhgConfigData.DataEnum.values().length];

            cellDataStrings[GhgConfigData.DataEnum.ACTION.ordinal()] = action;
            cellDataStrings[GhgConfigData.DataEnum.VTEC_STRING.ordinal()] = vtecString;
            cellDataStrings[GhgConfigData.DataEnum.ETN.ordinal()] = etn;
            cellDataStrings[GhgConfigData.DataEnum.PHEN_SIG.ordinal()] = getPhenSig();
            cellDataStrings[GhgConfigData.DataEnum.HAZARD.ordinal()] = hazard;
            cellDataStrings[GhgConfigData.DataEnum.PHEN.ordinal()] = phen;
            cellDataStrings[GhgConfigData.DataEnum.SIG.ordinal()] = sig;
            cellDataStrings[GhgConfigData.DataEnum.START.ordinal()] = dateFormatter
                    .format(startDate);
            cellDataStrings[GhgConfigData.DataEnum.END.ordinal()] = (ufn ? "UntilFurtherNotice"
                    : dateFormatter.format(endDate));
            cellDataStrings[GhgConfigData.DataEnum.PURGE.ordinal()] = dateFormatter
                    .format(purgeDate);
            cellDataStrings[GhgConfigData.DataEnum.ISSUE_TIME.ordinal()] = dateFormatter
                    .format(issueTime);
            cellDataStrings[GhgConfigData.DataEnum.PIL.ordinal()] = pil;
            cellDataStrings[GhgConfigData.DataEnum.SEG.ordinal()] = getSegNumString();
            cellDataStrings[GhgConfigData.DataEnum.WFO.ordinal()] = wfo;
            cellDataStrings[GhgConfigData.DataEnum.GEO_ID.ordinal()] = geoId;
            cellDataStrings[GhgConfigData.DataEnum.PRODUCT_CLASS.ordinal()] = productClass;
        }

        return cellDataStrings;
    }

    /**
     * Check to see if a specific alert type is set for this data.
     * 
     * @param alert
     *            the alert type to check
     * 
     * @return true is the alert is already set
     */
    public boolean isAlert(GhgConfigData.AlertsEnum alert) {
        return this.alert.equals(alert);
    }

    /**
     * Creates the alert text representing this alert.
     * 
     * @param time
     *            The time the alert is issued
     * @param type
     *            the type of alert
     * 
     * @return the formatted alert string
     */
    public String getAlertString(GhgConfigData.AlertsEnum type) {
        Date date = SimulatedTime.getSystemTime().getTime();
        SimpleDateFormat sdf = new SimpleDateFormat("yy/MM/dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        final String ONE_SPC = " ";
        StringBuffer sb = new StringBuffer();
        sb.append(sdf.format(date)).append(ONE_SPC);
        sb.append(type.display).append(ONE_SPC);
        sb.append(pil);
        sb.append(". ProductPurge Time of ").append(
                dateFormatter.format(purgeDate));
        sb.append(". Event=").append(getPhenSig());
        sb.append(ONE_SPC).append(hazard);
        return sb.toString();
    }

    /**
     * Expands out a string of zones like "NEZ103>105,NEZ212>214" to a string of
     * individual zone names, i.e., "NEZ103,NEZ104,NEZ105,NEZ212,NEZ213,NEZ214".
     * 
     * @param zoneString
     *            The input string, possibly with '>' entries
     * @return The output string; all '>' entries have been expanded.
     * @see #compressMultiZones(String)
     */
    public static String expandMultiZones(String zoneString) {
        StringBuilder sb = new StringBuilder(zoneString.length());
        String[] zones = zoneString.split(",");
        for (String zone : zones) {
            String[] zn = zone.split(">");
            if (zn.length == 2) {
                String stz = zn[0].substring(0, 3);
                int start = Integer.valueOf(zn[0].substring(3));
                int end = Integer.valueOf(zn[1]);
                for (int val = start; val <= end; val++) {
                    sb.append(String.format(",%s%03d", stz, val));
                }
            } else {
                sb.append(",").append(zn[0]);
            }
        }
        if (sb.length() > 0) {
            sb.deleteCharAt(0);
        }
        return sb.toString();
    }

    /**
     * Note: Key fields, alert level, and selection are not compared!
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(GhgData o) {
        int rtn = 0;
        do {
            rtn = etn.compareTo(o.etn);
            if (rtn != 0) {
                break;
            }
            rtn = (phen + "." + sig).compareTo(o.getPhenSig());
            if (rtn != 0) {
                break;
            }
            rtn = wfo.compareTo(o.wfo);
            if (rtn != 0) {
                break;
            }
            rtn = pil.compareTo(o.pil);
            if (rtn != 0) {
                break;
            }
            rtn = issueTime.compareTo(o.issueTime);
            if (rtn != 0) {
                break;
            }
            rtn = startDate.compareTo(o.startDate);
            if (rtn != 0) {
                break;
            }

            rtn = endDate.compareTo(o.endDate);
            if (rtn != 0) {
                break;
            }

            rtn = action.compareTo(o.action);
            if (rtn != 0) {
                break;
            }

            rtn = geoId.compareTo(o.geoId);
            if (rtn != 0) {
                break;
            }

            rtn = purgeDate.compareTo(o.purgeDate);
            if (rtn != 0) {
                break;
            }

            rtn = productClass.compareTo(o.productClass);
            if (rtn != 0) {
                break;
            }

            // don't use selection or alert in comparison
            /*
             * rtn = selection.compareTo(o.selection); if (rtn != 0) { break; }
             * 
             * rtn = alert.compareTo(o.alert); if (rtn != 0) { break; }
             */

        } while (false);

        return rtn;
    }

    @Override
    public boolean equals(Object o) {
        if (o == null) {
            return false;
        }
        if (!GhgData.class.equals(o.getClass())) {
            return false;
        }

        return (compareTo((GhgData) o) == 0);
    }

    /**
     * @param alert
     *            the alert to set
     */
    public void setAlert(GhgConfigData.AlertsEnum alert) {
        this.alert = alert;
    }

    /**
     * @return the alert
     */
    public GhgConfigData.AlertsEnum getAlert() {
        return alert;
    }

    /**
     * @param selection
     *            the selection to set
     */
    public void setSelection(GhgConfigData.SelectionEnum selection) {
        this.selection = selection;
    }

    /**
     * @return the selection
     */
    public GhgConfigData.SelectionEnum getSelection() {
        return selection;
    }

    /**
     * @param productClass
     *            the productClass to set
     */
    public void setProductClass(String productClass) {
        this.productClass = productClass;
    }

    /**
     * @return the productClass
     */
    public String getProductClass() {
        return productClass;
    }

    /**
     * Get the end date of the record.
     * 
     * @return the endDate
     */
    public Date getEndDate() {
        return endDate;
    }

    /**
     * @return the action
     */
    public String getAction() {
        return action;
    }

    /**
     * @param action
     *            the action to set
     */
    public void setAction(String action) {
        this.action = action;
    }

    /**
     * @return the etn
     */
    public String getEtn() {
        return etn;
    }

    /**
     * @param etn
     *            the etn to set
     */
    public void setEtn(String etn) {
        this.etn = etn;
    }

    /**
     * @return the phenSig
     */
    public String getPhenSig() {
        return phen + "." + sig;
    }

    /**
     * @return the hazard
     */
    public String getHazard() {
        return hazard;
    }

    /**
     * @param hazard
     *            the hazard to set
     */
    public void setHazard(String hazard) {
        this.hazard = hazard;
    }

    /**
     * @return the phen
     */
    public String getPhen() {
        return phen;
    }

    /**
     * @param phen
     *            the phen to set
     */
    public void setPhen(String phen) {
        this.phen = phen;
    }

    /**
     * @return the sig
     */
    public String getSig() {
        return sig;
    }

    /**
     * @param sig
     *            the sig to set
     */
    public void setSig(String sig) {
        this.sig = sig;
    }

    /**
     * @return the startDate
     */
    public Date getStartDate() {
        return startDate;
    }

    /**
     * @param startDate
     *            the startDate to set
     */
    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    /**
     * @return the purgeDate
     */
    public Date getPurgeDate() {
        return purgeDate;
    }

    /**
     * @param purgeDate
     *            the purgeDate to set
     */
    public void setPurgeDate(Date purgeDate) {
        this.purgeDate = purgeDate;
    }

    /**
     * @return the issueTime
     */
    public Date getIssueTime() {
        return issueTime;
    }

    /**
     * @param issueTime
     *            the issueTime to set
     */
    public void setIssueTime(Date issueTime) {
        this.issueTime = issueTime;
    }

    /**
     * @return the pil
     */
    public String getPil() {
        return pil;
    }

    /**
     * @param pil
     *            the pil to set
     */
    public void setPil(String pil) {
        this.pil = pil;
    }

    /**
     * @return the segNum
     */
    public List<Integer> getSegNum() {
        List<Integer> segmentNums = new ArrayList<>(segmentNumMap.keySet());
        Collections.sort(segmentNums);
        return segmentNums;
    }

    public String getSegNumString() {
        return StringUtils.join(getSegNum(), ',');
    }

    /**
     * @return the wfo
     */
    public String getWfo() {
        return wfo;
    }

    /**
     * @param wfo
     *            the wfo to set
     */
    public void setWfo(String wfo) {
        this.wfo = wfo;
    }

    /**
     * @return the geoId
     */
    public String getGeoId() {
        return geoId;
    }

    /**
     * @param geoId
     *            the geoId to set
     */
    public void setGeoId(String geoId) {
        this.geoId = geoId;
    }

    /**
     * @param endDate
     *            the endDate to set
     */
    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    /**
     * @return the vtecString
     */
    public String getVtecString() {
        return vtecString;
    }

    /**
     * @param vtecString
     *            the vtecString to set
     */
    public void setVtecString(String vtecString) {
        this.vtecString = vtecString;
    }

    /**
     * @return the combined
     */
    public boolean isCombined() {
        return combined;
    }

    /**
     * @param combined
     *            the combined to set
     */
    public void setCombined(boolean combined) {
        this.combined = combined;
    }

    /**
     * @return the rawMessage
     */
    public String getRawMessage() {
        return rawMessage;
    }

    /**
     * @param rawMessage
     *            the rawMessage to set
     */
    public void setRawMessage(String rawMessage) {
        this.rawMessage = rawMessage;
    }

    /**
     * @param geoId
     *            the GeoId for the segment text
     * @return the segmentText
     */
    public String getSegmentText(String geoId) {
        for (Entry<Integer, String> entry : segmentZoneMap.entries()) {
            if (geoId.equals(entry.getValue())) {
                return segmentNumMap.get(entry.getKey());
            }
        }

        return null;
    }

    /**
     * Adds the given segment to the segment data for this record.
     * 
     * @param geoId
     *            the zone id for the segment text
     * @param segmentNumber
     *            the number of the segment
     * @param segmentText
     *            the text of the segment
     */
    public void setSegmentText(String geoId, int segmentNumber,
            String segmentText) {
        /*
         * Need to normalize the segment number key for the segmentNumMap so
         * that it uses the lowest value for all the zones with the same text.
         * 
         * If this new segment number is lower than the existing we have to
         * update all the other entries in the MultiMap with the new value.
         */
        int newCommonSegNum = segmentNumber;
        int prevCommonSegNum = Integer.MAX_VALUE;
        for (Entry<Integer, String> entry : segmentNumMap.entrySet()) {
            if (segmentText.equals(entry.getValue())) {
                newCommonSegNum = Math.min(newCommonSegNum, entry.getKey());
                prevCommonSegNum = Math.min(prevCommonSegNum, entry.getKey());
            }
        }
        Collection<String> zonesToUpdate = segmentZoneMap.removeAll(Integer
                .valueOf(prevCommonSegNum));
        segmentZoneMap.putAll(Integer.valueOf(newCommonSegNum), zonesToUpdate);

        segmentZoneMap.put(Integer.valueOf(newCommonSegNum), geoId);
        segmentNumMap.put(Integer.valueOf(segmentNumber), segmentText);
    }

    /**
     * Returns a Map representing all the segments contained in this record.
     * Segments will be in order from the original product. Map key is the list
     * of zones in the segment and Map value is the segment text.
     * 
     * @return Map representing all segments in the record.
     */
    public Map<Collection<String>, String> getSegmentTextMap() {
        Map<Collection<String>, String> segmentTextMap = new LinkedHashMap<>();
        for (Integer segNum : segmentZoneMap.keySet()) {
            Collection<String> zones = segmentZoneMap.get(segNum);
            String segmentText = segmentNumMap.get(segNum);
            segmentTextMap.put(zones, segmentText);
        }
        return segmentTextMap;
    }

    @Override
    public String toString() {
        final String NL = "\n";
        StringBuilder sb = new StringBuilder();
        sb.append("[ " + action + NL);
        sb.append(etn + NL);
        sb.append(geoId + NL);
        sb.append(hazard + NL);
        sb.append(phen + NL);
        sb.append(pil + NL);
        sb.append(productClass + NL);
        sb.append(getSegNumString() + NL);
        sb.append(getPhenSig() + NL);
        sb.append(pil + NL);
        sb.append(vtecString + NL);
        sb.append(startDate + " ");
        sb.append(endDate.toString() + NL);
        sb.append(wfo + "]");

        return sb.toString();
    }

    /**
     * @return the overviewText
     */
    public String getOverviewText() {
        return overviewText;
    }

    /**
     * @param overviewText
     *            the overviewText to set
     */
    public void setOverviewText(String overviewText) {
        this.overviewText = overviewText;
    }

    /**
     * @return the alertLevel1
     */
    public boolean isAlertLevel1() {
        return alertLevel1;
    }

    /**
     * @param alertLevel1
     *            the alertLevel1 to set
     */
    public void setAlertLevel1(boolean alertLevel1) {
        this.alertLevel1 = alertLevel1;
    }

    /**
     * @return the alertLevel2
     */
    public boolean isAlertLevel2() {
        return alertLevel2;
    }

    /**
     * @param alertLevel2
     *            the alertLevel2 to set
     */
    public void setAlertLevel2(boolean alertLevel2) {
        this.alertLevel2 = alertLevel2;
    }

    /**
     * @return the alertExpired
     */
    public boolean isAlertExpired() {
        return alertExpired;
    }

    /**
     * @param alertExpired
     *            the alertExpired to set
     */
    public void setAlertExpired(boolean alertExpired) {
        this.alertExpired = alertExpired;
    }

    @Override
    public int hashCode() {
        int hash = 1;
        hash = (hash * 31) + hazard.hashCode();
        return hash;
    }

    /**
     * Set the list of GhgData records combined to create this GhgData record.
     * Normally, this method will not be used; the list is obtained through
     * getCombinedList() and then normal List methods are used to modify it.
     * 
     * @param combinedList
     *            the combinedList to set
     */
    public void setCombinedList(List<GhgData> combinedList) {
        this.combinedList = combinedList;
    }

    /**
     * Get the list of the original GhgData records combined to create this
     * GhgData record. This is a reference to the internal list, not a copy.
     * 
     * @return the combinedList
     */
    public List<GhgData> getCombinedList() {
        return combinedList;
    }
}