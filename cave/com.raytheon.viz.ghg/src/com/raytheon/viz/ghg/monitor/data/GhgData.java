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

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.AlertsEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.DataEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.SelectionEnum;

/**
 * This class containing the GHG data displayed in the table.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 25, 2008  N/A      lvenable  Initial creation
 * Jul 30, 2010  6721     mpduff    WFO now from officeid column.
 * Apr 28, 2015  4027     randerso  Expunged Calendar from ActiveTableRecord
 * Jan 05, 2016  5184     dgilling  Refactor constructor.
 * Feb 05, 2016  5316     randerso  Removed unnecessary exception handler
 * Aug 02, 2016  5790     dgilling  Use MultiMap to save segment data.
 * Nov 14, 2019  7919     randerso  Redesigned object to simplify code in
 *                                  GhgTableComp.
 * Oct 28, 2020  8268     randerso  Created UFN_STRING as public constant
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class GhgData {

    public static final String UFN_STRING = "UntilFurtherNotice";

    /**
     * Minimum number of consecutive zones to cause ">" group notation. This
     * should always be at least 2.
     */
    public static final int MIN_GROUP_ZONES = 2;

    private DateTimeFormatter dateFormatter = DateTimeFormatter
            .ofPattern("HH:mm'Z' dd-MMM-yy").withZone(ZoneOffset.UTC);

    private Map<DataEnum, Object> attributes;

    /**
     * Hazard has no set end time--runs until further notice
     */
    private boolean ufn;

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

    private AlertsEnum alert;

    private SelectionEnum selection;

    private String productClass;

    private String overviewText;

    private boolean alertLevel1 = false;

    private boolean alertLevel2 = false;

    private boolean alertExpired = false;

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
        attributes = new HashMap<>();
        if (warning != null) {
            attributes.put(DataEnum.ACTION, warning.getAct());
            attributes.put(DataEnum.VTEC_STRING, warning.getVtecstr());
            attributes.put(DataEnum.ETN, warning.getEtn());
            attributes.put(DataEnum.PHEN_SIG, warning.getPhensig());
            attributes.put(DataEnum.HAZARD, hazardDesc);
            attributes.put(DataEnum.PHEN, warning.getPhen());
            attributes.put(DataEnum.SIG, warning.getSig());
            attributes.put(DataEnum.START, warning.getStartTime());
            attributes.put(DataEnum.END, warning.getEndTime());
            attributes.put(DataEnum.PURGE, warning.getPurgeTime());
            attributes.put(DataEnum.ISSUE_TIME, warning.getIssueTime());
            attributes.put(DataEnum.PIL, warning.getPil());
            attributes.put(DataEnum.WFO, warning.getOfficeid());
            attributes.put(DataEnum.GEO_ID, warning.getUgcZone());
            attributes.put(DataEnum.PRODUCT_CLASS, warning.getProductClass());
            ufn = warning.isUfn();
            alert = AlertsEnum.NoAlerts;
            selection = SelectionEnum.NoSelection;
            overviewText = warning.getOverviewText();
            // An active table record has only one action, segment, and geoid.
            setSegmentText(warning.getUgcZone(), warning.getSeg(),
                    warning.getSegText());

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
        attributes = new HashMap<>(src.attributes);
        ufn = src.ufn;
        alert = src.alert;
        selection = src.selection;
        productClass = src.productClass;
        overviewText = src.overviewText;
        rawMessage = src.rawMessage;
        segmentNumMap.putAll(src.segmentNumMap);
        segmentZoneMap.putAll(src.segmentZoneMap);
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
        StringBuilder sb = new StringBuilder();
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
     * @param field
     * @return text to be displayed for the specified field
     */
    public String getDisplayString(DataEnum field) {
        if (field == DataEnum.SEG) {
            return getSegNumString();
        }

        Object value = attributes.get(field);
        if (field == DataEnum.END && isUfn()) {
            return UFN_STRING;
        }

        if (value instanceof Date) {
            return ZonedDateTime
                    .ofInstant(((Date) value).toInstant(), ZoneOffset.UTC)
                    .format(dateFormatter);
        }

        return value.toString();
    }

    /**
     * Check to see if a specific alert type is set for this data.
     *
     * @param alert
     *            the alert type to check
     *
     * @return true is the alert is already set
     */
    public boolean isAlert(AlertsEnum alert) {
        return this.alert.equals(alert);
    }

    /**
     * @param alert
     *            the alert to set
     */
    public void setAlert(AlertsEnum alert) {
        this.alert = alert;
    }

    /**
     * @return the alert
     */
    public AlertsEnum getAlert() {
        return alert;
    }

    /**
     * @param selection
     *            the selection to set
     */
    public void setSelection(SelectionEnum selection) {
        this.selection = selection;
        for (GhgData data : combinedList) {
            data.selection = selection;
        }
    }

    /**
     * @return the selection
     */
    public SelectionEnum getSelection() {
        return selection;
    }

    /**
     * @param field
     * @return the value of the specified field
     */
    public Object getAttribute(DataEnum field) {
        return attributes.get(field);
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
        return (Date) attributes.get(DataEnum.END);
    }

    /**
     * @return the action
     */
    public String getAction() {
        return (String) attributes.get(DataEnum.ACTION);
    }

    /**
     * @param action
     *            the action to set
     */
    public void setAction(String action) {
        attributes.put(DataEnum.ACTION, action);
    }

    /**
     * @return the etn
     */
    public String getEtn() {
        return (String) attributes.get(DataEnum.ETN);
    }

    /**
     * @param etn
     *            the etn to set
     */
    public void setEtn(String etn) {
        attributes.put(DataEnum.ETN, etn);
    }

    /**
     * @return the phenSig
     */
    public String getPhenSig() {
        return (String) attributes.get(DataEnum.PHEN_SIG);
    }

    /**
     * @return the hazard
     */
    public String getHazard() {
        return (String) attributes.get(DataEnum.HAZARD);
    }

    /**
     * @param hazard
     *            the hazard to set
     */
    public void setHazard(String hazard) {
        attributes.put(DataEnum.HAZARD, hazard);
    }

    /**
     * @return the phen
     */
    public String getPhen() {
        return (String) attributes.get(DataEnum.PHEN);
    }

    /**
     * @return the sig
     */
    public String getSig() {
        return (String) attributes.get(DataEnum.SIG);
    }

    /**
     * @return the startDate
     */
    public Date getStartDate() {
        return (Date) attributes.get(DataEnum.START);
    }

    /**
     * @return the purgeDate
     */
    public Date getPurgeDate() {
        return (Date) attributes.get(DataEnum.PURGE);
    }

    /**
     * @param purgeDate
     *            the purgeDate to set
     */
    public void setPurgeDate(Date purgeDate) {
        attributes.put(DataEnum.PURGE, purgeDate);
    }

    /**
     * @return the issueTime
     */
    public Date getIssueTime() {
        return (Date) attributes.get(DataEnum.ISSUE_TIME);
    }

    /**
     * @return the pil
     */
    public String getPil() {
        return (String) attributes.get(DataEnum.PIL);
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
        return (String) attributes.get(DataEnum.WFO);
    }

    /**
     * @return the geoId
     */
    public String getGeoId() {
        return (String) attributes.get(DataEnum.GEO_ID);
    }

    /**
     * @param geoId
     *            the geoId to set
     */
    public void setGeoId(String geoId) {
        attributes.put(DataEnum.GEO_ID, geoId);
    }

    /**
     * @return true if until further notice
     */
    public boolean isUfn() {
        return ufn;
    }

    /**
     * @return the vtecString
     */
    public String getVtecString() {
        return (String) attributes.get(DataEnum.VTEC_STRING);
    }

    /**
     * @param vtecString
     *            the vtecString to set
     */
    public void setVtecString(String vtecString) {
        attributes.put(DataEnum.VTEC_STRING, vtecString);
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
        Collection<String> zonesToUpdate = segmentZoneMap
                .removeAll(Integer.valueOf(prevCommonSegNum));
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
        StringBuilder sb = new StringBuilder("[");
        sb.append(getSelection()).append('\n');
        sb.append(getAction()).append('\n');
        sb.append(getEtn()).append('\n');
        sb.append(getGeoId()).append('\n');
        sb.append(getHazard()).append('\n');
        sb.append(getPil()).append('\n');
        sb.append(productClass).append('\n');
        sb.append(getSegNumString()).append('\n');
        sb.append(getPhenSig()).append('\n');
        sb.append(getVtecString()).append('\n');
        sb.append(getStartDate()).append(' ');
        sb.append(getEndDate()).append('\n');
        sb.append(getWfo()).append(']');
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

    /**
     * @return true if selected via the table
     */
    public boolean isMonitorSelected() {
        return selection == SelectionEnum.MonitorSelection;
    }

    /**
     * @return true if selected via the map
     */
    public boolean isMapSelected() {
        return selection == SelectionEnum.MapSelection;
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