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
package com.raytheon.uf.viz.monitor.ffmp.xml;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Config Basin xml object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Apr 12, 2013   1902    mpduff       Return a FFMPTableColumnXML object.
 * 
 * </pre>
 * 
 */

@XmlRootElement(name = "FfmpConfigBasin")
@XmlAccessorType(XmlAccessType.NONE)
public class FFMPConfigBasinXML implements ISerializableObject {
    /**
     * When on, the Basin Table will use the time of the D2D frame that launched
     * it. When off the Basin Table will either use the most recent time in the
     * inventory of will use the time of the last frame in the parent IGC.
     */
    @XmlElement(name = "LinkToFrame")
    private boolean linkToFrame;

    @XmlElement(name = "WorstCase")
    private boolean worstCase;

    @XmlElement(name = "MaintainLayer")
    private boolean maintainLayer;

    @XmlElement(name = "OnlyForParent")
    private boolean onlyForParent;

    @XmlElement(name = "AutoRefresh")
    private boolean autoRefresh;

    @XmlElement(name = "TimeFrame")
    private double timeFrame;

    @XmlElement(name = "D2DType")
    private String d2dType;

    /**
     * Column sorted on startup.
     */
    @XmlElement(name = "ColumnSorted")
    private String columnSorted;

    @XmlElement(name = "Layer")
    private String layer;

    @XmlElement(name = "GroupID")
    private String groupID;

    @XmlElement(name = "GuidSrc")
    private String guidSrc = "xxxxx";

    @XmlElement(name = "IncludedCWAs")
    private String includedCWAs;

    @XmlElement(name = "IncludedGuids")
    private String includedGuids;

    @XmlElement(name = "IncludedQPF")
    private String includedQPF;

    @XmlElement(name = "Underlay")
    private String underlay;

    @XmlElement(name = "BasinTrendPlots")
    private String basinTrendPlots;

    @XmlElements({ @XmlElement(name = "TableColumn", type = FFMPTableColumnXML.class) })
    private ArrayList<FFMPTableColumnXML> tableColumnData;

    /**
     * Temp data structure.
     */
    private transient Map<String, FFMPTableColumnXML> tableColumnMap = new HashMap<String, FFMPTableColumnXML>();

    public FFMPConfigBasinXML() {

    }

    public boolean getLinkToFrame() {
        return linkToFrame;
    }

    public void setLinkToFrame(boolean linkToFrame) {
        this.linkToFrame = linkToFrame;
    }

    public boolean getWorstCase() {
        return worstCase;
    }

    public void setWorstCase(boolean worstCase) {
        this.worstCase = worstCase;
    }

    public boolean getMaintainLayer() {
        return maintainLayer;
    }

    public void setMaintainLayer(boolean maintainLayer) {
        this.maintainLayer = maintainLayer;
    }

    public boolean getOnlyForParent() {
        return onlyForParent;
    }

    public void setOnlyForParent(boolean onlyForParent) {
        this.onlyForParent = onlyForParent;
    }

    public boolean getAutoRefresh() {
        return autoRefresh;
    }

    public void setAutoRefresh(boolean autoRefresh) {
        this.autoRefresh = autoRefresh;
    }

    public double getTimeFrame() {
        return timeFrame;
    }

    public void setTimeFrame(double timeFrame) {
        this.timeFrame = timeFrame;
    }

    public String getD2dType() {
        return d2dType;
    }

    public void setD2dType(String type) {
        d2dType = type;
    }

    public String getColumnSorted() {
        return columnSorted;
    }

    public void setColumnSorted(String columnSorted) {
        this.columnSorted = columnSorted;
    }

    public String getLayer() {
        return layer;
    }

    public void setLayer(String layer) {
        this.layer = layer;
    }

    public String getGroupID() {
        return groupID;
    }

    public void setGroupID(String groupID) {
        this.groupID = groupID;
    }

    public String getGuidSrc() {
        return guidSrc;
    }

    public void setGuidSrc(String guidSrc) {
        this.guidSrc = guidSrc;
    }

    public String getIncludedCWAs() {
        return includedCWAs;
    }

    public void setIncludedCWAs(String includedCWAs) {
        this.includedCWAs = includedCWAs;
    }

    public String getIncludedGuids() {
        return includedGuids;
    }

    public void setIncludedGuids(String includedGuids) {
        this.includedGuids = includedGuids;
    }

    public String getIncludedQPF() {
        return includedQPF;
    }

    public void setIncludedQPF(String includedQPF) {
        this.includedQPF = includedQPF;
    }

    public String getUnderlay() {
        return underlay;
    }

    public void setUnderlay(String underlay) {
        this.underlay = underlay;
    }

    public String getBasinTrendPlots() {
        return basinTrendPlots;
    }

    public void setBasinTrendPlots(String basinTrendPlots) {
        this.basinTrendPlots = basinTrendPlots;
    }

    public ArrayList<FFMPTableColumnXML> getTableColumnData() {
        return tableColumnData;
    }

    public void setTableColumnData(ArrayList<FFMPTableColumnXML> tableColumnData) {
        this.tableColumnData = tableColumnData;
    }

    /**
     * Get the FFMPTableColumn object for the provided column name
     * 
     * @param colName
     *            the column name
     * @return the FFMPTableColumnXML object
     */
    public FFMPTableColumnXML getTableColumnData(String colName) {
        if (!tableColumnMap.containsKey(colName)) {
            for (FFMPTableColumnXML tc : tableColumnData) {
                if (tc.getColumnName().equals(colName)) {
                    tableColumnMap.put(colName, tc);
                    break;
                }
            }
        }

        return tableColumnMap.get(colName);
    }
}
