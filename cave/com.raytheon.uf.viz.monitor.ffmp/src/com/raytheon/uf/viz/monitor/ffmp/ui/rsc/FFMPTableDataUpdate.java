package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

import java.util.Date;

import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPTableData;

/**
 * Table Data Updates go through this object
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 April, 2012 2521          dhladky     Initial creation
 * 02/01/13     1569       D. Hladky   Added constants
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class FFMPTableDataUpdate {

    private boolean allowNewTableUpdate = false;

    private boolean sourceUpdate = false;

    private FFMPTableData tableData = null;

    private boolean fireGraph = false;

    private String graphPfaf = null;

    private Double gapValueLabel = null;

    private Date date = null;

    private Date validTime = null;

    private Date graphTime = null;

    public FFMPTableDataUpdate() {
    }

    public void setTableData(FFMPTableData tableData) {
        this.tableData = tableData;
    }

    public FFMPTableData getTableData() {
        return tableData;
    }

    public void setGraphPfaf(String graphPfaf) {
        this.graphPfaf = graphPfaf;
    }

    public String getGraphPfaf() {
        return graphPfaf;
    }

    public void setGapValueLabel(Double gapValueLabel) {
        this.gapValueLabel = gapValueLabel;
    }

    public Double getGapValueLabel() {
        return gapValueLabel;
    }

    public void setFireGraph(boolean fireGraph) {
        this.fireGraph = fireGraph;
    }

    public boolean isFireGraph() {
        return fireGraph;
    }

    public void setValidTime(Date validTime) {
        this.validTime = validTime;
    }

    public Date getValidTime() {
        return validTime;
    }

    public void setGraphTime(Date graphTime) {
        this.graphTime = graphTime;
    }

    public Date getGraphTime() {
        return graphTime;
    }

    public void setAllowNewTableUpdate(boolean allowNewTableUpdate) {
        this.allowNewTableUpdate = allowNewTableUpdate;
    }

    public boolean isAllowNewTableUpdate() {
        return allowNewTableUpdate;
    }

    public void setSourceUpdate(boolean sourceUpdate) {
        this.sourceUpdate = sourceUpdate;
    }

    public boolean isSourceUpdate() {
        return sourceUpdate;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    public Date getDate() {
        return date;
    }

}
