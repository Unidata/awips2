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
package com.raytheon.viz.hydro.timeseries.util;

import java.util.ArrayList;

/**
 * This object holds the Group metadata for the Time Series Viewer.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 7, 2008				mpduff	Initial creation
 * 
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class GroupInfo {
    private boolean gridLines = true;
    private String traceMode;
    private int pastHours;
    private int futureHours;
    private int currentPage;
    private boolean groupSelected;
    
    private String gridMode = null;
    private String groupName = null;
    private String description = null;
    private ArrayList<PageInfo> pageInfoList = new ArrayList<PageInfo>();
    
    /**
     * @return the currentPage
     */
    public int getCurrentPage() {
        return currentPage;
    }
    /**
     * @param currentPage the currentPage to set
     */
    public void setCurrentPage(int currentPage) {
        this.currentPage = currentPage;
    }
    /**
     * @return the groupSelected
     */
    public boolean isGroupSelected() {
        return groupSelected;
    }
    /**
     * @param groupSelected the groupSelected to set
     */
    public void setGroupSelected(boolean groupSelected) {
        this.groupSelected = groupSelected;
    }
    /**
     * @return the gridLines
     */
    public boolean isGridLines() {
        return gridLines;
    }
    /**
     * @param gridLines the gridLines to set
     */
    public void setGridLines(boolean gridLines) {
        this.gridLines = gridLines;
    }
    /**
     * @return the traceMode
     */
    public String getTraceMode() {
        return traceMode;
    }
    /**
     * @param traceMode the traceMode to set
     */
    public void setTraceMode(String traceMode) {
        this.traceMode = traceMode;
    }
    /**
     * @return the pastHours
     */
    public int getPastHours() {
        return pastHours;
    }
    /**
     * @param pastHours the pastHours to set
     */
    public void setPastHours(int pastHours) {
        this.pastHours = pastHours;
    }
    /**
     * @return the futureHours
     */
    public int getFutureHours() {
        return futureHours;
    }
    /**
     * @param futureHours the futureHours to set
     */
    public void setFutureHours(int futureHours) {
        this.futureHours = futureHours;
    }
    /**
     * @return the gridMode
     */
    public String getGridMode() {
        return gridMode;
    }
    /**
     * @param gridMode the gridMode to set
     */
    public void setGridMode(String gridMode) {
        this.gridMode = gridMode;
    }
    /**
     * @return the groupName
     */
    public String getGroupName() {
        return groupName;
    }
    /**
     * @param groupName the groupName to set
     */
    public void setGroupName(String groupName) {
        this.groupName = groupName;
    }
    /**
     * @return the description
     */
    public String getDescription() {
        return description;
    }
    /**
     * @param description the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }
    /**
     * @return the pageInfoList
     */
    public ArrayList<PageInfo> getPageInfoList() {
        return pageInfoList;
    }
    /**
     * @param pageInfoList the pageInfoList to set
     */
    public void setPageInfoList(ArrayList<PageInfo> pageInfoList) {
        this.pageInfoList = pageInfoList;
    }
    
    /**
     * @param pageInfo the PageInfo object to add to the list
     * @param pageInfo
     */
    public void addPageInfo(PageInfo pageInfo) {
        pageInfoList.add(pageInfo);
    }
    
    /**
     * Print the data details
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Description:  " + description);
        sb.append("\nCurrent Page:  " + currentPage);
        sb.append("\nFuture Hours:  " + futureHours);
        sb.append("\nPast Hours:  " + pastHours);
        sb.append("\nGridLines:  " + gridLines);
        sb.append("\nGroup Selected:  " + groupSelected);
        sb.append("\nGrid Mode:  " + gridMode);
        sb.append("\nGroup Name:  " + groupName);
        sb.append("\nTrace Mode:  " + traceMode);
        for (int i = 0; i < pageInfoList.size(); i++) {
            PageInfo pi = pageInfoList.get(i);
            sb.append("\n" + pi.toString());
        }

        return sb.toString();
    }
}
