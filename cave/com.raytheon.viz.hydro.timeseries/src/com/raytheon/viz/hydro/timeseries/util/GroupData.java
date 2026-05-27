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
import java.util.Date;
import java.util.List;

/**
 * Group data container for time series graphs
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * May 29, 2018  6748        randerso     Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public class GroupData {
    private Date beginDate = null;

    private Date endDate = null;

    private GroupInfo groupInfo;

    private List<PageData> pageDataList;

    private int currentPageNum;

    private GraphData activeGraph;

    private TraceData selectedTrace;

    /**
     * Constructor
     *
     * @param groupInfo
     *            the metadata for the group
     */
    public GroupData(GroupInfo groupInfo) {
        this.groupInfo = groupInfo;
        this.pageDataList = new ArrayList<>();

        for (PageInfo pageInfo : groupInfo.getPageInfoList()) {
            PageData pageData = new PageData(pageInfo);
            addPageData(pageData);
        }
    }

    /**
     * @return the beginDate
     */
    public Date getBeginDate() {
        return beginDate;
    }

    /**
     * @param beginDate
     *            the beginDate to set
     */
    public void setBeginDate(Date beginDate) {
        this.beginDate = beginDate;
    }

    /**
     * @return the endCal
     */
    public Date getEndDate() {
        return endDate;
    }

    /**
     * @param endDate
     *            the endDate to set
     */
    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    /**
     * @return the currentPageNum
     */
    public int getCurrentPageNum() {
        return currentPageNum;
    }

    /**
     * @param pageNum
     *            the pageNum to set
     */
    public void setCurrentPageNum(int pageNum) {
        this.currentPageNum = pageNum;
    }

    /**
     * @return the activeGraph
     */
    public GraphData getActiveGraph() {
        return activeGraph;
    }

    /**
     * @param activeGraph
     *            the activeGraph to set
     */
    public void setActiveGraph(GraphData activeGraph) {
        this.activeGraph = activeGraph;
    }

    /**
     * @return the groupInfo
     */
    public GroupInfo getGroupInfo() {
        return groupInfo;
    }

    /**
     * @return the pageDataList
     */
    public List<PageData> getPageDataList() {
        return pageDataList;
    }

    /**
     * Add a PageData to the list
     *
     * @param pageData
     */
    public void addPageData(PageData pageData) {
        this.pageDataList.add(pageData);
    }

    /**
     * @return the number of pages in the group
     */
    public int getNumPages() {
        return this.pageDataList.size();
    }

    /**
     * @return the data for the current page
     */
    public PageData getCurrentPage() {
        PageData pageData = this.pageDataList.get(currentPageNum);
        return pageData;
    }

    /**
     * Set the selected trace
     *
     * @param selectedTrace
     */
    public void setSelectedTrace(TraceData selectedTrace) {
        this.selectedTrace = selectedTrace;
    }

    /**
     * @return the selected trace
     */
    public TraceData getSelectedTrace() {
        return this.selectedTrace;
    }
}
