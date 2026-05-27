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
 * Structure for Page data in general
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * May 23, 2018  6748        randerso     Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public class PageData {
    private PageInfo pageInfo;

    private List<GraphData> graphDataList;

    /**
     * Constructor
     *
     * @param pageInfo
     */
    public PageData(PageInfo pageInfo) {
        this.pageInfo = pageInfo;
        this.graphDataList = new ArrayList<>();

        for (GraphInfo graphInfo : pageInfo.getGraphInfoList()) {
            GraphData graphData = new GraphData(graphInfo);
            addGraphData(graphData);
        }
    }

    /**
     * @return the pageInfo
     */
    public PageInfo getPageInfo() {
        return pageInfo;
    }

    /**
     * @return the graphDataList
     */
    public List<GraphData> getGraphDataList() {
        return graphDataList;
    }

    /**
     * Add a GraphData to the graphDataList
     *
     * @param graphData
     */
    public void addGraphData(GraphData graphData) {
        this.graphDataList.add(graphData);
    }

    /**
     * @return the number of graphs
     */
    public int getNumGraphs() {
        return this.getGraphDataList().size();
    }

    /**
     * Load the data for this page over the given time interval
     *
     * @param beginDate
     * @param endDate
     */
    public void loadData(Date beginDate, Date endDate) {
        for (GraphData graphData : graphDataList) {
            graphData.getGraphData(beginDate, endDate);
            graphData.findMinMax(true, beginDate, endDate);
        }
    }
}
