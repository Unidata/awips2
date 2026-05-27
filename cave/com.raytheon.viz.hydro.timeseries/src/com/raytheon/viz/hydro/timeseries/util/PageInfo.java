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
import java.util.List;

/**
 * Structure for Page information
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Aug 08, 2008              mpduff       Initial creation
 * Jun 27, 2018  6748        randerso     Code cleanup
 *
 * </pre>
 *
 * @author mpduff
 */

public class PageInfo {
    /** List of GraphInfo objects, one per graph */
    private List<GraphInfo> graphInfoList = new ArrayList<>();

    /** Title */
    private String title = "";

    /**
     * @return the graphInfoArr
     */
    public List<GraphInfo> getGraphInfoList() {
        return graphInfoList;
    }

    /**
     * @param graphInfoList
     *            the graphInfoList to set
     */
    public void setGraphInfoList(List<GraphInfo> graphInfoList) {
        this.graphInfoList = graphInfoList;
    }

    /**
     * @param graphInfo
     */
    public void addGraphInfo(GraphInfo graphInfo) {
        graphInfoList.add(graphInfo);
    }

    /**
     * @return the numberGraphs
     */
    public int getNumberGraphs() {
        return graphInfoList.size();
    }

    /**
     * @return the title
     */
    public String getTitle() {
        return title;
    }

    /**
     * @param title
     *            the title to set
     */
    public void setTitle(String title) {
        this.title = title;
    }

    /**
     * Print data details
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\nPage Title:  " + title);
        sb.append("\nNumber of Graphs:  " + getNumberGraphs());
        for (int i = 0; i < graphInfoList.size(); i++) {
            GraphInfo gd = graphInfoList.get(i);
            sb.append("\n" + gd.toString());
        }

        return sb.toString();
    }

}
