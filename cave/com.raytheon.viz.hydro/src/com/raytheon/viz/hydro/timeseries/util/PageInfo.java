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
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 8, 2008            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class PageInfo {
    /** Number of graphs */
    private int numberGraphs = 0;
    
    /** List of GraphData objects, one per graph */
    private ArrayList<GraphData> graphDataList = new ArrayList<GraphData>();
    
    /** Title */
    private String title = null;

    /**
     * @return the graphInfoArr
     */
    public ArrayList<GraphData> getGraphDataList() {
        return graphDataList;
    }

    /**
     * @param graphInfoArr the graphInfoArr to set
     */
    public void setGraphData(ArrayList<GraphData> graphInfoList) {
        graphDataList = graphInfoList;
    }
    
    public void addGraphData(GraphData graphInfo) {
        graphDataList.add(graphInfo);
    }

    /**
     * @return the numberGraphs
     */
    public int getNumberGraphs() {
        return numberGraphs;
    }

    /**
     * @param numberGraphs the numberGraphs to set
     */
    public void setNumberGraphs(int numberGraphs) {
        this.numberGraphs = numberGraphs;
    }

    /**
     * @return the title
     */
    public String getTitle() {
        return title;
    }

    /**
     * @param title the title to set
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
        sb.append("\nNumber of Graphs:  " + numberGraphs);
        for (int i = 0; i < graphDataList.size(); i++) {
            GraphData gd = graphDataList.get(i);
            sb.append("\n" + gd.toString());
        }
                
        return sb.toString();
    }
    
    
}
