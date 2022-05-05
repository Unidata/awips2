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
package com.raytheon.uf.viz.monitor.scan;

import java.util.Date;
import java.util.Map;

import org.eclipse.swt.graphics.Color;

/**
 * Object to hold trend graph data and data point colors
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Aug 12, 2011           mpduff    Initial creation
 * Jul 16, 2018  6766     randerso  Code cleanup
 *
 * </pre>
 *
 * @author mpduff
 */

public class TrendGraphData {
    private Map<Date, Double> graphData;

    private Map<Date, Color> graphDataColors;

    /**
     * @return the graphData
     */
    public Map<Date, Double> getGraphData() {
        return graphData;
    }

    /**
     * @param graphData
     *            the graphData to set
     */
    public void setGraphData(Map<Date, Double> graphData) {
        this.graphData = graphData;
    }

    /**
     * @return the graphDataColors
     */
    public Map<Date, Color> getGraphDataColors() {
        return graphDataColors;
    }

    /**
     * @param graphDataColors
     *            the graphDataColors to set
     */
    public void setGraphDataColors(Map<Date, Color> graphDataColors) {
        this.graphDataColors = graphDataColors;
    }

}
