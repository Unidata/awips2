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
package com.raytheon.uf.viz.stats.ui;

import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.stats.data.GraphData;


/**
 * Stats display interface.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2012     728     mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public interface IStatsDisplay {
    /**
     * Get the GraphData object
     *
     * @return GraphData
     */
    GraphData getGraphData();

    /**
     * Draw grid lines flag
     *
     * @return true to draw the grid lines
     */
    boolean drawGridLines();

    /**
     * Draw data lines flag
     *
     * @return true to draw the data lines
     */
    boolean drawDataLines();

    /**
     * Get the group settings.
     *
     * @return The group settings map
     */
    Map<String, RGB> getGroupSettings();
}
