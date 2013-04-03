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
package com.raytheon.uf.common.stats;

import java.util.List;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.stats.data.GraphData;
import com.raytheon.uf.common.stats.xml.StatisticsConfig;

/**
 * Response object for the GraphDataRequest.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 11, 2012    728     mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class GraphDataResponse implements ISerializableObject {
    @DynamicSerializeElement
    private GraphData graphData;

    @DynamicSerializeElement
    private List<StatisticsConfig> configList;

    public GraphDataResponse() {

    }

    /**
     * @return the configList
     */
    public List<StatisticsConfig> getConfigList() {
        return configList;
    }

    /**
     * @param configList
     *            the configList to set
     */
    public void setConfigList(List<StatisticsConfig> configList) {
        this.configList = configList;
    }

    /**
     * Set the GraphData object
     *
     * @param graphData
     */
    public void setGraphData(GraphData graphData) {
        this.graphData = graphData;
    }

    /**
     * Get the GraphData object
     *
     * @return
     */
    public GraphData getGraphData() {
        return this.graphData;
    }
}
