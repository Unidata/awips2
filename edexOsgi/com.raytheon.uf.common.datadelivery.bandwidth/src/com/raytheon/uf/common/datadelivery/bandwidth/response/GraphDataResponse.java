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
package com.raytheon.uf.common.datadelivery.bandwidth.response;

import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthGraphData;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Response object for the GraphDataRequest.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 25, 2012   2369     mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class GraphDataResponse implements ISerializableObject {
    @DynamicSerializeElement
    private BandwidthGraphData binDuration;

    @DynamicSerializeElement
    private BandwidthGraphData graphData;

    /**
     * Constructor.
     */
    public GraphDataResponse() {

    }

    /**
     * @param binDuration
     *            the binDuration to set
     */
    public void setBinDuration(BandwidthGraphData binDuration) {
        this.binDuration = binDuration;
    }

    /**
     * @return the binDuration
     */
    public BandwidthGraphData getBinDuration() {
        return binDuration;
    }

    /**
     * @param graphData
     *            the graphData to set
     */
    public void setGraphData(BandwidthGraphData graphData) {
        this.graphData = graphData;
    }

    /**
     * @return the graphData
     */
    public BandwidthGraphData getGraphData() {
        return graphData;
    }
}
