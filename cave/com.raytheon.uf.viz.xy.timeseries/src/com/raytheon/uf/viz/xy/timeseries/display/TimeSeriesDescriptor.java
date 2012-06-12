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
package com.raytheon.uf.viz.xy.timeseries.display;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.uf.viz.xy.timeseries.graph.TimeSeriesGraph;

/**
 * Time Series descriptor, needed so loading bundles know what editor to load
 * with this descriptor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 21, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class TimeSeriesDescriptor extends XyGraphDescriptor {

    public static final int REAL_FRAME_COUNT_TO_USE_WHEN_FRAME_COUNT_IS_ONE = 999;

    public TimeSeriesDescriptor() {
        super();
    }

    public TimeSeriesDescriptor(PixelExtent pixelExtent) {
        super(pixelExtent);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.XyGraphDescriptor#constructGraph()
     */
    @Override
    public IGraph constructGraph() {
        return new TimeSeriesGraph(this);
    }

    @Override
    public int getNumberOfFrames() {
        int numFrames = super.getNumberOfFrames();
        if (numFrames == 1) {
            // reset to a different number because A1 did
            numFrames = Math.min(
                    REAL_FRAME_COUNT_TO_USE_WHEN_FRAME_COUNT_IS_ONE,
                    limitedNumberOfFrames);
        }
        return numFrames;
    }

}
