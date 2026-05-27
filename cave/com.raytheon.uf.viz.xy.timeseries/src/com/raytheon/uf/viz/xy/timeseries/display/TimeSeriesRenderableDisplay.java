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

import java.util.Map;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.d2d.ui.AbstractNonMapDisplay;
import com.raytheon.uf.viz.xy.map.rsc.GraphResourceData;
import com.raytheon.uf.viz.xy.map.rsc.GraphResourceData.OverlayMode;

/**
 * Time series renderable display, plugs in the time series graph factory,
 * descriptor and sets the graph resource
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2009            mschenke    Initial creation
 * Oct 12, 2022 8946       mapeters    Add getScaleType()
 * Dec 20, 2023 2036519    mapeters    Don't construct the graph resource in
 *                                     customizeResourceList()
 *
 * </pre>
 *
 * @author mschenke
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class TimeSeriesRenderableDisplay extends AbstractNonMapDisplay {

    public TimeSeriesRenderableDisplay() {
        this(new PixelExtent(0, 1000, 0, 1000));
    }

    public TimeSeriesRenderableDisplay(PixelExtent aPixelExtent) {
        super(aPixelExtent, new TimeSeriesDescriptor(aPixelExtent));
    }

    @Override
    public TimeSeriesDescriptor getDescriptor() {
        return (TimeSeriesDescriptor) super.getDescriptor();
    }

    @Override
    protected void customizeResourceList(ResourceList resourceList) {
        super.customizeResourceList(resourceList);

        // Add time series graph resource
        GraphResourceData grd = new GraphResourceData("Time series background");
        LoadProperties lprops = new LoadProperties();
        ResourceProperties rprops = new ResourceProperties();
        rprops.setMapLayer(true);
        grd.setOverlayMode(OverlayMode.VERTICAL);
        ResourcePair rp = new ResourcePair();
        rp.setResourceData(grd);
        rp.setProperties(rprops);
        rp.setLoadProperties(lprops);
        resourceList.add(rp);
    }

    @Override
    public Map<String, Object> getGlobalsMap() {
        Map<String, Object> globals = super.getGlobalsMap();
        if (globals.get(VizConstants.FRAMES_ID).equals(
                TimeSeriesDescriptor.REAL_FRAME_COUNT_TO_USE_WHEN_FRAME_COUNT_IS_ONE)) {
            globals.put(VizConstants.FRAMES_ID, 1);
        }
        return globals;
    }

    @Override
    public ScaleType getScaleType() {
        return ScaleType.NONE;
    }
}
