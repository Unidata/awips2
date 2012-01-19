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
package com.raytheon.uf.viz.cloudheight.rsc;

import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.cloudheight.CloudHeightAlgorithm;
import com.raytheon.uf.viz.cloudheight.CloudHeightAlgorithm.ICloudHeightSourceImplementation;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.d2d.core.sampling.CloudHeightResource;
import com.raytheon.uf.viz.d2d.core.sampling.CloudHeightResourceData.ICloudHeightAlgorithm;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 5, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PopupSkewTResource extends
        AbstractVizResource<PopupSkewTResourceData, MapDescriptor> {

    private CloudHeightAlgorithm algorithm;

    private CloudHeightResource resource;

    protected PopupSkewTResource(PopupSkewTResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        List<CloudHeightResource> chr = descriptor.getResourceList()
                .getResourcesByTypeAsType(CloudHeightResource.class);
        if (chr.size() > 0) {
            resource = chr.get(0);
            ICloudHeightAlgorithm alg = resource.getAlgorithm();
            if (alg instanceof CloudHeightAlgorithm) {
                algorithm = (CloudHeightAlgorithm) alg;
            }
        }

        if (algorithm == null) {
            descriptor.getResourceList().removeRsc(this);
            throw new VizException("Could not initialize " + getName()
                    + ", no CloudHeightAlgorithm found");
        }
    }

    @Override
    public String getName() {
        String name = "Radar Popup SkewT";
        DataTime cur = getCurrentTime();
        if (cur != null) {
            name += " " + cur.getLegendString();
        }
        return name;
    }

    private DataTime getCurrentTime() {
        FramesInfo info = descriptor.getFramesInfo();
        DataTime[] frameTimes = info.getFrameTimes();
        if (frameTimes != null) {
            int idx = info.getFrameIndex();
            if (idx > -1 && idx < frameTimes.length) {
                return frameTimes[idx];
            }
        }
        return null;
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        ICloudHeightSourceImplementation impl = algorithm
                .getCurrentImplementation();
        if (impl != null) {
            algorithm.setSkewT(true);

            Coordinate latLon = null;
            try {
                latLon = coord.asLatLon();
            } catch (Exception e) {

            }

            Float msl = getMSL(coord, descriptor.getResourceList());

            if (msl != null && latLon != null) {
                VerticalSounding vs = impl.createSounding(latLon,
                        getCurrentTime());
                if (vs != null) {
                    if (vs != CloudHeightAlgorithm.LOADING) {
                        algorithm.setSounding(vs);
                        algorithm.plotHeight(msl, 1e37f);
                    } else {
                        issueRefresh();
                    }
                }
            }
        }
        return "_";
    }

    /**
     * Recursively search for msl by interrogating the resources on the list
     * 
     * @param coord
     * @param list
     * @return
     * @throws VizException
     */
    private Float getMSL(ReferencedCoordinate coord, ResourceList list)
            throws VizException {
        Float msl = null;
        for (ResourcePair rp : list) {
            AbstractVizResource<?, ?> r = rp.getResource();
            if (r != null && rp.getProperties().isVisible()) {
                Map<String, Object> interrogatedVals = r.interrogate(coord);
                if (interrogatedVals != null) {
                    Object val = interrogatedVals.get("msl");
                    if (val != null) {
                        try {
                            msl = Float.parseFloat(String.valueOf(val));
                            return msl;
                        } catch (Throwable t) {
                            ;
                        }
                    }
                }
                if (r instanceof IResourceGroup) {
                    msl = getMSL(coord, ((IResourceGroup) r).getResourceList());
                    if (msl != null) {
                        return msl;
                    }
                }
            }
        }
        return null;
    }

    @Override
    protected void disposeInternal() {
        if (algorithm != null) {
            algorithm.dispose();
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

    }

}
