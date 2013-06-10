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
package com.raytheon.viz.awipstools.ui.layer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.tools.AwipsToolsResourceData;
import com.raytheon.viz.awipstools.capabilities.EAVCapability;
import com.raytheon.viz.awipstools.common.EstimatedActualVelocity;
import com.raytheon.viz.core.rsc.BestResResource;
import com.raytheon.viz.core.rsc.BlendedResource;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;

/**
 * The estimated actual velocity application, very similar to the VR-Shear app
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/12/2010              mnash       Initial building of application
 * 01/26/2010   4127       mpduff      Implemented parsing of eavConfigTable.txt
 * 10/08/2010   5953       bgonzale    refactored EAV code out of layer class.
 * 15Mar2013	15693	mgamazaychikov Added magnification capability.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class EstimatedActualVelocityLayer extends VRShearLayer {

    public static final String EST_ACT_VEL_LOCATION = "Estimated Act Vel";

    private EstimatedActualVelocity eavOfPrimaryVelocityLayer;

    /**
     * @param data
     * @param props
     * @param descriptor
     */
	public EstimatedActualVelocityLayer(
			AwipsToolsResourceData<VRShearLayer> data, LoadProperties props,
			MapDescriptor descriptor) {
		super(data, props, descriptor);
		// add magnification capability
		getCapabilities().addCapability(new MagnificationCapability());
	}

    @Override
    public String getName() {
        return EST_ACT_VEL_LOCATION;
    }

    @Override
    protected String calculateShearLabel(double length, Coordinate sCoor,
            Coordinate eCoor, Coordinate midpointCoor) throws VizException {
        Map<String, Object> sMap = getDataMap(sCoor);
        Map<String, Object> eMap = getDataMap(eCoor);

        if (eavOfPrimaryVelocityLayer == null) {
            return "";
        } else {
            String rval = eavOfPrimaryVelocityLayer.getEAVValues(sCoor, sMap,
                    eCoor, eMap);
            return rval;
        }
    }

    private Map<String, Object> getDataMap(Coordinate coor) throws VizException {
        ResourceList list = descriptor.getResourceList();
        Iterator<ResourcePair> iter = list.iterator();
        Map<String, Object> firstMap = null;

        while (iter.hasNext()) {
            ResourcePair rPair = iter.next();

            for (AbstractVizResource<AbstractResourceData, IDescriptor> rsc : getResources(rPair)) {
                Map<String, Object> map = rsc
                        .interrogate(new ReferencedCoordinate(coor));
                if (EstimatedActualVelocity.hasEAV(map)) {
                    EAVCapability eavCap = rsc
                            .getCapability(EAVCapability.class);
                    eavCap.setCapabilityActive(true);
                    registerListener(eavCap);
                    // if it is not necessary to display the EAV values in a
                    // sample for multiple velocity layers, then remove the
                    // following code and return the first map here.
                    if (firstMap == null) {
                        eavOfPrimaryVelocityLayer = eavCap.getEav();
                        firstMap = map;
                    } else {
                        eavCap.getEav()
                                .setPrimaryEav(eavOfPrimaryVelocityLayer);
                    }
                }
            }
        }
        return firstMap;
    }

    @Override
    public String drawLabeling(IGraphicsTarget target, LineString lineString,
            RGB theColor, PaintProperties paintProps) throws VizException {
        Coordinate startCoor = lineString.getCoordinateN(0);
        Coordinate endCoor = lineString.getCoordinateN(1);

        gc.setStartingGeographicPoint(startCoor.x, startCoor.y);
        gc.setDestinationGeographicPoint(endCoor.x, endCoor.y);
        UnitConverter conv = SI.METER.getConverterTo(NonSI.NAUTICAL_MILE);

        double azimuth = gc.getAzimuth();
        double length = conv.convert(gc.getOrthodromicDistance());

        gc.setDirection(azimuth, gc.getOrthodromicDistance());
        Coordinate midpoint = new Coordinate(gc.getDestinationGeographicPoint()
                .getX(), gc.getDestinationGeographicPoint().getY());

        Coordinate bottomLabelPoint = new Coordinate(gc
                .getDestinationGeographicPoint().getX(), gc
                .getDestinationGeographicPoint().getY());
        Coordinate topLabelPoint = new Coordinate(gc
                .getStartingGeographicPoint().getX(), gc
                .getStartingGeographicPoint().getY());

        String label = EstimatedActualVelocity.NO_DATA;

        label = calculateShearLabel(length, startCoor, endCoor, midpoint);
        // if (label != )
        if ("".equals(label) || EstimatedActualVelocity.NO_DATA.equals(label)) {
            double xDistance = 0;
            double yDistance = 0;
            xDistance = topLabelPoint.x - bottomLabelPoint.x;
            yDistance = topLabelPoint.y - bottomLabelPoint.y;
            if (((yDistance > 0) && (yDistance > xDistance))
                    || ((yDistance < 0) && (yDistance > xDistance))) {
                drawBaselineLabel(target, topLabelPoint, "NO");
                drawBaselineLabel(target, bottomLabelPoint, "DATA");
            } else {
                drawBaselineLabel(target, bottomLabelPoint, "NO");
                drawBaselineLabel(target, topLabelPoint, "DATA");
            }
        } else {
            String[] labels = new String[2];
            labels = label.split("\\s+");
            drawBaselineLabel(target, topLabelPoint, labels[0]);
            drawBaselineLabel(target, bottomLabelPoint, labels[1]);
        }
        return label;
    }

    @SuppressWarnings("unchecked")
    private static Collection<AbstractVizResource<AbstractResourceData, IDescriptor>> getResources(
            ResourcePair rPair) {
        Collection<AbstractVizResource<AbstractResourceData, IDescriptor>> resources = new ArrayList<AbstractVizResource<AbstractResourceData, IDescriptor>>();
        AbstractVizResource<?, ?> rsc = rPair.getResource();

        // if (rPair.getProperties().isVisible()) {
        if (rsc instanceof BlendedResource) {
            ResourceList rl = ((BlendedResource) rsc).getResourceList();

            for (int i = 0; i < rl.size(); ++i) {
                ResourcePair rp = rl.get(i);
                resources.addAll(getResources(rp));
            }
        } else if (rsc instanceof BestResResource) {
            ResourceList rl = ((BestResResource) rsc).getResourceList();
            for (int i = 0; i < rl.size(); ++i) {
                ResourcePair rp = rl.get(i);
                resources.addAll(getResources(rp));
            }
        } else if (rsc != null) {
            resources
                    .add((AbstractVizResource<AbstractResourceData, IDescriptor>) rsc);
        }
        // }
        return resources;
    }
}
