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
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.viz.awipstools.common.IRadialVelocityToolSource;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;

/**
 * Handles the VR Shear Tools Action.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Oct172007    #492        ebabin      Initial Creation.
 *  Dec042007    #492        ebabin      Minor updates.  Awaiting radarResource finish.
 *  19Dec2007    #642        ebabin      Update to fix VRShear/Baseline interaction.
 *  08Jul2010    #2659       bkowal      The sign of the VR Shear Tool will now be correct based
 *                                       on if the tool orientation indicates clockwise rotation or
 *                                       counter-clockwise rotation. The order of the points will also
 *                                       no longer impact the sign that is displayed.
 *  15Mar2013	15693	mgamazaychikov	 Added magnification capability.
 *  02May2013   DR 14587     D. Friedman Correct calculation.  Make source velocity data choice more
 *                                       explicit.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1
 */
public class VRShearLayer extends ShearLayer {

    public static final String VRSHEAR_LOCATION = "VR Shear";

    private static final int NUM_ANGLE_POINTERS = 7200;

    private AbstractVizResource<?, ?> selectedVelocitySource;

    /**
     * @param data
     * @param props
     * @param descriptor
     */
    public VRShearLayer(GenericToolsResourceData<VRShearLayer> data,
            LoadProperties props, MapDescriptor descriptor) {
        super(data, props, descriptor);
        // add magnification capability
        getCapabilities().addCapability(new MagnificationCapability());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        String name = VRSHEAR_LOCATION;
        if (selectedVelocitySource != null)
            name = name + makeLabel(selectedVelocitySource);
        return name;
    }

    @Override
    protected String calculateShearLabel(double length, Coordinate sCoor,
            Coordinate eCoor, Coordinate midpointCoor) throws VizException {

        List<AbstractVizResource<?, ?>> potentialSources = getEligibleResources(false);

        for (AbstractVizResource<?, ?> rsc : getVelocitySources()) {
            Map<String, Object> mapS = (rsc)
                    .interrogate(new ReferencedCoordinate(sCoor));
            Map<String, Object> mapE = (rsc)
                    .interrogate(new ReferencedCoordinate(eCoor));

            VelocityRange velS = new VelocityRange(mapS);
            VelocityRange velE = new VelocityRange(mapE);
            Float velocity = velS.add(velE);

            if (velocity != null) {
                // Apply magnitude, port of RadarPVImageDepict.C ~line 2415
                String s1 = String.valueOf(mapS.get("crsLocation"));
                String s2 = String.valueOf(mapE.get("crsLocation"));
                Double[] d2 = new Double[] { -1.0, -1.0 };
                Double[] d1 = new Double[] { -1.0, -1.0 };
                if (s1 != null) {
                    int i = 0;
                    for (String s : s1.split("[,]")) {
                        d1[i++] = Double.parseDouble(s);
                    }
                }
                if (s2 != null) {
                    int i = 0;
                    for (String s : s2.split("[,]")) {
                        d2[i++] = Double.parseDouble(s);
                    }
                }

                if (d1[0] != -1 && d2[0] != -1 && d1[1] != -1 && d2[1] != -1) {
                    Double[] top = d1;
                    Double[] bottom = d2;

                    double angle1 = Math.toDegrees(Math.atan2(top[0], top[1]));
                    if (angle1 < 0.0) {
                        angle1 += 360.0;
                    }
                    long angle1L = (long) ((angle1 * NUM_ANGLE_POINTERS) / 360.0);

                    double angle2 = Math.toDegrees(Math.atan2(bottom[0],
                            bottom[1]));
                    if (angle2 < 0.0) {
                        angle2 += 360.0;
                    }
                    long angle2L = (long) ((angle2 * NUM_ANGLE_POINTERS) / 360.0);

                    float mag = angle2L - angle1L;
                    if (mag > NUM_ANGLE_POINTERS / 2) {
                        mag = 1.0f;
                    } else if (mag < -NUM_ANGLE_POINTERS / 2 || mag > 0) {
                        mag = -1.0f;
                    } else {
                        mag = 1.0f;
                    }
                    velocity *= mag;
                }

                Map<String, Object> mapMid = (rsc)
                        .interrogate(new ReferencedCoordinate(midpointCoor));
                long midpointRange = new Float(getRangeValue(mapMid))
                        .longValue();
                String velocitySymbol = velS.pickSeparatorSymbol(velE);
                double sec = (velocity / (length * 1800));

                boolean needLabel = selectedVelocitySource != null
                        || potentialSources.size() > 1;
                return String.format("%1s%.1fkts %.1fnm %.4f/s dist:%2dnm%s",
                        velocitySymbol, velocity, length, sec, midpointRange,
                        needLabel ? makeLabel(rsc) : "");
            }
        }
        return "NO DATA"
                + (selectedVelocitySource != null ? makeLabel(selectedVelocitySource) : "");
    }

    @Override
    public String drawLabeling(IGraphicsTarget target, LineString lineString,
            RGB theColor, PaintProperties paintProps) throws VizException {

        // draw the labeling relative to the midpoint of the line.
        // find the midpoint, by using the GeodeticCalculator tools...

        Coordinate startCoor = lineString.getCoordinateN(0);
        Coordinate endCoor = lineString.getCoordinateN(1);

        gc.setStartingGeographicPoint(startCoor.x, startCoor.y);
        gc.setDestinationGeographicPoint(endCoor.x, endCoor.y);
        UnitConverter conv = SI.METER.getConverterTo(NonSI.NAUTICAL_MILE);

        double azimuth = gc.getAzimuth();
        double length = conv.convert(gc.getOrthodromicDistance());

        gc.setDirection(azimuth, gc.getOrthodromicDistance() / 2);
        Coordinate midpoint = new Coordinate(gc.getDestinationGeographicPoint()
                .getX(), gc.getDestinationGeographicPoint().getY());

        Coordinate labelPoint = getCoordinateOnCircle(midpoint, 10, 0);

        String label = "TOO FAR";

        // original code cuts at 27nm....
        if (length <= 27) {
            label = calculateShearLabel(length, startCoor, endCoor, midpoint);
        }
        drawBaselineLabel(target, labelPoint, label);
        return label;
    }

    public AbstractVizResource<?, ?> getSelectedVelocitySource() {
        return selectedVelocitySource;
    }

    public void setSelectedVelocitySource(
            AbstractVizResource<?, ?> selectedVelocitySource) {
        this.selectedVelocitySource = selectedVelocitySource;
        issueRefresh();
    }

    public List<AbstractVizResource<?, ?>> getEligibleResources(boolean forDefault) {
        ArrayList<AbstractVizResource<?, ?>> list = new ArrayList<AbstractVizResource<?,?>>();
        getEligibleResources1(getDescriptor().getResourceList(), forDefault, list);
        Collections.reverse(list); // Topmost first, but TODO: shouldn't reverse combos...
        return list;
    }

    public void getEligibleResources1(ResourceList resourceList, boolean forDefault,
            List<AbstractVizResource<?, ?>> list) {
        for (ResourcePair rp : resourceList) {
            if (rp.getResource() instanceof IResourceGroup) {
                getEligibleResources1(((IResourceGroup)rp.getResource()).getResourceList(), forDefault, list);
            } else if (isEligibleResource(rp, forDefault)) {
                list.add(rp.getResource());
            }
        }
    }

    private boolean isEligibleResource(ResourcePair rp, boolean forDefault) {
        AbstractVizResource<?, ?> resource = rp.getResource();
        return resource instanceof IRadialVelocityToolSource
                && ((IRadialVelocityToolSource) resource)
                        .isRadialVelocitySource() &&
                        (!forDefault || rp.getProperties().isVisible());
    }

    private List<AbstractVizResource<?, ?>> getVelocitySources() {
        List<AbstractVizResource<?, ?>> list;
        if (selectedVelocitySource != null) {
            /* Note: This checks for DISPOSED, but not if the resource has
             * somehow changed so that it no longer contains velocity data.
             * This could happen for combo resources if we held references
             * to the combo rather than the component resources.
             */
            if (selectedVelocitySource.getStatus() != ResourceStatus.DISPOSED) {
                list = new ArrayList<AbstractVizResource<?, ?>>(1);
                list.add(selectedVelocitySource);
                return list;
            } else {
                selectedVelocitySource = null;
            }
        }

        list = getEligibleResources(true);
        return list;
    }

    private String makeLabel(AbstractVizResource<?, ?> resource) {
        return String.format(" (%s)", resource.getName());
    }
}
