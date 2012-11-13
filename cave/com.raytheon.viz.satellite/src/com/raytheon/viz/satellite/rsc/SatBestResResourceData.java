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
package com.raytheon.viz.satellite.rsc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import org.geotools.referencing.CRS;
import org.geotools.referencing.operation.transform.ConcatenatedTransform;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.geospatial.BoundaryTool;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.NoDataAvailableException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ProgressiveDisclosureProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.satellite.SatelliteDataCubeAdapter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;

/**
 * A best res resource for satellite data, uses progressive disclosure
 * properties to pick which satellite image to display
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2010            mschenke     Initial creation
 * Nov 06, 2012  DR 15157  D. Friedman  Allow configured inclusion percentage
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SatBestResResourceData extends AbstractRequestableResourceData {

    private static final double DESIRED_PERCENTAGE = 0.4;

    @XmlElement(name = "resource")
    protected ResourceList resourceList = new ResourceList();

    @XmlAttribute
    protected Double inclusionFactor;

    private ResourcePair resourceToDraw;

    /**
     * Default Constructor.
     */
    public SatBestResResourceData() {
        this.retrieveData = false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        if (resourceToDraw == null) {
            selectResourceToDraw(descriptor);
        }

        if (resourceToDraw != null) {
            return resourceToDraw.getResourceData().construct(loadProperties,
                    descriptor);
        }

        throw new NoDataAvailableException(getClass());
    }

    public ResourcePair getResourceToDraw(IDescriptor descriptor)
            throws VizException {
        double minX = descriptor.getGridGeometry().getGridRange().getLow(0);
        double minY = descriptor.getGridGeometry().getGridRange().getLow(1);
        double maxX = descriptor.getGridGeometry().getGridRange().getHigh(0);
        double maxY = descriptor.getGridGeometry().getGridRange().getHigh(1);
        GeometryFactory gf = new GeometryFactory();
        Coordinate first = new Coordinate(minX, minY);
        Polygon extent = gf.createPolygon(
                gf.createLinearRing(new Coordinate[] { first,
                        new Coordinate(maxX, minY), new Coordinate(maxX, maxY),
                        new Coordinate(minX, maxY), new Coordinate(first) }),
                null);

        ResourcePair disclosedResource = null;
        if (resourceList.size() > 0) {
            ProgressiveDisclosureProperties props = resourceList
                    .get(resourceList.size() - 1).getProperties().getPdProps();
            if (props != null) {
                props.setMinDisplayWidth(0);
            }

            props = resourceList.get(0).getProperties().getPdProps();
            if (props != null) {
                props.setMaxDisplayWidth(Integer.MAX_VALUE);
            }

            int displayWidth = ((IMapDescriptor) descriptor).getMapWidth();

            int i = 0;
            for (ResourcePair rp : resourceList) {
                props = rp.getProperties().getPdProps();
                if (props != null && props.isDisclosed(displayWidth)) {
                    disclosedResource = rp;
                    break;
                }
                ++i;
            }
        }

        Map<ResourcePair, Double> percentOfIntersection = new HashMap<ResourcePair, Double>();
        if (disclosedResource != null) {
            final double inclusionPercentageToUse = inclusionFactor != null ?
                    inclusionFactor : DESIRED_PERCENTAGE;
            // check inclusion percentage of the disclosed resource
            Double inclusion = getInclusionPercentage(descriptor,
                    disclosedResource, extent);
            if (inclusion != Double.NaN) {
                percentOfIntersection.put(disclosedResource, inclusion);
                if (inclusion < inclusionPercentageToUse) {
                    disclosedResource = null;
                }
            } else {
                resourceList.remove(disclosedResource);
                disclosedResource = null;
            }
        }

        if (disclosedResource == null) {
            // find the first resource with desired inclusion percentage
            for (ResourcePair rp : resourceList) {
                Double inclusion = percentOfIntersection.get(rp);
                if (inclusion == null) {
                    inclusion = getInclusionPercentage(descriptor, rp, extent);
                    if (inclusion != Double.NaN) {
                        percentOfIntersection.put(rp, inclusion);
                    } else {
                        continue;
                    }
                }

                if (inclusion >= DESIRED_PERCENTAGE) {
                    disclosedResource = rp;
                    break;
                }
            }
        }

        if (disclosedResource == null) {
            // find the resource with the highest inclusion percentage
            // find highest area of intersection
            Double highest = 0.0;
            for (ResourcePair rp : percentOfIntersection.keySet()) {
                Double percent = percentOfIntersection.get(rp);
                if (percent > highest) {
                    disclosedResource = rp;
                    highest = percent;
                    break;
                }
            }
        }

        return disclosedResource;
    }

    private void selectResourceToDraw(IDescriptor descriptor)
            throws VizException {
        resourceToDraw = getResourceToDraw(descriptor);
    }

    @Override
    public void configure(LoadProperties loadProperties, IDescriptor descriptor)
            throws VizException {
        selectResourceToDraw(descriptor);
        super.configure(loadProperties, descriptor);
    }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        if (resourceToDraw != null
                && resourceToDraw.getResourceData() instanceof AbstractRequestableResourceData) {
            return ((AbstractRequestableResourceData) resourceToDraw
                    .getResourceData()).getAvailableTimes();
        } else {
            return super.getAvailableTimes();
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        for (ResourcePair pair : resourceList) {
            // If one of our resource choices equals obj then we consider
            // ourselves equal to obj.
            AbstractResourceData rd = pair.getResourceData();
            if ((rd instanceof SatResourceData || rd instanceof SatBlendedResourceData)
                    && pair.getResourceData().equals(obj)) {
                return true;
            }
        }

        if (obj instanceof SatBestResResourceData == false) {
            return false;
        }
        SatBestResResourceData other = (SatBestResResourceData) obj;

        if (this.resourceList != null && other.resourceList == null) {
            return false;
        } else if (this.resourceList == null && other.resourceList != null) {
            return false;
        } else if (this.resourceList != null
                && this.resourceList.equals(other.resourceList) == false) {
            return false;
        }

        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
     * )
     */
    @Override
    public void update(Object updateData) {
        // ignore?
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * constructResource(com.raytheon.uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.common.dataplugin.PluginDataObject[])
     */
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        // Do nothing
        return null;
    }

    private double getInclusionPercentage(IDescriptor descriptor,
            ResourcePair rp, Polygon extent) throws VizException {
        Double totalPercentage = Double.NaN;
        MathTransform crsToScreen = null;
        try {
            crsToScreen = descriptor.getGridGeometry().getGridToCRS().inverse();
        } catch (Exception e1) {
            throw new VizException(e1);
        }
        if (crsToScreen != null) {
            try {
                AbstractRequestableResourceData aard = (AbstractRequestableResourceData) rp
                        .getResourceData();
                DbQueryRequest request = new DbQueryRequest();
                Map<String, RequestConstraint> copy = new HashMap<String, RequestConstraint>(
                        aard.getMetadataMap());
                copy.remove(SatelliteDataCubeAdapter.DERIVED);
                request.setConstraints(copy);
                request.addRequestField("coverage");
                request.setDistinct(true);
                DbQueryResponse response = (DbQueryResponse) ThriftClient
                        .sendRequest(request);
                List<Polygon> prevs = new ArrayList<Polygon>();
                for (Map<String, Object> result : response.getResults()) {
                    ISpatialObject so = (ISpatialObject) result.get("coverage");
                    MathTransform gridToScreen = ConcatenatedTransform.create(
                            MapUtil.getGridGeometry(so).getGridToCRS(),
                            ConcatenatedTransform.create(CRS.findMathTransform(
                                    so.getCrs(), descriptor.getCRS()),
                                    crsToScreen));
                    Polygon polygon = BoundaryTool.calculateBoundaryGeometry(
                            gridToScreen, 0, 0, so.getNx(), so.getNy(), false,
                            Math.max(1, so.getNx() / 100),
                            Math.max(1, so.getNy() / 100));
                    Double percentage = polygon.intersection(extent).getArea()
                            / extent.getArea();
                    if (prevs.size() == 0) {
                        totalPercentage = percentage;
                    } else {
                        totalPercentage += percentage;

                        for (Polygon last : prevs) {
                            // Don't want to double include percentage areas
                            totalPercentage -= last.intersection(polygon)
                                    .getArea() / extent.getArea();
                        }
                    }
                    prevs.add(polygon);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return totalPercentage;
    }

    public Double getInclusionFactor() {
        return inclusionFactor;
    }

    public void setInclusionFactor(Double inclusionFactor) {
        this.inclusionFactor = inclusionFactor;
    }
}
