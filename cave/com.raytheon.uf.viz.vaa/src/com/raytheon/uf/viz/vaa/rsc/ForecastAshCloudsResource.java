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
package com.raytheon.uf.viz.vaa.rsc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.vaa.VAARecord;
import com.raytheon.uf.common.dataplugin.vaa.VAASubPart;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.vaa.util.CommonUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Resource for Forecast Ash Cloud data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2009 3268       jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class ForecastAshCloudsResource extends
        AbstractVizResource<ForecastAshCloudsResourceData, MapDescriptor> {

    protected DataTime displayedDataTime;

    private final static String FCST06 = "FCST06";

    private final static String FCST12 = "FCST12";

    private final static String FCST18 = "FCST18";

    private final static String AREA = "AREA";

    private Map<DataTime, Collection<VAARecord>> recordsToParse = new HashMap<DataTime, Collection<VAARecord>>();

    protected ForecastAshCloudsResource(
            ForecastAshCloudsResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(new IResourceDataChanged() {
            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.DATA_UPDATE) {
                    PluginDataObject[] pdo = (PluginDataObject[]) object;
                    for (PluginDataObject p : pdo) {
                        if (p instanceof VAARecord) {
                            addRecord((VAARecord) p);
                        }
                    }
                }
                issueRefresh();
            }
        });
        this.dataTimes = new ArrayList<DataTime>();
    }

    @Override
    public String getName() {
        return "VAA Forecast Ash Clouds";
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        String returnValue = "";
        if (this.displayedDataTime != null) {
            Collection<VAARecord> records = null;
            if (!this.recordsToParse.containsKey(this.displayedDataTime)) {
                return returnValue;
            } else {
                records = this.recordsToParse.get(this.displayedDataTime);
            }

            for (VAARecord record : records) {
                for (VAASubPart subPart : record.getSubParts()) {
                    if (subPart.getSubText() != null
                            && subPart.getSubText().startsWith("FCST")
                            && subPart.getShapeType().equals(AREA)) {
                        Polygon llPolygon = CommonUtil.getPolygon(record,
                                subPart.getSubText());
                        Polygon pixelPoly = CommonUtil.worldToPixel(llPolygon,
                                descriptor);
                        Point point;

                        try {
                            point = pixelPoly.getFactory()
                                    .createPoint(
                                            coord.asPixel(descriptor
                                                    .getGridGeometry()));
                        } catch (Exception e) {
                            throw new VizException(
                                    "Error inspecting VAA Forecast Ash Clouds",
                                    e);
                        }

                        if (pixelPoly.contains(point)) {
                            String temp = "";
                            if (subPart.getSubText().equals(FCST06)) {
                                temp = record.getFcst06Hr() != null ? record
                                        .getFcst06Hr() : "";
                            } else if (subPart.getSubText().equals(FCST12)) {
                                temp = record.getFcst12Hr() != null ? record
                                        .getFcst12Hr() : "";
                            } else if (subPart.getSubText().equals(FCST18)) {
                                temp = record.getFcst18Hr() != null ? record
                                        .getFcst18Hr() : "";
                            }
                            if (returnValue.length() > 0) {
                                returnValue += "\n \n";
                            }
                            returnValue += temp;
                        }
                    }
                }
            }
        }

        return returnValue.length() > 0 ? returnValue : "NO DATA";
    }

    /**
     * Adds a new record to this resource
     * 
     * @param obj
     */
    protected void addRecord(VAARecord obj) {
        DataTime dataTime = obj.getDataTime();
        Collection<VAARecord> toParse = recordsToParse.get(dataTime);
        if (toParse == null) {
            dataTimes.add(dataTime);
            Collections.sort(this.dataTimes);
            toParse = new ArrayList<VAARecord>();
            recordsToParse.put(dataTime, toParse);
        }
        toParse.add(obj);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime curDataTime = paintProps.getDataTime();
        if (curDataTime == null) {
            this.displayedDataTime = null;
            return;
        }

        Collection<VAARecord> toParse = recordsToParse.get(curDataTime);
        RGB color = getCapability(ColorableCapability.class).getColor();
        target.clearClippingPlane();
        if (toParse != null && toParse.size() > 0) {
            for (VAARecord record : toParse) {
                for (VAASubPart subPart : record.getSubParts()) {
                    if (subPart.getSubText() != null
                            && subPart.getSubText().startsWith("FCST")) {
                        Coordinate[] coordinates = CommonUtil
                                .getCoordinates(subPart.getLocations());
                        IWireframeShape shape = target.createWireframeShape(
                                false, descriptor);
                        shape.addLineSegment(coordinates);
                        target.drawWireframeShape(shape, color, 0.5f);
                        shape.dispose();
                    }
                }
            }
        }

        target.setupClippingPlane(paintProps.getView().getExtent());
        this.displayedDataTime = curDataTime;
    }

    @Override
    protected void disposeInternal() {

    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

    }

}
