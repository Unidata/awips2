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
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

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
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.vaa.util.CommonUtil;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;

/**
 * Resource for Forecast Ash Cloud data
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 28, 2009  3268     jsanchez  Initial creation
 * Sep 23, 2016  5887     mapeters  Added shapeMap to reuse wireframe shapes
 *                                  across paintInternal() calls
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * 
 * </pre>
 *
 * @author jsanchez
 */
public class ForecastAshCloudsResource extends
        AbstractVizResource<ForecastAshCloudsResourceData, MapDescriptor> {

    private static final String FCST06 = "FCST06";

    private static final String FCST12 = "FCST12";

    private static final String FCST18 = "FCST18";

    private static final String AREA = "AREA";

    private final Map<VAARecord, IWireframeShape> shapeMap = new HashMap<>();

    private final Map<DataTime, Collection<VAARecord>> recordsToParse = new HashMap<>();

    protected ForecastAshCloudsResource(
            ForecastAshCloudsResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, false);
    }

    @Override
    public String getName() {
        return "VAA Forecast Ash Clouds";
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        StringBuilder returnValue = new StringBuilder();
        DataTime displayedDataTime = descriptor.getTimeForResource(this);
        if (displayedDataTime != null) {
            Collection<VAARecord> records = null;
            if (!this.recordsToParse.containsKey(displayedDataTime)) {
                return returnValue.toString();
            } else {
                records = this.recordsToParse.get(displayedDataTime);
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
                            point = pixelPoly.getFactory().createPoint(coord
                                    .asPixel(descriptor.getGridGeometry()));
                        } catch (Exception e) {
                            throw new VizException(
                                    "Error inspecting VAA Forecast Ash Clouds",
                                    e);
                        }

                        if (pixelPoly.contains(point)) {
                            String temp = "";
                            if (subPart.getSubText().equals(FCST06)) {
                                temp = record.getFcst06Hr() != null
                                        ? record.getFcst06Hr() : "";
                            } else if (subPart.getSubText().equals(FCST12)) {
                                temp = record.getFcst12Hr() != null
                                        ? record.getFcst12Hr() : "";
                            } else if (subPart.getSubText().equals(FCST18)) {
                                temp = record.getFcst18Hr() != null
                                        ? record.getFcst18Hr() : "";
                            }
                            if (returnValue.length() > 0) {
                                returnValue.append("\n \n");
                            }
                            returnValue.append(temp);
                        }
                    }
                }
            }
        }

        return returnValue.length() == 0 ? "NO DATA" : returnValue.toString();
    }

    /**
     * Adds a new record to this resource
     *
     * @param record
     */
    protected void addRecord(VAARecord record) {
        DataTime dataTime = record.getDataTime();
        Collection<VAARecord> toParse = recordsToParse.get(dataTime);
        if (toParse == null) {
            dataTimes.add(dataTime);
            toParse = new ArrayList<>();
            recordsToParse.put(dataTime, toParse);
        }
        toParse.add(record);
    }

    @Override
    public void remove(DataTime dataTime) {
        super.remove(dataTime);

        Collection<VAARecord> toParse = recordsToParse.remove(dataTime);
        if (toParse != null) {
            for (VAARecord record : toParse) {
                synchronized (shapeMap) {
                    IWireframeShape shape = shapeMap.remove(record);
                    if (shape != null) {
                        shape.dispose();
                    }
                }
            }
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime curDataTime = paintProps.getDataTime();
        if (curDataTime == null) {
            return;
        }

        Collection<VAARecord> toParse = recordsToParse.get(curDataTime);
        RGB color = getCapability(ColorableCapability.class).getColor();
        target.clearClippingPlane();
        if (toParse != null) {
            for (VAARecord record : toParse) {
                IWireframeShape shape;
                synchronized (shapeMap) {
                    shape = shapeMap.get(record);
                    if (shape == null) {
                        for (VAASubPart subPart : record.getSubParts()) {
                            String subText = subPart.getSubText();
                            if (subText != null && subText.startsWith("FCST")) {
                                if (shape == null) {
                                    // Only create it if we find a FCST subPart
                                    shape = target.createWireframeShape(false,
                                            descriptor);
                                    shapeMap.put(record, shape);
                                }
                                Coordinate[] coordinates = CommonUtil
                                        .getCoordinates(subPart.getLocations());
                                shape.addLineSegment(coordinates);
                            }
                        }
                    }
                }
                if (shape != null) {
                    target.drawWireframeShape(shape, color, 0.5f);
                }
            }
        }

        target.setupClippingPlane(paintProps.getView().getExtent());
    }

    @Override
    public void resourceDataChanged(ChangeType type, Object object) {
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

    private void disposeShapes() {
        synchronized (shapeMap) {
            for (IWireframeShape shape : shapeMap.values()) {
                shape.dispose();
            }
            shapeMap.clear();
        }
    }

    @Override
    protected void disposeInternal() {
        disposeShapes();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        disposeShapes();
        issueRefresh();
    }
}
