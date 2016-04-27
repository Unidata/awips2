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
package com.raytheon.viz.radar.rsc.graphic;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedContourVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedVector;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyBlock;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import com.raytheon.viz.radar.rsc.RadarResourceData;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Displays the melting layer as provided by radar (i.e. displays the levels at
 * which snow is turned to a mixture of snow and water and then in turn that is
 * changed to water only).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 16, 2010            mnash       Initial creation
 * Jul 13, 2103 2223       njensen     Overrode remove() to fix memory leak
 * Jul 28, 2013 2227       mnash       Fixing the projection issues, moving things
 *                                     around for better logical separation
 * Aug 14, 2014 3523       mapeters    Updated deprecated {@link DrawableString#textStyle} 
 *                                     assignments.
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarMLResource extends RadarGraphicsResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarMLResource.class);

    private Map<DataTime, Map<Integer, IWireframeShape>> shapes;

    private boolean refresh = false;

    private Map<Integer, LineStyle> style;

    private static final String USING_BEAM_EDGE = "Using Beam Edge";

    private static final String USING_BEAM_CENTER = "Using Beam Center";

    private static final int X_OFFSET = 300;

    private static final int Y_OFFSET = 50;

    private IFont textFont;

    /**
     * @param rrd
     * @param loadProps
     * @param interrogator
     * @throws VizException
     */
    public RadarMLResource(RadarResourceData rrd, LoadProperties loadProps,
            IRadarInterrogator interrogator) throws VizException {
        super(rrd, loadProps, interrogator);
        shapes = new HashMap<DataTime, Map<Integer, IWireframeShape>>();

        style = new HashMap<Integer, LineStyle>();
        style.put(1, LineStyle.DASHED);
        style.put(2, LineStyle.SOLID);
        style.put(3, LineStyle.SOLID);
        style.put(4, LineStyle.DASHED);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.AbstractRadarResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // initialize the font
        textFont = target.initializeFont(java.awt.Font.DIALOG, 11.0f,
                new IFont.Style[] {});
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.graphic.RadarGraphicsResource#paintInternal
     * (com.raytheon.uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        Map<Integer, IWireframeShape> shapeMap = buildShapeMap(
                paintProps.getDataTime(), target);
        if (shapeMap != null) {
            for (Integer num : shapeMap.keySet()) {
                LineStyle lineStyle = style.get(num);
                if (getCapability(OutlineCapability.class).getLineStyle() != LineStyle.DEFAULT) {
                    lineStyle = getCapability(OutlineCapability.class)
                            .getLineStyle();
                }
                target.drawWireframeShape(shapeMap.get(num),
                        getCapability(ColorableCapability.class).getColor(),
                        getCapability(OutlineCapability.class)
                                .getOutlineWidth(), lineStyle);
            }
        }

        // paint the legend for melting layer
        IExtent extent = paintProps.getView().getExtent();
        double ratio = extent.getWidth() / paintProps.getCanvasBounds().width;
        textFont.setMagnification((float) (getCapability(
                MagnificationCapability.class).getMagnification().floatValue() * 1.5));
        LineStyle dashedLine = LineStyle.DASHED;
        LineStyle solidLine = LineStyle.SOLID;
        if (getCapability(OutlineCapability.class).getLineStyle() != LineStyle.DEFAULT) {
            dashedLine = getCapability(OutlineCapability.class).getLineStyle();
            solidLine = getCapability(OutlineCapability.class).getLineStyle();
        }

        String[] text = new String[2];
        text[0] = USING_BEAM_EDGE;
        text[1] = USING_BEAM_CENTER;
        target.clearClippingPlane();

        DrawableLine line1 = new DrawableLine();
        line1.basics.color = getCapability(ColorableCapability.class)
                .getColor();
        line1.lineStyle = dashedLine;
        line1.width = getCapability(OutlineCapability.class).getOutlineWidth();
        line1.addPoint(extent.getMinX() + (X_OFFSET - X_OFFSET / 2.7) * ratio,
                extent.getMinY() + (Y_OFFSET - 5 + textFont.getFontSize())
                        * ratio);
        line1.addPoint(extent.getMinX() + (X_OFFSET - 10) * ratio,
                extent.getMinY() + (Y_OFFSET - 5 + textFont.getFontSize())
                        * ratio);

        DrawableLine line2 = new DrawableLine();
        line2.basics.color = getCapability(ColorableCapability.class)
                .getColor();
        line2.lineStyle = solidLine;
        line2.width = getCapability(OutlineCapability.class).getOutlineWidth();
        line2.addPoint(extent.getMinX() + (X_OFFSET - X_OFFSET / 2.7) * ratio,
                extent.getMinY() + (Y_OFFSET - 5 + textFont.getFontSize() * 2)
                        * ratio);
        line2.addPoint(extent.getMinX() + (X_OFFSET - 10) * ratio,
                extent.getMinY() + (Y_OFFSET - 5 + textFont.getFontSize() * 2)
                        * ratio);
        target.drawLine(line1, line2);

        RGB[] rgbs = new RGB[text.length];
        for (int i = 0; i < text.length; i++) {
            rgbs[i] = getCapability(ColorableCapability.class).getColor();
        }
        DrawableString info = new DrawableString(text, rgbs);
        info.font = textFont;
        info.horizontalAlignment = HorizontalAlignment.LEFT;
        info.verticalAlignment = VerticalAlignment.TOP;
        info.setCoordinates(extent.getMinX() + X_OFFSET * ratio,
                extent.getMinY() + Y_OFFSET * ratio);
        target.drawStrings(info);
        target.setupClippingPlane(extent);
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        disposeShapes();
        if (textFont != null) {
            textFont.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.graphic.RadarGraphicsResource#resourceChanged
     * (com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            refresh = true;
        }
        super.resourceChanged(type, object);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.graphic.RadarGraphicsResource#project(org.
     * opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        refresh = true;
        disposeShapes();
        issueRefresh();
    }

    @Override
    public void remove(DataTime dataTime) {
        synchronized (shapes) {
            disposeShapeMap(shapes.get(dataTime));
            shapes.remove(dataTime);
        }
        super.remove(dataTime);
    }

    private Coordinate rectifyCoordinate(Coordinate c) {
        c.x += 2048;
        c.y += 2048;

        c.y = 4096 - c.y;
        return c;
    }

    /**
     * Builds the shape map
     * 
     * @param time
     * @param target
     * @return
     */
    private Map<Integer, IWireframeShape> buildShapeMap(DataTime time,
            IGraphicsTarget target) {
        Map<Integer, IWireframeShape> shapeMap = null;
        synchronized (shapes) {
            shapeMap = shapes.get(time);
            if (time != null && (shapeMap == null || refresh)) {
                disposeShapeMap(shapeMap);
                shapeMap = new HashMap<Integer, IWireframeShape>();
                shapes.put(time, shapeMap);
                displayedDate = time;

                IWireframeShape ws = null;

                displayedLevel = displayedDate.getLevelValue().floatValue();

                // retrieve record if not yet populated
                RadarRecord radarRecord = getRadarRecord(displayedDate);
                if (radarRecord == null) {
                    return null;
                }

                File loc = HDF5Util.findHDF5Location(radarRecord);
                IDataStore dataStore = DataStoreFactory.getDataStore(loc);
                try {
                    RadarDataRetriever.populateRadarRecord(dataStore,
                            radarRecord);
                } catch (FileNotFoundException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                } catch (StorageException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }

                Map<Integer, Coordinate[]> coordinates = buildCoordinates(
                        radarRecord.getSymbologyBlock(), radarRecord.getCRS());

                // looping through the coordinates in order to create a
                // wireframe
                // shape
                for (Integer num : coordinates.keySet()) {
                    if (shapeMap.get(num) == null) {
                        ws = target.createWireframeShape(true, this.descriptor);
                    } else {
                        ws = shapeMap.get(num);
                    }
                    ws.addLineSegment(coordinates.get(num));
                    shapeMap.put(num, ws);
                }
            }
        }
        return shapeMap;
    }

    /**
     * Builds the necessary coordinates for use in the wireframe shapes
     * 
     * @param block
     * @param crs
     * @return
     */
    private Map<Integer, Coordinate[]> buildCoordinates(SymbologyBlock block,
            ProjectedCRS crs) {
        GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
        // Per section 3.3.3
        generalEnvelope.setCoordinateReferenceSystem(crs);
        generalEnvelope.setRange(0, -256000 * 2, 256000 * 2);
        generalEnvelope.setRange(1, -256000 * 2, 256000 * 2);
        // [-2048, 2048] == range of 4095 (inclusive 0), plus 1 because
        // GGR is exclusive (?)
        GeneralGridGeometry gg = new GeneralGridGeometry(
                new GeneralGridEnvelope(new int[] { 0, 0 }, new int[] { 4096,
                        4096 }, false), generalEnvelope);
        ReferencedCoordinate coordinate;
        Map<Integer, Coordinate[]> coordinates = new HashMap<Integer, Coordinate[]>();
        if (block != null) {
            for (int i = 0; i < block.getNumLayers(); i++) {
                for (int j = 1; j < block.getNumPackets(i); j++) {
                    if (block.getPacket(i, j) instanceof LinkedContourVectorPacket) {
                        List<LinkedVector> vector = ((LinkedContourVectorPacket) block
                                .getPacket(i, j)).getVectors();
                        Coordinate[] coords = new Coordinate[vector.size() + 1];
                        for (int l = 0; l < coords.length - 1; l++) {
                            if (!coordinates.containsKey(vector.get(l)
                                    .getTheColor())) {
                                coordinates.put(Integer.valueOf(vector.get(l)
                                        .getTheColor()), coords);
                            }

                            // transform the coordinates to the correct
                            // locations
                            coordinate = new ReferencedCoordinate(
                                    rectifyCoordinate(new Coordinate(vector
                                            .get(l).getI2(), vector.get(l)
                                            .getJ2())), gg, Type.GRID_CENTER);
                            try {
                                coords[l] = coordinate.asLatLon();
                            } catch (TransformException e1) {
                                statusHandler.handle(Priority.PROBLEM,
                                        e1.getLocalizedMessage(), e1);
                            } catch (FactoryException e1) {
                                statusHandler.handle(Priority.PROBLEM,
                                        e1.getLocalizedMessage(), e1);

                            }
                        }
                        coords[coords.length - 1] = coords[0];
                    }
                }
            }
            refresh = false;
        }
        return coordinates;
    }

    /**
     * Disposes of all shapes within the shapes map.
     */
    private void disposeShapes() {
        synchronized (shapes) {
            for (Map<Integer, IWireframeShape> shapeMap : shapes.values()) {
                disposeShapeMap(shapeMap);
            }
            shapes.clear();
        }
    }

    /**
     * Disposes of the internal shapes, passing in the shapes you want disposed.
     */
    private void disposeShapeMap(Map<Integer, IWireframeShape> shapeMap) {
        if (shapeMap != null) {
            for (IWireframeShape shape : shapeMap.values()) {
                shape.dispose();
            }
        }
    }
}
