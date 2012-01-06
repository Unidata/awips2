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
package com.raytheon.uf.viz.monitor.scan.resource;

import java.util.HashMap;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.cwat.CWATRecord;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.monitor.scan.ThreatLocation;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.cwat.CWATResource;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Draws the CWAT city and Site locations with threat inspection
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2009   2307         dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class CWATLocalThreatResource extends CWATResource {

    private int lineWidth = 0;

    private static final int BOX_HEIGHT = 10;

    private static final int BOX_WIDTH = 10;

    private RGB color;

    private double previousZoom = 0.0;

    private PaintProperties myPaintProps;

    protected HashMap<ThreatLocation, PixelCoverage> drawables = new HashMap<ThreatLocation, PixelCoverage>();

    public CWATLocalThreatResource(CWATLocalThreatResourceData data,
            LoadProperties props) {

        super(data, props);
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        this.displayedDataTime = paintProps.getDataTime();
        this.myPaintProps = paintProps;
        this.record = resourceData.dataObjectMap.get(this.displayedDataTime);

        this.lineWidth = this.getCapability(OutlineCapability.class)
                .getOutlineWidth();
        this.color = this.getCapability(ColorableCapability.class).getColor();

        if (record == null) {
            // Don't have data for this time
            return;
        }

        if (record.getDataArray() == null) {
            record = resourceData.populateRecord(record);
        }

        if (record.getThreats() != null && drawables != null) {

            drawables.clear();
            previousZoom = myPaintProps.getZoomLevel();
            this.previousDataTime = displayedDataTime;

            for (ThreatLocation loc : record.getThreats().keySet()) {
                drawables.put(loc, getPixelCoverage(loc));
            }

            if (record != null && drawables != null && target != null) {
                for (ThreatLocation loc : record.getThreats().keySet()) {
                    if (drawables.containsKey(loc)) {
                        drawSquare(loc, drawables.get(loc), target);
                    } else {
                        PixelCoverage pc = getPixelCoverage(loc);
                        drawables.put(loc, pc);
                        drawSquare(loc, pc, target);
                    }
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        CWATRecord rec = resourceData.dataObjectMap.get(this.displayedDataTime);
        if (rec == null) {
            return "No Data Available";
        }

        StringBuilder prefix = new StringBuilder();
        prefix.append(rec.getIcao());
        prefix.append(" ");
        prefix.append(CWATRecord.THREATS);

        return prefix.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IInspectableResource#inspect(com
     * .vividsolutions.jts.geom.Coordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate latLon) throws VizException {
        StringBuffer inspect = new StringBuffer();
        if (drawables.size() > 0 && record != null
                && record.getThreats() != null) {
            for (ThreatLocation loc : drawables.keySet()) {
                try {
                    if (contains(drawables.get(loc), latLon.asLatLon())) {
                        if (record.getDataArray() != null) {
                            inspect.append(loc.getLocationName());
                            inspect.append(":  "
                                    + record.getThreats().get(loc)
                                            .getTstormMessage() + "\n");
                            inspect.append(record.getThreats().get(loc)
                                    .getThreatMessage());
                        }
                    }
                } catch (TransformException e) {
                    e.printStackTrace();
                } catch (FactoryException e) {
                    e.printStackTrace();
                }
            }
        }

        return inspect.toString();
    }

    /**
     * Draw the square and whatever else it needs
     * 
     * @param loc
     * @param pc
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void drawSquare(ThreatLocation loc, PixelCoverage pc,
            IGraphicsTarget target) throws VizException {
        font.setMagnification(getCapability(MagnificationCapability.class)
                .getMagnification().floatValue());
        if (record.getThreats() != null && record.getThreats().containsKey(loc)) {
            if (pc != null) {
                if (!record.getThreats().get(loc).isThreat()) {
                    target.drawLine(pc.getLl().x, pc.getLl().y, 0.0,
                            pc.getUl().x, pc.getUl().y, 0.0, color, lineWidth,
                            LineStyle.SOLID);
                    target.drawLine(pc.getUl().x, pc.getUl().y, 0.0,
                            pc.getUr().x, pc.getUr().y, 0.0, color, lineWidth,
                            LineStyle.SOLID);
                    target.drawLine(pc.getUr().x, pc.getUr().y, 0.0,
                            pc.getLr().x, pc.getLr().y, 0.0, color, lineWidth,
                            LineStyle.SOLID);
                    target.drawLine(pc.getLr().x, pc.getLr().y, 0.0,
                            pc.getLl().x, pc.getLl().y, 0.0, color, lineWidth,
                            LineStyle.SOLID);
                }
                // System.out.p
                if (record.getThreats().get(loc) != null) {
                    if (record.getThreats().get(loc).isThreat()) {
                        double[] center = descriptor.worldToPixel(new double[] {
                                loc.getLon(), loc.getLat() });
                        target.drawString(font, loc.getLocationName(),
                                center[0], center[1], 0.0, TextStyle.BOXED,
                                color, HorizontalAlignment.CENTER,
                                VerticalAlignment.MIDDLE, 0.0);
                    }
                }
            }
        }
    }

    /**
     * Set the width scalar
     * 
     * @param props
     * @return
     */
    private double getScaleWidth(ThreatLocation loc) {
        double screenToWorldWidthRatio = myPaintProps.getCanvasBounds().width
                / myPaintProps.getView().getExtent().getWidth();

        if (record.getThreats().get(loc).isThreat()) {
            return (BOX_WIDTH * 4 / 2.0) / screenToWorldWidthRatio;
        } else {
            return (BOX_WIDTH / 3.0) / screenToWorldWidthRatio;
        }

    }

    /**
     * Set the height scalar
     * 
     * @param props
     * @return
     */
    private double getScaleHeight(ThreatLocation loc) {
        double screenToWorldHeightRatio = myPaintProps.getCanvasBounds().height
                / myPaintProps.getView().getExtent().getHeight();
        if (record.getThreats().get(loc).isThreat()) {
            return (BOX_HEIGHT * 1.5 / 2.0) / screenToWorldHeightRatio;
        } else {
            return (BOX_HEIGHT / 3.0) / screenToWorldHeightRatio;
        }
    }

    /**
     * gets the pixel coverage for this drawable
     * 
     * @return
     */
    private PixelCoverage getPixelCoverage(ThreatLocation loc) {

        double wscale = getScaleWidth(loc);
        double hscale = getScaleHeight(loc);
        double[] center = descriptor.worldToPixel(new double[] { loc.getLon(),
                loc.getLat() });

        Coordinate ul = new Coordinate(center[0] - wscale, center[1] - hscale);
        Coordinate ur = new Coordinate(center[0] + wscale, center[1] - hscale);
        Coordinate lr = new Coordinate(center[0] + wscale, center[1] + hscale);
        Coordinate ll = new Coordinate(center[0] - wscale, center[1] + hscale);

        return new PixelCoverage(ul, ur, lr, ll);
    }

    /**
     * See if you are in the coverage of this feature
     * 
     * @param c
     * @return
     */
    private boolean contains(PixelCoverage pc, Coordinate c) {
        boolean inside = false;

        double[] center = descriptor.worldToPixel(new double[] { c.x, c.y });

        if (center[0] > pc.getMinX() && center[0] < pc.getMaxX()
                && center[1] > pc.getMinY() && center[1] < pc.getMaxY()) {
            inside = true;
        }
        return inside;
    }

}
