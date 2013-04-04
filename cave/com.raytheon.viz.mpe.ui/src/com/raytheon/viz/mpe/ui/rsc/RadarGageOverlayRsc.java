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
package com.raytheon.viz.mpe.ui.rsc;

import java.awt.geom.Rectangle2D;
import java.util.Date;
import java.util.Hashtable;
import java.util.List;
import java.util.ListIterator;

import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEDataManager.MPEGageData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Single Site Radar Gage Overlay Resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 8, 2009  3232       mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RadarGageOverlayRsc extends
        AbstractVizResource<RadarGageOverlayRscData, IDescriptor> {
    /** The MPE Gage Data object */
    private MPEGageData gageData = null;

    /** Map of location to Gage Data Object */
    private Hashtable<Coordinate, MPEGageData> dataMap = null;

    /** The current date */
    private Date currentdate = null;

    private IWireframeShape shape;

    public RadarGageOverlayRsc(RadarGageOverlayRscData radarGageOverlayRscData,
            LoadProperties loadProperties) {
        super(radarGageOverlayRscData, loadProperties);
        currentdate = MPEDisplayManager.getCurrent().getCurrentEditDate();
        addPoints(getGages(currentdate));
        getCapability(ColorableCapability.class).setColor(
                getResourceData().getColor());
        // getGages(currentdate);
    }

    /**
     * Get the Gage list to display
     * 
     * @param currentdate
     *            The current data time for display
     */
    public List<MPEGageData> getGages(Date currentdate) {
        List<MPEGageData> gage = MPEDataManager.getInstance().readGageData(
                currentdate);
        return gage;
    }

    /**
     * Add a point to this resource.
     * 
     * @param x
     *            The point's x coordinate
     * @param y
     *            The point's y coordinate
     * @param inspectString
     *            String to display when inspection is enabled
     * @param color
     *            The point's color
     */
    public void addPoints(List<MPEGageData> gages) {
        gageData = new MPEDataManager.MPEGageData();
        dataMap = new Hashtable<Coordinate, MPEGageData>();

        if (!gages.isEmpty()) {
            for (ListIterator<MPEGageData> it = gages.listIterator(); it
                    .hasNext();) {
                gageData = it.next();
                Coordinate xy = gageData.getLatLon();
                dataMap.put(xy, gageData);
            }
        }
    }

    /**
     * Paint method called to display this resource.
     * 
     * @param target
     *            The IGraphicsTarget
     * @param paintProps
     *            The Paint Properties
     * @throws VizException
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (getProperties().isVisible()) {
            if (shape == null) {
                IFont font = target.initializeFont(target.getDefaultFont()
                        .getFontName(), 9, null);

                DrawableString string = new DrawableString("+", null);
                string.font = font;
                Rectangle2D stringBounds = target.getStringsBounds(string);
                double strikeLength = (stringBounds.getWidth() + stringBounds
                        .getX());
                int halfStrikeLength = (int) (strikeLength / 2.0);

                shape = target.createWireframeShape(false, descriptor);

                for (Coordinate c : dataMap.keySet()) {
                    double[] v = descriptor.worldToPixel(new double[] { c.x,
                            c.y });

                    if (paintProps.getView().getExtent().contains(v)) {
                        double[][] line1 = new double[2][3];
                        double[][] line2 = new double[2][3];

                        double[] loc = getResourceContainer()
                                .translateInverseClick(c);
                        loc[0] = (int) loc[0];
                        loc[1] = (int) loc[1];

                        double[] tmp = descriptor.getRenderableDisplay()
                                .screenToGrid(loc[0] - halfStrikeLength + .5,
                                        loc[1] + .5, 0.0, target);
                        line1[0][0] = tmp[0];
                        line1[0][1] = tmp[1];
                        line1[0][2] = 0.0;

                        tmp = descriptor.getRenderableDisplay().screenToGrid(
                                loc[0] + halfStrikeLength + 1.5, loc[1] + .5,
                                0.0, target);
                        line1[1][0] = tmp[0];
                        line1[1][1] = tmp[1];
                        line1[1][2] = 0.0;

                        tmp = descriptor.getRenderableDisplay().screenToGrid(
                                loc[0] + .5, loc[1] - halfStrikeLength + .5,
                                0.0, target);
                        line2[0][0] = tmp[0];
                        line2[0][1] = tmp[1];
                        line2[0][2] = 0.0;

                        tmp = descriptor.getRenderableDisplay().screenToGrid(
                                loc[0] + .5, loc[1] + halfStrikeLength + 1.5,
                                0.0, target);
                        line2[1][0] = tmp[0];
                        line2[1][1] = tmp[1];
                        line2[1][2] = 0.0;

                        shape.addLineSegment(line1);
                        shape.addLineSegment(line2);
                    }
                }

                shape.compile();
                font.dispose();
            }

            target.drawWireframeShape(shape,
                    getCapability(ColorableCapability.class).getColor(),
                    getCapability(OutlineCapability.class).getOutlineWidth());
        }
    }

    @Override
    protected void disposeInternal() {
        if (shape != null) {
            shape.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

    }

    @Override
    public String getName() {
        return getResourceData().getName();
    }

}
