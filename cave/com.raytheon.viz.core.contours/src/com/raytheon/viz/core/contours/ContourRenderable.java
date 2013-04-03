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
package com.raytheon.viz.core.contours;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.jts.JTS;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.edex.meteoLib.Controller;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.viz.core.contours.ContourSupport.ContourGroup;
import com.raytheon.viz.core.style.contour.ContourPreferences;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Generalized contour renderable
 * 
 * May be embedded in other renderable displays or form the basis of contour
 * resources
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 10, 2008	#1233	    chammack	Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public abstract class ContourRenderable implements IRenderable {

    private ContourGroup[] contourGroup;

    private Map<String, ContourCreateRequest> requestMap;

    private final IMapDescriptor descriptor;

    private LineStyle lineStyle;

    private RGB color;

    private double density = 1.0;

    private double magnification = 1.0;

    private int outlineWidth;

    private final String uuid;

    // This is the width of CONUS
    private static final double METERS_AT_BASE_ZOOMLEVEL = 5878649.0;

    private static final double BASE_CANVAS_SIZE = 1000.0;

    private static final double ZOOM_REACTION_FACTOR = .45;

    private static final int NUMBER_CONTOURING_LEVELS = 10;

    public abstract IDataRecord[] getData() throws VizException;

    public abstract GeneralGridGeometry getGridGeometry() throws VizException;

    public abstract ContourPreferences getPreferences() throws VizException;

    private IFont font;

    private IFont minMaxFont;

    /**
     * Constructor
     * 
     * @param styleRule
     * @param descriptor
     * @param callback
     * @param lineStyle
     */
    public ContourRenderable(IMapDescriptor descriptor) {

        this.descriptor = descriptor;
        uuid = UUID.randomUUID().toString();
        this.requestMap = new HashMap<String, ContourCreateRequest>();
    }

    /**
     * Set color
     * 
     * @param color
     *            the color to set
     */
    public void setColor(RGB color) {
        this.color = color;
    }

    /**
     * @param density
     *            the density to set
     */
    public void setDensity(double density) {
        if (density > 4.0f) {
            density = 4.0f;
        }
        this.density = density;
    }

    /**
     * @param magnification
     *            the magnification to set
     */
    public void setMagnification(double magnification) {
        if (this.magnification != magnification) {
            if (font != null) {
                font.dispose();
                font = null;
            }
            if (minMaxFont != null) {
                minMaxFont.dispose();
                minMaxFont = null;
            }
        }
        this.magnification = magnification;
    }

    /**
     * Set outline width
     * 
     * @param outlineWidth
     *            the outlineWidth to set
     */
    public void setOutlineWidth(int outlineWidth) {
        this.outlineWidth = outlineWidth;
    }

    /**
     * Set the line style
     * 
     * @param lineStyle
     *            the lineStyle to set
     */
    public void setLineStyle(LineStyle lineStyle) {
        this.lineStyle = lineStyle;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        synchronized (this) {
            if (this.density == 0) {
                return;
            }

            if (contourGroup == null) {
                contourGroup = new ContourGroup[NUMBER_CONTOURING_LEVELS];
            }

            if (contourGroup != null) {
                if (font == null) {
                    font = target.getDefaultFont();

                    font = target.initializeFont(font.getFontName(),
                            (float) (font.getFontSize() / 1.4 * magnification),
                            null);
                }

                if (minMaxFont == null) {
                    minMaxFont = target.getDefaultFont();

                    minMaxFont = target
                            .initializeFont(
                                    minMaxFont.getFontName(),
                                    (float) (minMaxFont.getFontSize() / 1.3 * magnification),
                                    new Style[] { Style.BOLD });
                }

                // To convert from i to zoomLevel, take i + 1.0 / 2 so
                // contourGroup[0] is at zoomLevel 0.5, contourGroup[1] is at
                // zoomLevel 1, etc.
                int i = contourGroup.length - 1;
                double mapWidth = ((MapDescriptor) this.descriptor)
                        .getMapWidth();
                double widthInMeters = mapWidth * paintProps.getZoomLevel();
                while (i >= 0) {
                    double curlvl = (METERS_AT_BASE_ZOOMLEVEL)
                            * (Math.pow(ZOOM_REACTION_FACTOR, (i - 1.0) / 2));
                    curlvl *= paintProps.getCanvasBounds().width
                            / BASE_CANVAS_SIZE;
                    if (widthInMeters <= curlvl || i == 0) {
                        boolean contains = (contourGroup[i] == null || contourGroup[i].lastUsedPixelExtent == null) ? false
                                : (contourGroup[i].lastUsedPixelExtent
                                        .getEnvelope()
                                        .contains(((PixelExtent) paintProps
                                                .getView().getExtent())
                                                .getEnvelope()));
                        // calculate the pixel density
                        float pixelDensity = (float) (paintProps
                                .getCanvasBounds().width / paintProps.getView()
                                .getExtent().getWidth());

                        double pdRatio = contourGroup[i] == null ? 1
                                : pixelDensity
                                        / contourGroup[i].lastPixelDensity;
                        // Run contours if:
                        // 1. Contours was never ran before -or-
                        // 2. The area currently viewed is outside of the
                        // contoured window -or-
                        // 3. The density has changed
                        // 4. The pixel density has changed significantly
                        if (contourGroup[i] == null || !contains
                                || contourGroup[i].lastDensity != density
                                || pdRatio > 2 || pdRatio < 0.5) {

                            IDataRecord[] dataRecord = getData();
                            if (dataRecord == null) {
                                return;
                            }

                            GeneralGridGeometry gridGeometry = getGridGeometry();
                            ContourPreferences contourPrefs = getPreferences();

                            // If required data unavailable, quit now
                            if (gridGeometry == null) {
                                return;
                            }

                            dataRecord = smoothData(dataRecord, gridGeometry,
                                    contourPrefs);

                            ContourGroup cg = null;
                            // generate the identifier
                            String identifier = uuid + "::" + this + "::" + i;
                            // grab the existing request
                            ContourCreateRequest existingRequest = requestMap
                                    .get(identifier);
                            // create a new request
                            ContourCreateRequest request = new ContourCreateRequest(
                                    identifier, dataRecord, (i + 1) / 2.0f,
                                    paintProps.getView().getExtent(), density,
                                    magnification, gridGeometry, target,
                                    descriptor, contourPrefs, pixelDensity);

                            if (existingRequest != null) {
                                // check if new request needs to replace old
                                if (!(request.equals(existingRequest))) {
                                    // cancel old request
                                    existingRequest.setCanceled(true);
                                    // add new request
                                    requestMap.put(identifier, request);
                                    // send request
                                    ContourManagerJob.getInstance().request(
                                            request);
                                }
                            } else {
                                // there is no exiting request, insert new one
                                requestMap.put(identifier, request);
                                ContourManagerJob.getInstance()
                                        .request(request);
                            }

                            int retries = 0;
                            do {
                                // grab request from map
                                request = requestMap.get(identifier);
                                cg = request.getContourGroup();
                                try {
                                    if (cg == null
                                            && paintProps.getLoopProperties() != null
                                            && paintProps.getLoopProperties()
                                                    .isLooping()) {
                                        Thread.sleep(50);
                                    }
                                } catch (InterruptedException e) {
                                    // ignore
                                }
                                retries++;
                            } while (cg == null
                                    && paintProps.getLoopProperties() != null
                                    && paintProps.getLoopProperties()
                                            .isLooping() && retries < 10);

                            if (cg != null) {
                                if (cg != contourGroup[i]) {
                                    // Dispose old wireframe shapes
                                    if (contourGroup[i] != null
                                            && contourGroup[i].posValueShape != null) {
                                        contourGroup[i].posValueShape.dispose();
                                    }

                                    if (contourGroup[i] != null
                                            && contourGroup[i].negValueShape != null) {
                                        contourGroup[i].negValueShape.dispose();
                                    }

                                    contourGroup[i] = cg;
                                    contourGroup[i].posValueShape.compile();
                                    contourGroup[i].negValueShape.compile();
                                }
                            } else {
                                target.setNeedsRefresh(true);
                            }

                        }

                        if (contourGroup[i] != null
                                && paintProps
                                        .getView()
                                        .getExtent()
                                        .intersects(
                                                contourGroup[i].lastUsedPixelExtent)) {
                            // System.out.println("Painting group at " + i);
                            drawContourGroup(target, contourGroup[i]);
                        } else {
                            // see if we can display a higher level
                            if (i > 0) {
                                int j = i - 1;
                                if (contourGroup[j] != null) {
                                    if (contourGroup[j].posValueShape != null) {
                                        if (contourGroup[j].lastUsedPixelExtent
                                                .intersects(paintProps
                                                        .getView().getExtent())) {
                                            drawContourGroup(target,
                                                    contourGroup[j]);
                                        }
                                    }
                                }
                            }

                            target.setNeedsRefresh(true);

                        }
                        break;

                    }
                    i--;
                }
            }
        }
    }

    private IDataRecord[] smoothData(IDataRecord[] dataRecord,
            GeneralGridGeometry gridGeometry, ContourPreferences contourPrefs)
            throws VizException {
        if (contourPrefs != null && contourPrefs.getSmoothingDistance() != null) {
            // Calculate the Diagnol Distance of the Grid In Meters.
            DirectPosition2D upperCorner = new DirectPosition2D(gridGeometry
                    .getEnvelope().getUpperCorner());
            DirectPosition2D lowerCorner = new DirectPosition2D(gridGeometry
                    .getEnvelope().getLowerCorner());
            double distanceInM;
            try {
                MathTransform crs2ll = MapUtil
                        .getTransformToLatLon(gridGeometry
                                .getCoordinateReferenceSystem());
                crs2ll.transform(upperCorner, upperCorner);
                crs2ll.transform(lowerCorner, lowerCorner);
                upperCorner.x = MapUtil.correctLon(upperCorner.x);
                lowerCorner.x = MapUtil.correctLon(lowerCorner.x);
                distanceInM = JTS
                        .orthodromicDistance(
                                new Coordinate(lowerCorner.getOrdinate(0),
                                        lowerCorner.getOrdinate(1)),
                                new Coordinate(upperCorner.getOrdinate(0),
                                        upperCorner.getOrdinate(1)), MapUtil
                                        .getLatLonProjection());
            } catch (Exception e) {
                throw new VizException(e);
            }
            // Calculate the Dagnol Distance in Points
            FloatDataRecord rec = (FloatDataRecord) dataRecord[0];
            float[] data = rec.getFloatData();
            int nx = (int) rec.getSizes()[0];
            int ny = (int) rec.getSizes()[1];
            double distanceInPoints = Math.sqrt(nx * nx + ny * ny);
            // Determine the number of points to smooth, assume
            // smoothingDistance is in km
            float npts = (float) (distanceInPoints
                    * contourPrefs.getSmoothingDistance() / (distanceInM / 1000));
            // Replace our NaN with their NaN
            for (int j = 0; j < data.length; j++) {
                if (data[j] == -999999) {
                    data[j] = 1.0E37f;
                }
            }
            data = Controller.dist_filter(data, npts, nx, 0, 0, nx, ny);
            // Replace their NaN with our NaN
            for (int j = 0; j < data.length; j++) {
                if (data[j] == 1.0E37f) {
                    data[j] = -999999;
                }
            }
            rec = new FloatDataRecord(rec.getName(), rec.getGroup(), data,
                    rec.getDimension(), rec.getSizes());
            return new IDataRecord[] { rec };
        } else {
            return dataRecord;
        }
    }

    /**
     * Dispose the renderable
     */
    public void dispose() {
        if (contourGroup != null) {
            for (ContourGroup c : contourGroup) {
                if (c == null) {
                    continue;
                }

                if (c.posValueShape != null) {
                    c.posValueShape.dispose();
                }
                if (c.negValueShape != null) {
                    c.negValueShape.dispose();
                }
            }
        }
        Set<String> keys = requestMap.keySet();
        for (String key : keys) {
            requestMap.get(key).dispose();
        }
        requestMap.clear();

        if (font != null) {
            font.dispose();
            font = null;
        }
        if (minMaxFont != null) {
            minMaxFont.dispose();
            minMaxFont = null;
        }
    }

    private void drawContourGroup(IGraphicsTarget target,
            ContourGroup contourGroup) throws VizException {

        LineStyle posLineStyle = null;
        LineStyle negLineStyle = null;

        ContourPreferences prefs = getPreferences();

        if (prefs != null
                && prefs.getPositiveLinePattern() != null
                && (this.lineStyle == null || this.lineStyle == LineStyle.DEFAULT)) {
            posLineStyle = LineStyle.valueOf(prefs.getPositiveLinePattern());
        } else if (this.lineStyle == null
                || this.lineStyle == LineStyle.DEFAULT) {
            posLineStyle = LineStyle.SOLID;
        } else {
            posLineStyle = this.lineStyle;
        }

        if (prefs != null
                && prefs.getNegativeLinePattern() != null
                && (this.lineStyle == null || this.lineStyle == LineStyle.DEFAULT)) {
            negLineStyle = LineStyle.valueOf(prefs.getNegativeLinePattern());
        } else if (this.lineStyle == null
                || this.lineStyle == LineStyle.DEFAULT) {
            negLineStyle = LineStyle.DASHED_LARGE;
        } else {
            negLineStyle = this.lineStyle;
        }

        // if (this.lineStyle == null || this.lineStyle == LineStyle.DEFAULT) {
        // posLineStyle = LineStyle.SOLID;
        // negLineStyle = LineStyle.DASHED_LARGE;
        // } else {
        // posLineStyle = this.lineStyle;
        // negLineStyle = this.lineStyle;
        // }

        contourGroup.drawContours(target, this.color, this.outlineWidth,
                posLineStyle, negLineStyle, font, minMaxFont);
    }

}
