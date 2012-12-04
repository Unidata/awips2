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

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import org.apache.commons.collections.map.LRUMap;
import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.edex.meteoLib.Controller;
import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.util.WorldWrapChecker;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.style.LabelingPreferences;
import com.raytheon.viz.core.contours.cache.SubGridCacheKey;
import com.raytheon.viz.core.contours.util.ContourContainer;
import com.raytheon.viz.core.contours.util.FortConBuf;
import com.raytheon.viz.core.contours.util.FortConConfig;
import com.raytheon.viz.core.interval.XFormFunctions;
import com.raytheon.viz.core.style.contour.ContourPreferences;
import com.vividsolutions.jts.geom.Geometry;

/**
 * ContourSupport
 * 
 * Provides contouring wrapper
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Oct 22, 2007             chammack    Initial Creation.
 *    May 26, 2009 #2172       chammack    Use zoomLevel to calculate label spacing
 *    Apr 26, 2010 #4583       rjpeter     Replaced fortran fortconbuf with java port.
 *    Mar 4, 2011   #7747       njensen   Cached subgrid envelopes
 *    Jul 9, 2012  DR 14940    M. Porricelli  Adjust arrow size for streamlines
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ContourSupport {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ContourSupport.class);

    private static float smallestContourValue = Util.GRID_FILL_VALUE - 1;

    private static float largestContourValue = Util.GRID_FILL_VALUE + 1;

    private static LRUMap subgridCache = new LRUMap(10);

    private ContourSupport() {
        // No constructor
    }

    /**
     * Data structure for contouring
     */
    public static class ContourGroup {
        public double zoomLevel;

        public IWireframeShape posValueShape;

        public IWireframeShape negValueShape;

        public ContourGroup parent;

        public PixelExtent lastUsedPixelExtent;

        public double lastDensity;

        public double lastPixelDensity;

        public Geometry contourGeometry;

        public String minMark;

        public String[] minVals;

        public double[][] minLabelPoints;

        public String maxMark;

        public String[] maxVals;

        public double[][] maxLabelPoints;

        public void drawContours(IGraphicsTarget target, RGB color,
                float lineWidth, LineStyle posLineStyle,
                LineStyle negLineStyle, IFont contourLabelFont,
                IFont minMaxLabelFont) throws VizException {
            target.drawWireframeShape(posValueShape, color, lineWidth,
                    posLineStyle, contourLabelFont);
            target.drawWireframeShape(negValueShape, color, lineWidth,
                    negLineStyle, contourLabelFont);
            drawLabels(target, minMaxLabelFont, color, maxLabelPoints, maxVals,
                    maxMark);
            drawLabels(target, minMaxLabelFont, color, minLabelPoints, minVals,
                    minMark);
        }

        private void drawLabels(IGraphicsTarget target, IFont labelFont,
                RGB color, double[][] labelPoints, String[] vals, String mark)
                throws VizException {
            if (labelPoints != null) {
                int size = labelPoints.length;
                boolean isMark = mark != null;
                boolean isVal = vals != null;
                VerticalAlignment markVert = VerticalAlignment.MIDDLE;
                VerticalAlignment valVert = VerticalAlignment.MIDDLE;
                if (isMark && isVal) {
                    markVert = VerticalAlignment.BOTTOM;
                    valVert = VerticalAlignment.TOP;
                }
                List<DrawableString> strings = new ArrayList<DrawableString>(
                        size);
                for (int i = 0; i < size; i++) {
                    double point[] = labelPoints[i];
                    if (isMark) {
                        DrawableString string = new DrawableString(mark, color);
                        string.font = labelFont;
                        string.setCoordinates(point[0], point[1], 1.0);
                        string.horizontalAlignment = HorizontalAlignment.CENTER;
                        string.verticallAlignment = markVert;
                        strings.add(string);
                    }
                    if (isVal) {
                        DrawableString string = new DrawableString(vals[i],
                                color);
                        string.font = labelFont;
                        string.setCoordinates(point[0], point[1], 1.0);
                        string.horizontalAlignment = HorizontalAlignment.CENTER;
                        string.verticallAlignment = valVert;
                        strings.add(string);
                    }
                }
                // draw all strings in a bulk operation for better performance
                target.drawStrings(strings);
            }
        }

    }

    /**
     * Create contours from provided parameters
     * 
     * @param records
     * @param level
     * @param extent
     * @param currentDensity
     * @param worldGridToCRSTransform
     * @param imageGridGeometry
     * @param mapGridGeometry
     * @param target
     * @param descriptor
     * @param prefs
     * @param zoom
     * @return the ContourGroup
     * @throws VizException
     */
    public static ContourGroup createContours(IDataRecord[] records,
            float level, IExtent extent, double currentDensity,
            double currentMagnification, GeneralGridGeometry imageGridGeometry,
            IGraphicsTarget target, IMapDescriptor descriptor,
            ContourPreferences prefs, float zoom) throws VizException {

        ContourGroup contourGroup = new ContourGroup();
        contourGroup.lastDensity = currentDensity;
        contourGroup.lastPixelDensity = zoom;
        contourGroup.posValueShape = target.createWireframeShape(false,
                descriptor);
        contourGroup.negValueShape = target.createWireframeShape(false,
                descriptor);
        contourGroup.zoomLevel = level;

        // Copy the pixel extent (deep copy required!)
        // expand by 50% to cover the subgrid expansion
        PixelExtent workingExtent = (PixelExtent) extent.clone();
        workingExtent.getEnvelope().expandBy(workingExtent.getWidth() * .5,
                workingExtent.getHeight() * .5);

        // Recontour whenever outside by more than 25%
        contourGroup.lastUsedPixelExtent = (PixelExtent) extent.clone();
        contourGroup.lastUsedPixelExtent.getEnvelope().expandBy(
                contourGroup.lastUsedPixelExtent.getWidth() * .25,
                contourGroup.lastUsedPixelExtent.getHeight() * .25);

        GeneralGridGeometry mapGridGeometry = descriptor.getGridGeometry();

        // Step 0: Set up necessary math transforms
        MathTransform rastPosToWorldGrid = null;
        MathTransform rastPosToLatLon = null;
        try {
            DefaultMathTransformFactory factory = new DefaultMathTransformFactory();

            CoordinateReferenceSystem rastCrs = imageGridGeometry
                    .getCoordinateReferenceSystem();
            CoordinateReferenceSystem mapCrs = mapGridGeometry
                    .getCoordinateReferenceSystem();

            MathTransform rastGridToCrs = imageGridGeometry
                    .getGridToCRS(PixelInCell.CELL_CENTER);
            MathTransform mapCrsToGrid = mapGridGeometry.getGridToCRS(
                    PixelInCell.CELL_CENTER).inverse();

            MathTransform rastCrsToLatLon = MapUtil
                    .getTransformToLatLon(rastCrs);

            MathTransform crs2crs = CRSCache.getInstance().findMathTransform(
                    rastCrs, mapCrs);

            rastPosToWorldGrid = factory
                    .createConcatenatedTransform(
                            factory.createConcatenatedTransform(rastGridToCrs,
                                    crs2crs), mapCrsToGrid);

            rastPosToLatLon = factory.createConcatenatedTransform(
                    rastGridToCrs, rastCrsToLatLon);
        } catch (Exception e) {
            throw new VizException("Error building Transforms", e);
        }

        // Step 1:First determine the subgrid to contour
        long tsg0 = System.currentTimeMillis();
        SubGridCacheKey key = new SubGridCacheKey(workingExtent,
                mapGridGeometry, imageGridGeometry);
        GeneralEnvelope env = (GeneralEnvelope) subgridCache.get(key);
        if (env == null) {
            env = calculateSubGrid(workingExtent, mapGridGeometry,
                    imageGridGeometry);
            subgridCache.put(key, env);
        }
        long tsg1 = System.currentTimeMillis();
        System.out.println("calculate sub grid time: " + (tsg1 - tsg0));

        // Step 3: Get the actual data

        if (records.length == 1 && records[0] != null) {
            IDataRecord record = records[0];
            float[] data1D = null;
            long[] sz = record.getSizes();

            if (record instanceof ByteDataRecord) {

                byte[] data1Db = ((ByteDataRecord) record).getByteData();
                data1D = new float[data1Db.length];
                for (int i = 0; i < data1D.length; i++) {
                    data1D[i] = data1Db[i] & 0xFF;
                }
            } else if (record instanceof FloatDataRecord) {
                data1D = ((FloatDataRecord) record).getFloatData();
            } else {
                throw new UnsupportedOperationException(
                        "Contouring is not supported for data type: "
                                + record.getClass().getName());
            }

            // Step 4: Determine the subgrid, if any

            int minX = (int) Math.floor(Math.max(env.getMinimum(0), 0));
            int minY = (int) Math.floor(Math.max(env.getMinimum(1), 0));
            int maxX = (int) Math.ceil(Math.min(env.getMaximum(0), sz[0] - 1));
            int maxY = (int) Math.ceil(Math.min(env.getMaximum(1), sz[1] - 1));

            int szX = (maxX - minX) + 1;
            int szY = (maxY - minY) + 1;
            if (szX * szY <= 0) {
                return contourGroup;
            }

            float[][] subgriddedData = new float[szX][szY];

            for (int j = 0; j < szY; j++) {
                for (int i = 0; i < szX; i++) {
                    float val = data1D[((int) sz[0] * (j + minY)) + (i + minX)];
                    if (Float.isNaN(val)) {
                        val = Util.GRID_FILL_VALUE;
                    }
                    subgriddedData[i][j] = val;
                }
            }

            // Use ported legacy code to determine contour interval
            ContourContainer contours = null;
            FortConConfig config = new FortConConfig();
            long t0 = System.currentTimeMillis();
            String minMaxLabelFormat = null;
            String minLabel = null;
            String maxLabel = null;
            String labelFormat = null;
            int labelTrimLeft = 0;
            int maxMinTrimLeft = 0;
            config.badlo = smallestContourValue;
            config.badhi = largestContourValue;
            // The +/- 0.5 is necessary to match the contouring location on
            // Awips 1
            config.xOffset = minX;
            config.yOffset = minY;
            config.labelSpacingLine = subgriddedData.length / 3;
            if (config.labelSpacingLine < 1) {
                config.labelSpacingLine = 1;
            }

            config.labelSpacingOverall = (int) (subgriddedData.length * 60
                    * currentMagnification / ((PixelExtent) extent).getWidth() + 0.5);

            // If nothing provided, attempt to get approximately 50 contours
            if (prefs == null || prefs.getContourLabeling() == null) {
                // TODO this is fairly inefficient to do every time.
                float min = Float.POSITIVE_INFINITY;
                float max = Float.NEGATIVE_INFINITY;
                for (float f : data1D) {
                    if (f != Util.GRID_FILL_VALUE) {
                        min = Math.min(min, f);
                        max = Math.max(max, f);
                    }
                }
                float interval = XFormFunctions
                        .newDataIntervalFromZoom((max - min) / 50,
                                (float) (contourGroup.lastDensity * zoom),
                                true, "", 10);
                config.seed = new float[] { interval };
                config.mode = -50;
                contours = FortConBuf.contour(subgriddedData, config);
            } else {
                LabelingPreferences contourLabeling = prefs
                        .getContourLabeling();
                if (contourLabeling.getLabelSpacing() > 0) {
                    config.labelSpacingLine = subgriddedData.length
                            / contourLabeling.getLabelSpacing();
                    if (config.labelSpacingLine < 1) {
                        config.labelSpacingLine = 1;
                    }
                }

                // setup min/max processing
                minMaxLabelFormat = contourLabeling.getMinMaxLabelFormat();
                minLabel = contourLabeling.getMinLabel();
                maxLabel = contourLabeling.getMaxLabel();
                labelFormat = contourLabeling.getLabelFormat();
                labelTrimLeft = contourLabeling.getLabelTrimLeft();
                maxMinTrimLeft = contourLabeling.getMaxMinTrimLeft();
                config.labelFormat = labelFormat;
                config.generateMins = minLabel != null
                        && (!minLabel.equals(".") || (minMaxLabelFormat != null && minLabel
                                .equals(".")));
                config.generateMaxes = maxLabel != null
                        && (!maxLabel.equals(".") || (minMaxLabelFormat != null && maxLabel
                                .equals(".")));

                config.minMaxRadius = (int) (currentMagnification * 3 * szX / 90);
                if (config.minMaxRadius < 1) {
                    config.minMaxRadius = 1;
                }
                if (config.minMaxRadius > 7) {
                    config.minMaxRadius = 7;
                }

                if (prefs.getContourLabeling().getValues() == null
                        && prefs.getContourLabeling().getIncrement() > 0.0) {
                    // interval provided
                    float initialInterval = prefs.getContourLabeling()
                            .getIncrement();
                    float interval;
                    if (contourLabeling.getNumberOfContours() > 0) {
                        float minData = 1e37f;
                        float maxData = -1e37f;
                        for (float[] dataRow : subgriddedData)
                            for (float data : dataRow)
                                if (data < minData && data != -999999)
                                    minData = data;
                                else if (data < 999998 && data > maxData)
                                    maxData = data;
                        interval = (maxData - minData)
                                / contourLabeling.getNumberOfContours();
                        if (interval < 0)
                            interval = -interval;
                        if (interval > initialInterval) {
                            initialInterval = interval;
                            prefs.getContourLabeling().setIncrement(
                                    initialInterval);
                        }

                        // removed +1 to fix EPTC plan view and T_std
                        interval = XFormFunctions.newDataIntervalFromZoom(
                                initialInterval,
                                (float) ((level) * contourGroup.lastDensity),
                                false, "", 10);
                    } else {
                        interval = XFormFunctions.newDataIntervalFromZoom(
                                initialInterval,
                                (float) ((level) * contourGroup.lastDensity),
                                true, "", 10);
                    }

                    float[] controls = new float[] { interval };
                    config.mode = 0;
                    config.seed = controls;

                    contours = FortConBuf.contour(subgriddedData, config);
                } else if (prefs.getContourLabeling().getValues() != null) {
                    // explicit contouring values provided

                    // convert Float[] to float[]
                    float[] vals = prefs.getContourLabeling().getValues();
                    // the +0.1 makes it so numbers greater than 0.9 are rounded
                    // up and all other numbers are rounded down. This seems to
                    // make it match A1 betterwhen the density is 0.67 than
                    // 2.98507 is rounded to 3 instead of 2
                    int d = (int) (2 / ((level) * contourGroup.lastDensity) + 0.1);
                    if (d > 1) {
                        int myCount = (vals.length + d - 1) / d;
                        float[] newVals = new float[myCount];
                        int i, j;
                        for (i = j = 0; j < myCount; i += d, j++) {
                            newVals[j] = vals[i];
                        }
                        vals = newVals;
                    }
                    if (prefs.getContourLabeling().isCreateNegativeValues()) {
                        float[] newVals = new float[vals.length * 2];
                        for (int i = 0; i < vals.length; i++) {
                            newVals[vals.length + i] = vals[i];
                            newVals[vals.length - i - 1] = -1 * vals[i];
                        }
                        vals = newVals;
                    }
                    config.seed = vals;
                    if (contourLabeling.getNumberOfContours() > 0) {
                        config.mode = contourLabeling.getNumberOfContours();
                    } else {
                        config.mode = vals.length;
                    }
                    contours = FortConBuf.contour(subgriddedData, config);
                }
            }

            long t1 = System.currentTimeMillis();
            System.out.println("Contouring took: " + (t1 - t0));

            float contourValue = 0;

            long tTransformAccum = 0;
            long tLabelAccum = 0;
            long tMinMaxAccum = 0;
            List<double[]> labelPoints = new ArrayList<double[]>(512);

            try {
                DecimalFormat dfLabel = null;

                if (labelFormat != null) {
                    dfLabel = new DecimalFormat(labelFormat);
                } else {
                    dfLabel = determineLabelFormat(contours.contourVals);
                }

                long tZ0 = System.currentTimeMillis();
                // process min/max
                processMinMaxLabels(contours, minMaxLabelFormat, minLabel,
                        maxLabel, rastPosToWorldGrid, contourGroup,
                        labelPoints, maxMinTrimLeft);
                long tZ1 = System.currentTimeMillis();
                tMinMaxAccum += tZ1 - tZ0;

                checkWorldWrapping(contours, descriptor, rastPosToLatLon);

                int size = contours.xyContourPoints.size();
                // total coordinates
                int totalPosCoords = 0;
                int totalNegCoords = 0;
                for (int i = 0; i < size; i++) {
                    if (contours.contourVals.get(i) > 0) {
                        totalPosCoords += (int) contours.xyContourPoints.get(i).length / 2;
                    } else {
                        totalNegCoords += (int) contours.xyContourPoints.get(i).length / 2;
                    }
                }

                contourGroup.posValueShape.allocate(totalPosCoords);
                contourGroup.negValueShape.allocate(totalNegCoords);

                for (int i = 0; i < size; i++) {
                    float[] contourGridPoints = contours.xyContourPoints.get(i);
                    float[] contourWorldPoints = new float[contourGridPoints.length];

                    try {
                        int contourPoints = contourGridPoints.length / 2;
                        tZ0 = System.currentTimeMillis();
                        rastPosToWorldGrid.transform(contourGridPoints, 0,
                                contourWorldPoints, 0, contourPoints);
                        tZ1 = System.currentTimeMillis();
                        tTransformAccum += (tZ1 - tZ0);
                        double[][] contourWorldPointsArr = new double[contourPoints][2];
                        int index = 0;
                        for (int j = 0; j < contourPoints; j++) {
                            contourWorldPointsArr[j][0] = contourWorldPoints[index++];
                            contourWorldPointsArr[j][1] = contourWorldPoints[index++];
                        }
                        contourValue = contours.contourVals.get(i);
                        String label = dfLabel.format(contourValue);
                        label = label.substring(labelTrimLeft);
                        if (prefs != null
                                && prefs.getContourLabeling() != null
                                && prefs.getContourLabeling().getValues() != null
                                && labelFormat == null) {
                            // If we have explicit values and no explicit format
                            // use the default String representation of the
                            // given values
                            float[] values = prefs.getContourLabeling()
                                    .getValues();
                            for (int k = 0; k < values.length; k++) {
                                if (contourValue == values[k]) {
                                    label = Float.toString(values[k]);
                                    break;
                                }
                            }
                        }
                        IWireframeShape shapeToAddTo = null;

                        if (contourValue >= 0) {
                            shapeToAddTo = contourGroup.posValueShape;
                        } else {
                            shapeToAddTo = contourGroup.negValueShape;
                        }

                        shapeToAddTo.addLineSegment(contourWorldPointsArr);

                        // process contour labels
                        // TODO perform formatting
                        tZ0 = System.currentTimeMillis();
                        prepareLabel(shapeToAddTo, zoom, label, labelPoints,
                                contourWorldPointsArr);
                        tZ1 = System.currentTimeMillis();
                        tLabelAccum += tZ1 - tZ0;
                    } catch (TransformException e) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Error transforming points: "
                                                + Arrays.toString(contourGridPoints),
                                        e);
                    }
                }

                System.out.println("Min/Max process time: " + tMinMaxAccum);
                System.out.println("label time: " + tLabelAccum);
                System.out.println("transformation time: " + tTransformAccum);
            } catch (Throwable e) {
                throw new VizException("Error postprocessing contours", e);
            }
        } else if (records.length == 2) {
            float[] uW = null;
            float[] vW = null;
            long[] sz = records[0].getSizes();

            if (records[0] instanceof FloatDataRecord) {
                uW = ((FloatDataRecord) records[0]).getFloatData();
                vW = ((FloatDataRecord) records[1]).getFloatData();
            } else {
                throw new UnsupportedOperationException(
                        "Streamlining is not supported for data type: "
                                + records.getClass().getName());
            }

            // Step 4: Determine the subgrid, if any

            int minX = (int) Math.floor(Math.max(env.getMinimum(0), 0));
            int minY = (int) Math.floor(Math.max(env.getMinimum(1), 0));
            int maxX = (int) Math.ceil(Math.min(env.getMaximum(0), sz[0] - 1));
            int maxY = (int) Math.ceil(Math.min(env.getMaximum(1), sz[1] - 1));

            makeStreamLines(uW, vW, minX, minY, maxX, maxY, sz, contourGroup,
                    currentMagnification, zoom, contourGroup.lastDensity,
                    rastPosToWorldGrid);
        }

        return contourGroup;

    }

    public static GeneralEnvelope calculateSubGrid(IExtent workingExtent,
            GeneralGridGeometry mapGridGeometry,
            GeneralGridGeometry imageGridGeometry) throws VizException {
        GeneralEnvelope env = new GeneralEnvelope(2);
        try {
            GridGeometry2D imageGeometry2D = GridGeometry2D
                    .wrap(imageGridGeometry);

            org.opengis.geometry.Envelope subgridCRSEnvelope = MapUtil
                    .reprojectAndIntersect(mapGridGeometry.getEnvelope(),
                            imageGridGeometry.getEnvelope());

            GridEnvelope2D subgridEnv = imageGeometry2D
                    .worldToGrid(new Envelope2D(subgridCRSEnvelope));
            // Add a 1 pixel border since worldToGrid is only guaranteed to
            // include a cell if the cell center is in the envelope but we want
            // to include the data in the subgrid if even a tiny bit of the edge
            // overlaps.
            subgridEnv.grow(1, 1);
            if (!subgridEnv.isEmpty()) {
                // Convert GridEnvelope to general envelope.
                env.setRange(0, subgridEnv.getMinX(), subgridEnv.getMaxX());
                env.setRange(1, subgridEnv.getMinY(), subgridEnv.getMaxY());
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN,
                    "Cannot compute subgrid, contouring may be slow.", e);
            // Don't use a subgrid, just contour the complete image.
            // This may result in doing way to much contouring which can be
            // slow, but it is better than no contouring at all. It's also worth
            // noting that it gets slower as you zoom in and more contours are
            // generated, but as you zoom in it also becomes more likely you
            // will be successful in your transformation since the smaller area
            // is less likely to contain invalid points.
            env.setRange(0, imageGridGeometry.getGridRange().getLow(0),
                    imageGridGeometry.getGridRange().getHigh(0));
            env.setRange(1, imageGridGeometry.getGridRange().getLow(1),
                    imageGridGeometry.getGridRange().getHigh(1));
        }
        System.out.println("*** Subgrid: " + env);
        return env;
    }

    /**
     * Create labels for a linestring
     * 
     * @param shapeToAddTo
     * @param levelOffset
     * @param contourValue
     * @param labelPoints
     * @param valsArr
     */
    private static void prepareLabel(IWireframeShape shapeToAddTo,
            double screenToPixel, String label, List<double[]> labelPoints,
            double[][] valsArr) {
        double[] lastPoint = new double[] { Double.POSITIVE_INFINITY,
                Double.POSITIVE_INFINITY };

        double d = 0.0;

        final double threshold1 = (200.0 / screenToPixel);
        final double threshold2 = (50.0 / screenToPixel);

        long tAccum = 0;
        double q1, q2, p1, p2;
        for (int n = 0; n < valsArr.length; n++) {

            // Distance approximation between last label point
            // and current point

            // Absolute value logic inlined for performance
            q1 = lastPoint[0] - valsArr[n][0];
            q2 = lastPoint[1] - valsArr[n][1];
            q1 = (q1 <= 0.0D) ? 0.0D - q1 : q1;
            q2 = (q2 <= 0.0D) ? 0.0D - q2 : q2;

            d = q1 + q2;

            // If distance has been enough, add a label
            if (d > (threshold1)
            /* || (labeledAtLeastOnce == false && n == valsArr.length - 1) */) {
                // Search for any labels that are too close
                // to the current one
                boolean tooClose = false;
                p1 = valsArr[n][0];
                p2 = valsArr[n][1];
                q1 = 0;
                q2 = 0;
                for (double[] test : labelPoints) {
                    // Distance approximation between each label
                    // point and current point
                    // Absolute value logic inlined for performance
                    q1 = test[0] - p1;
                    q2 = test[1] - p2;
                    q1 = (q1 <= 0.0D) ? -1.0D * q1 : q1;
                    q2 = (q2 <= 0.0D) ? -1.0D * q2 : q2;
                    d = q1 + q2;
                    if (d < threshold2) {
                        tooClose = true;
                        break;
                    }
                }
                if (!tooClose
                /* || (labeledAtLeastOnce == false && n == valsArr.length - 1) */) {
                    long t0 = System.currentTimeMillis();
                    shapeToAddTo.addLabel(label, valsArr[n]);
                    labelPoints.add(valsArr[n]);
                    lastPoint = valsArr[n];
                    tAccum += (System.currentTimeMillis() - t0);
                }
            }
        }

    }

    public static ContourGroup createContours(Object data, int level,
            IExtent extent, double currentDensity,
            GeneralGridGeometry imageGridGeometry, IGraphicsTarget target,
            ContourPreferences prefs) throws VizException {

        GeneralEnvelope imageEnvelope = new GeneralEnvelope(new double[] {
                extent.getMinX(), extent.getMinY() }, new double[] {
                extent.getMaxX(), extent.getMaxY() });
        GeneralGridEnvelope imageRange = new GeneralGridEnvelope(new int[] { 0,
                0 }, new int[] { imageGridGeometry.getGridRange().getSpan(0),
                imageGridGeometry.getGridRange().getSpan(1) }, false);
        GridGeometry2D imageGeom = new GridGeometry2D(imageRange, imageEnvelope);
        imageGeom = GridGeometry2D.wrap(imageGridGeometry);
        MathTransform gridToPixel = imageGeom
                .getGridToCRS(PixelInCell.CELL_CENTER);

        ContourGroup contourGroup = new ContourGroup();
        contourGroup.lastDensity = currentDensity;
        contourGroup.zoomLevel = 1.0 / Math.pow(2.0, level);

        GeneralEnvelope pixelEnvelope = new GeneralEnvelope(new double[] {
                extent.getMinX(), extent.getMinY() }, new double[] {
                extent.getMaxX(), extent.getMaxY() });

        GeneralGridEnvelope range = new GeneralGridEnvelope(new int[] {
                (int) extent.getMinX(), (int) extent.getMinY() }, new int[] {
                (int) (extent.getMaxX()), (int) extent.getMaxY() }, false);
        GridGeometry2D pixelGeom = new GridGeometry2D(range, pixelEnvelope);

        contourGroup.posValueShape = target.createWireframeShape(false,
                pixelGeom);
        contourGroup.posValueShape.allocate(1000);
        contourGroup.negValueShape = target.createWireframeShape(false,
                pixelGeom);
        contourGroup.negValueShape.allocate(1000);

        // Step 3: Get the actual data

        float[] data1D = null;
        long[] sz = new long[] { imageGridGeometry.getGridRange().getSpan(0),
                imageGridGeometry.getGridRange().getSpan(1) };

        if (data instanceof float[]) {
            data1D = (float[]) data;

            int minX = 0;
            int minY = 0;
            int maxX = (int) (sz[0] - 1);
            int maxY = (int) (sz[1] - 1);

            int szX = (maxX - minX) + 1;
            int szY = (maxY - minY) + 1;
            int totalSz = szX * szY;
            if (totalSz <= 0) {
                return contourGroup;
            }

            float[][] subgriddedData = new float[szX][szY];

            for (int j = 0; j < szY; j++) {
                for (int i = 0; i < szX; i++) {
                    subgriddedData[i][j] = data1D[((int) sz[0] * (j + minY))
                            + (i + minX)];
                }
            }

            // Use ported legacy code to determine contour interval
            ContourContainer contours = null;
            FortConConfig config = new FortConConfig();
            long t0 = System.currentTimeMillis();
            String minMaxLabelFormat = null;
            String minLabel = null;
            String maxLabel = null;
            String labelFormat = null;
            config.badlo = smallestContourValue;
            config.badhi = largestContourValue;
            config.xOffset = minX;
            config.yOffset = minY;
            config.labelSpacingLine = subgriddedData.length / 3;
            if (config.labelSpacingLine < 1) {
                config.labelSpacingLine = 1;
            }

            // If nothing provided, attempt to get approximately 50 contours
            if (prefs == null || prefs.getContourLabeling() == null) {

                float interval = XFormFunctions.newDataIntervalFromZoom(0.5f,
                        (float) ((level + 1) * contourGroup.lastDensity), true,
                        "", 10);

                config.seed = new float[] { interval };
                config.mode = -50;
                contours = FortConBuf.contour(subgriddedData, config);
            } else {
                LabelingPreferences contourLabeling = prefs
                        .getContourLabeling();
                if (contourLabeling.getLabelSpacing() > 0) {
                    config.labelSpacingLine = subgriddedData.length
                            / contourLabeling.getLabelSpacing();
                    if (config.labelSpacingLine < 1) {
                        config.labelSpacingLine = 1;
                    }
                }

                // setup min/max processing
                minMaxLabelFormat = contourLabeling.getMinMaxLabelFormat();
                minLabel = contourLabeling.getMinLabel();
                maxLabel = contourLabeling.getMaxLabel();
                labelFormat = contourLabeling.getLabelFormat();
                config.generateMins = minLabel != null
                        && (!minLabel.equals(".") || (minMaxLabelFormat != null && minLabel
                                .equals(".")));
                config.generateMaxes = maxLabel != null
                        && (!maxLabel.equals(".") || (minMaxLabelFormat != null && maxLabel
                                .equals(".")));

                config.minMaxRadius = (int) (1.0 * 3 * ((PixelExtent) extent)
                        .getWidth() / 90);
                if (config.minMaxRadius < 1) {
                    config.minMaxRadius = 1;
                }
                if (config.minMaxRadius > 7) {
                    config.minMaxRadius = 7;
                }

                if (prefs.getContourLabeling().getValues() == null
                        && prefs.getContourLabeling().getIncrement() > 0.0) {
                    // interval provided
                    float initialInterval = prefs.getContourLabeling()
                            .getIncrement();
                    float interval;
                    if (contourLabeling.getNumberOfContours() > 0) {
                        // use numberOfContours
                        float minData = 1e37f;
                        float maxData = -1e37f;
                        for (float[] dataRow : subgriddedData)
                            for (float dataCell : dataRow)
                                if (dataCell < minData && dataCell != -999999)
                                    minData = dataCell;
                                else if (dataCell < 999998
                                        && dataCell > maxData)
                                    maxData = dataCell;
                        interval = (maxData - minData)
                                / contourLabeling.getNumberOfContours();
                        if (interval < 0)
                            interval = -interval;

                        if (interval > initialInterval) {
                            initialInterval = interval;
                            // why would this be done we want to maintain the
                            // configured minimum, it will cause problems when
                            // showing the same parameter on cross section and
                            // time
                            // height?

                            // prefs.getContourLabeling()
                            // .setIncrement(initialInterval);
                        }

                        interval = XFormFunctions
                                .newDataIntervalFromZoom(
                                        initialInterval,
                                        (float) ((level + 1) * contourGroup.lastDensity),
                                        false, "", 10);
                    } else {
                        interval = XFormFunctions
                                .newDataIntervalFromZoom(
                                        initialInterval,
                                        (float) ((level + 1) * contourGroup.lastDensity),
                                        true, "", 10);
                    }

                    float[] controls = new float[] { interval };
                    config.mode = 0;
                    config.seed = controls;

                    contours = FortConBuf.contour(subgriddedData, config);
                } else if (prefs.getContourLabeling().getValues() != null) {
                    // explicit contouring values provided

                    // convert Float[] to float[]
                    float[] vals = prefs.getContourLabeling().getValues();
                    // the +0.1 makes it so numbers greater than 0.9 are rounded
                    // up
                    // and all other numbers are rounded down.
                    int d = (int) (2 / ((level + 1) * contourGroup.lastDensity) + 0.1);
                    if (d > 1) {
                        int myCount = (vals.length + d - 1) / d;
                        float[] newVals = new float[myCount];
                        int i, j;
                        for (i = j = 0; j < myCount; i += d, j++) {
                            newVals[j] = vals[i];
                        }
                        vals = newVals;
                    }
                    if (prefs.getContourLabeling().isCreateNegativeValues()) {
                        float[] newVals = new float[vals.length * 2];
                        for (int i = 0; i < vals.length; i++) {
                            newVals[vals.length + i] = vals[i];
                            newVals[vals.length - i - 1] = -1 * vals[i];
                        }
                        vals = newVals;
                    }
                    config.seed = vals;
                    if (contourLabeling.getNumberOfContours() > 0) {
                        config.mode = contourLabeling.getNumberOfContours();
                    } else {
                        config.mode = vals.length;
                    }
                    contours = FortConBuf.contour(subgriddedData, config);
                }
            }

            long t1 = System.currentTimeMillis();
            System.out.println("Contouring took: " + (t1 - t0));

            double levelOffset = Math.pow(2, (level - 1));

            float contourValue = 0;
            long tTransformAccum = 0;
            long tLabelAccum = 0;
            long tMinMaxAccum = 0;
            List<double[]> labelPoints = new ArrayList<double[]>();

            try {
                int size = contours.xyContourPoints.size();

                if (size > 0) {
                    DecimalFormat dfLabel = null;

                    if (labelFormat != null) {
                        dfLabel = new DecimalFormat(labelFormat);
                    } else {
                        dfLabel = determineLabelFormat(contours.contourVals);
                    }

                    long tZ0 = System.currentTimeMillis();
                    // process min/max
                    processMinMaxLabels(contours, minMaxLabelFormat, minLabel,
                            maxLabel, gridToPixel, contourGroup, labelPoints, 0);
                    long tZ1 = System.currentTimeMillis();
                    tMinMaxAccum += tZ1 - tZ0;

                    for (int i = 0; i < size; i++) {
                        float[] contourGridPoints = contours.xyContourPoints
                                .get(i);
                        float[] contourWorldPoints = new float[contourGridPoints.length];

                        try {
                            int contourPoints = contourGridPoints.length / 2;
                            tZ0 = System.currentTimeMillis();
                            gridToPixel.transform(contourGridPoints, 0,
                                    contourWorldPoints, 0, contourPoints);
                            tZ1 = System.currentTimeMillis();
                            tTransformAccum += (tZ1 - tZ0);
                            double[][] contourWorldPointsArr = new double[contourPoints][2];
                            int index = 0;
                            for (int j = 0; j < contourPoints; j++) {
                                contourWorldPointsArr[j][0] = contourWorldPoints[index++];
                                contourWorldPointsArr[j][1] = contourWorldPoints[index++];
                            }
                            contourValue = contours.contourVals.get(i);

                            String label = dfLabel.format(contourValue);
                            IWireframeShape shapeToAddTo = null;

                            if (contourValue >= 0) {
                                shapeToAddTo = contourGroup.posValueShape;
                            } else {
                                shapeToAddTo = contourGroup.negValueShape;
                            }

                            shapeToAddTo.addLineSegment(contourWorldPointsArr);
                            prepareLabel(shapeToAddTo, levelOffset, label,
                                    labelPoints, contourWorldPointsArr);
                        } catch (TransformException e) {
                            statusHandler
                                    .handle(Priority.PROBLEM,
                                            "Error transforming points: "
                                                    + Arrays.toString(contourGridPoints),
                                            e);
                        }
                    }
                }
                System.out.println("Min/Max process time: " + tMinMaxAccum);
                System.out.println("label time: " + tLabelAccum);
                System.out.println("transformation time: " + tTransformAccum);

            } catch (Exception e) {
                throw new VizException("Error postprocessing contours", e);
            }

            return contourGroup;
        } else if (data instanceof List && ((List<?>) data).size() == 2) {
            // Step 1:First determine the subgrid to contour

            // Step 3: Get the actual data

            float[] uW = (float[]) ((List<?>) data).get(0);
            float[] vW = (float[]) ((List<?>) data).get(1);

            // Step 4: Determine the subgrid, if any

            int minX = 0;
            int minY = 0;
            int maxX = (int) (sz[0] - 1);
            int maxY = (int) (sz[1] - 1);
            makeStreamLines(uW, vW, minX, minY, maxX, maxY, sz, contourGroup,
                    1, 1, contourGroup.lastDensity * 2, gridToPixel);
            return contourGroup;

        } else {
            throw new UnsupportedOperationException(
                    "Contouring is not supported for data type: "
                            + data.getClass().getName());
        }

    }

    private static void makeStreamLines(float[] uW, float[] vW, int minX,
            int minY, int maxX, int maxY, long[] sz, ContourGroup contourGroup,
            double currentMagnification, float zoom, double density,
            MathTransform rastPosToWorldGrid) throws VizException {

        int szX = (maxX - minX) + 1;
        int szY = (maxY - minY) + 1;
        int totalSz = szX * szY;
        if (totalSz <= 0) {
            return;
        }
        int x = (int) sz[0];
        int y = (int) sz[1];

        float[] adjustedUw = new float[totalSz];
        float[] adjustedVw = new float[totalSz];

        int n = 0;

        for (int j = 0; j < szY; j++) {
            for (int i = 0; i < szX; i++) {

                adjustedUw[n] = uW[(x * (j + minY)) + (i + minX)];
                adjustedVw[n] = vW[(x * (j + minY)) + (i + minX)];
                n++;
            }
        }

        Util.flipVert(adjustedUw, szY, szX);
        Util.flipVert(adjustedVw, szY, szX);

        int arrSz = Math.max(10 * adjustedUw.length, uW.length);

        int[] work = new int[arrSz];
        float[] xPoints = new float[arrSz];
        float[] yPoints = new float[arrSz];
        int[] numPoints = new int[1];

        // Use ported legacy code to determine contour interval
        long t0 = System.currentTimeMillis();

        double[] center = new double[2];
        double[] offCenter = new double[2];

        try {
            rastPosToWorldGrid.transform(new double[] { x / 2.0, y / 2.0 }, 0,
                    center, 0, 1);
            rastPosToWorldGrid.transform(new double[] { (x / 2.0) + 1.0,
                    y / 2.0 }, 0, offCenter, 0, 1);
        } catch (TransformException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }

        double gridPixelSize = offCenter[0] - center[0];
        double gridPixelMax = 2000.;

        // If gridPixelSize is large, arrows on streamline will be too small, so
        // adjust here
        if (gridPixelSize > gridPixelMax) {
            gridPixelSize = gridPixelSize / 5;
        }
        float arrowSize = (float) (currentMagnification * 5 / zoom / gridPixelSize);

        double spadiv = zoom * density * gridPixelSize / 25;

        double minSpacing = 1.0 / spadiv;
        double maxSpacing = 3.0 / spadiv;
        float minspc = 0;
        float maxspc = 0;

        if (minSpacing > 1) {
            minspc = (float) Math.sqrt(minSpacing);
        }
        if (minspc < 0.1) {
            minspc = 0.1f;
        }
        if (maxSpacing > 1) {
            maxspc = (float) Math.sqrt(maxSpacing);
        }
        if (maxspc < 0.25) {
            maxspc = 0.25f;
        }

        Controller.strmpak(adjustedUw, adjustedVw, work, szX, szX, szY,
                arrowSize, xPoints, yPoints, numPoints, minspc, maxspc,
                -1000000f, -999998f);

        long t1 = System.currentTimeMillis();
        System.out.println("Contouring took: " + (t1 - t0));

        List<double[]> vals = new ArrayList<double[]>();

        long tAccum = 0;

        try {
            for (int i = 0; i < numPoints[0] && i < xPoints.length; i++) {
                if (xPoints[i] == -99999.0) {
                    if (vals.size() > 0) {
                        double[][] valsArr = vals.toArray(new double[vals
                                .size()][2]);
                        contourGroup.posValueShape.addLineSegment(valsArr);
                        vals.clear();
                    }
                } else {
                    double[] out = new double[2];
                    try {
                        long tZ0 = System.currentTimeMillis();
                        rastPosToWorldGrid.transform(new double[] {
                                maxX - xPoints[i] + 1, yPoints[i] + minY - 1 },
                                0, out, 0, 1);
                        long tZ1 = System.currentTimeMillis();
                        tAccum += (tZ1 - tZ0);
                    } catch (TransformException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                    vals.add(out);
                }
            }

            System.out.println("streamline transformation time: " + tAccum);

            if (vals.size() > 0) {

                double[][] valsArr = vals.toArray(new double[vals.size()][2]);
                contourGroup.posValueShape.addLineSegment(valsArr);
                vals.clear();
            }
        } catch (Throwable e) {
            throw new VizException("Error postprocessing contours", e);
        }
    }

    private static void processMinMaxLabels(ContourContainer contours,
            String minMaxLabelFormat, String minLabel, String maxLabel,
            MathTransform transform, ContourGroup contourGroup,
            List<double[]> labelPoints, int maxMinTrimLeft)
            throws TransformException {
        if (contours.minLabelPoints != null) {
            if (minLabel != null && !minLabel.equals(".")) {
                contourGroup.minMark = minLabel;
            }
            if (minMaxLabelFormat != null) {
                if (minMaxLabelFormat.trim().length() > 0) {
                    DecimalFormat df = new DecimalFormat(minMaxLabelFormat);
                    int size = contours.minVals.size();
                    contourGroup.minVals = new String[size];
                    for (int i = 0; i < size; i++) {
                        String label = df.format(contours.minVals.get(i));
                        label = label.substring(maxMinTrimLeft);
                        contourGroup.minVals[i] = label;
                    }
                } else {
                    int size = contours.minVals.size();
                    contourGroup.minVals = new String[size];
                    for (int i = 0; i < size; i++) {
                        float val = contours.minVals.get(i);
                        DecimalFormat df = determineLabelFormat(val);
                        contourGroup.minVals[i] = df.format(val);
                    }
                }
            }
            contourGroup.minLabelPoints = transformPointList(
                    contours.minLabelPoints, transform);
            for (double[] vals : contourGroup.minLabelPoints) {
                labelPoints.add(vals);
            }
        }
        if (contours.maxLabelPoints != null) {
            if (maxLabel != null && !maxLabel.equals(".")) {
                contourGroup.maxMark = maxLabel;
            }
            if (minMaxLabelFormat != null) {
                if (minMaxLabelFormat.trim().length() > 0) {
                    DecimalFormat df = new DecimalFormat(minMaxLabelFormat);
                    int size = contours.maxVals.size();
                    contourGroup.maxVals = new String[size];
                    for (int i = 0; i < size; i++) {
                        String label = df.format(contours.maxVals.get(i));
                        label = label.substring(maxMinTrimLeft);
                        contourGroup.maxVals[i] = label;
                    }
                } else {
                    int size = contours.maxVals.size();
                    contourGroup.maxVals = new String[size];
                    for (int i = 0; i < size; i++) {
                        float val = contours.maxVals.get(i);
                        DecimalFormat df = determineLabelFormat(val);
                        contourGroup.maxVals[i] = df.format(val);
                    }
                }
            }
            contourGroup.maxLabelPoints = transformPointList(
                    contours.maxLabelPoints, transform);
            for (double[] vals : contourGroup.maxLabelPoints) {
                labelPoints.add(vals);
            }
        }

    }

    private static double[][] transformPointList(List<float[]> xyPoints,
            MathTransform transformToApply) throws TransformException {
        int size = xyPoints.size();
        float in[] = new float[size * 2];
        int index = 0;
        for (float[] xy : xyPoints) {
            in[index++] = xy[0];
            in[index++] = xy[1];
        }

        float out[] = new float[in.length];
        for (int i = 0; i < size; i++) {
            transformToApply.transform(in, 0, out, 0, size);
        }
        double[][] rval = new double[size][2];
        index = 0;
        for (int i = 0; i < rval.length; i++) {
            rval[i][0] = out[index++];
            rval[i][1] = out[index++];
        }

        return rval;
    }

    // determine based on spread of values
    private static DecimalFormat determineLabelFormat(List<Float> vals) {
        // determine format based on the average difference between the contours
        SortedSet<Float> sortedValues = new TreeSet<Float>(vals);
        Iterator<Float> iter = sortedValues.iterator();
        if (iter.hasNext()) {
            float prevVal = iter.next();
            int count = 0;
            double diffSum = 0;
            while (iter.hasNext()) {
                float val = iter.next();
                diffSum += val - prevVal;
                count++;
                prevVal = val;
            }
            return determineLabelFormat((float) (diffSum / count));
        } else {
            return determineLabelFormat(0.0f);
        }

    }

    // determine based on spread of values
    private static DecimalFormat determineLabelFormat(float val) {
        float absval = Math.abs(val);

        DecimalFormat rval = null;
        if (absval == 0) {
            rval = new DecimalFormat("0.#");
        } else if (absval < 0.001 || absval > 1e7) {
            rval = new DecimalFormat("0.###E0");
        } else if (absval < 0.01) {
            rval = new DecimalFormat("0.###");
        } else if (absval < 0.1) {
            rval = new DecimalFormat("0.##");
        } else {
            rval = new DecimalFormat("0.#");
        }

        return rval;
    }

    private static void checkWorldWrapping(ContourContainer contours,
            IMapDescriptor descriptor, MathTransform rastPosToLatLon)
            throws TransformException {
        long tZ0 = System.currentTimeMillis();

        WorldWrapChecker wwc = new WorldWrapChecker(
                descriptor.getGridGeometry());

        List<float[]> splitLines = new ArrayList<float[]>();
        List<Float> dupValues = new ArrayList<Float>();

        Iterator<float[]> lineIter = contours.xyContourPoints.iterator();
        Iterator<Float> valueIter = contours.contourVals.iterator();

        while (lineIter.hasNext()) {
            float[] data = lineIter.next();
            Float val = valueIter.next();
            double[] ll = new double[data.length];
            rastPosToLatLon.transform(data, 0, ll, 0, data.length / 2);
            boolean remove = false;
            int startIndex = 0;
            double lastLon = ll[0];
            for (int i = 2; i < ll.length; i += 2) {
                double lon = ll[i];
                if (wwc.check(lastLon, lon)) {
                    remove = true;
                    if (startIndex + 1 < i) {
                        splitLines.add(Arrays.copyOfRange(data, startIndex, i));
                        dupValues.add(val);
                    }
                    startIndex = i;
                }
                lastLon = lon;
            }
            if (remove) {
                lineIter.remove();
                valueIter.remove();
                if (startIndex + 1 < data.length) {
                    splitLines.add(Arrays.copyOfRange(data, startIndex,
                            data.length));
                    dupValues.add(val);
                }
            }

        }
        contours.xyContourPoints.addAll(splitLines);
        contours.contourVals.addAll(dupValues);

        long tZ1 = System.currentTimeMillis();

        System.out.println("Time to process world wrap checking = "
                + (tZ1 - tZ0) + "ms");
    }
}
