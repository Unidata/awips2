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

package com.raytheon.uf.common.dataplugin.gfe;

import java.awt.RenderingHints;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.nio.ByteBuffer;

import javax.media.jai.BorderExtender;
import javax.media.jai.Interpolation;
import javax.media.jai.JAI;
import javax.media.jai.ParameterBlockJAI;
import javax.media.jai.PlanarImage;

import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridCoverageFactory;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.opengis.geometry.DirectPosition;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.data.DataSource;
import com.raytheon.uf.common.geospatial.interpolation.data.FloatArrayWrapper;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Port of original RemapGrid class. The interface remains the same, but
 * GeoTools carries out the heavy lifting of resampling the data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 5/16/08      875        bphillip    Initial Creation.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class RemapGrid {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RemapGrid.class);

    /** The input grid location describing the source data */
    private GridLocation sourceGloc;

    /** The output grid location describing the destination data */
    private GridLocation destinationGloc;

    private boolean rescale = false;

    private Grid2DFloat rotation;

    private GridReprojection interp;

    /**
     * Constructs a new RemapGrid with the given input and output grid locations
     * 
     * @param sourceGloc
     *            The source grid location describing the source data
     * @param destinationGloc
     *            The destination grid location describing the destination data
     */
    public RemapGrid(GridLocation sourceGloc, GridLocation destinationGloc) {
        this.sourceGloc = sourceGloc;
        this.destinationGloc = destinationGloc;
    }

    public RemapGrid(GridLocation sourceGloc, GridLocation destinationGloc,
            boolean rescale) {
        this.sourceGloc = sourceGloc;
        this.destinationGloc = destinationGloc;
        this.rescale = rescale;
    }

    /**
     * Returns a Grid2D<float> that has been remapped from the input grid in the
     * source GridLocation domain space to the destination GridLocation domain
     * space. The input grid must be in the same coordinate system as the source
     * GridLocation. The data will be sampled and truncated to min and max.
     * Points outside the area will be assigned the input fillValue.
     * 
     * @param input
     *            The input float grid data
     * @param inputFill
     *            The input fill value
     * @param max
     *            The max data value for data
     * @param min
     *            The min data value for data
     * @param outputFill
     *            The outputFill value
     * @return A remapped Grid2DFloat object
     * @throws TransformException
     * @throws FactoryException
     */
    public Grid2DFloat remap(final Grid2DFloat input, float inputFill,
            float max, float min, float outputFill) throws FactoryException,
            TransformException {

        Grid2DFloat retVal = resample(input);
        limitGrid(retVal, inputFill, max, min, outputFill);
        return retVal;
    }

    /**
     * Returns a Grid2D<byte> that has been remapped from the input grid in the
     * source GridLocation domain space to the destination GridLocation domain
     * space. The input grid must be in the same coordinate system as the source
     * GridLocation. The data will be sampled. Points outside the area will be
     * assigned the input fillValue.
     * 
     * @param input
     *            The input byte data
     * @param inputFill
     *            The input fill value
     * @param outputFill
     *            The output fill value
     * @return The remapped Grid2DByte object
     * @throws IllegalArgumentException
     *             If the input dimensions do not match the source dimensions or
     *             when problems occur during resampling
     */
    public Grid2DByte remap(final Grid2DByte input, byte inputFill,
            byte outputFill) {

        Grid2DByte retVal = null;

        if (input.getXdim() != sourceGloc.gridSize().x
                || input.getYdim() != sourceGloc.gridSize().y) {
            throw new IllegalArgumentException(
                    "Input grid dimensions do not match source grid location dimensions");
        }

        // same as input -- do nothing
        if (sourceGloc.equals(destinationGloc)) {
            return input;
        }

        retVal = resample(input);

        ByteBuffer buffer = retVal.getBuffer();
        buffer.rewind();

        for (int x = 0; x < retVal.getXdim(); x++) {
            for (int y = 0; y < retVal.getYdim(); y++) {
                byte value = retVal.get(x, y);
                if (value == inputFill) {
                    retVal.set(x, y, outputFill);
                }
            }
        }

        return retVal;
    }

    public Grid2DByte remap(final Grid2DByte input, int inputFill,
            int outputFill) {
        return remap(input, (byte) inputFill, (byte) outputFill);
    }

    /**
     * For vector data in u and v rotated coordinates. Returns a mag and dir
     * grid that has been remapped from the input grid in the source
     * GridLocation domain space to the destination GridLocation domain space.
     * The input grids must be in the same coordinate system as the source
     * GridLocation. The data will be sampled and truncated to min and max.
     * Points outside the area will be assigned the input fillValue. If the
     * rotate flag is set, then the u and v input coordinates are assumed to be
     * relative to grid north, and not true north, and the resulting mag/dir
     * output will be set to true north. If flip is set, then the direction is
     * switched by 180 degrees to indicate "from" instead of "to" direction.
     * 
     * @param uinput
     *            The input u grid
     * @param vinput
     *            The input v grid
     * @param inputFill
     *            The input fill value
     * @param maxLimit
     *            The max value for data values
     * @param minLimit
     *            The min value for data values
     * @param outputFillValue
     *            The output fill value
     * @param rotate
     *            The rotation flag
     * @param flip
     *            The flip flag
     * @param magGrid
     *            The output magnitude grid
     * @param dirGrid
     *            The output direction grid
     * @throws Exception
     *             If grid dimensions do not match or resampling fails
     */
    public void remapUV(final Grid2DFloat uinput, final Grid2DFloat vinput,
            float inputFill, float maxLimit, float minLimit,
            float outputFillValue, boolean rotate, boolean flip,
            Grid2DFloat magGrid, Grid2DFloat dirGrid) throws Exception {

        if (uinput.getXdim() != sourceGloc.getNx()
                || uinput.getYdim() != sourceGloc.getNy()
                || vinput.getXdim() != sourceGloc.getNx()
                || vinput.getYdim() != sourceGloc.getNy()) {
            String error = "Source grid sizes do not match source grid location: \n";
            error += "source (" + sourceGloc.getNx() + ", "
                    + sourceGloc.getNy() + ")\n";
            error += "uinput (" + uinput.getXdim() + ", " + uinput.getYdim()
                    + ")\n";
            error += "vinput (" + vinput.getXdim() + ", " + vinput.getYdim()
                    + ")\n";
            throw new GfeException(error);
        }

        for (int i = 0; i < uinput.getXdim(); i++) {
            for (int j = 0; j < uinput.getYdim(); j++) {
                if (uinput.get(i, j) == inputFill) {
                    uinput.set(i, j, Float.NaN);
                    vinput.set(i, j, Float.NaN);
                } else if (vinput.get(i, j) == inputFill) {
                    uinput.set(i, j, Float.NaN);
                    vinput.set(i, j, Float.NaN);
                }
            }
        }

        Grid2DFloat resampledUinput = resample(uinput);
        Grid2DFloat resampledVinput = resample(vinput);

        calculateWindGrid(resampledUinput, resampledVinput, rotate, flip,
                magGrid, dirGrid);

        limitGrid(magGrid, inputFill, maxLimit, minLimit, outputFillValue);
        limitGrid(dirGrid, 0.0f, 360.0f, 0.0f, 0.0f);
    }

    /**
     * For vector data in mag/dir coordinates. Returns a mag/dir grid that has
     * been remapped from the input grid in the source GridLocation domain space
     * to the destination GridLocation domain space. The input grids must be in
     * the same coordinate system as the source GridLocation. The data will be
     * sampled and truncated to min and max. Points outside the area will be
     * assigned the input fillValue.
     * 
     * @param magInput
     *            The magnitude input grid
     * @param dirInput
     *            The direction input grid
     * @param inputFill
     *            The input fill value
     * @param maxLimit
     *            The max data value limit
     * @param minLimit
     *            The min data value limit
     * @param outputFillValue
     *            The output fill value
     * @param magOutput
     *            The output magnitude grid
     * @param dirOutput
     *            The output direction grid
     * @throws Exception
     *             If resampling fails
     */
    public void remap(Grid2DFloat magInput, Grid2DFloat dirInput,
            float inputFill, float maxLimit, float minLimit,
            float outputFillValue, Grid2DFloat magOutput, Grid2DFloat dirOutput)
            throws Exception {

        // same as input -- do nothing
        if (sourceGloc.equals(destinationGloc)) {
            magOutput.assign(magInput);
            dirOutput.assign(dirInput);
            limitGrid(magOutput, inputFill, maxLimit, minLimit, outputFillValue);
            limitGrid(dirOutput, 0.0f, 360.0f, 0.0f, 0.0f);
            return;
        }

        // Convert the grid from mag/dir to u/v for sampling
        Grid2DFloat uComp = new Grid2DFloat(dirInput.getXdim(),
                dirInput.getYdim());
        Grid2DFloat vComp = new Grid2DFloat(dirInput.getXdim(),
                dirInput.getYdim());

        double angle;

        // Convert all of the points within the expanded bounds
        for (int i = 0; i < dirInput.getXdim(); i++) {
            for (int j = 0; j < dirInput.getYdim(); j++) {
                if (magInput.get(i, j) != inputFill) {
                    angle = Math.toRadians(dirInput.get(i, j));
                    uComp.set(i, j,
                            (float) Math.sin(angle) * magInput.get(i, j));
                    vComp.set(i, j,
                            (float) Math.cos(angle) * magInput.get(i, j));
                } else {
                    uComp.set(i, j, inputFill);
                    vComp.set(i, j, inputFill);
                }
            }
        }
        this.remapUV(uComp, vComp, inputFill, maxLimit, minLimit,
                outputFillValue, false, false, magOutput, dirOutput);

    }

    /**
     * Puts a limit on the grid and adjusts the data values to fit within these
     * 
     * @param grid
     *            The grid
     * @param inputFill
     *            The input fill value
     * @param maxLimit
     *            The max limit value
     * @param minLimit
     *            The min limit value
     * @param outputFillValue
     *            The output fill value
     */
    private void limitGrid(final Grid2DFloat grid, float inputFill,
            float maxLimit, float minLimit, float outputFillValue) {

        for (int x = 0; x < grid.getXdim(); x++) {
            for (int y = 0; y < grid.getYdim(); y++) {
                float val = grid.get(x, y);
                if (Float.isNaN(val) || val == inputFill) {
                    grid.set(x, y, outputFillValue);
                } else if (val != outputFillValue) {
                    if (val < minLimit) {
                        grid.set(x, y, minLimit);

                    } else if (val > maxLimit) {
                        grid.set(x, y, maxLimit);
                    }
                }
            }
        }
    }

    /**
     * Calculates the magnitude and direction grid
     * 
     * @param uGrid
     *            The input u grid
     * @param vGrid
     *            The input v grid
     * @param rotate
     *            The rotation flag
     * @param flip
     *            The flip flag
     * @param magGrid
     *            The output magnitude grid
     * @param dirGrid
     *            The output direction grid
     */
    private void calculateWindGrid(final Grid2DFloat uGrid,
            final Grid2DFloat vGrid, boolean rotate, boolean flip,
            Grid2DFloat magGrid, Grid2DFloat dirGrid) {

        for (int x = 0; x < uGrid.getXdim(); x++) {
            for (int y = 0; y < uGrid.getYdim(); y++) {

                float uVal = uGrid.get(x, y);
                float vVal = vGrid.get(x, y);

                float magValue = (float) Math.sqrt(uVal * uVal + vVal * vVal);
                float dirValue = (float) Math.toDegrees(Math.atan2(uVal, vVal));

                if (rotate) {
                    dirValue -= getRot(x, y);
                }
                if (flip) {
                    dirValue += 180;
                }
                while (dirValue >= 360.0f) {
                    dirValue -= 360;
                }
                while (dirValue < 0) {
                    dirValue += 360;
                }

                // Rounding errors can cause this to happen
                if (dirValue >= 360.0) {
                    dirValue = 0;
                }

                magGrid.set(x, y, magValue);
                dirGrid.set(x, y, dirValue);
            }
        }
    }

    /**
     * Resamples the data from the input grid location to the destination grid
     * location.
     * 
     * @param input
     *            The input data
     * @return The resampled data
     */
    private Grid2DByte resample(final Grid2DByte input) {

        if (input.getXdim() != sourceGloc.getNx()
                || input.getYdim() != sourceGloc.getNy()) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Cannot resample data. Input data dimensions do not match the input grid location");
            return input;
        }

        int dx = destinationGloc.getNx();
        int dy = destinationGloc.getNy();
        Grid2DByte data = new Grid2DByte(dx, dy);

        Coordinate srcCoord = null;
        int roundedX = 0;
        int roundedY = 0;
        try {
            for (int x = 0; x < dx; x++) {
                for (int y = 0; y < dy; y++) {
                    srcCoord = getSourceCoord(x, y);
                    roundedX = (int) Math.round(srcCoord.x);
                    roundedY = (int) Math.round(srcCoord.y);
                    if (roundedX < 0 || roundedY < 0
                            || roundedX >= input.getXdim()
                            || roundedY >= input.getYdim()) {
                        data.set(x, y, 0);
                    } else {
                        data.set(x, y, input.get(roundedX, roundedY));
                    }

                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error resampling byte data!", e);
        }

        return data;
    }

    private Coordinate getSourceCoord(int x, int y) throws Exception {
        GridGeometry2D destGeom = MapUtil.getGridGeometry(destinationGloc);
        GridGeometry2D srcGeom = MapUtil.getGridGeometry(sourceGloc);

        MathTransform destToCRS = destGeom.getGridToCRS();
        MathTransform srcGridFromCRS = srcGeom.getCRSToGrid2D();

        DirectPosition destLatLon = destToCRS.transform(new DirectPosition2D(x,
                y), null);
        DirectPosition srcCoord = srcGridFromCRS.transform(destLatLon, null);
        return new Coordinate(srcCoord.getOrdinate(0), srcCoord.getOrdinate(1));
    }

    /**
     * Resamples the data from the input grid location to the destination grid
     * location
     * 
     * @param input
     *            The input data
     * @return The resampled data
     * @throws TransformException
     * @throws FactoryException
     */
    private Grid2DFloat resample(final Grid2DFloat input)
            throws FactoryException, TransformException {

        GridGeometry2D sourceGeometry = MapUtil.getGridGeometry(sourceGloc);

        float[] data = input.getFloats();
        float[] f1 = null;

        /*
         * Checks the CRSs to see if they are the same. If they are, then
         * reprojection is not necessary, only resampling
         */
        if (rescale
                && sourceGeometry.getCoordinateReferenceSystem().toWKT()
                        .equals(destinationGloc.getCrsWKT())) {

            GridCoverageFactory factory = new GridCoverageFactory();

            Envelope inputEnvelope = sourceGeometry.getEnvelope();

            // Map the data into an array
            float[][] dataPoints = new float[input.getYdim()][input.getXdim()];

            int i = 0;
            for (int y = 0; y < input.getYdim(); y++) {
                for (int x = 0; x < input.getXdim(); x++) {
                    dataPoints[y][x] = data[i++];
                }
            }

            // Construct the source grid coverage object
            GridCoverage2D inputGC = factory.create("in", dataPoints,
                    inputEnvelope);

            float scaleX = ((float) destinationGloc.getNx() / (float) sourceGloc
                    .getNx());
            float scaleY = ((float) destinationGloc.getNy() / (float) sourceGloc
                    .getNy());
            PlanarImage image = scaleGrid(inputGC.getRenderedImage(), scaleX,
                    scaleY);
            Raster rasterData = image.getData();
            f1 = rasterData.getPixels(rasterData.getMinX(),
                    rasterData.getMinY(), rasterData.getWidth(),
                    rasterData.getHeight(), f1);
        } else {
            GridGeometry2D destGeometry = MapUtil
                    .getGridGeometry(destinationGloc);
            synchronized (this) {
                if (interp == null) {
                    interp = new GridReprojection(sourceGeometry, destGeometry);
                    interp.computeTransformTable();
                }
            }
            DataSource source = new FloatArrayWrapper(data, sourceGeometry);
            f1 = interp.reprojectedGrid(new BilinearInterpolation(), source,
                    new FloatArrayWrapper(destGeometry)).getArray();
        }

        // Remap the the output data into a Grid2DFloat object

        Grid2DFloat retVal = new Grid2DFloat(destinationGloc.getNx(),
                destinationGloc.getNy(), f1);

        JAI.getDefaultInstance().getTileCache().flush();

        return retVal;
    }

    /**
     * Resamples the grid to a new resolution using scale values
     * 
     * @param img
     *            The image to scale
     * @param xScale
     *            The new horizontal resolution
     * @param yScale
     *            The new vertical resolution
     * @return The resampled image
     */
    private PlanarImage scaleGrid(RenderedImage img, float xScale, float yScale) {

        PlanarImage scaledImg;

        ParameterBlockJAI param = new ParameterBlockJAI("Scale");
        param.addSource(img);
        param.setParameter("xScale", xScale);
        param.setParameter("yScale", yScale);
        Interpolation interpol = Interpolation
                .getInstance(Interpolation.INTERP_BICUBIC);
        RenderingHints hint = new RenderingHints(JAI.KEY_BORDER_EXTENDER,
                BorderExtender.createInstance(BorderExtender.BORDER_COPY));

        param.setParameter("interpolation", interpol);

        scaledImg = JAI.create("Scale", param, hint);

        return scaledImg;

    }

    private float getRot(int x, int y) {
        if (this.rotation == null || !this.rotation.isValid()) {
            this.rotation = new Grid2DFloat(destinationGloc.gridSize().x,
                    destinationGloc.gridSize().y);
            for (int x1 = 0; x1 < rotation.getXdim(); x1++) {
                for (int y1 = 0; y1 < rotation.getYdim(); y1++) {
                    Coordinate llc = destinationGloc
                            .latLonCenter(new Coordinate(x1, y1));
                    this.rotation.set(x1, y1,
                            (float) (180 - MapUtil.rotation(llc, sourceGloc)));
                }
            }
        }
        return rotation.get(x, y);
    }
}
