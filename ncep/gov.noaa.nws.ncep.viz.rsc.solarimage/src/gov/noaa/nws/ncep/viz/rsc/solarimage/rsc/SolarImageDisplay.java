package gov.noaa.nws.ncep.viz.rsc.solarimage.rsc;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;
import gov.noaa.nws.ncep.viz.rsc.solarimage.LogSolarImageDataCallback;
import gov.noaa.nws.ncep.viz.rsc.solarimage.SolarImageDataCallback;
import gov.noaa.nws.ncep.viz.rsc.solarimage.util.ImageData;
import gov.noaa.nws.ncep.viz.rsc.solarimage.util.ImageFunctionParser;
import gov.noaa.nws.ncep.viz.rsc.solarimage.wcs.CSConversions;
import gov.noaa.nws.ncep.viz.rsc.solarimage.wcs.CylindricalConverter;
import gov.noaa.nws.ncep.viz.rsc.solarimage.wcs.WCSConverter;

import java.awt.geom.AffineTransform;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.referencing.operation.transform.AffineTransform2D;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.graph.GraphProperties;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Display the image
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer         Description
 * ------------ ---------- -----------      --------------------------
 * 02/21/2013   958        qzhou, sgurung   Initial creation
 * 03/19/2013   958        qzhou, sgurung   implemented colormap changing
 * 03/28/2013   958        qzhou            Added location adjusting for THEMATIC
 * 11/04/2013   958        qzhou            Refacted code and Combined CylindricalDisplay to here
 * 11/07/2013   958        qzhou            Fixed problems of sampling and latlon curve direction and North direction
 * 11/27/2013   958        sgurung, qzhou   Fix for OutOfMemoryError: get data from hdf5 one frame at a time during paint,
 *                                          combine callback classes (use same for normal and cylindrical)
 * 12/31/2013   1046       qzhou            Added image function feature.
 *                                          Added getFunctioningImageData. Fixed OutOfMemoryError for imageFunction.
 *                                          Added imgDifference, and work on poler rotated image differencing(4 types).
 *                                          Modified getImageValue. Modified paint. 
 *                                          Solved transform reverse caused cylindrical image & sampling problems.
 * 01/15/2013   958        qzhou            Solved problem that looping on a lot frames, cylindrical image become normal image.
 * </pre>
 * 
 * @author qzhou, sgurung
 * @version 1.0
 */

public class SolarImageDisplay implements IRenderable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SolarImageDisplay.class);

    private SolarImageRecord record;

    private IImage image = null;

    private float brightness = 1.0f;

    private float contrast = 1.0f;

    private boolean isInterpolated = true;

    private SolarImageDataCallback dataCallback;

    private ColorMapParameters colorMapParameters;

    private boolean isColorMapChanged = false;

    private final boolean logConvert;

    private WCSConverter transform;

    private CylindricalConverter transformCylin;

    private PixelCoverage extent = null;

    private AffineTransform at;

    private MathTransform worldToPixel;

    private MathTransform pixelToWorld;

    private final GeneralGridGeometry gridGeom;

    private double scale;

    protected CSConversions csConv = null;;

    private int cylindrical = 0; // 0--no cylindrical, 1--stony, 2--carrington

    // private boolean projectionChanged = false;

    private boolean isCarr = false;

    private boolean recreateLatLonOverlay = false;

    private LatLonOverlay latLonOverlay;

    private int latLonInterval = 0; // 0, 15,30,45,60

    private String imageFunction;

    protected TreeMap<Long, SolarImageRecord> functioningRecordMap = new TreeMap<Long, SolarImageRecord>();

    private long currTime;

    private int imgDiff = 0;

    public SolarImageDisplay(SolarImageRecord rec, long currTime,
            GeneralGridGeometry gridGeometry, boolean logConvert,
            int cylindrical, String imageFunction,
            TreeMap<Long, SolarImageRecord> functioningRecordMap)
            throws VizException {

        this.gridGeom = gridGeometry;
        this.logConvert = logConvert;
        this.cylindrical = cylindrical;
        this.record = rec;
        this.currTime = currTime;
        this.imageFunction = imageFunction;
        this.functioningRecordMap = functioningRecordMap;

    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        /*
         * remove the following conditon to create a new image on every single
         * paint(). This is not efficient. But solves the problem that looping
         * on a lot frames, cylindrical image become normal image. Another way
         * to solve this problem is to create and use CylindDataCallback and
         * LogCylindDataCallback classes.
         */
        // if (image == null || projectionChanged || this.getColorMapChanged())
        // {

        if (this.logConvert) {
            dataCallback = new LogSolarImageDataCallback(record);

        } else {
            dataCallback = new SolarImageDataCallback(record);
        }

        csConv = new CSConversions(dataCallback.getHeaderData());
        if (image != null)
            image.dispose();
        image = target.getExtension(IColormappedImageExtension.class)
                .initializeRaster(dataCallback, this.getColorMapParameters());
        // }

        if (transform == null) {
            try {
                transform = new WCSConverter(dataCallback.getHeaderData()
                        .getHeader(), cylindrical);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                throw new VizException(
                        "Could not create image to world coordinate transform",
                        e);
            }
        }

        double[] ll = transform.imageToWorld(new double[] { 0, 0 });
        double[] lr = transform.imageToWorld(new double[] {
                dataCallback.getImageNx(), 0 });
        double[] ur = transform.imageToWorld(new double[] {
                dataCallback.getImageNx(), dataCallback.getImageNy() });
        double[] ul = transform.imageToWorld(new double[] { 0,
                dataCallback.getImageNy() });

        /*
         * image functioning
         */
        String[] elements = ImageFunctionParser.parse(imageFunction);

        if (elements != null && elements.length > 1) {

            // image differencing
            if (elements[0].startsWith("run") || elements[0].startsWith("base")) {
                imgDiff = 11;
                dataCallback.setImgDiff(imgDiff);

            }
            // other image function
            // else if (...) {
            // imgDiff = 12;
            // }

            dataCallback
                    .setImageData(getFunctioningImageData(target, elements));

            if (image != null)
                image.dispose();
            image = target.getExtension(IColormappedImageExtension.class)
                    .initializeRaster(dataCallback,
                            this.getColorMapParameters());

        }

        /*
         * create new cylindrical image
         */
        if (cylindrical != 0) {

            image.stage(); // for using in getImageValue
            dataCallback.setCylindrical(cylindrical);

            // if (projectionChanged || this.getColorMapChanged()) {

            dataCallback.setImageData(getCylindImageData());

            if (image != null)
                image.dispose();
            image = target.getExtension(IColormappedImageExtension.class)
                    .initializeRaster(dataCallback,
                            this.getColorMapParameters());
            // }

            if ((transformCylin == null) || (extent == null)) {

                try {
                    transformCylin = new CylindricalConverter();
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                    throw new VizException(
                            "Could not create image to world coordinate transform",
                            e);

                }
            }

            ll = transformCylin.imageToWorld(new double[] { 0, 0 });
            lr = transformCylin.imageToWorld(new double[] { 360, 0 });
            ur = transformCylin.imageToWorld(new double[] { 360, 180 });
            ul = transformCylin.imageToWorld(new double[] { 0, 180 });

        }

        /*
         * for all
         */
        double minX = Math.min(ll[0], ul[0]);
        double maxX = Math.max(lr[0], ur[0]);
        double minY = Math.min(ll[1], lr[1]);
        double maxY = Math.max(ul[1], ur[1]);

        if ((maxX - minX) > (maxY - minY)) {
            scale = gridGeom.getEnvelope().getSpan(0) / (maxX - minX);
        } else {
            scale = gridGeom.getEnvelope().getSpan(1) / (maxY - minY);
        }

        double[] center = new double[2];
        if (paintProps instanceof GraphProperties) {
            center = ((GraphProperties) paintProps).getWorldExtent()
                    .getCenter();
        } else {
            center = paintProps.getView().getExtent().getCenter();
        }

        center[0] = gridGeom.getEnvelope().getMedian(0);
        center[1] = gridGeom.getEnvelope().getMedian(1);

        double[] llp = new double[2];
        double[] lrp = new double[2];
        double[] ulp = new double[2];
        double[] urp = new double[2];

        try {
            at = AffineTransform.getTranslateInstance(center[0], center[1]);
            at.concatenate(AffineTransform.getScaleInstance(scale, scale));
            worldToPixel = new AffineTransform2D(at);
            pixelToWorld = worldToPixel.inverse();

            worldToPixel.transform(ll, 0, llp, 0, 1);
            worldToPixel.transform(lr, 0, lrp, 0, 1);
            worldToPixel.transform(ul, 0, ulp, 0, 1);
            worldToPixel.transform(ur, 0, urp, 0, 1);

        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            throw new VizException("Could not create pixel extent for image", e);
        }

        extent = new PixelCoverage(new Coordinate(ulp[0], ulp[1]),
                new Coordinate(urp[0], urp[1]), new Coordinate(lrp[0], lrp[1]),
                new Coordinate(llp[0], llp[1]));

        image.setContrast(contrast);
        image.setBrightness(brightness);
        image.setInterpolated(isInterpolated);
        target.drawRaster(image, extent, paintProps);

        if (dataCallback != null) {
            dataCallback.setImageData(null);

        }

        // projectionChanged = false;

    }

    public void drawOverlay(IGraphicsTarget target, IDescriptor descriptor,
            PaintProperties paintProps, boolean isCarrington)
            throws VizException {

        try {
            if (latLonOverlay == null || recreateLatLonOverlay
                    || latLonOverlay.isCarrington() != isCarrington) {
                latLonOverlay = new LatLonOverlay(this, descriptor,
                        latLonInterval, paintProps, isCarrington, cylindrical,
                        dataCallback.getHeaderData());
                latLonOverlay.createLatLines(target);
                latLonOverlay.createLonLines(target);
            }

            latLonOverlay.drawLatLines(target);
            latLonOverlay.drawLonLines(target);
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        recreateLatLonOverlay = false;
    }

    public ImageData getCylindImageData() {

        double hgln = dataCallback.getHeaderData().getHgln();
        double crln = dataCallback.getHeaderData().getCrln();
        double L0 = dataCallback.getHeaderData().getL0();

        ImageData newImageData = new ImageData();
        newImageData.setNx(360);
        newImageData.setNy(180);

        float[][] newImgArray = new float[newImageData.getNy()][newImageData
                .getNx()];
        double[] centric = new double[2];

        int primeMeridian = 0; // related to the center of the image

        if (cylindrical == 2) {
            isCarr = true;
        } else {
            isCarr = false;
        }

        if (cylindrical == 2) {
            if (crln != 0) {
                primeMeridian = 180 + (int) crln; // Canvas started from -180
            } else {
                primeMeridian = 180 + (int) (L0 + hgln);
            }

            if (primeMeridian < 360) {
                primeMeridian = primeMeridian + 360;
            }
            if (primeMeridian > 360) {
                primeMeridian = primeMeridian - 360;
            }
        } else if (cylindrical == 1) {
            primeMeridian = 180 + (int) hgln; // Canvas started from -180
        }

        WCSConverter transformAlt = null;
        try {
            transformAlt = new WCSConverter(dataCallback.getHeaderData()
                    .getHeader(), cylindrical);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        for (int i = 0; i < newImageData.getNy(); i++) {
            for (int j = 0; j < newImageData.getNx(); j++) {
                if (((j >= (primeMeridian - 90)) && (j <= (primeMeridian + 90)))
                        || (((primeMeridian + 90) > 360) && (j >= 0) && (j < ((primeMeridian + 90) - 360)))
                        || (((primeMeridian - 90) < 0) && (j <= 360) && (j > ((primeMeridian - 90) + 360)))) {

                    double[] latlon = new double[] { j - 180, i - 90 };

                    centric = csConv.heliographicToHeliocentric(latlon, isCarr);

                    double img[] = transformAlt.WorldToImage(centric);

                    double val = getImageValue((int) (Math.round(img[0])),
                            (int) (Math.round(img[1])), image);
                    double imgVal = logConvert ? new Double(Math.pow(10.0,
                            formatValue(val))) : val;

                    newImgArray[i][j] = (float) (imgVal);
                }

                else if (dataCallback.getHeaderData().isSdoHmi()) {
                    // hmi default is not 0
                    newImgArray[i][j] = (-1500);
                }
            }
        }

        int n = 0;
        float[] imgDataRec = new float[newImageData.getNy()
                * newImageData.getNx()];
        // for (int j = 180 - 1; j >= 0; j--) { // Reverse order of rows
        for (int i = 0; i < newImageData.getNy(); i++) {
            for (int j = 0; j < newImageData.getNx(); j++) {
                imgDataRec[n++] = newImgArray[i][j];
            }
        }

        newImageData.setImageValues(imgDataRec);

        return newImageData;
    }

    /*
     * get new imageData and directly set to the dataCallback
     */
    public ImageData getFunctioningImageData(IGraphicsTarget target,
            String[] elements) throws VizException {
        // the following objects are declared as lists for general
        ImageData functioningImageData = null;
        List<SolarImageDataCallback> functioningCallback = new ArrayList<SolarImageDataCallback>();
        List<SolarImageRecord> functioningRecords = new ArrayList<SolarImageRecord>();

        Iterator iterator = functioningRecordMap.entrySet().iterator();
        while (iterator.hasNext()) {
            @SuppressWarnings("unchecked")
            Map.Entry<Long, SolarImageRecord> entry = (Map.Entry<Long, SolarImageRecord>) iterator
                    .next();

            long time = (Long) entry.getKey();
            if (elements[0].startsWith("run") || elements[0].startsWith("base")) {
                if (time == currTime) {
                    functioningRecords.add((SolarImageRecord) entry.getValue());
                    break;
                }
            } else {
                functioningRecords.add((SolarImageRecord) entry.getValue());
            }
        }

        if (functioningRecords.size() > 0) {
            for (int i = 0; i < functioningRecords.size(); i++) {
                if (this.logConvert) {
                    functioningCallback.add(new LogSolarImageDataCallback(
                            functioningRecords.get(i)));

                } else {
                    functioningCallback.add(new SolarImageDataCallback(
                            functioningRecords.get(i)));
                }
            }
        } else {
            return dataCallback.getImageData();
        }

        if (functioningCallback.size() > 0) {

            if (elements[0].startsWith("run") || elements[0].startsWith("base")) {
                // functioningCallback size = 1
                if (dataCallback.getImageNx() == functioningCallback.get(0)
                        .getImageNx()
                        && dataCallback.getImageNy() == functioningCallback
                                .get(0).getImageNy()) {

                    functioningImageData = dataCallback.getImageData();
                    functioningImageData
                            .setImageValues(imgDifference(functioningCallback
                                    .get(0)));

                }
            }
        }

        for (int i = 0; i < functioningCallback.size(); i++) {
            if (functioningCallback.get(i) != null) {
                functioningCallback.get(i).setImageData(null);
                functioningCallback.remove(i);

            }
        }

        return functioningImageData;
    }

    public float[] imgDifference(SolarImageDataCallback callback)
            throws VizException {

        int currImageDataSize = dataCallback.getImageNx()
                * dataCallback.getImageNy();
        float[] imgDataRec = new float[currImageDataSize];

        ImageData currImageData = dataCallback.getImageData();
        if (currImageData == null) { // now not happen
            if (this.logConvert) {
                dataCallback = new LogSolarImageDataCallback(record);
                currImageData = dataCallback.getImageData();
            } else {
                dataCallback = new SolarImageDataCallback(record);
                currImageData = dataCallback.getImageData();
            }
        }

        ImageData functioningImageData = callback.getImageData();

        String[] elements = ImageFunctionParser.parse(imageFunction);

        WCSConverter funcTransform = null;
        try {
            funcTransform = new WCSConverter(callback.getHeaderData()
                    .getHeader(), cylindrical);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        int n = 0;

        if (!transform.hasEqualNavigation()) {

            for (int i = 0; i < currImageData.getNy(); i++) {
                for (int j = 0; j < currImageData.getNx(); j++) {

                    double[] world = transform
                            .imageToWorld(new double[] { i, j });
                    double[] img2 = funcTransform.WorldToImage(world);

                    int ii = (int) Math.round(img2[0]);
                    int jj = (int) Math.round(img2[1]);

                    if (ii < 0 || jj < 0 || ii >= currImageData.getNy()
                            || jj >= currImageData.getNx()) {

                        imgDataRec[n] = Float.NaN;
                    } else {
                        imgDataRec[n] = getImgDataRec(elements[0],
                                currImageData.getImageValues()[j + i
                                        * currImageData.getNx()],
                                functioningImageData.getImageValues()[jj + ii
                                        * currImageData.getNx()]);
                    }
                    n++;
                }
            }
        } else {
            for (int i = 0; i < currImageDataSize; i++) {
                imgDataRec[n] = getImgDataRec(elements[0],
                        currImageData.getImageValues()[i],
                        functioningImageData.getImageValues()[i]);

                n++;
            }
        }

        return imgDataRec;
    }

    public float getImgDataRec(String element, float val1, float val2) {
        float dataRec = 0.0f;

        if (element.contains("Ratio")) {
            dataRec = val1 / val2;

        } else if (element.contains("Diff")) {
            dataRec = val1 - val2;
        }

        return dataRec;
    }

    public void dispose() {

        if (this.image != null)
            this.image.dispose();

        if (latLonOverlay != null) {
            latLonOverlay.dispose();
        }

    }

    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {

        Map<String, Object> map = new LinkedHashMap<String, Object>();

        try {
            double[] pixel = new double[] { coord.asLatLon().x,
                    coord.asLatLon().y };

            double[] world = new double[2];
            pixelToWorld.transform(pixel, 0, world, 0, 1);

            map.put("HCC", new Coordinate(formatValue(world[0]),
                    formatValue(world[1])));// move down

            double img[] = transform.WorldToImageSamp(world);

            if (cylindrical != 0) {
                img = transformCylin.WorldToImage(world);
            }
            map.put("Image", new Coordinate(formatValue(img[0]),
                    formatValue(img[1])));

            if (cylindrical == 1) {
                map.put("StonyHurst", new Coordinate(formatValue(world[0]),
                        formatValue(world[1])));
            } else if (cylindrical == 2) {
                map.put("Carrington", new Coordinate(formatValue(world[0]),
                        formatValue(world[1])));
            } else {
                double[] locWorld = csConv.heliocentricToHeliographic(world,
                        false);

                if (locWorld[0] >= 180)
                    locWorld[0] = locWorld[0] - 360;
                else if (locWorld[0] <= -180)
                    locWorld[0] = locWorld[0] + 360;

                map.put("StonyHurst", new Coordinate(formatValue(locWorld[0]),
                        formatValue(locWorld[1])));

                locWorld = csConv.heliocentricToHeliographic(world, true);
                if (locWorld[0] >= 180)
                    locWorld[0] = locWorld[0] - 360;
                else if (locWorld[0] <= -180)
                    locWorld[0] = locWorld[0] + 360;

                map.put("Carrington", new Coordinate(formatValue(locWorld[0]),
                        formatValue(locWorld[1])));
            }

            double val = getImageValue((int) Math.round(img[0]),
                    (int) Math.round(img[1]), image);
            map.put("Display Value", new Double(formatValue(val)));

            map.put("Data Value",
                    new Double(formatValue(dataCallback.getOriginalValue(val))));

            // remove cylindrical invalid area map
            if (cylindrical != 0 && val == Double.POSITIVE_INFINITY
                    || val == Double.NEGATIVE_INFINITY) {
                return null;
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        return map;
    }

    public double formatValue(double val) {

        DecimalFormat df = new DecimalFormat("#.#####");
        double newVal = val;
        try {
            String valStr = df.format(val);
            Double valDbl = Double.parseDouble(valStr);
            newVal = valDbl;
        } catch (Exception e) {
            return val;
        }

        return newVal;
    }

    public double getImageValue(int x, int y, IImage image) {
        double value = Float.NaN;

        if (isInImageRange(x, y)) {
            if (image instanceof IColormappedImage) {
                value = ((IColormappedImage) image).getValue(x, y);
            }
        }

        return value;
    }

    private boolean isInImageRange(int x, int y) {
        return (x >= 0) && (x < dataCallback.getImageNx()) && (y >= 0)
                && (y < dataCallback.getImageNy());
    }

    public void setBrightness(float brightness) {
        this.brightness = brightness;
    }

    public void setContrast(float contrast) {
        this.contrast = contrast;
    }

    public void setInterpolationState(boolean isInterpolated) {
        this.isInterpolated = isInterpolated;
    }

    public ColorMapParameters getColorMapParameters() {
        return colorMapParameters;
    }

    public void setColorMapParameters(ColorMapParameters params) {
        this.colorMapParameters = params;
    }

    public boolean getColorMapChanged() {
        return isColorMapChanged;
    }

    public void setColorMapChanged(boolean val) {
        this.isColorMapChanged = val;
    }

    public PixelCoverage getPixelCoverage() {
        return extent;
    }

    public MathTransform getWorldToPixel() {
        return worldToPixel;
    }

    public MathTransform getPixelToWorld() {
        return pixelToWorld;
    }

    public CSConversions getCSConversions() {
        return csConv;
    }

    public int getCylindrical() {
        return cylindrical;
    }

    public void setCylindrical(int cylindrical) {
        if (this.cylindrical != cylindrical) {
            this.cylindrical = cylindrical;
            recreateLatLonOverlay = true;
            // projectionChanged = true;
        }
    }

    public int getLatLonInterval() {
        return latLonInterval;
    }

    public void setLatLonInterval(int latLonInterval) {
        if (this.latLonInterval != latLonInterval) {
            this.latLonInterval = latLonInterval;
            recreateLatLonOverlay = true;
        }
    }

    public SolarImageRecord getImageRecord() {
        return record;
    }

    public void setImageRecord(SolarImageRecord record) {
        this.record = record;
    }

    public Coordinate getPixelCoordFromLatLon(Coordinate latlonCoord) {
        Coordinate pixelCoord = null;

        try {

            double[] pixel = new double[2];
            if (cylindrical == 0) {
                double[] latlon = { latlonCoord.x, latlonCoord.y };
                double[] centric = csConv.heliographicToHeliocentric(latlon,
                        false);
                worldToPixel.transform(centric, 0, pixel, 0, 1);
                pixelCoord = new Coordinate(pixel[0], pixel[1]);
            } else {
                double x = (latlonCoord.x > 180) ? (latlonCoord.x - 360.0)
                        : latlonCoord.x;
                double[] latlon = { x, latlonCoord.y };
                worldToPixel.transform(latlon, 0, pixel, 0, 1);
                pixelCoord = new Coordinate(pixel[0], pixel[1]);
            }

        } catch (Exception e) {
            return null;
        }

        return pixelCoord;
    }

    public Coordinate getLatLonFromPixel(Coordinate pixelCoord) {
        Coordinate latlonCoord = null;

        try {
            double[] pixel = { pixelCoord.x, pixelCoord.y };

            double[] world = new double[2];
            pixelToWorld.transform(pixel, 0, world, 0, 1);

            if (cylindrical == 0) {
                double[] latlon = csConv.heliocentricToHeliographic(world,
                        false);
                // if (latlon[0] >= 180)
                // latlon[0] = latlon[0] - 360;
                // else if (latlon[0] <= -180)
                // latlon[0] = latlon[0] + 360;
                latlonCoord = new Coordinate(latlon[0], latlon[1]);
            } else {
                latlonCoord = new Coordinate(world[0], world[1]);
            }

        } catch (Exception e) {
            return null;
        }

        return latlonCoord;
    }

}
