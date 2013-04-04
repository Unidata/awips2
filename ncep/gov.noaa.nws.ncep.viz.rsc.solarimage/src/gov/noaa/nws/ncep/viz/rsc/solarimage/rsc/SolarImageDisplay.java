package gov.noaa.nws.ncep.viz.rsc.solarimage.rsc;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;
import gov.noaa.nws.ncep.viz.rsc.solarimage.LogSolarImageDataCallback;
import gov.noaa.nws.ncep.viz.rsc.solarimage.SolarImageDataCallback;
import gov.noaa.nws.ncep.viz.rsc.solarimage.util.HeaderData;
import gov.noaa.nws.ncep.viz.rsc.solarimage.wcs.CSConversions;
import gov.noaa.nws.ncep.viz.rsc.solarimage.wcs.WCSConverter;

import java.awt.geom.AffineTransform;
import java.io.File;
import java.io.FileNotFoundException;
import java.text.DecimalFormat;
import java.util.LinkedHashMap;
import java.util.Map;

import nom.tam.fits.Header;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.referencing.operation.transform.AffineTransform2D;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.NoninvertibleTransformException;
import org.opengis.referencing.operation.TransformException;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.graph.GraphProperties;
import com.vividsolutions.jts.geom.Coordinate;

/**
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer         Description
 * ------------ ---------- -----------      --------------------------
 * 02/21/2013   958        qzhou, sgurung   Initial creation
 * 
 * </pre>
 * 
 * @author qzhou, sgurung
 * @version 1.0
 */

public class SolarImageDisplay implements IRenderable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SolarImageDisplay.class);

    private IImage image = null;

    private float brightness = 1.0f;

    private float contrast = 1.0f;

    private boolean isInterpolated = true;

    private SolarImageRecord record;

    private SolarImageDataCallback dataCallback;

    private ColorMapParameters colorMapParameters;

    private GeneralGridGeometry gridGeom;

    private boolean logConvert;

    private WCSConverter transform;

    private PixelCoverage extent = null;

    private MathTransform worldToPixel;

    private MathTransform pixelToWorld;
    
    private HeaderData headerData;
    private Header header;
    
    private int nx;
    private int ny;
    
    private double scale;
    private AffineTransform at;
    
    public CSConversions csConv;

    public SolarImageDisplay(SolarImageRecord record, ColorMapParameters cmp,
            GeneralGridGeometry gridGeometry, boolean logConvert) throws VizException {
        this.record = record;
        this.colorMapParameters = cmp;
        this.gridGeom = gridGeometry;
        this.logConvert = logConvert;
        if (this.logConvert)
            dataCallback = new LogSolarImageDataCallback(record);
        else
            dataCallback = new SolarImageDataCallback(record);
            
        header = dataCallback.imgData.getHeader();
//        if (header == null)
//            populateHeader();
        nx = dataCallback.imgData.getNx();
        ny = dataCallback.imgData.getNy();
        
        headerData = new HeaderData(record);
        csConv = new CSConversions(headerData);
        (new HeaderData(record)).calculateRsun();
        
    }
    
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        if (image == null) {
            // ColormappedRenderedImageCallback imageCallback = new
            // ColormappedRenderedImageCallback(dataCallback, colorMapParameters);
            // image = target.initializeRaster(imageCallback);
            image = target.getExtension(IColormappedImageExtension.class)
                    .initializeRaster(dataCallback, colorMapParameters);
        }
       
       if (transform == null || extent == null) {
            try {
            	transform = new WCSConverter(header);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                throw new VizException(
                        "Could not create image to world coordinate transform",
                        e);
            }
            
            
            double[] ll = transform.imageToWorld(new double[] { -0.5, -0.5 });
   
            double[] lr = transform.imageToWorld(new double[] {
            		nx - 0.5, -0.5 });
            double[] ur = transform.imageToWorld(new double[] {
            		nx - 0.5, ny - 0.5 });
            double[] ul = transform.imageToWorld(new double[] { -0.5,
                    ny - 0.5 });

            double minX = Math.min(ll[0], ul[0]);
            double maxX = Math.max(lr[0], ur[0]);
            double minY = Math.min(ll[1], lr[1]);
            double maxY = Math.max(ul[1], ur[1]);
            
            if ((maxX - minX) > (maxY - minY)) {
                scale = gridGeom.getEnvelope().getSpan(0) / (maxX - minX);
            } else {
                scale = gridGeom.getEnvelope().getSpan(1) / (maxY - minY);
            }
           
            double[] center;
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
                at = AffineTransform.getTranslateInstance(
                        center[0], center[1]);
                at.concatenate(AffineTransform.getScaleInstance(scale, -scale));
                worldToPixel = new AffineTransform2D(at);
                // System.out.println(worldToPixel.toWKT());
                pixelToWorld = worldToPixel.inverse();
                // System.out.println(pixelToWorld.toWKT());

                worldToPixel.transform(ll, 0, llp, 0, 1);
                worldToPixel.transform(lr, 0, lrp, 0, 1);
                worldToPixel.transform(ul, 0, ulp, 0, 1);
                worldToPixel.transform(ur, 0, urp, 0, 1);
            } catch (NoninvertibleTransformException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                throw new VizException(
                        "Could not create world to pixel coordinate transform",
                        e);
            } catch (TransformException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                throw new VizException(
                        "Could not create pixel extent for image", e);
            }

            extent = new PixelCoverage(new Coordinate(ulp[0], ulp[1]),
                    new Coordinate(urp[0], urp[1]), new Coordinate(lrp[0],
                            lrp[1]), new Coordinate(llp[0], llp[1]));

         
        }
        image.setContrast(contrast);
        image.setBrightness(brightness);
        image.setInterpolated(isInterpolated);
        target.drawRaster(image, extent, paintProps);
        
        
    }

    public void dispose() {

        if (this.image != null)
            this.image.dispose();
    }

    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {

        Map<String, Object> map = new LinkedHashMap<String, Object>();
   
        try {
            double[] pixel = new double[] { coord.asLatLon().x,
                    coord.asLatLon().y };
            
            double[] world = new double[2];
            pixelToWorld.transform(pixel, 0, world, 0, 1);  
            //world = formatValue(world);
            map.put("HCC", new Coordinate(formatValue(world[0]), formatValue(world[1])));//move down

            double image[] = transform.WorldToImage(world);
            // remember that image was flipped during earlier and stored with first pixel as top left
            image[1] = ny - 1 - image[1];
            map.put("Image", new Coordinate(formatValue(image[0]), formatValue(image[1])));
           
            double[] locWorld = (new CSConversions(headerData)).heliocentricToHeliographic(world, false);            
            if (locWorld[0] > 180)
            	locWorld[0] = locWorld[0] - 360;
            else if (locWorld[0] < -180)
            	locWorld[0] = locWorld[0] +  360;
            map.put("StonyHurst", new Coordinate(formatValue(locWorld[0]), formatValue(locWorld[1])));
            
            locWorld = (new CSConversions(headerData)).heliocentricToHeliographic(world, true); 
            if (locWorld[0] > 360)
            	locWorld[0] = locWorld[0] - 360;
            else if (locWorld[0] < -360)
            	locWorld[0] = locWorld[0] +  360;
            map.put("Carrington", new Coordinate(formatValue(locWorld[0]), formatValue(locWorld[1])));
                     
            long x = Math.round(image[0]);
            long y = Math.round(image[1]);
            double val = getImageValue((int) x, (int) y);
            map.put("Display Value", new Double(formatValue(val)));

            map.put("Data Value",
                    new Double(formatValue(dataCallback.getOriginalValue(val))));

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
    	}
    	catch (Exception e) {
    		return val;
    	}
    	
        return newVal;
    } 

    public double getImageValue(int x, int y) {
        double value = Float.NaN;
        if (isInImageRange(x, y)) {
            if (image instanceof IColormappedImage) {
                value = ((IColormappedImage) image).getValue(x, y);
            }
        }
        return value;
    }

    private boolean isInImageRange(int x, int y) {

//        return (x >= 0) && (x < record.getNx()) && (y >= 0)
//                && (y < record.getNy());
        return (x >= 0) && (x < nx) && (y >= 0)
        && (y < ny);
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

    public PixelCoverage getPixelCoverage() {
        return extent;
    }
    
    public MathTransform getWorldToPixel() {
        return worldToPixel;
    }
    
    public CSConversions getCSConversions() {
    	return csConv;
    }
    
    public HeaderData getHeaderData() {
    	return headerData;
    }
    
}
