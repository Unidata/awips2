package gov.noaa.nws.ncep.viz.rsc.solarimage.rsc;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;
import gov.noaa.nws.ncep.viz.rsc.solarimage.CylindCedDataCallback;
import gov.noaa.nws.ncep.viz.rsc.solarimage.LogCylindCedDataCallback;
import gov.noaa.nws.ncep.viz.rsc.solarimage.LogSolarImageDataCallback;
import gov.noaa.nws.ncep.viz.rsc.solarimage.SolarImageDataCallback;
import gov.noaa.nws.ncep.viz.rsc.solarimage.util.HeaderData;
import gov.noaa.nws.ncep.viz.rsc.solarimage.util.ImageData;
import gov.noaa.nws.ncep.viz.rsc.solarimage.wcs.CSConversions;
import gov.noaa.nws.ncep.viz.rsc.solarimage.wcs.CylindricalConverter;
import gov.noaa.nws.ncep.viz.rsc.solarimage.wcs.WCSConverter;

import java.awt.geom.AffineTransform;
import java.text.DecimalFormat;
import java.util.LinkedHashMap;
import java.util.Map;

import nom.tam.fits.Header;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.referencing.operation.transform.AffineTransform2D;
import org.opengis.referencing.operation.MathTransform;
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
import com.raytheon.viz.core.style.image.DataScale;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Display Cylindrical
 * <pre>
 * 
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   --------------------------
 * 04/01/2013   958        qzhou         Initial creation
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */

public class CylindCedDisplay implements IRenderable {
	
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CylindCedDisplay.class);

    private IImage image = null;
    private IImage cylindImage = null;

    private float brightness = 1.0f;
    private float contrast = 1.0f;
    private boolean isInterpolated = true;

    private SolarImageRecord record;

    private CylindCedDataCallback dataCallback;
    private SolarImageDataCallback solarDataCallback;

    private ColorMapParameters colorMapParameters;    
    private boolean isColorMapChanged =  false;

    private GeneralGridGeometry gridGeom;

    private boolean logConvert;
    
    private WCSConverter transformOld;
    private CylindricalConverter transform;
    private AffineTransform at;
    
    private MathTransform worldToPixel;
    private MathTransform pixelToWorld;
    
    private double scale;
    private PixelCoverage extent = null;


    private ImageData imageData;
    private ImageData cylindImageData;
    private HeaderData headerData;
    private Header header;
    private CSConversions csConv;
    
    private int nx;
    private int ny;
    private double hgln ;
    private double crln;
    private double L0; 
	
    private DataScale dataScale = null;
    
    private int cylindrical = 0; //0--no cylindrical, 1--stony, 2--carrington   
    private boolean isCarr = false;
    private int status = 0;
    
	
    public CylindCedDisplay(SolarImageRecord rec, ColorMapParameters cmp, GeneralGridGeometry gridGeometry, 
                boolean logConvert, DataScale dataScale, int cylindrical, int latLonInterval) throws VizException { 
        this.record = rec;
        this.colorMapParameters = cmp;
        this.gridGeom = gridGeometry;
        this.logConvert = logConvert;
        this.dataScale = dataScale;
        this.cylindrical = cylindrical; 
        status = 0;
        if (this.logConvert){
            solarDataCallback = new LogSolarImageDataCallback(record);
            imageData = solarDataCallback.getImageData();
        }
        else {
        	solarDataCallback = new SolarImageDataCallback(record);
            imageData = solarDataCallback.getImageData();
        }
        
        header = imageData.getHeader();      
        headerData = new HeaderData(record);
        
        csConv = new CSConversions(headerData);
        nx = headerData.getNx();
        ny = headerData.getNy();
    	hgln = headerData.getHgln();
    	crln = headerData.getCrln();
    	L0 = headerData.getL0(); 
    	//System.out.println("Angles "+hgln+" " +crln+" " +L0);
    }
    
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
    	// old image
    	if (image == null ) {//|| isColorMapChanged) { 
        	        	
        	image = target.getExtension(IColormappedImageExtension.class)
                    .initializeRaster(solarDataCallback, colorMapParameters); 
        	if (image.getStatus().equals("UNLOADED"))
        		image.stage();
        }
    	
    	if (transformOld == null) {
    		try {
    			transformOld = new WCSConverter(header);
    		} catch (Exception e) {
    			statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);                     
    			throw new VizException(
                      "Could not create image to world coordinate transform", e);
    		}
    	}
    	
    	
    	// create new image
    	if (status != cylindrical) {
    		this.status = cylindrical;
            
    		cylindImageData = getNewImageData(paintProps);//, imageData);
    		
	    	if (this.logConvert)
	        	dataCallback = new LogCylindCedDataCallback(record, cylindImageData);
	        else
	        	dataCallback = new CylindCedDataCallback(record, cylindImageData);
    	} 
    	
//    	if (image == null || isColorMapChanged){	
        	float scaleMin = dataScale.getMinValue().floatValue();
        	float scaleMax = dataScale.getMaxValue().floatValue();
        	float range = scaleMax - scaleMin;
        	
        	float cMapMax = (float)(range/255.0) * colorMapParameters.getColorMapMax(); 
        	float cMapMin = (float) (range/255.0) * colorMapParameters.getColorMapMin(); 
        	 
        	if (range <= 255.0) {
        		if ((colorMapParameters.getColorMapMax() > range)) {       		
        			colorMapParameters.setColorMapMax(cMapMax);
        		} 
        		if ((colorMapParameters.getColorMapMin() > range)) {            		
        			colorMapParameters.setColorMapMin(cMapMin);
                } 
            }

        	else {	
        		if (scaleMin >= 0) {
            		if (colorMapParameters.getColorMapMax() >= 0 && cMapMax <= range) {
                		colorMapParameters.setColorMapMax(cMapMax);
                	}        	
                	
                	if (colorMapParameters.getColorMapMin() >= 0 && cMapMin <= range) {
                		colorMapParameters.setColorMapMin(cMapMin);
                	}
        		}
        		else {
        			if (colorMapParameters.getColorMapMax() >= 0 && cMapMax <= range) {
                		colorMapParameters.setColorMapMax(cMapMax-scaleMax);
                	}     	
                	
                	if (colorMapParameters.getColorMapMin() >= 0 && cMapMin <= range) {
                		colorMapParameters.setColorMapMin(cMapMin-scaleMax);
                	}
        		}
        	}
        
        	if (cylindImage == null || isColorMapChanged)
        	cylindImage = target.getExtension(IColormappedImageExtension.class)
                    .initializeRaster(dataCallback, colorMapParameters);
//    	} 	
       
       if (transform == null || extent == null) {   
    	   
    	   try {
   				transform = new CylindricalConverter();
   			} catch (Exception e) {
   				statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);                     
   				throw new VizException(
                     "Could not create image to world coordinate transform", e);
   			}
   			
//            double[] ll = new double[] { -180, -90 };
//            double[] lr = new double[] {180, -90};
//            double[] ur = new double[] {180, 90};
//            double[] ul = new double[] {-180, 90};

   			double[] ll = transform.imageToWorld(new double[] { 0, 0 });   
            double[] lr = transform.imageToWorld(new double[] { 360, 0 });            		
            double[] ur = transform.imageToWorld(new double[] { 360, 180 });
            double[] ul = transform.imageToWorld(new double[] { 0, 180 });
            
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
                at = AffineTransform.getTranslateInstance(
                        center[0], center[1]);
                at.concatenate(AffineTransform.getScaleInstance(scale, -scale));
                worldToPixel = new AffineTransform2D(at);
                pixelToWorld = worldToPixel.inverse();
                // System.out.println(pixelToWorld.toWKT());
                
				worldToPixel.transform(ll, 0, llp, 0, 1);				
                worldToPixel.transform(lr, 0, lrp, 0, 1);
                worldToPixel.transform(ul, 0, ulp, 0, 1);
                worldToPixel.transform(ur, 0, urp, 0, 1);

            } catch (TransformException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);                       
                throw new VizException(
                        "Could not create pixel extent for image", e);
            }

            extent = new PixelCoverage(new Coordinate(ulp[0], ulp[1]), new Coordinate(urp[0], urp[1]),
                     new Coordinate(lrp[0], lrp[1]), new Coordinate(llp[0], llp[1]));                            
            //System.out.println("**** cylinextent "+ulp[0]+" "+ ulp[1] +" "+urp[0]+" "+ urp[1]+" "+ lrp[0]+" "+lrp[1]+" "+ llp[0]+" "+llp[1] +" "+scale);                     
        }
       
        cylindImage.setContrast(contrast);
        cylindImage.setBrightness(brightness);
        cylindImage.setInterpolated(isInterpolated);
        target.drawRaster(cylindImage, extent, paintProps);   
    }

    public ImageData getNewImageData( PaintProperties paintProps) { 
    	ImageData newImageData = null;
		try {
			newImageData = new ImageData(record);
			newImageData.setNx(360);
			newImageData.setNy(180);
			
		} catch (VizException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
				
    	
    	float[][] newImgArray = new float[360][180];  //lat, lon
    	double[] centric = new double[2];
    	
    	int primeMeridian = 0; //related to the center of the image
    	
		if (cylindrical ==2 )
			isCarr = true;
		else
			isCarr = false;
		
    	if (cylindrical == 2 ) { 
    		if (crln != 0)
    			primeMeridian = 180 +(int) crln; //Canvas started from -180
    		else {
    			primeMeridian = 180 +(int) (L0+hgln); 
    		}
    			
    		if (primeMeridian < 360) // here 0-360
    			primeMeridian = primeMeridian + 360;
    		if (primeMeridian > 360)
    			primeMeridian = primeMeridian - 360;    		
    	}
    	else if (cylindrical == 1 ) { 
    		primeMeridian = 180 + (int) hgln; //Canvas started from -180
    	}
    		
    	
    	for (int i= 0; i<360; i++){
    		for (int j=0; j<180; j++){
    			if ( i >= primeMeridian-90 && i <= primeMeridian+90 
    				||	primeMeridian+90>360 && i>=0 && i<primeMeridian+90-360
    				||	primeMeridian-90<0 && i<=360 && i>primeMeridian-90+360) {
    			
    				double[] latlon = new double [] {i-180, j-90};	 
    				
	    			centric = (new CSConversions(headerData)).heliographicToHeliocentric( latlon, isCarr); 
	    			
					double img[] = transformOld.WorldToImage(centric);
					// remember that image was flipped during earlier and stored with first pixel as top left
//					try {
//						headerData = new HeaderData(record);
//					} catch (VizException e) {
//						// TODO Auto-generated catch block
//						e.printStackTrace();
//					}
	    			img[1] = ny - 1 - img[1];
	    			//double val = getImageValue((int) (img[0]), (int) img[1]);
	                double val = getImageValue((int) (Math.round(img[0])), (int) (Math.round(img[1])));
	                double imgVal = logConvert ?new Double(Math.pow(10.0, formatValue(val))) : val;
	                
	                newImgArray[i][j] = (float) (imgVal);  	                
    			}
    			
    			else if (headerData.isSdoHmi()) {         // hmi default is not 0
    				newImgArray[i][j] = (float) (-1500);   
    			}
    		}
    	} 
	    
	    int n = 0;
	    float[] imgDataRec = new float[360 * 180];	    
	    for (int j = 180 - 1; j >= 0; j--) { // Reverse order of rows
            for (int i = 0; i < 360; i++) {
                imgDataRec[n++] = newImgArray[i][j];
            }
        }

    	newImageData.setImageArrays(newImgArray); 
    	newImageData.setImageValues(imgDataRec);
    	return newImageData;
    }

    public void dispose() {

        if (this.image != null)
            this.image.dispose();
        if (this.cylindImage != null)
            this.cylindImage.dispose();
    }

    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {

        Map<String, Object> map = new LinkedHashMap<String, Object>();
   
        try {
            double[] pixel = new double[] { coord.asLatLon().x,
                    coord.asLatLon().y };
            
            double[] world = new double[2];
            pixelToWorld.transform(pixel, 0, world, 0, 1);  // now hcc is latlon
            map.put("HCC", new Coordinate(formatValue(world[0]), formatValue(world[1])));//move down

            double image[] = transform.WorldToImage(world);
            // remember that image was flipped during earlier and stored with first pixel as top left
            image[1] = ny - 1 - image[1];
            map.put("Image", new Coordinate(formatValue(image[0]), formatValue(image[1])));
           
            if (cylindrical ==1 )
            	map.put("StonyHurst", new Coordinate(formatValue(world[0]), formatValue(world[1])));
            else if (cylindrical ==2 )
            	map.put("StonyHurst", new Coordinate(formatValue(world[0]), formatValue(world[1])));
//            double[] locWorld = new double [2];
//            if (cylindrical ==1 ) {
//            	locWorld[0] = world[0];
//            	locWorld[1] = world[1];
//            	map.put("StonyHurst", new Coordinate(formatValue(locWorld[0]), formatValue(locWorld[1])));
//            	//map.put("Carrington", new Coordinate(formatValue(locWorld[0]+L0), formatValue(locWorld[1])));
//            }
//
//            if (cylindrical ==2 ) {
//            	locWorld[0] = world[0];
//            	locWorld[1] = world[1];
//            	map.put("Carrington", new Coordinate(formatValue(locWorld[0]), formatValue(locWorld[1])));
//            	//map.put("StonyHurst", new Coordinate(formatValue(locWorld[0]-L0), formatValue(locWorld[1])));            	
//            }
            
            
            long x = Math.round(image[0]);
            long y = Math.round(image[1]);
            double val = getImageValue((int) x, (int) y);
            map.put("Display Value", new Double(formatValue(val)));

            map.put("Data Value", new Double(formatValue(dataCallback.getOriginalValue(val))));
            
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
    
    public void setColorMapParameters(ColorMapParameters params) {
        this.colorMapParameters = params;
        this.isColorMapChanged = true;
    }

    public void setColorMapChanged(boolean val) {
    	this.isColorMapChanged = val;;
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
    
    public void setCylindrical(int cylindrical) {
    	this.cylindrical = cylindrical;;
    }
    public int getCylindrical() {
    	return cylindrical;
    }
}
