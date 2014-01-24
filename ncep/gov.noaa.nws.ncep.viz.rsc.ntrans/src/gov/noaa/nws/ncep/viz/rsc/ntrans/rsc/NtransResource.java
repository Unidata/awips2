package gov.noaa.nws.ncep.viz.rsc.ntrans.rsc;

import gov.noaa.nws.ncep.common.dataplugin.ntrans.NtransRecord;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.IRscDataObject;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.Command;
import gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm.INcCommand;
import gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm.NcCGM;
import gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm.NcText;
import gov.noaa.nws.ncep.viz.ui.display.NCNonMapDescriptor;

import java.io.ByteArrayInputStream;
import java.io.DataInput;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * NtransResource - Resource for Display of NTRANS Metafiles.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 21 Nov 2012   838        B. Hebbard  Initial creation.
 * 25 Apr 2013   838        G. Hull     add request constraint to the query for the cycle time  
 * 30 Apr 2013   838        B. Hebbard  IOC version (for OB13.4.1)
 * 30 May 2013   838        B. Hebbard  Update for compatibility with changes by RTS in OB13.3.1
 *                                      [ DataStoreFactory.getDataStore(...) parameter ]

 * 
 * </pre>
 * 
 * @author bhebbard 
 * @version 1.0
 */
public class NtransResource extends AbstractNatlCntrsResource<NtransResourceData, NCNonMapDescriptor>
                                implements INatlCntrsResource {
	
	private NtransResourceData ntransResourceData;

	private String legendStr = "NTRANS "; // init so not-null

	private IFont font = null;

	private final Log logger = LogFactory.getLog(this.getClass());  //TODO static better??

	private class NtransDisplayablePictureInfo {  // CGM calls it a "picture".
		//  This is just a container holding the ready-to-paint information for
		//  a single NTRANS image.
		NtransRecord ntransRecord ;       //  the metadata record (PDO)
		NcCGM cgm;                        //  constructed "jcgm" representation of the image
		public IShadedShape shadedShape;  //  shaded shapes (filled polygons) get special
		                                  //  handling due to memory (heap runaway) issues;
		                                  //  once generated (at first paint), cache here
		                                  //  for future paints, instead of regenerating each time
		
		public NtransDisplayablePictureInfo(NtransRecord nr, NcCGM nc) {
			ntransRecord = nr;
			cgm = nc;
			shadedShape = null;
		}
		
		public void dispose() {
		    if (shadedShape != null) {
		        shadedShape.dispose();
		    }
		}
	}

    private class FrameData extends AbstractFrameData {
    	
    	// FrameData holds the displayable information for a single frame (time).
    	NtransDisplayablePictureInfo pictureInfo;

		public FrameData(DataTime frameTime, int timeInt) {
			super( frameTime, timeInt );
    	}

        public boolean updateFrameData(IRscDataObject rscDataObj) {
        	if( !(rscDataObj instanceof DfltRecordRscDataObj) ) {
        		logger.error("NtransResource.updateFrameData expecting DfltRecordRscDataObj "
            			+ " instead of: " + rscDataObj.getClass().getName() );
        		return false;
        	}

        	// TODO : check that the cycle times match.
        	
        	if( pictureInfo != null ) {

        		System.out.println("adding record to frame that has already been populated");
        		// add code here to check if the new data is a better time match. if not then discard and
        		// if so dispose of the existing data and process the new record
        		return false;
        	}
        	
        	//  Get PDO from the given RDO

        	DfltRecordRscDataObj ntransRDO = (DfltRecordRscDataObj) rscDataObj;
        	NtransRecord nr = (NtransRecord) ntransRDO.getPDO();

        	//  Get image data HDF5.

    		long t0 = System.currentTimeMillis();
        	byte[] imageBytes = getCgmFromNtrans(nr);
    		long t1 = System.currentTimeMillis();
    		if (imageBytes == null) {
        		logger.error("NtransResource.updateFrameData imageBytes from NtransRecord is null ");
        		return false;
    		}
    		
    		//  Fix endianess if needed
    		
    		boolean flipped = false;
        	if (imageBytes[0] == 96) {  //TODO clean up and/or move to decoder?
        		imageBytes = shuffleByteArray(imageBytes);
        		flipped = true;
        	}
    		
    		//  Construct "jcgm" representation of the image
    		
    		NcCGM cgm = new NcCGM();
    		InputStream is = new ByteArrayInputStream(imageBytes);
    		DataInput di = new DataInputStream(is);
    		
    		try {
        		long t2 = System.currentTimeMillis();
				cgm.read(di);
        		long t3 = System.currentTimeMillis();
	    		logger.info("CGM image " + nr.getImageByteCount() +
	    				" bytes retrieved from HDF5 in " + (t1-t0) + " ms"
	    				+ " and parsed in " + (t3-t2) + " ms");
			} catch (Exception e) {
	    		logger.info("CGM image " + nr.getImageByteCount() +
	    				" bytes retrieved from HDF5 in " + (t1-t0) + " ms");
				logger.error("EXCEPTION occurred interpreting CGM"
						+ " for metafile " + nr.getMetafileName() 
						+ " product " + nr.getProductName());
				e.printStackTrace();
			}
			
			//  Endianess revisited
			
			if (flipped) {  // if we shuffled the bytes before parsing...
				flipStrings(cgm); // ...then unshuffle the strings (now we know where they are) 
			}
    		
    		//TODO  Add optional (cool) debug dump of CGM representation 
    		
    		// cgm.showCGMCommands();

        	//  Save away just the info needed to draw this frame
        	
			pictureInfo = new NtransDisplayablePictureInfo(nr, cgm);
        	
        	return true;
        }

    	private void flipStrings(NcCGM cgm) {
    		for (Command c : cgm.getCommands()) {
    			if (c instanceof NcText) {
    				NcText nct = (NcText) c;
    				nct.flipString();
    			}
    		}
		}

		private byte[] getCgmFromNtrans(NtransRecord nr) {

    		// Given the NcscatRecord, locate the associated HDF5 data...
			
    		File location = HDF5Util.findHDF5Location(nr);
    		
    		//TODO... Investigate:  Why is the following statement needed?
    		// Starting in OB13.5.3, the PDO (nr) has a non-null, but bogus,
    		// value in its dataURI field at this point (and earlier,
    		// as soon as it is deserialized after return from the metadata
    		// query).  nr.getDataURI() below will get this bad value, leading
    		// to failure on the ds.retrieve(...).  Instead we force it to
    		// synthesize the dataURI -- which getDataURI() does correctly --
    		// by setting the field to null first.  But why is this happening,
    		// and why only in OB13.5.3, and why only for some resources...?  (bh)
    		// (see also NCSCAT resource)
    		nr.setDataURI(null);  // force it to construct one
    		
    		String group = nr.getDataURI();
    		//String uri = nr.getDataURI();
    		String dataset = "NTRANS";
    		
    		// get filename and directory for IDataStore
    		//String dir = nr.getHDFPathProvider().getHDFPath(nr.getPluginName(), nr);
    		//String filename = nr.getHDFPathProvider().getHDFFileName(nr.getPluginName(), nr);
    		//File file = new File(dir, filename);

    		// ...and retrieve it

    		IDataStore ds = DataStoreFactory.getDataStore(location);
    		IDataRecord dr = null;
    		//IDataRecord[] dr;
    		try {
    			dr = ds.retrieve(group, dataset, Request.ALL);
    			//dr = ds.retrieve(uri);
    		} catch (FileNotFoundException e) {
				logger.error("[EXCEPTION occurred retrieving CGM"
						+ " for metafile " + nr.getMetafileName() 
						+ " product " + nr.getProductName() + "]");
    			e.printStackTrace();
    			return null;
    		} catch (StorageException e) {
				logger.error("[EXCEPTION occurred retrieving CGM"
						+ " for metafile " + nr.getMetafileName() 
						+ " product " + nr.getProductName() + "]");
    			e.printStackTrace();
    			return null;
    		}

    		//return (byte[]) dr[0].getDataObject();
    		return (byte[]) dr.getDataObject();
    	}
		
		public void dispose() {
			if (pictureInfo != null) {
				pictureInfo.dispose();
			}
			super.dispose();
		}
    }
    
    //  ------------------------------------------------------------
    
    public class ImageBuilder {
    	
    	//  This class holds the state of the image while it's under
    	//  construction by sequential execution of the CGM commands.
    	
    	public Map<RGB,IWireframeShape> wireframes = new HashMap<RGB,IWireframeShape>();
    	public RGB currentLineColor = new RGB(255,255,255);
    	public double currentLineWidth = 1.0;
 
    	public List<DrawableString> strings = new ArrayList<DrawableString>();
    	public RGB currentTextColor = new RGB(255,255,255);
    	public IFont currentFont = null;
    	public TextStyle textStyle = TextStyle.NORMAL;
    	public HorizontalAlignment horizontalAlignment = HorizontalAlignment.CENTER;
    	public VerticalAlignment verticalAlignment = VerticalAlignment.TOP;
    	
    	public List<DrawableCircle> circles = new ArrayList<DrawableCircle>();
    	public RGB currentCircleColor = new RGB(255, 0, 0);
    	
		public IShadedShape shadedShape;
    	public RGB currentFillColor = new RGB(0, 255, 0);
    	
    	public boolean shadedShapeReady = false;   //  if true, shaded shape constructed on
    	                                           //  first paint of this frame are already saved
    	                                           //  (in PictureInfo), and so we can skip
                                                   //  regeneration on subsequent paints
    	
    	public double scale = 1.0;
       	public double scaleNoZoom = 1.0;
    	
       	public double[] scalePoint (double[] oldpoint) {
       		return scalePoint (oldpoint[0],oldpoint[1]);
       	}
       	
       	public double[] scalePointNoZoom (double[] oldpoint) {
       		return scalePointNoZoom (oldpoint[0],oldpoint[1]);
       	}

		public double[] scalePoint(double x, double y) {
       		double[] newpoint = new double[2];
       		newpoint[0] = x * scale;
       		newpoint[1] = 1000.000 - y * scale; // TODO:  Avoid hardcoding 1000
       		return newpoint;
		}

		public double[] scalePointNoZoom(double x, double y) {
       		double[] newpoint = new double[2];
       		newpoint[0] = x * scaleNoZoom; // TODO plus translation
       		newpoint[1] = 1000.000 - y * scaleNoZoom; // TODO plus translation
       		return newpoint;
		}
    	
    }
    
    //  ------------------------------------------------------------
    
    /**
     * Create an NTRANS Metafile display resource.
     * 
     * @throws VizException
     */
    public NtransResource(NtransResourceData resourceData,
    		LoadProperties loadProperties) throws VizException {
    	super(resourceData, loadProperties);
    	ntransResourceData = (NtransResourceData) resourceData;
        
        // set the legend from the metafileName and productName
        // NOTE : this assumes that the request type of EQUALS (ie only one kind of metafileName and productName) (??)
        //
        if( ntransResourceData.getMetadataMap().containsKey("metafileName") && 
        		ntransResourceData.getMetadataMap().containsKey("productName") ) {
        	legendStr = 
        		" "   + ntransResourceData.getMetadataMap().get("metafileName").getConstraintValue() +
        		" / " + ntransResourceData.getMetadataMap().get("productName").getConstraintValue();
        }
    }

    protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
    	return (AbstractFrameData) new FrameData( frameTime, timeInt );
    }
    
    // query all the data in the db matching the request constraints (ie modelName, metaFile, and productName)
    // and also match the selected cycle time.
    //    
    public void initResource(IGraphicsTarget grphTarget) throws VizException {
    	// set initial display values from resource attributes (as if after modification)
    	//resourceAttrsModified();
		long t0 = System.currentTimeMillis();
		
    	ResourceName rscName = getResourceData().getResourceName();
        
        // set the constraints for the query. 
    	String[] dts = rscName.getCycleTime().toString().split(" ");
    	String cycleTimeStr = dts[0] + " " + dts[1].substring(0, dts[1].length()-2);

        HashMap<String, RequestConstraint> reqConstraintsMap = 
        	new HashMap<String, RequestConstraint>( ntransResourceData.getMetadataMap() );
        
        RequestConstraint timeConstraint = new RequestConstraint( cycleTimeStr );
        reqConstraintsMap.put("dataTime.refTime", timeConstraint );

		LayerProperty prop = new LayerProperty();
		prop.setDesiredProduct(ResourceType.PLAN_VIEW);
		prop.setEntryQueryParameters( reqConstraintsMap, false);
		prop.setNumberOfImages(15000); // TODO: max # records ?? should we cap this ?
		String script = null;
		script = ScriptCreator.createScript(prop);

		if (script == null)
			return;

		Object[] pdoList = Connector.getInstance().connect(script, null, 60000);

		for( Object pdo : pdoList ) {
			for( IRscDataObject dataObject : processRecord( pdo ) )	{	
				newRscDataObjsQueue.add(dataObject);
			}
		}

		// 
		setAllFramesAsPopulated();
    	
		logger.info("Metadata records for " + this.newRscDataObjsQueue.size() +
				" images retrieved from DB in " +
				(System.currentTimeMillis()-t0) + " ms");
    }

    public void paintFrame(AbstractFrameData frameData, IGraphicsTarget target, PaintProperties paintProps) throws VizException {

    	FrameData fd = (FrameData) frameData;

    	if ( target != null && paintProps != null ) {
    		NtransDisplayablePictureInfo pictureInfo = fd.pictureInfo;
    		
    		if (pictureInfo == null) return;

    		//  Paint it
    		
    		if (this.font == null) {
        		this.font = target.initializeFont("Monospace", 12, new IFont.Style[] { Style.BOLD});
    		}
    		ImageBuilder ib = new ImageBuilder();
    		
    		ib.currentFont = this.font;
    		ib.scale = 1000.000 / (double) pictureInfo.ntransRecord.getImageSizeX();  //TODO avoid hardcoding 1000
    		if (pictureInfo.shadedShape != null) {
    			//  if we've saved shaded shapes from a previous paint, NcPolygonElement
    			//  can skip generating them all over again
    			ib.shadedShapeReady = true;
    		}
    		else {
    			ib.shadedShape = target.createShadedShape(
    					false,  // mutable
    					descriptor.getGridGeometry(),
    					false); // tesselate
    		}

    		double screenToWorldRatio = paintProps.getCanvasBounds().width
            / paintProps.getView().getExtent().getWidth();
    		ib.scaleNoZoom = ib.scale / screenToWorldRatio;
    		ib.scaleNoZoom = ib.scale * paintProps.getZoomLevel();

            IExtent screenExtent = paintProps.getView().getExtent();
            IExtent mapExtent = new PixelExtent(descriptor.getGridGeometry().getGridRange());

    		for (Command c : pictureInfo.cgm.getCommands()) {
    			if (c instanceof INcCommand) {
    				((INcCommand) c).paint(target, paintProps, descriptor, ib);
    			}
    		}
    		
    		//  Shaded Shape (filled polygons) was deferred.  Paint it now.
    		
    		//  if first paint of this image, save away shaded shape for future use
    		if (! ib.shadedShapeReady /* OR pictureInfo.shadedShape == null */) {
    			ib.shadedShape.compile();
    			//pictureInfo.completedShadedShapes = ib.shadedShapes.toArray(new IShadedShape[ib.shadedShapes.size()]);
    			pictureInfo.shadedShape = ib.shadedShape;
    			ib.shadedShapeReady = true;
    		}
    		float alpha = 1.0f;  //TODO  verify
			float brightness = 1.0f;
			target.drawShadedShape(pictureInfo.shadedShape, alpha, brightness);
			//TODO now that we don't dispose on each paint, need to do on resource termination?
			//for (IShadedShape ss : ib.shadedShapes) {
			//	ss.dispose();
			//}
    		
    		//  Wireframes were deferred.  Paint them now.
    		
    		for (RGB color : ib.wireframes.keySet()) {
    			IWireframeShape wireframeForThisColor = ib.wireframes.get(color);
    			if (wireframeForThisColor == null) {
    				//TODO  assert
    			}
    			else {
    				wireframeForThisColor.compile();
    				//TODO  to be correct, should be lineWidth in effect at individual wireframe paints
    				target.drawWireframeShape(wireframeForThisColor, color, (float) ib.currentLineWidth);
    				wireframeForThisColor.dispose();
    			}
    		}
    		
    		//  Strings were deferred.  Paint them now.
    		
    		target.drawStrings(ib.strings);
    		
    		//  Circles were deferred.  Paint them now.
    		
    		target.drawCircle(ib.circles.toArray(new DrawableCircle[ib.circles.size()]));

        	/* --  Greg's handy outline box...
    		
        	RGB color = new RGB( 250, 10, 10 );
        	
    		if( font == null ) {
    			font = target.initializeFont("Monospace", 14, new IFont.Style[] { IFont.Style.BOLD});
    		}
    		PixelCoordinate textLoc = new PixelCoordinate( 500, 500, 0 );
    		target.drawString(font,
    				"CGM IMAGE", textLoc.getX(),
    				textLoc.getY(), 0.0,
    				TextStyle.NORMAL,
    				new RGB( 255, 255, 200 ),
    				HorizontalAlignment.CENTER,
    				VerticalAlignment.MIDDLE, 0.0);

    		target.drawLine(0.0f, 0.0f, 0.0f, 0.0f, 1000.0f, 0.0f, color, 1.0f );
    		target.drawLine(0.0f, 0.0f, 0.0f, 1000.0f, 0.0f, 0.0f, color, 1.0f );
    		target.drawLine(1000.0f, 0.0f, 0.0f, 1000.0f, 1000.0f, 0.0f, color, 1.0f );
    		target.drawLine(0.0f, 1000.0f, 0.0f, 1000.0f, 1000.0f, 0.0f, color, 1.0f );
    		color = new RGB(0,255,0);
    		target.drawLine(10.0f, 10.0f, 10.0f, 10.0f, 990.0f, 10.0f, color, 1.0f );
    		target.drawLine(10.0f, 10.0f, 10.0f, 990.0f, 10.0f, 10.0f, color, 1.0f );
    		target.drawLine(990.0f, 10.0f, 10.0f, 990.0f, 990.0f, 10.0f, color, 1.0f );
    		target.drawLine(10.0f, 990.0f, 10.0f, 990.0f, 990.0f, 10.0f, color, 1.0f );
    		*/
    		
    	}
    	
    }

    private byte[] shuffleByteArray(byte[] image) {
    	// Flip every even byte with its odd sibling (endianess reversal)
    	byte [] returnArray = new byte[image.length];
    	for (int i = 0 ; i < image.length ; i = i + 2 ) {
    		returnArray[i] = image[i+1];
    		returnArray[i+1] = image[i];
    	}
		return returnArray;
	}

	public void resourceAttrsModified() {
    	//TODO??
    }
    
    @Override
	public String getName() {
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null
				|| fd.pictureInfo == null
				|| fd.pictureInfo.cgm == null) {
			return legendStr + "-No Data";
		}
		return legendStr + " "
		    + NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}