package gov.noaa.nws.ncep.ui.nsharp.display.rsc;
/**
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/23/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpShapeAndLineProperty;
import gov.noaa.nws.ncep.ui.nsharp.background.NsharpGenericPaneBackground;
import gov.noaa.nws.ncep.ui.nsharp.background.NsharpGenericPaneBackground.ViewablePressureContainer;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpAbstractPaneDescriptor;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNativeConstants;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.core.graphing.WGraphics;
import com.sun.jna.ptr.FloatByReference;

public class NsharpWitoPaneResource extends NsharpAbstractPaneResource{
	private boolean inSidePane = false;
	private float xMagFactor=1.0f;
	private float yMagFactor=1.0f;
	private int omegaXOrig = NsharpConstants.OMEGA_X_ORIG;
	private int omegaYOrig = NsharpConstants.OMEGA_Y_ORIG;
	private int omegaWidth = NsharpConstants.OMEGA_WIDTH;
	private int omegaHeight = NsharpConstants.OMEGA_HEIGHT;
	private int omegaYEnd = NsharpConstants.OMEGA_Y_END;
	private int omegaMF = NsharpConstants.OMEGA_MAGNIFICATION_FACTOR;
	private float windBoxXOrig = NsharpConstants.WIND_BX_X_ORIG;
	private float windBoxYOrig = NsharpConstants.WIND_BX_Y_ORIG;
	private float windBoxWidth = NsharpConstants.WIND_BX_WIDTH;
	private float windBoxHeight = NsharpConstants.WIND_BX_HEIGHT;
	//private float windBoxYEnd = windBoxYOrig + windBoxHeight;
	private float verticalWindXOrig = NsharpConstants.VRTCAL_WIND_X_ORIG;
	private float verticalWindYOrig = NsharpConstants.VRTCAL_WIND_Y_ORIG;
	private float verticalWindWidth = NsharpConstants.VRTCAL_WIND_WIDTH;
	private float verticalWindHeight = NsharpConstants.VRTCAL_WIND_HEIGHT;
	private float verticalWindYEnd = NsharpConstants.VRTCAL_WIND_Y_END;
	private float witoPanewidth = NsharpConstants.WITO_PANE_REC_WIDTH;
	private float witoPaneHeight = NsharpConstants.WITO_PANE_REC_HEIGHT;
	private IWireframeShape omegaBkgShape = null;
	private IWireframeShape windBoxBkgShape = null;
	private IWireframeShape omegaRscShape=null;
	private IWireframeShape verticalWindLabelShape = null;
	private IWireframeShape verticalWindSbShape = null;
	private IWireframeShape verticalWindRShape = null;
	private List<NsharpShapeAndLineProperty>windBoxWindRscShapeList  = new ArrayList<NsharpShapeAndLineProperty>();
	private ViewablePressureContainer vpc=null;
	private ViewablePressureContainer vplc=null;
	public NsharpWitoPaneResource(AbstractResourceData resourceData,
			LoadProperties loadProperties, NsharpAbstractPaneDescriptor desc) {
		super(resourceData, loadProperties, desc);
		this.dataTimes = new ArrayList<DataTime>();
	}

	@Override
	protected void disposeInternal() {	
		disposeAllWireFrameShapes();
		for (NsharpShapeAndLineProperty el: windBoxWindRscShapeList) {
			el.getShape().dispose();
		}
		windBoxWindRscShapeList.clear();
		super.disposeInternal();
	}
	@SuppressWarnings("deprecation")
	@Override
	protected void paintInternal(IGraphicsTarget target,
			PaintProperties paintProps) throws VizException {
		//System.out.println("NsharpWitoPaneResource paintInternal called! I am pane #"+ this.toString());
		//double X = windBoxXOrig;
		//double Y = 80;
		super.paintInternal(target, paintProps);
		//System.out.println("wito paintInternal zoomL="+currentZoomLevel);
		if(rscHandler== null ||  inSidePane )
			return;
		if(soundingLys!= null){
			this.font10.setSmoothing(false);
			this.font10.setScaleFont(false);
			this.font9.setSmoothing(false);
			this.font9.setScaleFont(false);
			this.font12.setSmoothing(false);
			this.font12.setScaleFont(false);
			/*
			PixelExtent extent = new PixelExtent(new Rectangle(omegaXOrig,omegaYOrig,
					omegaWidth,omegaHeight));
						
			target.drawRect(extent, NsharpConstants.backgroundColor, 1.0f, 1.0f);
			target.drawWireframeShape(omegaBkgShape, NsharpConstants.color_violet_red, commonLinewidth,
					LineStyle.DASHED,font10);
			if(graphConfigProperty != null ){        		        		

				if(graphConfigProperty.isOmega() == true){
					if(NsharpLoadDialog.getAccess()!= null && 
							(NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.MODEL_SND ||
									NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.PFC_SND )){
						//plot omega
						target.drawWireframeShape(omegaRscShape, NsharpConstants.color_cyan, commonLinewidth,
								commonLineStyle,font10);
					}
				}
			}
			else{
				//by default, draw everything
				if(NsharpLoadDialog.getAccess()!= null && 
						(NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.MODEL_SND ||
								NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.PFC_SND )){
					//plot omega
					target.drawWireframeShape(omegaRscShape, NsharpConstants.color_cyan, commonLinewidth,
							commonLineStyle,font10);
				}
			}*/
			//target.clearClippingPlane();

			//wind box background and wind
			
			PixelExtent extent = new PixelExtent(new Rectangle((int)windBoxXOrig,(int)windBoxYOrig,
					(int)windBoxWidth,(int)windBoxHeight));
			//target.setupClippingPlane(extent);
			target.drawRect(extent, NsharpConstants.backgroundColor, 1.0f, 1.0f);
			target.drawWireframeShape(windBoxBkgShape, NsharpConstants.color_white,
					0.5F, LineStyle.DOTS, font10);
			for(NsharpShapeAndLineProperty shapeNColor: windBoxWindRscShapeList){
				target.drawWireframeShape(shapeNColor.getShape(), shapeNColor.getLp().getLineColor(), commonLinewidth,commonLineStyle,font10);
			}
			
			//plot vertical wind profile (advection layer)
			
			extent = new PixelExtent(new Rectangle((int)verticalWindXOrig,(int)verticalWindYOrig,
					(int)verticalWindWidth,(int)verticalWindHeight));
			target.setupClippingPlane(extent);
			target.drawRect(extent, NsharpConstants.backgroundColor, 1.0f, 1.0f);
			float x1 = verticalWindXOrig+ (verticalWindWidth/2);
			target.drawLine(x1, verticalWindYOrig, 0, x1, verticalWindYOrig+verticalWindHeight, 0,
					NsharpConstants.color_white, 1, LineStyle.DASHED);
			target.drawWireframeShape(verticalWindLabelShape, NsharpConstants.color_white,
					0.3F, commonLineStyle,font10);
			target.drawWireframeShape(verticalWindRShape, NsharpConstants.color_red,
					0.3F, commonLineStyle,font10);
			target.drawWireframeShape(verticalWindSbShape, NsharpConstants.color_skyblue,
					0.3F, commonLineStyle,font10);
			target.clearClippingPlane();
		}
	}

	@Override
	protected void initInternal(IGraphicsTarget target) throws VizException {
		super.initInternal(target);
		currentCanvasBoundWidth = witoPanewidth;
		currentCanvasBoundHeight = witoPaneHeight;
		myDefaultCanvasWidth = witoPanewidth;
		myDefaultCanvasHeight = witoPaneHeight;	
		//System.out.print("NsharpWitoPaneResource ::: initInternal entered!!!!!\n");
		this.rectangle = new Rectangle((int)windBoxXOrig,(int) windBoxYOrig,
				(int)witoPanewidth,(int) witoPaneHeight);
		pe = new PixelExtent(this.rectangle);
		world = new WGraphics(this.rectangle);

		world.setWorldCoordinates(NsharpConstants.left, NsharpConstants.top,
				NsharpConstants.right, NsharpConstants.bottom);
		
		
		createAllWireFrameShapes();
	}
	
	private void createBkgWindBoxShape(){
		windBoxBkgShape = target.createWireframeShape(false,descriptor );
		windBoxBkgShape.allocate(30);
		double xOri = windBoxXOrig;
		double yOri = windBoxYOrig;
		//int tickLength = 5;
		//double [][] lines = {{xOri - tickLength, yOri},{xOri+windBoxWidth, yOri}};
		double [][] lines = {{0,0},{0,0}};
		windBoxBkgShape.addLineSegment(lines);
		//double [][] lines1 = {{xOri - tickLength, yOri + windBoxHeight},{xOri+windBoxWidth, yOri+ windBoxHeight}};
		//windBoxBkgShape.addLineSegment(lines1);
		double [] lblXy = {xOri+windBoxWidth/2, yOri+20*yMagFactor};
		windBoxBkgShape.addLabel("wind(kt) ", lblXy);
		double [] lblXy1 = {xOri+windBoxWidth/2, yOri+35*yMagFactor};
		windBoxBkgShape.addLabel("vs Ht ", lblXy1);
		// draw wind speed vs height box
		double xtemp;
		for (int i = 20; i < 140 ; i= i+20){
			xtemp = xOri + (windBoxWidth/7.0f) * (i /20); 
			double [][] lines2 = {{xtemp, yOri},{xtemp, yOri+windBoxHeight-2}};
			windBoxBkgShape.addLineSegment(lines2);
			if( (i==20) || (i==60) || (i==100)){
				double [] lblXy2 = {xtemp,  yOri+windBoxHeight-10};
				windBoxBkgShape.addLabel(String.format("%d",i), lblXy2);
			}

		}
		windBoxBkgShape.compile();
	}
	private void createBkgOmegaShape(){
		//draw label and vertical lines
		omegaBkgShape = target.createWireframeShape(false,descriptor );
	
		omegaBkgShape.allocate(6);
		//we dont really care about temp, as we use pressure for Y axis. 
		// For X-axis, we will convert it proportionally.
		//left dash line, +1 omega line (distance from center line = -1* omegaMF)
		double [][] lines = {{omegaXOrig+omegaWidth/2-omegaMF, omegaYOrig},{omegaXOrig+omegaWidth/2-omegaMF, omegaYEnd}};
		omegaBkgShape.addLineSegment(lines);
		//center line
		double [][] lines1 = {{omegaXOrig+omegaWidth/2, omegaYOrig},{omegaXOrig+omegaWidth/2, omegaYEnd}};
		omegaBkgShape.addLineSegment(lines1);
		//right dash line, -1 omega line (distance from center line = +1* omegaMF)
		double [][] lines2 = {{omegaXOrig+omegaWidth/2+omegaMF, omegaYOrig},{omegaXOrig+omegaWidth/2+omegaMF, omegaYEnd}};
		omegaBkgShape.addLineSegment(lines2);
		double [] lblXy = {omegaXOrig+omegaWidth/2, omegaYOrig+10*yMagFactor};
		omegaBkgShape.addLabel("+1 OMEGA -1", lblXy);
		//double [] lblXy1 = {omegaXOrig+15*xMagFactor, omegaYOrig+20*yMagFactor};
		//omegaBkgShape.addLabel("+1", lblXy1);
		//double [] lblXy2 = {omegaXOrig+(omegaWidth/3)*xMagFactor, omegaYOrig+20*yMagFactor};
		//omegaBkgShape.addLabel("-1", lblXy2);
		omegaBkgShape.compile();

	}
	private void createRscOmegaShape(){
		
		omegaRscShape = target.createWireframeShape(false,descriptor );
		omegaRscShape.allocate(soundingLys.size() * 2);
		float p, omega;
		double myYViewRange = omegaYEnd- omegaYOrig;
		for (NcSoundingLayer layer : this.soundingLys) {
			p = layer.getPressure();
			if(vpc!=null && vpc.getPyMap().get(p)!= null && layer.getOmega() > -999){
				omega = layer.getOmega()* -omegaMF;
				//axis is for negative omega, therefore, times "-" sign.
				float yRatio = vpc.getPyMap().get(p);
				double omegaY = myYViewRange * yRatio + omegaYOrig;
				double [][] lines = {{omegaXOrig+omegaWidth/2, omegaY},{omegaXOrig+omegaWidth/2 + omega, omegaY}};
				omegaRscShape.addLineSegment(lines);
			}
		}
		omegaRscShape.compile();
	}
	private void createRscWindBoxWindShape(){
        double windBoxX = windBoxXOrig;
        double windBoxY;
        float xRatio =  ((float)windBoxWidth) / 140.00F;
        NsharpShapeAndLineProperty shNcolor = new NsharpShapeAndLineProperty();
        shNcolor.setShape(target.createWireframeShape(true,descriptor) );
        IWireframeShape shapePline= shNcolor.getShape();
        shapePline.allocate(NsharpConstants.PRESSURE_MARK_LEVELS.length*2);
        shNcolor.getLp().setLineColor(NsharpConstants.pressureColor);
        windBoxWindRscShapeList.add(shNcolor);
        shNcolor = new NsharpShapeAndLineProperty();
        shNcolor.setShape(target.createWireframeShape(false,descriptor ));
        IWireframeShape shapeR = shNcolor.getShape();
        shapeR.allocate(soundingLys.size()*2);
        shNcolor.getLp().setLineColor(NsharpConstants.color_red);
        windBoxWindRscShapeList.add(shNcolor);
        shNcolor = new NsharpShapeAndLineProperty();
        shNcolor.setShape(target.createWireframeShape(false,descriptor) );
        IWireframeShape shapeG= shNcolor.getShape();
        shapeG.allocate(soundingLys.size()*2);
        shNcolor.getLp().setLineColor(NsharpConstants.color_green);
        windBoxWindRscShapeList.add(shNcolor);
        shNcolor = new NsharpShapeAndLineProperty();
        shNcolor.setShape(target.createWireframeShape(false,descriptor) );
        IWireframeShape shapeY=  shNcolor.getShape();
        shapeY.allocate(soundingLys.size()*2);
        shNcolor.getLp().setLineColor(NsharpConstants.color_yellow);
        windBoxWindRscShapeList.add(shNcolor);
        shNcolor = new NsharpShapeAndLineProperty();
        shNcolor.setShape(target.createWireframeShape(false,descriptor) );
        IWireframeShape shapeC=  shNcolor.getShape();
        shapeC.allocate(soundingLys.size()*2);
        shNcolor.getLp().setLineColor(NsharpConstants.color_cyan);
        windBoxWindRscShapeList.add(shNcolor);
        shNcolor = new NsharpShapeAndLineProperty();
        shNcolor.setShape(target.createWireframeShape(false,descriptor) );
        IWireframeShape shapeV =  shNcolor.getShape();
        shapeV.allocate(soundingLys.size()*2);
        shNcolor.getLp().setLineColor(NsharpConstants.color_violet);
        windBoxWindRscShapeList.add(shNcolor);
        
        
       
        //System.out.println("my wolrd minvY="+ myYViewMin+ " maxVY="+myYViewMax+ " YRange="+myYViewRange);
        for (NcSoundingLayer layer : soundingLys) {
        	float pressure = layer.getPressure();
        	if(vpc!=null && vpc.getPyMap().get(pressure)!= null){
        		float spd = layer.getWindSpeed();
        		if ( spd < 0 ) {
        			continue;
        		}
        		if(spd > 140)
        			spd = 140;

        		float yRatio = vpc.getPyMap().get(pressure);
        		windBoxY = windBoxHeight * yRatio + windBoxYOrig;
        		//System.out.println("Wind p="+pressure+" yP="+ windBoxY+ " ratio="+yRatio);
        		float geoHt = layer.getGeoHeight();
        		double [][] lines = {{windBoxX, windBoxY},{windBoxX + (spd) * xRatio,windBoxY}};
        		if(geoHt <nsharpNative.nsharpLib.msl(3000))
        			shapeR.addLineSegment(lines);
        		else if(geoHt < nsharpNative.nsharpLib.msl(6000))
        			shapeG.addLineSegment(lines);
        		else if(geoHt < nsharpNative.nsharpLib.msl(9000))
        			shapeY.addLineSegment(lines);
        		else if(geoHt < nsharpNative.nsharpLib.msl(12000))
        			shapeC.addLineSegment(lines);
        		else
        			shapeV.addLineSegment(lines);
        		if(spd > 140)
        			spd = 140;
        	}
        }
        for (float pressure = (float)NsharpConstants.MIN_PRESSURE+100;pressure <= (float)NsharpConstants.MAX_PRESSURE; pressure=pressure+100) {
        	
        	if(vplc!=null && vplc.getPyMap().get(pressure)!= null){
        		float yRatio = vplc.getPyMap().get(pressure);
        		windBoxY = windBoxHeight * yRatio + windBoxYOrig;
        		double [][] lines = {{windBoxX, windBoxY},{windBoxX + witoPanewidth,windBoxY}};
        		shapePline.addLineSegment(lines);
        		//System.out.println("Line p="+pressure+" yP="+ windBoxY+ " ratio="+yRatio + " ymin="+windBoxYOrig+ " yend="+windBoxYEnd);
        	}
        }
        shapeR.compile();
    	shapeG.compile();
    	shapeY.compile();
    	shapeV.compile();
    	shapeC.compile();
    	shapePline.compile();
	}
	/*
	 * Chin:: NOTE:::
	 * This plotting function is based on the algorithm of plot_advectionprofile() at xwvid5.c of Bignsharp source code
	 * 
	 */
	private void createRscVerticalWindShape(){
        //double origX = verticalWindXOrig;
		double y1, y2, x1,x2;//,origY;
		//float xRatio =  ((float)windBoxWidth) / 140.00F;
		verticalWindLabelShape = target.createWireframeShape(false,descriptor );
		verticalWindLabelShape.allocate(2);
		// add a virtual line to make wire frame shape worked when there is only strings in shape       
		double [][] lines = {{0, 0},{0,0}}; 
		verticalWindLabelShape.addLineSegment(lines);
		double [] lblXy = { verticalWindXOrig+verticalWindWidth/2, verticalWindYOrig+20*yMagFactor};
		verticalWindLabelShape.addLabel("Inferred", lblXy);
		double [] lblXy1 = { verticalWindXOrig+verticalWindWidth/2, verticalWindYOrig+35*yMagFactor};
		verticalWindLabelShape.addLabel("Temp", lblXy1);
		double [] lblXy2 = { verticalWindXOrig+verticalWindWidth/2, verticalWindYOrig+50*yMagFactor};
		verticalWindLabelShape.addLabel("Advection", lblXy2);
		double [] lblXy3 = { verticalWindXOrig+verticalWindWidth/2, verticalWindYOrig+65*yMagFactor};
		verticalWindLabelShape.addLabel("(C/hr)", lblXy3);

		verticalWindSbShape = target.createWireframeShape(false,descriptor );
		verticalWindSbShape.allocate(72);
		verticalWindRShape = target.createWireframeShape(false,descriptor );
		verticalWindRShape.allocate(72);
		FloatByReference dummy1= new FloatByReference(-999);
		FloatByReference dummy2= new FloatByReference(-999);
		FloatByReference Surfpressure = new FloatByReference(-999);
		nsharpNative.nsharpLib.get_surface(Surfpressure, dummy1, dummy2);
		NsharpGenericPaneBackground skewtBgd = rscHandler.getSkewtPaneRsc().getActiveBackground();
		if(skewtBgd!=null){
			//float vpMax = (float)skewtBgd.getViewableMaxPressure();
			float vpMin = (float)skewtBgd.getViewableMinPressure();

			if(nsharpNative.nsharpLib.qc(Surfpressure.getValue()) == 1) {
				float advt;
				x1 = verticalWindXOrig+ (verticalWindWidth/2);
				double myYViewMin = verticalWindYOrig;
				double myYViewMax = verticalWindYEnd;
				double myYViewRange = myYViewMax- myYViewMin;
				for (float pressure=Surfpressure.getValue(); pressure>=200; pressure-=100) {
					advt = nsharpNative.nsharpLib.advection_layer(dummy1, pressure, pressure - 100);
					//System.out.println("advt="+advt);
					if(advt <= NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA)
						continue;
					double pYRatio1 = skewtBgd.getYPositionRatioByPressure(pressure);
					float p2 = pressure-100;
					if(p2 < vpMin)
						p2 = vpMin;
					double pYRatio2 = skewtBgd.getYPositionRatioByPressure(p2);
					y1 = myYViewRange * pYRatio1 + myYViewMin;
					y2 = myYViewRange * pYRatio2 + myYViewMin;

					x2 = x1+advt*3*xMagFactor;
					double [][] lines1 = {{x1,y1},{x1,y2}};
					double [][] lines2 = {{x1,y1},{x2,y1}};
					double [][] lines3 = {{x2,y2},{x1,y2}};
					double [][] lines4 = {{x2,y2},{x2,y1}};
					String advtStr = String.format( "%.1f", advt);
					if(advt > 0.0f){
						verticalWindRShape.addLineSegment(lines1);
						verticalWindRShape.addLineSegment(lines2);
						verticalWindRShape.addLineSegment(lines3);
						verticalWindRShape.addLineSegment(lines4);
						double [] advtLblXy = { x1+20*xMagFactor, y2+ (y1-y2)/2};
						verticalWindRShape.addLabel(advtStr, advtLblXy);
					}
					else {
						verticalWindSbShape.addLineSegment(lines1);
						verticalWindSbShape.addLineSegment(lines2);
						verticalWindSbShape.addLineSegment(lines3);
						verticalWindSbShape.addLineSegment(lines4);
						double [] advtLblXy = { x1-20*xMagFactor,y2+ (y1-y2)/2};
						verticalWindSbShape.addLabel(advtStr, advtLblXy);
					}
				}
			}
		}
		verticalWindSbShape.compile();
		verticalWindRShape.compile();
		verticalWindLabelShape.compile();

	}

	/**
	 * Create all wire frame shapes at one place.
	 * Should be used only when a new resource is becoming Current active resource to be displayed.
	 *  
	 */
	public void createAllWireFrameShapes(){
		if(target== null || rscHandler== null || inSidePane )
			return;
		//System.out.println("whitoPane="+this.toString()+" createAllWireFrameShapes called");
		rscHandler.repopulateSndgData();
		
			disposeAllWireFrameShapes();
			createRscWireFrameShapes();;
			//create static shape
			createBkgOmegaShape();
			createBkgWindBoxShape();
		
	}
	public void createRscWireFrameShapes(){
		//System.out.println("createRscWireFrameShapes called");
		if(target!=null){
			disposeRscWireFrameShapes();
			if(soundingLys != null){
				NsharpGenericPaneBackground skewtBgd = rscHandler.getSkewtPaneRsc().getActiveBackground();
		        if(skewtBgd!=null){
		        	vpc = skewtBgd.getViewablePressureContainer(soundingLys);
		        	vplc = skewtBgd.getViewablePressureLinesContainer();
		        }
				createRscOmegaShape();
				createRscWindBoxWindShape();
				createRscVerticalWindShape();
			}
		}
	}
	public void disposeAllWireFrameShapes(){
		disposeRscWireFrameShapes();
		//also dispose static background shpae
		if(omegaBkgShape!=null)
			omegaBkgShape.dispose();

		if(windBoxBkgShape!=null)
			windBoxBkgShape.dispose();

	}
	public void disposeRscWireFrameShapes(){
		if(omegaRscShape!=null){
			omegaRscShape.dispose();
			omegaRscShape=null;
		}
		if(verticalWindSbShape!=null){
			verticalWindSbShape.dispose();
			verticalWindSbShape=null;
		}
		if(verticalWindLabelShape!=null){
			verticalWindLabelShape.dispose();
			verticalWindLabelShape=null;
		}
		if(verticalWindRShape!=null){
			verticalWindRShape.dispose();
			verticalWindRShape=null;
		}
		if(windBoxWindRscShapeList != null && windBoxWindRscShapeList.size()>0){
			for(NsharpShapeAndLineProperty shapeColor: windBoxWindRscShapeList){
				shapeColor.getShape().dispose();
			}
			windBoxWindRscShapeList.clear();

		}
	}

	
	/*
	 * (non-Javadoc)
	 * @see gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpAbstractPaneResource#handleResize()
	 * Does not override parent's handleResize() but only called from skewtPaneResource to be in sync
	 * with it (skewtPaneResource).
	 */
	public void handleResize() {
		if(getDescriptor().getRenderableDisplay() == null)
			return;
		super.handleResize();
		IExtent ext = getDescriptor().getRenderableDisplay().getExtent();
		ext.reset();
		if (ext.getWidth() == 0.0 || ext.getHeight() == 0.0) {
		    return;
		}
		this.rectangle = new Rectangle((int)ext.getMinX(), (int) ext.getMinY(),
				(int) ext.getWidth(), (int) ext.getHeight());
		world = new WGraphics(this.rectangle);

		/*world.setWorldCoordinates(NsharpConstants.left, NsharpConstants.top,
				NsharpConstants.right, NsharpConstants.bottom);*/
		world.setWorldCoordinates(rectangle.x,rectangle.y,
        		rectangle.x + rectangle.width, rectangle.y+rectangle.height);
		pe = new PixelExtent(this.rectangle);
		getDescriptor().setNewPe(pe);
		float prevWidth= witoPanewidth;
		float prevHeight= witoPaneHeight;
		witoPanewidth = (int)ext.getWidth();
		witoPaneHeight = (int)ext.getHeight();
		xMagFactor = xMagFactor * ((float)witoPanewidth/prevWidth);
		yMagFactor = yMagFactor * ((float)witoPaneHeight/prevHeight);
		omegaMF = (int)((float)witoPanewidth/prevWidth * (float)omegaMF);		
		omegaXOrig = (int) (ext.getMinX()+ witoPanewidth*2/3);
		omegaYOrig = (int) ext.getMinY();
		omegaWidth = (int) (witoPanewidth/3);
		omegaHeight = (int) ext.getHeight();
		omegaYEnd = omegaYOrig + omegaHeight;
		windBoxXOrig = (float) (ext.getMinX());
		windBoxYOrig = (float) ext.getMinY();
		windBoxWidth = (witoPanewidth/9*5);
		windBoxHeight =  (float) ext.getHeight();
		//windBoxYEnd = windBoxYOrig + windBoxHeight;
		verticalWindXOrig = (float) (ext.getMinX()+windBoxWidth);
		verticalWindYOrig = (float) ext.getMinY();
		verticalWindWidth = (witoPanewidth/9*4);
		verticalWindHeight = (float) ext.getHeight();
		verticalWindYEnd = verticalWindYOrig + verticalWindHeight;
		//System.out.println("Wito: handle resize w="+witoPanewidth+ " h="+ witoPaneHeight);
		createAllWireFrameShapes();
	}

	@Override
	public void handleZooming() {
		createAllWireFrameShapes();
	}
	@Override
	protected void adjustFontSize(float canvasW, float canvasH) {
		/*
		super.adjustFontSize(canvasW, canvasH);
		//make a bit bigger font10 size 
		float font10Size=10;
		if(font10!=null){
			font10Size = font10.getFontSize()+1;
			font10.dispose();
		}
		font10 = target.initializeFont("Monospace", font10Size, null);
		*/
	}
	
	public boolean isInSidePane() {
		//System.out.println("isInSidePane returned="+inSidePane + this.toString());
		return inSidePane;
	}

	public void setInSidePane(boolean inSidePane) {
		//System.out.println("setInSidePane to="+inSidePane+ this.toString());
		this.inSidePane = inSidePane;
	}
	
}
