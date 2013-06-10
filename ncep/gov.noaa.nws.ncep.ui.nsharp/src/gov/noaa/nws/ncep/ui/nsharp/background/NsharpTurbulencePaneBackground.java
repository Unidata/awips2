/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.background.NsharpTurbulencePaneBackground
 * 
 * This java class performs the NSHARP NsharpSkewTPaneBackground functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 05/02/2012	229			Chin Chen	Initial coding for multiple display panes implementation
 * 									    
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.background;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGraphProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpLineProperty;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpSkewTPaneDescriptor;

import java.util.HashMap;
import java.util.List;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.graphing.WGraphics;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpTurbulencePaneBackground extends NsharpGenericPaneBackground{
	private IWireframeShape linesNumbersShape;
	private IWireframeShape lNLabelShape; //Richardson Number
	private IWireframeShape windShearLabelShape;
	private double currentZoomLevel=1;
	private final float defaultLabelSpace = 50;
	private float labelSpace = defaultLabelSpace;
	private int turbXOrig=NsharpConstants.TURB_X_ORIG;
	private int turbYOrig=NsharpConstants.TURB_Y_ORIG+(int)labelSpace;
	private int turbXEnd=NsharpConstants.TURB_X_END;
	private int turbWidth=NsharpConstants.TURB_WIDTH;
	private int turbYEnd=turbYOrig+NsharpConstants.TURB_HEIGHT;
	private int paneHeight = NsharpConstants.SKEWT_HEIGHT;
	private float yMagFactor=1;
	private float xMagFactor=1;
	private NsharpGraphProperty graphConfigProperty;
	public NsharpTurbulencePaneBackground(NsharpSkewTPaneDescriptor desc) {
        super();

        this.rectangle = new Rectangle(turbXOrig, turbYOrig,
        		NsharpConstants.TURB_WIDTH, NsharpConstants.TURB_HEIGHT);
        pe = new PixelExtent(this.rectangle);
        world = new WGraphics(this.rectangle);
        NsharpConfigManager configMgr = NsharpConfigManager.getInstance();
        graphConfigProperty = configMgr.retrieveNsharpConfigStoreFromFs().getGraphProperty();
        this.desc = desc;
    }
	@Override
	protected WGraphics computeWorld() {
		// TODO Auto-generated method stub
		return null;
	}
	private void createAllShapes(){
		IExtent ext = desc.getRenderableDisplay().getExtent();
        double xmin = ext.getMinX();  //Extent's viewable envelope min x and y
        //double xmax = ext.getMaxX();  //Extent's viewable envelope min x and y
        double ymax = ext.getMaxY();
        double ymin = ext.getMinY();
        double dispX,pX=0;// = xmin + 20 * currentZoomLevel;
        double dispY, pY=0;// = ymax - 20 * currentZoomLevel;
		String s = "";
        linesNumbersShape = target.createWireframeShape(false,desc );
        windShearLabelShape = target.createWireframeShape(false,desc );
        lNLabelShape = target.createWireframeShape(false,desc );
        linesNumbersShape.allocate(100);
        windShearLabelShape.allocate(20);
        lNLabelShape.allocate(20);
        //set world based on pressure/In
        world.setWorldCoordinates(NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_LEFT, toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_TOP),
        		NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_RIGHT, toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_BOTTOM));
         //System.out.println("vYmax="+ymax+" vYmin="+ymin);
        pX= world.mapX( NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_LEFT);
        if(pX < xmin)
        	dispX =  xmin + 30 * currentZoomLevel*xMagFactor;
        else
        	dispX =  pX + 30 * currentZoomLevel*xMagFactor;
        //dispX = turbXOrig + 30 * currentZoomLevel;
        //pressure lines and labels
        for (double i = NsharpConstants.TURBULENCE_PRESSURE_LEVEL_TOP; i <= NsharpConstants.TURBULENCE_PRESSURE_LEVEL_BOTTOM; i=i+ NsharpConstants.TURBULENCE_PRESSURE_LEVEL_INC) {
        	//Pressure lines
        	double [][] lines = {{turbXOrig, world.mapY(toLogScale(i))},{turbXEnd, world.mapY(toLogScale(i))}};
        	linesNumbersShape.addLineSegment(lines);
        	s = NsharpConstants.pressFormat.format(i);
        	//pressure labels
        	double [] lblXy = {dispX,world.mapY(toLogScale(i))+5};
        	linesNumbersShape.addLabel(s, lblXy);
        	//System.out.println("p="+reverseLogScale(toLogScale(i))+" yPos="+world.mapY(toLogScale(i)));
        }
        //LN label
        //double [] lblRhXy = {turbXOrig+ NsharpConstants.TURB_WIDTH* 0.6, turbYOrig-40};
    	//lNLabelShape.addLabel("*****TURBULENCE Display*****", lblRhXy);
    	double [] lblRhXy1 = {turbXOrig+ 0.5*turbWidth  ,turbYOrig-30*yMagFactor};
    	lNLabelShape.addLabel("*****TURBULENCE Display*****", lblRhXy1);
    	double [] lblRhXy2 = {turbXOrig+ 0.5*turbWidth  ,turbYEnd+10*yMagFactor};
    	lNLabelShape.addLabel("LN(RICHARDSON NUMBER)", lblRhXy2);
    	double [][] lineRH = {{0,0},{0,0}};
    	lNLabelShape.addLineSegment(lineRH);//add dummy line
    	pY = world.mapY(toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_TOP));
    	if(ymin < pY)
    		dispY = pY + 20 * currentZoomLevel*yMagFactor;
    	else
    		dispY = ymin + 20 * currentZoomLevel*yMagFactor;
    	//dispY = turbYOrig + 20 * currentZoomLevel;
        for (double i = NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_LEFT; i >= NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_RIGHT; i= i+NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_INC) {
        	// temperature/LN vertical lines
        	double [][] lines = {{world.mapX(i), turbYOrig},{world.mapX(i), turbYEnd}};
        	linesNumbersShape.addLineSegment(lines);
        	//RH label
        	s = NsharpConstants.pressFormat.format(i);
        	double [] lblXy = {world.mapX(i),dispY};
        	lNLabelShape.addLabel(s, lblXy);
        }
        //wind shear label
        double [] lblTXy = {turbXOrig+  0.5*turbWidth, turbYEnd+35*yMagFactor};
        windShearLabelShape.addLabel("WIND SHEAR TKE PRODUCTION x 1E3 joules/sec", lblTXy);
    	double [][] lineT = {{0,0},{0,0}};
    	windShearLabelShape.addLineSegment(lineT);//add dummy line
    	//set world based on pressure/windShear
    	world.setWorldCoordinates(NsharpConstants.TURBULENCE_WIND_SHEAR_TKE_LEFT, toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_TOP),
        		NsharpConstants.TURBULENCE_WIND_SHEAR_TKE_RIGHT, toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_BOTTOM));
    	pY = world.mapY(toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_BOTTOM));
    	if(ymax > pY)
    		dispY = pY - 10 * currentZoomLevel*yMagFactor;
    	else
    		dispY = ymax - 10 * currentZoomLevel*yMagFactor;
    	
    	for (double i = NsharpConstants.TURBULENCE_WIND_SHEAR_TKE_LEFT; i <= NsharpConstants.TURBULENCE_WIND_SHEAR_TKE_RIGHT; i= i+NsharpConstants.TURBULENCE_WIND_SHEAR_TKE_INC) {
        	// temperature label
        	s = NsharpConstants.pressFormat.format(i);
        	double [] lblXy = {world.mapX(i),dispY};
        	windShearLabelShape.addLabel(s, lblXy);
        }
    	linesNumbersShape.compile();
        lNLabelShape.compile();
        windShearLabelShape.compile();

	}

	@Override
	public synchronized void initInternal(IGraphicsTarget target) {
		// TODO Auto-generated method stub
		super.initInternal(target);
		createAllShapes();
	}

	@Override
	protected void paintInternal(IGraphicsTarget target,
			PaintProperties paintProps) throws VizException {
		// TODO Auto-generated method stub
		//super.paintInternal(target, paintProps);
		target.setupClippingPlane(pe);
		target.drawRect(pe, NsharpConstants.backgroundColor, 1.0f, 1.0f);
		target.clearClippingPlane();
		double zoomLevel = paintProps.getZoomLevel();
		if(zoomLevel > 1.0f)
			zoomLevel = 1.0f;
        if(zoomLevel != currentZoomLevel)
		{
			currentZoomLevel = zoomLevel;
			if(linesNumbersShape!= null){
				linesNumbersShape.dispose();
			}
			if(lNLabelShape!= null){
				lNLabelShape.dispose();
			}
			if(windShearLabelShape!= null){
				windShearLabelShape.dispose();
			}
			createAllShapes();
		}
		this.smallFont.setSmoothing(false);
		this.smallFont.setScaleFont(false);
		target.drawWireframeShape(linesNumbersShape, NsharpConstants.pressureColor, 1, LineStyle.SOLID,smallFont);
		HashMap<String, NsharpLineProperty> lpMap = ((NsharpSkewTPaneDescriptor)desc).getSkewtResource().getLinePropertyMap();
		if(lpMap!=null){
			NsharpLineProperty lp =lpMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_TURBULENCE_LN]);
			target.drawWireframeShape(lNLabelShape,lp.getLineColor(), 1, LineStyle.SOLID,smallFont);
			lp =lpMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_TURBULENCE_WS]);
			target.drawWireframeShape(windShearLabelShape, lp.getLineColor(), 1, LineStyle.SOLID,smallFont);
		}
		else{
			target.drawWireframeShape(lNLabelShape, NsharpConstants.color_violet_red, 1, LineStyle.SOLID,smallFont);
			target.drawWireframeShape(windShearLabelShape, NsharpConstants.color_pink, 1, LineStyle.SOLID,smallFont);
		}
	}
	@Override
	public void disposeInternal() {
		// TODO Auto-generated method stub
		super.disposeInternal();
		if(linesNumbersShape!= null){
			linesNumbersShape.dispose();
		}
		if(lNLabelShape!= null){
			lNLabelShape.dispose();
		}
		if(windShearLabelShape!= null){
			windShearLabelShape.dispose();
		}
	}
	@Override
    public double getViewableMaxPressure(){
    	IExtent ext = desc.getRenderableDisplay().getExtent();
    	double ymax = ext.getMaxY();
    	double xmin = ext.getMinX();  
    	world.setWorldCoordinates(NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_LEFT, toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_TOP),
        		NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_RIGHT, toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_BOTTOM));
    	double viewablePmax = reverseLogScale(world.unMap(xmin, ymax).y);
        return viewablePmax;
    }
    @Override
    public double getViewableMinPressure(){
    	IExtent ext = desc.getRenderableDisplay().getExtent();
    	double ymin = ext.getMinY();
    	double xmin = ext.getMinX();  
    	world.setWorldCoordinates(NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_LEFT, toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_TOP),
        		NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_RIGHT, toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_BOTTOM));
        double viewablePmin = reverseLogScale(world.unMap(xmin, ymin).y);
        return viewablePmin;
    }
    @Override
    public double getYPositionRatioByPressure(double pressure){
    	double pY= world.mapY(toLogScale(pressure));
    	IExtent ext = desc.getRenderableDisplay().getExtent();
    	double ymin = ext.getMinY();
    	double ymax = ext.getMaxY();
        double ratio = (pY-ymin) / (ymax-ymin);
        //System.out.println("ratio="+ratio+ "rtP="+rtP+" maxP="+maxP+" minP="+minP);
        return ratio;
    }
    @Override
    public ViewablePressureContainer getViewablePressureContainer(List<NcSoundingLayer> soundingLys){
    	world.setWorldCoordinates(NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_LEFT, toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_TOP),
        		NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_RIGHT, toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_BOTTOM));
    	ViewablePressureContainer vpc = new ViewablePressureContainer();
    	IExtent ext = desc.getRenderableDisplay().getExtent();
    	float ymax = (float)ext.getMaxY();
    	float ymin =(float) ext.getMinY();
    	double vPmax = getViewableMaxPressure();
    	double vPmin = getViewableMinPressure();
    	vpc.maxVIewablePressure = (float)vPmax;
    	vpc.minVIewablePressure = (float)vPmin;
    	for(NcSoundingLayer ly: soundingLys){
    		float pressure = ly.getPressure();
    		if(pressure >= vPmin &&pressure <= vPmax){
    			double pY= world.mapY(toLogScale(pressure));
    	        float ratio = (float) (pY-ymin) / (ymax-ymin);
    	        vpc.pyMap.put(pressure, ratio);
    	       //System.out.println("Turb press="+pressure+" ratio="+ratio+ " pY="+pY+" maxPy="+ymax+" minPy="+ymin);
    		}
    	}
    	return vpc;
    }
    @Override
    public ViewablePressureContainer getViewablePressureLinesContainer(){
		ViewablePressureContainer vpc = new ViewablePressureContainer();
    	IExtent ext = desc.getRenderableDisplay().getExtent();
    	float ymax = (float)ext.getMaxY();
    	float ymin =(float) ext.getMinY();
    	double vPmax = getViewableMaxPressure();
    	double vPmin = getViewableMinPressure();
    	vpc.maxVIewablePressure = (float)vPmax;
    	vpc.minVIewablePressure = (float)vPmin;
    	for (float pressure = 100; pressure <= 1000;pressure= pressure+100) {
    		/*if(pressure >= vPmin &&pressure <= vPmax)*/{
    			double pY= world.mapY(toLogScale(pressure));
    	        float ratio = (float) (pY-ymin) / (ymax-ymin);
    	        vpc.pyMap.put(pressure, ratio);
    	       //System.out.println("Icing press="+pressure+" ratio="+ratio+ " pY="+pY+" maxPy="+ymax+" minPy="+ymin);
    		}
    	}
    	return vpc;
    }
    
    /*
     * Called from handleResize() in skewTPaneResource only
     */
    public void handleResize(IExtent ext){
    	if(target==null)
 			return;
    	//IExtent ext = desc.getRenderableDisplay().getExtent();
		//ext.reset();
		float prevHeight = paneHeight;
    	paneHeight = (int) (ext.getHeight());
    	yMagFactor = yMagFactor * ((float)paneHeight/prevHeight);
    	labelSpace = defaultLabelSpace * yMagFactor;
		turbXOrig=(int) (ext.getMinX());
    	turbYOrig=(int) (ext.getMinY())+ (int)labelSpace;
    	turbXEnd=turbXOrig+(int) (ext.getWidth());
    	turbYEnd=turbYOrig+(int) (ext.getHeight()) - 2*(int)labelSpace;
    	float prevWidth = turbWidth;
    	turbWidth = (int) (ext.getWidth());
    	xMagFactor =  xMagFactor* ((float)turbWidth/prevWidth);
		this.rectangle = new Rectangle(turbXOrig, turbYOrig,
				turbWidth, (int) ext.getHeight()-2*(int)labelSpace);
        pe = new PixelExtent(this.rectangle);
        //desc.setNewPe(pe);
        world = new WGraphics(this.rectangle);
        
        if(linesNumbersShape!= null){
			linesNumbersShape.dispose();
		}
		if(lNLabelShape!= null){
			lNLabelShape.dispose();
		}
		if(windShearLabelShape!= null){
			windShearLabelShape.dispose();
		}
        createAllShapes();
    }
    public void handleZooming(){
    	if(linesNumbersShape!= null){
			linesNumbersShape.dispose();
		}
		if(lNLabelShape!= null){
			lNLabelShape.dispose();
		}
		if(windShearLabelShape!= null){
			windShearLabelShape.dispose();
		}
        createAllShapes();
    }
    public double getWindBarbXPosition(){
		IExtent ext = desc.getRenderableDisplay().getExtent();
        double xmax = ext.getMaxX();  //Extent's viewable envelope min x and y
        double ymax = ext.getMaxY();
      //Chin:NOTE: LN Richardson number is plotted with positive number at left and negative number to its right side.
		// Therefore,its world X coordinate maximum is at  TURBULENCE_LN_RICHARDSON_NUMBER_LEFT when plotting wind barb
		
        double pX= world.mapX( NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_LEFT);
        if(pX < xmax)
        	xmax = pX;
        double windBarbSizfactor = graphConfigProperty.getWindBarbSize()/2f;
        if(windBarbSizfactor < 1)
        	windBarbSizfactor=1;
        double dispX = xmax - 50 * currentZoomLevel * xMagFactor* windBarbSizfactor;
        
        Coordinate cumap = world.unMap(dispX,ymax);
        
        return cumap.x;
	}
}
