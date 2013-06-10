/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.background.NsharpIcingPaneBackground
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

public class NsharpIcingPaneBackground extends NsharpGenericPaneBackground {
	//private NsharpSkewTPaneDescriptor desc;
	
	private IWireframeShape linesNumbersShape;
	private IWireframeShape RHLabelShape;
	private IWireframeShape tempLabelShape;
	private IWireframeShape EPILabelShape;//Equivalent Potential Instability
	private double currentZoomLevel=1;
	private final float defaultLabelSpace = 50;
	private float labelSpace = defaultLabelSpace;
	private int iceXOrig=NsharpConstants.ICING_X_ORIG;
	private int iceYOrig=NsharpConstants.ICING_Y_ORIG+(int)labelSpace;
	private int iceXEnd=NsharpConstants.ICING_X_END;
	private int iceWidth=NsharpConstants.ICING_WIDTH;
	private int iceYEnd=iceYOrig+NsharpConstants.ICING_HEIGHT;
	private int paneHeight = NsharpConstants.SKEWT_HEIGHT;
	private float yMagFactor=1;
	private float xMagFactor=1;
	private NsharpGraphProperty graphConfigProperty;
	public NsharpIcingPaneBackground(NsharpSkewTPaneDescriptor desc) {
        super();

        this.rectangle = new Rectangle(iceXOrig, iceYOrig,
        		iceWidth, NsharpConstants.ICING_HEIGHT);
        pe = new PixelExtent(this.rectangle);
        world = new WGraphics(this.rectangle);

        NsharpConfigManager configMgr = NsharpConfigManager.getInstance();
        graphConfigProperty = configMgr.retrieveNsharpConfigStoreFromFs().getGraphProperty();
        //System.out.println("NsharpIcingPaneBackground created");
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
       double ymax = ext.getMaxY();
        double ymin = ext.getMinY();
        double dispX,pX=0;
        double dispY, pY=0;
        String s = "";
        linesNumbersShape = target.createWireframeShape(false,desc );
        tempLabelShape = target.createWireframeShape(false,desc );
        RHLabelShape = target.createWireframeShape(false,desc );
        EPILabelShape = target.createWireframeShape(false,desc );
        linesNumbersShape.allocate(100);
        tempLabelShape.allocate(20);
        RHLabelShape.allocate(20);
        EPILabelShape.allocate(4);
        //set world based on pressure/RH
        world.setWorldCoordinates(NsharpConstants.ICING_RELATIVE_HUMIDITY_LEFT, toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_TOP),
        		NsharpConstants.ICING_RELATIVE_HUMIDITY_RIGHT, toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM));
        pX= world.mapX( NsharpConstants.ICING_RELATIVE_HUMIDITY_LEFT);
        if(pX < xmin)
        	dispX =  xmin + 20 * currentZoomLevel*xMagFactor;
        else
        	dispX =  pX + 20 * currentZoomLevel*xMagFactor;
    	
    	//pressure lines and labels
        for (double i = NsharpConstants.ICING_PRESSURE_LEVEL_TOP; i <= NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM; i=i+ NsharpConstants.ICING_PRESSURE_LEVEL_INC) {
        	//Pressure lines
        	double [][] lines = {{iceXOrig, world.mapY(toLogScale(i))},{iceXEnd, world.mapY(toLogScale(i))}};
        	linesNumbersShape.addLineSegment(lines);
        	s = NsharpConstants.pressFormat.format(i);
        	//pressure labels
        	dispY = world.mapY(toLogScale(i))+5;
        	double [] lblXy = {dispX,dispY};
        	linesNumbersShape.addLabel(s, lblXy);
        }
        //RHLabel
        //double [] lblRhXy = {iceXOrig+ iceWidth/2, iceYOrig-45};
    	//RHLabelShape.addLabel("*****ICING Display*****", lblRhXy);
    	double [] lblRhXy1 = {iceXOrig+iceWidth/2,iceYOrig-35*yMagFactor};
    	RHLabelShape.addLabel("*****ICING Display*****     RELATIVE HUMIDITY", lblRhXy1);
    	double [][] lineRH = {{0,0},{0,0}};
    	RHLabelShape.addLineSegment(lineRH);//add dummy line
    	pY = world.mapY(toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_TOP));
    	if(ymin < pY)
    		dispY = pY + 20 * currentZoomLevel*yMagFactor;
    	else
    		dispY = ymin + 20 * currentZoomLevel*yMagFactor;
        for (double i = NsharpConstants.ICING_RELATIVE_HUMIDITY_LEFT; i <= NsharpConstants.ICING_RELATIVE_HUMIDITY_RIGHT; i= i+NsharpConstants.ICING_RELATIVE_HUMIDITY_INC) {
        	// temperature/humidity vertical lines
        	double [][] lines = {{world.mapX(i), iceYOrig},{world.mapX(i), iceYEnd}};
        	linesNumbersShape.addLineSegment(lines);
        	//RH label
        	s = NsharpConstants.pressFormat.format(i);
        	
        	double [] lblXy = {world.mapX(i), dispY};
        	RHLabelShape.addLabel(s, lblXy);
        }
        //temperature label
        double [] lblTXy = {iceXOrig+ iceWidth/2, iceYEnd+20*yMagFactor};
        tempLabelShape.addLabel("TEMPERATURE (C)", lblTXy);
    	double [][] lineT = {{0,0},{0,0}};
    	tempLabelShape.addLineSegment(lineT);//add dummy line
    	//set world based on pressure/twmp
    	world.setWorldCoordinates(NsharpConstants.ICING_TEMPERATURE_LEFT, toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_TOP),
        		NsharpConstants.ICING_TEMPERATURE_RIGHT, toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM));
    	pY = world.mapY(toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM));
    	if(ymax > pY)
    		dispY = pY - 10 * currentZoomLevel*yMagFactor;
    	else
    		dispY = ymax - 10 * currentZoomLevel*yMagFactor;
    	for (double i = NsharpConstants.ICING_TEMPERATURE_LEFT; i <= NsharpConstants.ICING_TEMPERATURE_RIGHT; i= i+NsharpConstants.ICING_TEMPERATURE_INC) {
        	// temperature label
        	s = NsharpConstants.pressFormat.format(i);
        	
        	double [] lblXy = {world.mapX(i),dispY};// iceYEnd+10};
        	tempLabelShape.addLabel(s, lblXy);
        }
    	//EPI label
    	double [] lblEPIXy = {iceXOrig+ iceWidth/2, iceYEnd+35*yMagFactor};
    	EPILabelShape.addLabel("EQUIVALENT POTENTIAL INSTABILITY x 1E-3 K/m", lblEPIXy);
    	double [][] lineE = {{0,0},{0,0}};
    	EPILabelShape.addLineSegment(lineE);//add dummy line
        linesNumbersShape.compile();
        RHLabelShape.compile();
        tempLabelShape.compile();
        EPILabelShape.compile();
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
			if(RHLabelShape!= null){
				RHLabelShape.dispose();
			}
			if(tempLabelShape!= null){
				tempLabelShape.dispose();
			}
			if(EPILabelShape!= null){
				EPILabelShape.dispose();
			}
			createAllShapes();
		}
		this.smallFont.setSmoothing(false);
		this.smallFont.setScaleFont(false);
		target.drawWireframeShape(linesNumbersShape, NsharpConstants.pressureColor, 1, LineStyle.SOLID,smallFont);
		HashMap<String, NsharpLineProperty> lpMap = ((NsharpSkewTPaneDescriptor)desc).getSkewtResource().getLinePropertyMap();
		if(lpMap!=null){
			NsharpLineProperty lp =lpMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_ICING_RH]);
			target.drawWireframeShape(RHLabelShape, lp.getLineColor(), 1, LineStyle.SOLID,smallFont);
			lp =lpMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_ICING_TEMP]);
			target.drawWireframeShape(tempLabelShape,lp.getLineColor(), 1, LineStyle.SOLID,smallFont);
			lp =lpMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_ICING_EPI]);
			target.drawWireframeShape(EPILabelShape, lp.getLineColor(), 1, LineStyle.SOLID,smallFont);
		}
		else {
			target.drawWireframeShape(RHLabelShape, NsharpConstants.color_green, 1, LineStyle.SOLID,smallFont);
			target.drawWireframeShape(tempLabelShape, NsharpConstants.color_red, 1, LineStyle.SOLID,smallFont);
			target.drawWireframeShape(EPILabelShape, NsharpConstants.color_violet_red, 1, LineStyle.SOLID,smallFont);
		}
		
	}
	@Override
	public void disposeInternal() {
		// TODO Auto-generated method stub
		super.disposeInternal();
		if(linesNumbersShape!= null){
			linesNumbersShape.dispose();
		}
		if(RHLabelShape!= null){
			RHLabelShape.dispose();
		}
		if(tempLabelShape!= null){
			tempLabelShape.dispose();
		}
		if(EPILabelShape!= null){
			EPILabelShape.dispose();
		}
	}
	@Override
    public double getViewableMaxPressure(){
    	IExtent ext = desc.getRenderableDisplay().getExtent();
    	double ymax = ext.getMaxY();
    	double xmin = ext.getMinX();  
    	world.setWorldCoordinates(NsharpConstants.ICING_RELATIVE_HUMIDITY_LEFT, toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_TOP),
        		NsharpConstants.ICING_RELATIVE_HUMIDITY_RIGHT, toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM));
    	//Coordinate c = world.unMap(xmin, ymax);
    	double viewablePmax = reverseLogScale(world.unMap(xmin, ymax).y);
        return viewablePmax;
    }
    @Override
    public double getViewableMinPressure(){
    	IExtent ext = desc.getRenderableDisplay().getExtent();
    	double ymin = ext.getMinY();
    	double xmin = ext.getMinX();  
    	world.setWorldCoordinates(NsharpConstants.ICING_RELATIVE_HUMIDITY_LEFT, toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_TOP),
        		NsharpConstants.ICING_RELATIVE_HUMIDITY_RIGHT, toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM));
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
    public ViewablePressureContainer getViewablePressureLinesContainer(){
		ViewablePressureContainer vpc = new ViewablePressureContainer();
    	IExtent ext = desc.getRenderableDisplay().getExtent();
    	float ymax = (float)ext.getMaxY();
    	float ymin =(float) ext.getMinY();
    	double vPmax = getViewableMaxPressure();
    	double vPmin = getViewableMinPressure();
    	vpc.maxVIewablePressure = (float)vPmax;
    	vpc.minVIewablePressure = (float)vPmin;
    	for (float pressure = 300; pressure <= 1000;pressure= pressure+100) {
    		if(pressure >= vPmin &&pressure <= vPmax){
    			double pY= world.mapY(toLogScale(pressure));
    	        float ratio = (float) (pY-ymin) / (ymax-ymin);
    	        vpc.pyMap.put(pressure, ratio);
    	       //System.out.println("Icing press="+pressure+" ratio="+ratio+ " pY="+pY+" maxPy="+ymax+" minPy="+ymin);
    		}
    	}
    	return vpc;
    }
    @Override
	public ViewablePressureContainer getViewablePressureContainer(List<NcSoundingLayer> soundingLys){
    	world.setWorldCoordinates(NsharpConstants.ICING_RELATIVE_HUMIDITY_LEFT, toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_TOP),
        		NsharpConstants.ICING_RELATIVE_HUMIDITY_RIGHT, toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM));
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
		float prevWidth = iceWidth;
    	paneHeight = (int) (ext.getHeight());
    	yMagFactor =  yMagFactor*((float)paneHeight/prevHeight);
    	labelSpace = defaultLabelSpace * yMagFactor;
		iceXOrig=(int) (ext.getMinX());
    	iceYOrig=(int) (ext.getMinY())+ (int)labelSpace;
    	iceXEnd=iceXOrig+(int) (ext.getWidth());
    	iceYEnd=iceYOrig+(int) (ext.getHeight()) - 2*(int)labelSpace;
    	iceWidth = (int) (ext.getWidth());
    	xMagFactor =  xMagFactor* ((float)iceWidth/prevWidth);
		this.rectangle = new Rectangle(iceXOrig, iceYOrig,
				iceWidth, (int) ext.getHeight()-2*(int)labelSpace);
        pe = new PixelExtent(this.rectangle);
        //desc.setNewPe(pe);
        world = new WGraphics(this.rectangle);
        
        if(linesNumbersShape!= null){
			linesNumbersShape.dispose();
		}
        if(RHLabelShape!= null){
			RHLabelShape.dispose();
		}
		if(tempLabelShape!= null){
			tempLabelShape.dispose();
		}
		if(EPILabelShape!= null){
			EPILabelShape.dispose();
		}
        createAllShapes();
    }
    public void handleZooming(){
    	if(linesNumbersShape!= null){
			linesNumbersShape.dispose();
		}
        if(RHLabelShape!= null){
			RHLabelShape.dispose();
		}
		if(tempLabelShape!= null){
			tempLabelShape.dispose();
		}
		if(EPILabelShape!= null){
			EPILabelShape.dispose();
		}
        createAllShapes();
    }
    public double getWindBarbXPosition(){
		IExtent ext = desc.getRenderableDisplay().getExtent();
        double xmax = ext.getMaxX();  //Extent's viewable envelope min x and y
        double ymax = ext.getMaxY();
        double pX= world.mapX( NsharpConstants.ICING_RELATIVE_HUMIDITY_RIGHT);
        if(pX < xmax)
        	xmax = pX;
        
        double windBarbSizfactor = graphConfigProperty.getWindBarbSize()/2.2f;
        if(windBarbSizfactor < 1)
        	windBarbSizfactor=1;
        double dispX = xmax - 50 * currentZoomLevel * xMagFactor* windBarbSizfactor;
        
        Coordinate cumap = world.unMap(dispX,ymax);
        
        return cumap.x;
	}
}
