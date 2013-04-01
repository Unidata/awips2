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
import gov.noaa.nws.ncep.ui.nsharp.NsharpGraphProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpLineProperty;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpAbstractPaneDescriptor;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNativeConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.core.graphing.WGraphics;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpAbstractPaneResource extends AbstractVizResource<AbstractResourceData, NsharpAbstractPaneDescriptor>{
	NsharpNative nsharpNative=null;
	protected IGraphicsTarget target=null;
	protected Rectangle rectangle;
	protected WGraphics world;
	protected PixelExtent pe;
	protected static final UnitConverter celciusToFahrenheit = SI.CELSIUS.getConverterTo(NonSI.FAHRENHEIT);
	protected static final UnitConverter celciusToKelvin = SI.CELSIUS.getConverterTo(SI.KELVIN);
	protected List<NcSoundingLayer> soundingLys = null;
	protected List<NcSoundingLayer> previousSoundingLys = null;
	protected NsharpResourceHandler rscHandler=null;
	protected NsharpGraphProperty graphConfigProperty=null;
	protected HashMap<String, NsharpLineProperty> linePropertyMap=null; 
	protected int currentSoundingLayerIndex =0;
	protected IFont font9=null;
    protected IFont font10=null;
    protected IFont font11=null;
    protected IFont font12=null;
    protected float currentFont10Size=10;
    protected int commonLinewidth;
    protected LineStyle commonLineStyle;
    protected Coordinate interactiveTempPointCoordinate;
    protected Float currentZoomLevel=1f;
    protected float currentCanvasBoundWidth;//= NsharpConstants.DEFAULT_CANVAS_WIDTH;
    protected float currentCanvasBoundHeight;  //= NsharpConstants.DEFAULT_CANVAS_HEIGHT;
    protected float myDefaultCanvasHeight;// = NsharpConstants.DEFAULT_CANVAS_HEIGHT*4/5;
    protected float myDefaultCanvasWidth;//= NsharpConstants.DEFAULT_CANVAS_WIDTH/2;
	//protected Float zoomLevel;
	protected boolean resize=false;
	protected String paneConfigurationName; 
    public static final float INVALID_DATA = NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;
	protected Coordinate cursorCor;
	protected int charHeight = NsharpConstants.CHAR_HEIGHT_;
	protected double charWidth;
    protected PaintProperties paintProps;
	
	public NsharpAbstractPaneResource(AbstractResourceData resourceData,
			LoadProperties loadProperties, NsharpAbstractPaneDescriptor desc) {
		super(resourceData, loadProperties);
		descriptor = desc;
		this.dataTimes = new ArrayList<DataTime>();
		
	}

	@Override
	protected void disposeInternal() {
		if(font9!=null){
			font9.dispose();
			font9=null;
		}
		if(font10!=null){
			font10.dispose();
			font10=null;
		}
		if(font11!=null){
			font11.dispose();
			font11=null;
		}
		if(font12!=null){
			font12.dispose();
			font12=null;
		} 
		this.target.dispose();
		target = null;
	}


	@Override
	protected void paintInternal(IGraphicsTarget target,
			PaintProperties paintProps) throws VizException {
		this.paintProps = paintProps;
		this.target = target;
		if(rscHandler== null || rscHandler.getSoundingLys()==null)
			return;
		float zoomLevel = paintProps.getZoomLevel();
		/*if( currentCanvasBoundWidth!= paintProps.getCanvasBounds().width || currentCanvasBoundHeight!=paintProps.getCanvasBounds().height){

			currentCanvasBoundWidth= paintProps.getCanvasBounds().width;
			currentCanvasBoundHeight=paintProps.getCanvasBounds().height;
			adjustFontSize(currentCanvasBoundWidth,currentCanvasBoundHeight);
		}
		*/
		//System.out.println("currentZoomLevel="+currentZoomLevel+" paintProps's zoomLevel="+zoomLevel);
		if(zoomLevel > 1.0f)
			zoomLevel = 1.0f;
		if((zoomLevel != currentZoomLevel)  ){
			currentZoomLevel = zoomLevel;
			handleZooming();
			
		}
		if(this.resize==true ){
			handleResize();
		}
	}

	@Override
	protected void initInternal(IGraphicsTarget target) throws VizException {	
		this.target = target;
		this.font9 = target.initializeFont("Monospace", 9, null);
		this.font10 = target.initializeFont("Monospace", 10, null);
		this.font11 = target.initializeFont("Monospace", 11, null);
		IFont.Style[] style = {IFont.Style.BOLD};
		this.font12 = target.initializeFont("Monospace", 12, style);
		this.font9.setSmoothing(false);
		this.font9.setScaleFont(false);
		this.font10.setSmoothing(false);
		this.font10.setScaleFont(false);
		this.font11.setSmoothing(false);
		this.font11.setScaleFont(false);
		this.font12.setSmoothing(false);
		this.font12.setScaleFont(false);
		commonLinewidth = getCapability(OutlineCapability.class).getOutlineWidth();
        commonLineStyle = getCapability(OutlineCapability.class)
                .getLineStyle();
        this.resize=true ;
		//nsharpNative = new NsharpNative();	
		//System.out.println("NsharpDefaultPaneResource ::: initInternal with native "+ nsharpNative.toString());
	}
	
	@SuppressWarnings("deprecation")
	public void resetData(List<NcSoundingLayer> soundingLys, List<NcSoundingLayer> prevsoundingLys){
		this.soundingLys = soundingLys;
		this.previousSoundingLys = prevsoundingLys;
		descriptor.setFrame(0);
	}

	public WGraphics getWorld() {
		return world;
	}
	
	
	protected void adjustFontSize(float canvasW, float canvasH ) {
		float font9Size,font10Size,font11Size,font12Size;
		
		float fontAdjusted=0;
		float fontBaseH=90f; //Chin:  why 70 & 100? After many "try and error" experiments...
		float fontBaseW=120f;
		if(canvasH < myDefaultCanvasHeight && canvasW< myDefaultCanvasWidth){
			//both width and height are smaller than default
			float wAdjust = (float)(myDefaultCanvasWidth-canvasW)/fontBaseW;
			float hAdjust = (float)(myDefaultCanvasHeight-canvasH)/fontBaseH;
			fontAdjusted = Math.max(wAdjust,hAdjust);
		}
		else if(canvasW< myDefaultCanvasWidth){
			// only width smaller than default
			fontAdjusted = (float)(myDefaultCanvasWidth-canvasW)/fontBaseW;
		}
		else if(canvasH < myDefaultCanvasHeight ){
			// only height smaller than default
			fontAdjusted = (float)(myDefaultCanvasHeight-canvasH)/fontBaseH;
		}
		//Chin: Can not bigger than 9, otherwise, fint9 size willbe negative. 
		//Why 8.8 ? After many "try and error" experiments...
		if(fontAdjusted > 8.8) 
			fontAdjusted=8.8f;
				
		font9Size = 9-fontAdjusted;
		font10Size = 10-fontAdjusted;
		font11Size =11-fontAdjusted;
		font12Size = 12-fontAdjusted;

		if(font9!=null){
			font9.dispose();
		}
		font9 = target.initializeFont("Monospace", font9Size, null);
		
		if(font10!=null){
			font10.dispose();
		}
		font10 = target.initializeFont("Monospace", font10Size, null);
		if(font11!=null){
			font11.dispose();
		}
		font11 = target.initializeFont("Monospace", font11Size, null);
		if(font12!=null){
			font12.dispose();
		}
		IFont.Style[] style = {IFont.Style.BOLD};
		font12 = target.initializeFont("Monospace", font12Size, style);
		currentFont10Size = font10Size;
		//System.out.println(descriptor.getPaneNumber()+": adjusted font10 size ="+currentFont10Size);	
	}
	protected void magnifyFont(double zoomLevel) {
		float magFactor = 1.0f / (float)zoomLevel;
		font9.setMagnification(magFactor);
		font10.setMagnification(magFactor);
		font11.setMagnification(magFactor);
		font12.setMagnification(magFactor);
	}
	@Override
    public void setDescriptor(NsharpAbstractPaneDescriptor descriptor) {
        super.setDescriptor(descriptor);
        RGB rgb = ColorUtil.getNewColor(descriptor);
        getCapability(ColorableCapability.class).setColor(rgb);
        //System.out.println("screwT Rsc  setDescriptor called");
    }

	public void setSoundingLys(List<NcSoundingLayer> soundingLys) {
		this.soundingLys = soundingLys;
	}

	public HashMap<String, NsharpLineProperty> getLinePropertyMap() {
		return linePropertyMap;
	}

	public void setLinePropertyMap(
			HashMap<String, NsharpLineProperty> linePropertyMap) {
		this.linePropertyMap = linePropertyMap;
		
	}

	public NsharpGraphProperty getGraphConfigProperty() {
		return graphConfigProperty;
	}

	public void setGraphConfigProperty(NsharpGraphProperty graphConfigProperty) {
		this.graphConfigProperty = graphConfigProperty;
		paneConfigurationName = this.graphConfigProperty.getPaneConfigurationName();
		
	}

	public NsharpResourceHandler getRscHandler() {
		return rscHandler;
	}

	public int getCurrentSoundingLayerIndex() {
		return currentSoundingLayerIndex;
	}

	public void setRscHandler(NsharpResourceHandler rscHandler) {
		this.rscHandler = rscHandler;
		if(descriptor != null)
			descriptor.setRscHandler(rscHandler);
	}

	public void setNsharpNative(NsharpNative nsharpNative) {
		this.nsharpNative = nsharpNative;
	}
	public void handleResize(){
		this.resize=false;
		//double vertRatio = paintProps.getView().getExtent().getHeight() / paintProps.getCanvasBounds().height;
		//double hRatio = paintProps.getView().getExtent().getWidth() / paintProps.getCanvasBounds().width;
		//System.out.println(descriptor.getPaneNumber()+"viewWidth="+paintProps.getView().getExtent().getWidth()+" viewHeight="+paintProps.getView().getExtent().getHeight() );
		//System.out.println(descriptor.getPaneNumber()+"canvWidth="+paintProps.getCanvasBounds().width+" canvHeight="+paintProps.getCanvasBounds().height );
		//System.out.println(descriptor.getPaneNumber()+": vertRatio="+vertRatio + " hRatio="+hRatio);	
		if(paintProps!=null &&  (currentCanvasBoundWidth!= paintProps.getCanvasBounds().width || currentCanvasBoundHeight!=paintProps.getCanvasBounds().height)){
			currentCanvasBoundWidth= paintProps.getCanvasBounds().width;
			currentCanvasBoundHeight=paintProps.getCanvasBounds().height;
			adjustFontSize(currentCanvasBoundWidth,currentCanvasBoundHeight);
		}
		
	}
	public void setResize(boolean resize) {
		this.resize = resize;
	}
	public void handleZooming(){
		
	}
	protected void defineCharHeight(IFont font){
		if(paintProps == null)
			return;
		DrawableString str =new DrawableString("CHINCHEN",NsharpConstants.color_black);
		str.font = font;
		double vertRatio = paintProps.getView().getExtent().getHeight() / paintProps.getCanvasBounds().height;
		double horizRatio = paintProps.getView().getExtent().getWidth() / paintProps.getCanvasBounds().width;
		charHeight = (int) (target.getStringsBounds(str).getHeight() * vertRatio);
		charWidth = target.getStringsBounds(str).getWidth() * horizRatio /8;
		//System.out.println(descriptor.getPaneNumber()+": font10 char height ="+charHeight+ " vertRatio="+vertRatio);	
	}
}
