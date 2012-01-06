/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource
 * 
 * This java class performs the NSHARP NsharpSkewTResource functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 * 										Reused some software from com.raytheon.viz.skewt
 * 06/14/2011   11-5        Chin Chen   migration
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.skewt.rsc;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.maprsc.NsharpMapMouseHandler;
import gov.noaa.nws.ncep.ui.nsharp.maprsc.NsharpMapResource;
import gov.noaa.nws.ncep.ui.nsharp.menu.NsharpLoadDialog;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpDataHandling;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNativeConstants;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative.NsharpLibrary._lplvalues;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative.NsharpLibrary._parcel;
import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpGraphConfigDialog;
import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpParcelDialog;
import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpShowTextDialog;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDescriptor;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDisplay;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTEditor;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FrameChangeMode;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FrameChangeOperation;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.sounding.SoundingParams;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.WxMath;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.core.graphing.LineStroke;
import com.raytheon.viz.core.graphing.WGraphics;
import com.raytheon.viz.core.graphing.WindBarbFactory;
import com.sun.jna.ptr.FloatByReference;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpSkewTResource extends
    AbstractVizResource<AbstractResourceData, NsharpSkewTDescriptor> {
	private NsharpSkewTDescriptor desc=null;
	NsharpNative nsharpNative=null;
	private static final int  DATAPAGEMAX =5;
	private static final int  INSETPAGEMAX =2;
	private int currentTextPage= 1;
	private int currentInsetPage= 1;
	/* Hodograph Modes - definition is based on definitions in globals_xw.h of BigNsharp  */
	private static final int HODO_NORMAL	=		0;
	//private static int HODO_EFFECTIVE=		1; not used in BigNsharp source code
	private static final int HODO_STORMRELATIVE=	2;
	private static final int HODO_BNDRY=			3;
	private static final int HODO_MEANWIND=		4;
	private int currentHodoWindMode = HODO_MEANWIND;
	
	private int parcelLinesInPhysicalPanelNumber = 1;
	private boolean overlayIsOn = false;
	private boolean interpolateIsOn = false;
	private boolean compareIsOn = false;
	private boolean editGraphOn=false;
	private IGraphicsTarget target=null;
	private RGB wwTypeColor;
	private boolean cursorInSkewT = false;
	private Coordinate cursorCor;
	private double currentZoomLevel=1;
	private int currentCanvasBoundWidth= NsharpConstants.DEFAULT_CANVAS_WIDTH;
	private int currentCanvasBoundHeight= NsharpConstants.DEFAULT_CANVAS_HEIGHT;
	
	public boolean isCompareIsOn() {
		return compareIsOn;
	}
	public void setCursorInSkewT(boolean cursorInSkewT, Coordinate c) {
		this.cursorInSkewT = cursorInSkewT;
		this.cursorCor = c;
	}
	public int getParcelLinesInPhysicalPanelNumber() {
		return parcelLinesInPhysicalPanelNumber;
	}
	public NsharpNative getNsharpNative() {
		return nsharpNative;
	}
	
	public void setNextTextPage(){
		if(currentTextPage == DATAPAGEMAX){
			currentTextPage = 1;
		}
		else
			currentTextPage++;
	}
	public void setNextInsetPage(){
		if(currentInsetPage == INSETPAGEMAX){
			currentInsetPage = 1;
		}
		else
			currentInsetPage++;
	}
	
	
	public void setOverlayIsOn(boolean overlayIsOn) {
		this.overlayIsOn = overlayIsOn;
		createRscHodoWindShapeAll();
		createRscPressTempCurveShapeAll();
	}
	public void setInterpolateIsOn(boolean interpolateIsOn) {
		this.interpolateIsOn = interpolateIsOn;
		
	}
	public void setCompareIsOn(boolean compareIsOn) {
		this.compareIsOn = compareIsOn;
		createRscHodoWindShapeAll();
		createRscPressTempCurveShapeAll();
	}

	public void setEditGraphOn(boolean editGraphOn) {
		this.editGraphOn = editGraphOn;
	}

	public boolean isEditGraphOn() {
		return editGraphOn;
	}

	public int TEMP_TYPE = 1;
	public int DEWPOINT_TYPE = 2;
	private int currentTempCurveType;
	private int currentSoundingLayerIndex =0;
	private int hodoEditingSoundingLayerIndex =0;
	private boolean plotInteractiveTemp= false;
	//private boolean compareSoundingIsOff = true;
	private Coordinate interactiveTempPointCoordinate;
	private int curTimeLinePage=1;
	private int totalTimeLinePage=1;
	private List<NcSoundingLayer> intpSndLst = new ArrayList<NcSoundingLayer>();
	public static final float INVALID_DATA = NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;
    //protected static final int TEMP_CHANGE_WINDOW = 4;

    protected static final double BARB_LENGTH = 3.5;
    
    private String soundingType= null;

    protected Map<Date, SoundingParams> soundingMap;

    protected DataTime displayedSounding;

    
    
    private boolean sortByStn = true;
    //protected boolean editable = false;
    
    
    protected IFont font9=null;
    protected IFont font10=null;
    protected IFont font11=null;
    protected IFont font12=null;
    private float currentFont10Size=10;
    
    protected ListenerList listenerList = new ListenerList();
    
    private List<List<NcSoundingLayer>> soundingLysList = null;
    
    //current active sounding layer list
	private List<NcSoundingLayer> soundingLys = null;
	//private List<SoundingLayer> originalSoundingLys = null;
	private List<NcSoundingLayer> previousSoundingLys = null;
	private String pickedStnInfoStr; // current picked stn info with time line, e.g. "ATLH 2010-12-12 12:00:00"
	//private String preStnInfoStr; // previous picked stn info with time line, e.g. "ATLH 2010-12-12 12:00:00"
	private NsharpStationInfo pickedStnInfo = null;
	private int pickedDataTimeLineIndex=0;
	private int preDataTimeLineIndex=0;
	private FrameChangeOperation currentOpDirection = FrameChangeOperation.NEXT; // next =forward
	private int commonLinewidth;
	private LineStyle commonLineStyle;
	
	private HashMap<Integer, RGB> stormSlinkyColorMap = new HashMap<Integer, RGB>();
    //list of sounding layer list for each data time line
    private HashMap<String, List<NcSoundingLayer>> dataTimelineSndLysListMap = new HashMap<String, List<NcSoundingLayer>>();
    private HashMap<String, List<NcSoundingLayer>> originalDataTimelineSndLysListMap= new HashMap<String, List<NcSoundingLayer>>();
	enum State {
		PICKED, GROUPED, IDLE//was , DISABLED
	}
	private static int PICKED_COLOR=1;
	private static int GROUPED_COLOR=2;
	private static int IDLE_COLOR=3;
	//private static int DISABLED_COLOR=4;
	private HashMap<Integer, RGB> elementColorMap = new HashMap<Integer, RGB>();
	public class ElementStateProperty {
	   String elementDescription;
	   State elementState;
	   Integer elementColor;
	   NsharpStationInfo stnInfo;
	   public NsharpStationInfo getStnInfo() {
		   return stnInfo;
	   }
	   public void setStnInfo(NsharpStationInfo stnInfo) {
		   this.stnInfo = stnInfo;
	   }
	   public String getElementDescription() {
		   return elementDescription;
	}   
    }
    //dataTimelineArray: time line selected by user, but is updated based on available time line at DB at setRsc()
	// this is derived from dataTimelineSndLysListMap. It has stn info + sounding time line info
   // used field is used to identify if this time line is picked by user. user could pick multiple time lines for comparison
	private List<ElementStateProperty> dataTimelineList=null;
	public List<ElementStateProperty> getDataTimelineList() {
		return dataTimelineList;
	}

	//stationIdList derived from dataTimelineArray, It only has station id info
	private List<ElementStateProperty> stationIdList=null;
	
	private NsharpDrawPanels drawPanel; 
    
	public class ParcelData{
		short parcelType;
		float parcelLayerPressure;
		
		public short getParcelType() {
			return parcelType;
		}
		public void setParcelType(short parcelType) {
			this.parcelType = parcelType;
		}
		public float getParcelLayerPressure() {
			return parcelLayerPressure;
		}
		public void setParcelLayerPressure(float parcelLayerPressure) {
			this.parcelLayerPressure = parcelLayerPressure;
		}
		
		
	};
	private List<ParcelData> parcelList = new ArrayList<ParcelData>(); 
	private short currentParcel = NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;
	private float currentParcelLayerPressure = NsharpNativeConstants.MU_LAYER; 

	public short getCurrentParcel() {
		return currentParcel;
	}

	//native nsharp c/fortran lib
	//NsharpNative is native nsharp lib awips/lib/libnsharp.so wrapper class
	private Integer     	markerWidth = 1;
	private Coordinate hodoHouseC = new Coordinate(NsharpConstants.HODO_CENTER_X, NsharpConstants.HODO_CENTER_Y);
	private float smWindDir, smWindSpd;
	//shape and color storage
	public class ShapeAndColor {
		public IWireframeShape shape;
		public RGB color;
	}
	//dynamic resource shapes
	private IWireframeShape heightMarkRscShape=null;
	private IWireframeShape omegaRscShape=null;
	private IWireframeShape wetBulbTraceRscShape = null;
	private IWireframeShape vtempTraceCurveRscShape = null;
	private IWireframeShape thetaEPressureYRscShape = null;
	private IWireframeShape thetaEPressureWRscShape = null;
	private IWireframeShape thetaEPressureRRscShape = null;
	private IWireframeShape thetaEHeightYRscShape = null;
	private IWireframeShape thetaEHeightWRscShape = null;
	private IWireframeShape thetaEHeightRRscShape = null;
	private IWireframeShape srWindBRscShape = null;
	private IWireframeShape srWindWRscShape = null;
	private IWireframeShape srWindRRscShape = null;
	private IWireframeShape srWindGRscShape = null;
	private IWireframeShape srWindMRscShape = null;	
	private IWireframeShape verticalWindLabelShape = null;
	private IWireframeShape verticalWindSbShape = null;
	private IWireframeShape verticalWindRShape = null;
	private List<IWireframeShape> parcelTraceRscShapeList = new ArrayList<IWireframeShape>();
	private List<ShapeAndColor>hodoWindRscShapeList  = new ArrayList<ShapeAndColor>();
	private List<ShapeAndColor>pressureTempRscShapeList  = new ArrayList<ShapeAndColor>();
	private List<ShapeAndColor>windBoxWindRscShapeList  = new ArrayList<ShapeAndColor>();
	//static bk shape that not handled at background resource class
	private IWireframeShape omegaBkgShape = null;
	private IWireframeShape psblWatchTypeBkgShape = null;
	private IWireframeShape windBoxBkgShape = null;
	private IWireframeShape hodoWindMotionBoxShape = null;
	
	
	public float getSmWindDir() {
		return smWindDir;
	}
	public float getSmWindSpd() {
		return smWindSpd;
	}

	public void setCurrentHodoWindMode(int cursorPositionIndex) {
		switch(cursorPositionIndex){
		case 0:
			this.currentHodoWindMode = HODO_NORMAL;
			break;
		case 1:
			this.currentHodoWindMode = HODO_STORMRELATIVE;
			break;
		case 2:
		default:
			this.currentHodoWindMode = HODO_MEANWIND;
			break;
		}
	}


	
	//private void setElementStatePropertyList(List<ElementStateProperty> eleList, State sta,int color ) {
	//	for (ElementStateProperty e: eleList){
	//		e.elementState = sta;
	//		e.elementColor = color;
	//	}
	//}
	private void setStnDataTimelineStatus(String stn, State sta, int color) {
		for (ElementStateProperty e: dataTimelineList){
			StringTokenizer st1 = new StringTokenizer(e.elementDescription);
			String tok = st1.nextToken();
			if(tok.equals(stn)){
				e.elementState = sta;
				e.elementColor = color;
			}
		}
	}
	private void refreshPickedTimeLineState(){
		dataTimelineList.get(pickedDataTimeLineIndex).elementState = State.PICKED;
		dataTimelineList.get(pickedDataTimeLineIndex).elementColor = PICKED_COLOR;
		if(preDataTimeLineIndex != pickedDataTimeLineIndex){
			dataTimelineList.get(preDataTimeLineIndex).elementState = State.GROUPED;
			dataTimelineList.get(preDataTimeLineIndex).elementColor = GROUPED_COLOR;
		}
	}
	public NsharpStationInfo getPickedStnInfo() {
		return pickedStnInfo;
	}
    public String getSoundingType() {
		return soundingType;
	}


	public void setSoundingType(String soundingType) {
		this.soundingType = soundingType;
	}

	public static NsharpSkewTResource createSkewtResource() {
        LoadProperties loadProperties1 = new LoadProperties();
        ColorableCapability colorable1 = new ColorableCapability();
        colorable1.setColor(NsharpConstants.backgroundColor);
        loadProperties1.getCapabilities().addCapability(colorable1);
        return new NsharpSkewTResource(new NsharpSkewTResourceData(),
                loadProperties1);
    }
	
    public List<List<NcSoundingLayer>> getSoundingLysList() {
		return soundingLysList;
	}
    
    public void setCurrentParcel(short currentParcel) {
		this.currentParcel = currentParcel;
		currentParcelLayerPressure=NsharpNativeConstants.parcelToLayerMap.get(currentParcel);
		//inform draw panel as well
		if(drawPanel!=null){
			drawPanel.setCurrentParcel(currentParcel);
		}
	}
	/*
     * NOTE:::ONly one parcel will be in parcel list as current design changed to only show one configured parcel
     * This is how BigNsharp does.
     * Todo: replace List<ParcelData>  with just one ParcelData
     */
	public void setParcelList(List<ParcelData> parcelList) {
		this.parcelList = parcelList;
		if(parcelTraceRscShapeList.size()>0){
			for(IWireframeShape shape: parcelTraceRscShapeList){
				shape.dispose();
			}
			parcelTraceRscShapeList.clear();
		}
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();   
		WGraphics WGc = bkRsc.getSkewTBackground().getWorld();
		for (ParcelData parData: parcelList){
			createRscParcelTraceShape( WGc, parData.parcelType,parData.parcelLayerPressure);
		}
	}
	public void updateParcelFromPanel(short currentParcel){
		this.currentParcel = currentParcel;
		currentParcelLayerPressure=NsharpNativeConstants.parcelToLayerMap.get(currentParcel);
		//update parcel shape
		List<ParcelData> parcelList = new ArrayList<ParcelData>(); 
		ParcelData pd= new ParcelData();
		pd.setParcelType(currentParcel);
		pd.setParcelLayerPressure(currentParcelLayerPressure);
		parcelList.add(pd);
		setParcelList(parcelList);
	}
    /*
     * NOTE:::when called, it assumed that this new element is current parcel. Therefore caller has to make sure of this.
     *
	public void addParcelToList(ParcelData parceldata) {
		boolean addToList = true;
		for(ParcelData pdata : parcelList){
			if((pdata.parcelType == parceldata.parcelType) && (pdata.parcelLayerPressure == parceldata.parcelLayerPressure)){
				addToList= false;
				break;
			}
		}
		if(addToList== true)
			this.parcelList.add(parceldata);
		
		currentParcel = parceldata.getParcelType();
		currentParcelLayerPressure = parceldata.getParcelLayerPressure();
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();   	
		WGraphics WGc = bkRsc.getSkewTBackground().getWorld();
		createRscParcelTraceShape( WGc, parceldata.parcelType,parceldata.parcelLayerPressure);
		
	}*/


	public void setSoundingLysList(List<List<NcSoundingLayer>> soundingLysList) {
		this.soundingLysList = soundingLysList;
	}
	
	public void setHodoHouseC(Coordinate hodoHouseC) {
		this.hodoHouseC = hodoHouseC;
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource(); 
		Coordinate c = bkRsc.getHodoBackground().getWorld().unMap(hodoHouseC.x, hodoHouseC.y);
		c = WxMath.speedDir((float) c.x, (float) c.y);
		smWindDir = (float) c.y;
		smWindSpd = (float)c.x;
		//System.out.println("setHodo smWindDir="+smWindDir+" smWindSpd="+smWindSpd);
		nsharpNative.nsharpLib.set_storm(smWindSpd, smWindDir);
		
		WGraphics WGc = bkRsc.getPsblWatchTypeBackground().getWorld();
		createBkgPsblWatchShape(WGc);
		//Sr wind vs Height graph shape need to recreate
		WGc=  bkRsc.getSrWindsBackground().getWorld();	
		createRscSrWindShape(WGc);
		
	}
	
	//This function is called only when interpolation "on/off" is changed by user
	public void resetInfoOnInterpolate(boolean interpolateIsOn) throws CloneNotSupportedException{
		//We dont want to assume previous interpolation on/off state. So, reset soundingLys any how.
		this.interpolateIsOn = interpolateIsOn;
		if(interpolateIsOn==false){
				//System.out.println(" interpolate is off ");
			soundingLys = dataTimelineSndLysListMap.get(pickedStnInfoStr);
						
		}
		else{
			//interpolation is on 
			//we dont want to change original raw data, so use a deep copy			
			//List<NcSoundingLayer> intpSndLst = new ArrayList<NcSoundingLayer>();	
			intpSndLst.removeAll(intpSndLst);
			for(NcSoundingLayer layer: dataTimelineSndLysListMap.get(pickedStnInfoStr)){
				intpSndLst.add((NcSoundingLayer) layer.clone());
			}
			
			// interpolation
			intpSndLst = performInterpolation(intpSndLst);
		
			soundingLys = intpSndLst;
			
		}
		
		//#10440 test
		nsharpNative.populateSndgData(soundingLys);
		
		//re-create shape
		createRscWireFrameShapes();
		
	}
	/*
	public void swapOverlay(){
		//System.out.println("swapOverlay entered");
		if(previousSoundingLys!=null && previousSoundingLys.size()>0){
			List<NcSoundingLayer> tempSoundingLys = new ArrayList<NcSoundingLayer>();
			tempSoundingLys = previousSoundingLys;
			previousSoundingLys = soundingLys;
			soundingLys = tempSoundingLys;
			String tempStr = pickedStnInfoStr;
			pickedStnInfoStr = preStnInfoStr;
			preStnInfoStr = tempStr;
			int tempIndex = preDataTimeLineIndex;
			preDataTimeLineIndex = pickedDataTimeLineIndex;
			pickedDataTimeLineIndex = tempIndex;
		
			resetData();
		}
	}*/
	private void deepCopyDataMap(HashMap<String, List<NcSoundingLayer>> src, HashMap<String, List<NcSoundingLayer>> dest) {
		dest.clear();
		Set<String> keySet = src.keySet();
		for (String key: keySet){
			List<NcSoundingLayer> oriLys = src.get(key);
			List<NcSoundingLayer> lys = new ArrayList<NcSoundingLayer>(oriLys.size());
			for(NcSoundingLayer ly : oriLys){
				try {
					lys.add((NcSoundingLayer)ly.clone());
				} catch (CloneNotSupportedException e) {
					e.printStackTrace();
				}
			}
			dest.put(key, lys);
		}
	}
	public void resetRsc() {
		//System.out.println("resetRsc called");
		this.dataTimelineSndLysListMap = new HashMap<String, List<NcSoundingLayer>>();
		deepCopyDataMap(this.originalDataTimelineSndLysListMap,this.dataTimelineSndLysListMap);
		this.soundingLys = this.dataTimelineSndLysListMap.get(pickedStnInfoStr);
		//Set default parcel trace data
		parcelList.clear();
		ParcelData defParcel = new ParcelData();
		defParcel.setParcelType(NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE );
		defParcel.setParcelLayerPressure(NsharpNativeConstants.MU_LAYER);
		parcelList.add(defParcel);
		setParcelList(parcelList);
		setSoundingInfo(dataTimelineSndLysListMap.get(pickedStnInfoStr));
		currentTextPage = 1;
		overlayIsOn = false;
		interpolateIsOn = false;
		compareIsOn = false;
		editGraphOn = false;
		resetData();

	}
	@SuppressWarnings("deprecation")
	private synchronized void resetData(){
		//System.out.println("resetData called");
		//update active sounding layer and picked stn info						
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		
		//re-populate snd data to nsharp native code lib for later calculating
		nsharpNative.populateSndgData(soundingLys);// # 10440 test dataTimelineSndLysListMap.get(pickedStnInfoStr));
		//#10438  FloatByReference stwdir= new FloatByReference(-999);
		//FloatByReference stwspd= new FloatByReference(-999);
		//nsharpNative.nsharpLib.get_storm(stwspd, stwdir);
		//#10438 bkRsc.setSmDir(stwdir.getValue());
		//bkRsc.setSmSpd(stwspd.getValue());
		//notify window pallet to change storm relative motion number
		if(desc!=null){
			desc.setFrame(pickedDataTimeLineIndex);
		}
		
		if(soundingLys!= null && soundingLys.size() >0){
			//set initial hodohouseC
			FloatByReference dummy1= new FloatByReference(-999);
			FloatByReference dummy2= new FloatByReference(-999);
			//  #10438 FloatByReference wdir= new FloatByReference(-999);
			// FloatByReference wspd= new FloatByReference(-999);
			/* #10438 FloatByReference Surfpressure = new FloatByReference(-999);
			nsharpNative.nsharpLib.get_surface(Surfpressure, dummy1, dummy2);
			if(nsharpNative.nsharpLib.qc(Surfpressure.getValue()) == 1) {
				nsharpNative.nsharpLib.mean_wind(Surfpressure.getValue(), nsharpNative.nsharpLib.ipres (nsharpNative.nsharpLib.msl (6000.0F)),dummy1, dummy2, wdir, wspd);
				if( nsharpNative.nsharpLib.qc(wdir.getValue())==1 && nsharpNative.nsharpLib.qc(wspd.getValue())==1) {
					// ----- set hodo circle at Bunkers Right, Chin according to TTR6065 or RaytheonTicket#10438
					
					smWindDir = (wdir.getValue() + 30.0f)%360;
					smWindSpd = wspd.getValue() * 0.75f;
					Coordinate c = WxMath.uvComp(smWindSpd,smWindDir);
					hodoHouseC= bkRsc.getHodoBackground().getWorld().map(c);
					nsharpNative.nsharpLib.set_storm(smWindSpd, smWindDir);
				}
			}*/
			// ----- set hodo circle at Bunkers Right, Chin according to TTR6065 or RaytheonTicket#10438
			FloatByReference bwdir= new FloatByReference(-999);
			FloatByReference bwspd= new FloatByReference(-999);
			nsharpNative.nsharpLib.bunkers_storm_motion(dummy1, dummy2, bwdir, bwspd);
			Coordinate c = WxMath.uvComp(bwspd.getValue(),bwdir.getValue());
			hodoHouseC= bkRsc.getHodoBackground().getWorld().map(c);
			smWindSpd = bwspd.getValue();
			smWindDir = bwdir.getValue();
			nsharpNative.nsharpLib.set_storm(smWindSpd, smWindDir);
			//reset parcel
			currentParcel = NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;
			currentParcelLayerPressure = NsharpNativeConstants.MU_LAYER;
			//reset parcel dialog as well
			if(NsharpParcelDialog.getAccess()!=null){
				NsharpParcelDialog.getAccess().resetUserDefParcel();
			}
			//reset draw panel as well
			if(drawPanel!=null){
				drawPanel.resetCurrentParcel();
			}
		}
		if(target != null){
			createRscWireFrameShapes();
		}
		
		
	}
	//NOTE: this comparator is coded only for dataTimelineArray and stationIdList to use
	//compare station name first, then day, then hour
	//if only sta name available, then just compare sta name
	//e.g. stationIdList only contain stn name in its element
	public class ElementComparatorNDH  implements Comparator<ElementStateProperty>{
		@Override
		public int compare(ElementStateProperty o1, ElementStateProperty o2) {
			
			String s1tok1="", s1tok2="", s1tok3="";
			String s2tok1="", s2tok2="", s2tok3="";
			StringTokenizer st1 = new StringTokenizer(o1.elementDescription);
			int tkCount1 = st1.countTokens();
			if(tkCount1 < 3)
			{
				//stationIdList only contain stn name in its element
				s1tok1 = st1.nextToken(); //stn name
			}
			else{
				s1tok1 = st1.nextToken(); //stn name
				s1tok2 = st1.nextToken(); //day
				s1tok3 = st1.nextToken(); //hour
				
			}
			StringTokenizer st2 = new StringTokenizer(o2.elementDescription);
			int tkCount2 = st2.countTokens();
			if(tkCount2 < 3)
			{
				s2tok1 = st2.nextToken(); //stn name
			}
			else{
				s2tok1 = st2.nextToken(); //stn name
				s2tok2 = st2.nextToken(); //day
				s2tok3 = st2.nextToken(); //hour
				
			}
			if(s1tok1.compareTo(s2tok1) == 0){
				//same station name
				if(s1tok2.compareTo(s2tok2) == 0){
					//same day
					if(s1tok3.compareTo(s2tok3) == 0){
						//same hour
						return 0;
					}else if (s1tok3.compareTo(s2tok3) < 0){
						return 1;
					} else if (s1tok3.compareTo(s2tok3) > 0) {
						return -1;
					}
				}else if (s1tok2.compareTo(s2tok2) < 0){
					return 1;
				} else if (s1tok2.compareTo(s2tok2) > 0) {
					return -1;
				}
				
			} else if (s1tok1.compareTo(s2tok1) < 0){
				return -1;
			} else if (s1tok1.compareTo(s2tok1) > 0) {
				return 1;
			}
			return 0;
		}
	}
	//NOTE: this comparator is coded only for dataTimelineArray to use
	//compare  day first, then hour, then station name
	public class ElementComparatorDHN  implements Comparator<ElementStateProperty>{
		@Override
		public int compare(ElementStateProperty o1, ElementStateProperty o2) {
			
			String s1tok1="", s1tok2="", s1tok3="";
			String s2tok1="", s2tok2="", s2tok3="";
			StringTokenizer st1 = new StringTokenizer(o1.elementDescription);
			int tkCount1 = st1.countTokens();
			if(tkCount1 < 3)
			{
				return -1;
			}
			else{
				s1tok1 = st1.nextToken(); //stn name
				s1tok2 = st1.nextToken(); //day
				s1tok3 = st1.nextToken(); //hour
				
			}
			StringTokenizer st2 = new StringTokenizer(o2.elementDescription);
			int tkCount2 = st2.countTokens();
			if(tkCount2 < 3)
			{
				return -1;
			}
			else{
				s2tok1 = st2.nextToken(); //stn name
				s2tok2 = st2.nextToken(); //day
				s2tok3 = st2.nextToken(); //hour
				
			}
			
			if(s1tok2.compareTo(s2tok2) == 0){ //compare day
				//same day
				if(s1tok3.compareTo(s2tok3) == 0){//compare hour
					//same hour
					if(s1tok1.compareTo(s2tok1) == 0){//compare stn
						//same stn
						return 0;
					}else if (s1tok1.compareTo(s2tok1) < 0){
						return -1;
					} else if (s1tok1.compareTo(s2tok1) > 0) {
						return 1;
					}
				}else if (s1tok3.compareTo(s2tok3) < 0){
					return 1;
				} else if (s1tok3.compareTo(s2tok3) > 0) {
					return -1;
				}
				
			} else if (s1tok2.compareTo(s2tok2) < 0){
				return 1;
			} else if (s1tok2.compareTo(s2tok2) > 0) {
				return -1;
			}
			return 0;
		}
	}
	private void addStnIdToList(String stnId, NsharpStationInfo stnInfo){
		//System.out.println("stn to be added "+ stnId);
		for(ElementStateProperty tempNameStr: stationIdList){
			if(tempNameStr.elementDescription.equals(stnId) )
				return; //nothing to do as already in list
		}
		//to here, this new stn is not in list
		ElementStateProperty newStnElem = new ElementStateProperty();
		newStnElem.elementDescription = stnId;
		newStnElem.elementState = State.GROUPED;//State.PICKED;		
		newStnElem.elementColor = GROUPED_COLOR;//PICKED_COLOR;
		newStnElem.stnInfo = new NsharpStationInfo();
		newStnElem.stnInfo.setLatitude(stnInfo.getLatitude());
		newStnElem.stnInfo.setLongitude(stnInfo.getLongitude());
		newStnElem.stnInfo.setSndType(stnInfo.getSndType());
		
		stationIdList.add(newStnElem);
		Collections.sort(stationIdList, new ElementComparatorNDH());
	}
	private void findAndSetPickedTimeLine(){
		//reset picked stn info if necessary
		boolean pickedFound=false;
		for(int i=0; i < dataTimelineList.size(); i++){
			if(dataTimelineList.get(i).elementState == State.PICKED){
				pickedFound=true;
				pickedDataTimeLineIndex = i ;
				return;
			}
		}
		if(pickedFound==false){
			//picked first grouped one in list
			for(int i=0; i < dataTimelineList.size(); i++){
				if(dataTimelineList.get(i).elementState == State.GROUPED){
					pickedFound=true;
					pickedDataTimeLineIndex = i ;
					break;
				}
			}
			
		}
		if(pickedFound==false){
			//picked first idle one in list
			for(int i=0; i < dataTimelineList.size(); i++){
				if(dataTimelineList.get(i).elementState == State.IDLE){
					pickedFound=true;
					pickedDataTimeLineIndex = i ;
					break;
				}
			}
			
		}
		if(pickedFound==true){
			dataTimelineList.get(pickedDataTimeLineIndex).elementState = State.PICKED;
			dataTimelineList.get(pickedDataTimeLineIndex).elementColor = PICKED_COLOR;
			setPickedStnInfos(dataTimelineList.get(pickedDataTimeLineIndex).elementDescription, pickedDataTimeLineIndex);
			setSoundingInfo(dataTimelineSndLysListMap.get(dataTimelineList.get(pickedDataTimeLineIndex).elementDescription));			
			resetData();
		}
	}
	
	/*
	 * Note: each deletingTimeList's element (a String) is used to mapped to elementDescription (a String)
	 * of dataTimelineList's elements, which has same value as key (a String) of dataTimelineSndLysListMap
	 */
	public void deleteRsc(List<String> deletingTimeList){
		for(String tmLine: deletingTimeList){
			for(ElementStateProperty elm: dataTimelineList){
				if(tmLine.equals(elm.elementDescription)){
					dataTimelineList.remove(dataTimelineList.indexOf(elm));
					dataTimelineSndLysListMap.remove(tmLine);
					break;
				}
			}
		}
		//refresh stationIdList
		//Start by creating a new temporary map, using stn id (String) as key
		String tok1=null;
		StringTokenizer st1;
		Map<String, Integer> tempStnIdMap = new HashMap<String, Integer>();
		for(ElementStateProperty elm: dataTimelineList){
			st1  = new StringTokenizer(elm.elementDescription);
			tok1 = st1.nextToken();//first token should be stn name
			if(tok1!=null && tempStnIdMap.get(tok1)==null){
				tempStnIdMap.put(tok1, 1); // stn not in map, add it
			}
		}
		//refresh new stationIdList, by deleting element not found in map
		//looping from end of list, so wont crash system while deleting element
		for (int i = stationIdList.size()-1; i >= 0; i--){
			ElementStateProperty stn = stationIdList.get(i);
			if(tempStnIdMap.get(stn.elementDescription) == null)
				stationIdList.remove(i);
		}
		
		//reinitRsc();
		findAndSetPickedTimeLine();
		// anything more to do?
	}
	public void deleteRscAll(){
		NsharpMapResource nsharpMapResource = NsharpMapResource.getOrCreateNsharpMapResource();
		nsharpMapResource.setPoints(null);
		cleanUpRsc();
        
	}
	@SuppressWarnings("deprecation")
	public void addRsc(Map<String, List<NcSoundingLayer>> soundMap,  NsharpStationInfo stnInfo){
		//make sure not adding duplicated sounding data
		//System.out.println("NsharpSkewTResource addRsc called");
		Set<String> duplicateKeys = new HashSet<String>();
		for(String key: soundMap.keySet()) {
			if(dataTimelineSndLysListMap.containsKey(key)==true)
				duplicateKeys.add(key);
		}
		for(String key: duplicateKeys) {
			soundMap.remove(key);
		}
		//add new data 
		dataTimelineSndLysListMap.putAll(soundMap);
		Set<String> dataTimelineSet = soundMap.keySet();		
		String[] tempTimeLineArr = dataTimelineSet.toArray(new String[dataTimelineSet.size()]);
		
		//set current "picked" stn to become "grouped"
		if(pickedDataTimeLineIndex <dataTimelineList.size() ){
			dataTimelineList.get(pickedDataTimeLineIndex).elementColor = GROUPED_COLOR;
			dataTimelineList.get(pickedDataTimeLineIndex).elementState = State.GROUPED;
		}
		
		for (int i=0; i< tempTimeLineArr.length; i++){
			ElementStateProperty newElm = new ElementStateProperty();
			newElm.elementDescription =  tempTimeLineArr[i].toString();
			// set last newly added time line as new "PICKED" time line
			if(i == tempTimeLineArr.length-1){ 
				newElm.elementState = State.PICKED;
				newElm.elementColor = PICKED_COLOR;				
			}
			else {
				//set all other newly added time line as "grouped"
				newElm.elementState = State.GROUPED;
				newElm.elementColor = GROUPED_COLOR;
			}
			newElm.stnInfo = new NsharpStationInfo();
			newElm.stnInfo.setLatitude(stnInfo.getLatitude());
			newElm.stnInfo.setLongitude(stnInfo.getLongitude());
			newElm.stnInfo.setSndType(stnInfo.getSndType());
			dataTimelineList.add(newElm);
			
			//System.out.println(dataTimelineArray[i].strData);
			StringTokenizer st1 = new StringTokenizer(newElm.elementDescription);
			if(st1.hasMoreTokens()){
				addStnIdToList(st1.nextToken(), stnInfo);
				//Chin bug? setElementStatePropertyList(stationIdList,State.PICKED,PICKED_COLOR);
			}
			
		}
		Collections.sort(dataTimelineList, new ElementComparatorNDH());
		
		//Set default parcel trace data
		parcelList.clear();
		ParcelData defParcel = new ParcelData();
		defParcel.setParcelType(NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE );
		defParcel.setParcelLayerPressure(NsharpNativeConstants.MU_LAYER);
		parcelList.add(defParcel);
		
		//set pickedDataTimeLineIndex using newly picked stn index
		for (int i= 0; i< dataTimelineList.size() ; i++){
			if(dataTimelineList.get(i).elementState == State.PICKED){
				pickedDataTimeLineIndex = i;	
				preDataTimeLineIndex = pickedDataTimeLineIndex;
				break;
			}
		}
		String stnStr="";
		stnStr= dataTimelineList.get(pickedDataTimeLineIndex).elementDescription; 	
		setPickedStnInfos(stnStr,pickedDataTimeLineIndex);
		setSoundingInfo(dataTimelineSndLysListMap.get(stnStr));
		resetData();

		//set total time line page number
		int numTimeLinePerPage = (NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END)/NsharpConstants.CHAR_HEIGHT;
		totalTimeLinePage = dataTimelineList.size() /numTimeLinePerPage + 1;
		//System.out.println("totalTimeLinePage ="+totalTimeLinePage+ " hash:"+ this.hashCode());
		// bsteffen only set the editorNum on the renderable display if it is in an editor to avoid opening new editors
		NsharpSkewTDisplay renderableDisplay = (NsharpSkewTDisplay) descriptor.getRenderableDisplay();
		if(renderableDisplay != null) {
		    IDisplayPaneContainer editor = renderableDisplay.getContainer();
		    if(editor instanceof NsharpSkewTEditor) {
		       int editorNum = ((NsharpSkewTEditor) editor).getEditorNum();
		       renderableDisplay.setEditorNum(editorNum);
		    }
		}

		//set data time to descriptor
		//this is for stepping/looping	
		//
		//  modified this to allow the time matcher to control descriptor times if there is one
        if (descriptor.getTimeMatcher() == null || descriptor.getTimeMatcher().getTimeMatchBasis() == null) {
            DataTime[] dataTimes = new DataTime[dataTimelineList.size()];
            Date now = new Date();
            for(int k =0; k < dataTimelineList.size() ; k++){
                dataTimes[k]= new DataTime(now, k);
            }
            //bsteffen no need to get a descriptor from a renderableDispaly since we have a descriptor
            descriptor.setDataTimes(dataTimes);
        }
        
		NsharpShowTextDialog textarea =  NsharpShowTextDialog.getAccess();
		if(textarea != null){
			textarea.refreshTextData();
		}

		deepCopyDataMap(this.dataTimelineSndLysListMap,this.originalDataTimelineSndLysListMap);
		
		
	}

 	public String getPickedStnInfoStr() {
		return pickedStnInfoStr;
	}

 	private void setPickedStnInfos(String pickedStnInfoStr, int timeLineIndex) {
		//preStnInfoStr = this.pickedStnInfoStr;
		this.pickedStnInfoStr = pickedStnInfoStr;
		pickedStnInfo = dataTimelineList.get(timeLineIndex).getStnInfo();
		//System.out.println("setPickedStnInfo() set " + pickedStnInfoStr);
	}

	public void setUserPickedStationId(Coordinate c) {
		int index =((int)(c.y - NsharpConstants.STATION_ID_REC_Y_ORIG))/ NsharpConstants.CHAR_HEIGHT;
		if( index  < stationIdList.size() ){
			String picked = stationIdList.get(index).elementDescription;
			switch(stationIdList.get(index).elementState) {
			case GROUPED:
				stationIdList.get(index).elementState = State.IDLE;
				stationIdList.get(index).elementColor = IDLE_COLOR;
				//set all time lines with same station to enabled
				setStnDataTimelineStatus(picked, State.IDLE, IDLE_COLOR);
				break;
			case IDLE:
				stationIdList.get(index).elementState = State.GROUPED;
				stationIdList.get(index).elementColor = GROUPED_COLOR;
				//set all time lines with same station to enabled
				setStnDataTimelineStatus(picked, State.GROUPED, GROUPED_COLOR);
				break;
			/* was...
			 case GROUPED:
				stationIdList.get(index).elementState = State.DISABLED;
				stationIdList.get(index).elementColor = DISABLED_COLOR;
				//set all time lines with same station to disabled
				setStnDataTimelineStatus(picked, State.DISABLED, DISABLED_COLOR);
				break;
			case DISABLED:
				stationIdList.get(index).elementState = State.IDLE;
				stationIdList.get(index).elementColor = IDLE_COLOR;
				//set all time lines with same station to enabled
				setStnDataTimelineStatus(picked, State.IDLE, IDLE_COLOR);
				break;
			case IDLE:
				stationIdList.get(index).elementState = State.GROUPED;
				stationIdList.get(index).elementColor = GROUPED_COLOR;
				//set all time lines with same station to enabled
				setStnDataTimelineStatus(picked, State.GROUPED, GROUPED_COLOR);
				break;
			 */
			default:
				break;
			}
			
			//reset picked stn info if necessary
			findAndSetPickedTimeLine();
			if(compareIsOn){
				createRscHodoWindShapeAll();
				createRscPressTempCurveShapeAll();
			}
		}
	}
	
	public void setUserPickedDataTimeLine(Coordinate c) {
		int numTimeLinePerPage = (NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END)/NsharpConstants.CHAR_HEIGHT;
		//first to find if it is for change to next page, or change sorting
		//System.out.println("numTimeLinePerPage="+numTimeLinePerPage+"gap="+(NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END));
		int index =((int)(c.y - NsharpConstants.DATA_TIMELINE_REC_Y_ORIG))/ NsharpConstants.CHAR_HEIGHT;
		if(index == 0 ){
			if( c.x  < NsharpConstants.DATA_TIMELINE_SORT_X_START ){
				if( totalTimeLinePage == 1) //
					return;

				curTimeLinePage++;
				if(curTimeLinePage>totalTimeLinePage)
					curTimeLinePage=1;
			}
			else {
				//resorting time lines
				if(sortByStn){
					sortByStn=false;
					//System.out.println("re-sort by time first");
					//resort data time line list by day, hour, station name
					Collections.sort(dataTimelineList, new ElementComparatorDHN());
				}
				else {
					sortByStn=true;
					//System.out.println("re-sort by stn first");
					//resort data time line list by station name, day, hour
					Collections.sort(dataTimelineList, new ElementComparatorNDH());
				}
			}
			
			return;
		}
		// recalculate index for time line
		index =((int)(c.y - NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END))/ NsharpConstants.CHAR_HEIGHT +
		 			(curTimeLinePage-1)* numTimeLinePerPage ;	
		
		if( index  < this.dataTimelineList.size() ){
			String picked = dataTimelineList.get(index).elementDescription;
			
			switch(dataTimelineList.get(index).elementState){
			case PICKED:
				return;
			case IDLE:
				dataTimelineList.get(index).elementState = State.GROUPED;
				dataTimelineList.get(index).elementColor = GROUPED_COLOR;
				//if it is only grouped element, it should be picked element by default
				boolean pickedFound=false;
				for(int i=0; i < dataTimelineList.size(); i++){
					if(dataTimelineList.get(i).elementState == State.PICKED){
						pickedFound=true;
						break;
					}
				}
				if(pickedFound==false){
					//"Index" element is the only one data time line picked
					dataTimelineList.get(index).elementState = State.PICKED;
					dataTimelineList.get(index).elementColor = PICKED_COLOR;
					setPickedStnInfos(picked, index);
					setSoundingInfo(dataTimelineSndLysListMap.get(picked));
					pickedDataTimeLineIndex = index ;
					resetData();
				}
				break;
			case GROUPED:
				dataTimelineList.get(index).elementState = State.IDLE;
				dataTimelineList.get(index).elementColor = IDLE_COLOR;
				break;
			}
		}
		if(compareIsOn){
			createRscHodoWindShapeAll();
			createRscPressTempCurveShapeAll();
		}
	}
	
	
	private void changeDataTimeLine(){
		//System.out.println("changeDataTimeLine called");
		String picked = this.dataTimelineList.get(pickedDataTimeLineIndex).elementDescription; 
		
		//Check to see if a new time line is picked by user
		if(!picked.contentEquals(pickedStnInfoStr)){
			//This is a new data time line picked
			
			refreshPickedTimeLineState();
			setPickedStnInfos(picked, pickedDataTimeLineIndex);
			
			setSoundingInfo(dataTimelineSndLysListMap.get(picked));
			resetData();
			//System.out.println("changeDataTimeLine reseting data to "+ pickedStnInfoStr);
		}
				// reset current page number if necessary
		int numTimeLinePerPage = (NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END)/NsharpConstants.CHAR_HEIGHT;
		curTimeLinePage = pickedDataTimeLineIndex/numTimeLinePerPage + 1;
		
		NsharpShowTextDialog textarea =  NsharpShowTextDialog.getAccess();
		if(textarea != null){
			// chin TBD textarea.refreshTextData();
			textarea.updateTextFromWorkerThread();
		}
		
	}
	
	private void moveTimeLineIndexBackward(){
		preDataTimeLineIndex = pickedDataTimeLineIndex;
		int counter=0;
		while(true){
			pickedDataTimeLineIndex++;
			pickedDataTimeLineIndex = pickedDataTimeLineIndex % this.dataTimelineList.size();
			counter++;
			if(counter > dataTimelineList.size())
				break;
			if(// was... dataTimelineList.get(pickedDataTimeLineIndex).elementState != State.DISABLED && 
					dataTimelineList.get(pickedDataTimeLineIndex).elementState != State.IDLE)
				break;
			
		}
	}
	private void moveTimeLineIndexForward (){
		preDataTimeLineIndex = pickedDataTimeLineIndex;
		int counter=0;
		while(true){
			pickedDataTimeLineIndex= pickedDataTimeLineIndex + this.dataTimelineList.size();// so, we wont get a negative number
			pickedDataTimeLineIndex--;
			pickedDataTimeLineIndex = pickedDataTimeLineIndex % this.dataTimelineList.size();
			counter++;
			if(counter > dataTimelineList.size())
				break;
			if(//was... dataTimelineList.get(pickedDataTimeLineIndex).elementState != State.DISABLED && 
					dataTimelineList.get(pickedDataTimeLineIndex).elementState != State.IDLE)
				break;
		}
		//System.out.println("timeline="+dataTimelineList.get(pickedDataTimeLineIndex).getElementDescription());
	}
	private void moveTimeLineIndexCycle (){
		preDataTimeLineIndex = pickedDataTimeLineIndex;
		//Note: direction should only be NEXT or PREVIOUS
		int counter=0;
		while(true){
			counter++;
			if(counter > dataTimelineList.size()){
				pickedDataTimeLineIndex = preDataTimeLineIndex;
				break;
			}
			if(currentOpDirection == FrameChangeOperation.NEXT ){
				pickedDataTimeLineIndex--;
				if(pickedDataTimeLineIndex == 0){
					//the end of forward direction, change direction to backward
					currentOpDirection = FrameChangeOperation.PREVIOUS;
				}

			}
			else{ // direction is FrameChangeOperation.PREVIOUS
				pickedDataTimeLineIndex++;
				if(pickedDataTimeLineIndex == dataTimelineList.size()-1){
					//the end of backward direction, change direction to forward
					currentOpDirection = FrameChangeOperation.NEXT;
				}
			}
			if(dataTimelineList.get(pickedDataTimeLineIndex).elementState != State.IDLE)
				break;
		}

	}
	public void setLoopingDataTimeLine(LoopProperties loopProperties) {
		//System.out.println("setLoopingDataTimeLine loopmode ="+loopProperties.getMode().toString());
		if( this.dataTimelineList.size()>0) { 
			switch(loopProperties.getMode()){
			case  Forward:
				moveTimeLineIndexForward();
				break;
			case  Backward:
				moveTimeLineIndexBackward();
				break;
			case  Cycle:
				moveTimeLineIndexCycle();
				
				break;
			}
			changeDataTimeLine();
			
		}
				
	}
	public enum LoopMode {
        Forward, Backward, Cycle
    };
 	public void setSteppingDataTimeLine(FrameChangeOperation operation, FrameChangeMode mode) {
		if( this.dataTimelineList.size() > 0) { 
			preDataTimeLineIndex = pickedDataTimeLineIndex;

			//preset index for LAST and FIRST operation
			switch(operation){
			case LAST: //the future-est time, at top of time line shown. set to -1, so in while loop, it starts from 0
				pickedDataTimeLineIndex=-1;//
				break;
			case FIRST: //the oldest time, set to dataTimelineArray.length, so in while loop, it starts from dataTimelineArray.length-1
				pickedDataTimeLineIndex = dataTimelineList.size(); 
				break;
			}
				
			int counter=0;
			while(true){
				switch(operation){
				case LAST: //the future-est time, at top of time line shown
					pickedDataTimeLineIndex++;
					break;
				case FIRST: //the oldest time
					pickedDataTimeLineIndex--; 
					break;
				case PREVIOUS:
					pickedDataTimeLineIndex++;
					pickedDataTimeLineIndex = pickedDataTimeLineIndex % this.dataTimelineList.size(); 
					break;
				case NEXT:
					// so, we wont get a negative number
					pickedDataTimeLineIndex= pickedDataTimeLineIndex + this.dataTimelineList.size();
					pickedDataTimeLineIndex--;
					pickedDataTimeLineIndex = pickedDataTimeLineIndex % this.dataTimelineList.size();	
					break;
				}
				counter++;
				//System.out.println("counter = "+ counter);
				if(counter > dataTimelineList.size())
					return; // looped through whole list already, and index back to original
				if(// was.. dataTimelineList.get(pickedDataTimeLineIndex).elementState != State.DISABLED && 
						dataTimelineList.get(pickedDataTimeLineIndex).elementState != State.IDLE)
					break;//out of while loop
				
			}
			changeDataTimeLine();

		}
		
	}
	
	//used for sorting 
	public class tempPoint  implements Comparable<tempPoint>{
		double diff;
		double temp;
		double pressure;
		int type; //1= temp, 2 = dewpoint
		tempPoint(double diff,double temp,double pressure, int type){
			this.diff = diff;
			this.temp = temp;
			this.pressure = pressure;
			this.type = type;
		}

		@Override
		public int compareTo(tempPoint o) {
			if(this.diff >= o.diff)
				return 1;
			else
				return 0;
		}
	}
	/*
	 * Return the closest point to the input point on Hodo graph
	 */
	public Coordinate getClosestHodoPoint(Coordinate inputC){
		//System.out.println("picked pt  CX "+ inputC.x + " CY "+ inputC.y);
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		Coordinate closeptC = new Coordinate();
		double curSmallestDist=10000; // picked a impossible big number to start with
		double distance;
		boolean ptFound = false;
		NcSoundingLayer layer;
		/*
		 * Note: soundingLys list sorted with highest pressure as first element
		 */		
		for (int i=0; i< this.soundingLys.size(); i++) {
			layer = this.soundingLys.get(i);
			double curS, curD;
			curS = layer.getWindSpeed();
			curD = layer.getWindDirection();
			closeptC = WxMath.uvComp((float)curS, (float)curD);
			closeptC = bkRsc.getHodoBackground().getWorld().map(closeptC);
			//System.out.println("closeptCx " + closeptC.x+ " closeptCy "+ closeptC.y);
			distance = inputC.distance(closeptC);
			//System.out.println("closeptCx " + closeptC.x+ " closeptCy "+ closeptC.y+" distance "+ distance + " curSmallestDist " +curSmallestDist);
			if(distance < curSmallestDist){
				curSmallestDist = distance;
				hodoEditingSoundingLayerIndex = i;
				ptFound = true;
			}
		}
		if(ptFound == false){
			closeptC.x=closeptC.y=0;
		} else{
			layer = this.soundingLys.get(hodoEditingSoundingLayerIndex);
			closeptC = WxMath.uvComp((float)layer.getWindSpeed(), (float)layer.getWindDirection());
			closeptC = bkRsc.getHodoBackground().getWorld().map(closeptC);
		}
		
		//System.out.println("picked closeptCx " + closeptC.x+ " closeptCy "+ closeptC.y);
		return closeptC;
	}

	/*
	 * Return the closest point to the input point on either Temp or Dewpoint trace line
	 * Also set currentSoundingLayerIndex for plotting later
	 */
	public Coordinate getPickedTempPoint(Coordinate c){
		
		//System.out.println("picked pt X "+ x + " Y "+ y);
		//System.out.println("picked pt CX "+ c.x + " CY "+ c.y);
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		
		WGraphics WGc = bkRsc.getSkewTBackground().getWorld();
		Coordinate inC = WxMath.reverseSkewTXY(WGc.unMap(c));
		double inPressure = inC.y;
		double inTemp = inC.x;
		//System.out.println("user inout pt pressure "+ inPressure+ " temp "+inTemp  );
		double prevPressure=1000;
		double prevT=0, prevD=0;
		Coordinate closeptC = new Coordinate(1,1,0);
		boolean firstPrevPicked= false;
		
		/*
		 * Note: soundingLys list sorted with highest pressure as first element
		 */		
		for (NcSoundingLayer layer : this.soundingLys) {
			double t, d;
			t = layer.getTemperature();
			d = layer.getDewpoint();
			double pressure = layer.getPressure();
			if(firstPrevPicked==false){
				// this is to handle the case, if picked point has same pressure (largest pressure) as first layer 
				firstPrevPicked = true;
				prevPressure = pressure;
				prevT = t;
				prevD = d;
			}
			//System.out.println(" pressure "+ pressure );
			if(  pressure >= 100 && pressure <=  inPressure){
				// decide which pressure (layer) should be used. current one or previous one
				double disCurrentP =  Math.abs(pressure -inPressure );
				double disPreviousP =  Math.abs(prevPressure -inPressure );
				double pickedPressure, pickedTemp, pickedDewpoint;
				if(disPreviousP <= disCurrentP){
					pickedPressure = prevPressure;
					pickedTemp = prevT;
					pickedDewpoint = prevD;
					if(this.soundingLys.indexOf(layer) == 0)
						currentSoundingLayerIndex = this.soundingLys.indexOf(layer);
					else
						currentSoundingLayerIndex = this.soundingLys.indexOf(layer)-1;
				}
				else {
					pickedPressure = pressure;
					pickedTemp = t;
					pickedDewpoint = d;
					currentSoundingLayerIndex = this.soundingLys.indexOf(layer);
				}
				//decide which line, temp or dewpoint, closer to user picked point
				double disTemp = Math.abs(pickedTemp- inTemp);
				double disDew = Math.abs(pickedDewpoint- inTemp);
				if(disTemp <= disDew){
					closeptC =  WxMath.getSkewTXY(pickedPressure, pickedTemp);
					closeptC = WGc.map(closeptC);
					currentTempCurveType = TEMP_TYPE;	
					//System.out.println("picked pressure "+ pickedPressure + " temp " +pickedTemp);
				}
				else {
					closeptC =  WxMath.getSkewTXY(pickedPressure, pickedDewpoint);
					closeptC = WGc.map(closeptC);
					currentTempCurveType = DEWPOINT_TYPE;
					//System.out.println("picked pressure "+ pickedPressure + " dewpoint " +pickedDewpoint);
				}
				
				//System.out.println("currentSoundingLayerIndex P = "+ this.soundingLys.get(currentSoundingLayerIndex).getPressure());
				break;
			}			
			prevPressure = pressure;
			prevT = t;
			prevD = d;
		}
		
		return closeptC;
	}
	private void plotNsharpInteractiveTemp(IGraphicsTarget target, double zoomLevel,
            WGraphics world, RGB color) throws VizException {
		if(soundingLys.size() < 4)
			return;
		//double inPressure = soundingLys.get(currentSoundingLayerIndex).getPressure();
		double aboveLayerPressure, belowLayerPressure;
		double aboveLayerT=0, aboveLayerD=0, belowLayerT=0, belowLayerD=0;
		int aboveLayerIndex, belowLayerIndex;
		double plotAboveT, plotBelowT;
		if(currentSoundingLayerIndex ==0){
			aboveLayerIndex = currentSoundingLayerIndex;
			belowLayerIndex = currentSoundingLayerIndex+1;
		}
		else if(currentSoundingLayerIndex == soundingLys.size()-1 ){
			belowLayerIndex = currentSoundingLayerIndex;
			aboveLayerIndex = currentSoundingLayerIndex-1;
		}
		else{
			belowLayerIndex = currentSoundingLayerIndex + 1;
			aboveLayerIndex = currentSoundingLayerIndex - 1;
		}
		aboveLayerPressure = soundingLys.get(aboveLayerIndex).getPressure();
		aboveLayerT = soundingLys.get(aboveLayerIndex).getTemperature();
		aboveLayerD = soundingLys.get(aboveLayerIndex).getDewpoint();
		belowLayerPressure = soundingLys.get(belowLayerIndex).getPressure();
		belowLayerT = soundingLys.get(belowLayerIndex).getTemperature();
		belowLayerD = soundingLys.get(belowLayerIndex).getDewpoint();
		
		if(currentTempCurveType == TEMP_TYPE){
			plotAboveT = aboveLayerT;
			plotBelowT = belowLayerT;

		}
		else{
			plotAboveT = aboveLayerD;
			plotBelowT = belowLayerD;

		}
		Coordinate c1 = WxMath.getSkewTXY(aboveLayerPressure, plotAboveT);                
		c1.x = world.mapX(c1.x);
		c1.y = world.mapY(c1.y);
		target.drawLine(c1.x, c1.y, 0.0, interactiveTempPointCoordinate.x, interactiveTempPointCoordinate.y, 0.0, color,
				commonLinewidth, LineStyle.DASHED);
		c1 = WxMath.getSkewTXY(belowLayerPressure,plotBelowT);            
		c1.x = world.mapX(c1.x);
		c1.y = world.mapY(c1.y);
		target.drawLine(c1.x, c1.y, 0.0, interactiveTempPointCoordinate.x, interactiveTempPointCoordinate.y, 0.0, color,
				commonLinewidth, LineStyle.DASHED);
		//System.out.println("In pressure="+ inPressure+ " above P="+aboveLayerPressure+ " below P="+belowLayerPressure);
	}
	public List<NcSoundingLayer> getSoundingLys() {
		return soundingLys;
	}

	/*
	 * This interpolation is to smooth data out with a pressure gap of 25 mb per layer and also keep
	 *  original lowest and highest layers.
	 */
	private List<NcSoundingLayer> performInterpolation(List<NcSoundingLayer> rawSndLysLst){
		NcSoundingLayer newLayer= new NcSoundingLayer();
		//System.out.println(" performInterpolation");
		//populate snd data to nsharp native code lib for later calculating
		//NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		
		
		nsharpNative.populateSndgData(rawSndLysLst);		
		//get storm motion wind data after populate sounding from NsharpLib
		//FloatByReference stwdir= new FloatByReference(-999);
		//FloatByReference stwspd= new FloatByReference(-999);
	//  #10438 nsharpNative.nsharpLib.get_storm(stwspd, stwdir);
		//bkRsc.setSmDir(stwdir.getValue());
		//bkRsc.setSmSpd(stwspd.getValue());

		List<NcSoundingLayer> mySndLst = new ArrayList<NcSoundingLayer>();
		// add top layer
		try {
			//here a shallowCopy is enough
			newLayer = (NcSoundingLayer) rawSndLysLst.get(0).clone();
			mySndLst.add(newLayer);
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		} 
		//Chin TEST
		//mySndLst = NsharpDataHandling.organizeSoundingDataForShow(mySndLst, 0);
		// The first layer has highest pressure
		// get a pressure value below first layer and can be divided by 25 exactly
		int p =(int) (rawSndLysLst.get(0).getPressure()/25) * 25;
		float interpolatedValue;
		if ( p == (int)rawSndLysLst.get(0).getPressure())
		{   // if p is same as first layer, then this layer is added already, start from 2nd layer
			p = p-25; 
		}
		for (; p >= 50 ; p = p-25){
			newLayer= new NcSoundingLayer();
			newLayer.setPressure(p);
			interpolatedValue = nsharpNative.nsharpLib.itemp(p);
			if(interpolatedValue ==NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA)
				//this is not good layer data, usually happened when lowest layer pressure is 
				// more than 50, then when interpolate layer for pressure 50, will return unvalid value
				continue; 
			newLayer.setTemperature(interpolatedValue);
			interpolatedValue = nsharpNative.nsharpLib.idwpt(p);
			newLayer.setDewpoint(interpolatedValue);
			interpolatedValue = nsharpNative.nsharpLib.iwdir(p);
			newLayer.setWindDirection(interpolatedValue);
			interpolatedValue = nsharpNative.nsharpLib.iwspd(p);
			newLayer.setWindSpeed(interpolatedValue);
			interpolatedValue = nsharpNative.nsharpLib.ihght(p);
			newLayer.setGeoHeight(interpolatedValue);
			interpolatedValue = nsharpNative.nsharpLib.iomeg(p);
			newLayer.setOmega(interpolatedValue);
			
			mySndLst.add(newLayer);
		}
		

		return mySndLst;
	}

	
	private void setSoundingInfo(List<NcSoundingLayer> sndLys) {
		if(overlayIsOn){ 
			
			previousSoundingLys = soundingLys;
			
			//System.out.println("setSoundingInfo preDataTimeLineIndex="+preDataTimeLineIndex+" pickedDataTimeLineIndex"+pickedDataTimeLineIndex);
			//System.out.println("setSoundingInfo preStnInfoStr="+preStnInfoStr+" pickedStnInfoStr"+pickedStnInfoStr);
			
		}
		else {
			previousSoundingLys = null;
		}
		
		if(interpolateIsOn == true){ 
			//we dont want to change original raw data, so use a new copy
			NcSoundingLayer newLayer;
			List<NcSoundingLayer> mySndLst = new ArrayList<NcSoundingLayer>();
			for(NcSoundingLayer layer: dataTimelineSndLysListMap.get(pickedStnInfoStr)){
				newLayer = new NcSoundingLayer();
				try {
					newLayer = (NcSoundingLayer) layer.clone();
					
				} catch (CloneNotSupportedException e) {
					e.printStackTrace();
				} //here a shallowCopy is enough
				mySndLst.add(newLayer);
			}
			// interpolation
			mySndLst = performInterpolation(mySndLst);
			
			soundingLys = mySndLst;
		}
		else {
			soundingLys = sndLys;
			
		}
	}
	/**
     * @param resourceData
     * @param properties
     */
    public NsharpSkewTResource(AbstractResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        this.dataTimes = new ArrayList<DataTime>();
        this.soundingMap = new HashMap<Date, SoundingParams>();
        elementColorMap.put(new Integer(PICKED_COLOR),NsharpConstants.color_green); //green
        elementColorMap.put(new Integer(GROUPED_COLOR),NsharpConstants.color_cyan);//cyan
        elementColorMap.put(new Integer(IDLE_COLOR),NsharpConstants.color_white);//white
        //elementColorMap.put(new Integer(DISABLED_COLOR),NsharpConstants.color_red);//red
        dataTimelineList = new ArrayList<ElementStateProperty>();
		stationIdList = new ArrayList<ElementStateProperty>();
		nsharpNative = new NsharpNative();
		//System.out.println("NsharpSkewTResource constructed");
		//based on BigNsharp storm slinky color used and gempak color definition
		stormSlinkyColorMap.put(new Integer(3),NsharpConstants.color_green); //green
		stormSlinkyColorMap.put(new Integer(7),NsharpConstants.color_magenta);
		stormSlinkyColorMap.put(new Integer(6),NsharpConstants.color_cyan);
		stormSlinkyColorMap.put(new Integer(13),NsharpConstants.color_violet_md);
		stormSlinkyColorMap.put(new Integer(20),NsharpConstants.color_yellow_DK);
		stormSlinkyColorMap.put(new Integer(27),NsharpConstants.color_cyan_md);
		
    }
    public void cleanUpRsc(){
    	if(dataTimelineSndLysListMap!= null)
    		dataTimelineSndLysListMap.clear();
    	if(originalDataTimelineSndLysListMap!=null)
    		originalDataTimelineSndLysListMap.clear();
    	if(soundingLysList!=null)
    		soundingLysList.clear();
    	if(soundingLys!=null)
    		soundingLys.clear();
    	if(previousSoundingLys!=null)
    		previousSoundingLys.clear();
    	if(dataTimelineList!= null)
    		dataTimelineList.clear();
    	if(stationIdList!= null)
    		stationIdList.clear();
    	if(intpSndLst!= null)
    		intpSndLst.clear();
    }
	@Override
	protected void disposeInternal() {
		//System.out.println("NsharpSkewTResource disposeInternal called");
		if(NsharpMapMouseHandler.getAccess() != null) 
			NsharpMapMouseHandler.getAccess().setSkewRsc(null);
		
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
	    soundingMap= null;
	    parcelList= null;
	    listenerList=null;
	    soundingLysList=null;
	    soundingLys = null;
		previousSoundingLys = null;
		dataTimelineSndLysListMap = null;
		originalDataTimelineSndLysListMap= null;
		dataTimelineList= null;
		intpSndLst = null;
		stationIdList= null;
		stormSlinkyColorMap = null;
		elementColorMap= null;
		disposeAllWireFrameShapes();
		windBoxWindRscShapeList=null;
		pressureTempRscShapeList=null;
		hodoWindRscShapeList=null;
		parcelTraceRscShapeList = null;
		if(NsharpParcelDialog.getAccess()!= null){
			NsharpParcelDialog.getAccess().reset();
		}
		this.target.dispose();
		nsharpNative = null;
		drawPanel = null;
	}

	@Override
	protected synchronized void initInternal(IGraphicsTarget target) throws VizException {
		//System.out.println("NsharpSkewTResource initInternal called");
		this.font9 = target.initializeFont("Monospace", 9, null);
		this.font10 = target.initializeFont("Monospace", 10, null);
		this.font11 = target.initializeFont("Monospace", 11, null);
		IFont.Style[] style = {IFont.Style.BOLD};
		this.font12 = target.initializeFont("Monospace", 12, style);
		commonLinewidth = getCapability(OutlineCapability.class).getOutlineWidth();
        commonLineStyle = getCapability(OutlineCapability.class)
                .getLineStyle();
        this.target = target;
        drawPanel = new NsharpDrawPanels(descriptor.getSkewTBkGResource(), this.font10);
		 
		
        //System.out.println("initInternal target=" + target.toString());
        createAllWireFrameShapes();		
	}
	private void adjustFontSize(int canvasW, int canvasH ) {
		float font9Size,font10Size,font11Size,font12Size;
		
		float fontAdjusted=0;
		float fontBaseH=70f; //Chin:  why 70 & 100? After many "try and error" experiments...
		float fontBaseW=100f;
		if(canvasH < NsharpConstants.DEFAULT_CANVAS_HEIGHT && canvasW< NsharpConstants.DEFAULT_CANVAS_WIDTH){
			//both width and height are smaller than default
			float wAdjust = (float)(NsharpConstants.DEFAULT_CANVAS_WIDTH-canvasW)/fontBaseW;
			float hAdjust = (float)(NsharpConstants.DEFAULT_CANVAS_HEIGHT-canvasH)/fontBaseH;
			fontAdjusted = Math.max(wAdjust,hAdjust);
		}
		else if(canvasW< NsharpConstants.DEFAULT_CANVAS_WIDTH){
			// only width smaller than default
			fontAdjusted = (float)(NsharpConstants.DEFAULT_CANVAS_WIDTH-canvasW)/fontBaseW;
		}
		else if(canvasH < NsharpConstants.DEFAULT_CANVAS_HEIGHT ){
			// only height smaller than default
			fontAdjusted = (float)(NsharpConstants.DEFAULT_CANVAS_HEIGHT-canvasH)/fontBaseH;
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
		//System.out.println("font10 size ="+currentFont10Size);	
	}
	private void magnifyFont(double zoomLevel) {
		float magFactor = 1.0f / (float)zoomLevel;
		
		font10.setMagnification(magFactor);
		font9.setMagnification(magFactor);
		font11.setMagnification(magFactor);
		font12.setMagnification(magFactor);
	}
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
    	//System.out.println("paintInternal called");
    	//Chin: place holder for build option using AWIPS product system configuration file, config.ini
    	//To use this, add nsharp.build = xxxx at config.ini
    	//if ("xxxx".equals(System.getProperty("nsharp.build"))) {
    		
    	//}
    	//
    	//Chin: place holder for build option using NSHARP plugin's build.properties file to set nsharp.build
    	//run time variable.
    	//To use this, add nsharp.build = xxxx at build.properties
    	//if ("standard".equals(Activator.getDefault().getMyProperties().getString("nsharp.build"))){
    	//	System.out.println("nsharp.build ="+Activator.getDefault().getMyProperties().getString("nsharp.build"));
    	//}
    	NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
    	double zoomLevel = paintProps.getZoomLevel();
    	
    	 if( currentCanvasBoundWidth!= paintProps.getCanvasBounds().width || currentCanvasBoundHeight!=paintProps.getCanvasBounds().height){
    		
    		currentCanvasBoundWidth= paintProps.getCanvasBounds().width;
    		currentCanvasBoundHeight=paintProps.getCanvasBounds().height;
    		adjustFontSize(currentCanvasBoundWidth,currentCanvasBoundHeight);
    		drawPanel.setMyFont(font10);
    		bkRsc.getSkewTBackground().setCurrentFont(currentFont10Size);
    		bkRsc.getHodoBackground().setCurrentFont(currentFont10Size);
    	}
    	if(zoomLevel != currentZoomLevel)
    	{
    		currentZoomLevel = zoomLevel;
    		magnifyFont(zoomLevel);
    		bkRsc.getSkewTBackground().magnifyFont(zoomLevel);
    		bkRsc.getHodoBackground().magnifyFont(zoomLevel);
    	}
    	//System.out.println("canvas h="+paintProps.getCanvasBounds().height + " canvas w="+ paintProps.getCanvasBounds().width + " zoomlvl="+currentZoomLevel);
    	
    	
    	WGraphics plotWorld;

    	if((soundingLys != null) && (soundingLys.size()>= 4))
    	{

    		plotWorld = bkRsc.getSkewTBackground().getWorld();
    		NsharpGraphConfigDialog configD = NsharpGraphConfigDialog.getAccess();

    		PixelExtent extent = new PixelExtent(bkRsc.getSkewTBackground()
    				.getRectangle());
    		target.setupClippingPlane(extent);
    		//plot temp curve, when constructing pressureTempRscShapeList, it already considered 
    		// comparison, overlay, etc..so, just draw it.
			for(ShapeAndColor shapeNColor: pressureTempRscShapeList){
				target.drawWireframeShape(shapeNColor.shape, shapeNColor.color, commonLinewidth*2,commonLineStyle,font10);
    		}
    		if(configD != null ){        		        		
    			if(configD.isTemp() == true && !compareIsOn){
    				if(editGraphOn)
    					plotPressureTempEditPoints(target, plotWorld, NsharpConstants.color_red, TEMP_TYPE, this.soundingLys);
    			}
    			// dew point curve
    			if(configD.isDewp() == true && !compareIsOn){
   				if(editGraphOn)
    					plotPressureTempEditPoints(target, plotWorld, NsharpConstants.color_green, DEWPOINT_TYPE, this.soundingLys);
    			}
    			//plot wetbulb trace
    			if(configD.isWetBulb() == true && !compareIsOn)
    				target.drawWireframeShape(wetBulbTraceRscShape, NsharpConstants.color_cyan, commonLinewidth,commonLineStyle,font10);
    			//plot virtual temp trace
    			if(configD.isVTemp() == true && !compareIsOn)			
    				target.drawWireframeShape(vtempTraceCurveRscShape, NsharpConstants.color_red, commonLinewidth*2, LineStyle.DASHED,font10);
    			// parcel trace curve
    			if(configD.isParcel() == true && !compareIsOn){
    				if(soundingLys.size() > 0){
    					for (IWireframeShape shape: parcelTraceRscShapeList){
    						target.drawWireframeShape(shape, NsharpConstants.color_white, commonLinewidth,
    			            		LineStyle.DASHED,font10);
    					}
    				}
    			}
    			if(configD.isOmega() == true){
    				if(NsharpLoadDialog.getAccess()!= null && 
    						(NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.MODEL_SND ||
    								NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.PFC_SND ))
    					//plot omega
    					target.clearClippingPlane();
    					target.drawWireframeShape(omegaBkgShape, NsharpConstants.color_violet_red, commonLinewidth,
    							LineStyle.DASHED,font10);
    					target.drawWireframeShape(omegaRscShape, NsharpConstants.color_cyan, commonLinewidth,
    			                commonLineStyle,font10);
    			}
    		}
    		else{
    			//by default, draw everything
    			if(!compareIsOn){
        			if(editGraphOn)
        					plotPressureTempEditPoints(target, plotWorld, NsharpConstants.color_red, TEMP_TYPE, this.soundingLys);
      				// dew point curve
    				if(editGraphOn)
    					plotPressureTempEditPoints(target, plotWorld, NsharpConstants.color_green, DEWPOINT_TYPE, this.soundingLys);
    				//plot wetbulb trace
    				target.drawWireframeShape(wetBulbTraceRscShape, NsharpConstants.color_cyan, commonLinewidth,LineStyle.DEFAULT,font10);
    				//plot virtual temp trace
    				target.drawWireframeShape(vtempTraceCurveRscShape, NsharpConstants.color_red, commonLinewidth*2, LineStyle.DASHED,font10);
    				// parcel trace curve
    				if(soundingLys.size() > 0){
    					for (IWireframeShape shape: parcelTraceRscShapeList){
    						target.drawWireframeShape(shape, NsharpConstants.color_white, commonLinewidth,
    			            		LineStyle.DASHED,font10);
    					}
    				}
    			}
    			if(NsharpLoadDialog.getAccess()!= null && 
    					(NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.MODEL_SND ||
    							NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.PFC_SND ))
    				//plot omega
    				target.clearClippingPlane();
    				target.drawWireframeShape(omegaBkgShape, NsharpConstants.color_violet_red, commonLinewidth,
							LineStyle.DASHED,font10);
					target.drawWireframeShape(omegaRscShape, NsharpConstants.color_cyan, commonLinewidth,
			                commonLineStyle,font10);
    		}
    		if(plotInteractiveTemp == true ){
    			plotNsharpInteractiveTemp( target,  zoomLevel,
    					plotWorld,  NsharpConstants.color_white);
    		}
    		target.clearClippingPlane();


    		// Wind Barb
    		if(!compareIsOn && !overlayIsOn)
    			drawNsharpWindBarb(target, zoomLevel, plotWorld, NsharpConstants.color_yellow);

    		//wind box background and wind
    		target.drawWireframeShape(windBoxBkgShape, NsharpConstants.color_white,
                    0.5F, LineStyle.DOTS, font10);
    		for(ShapeAndColor shapeNColor: windBoxWindRscShapeList){
				target.drawWireframeShape(shapeNColor.shape, shapeNColor.color, commonLinewidth,commonLineStyle,font10);
    		}
    		
    		//plot vertical wind profile (advection layer)
    		extent = new PixelExtent(bkRsc.getVerticalWindBackground()
    				.getRectangle());
    		target.setupClippingPlane(extent);
    		float x1 = NsharpConstants.VERTICAL_WIND_X_ORIG+ (NsharpConstants.VERTICAL_WIND_WIDTH/2);
    		target.drawLine(x1, NsharpConstants.VERTICAL_WIND_Y_ORIG, 0, x1, NsharpConstants.VERTICAL_WIND_Y_ORIG+NsharpConstants.VERTICAL_WIND_HEIGHT, 0,
    				NsharpConstants.color_white, 1, LineStyle.DASHED);
    		target.drawWireframeShape(verticalWindLabelShape, NsharpConstants.color_white,
                    0.3F, commonLineStyle,font10);
    		target.drawWireframeShape(verticalWindRShape, NsharpConstants.color_red,
                    0.3F, commonLineStyle,font10);
    		target.drawWireframeShape(verticalWindSbShape, NsharpConstants.color_skyblue,
                    0.3F, commonLineStyle,font10);
    		target.clearClippingPlane();
    		//data time title
    		drawNsharpDataTimelineTitle(target);
    		drawNsharpStationIdTitle(target);
    		//drawHeightMark(target);
    		target.drawWireframeShape(heightMarkRscShape, NsharpConstants.color_red, 1, LineStyle.SOLID, font10);
    		
    		
    		if(!compareIsOn){
    			//draw EL, LFC, LCL, FZL, -20C, -30C lines
    			drawLclLine(target);
    			
    			//draw effective layer lines
    			drawEffectiveLayerLines(target);
    			
    			// draw cursor data
    			if(cursorInSkewT== true){
    				drawNsharpSkewtCursorData(target);
    			}
    		}

    		//draw dynamic temp, theta, height 
    		drawNsharpSkewtDynamicData(target, zoomLevel, plotWorld);

    		drawNsharpDataFilelabel(target, zoomLevel, plotWorld);
    		
    		//plot HODO
    		plotWorld = bkRsc.getHodoBackground().getWorld();
    		extent = new PixelExtent(bkRsc.getHodoBackground()
    				.getRectangle());
    		target.setupClippingPlane(extent);
    		if(((configD != null )&& configD.isHodo())|| (configD == null)){
    		for(ShapeAndColor shapeNColor: hodoWindRscShapeList){
				target.drawWireframeShape(shapeNColor.shape, shapeNColor.color, commonLinewidth*2,commonLineStyle,font10);
    		}
    		}
    		if(editGraphOn)
    			plotHodoEditPoints( target,plotWorld, NsharpConstants.color_white);
    		if(!compareIsOn){
    			plotNsharpHodoVectors(target, zoomLevel, plotWorld, null, false);
    		} 
    		target.clearClippingPlane();
    		drawHodoDynamicData(target, zoomLevel, plotWorld);
    		
    		//plot wind motion on top of HODO left upper corner   		
    		//TBD drawHodoWindMotionBox(target, bkRsc.getWindMotionBackground().getRectangle());

    		//plot inset graphs
    		//plot SRWinds- currently always on page 1 and page 2
    		extent = new PixelExtent(bkRsc.getSrWindsBackground().getRectangle());
            target.setupClippingPlane(extent);
    		target.drawWireframeShape(srWindBRscShape, NsharpConstants.color_cyan, commonLinewidth, LineStyle.SOLID,font10);
    		target.drawWireframeShape(srWindWRscShape, NsharpConstants.color_white, commonLinewidth, LineStyle.SOLID,font10);
    		target.drawWireframeShape(srWindRRscShape, NsharpConstants.color_red, commonLinewidth, LineStyle.SOLID,font10);
    		target.drawWireframeShape(srWindGRscShape, NsharpConstants.color_green, commonLinewidth, LineStyle.SOLID,font10);
    		target.drawWireframeShape(srWindMRscShape, NsharpConstants.color_magenta, commonLinewidth, LineStyle.SOLID,font10);
    		target.clearClippingPlane();
    		//plot storm slinky - currently always on page 1 and page 2
    		plotWorld =  bkRsc.getStormSlinkyBackground().getWorld();
    		plotNsharpStormSlinky(target, zoomLevel, plotWorld, bkRsc.getStormSlinkyBackground().getRectangle());

    		if(currentInsetPage == 1){
    			//plot ThetaE-Pressure 
    			extent = new PixelExtent(bkRsc.getThetaEPresureBackground().getRectangle());
    			target.setupClippingPlane(extent);
    			target.drawWireframeShape(thetaEPressureYRscShape, NsharpConstants.color_yellow, commonLinewidth, LineStyle.SOLID,font10);
    			target.drawWireframeShape(thetaEPressureWRscShape, NsharpConstants.color_white, commonLinewidth, LineStyle.SOLID,font10);
    			target.drawWireframeShape(thetaEPressureRRscShape, NsharpConstants.color_red, commonLinewidth, LineStyle.SOLID,font10);
    			target.clearClippingPlane();
    			//plot possibleWatchType
    			if(psblWatchTypeBkgShape==null){
    			WGraphics WGc = bkRsc.getPsblWatchTypeBackground().getWorld();
    			createBkgPsblWatchShape(WGc);
    			}
    			
    			extent = new PixelExtent(bkRsc.getPsblWatchTypeBackground().getRectangle());
    			target.setupClippingPlane(extent);
    			target.drawWireframeShape(psblWatchTypeBkgShape, wwTypeColor, commonLinewidth, LineStyle.SOLID,font10);
    			target.clearClippingPlane();

    		} 
    		else if(currentInsetPage == 2){
    			//plot ThetaE-Height 
    			extent = new PixelExtent(bkRsc.getThetaEHeightBackground().getRectangle());
    			target.setupClippingPlane(extent);
    			target.drawWireframeShape(thetaEHeightYRscShape, NsharpConstants.color_yellow, commonLinewidth, LineStyle.SOLID,font10);
    			target.drawWireframeShape(thetaEHeightWRscShape, NsharpConstants.color_white, commonLinewidth, LineStyle.SOLID,font10);
    			target.drawWireframeShape(thetaEHeightRRscShape, NsharpConstants.color_red, commonLinewidth, LineStyle.SOLID,font10);
    			target.clearClippingPlane();
    			//plot SR Wind Vectors
    			plotWorld = bkRsc.getSrWindVectorBackground().getWorld();
    			plotNsharpSRWindVectors(target, zoomLevel, plotWorld, bkRsc.getSrWindVectorBackground().getRectangle());
    		}
    		
    		//plot data time line
    		plotWorld = bkRsc.getDataTimelineBackground().getWorld();
    		drawNsharpDataTimelines(target, plotWorld, bkRsc.getDataTimelineBackground().getRectangle());

    		//plot station id
    		plotWorld = bkRsc.getStationIdBackground().getWorld();
    		drawNsharpStationId(target, plotWorld, bkRsc.getStationIdBackground().getRectangle());
    		
    		//plot color notations
    		plotWorld = bkRsc.getColorNotationsBackground().getWorld();
    		drawNsharpColorNotation(target, plotWorld, bkRsc.getColorNotationsBackground().getRectangle());
    		
    		
    		// write to panels
    		//Chin: Note: 
    		// Current display algorithm is: One page = 2 panels. show 1 page each with 2 panels at one time.
    		// i.e. show current page and its next page with 2 panels in total.
    		// currently, we have total of 10 "virtual panels" to display on 2 "physical
    		// display panels per design.
    		boolean virtualPanel1Visible= false;
    		for (int i = currentTextPage*2 -1,  physicalPanelNum = 1; i <  currentTextPage*2  + (NsharpConstants.dsiplayPanelSize-1); i++, physicalPanelNum++){
    			int virtuslPanelNum = i%10;
    			if (virtuslPanelNum==0) virtuslPanelNum=10;
    			drawPanel.drawPanel(target,virtuslPanelNum,physicalPanelNum);
    			//System.out.println("vPanel="+virtuslPanelNum+ " disPanel="+physicalPanelNum);
    			//Chin: parcel lines is designed in virtual panel number 1. And by current page shifting mechanism,
    			// It will only shown in either physical panel 1 or 3.
    			if(virtuslPanelNum ==1) {
    				parcelLinesInPhysicalPanelNumber = physicalPanelNum;
    				virtualPanel1Visible=true;
    			}
    		}
    		if(virtualPanel1Visible== false){
    			parcelLinesInPhysicalPanelNumber =0;
    		}
    		//drawPanel.createShape(target, 1, 1);
    	} 
    	else {
    		//draw side lines between skewt and hodo to make graph reasonable to view
    		target.drawLine(NsharpConstants.SKEWT_REC_X_ORIG + NsharpConstants.SKEWT_REC_WIDTH, NsharpConstants.SKEWT_REC_Y_ORIG, 0.0, 
    				NsharpConstants.HODO_REC_X_ORIG, NsharpConstants.SKEWT_REC_Y_ORIG,
    				0.0, NsharpConstants.pressureColor, 1);
    		target.drawLine(NsharpConstants.SKEWT_REC_X_ORIG + NsharpConstants.SKEWT_REC_WIDTH, NsharpConstants.SKEWT_REC_Y_ORIG+NsharpConstants.SKEWT_REC_HEIGHT, 0.0, 
    				NsharpConstants.HODO_REC_X_ORIG, NsharpConstants.SKEWT_REC_Y_ORIG+NsharpConstants.SKEWT_REC_HEIGHT,
    				0.0, NsharpConstants.pressureColor, 1);
    		//bottom line between hodo and timeline
    		target.drawLine(NsharpConstants.SRWINDS_REC_X_ORIG, NsharpConstants.SKEWT_REC_Y_ORIG+NsharpConstants.SKEWT_REC_HEIGHT, 0.0, 
    				NsharpConstants.DATA_TIMELINE_REC_X_ORIG, NsharpConstants.SKEWT_REC_Y_ORIG+NsharpConstants.SKEWT_REC_HEIGHT,
    				0.0, NsharpConstants.pressureColor, 1);

    	}

    }

    @SuppressWarnings("deprecation")
	private void drawHodoDynamicData(IGraphicsTarget target, double zoomLevel,
            WGraphics world) throws VizException {
    	// draw running temp, theta, height etc data at window palette bottom
    	NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
    	
    	double dispX =  world.getViewXmin();
    	double dispY =  world.getViewYmin()-20;
    	//display wind direction, speed in m/s and knots
    	
    	//Line 1 - wind direction, speed
		target.drawString(font9, bkRsc.getSWindDirection(), dispX+100, dispY, 0.0,
                TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
                VerticalAlignment.BOTTOM, null);
		target.drawString(font9, bkRsc.getSWindSpeed(), dispX + 200, dispY, 0.0,
                TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
                VerticalAlignment.BOTTOM, null);
    }

    @SuppressWarnings("deprecation")
	private void drawNsharpSkewtDynamicData(IGraphicsTarget target, double zoomLevel,
            WGraphics world) throws VizException {
        
    	NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
    	
    	double dispX = world.getViewXmin() + 450;
    	double dispY =  world.getViewYmin()-100;
    	   	
    	//Column 1: pressure, C and F
    	target.drawString(font9, bkRsc.getSPressure(), dispX, dispY, 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.BOTTOM, null);

    	target.drawString(font9, bkRsc.getSTemperatureC(), dispX, dispY+25, 0.0,
    			TextStyle.NORMAL, NsharpConstants.color_red, HorizontalAlignment.LEFT,
    			VerticalAlignment.BOTTOM, null);
    	target.drawString(font9, bkRsc.getSTemperatureF(), dispX, dispY+50, 0.0,
    			TextStyle.NORMAL, NsharpConstants.color_red, HorizontalAlignment.LEFT,
    			VerticalAlignment.BOTTOM, null);
    	//column 2: m, ft, mixing ratio
    	float heightM = nsharpNative.nsharpLib.ihght((float)bkRsc.getDPressure());
    	String sHeightM = String.format("%.0fm",heightM);
    	target.drawString(font9, sHeightM, dispX+150, dispY, 0.0,
    			TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
    			VerticalAlignment.BOTTOM, null);
    	String sHeightFt = String.format("%.0fft",NsharpConstants.metersToFeet.convert(heightM));
    	target.drawString(font9, sHeightFt, dispX+150, dispY+25, 0.0,
    			TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
    			VerticalAlignment.BOTTOM, null);
    	target.drawString(font9, bkRsc.getSMixingRatio(), dispX+150, dispY+50, 0.0,
    			TextStyle.NORMAL, NsharpConstants.color_green, HorizontalAlignment.LEFT,
    			VerticalAlignment.BOTTOM, null);
    	//column 3: Theta, ThetaW, ThetaE
    	target.drawString(font9, bkRsc.getSThetaInK(), dispX+300, dispY, 0.0,
                TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
                VerticalAlignment.BOTTOM, null);
    	target.drawString(font9, bkRsc.getSWThetaInK(), dispX+300, dispY+25, 0.0,
                TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
                VerticalAlignment.BOTTOM, null);
    	target.drawString(font9, bkRsc.getSEThetaInK(), dispX+300, dispY+50, 0.0,
                TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
                VerticalAlignment.BOTTOM, null);
        
    }
    @SuppressWarnings("deprecation")
	private void drawNsharpDataFilelabel(IGraphicsTarget target, double zoomLevel,
            WGraphics world)
            throws VizException {
    	double X = NsharpConstants.SKEWT_REC_X_ORIG;
    	double Y = /*NsharpConstants.top -*/ 15;
    	target.drawString(font10, pickedStnInfoStr, X, Y, 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.MIDDLE, null);
    	
    	//Also draw stn lat/lon info string and sounding type string
    	if(pickedStnInfo != null){
    		String latlonStr = pickedStnInfo.getLatitude() + "," + pickedStnInfo.getLongitude();
    		target.drawString(font10, latlonStr, X, 3*Y, 0.0,
    				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
    				VerticalAlignment.MIDDLE, null);
    		
    		target.drawString(font10, "Sounding Type", NsharpConstants.SKEWT_REC_X_ORIG - 300, Y, 0.0,
    				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
    				VerticalAlignment.MIDDLE, null);
    		String sndTypeStr = pickedStnInfo.getSndType();
    		int len = Math.min(12, sndTypeStr.length());
    		sndTypeStr = sndTypeStr.substring(0, len);
    		target.drawString(font10, sndTypeStr, NsharpConstants.SKEWT_REC_X_ORIG - 300,3* Y, 0.0,
    				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
    				VerticalAlignment.MIDDLE, null);
    	}
    }
    @SuppressWarnings("deprecation")
	public void drawNsharpSkewtCursorData(IGraphicsTarget target) throws VizException{
    	NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
    	
    	WGraphics WGc = bkRsc.getSkewTBackground().getWorld();
    	Coordinate c = WxMath.reverseSkewTXY(WGc.unMap(cursorCor.x, cursorCor.y));
		double p_mb = c.y;
		float htFt, htM, relh=-1;
		String curStrFormat;
		String curStr;
		htM = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght((float)p_mb));
		htFt= nsharpNative.nsharpLib.mtof(htM);
		if (nsharpNative.nsharpLib.itemp((float)p_mb) > -9998.0 && nsharpNative.nsharpLib.idwpt((float)p_mb) > -9998.0){
			FloatByReference parm= new FloatByReference(0);
			relh= nsharpNative.nsharpLib.relh((float)p_mb, parm);
			curStrFormat= "%4.0fmb  %5.0fft/%.0fm agl  %2.0f%%";
			curStr = String.format(curStrFormat, p_mb,htFt,htM,relh);
		}
		else{
			curStrFormat= "%4.0fmb  %5.0fft/%.0fm agl";
			curStr = String.format(curStrFormat, p_mb,htFt,htM);
		}
		double y = WGc.mapY(WxMath.getSkewTXY(p_mb, 10).y);
		target.drawString(font10,curStr, WGc.mapX(NsharpConstants.left)+10,
				y, 0.0, TextStyle.NORMAL,
				NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
				VerticalAlignment.MIDDLE, null);
		
		curStrFormat = "%4.1f/%4.1fC  %4.0f/%.0f kt";
		curStr = String.format(curStrFormat, nsharpNative.nsharpLib.itemp((float)p_mb),
				nsharpNative.nsharpLib.idwpt((float)p_mb), nsharpNative.nsharpLib.iwdir((float)p_mb),
				nsharpNative.nsharpLib.iwspd((float)p_mb));
		target.drawString(font10,curStr, WGc.mapX(NsharpConstants.right)-300,
				y, 0.0, TextStyle.NORMAL,
				NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
				VerticalAlignment.MIDDLE, null);
    }
    
    /*
     * This function mostly follow display_effective_layer() of xwvid1.c     */
    @SuppressWarnings("deprecation")
	private void drawEffectiveLayerLines(IGraphicsTarget target) throws VizException{
    	NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
    	
    	WGraphics WGc = bkRsc.getSkewTBackground().getWorld();
    	FloatByReference topPF= new FloatByReference(0);
		FloatByReference botPF= new FloatByReference(0);
    	nsharpNative.nsharpLib.get_effectLayertopBotPres(topPF, botPF);
    	if(botPF.getValue() < 1 ) return;
    	String botStr, topStr;
    	float aglTop, aglBot;
    	aglTop = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(topPF.getValue()));
    	aglBot = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(botPF.getValue()));
    	// Draw effective sfc level 
    	if (aglBot < 1)
		{ 
    		botStr = "SFC";
		}
		else
		{  
			botStr = String.format( "%.0fm", aglBot);
		}
    	double y = WGc.mapY(WxMath.getSkewTXY(botPF.getValue(), 10).y);
    	target.drawLine( WGc.mapX(NsharpConstants.left) +200, y, 0.0,  WGc.mapX(NsharpConstants.left) +280, y, 0.0,
				NsharpConstants.color_cyan_md, 2);
		target.drawString(font10,botStr, WGc.mapX(NsharpConstants.left)+180,
				y, 0.0, TextStyle.NORMAL,
				NsharpConstants.color_cyan_md, HorizontalAlignment.RIGHT,
				VerticalAlignment.MIDDLE, null);
    	// Draw effective top level		
    	topStr = String.format( "%.0fm", aglTop);
    	double y1 = WGc.mapY(WxMath.getSkewTXY(topPF.getValue(), 10).y);
    	target.drawLine( WGc.mapX(NsharpConstants.left) +200, y1, 0.0,  WGc.mapX(NsharpConstants.left) +280, y1, 0.0,
				NsharpConstants.color_cyan_md, 2);
    	if(aglTop > aglBot){
    		target.drawString(font10,topStr, WGc.mapX(NsharpConstants.left)+180,
    				y1, 0.0, TextStyle.NORMAL,
    				NsharpConstants.color_cyan_md, HorizontalAlignment.RIGHT,
    				VerticalAlignment.MIDDLE, null);
    		//System.out.println("aglbot="+aglBot+" agltop="+aglTop);
    	}
		
    	// Draw connecting line
		target.drawLine( WGc.mapX(NsharpConstants.left) +240, y, 0.0,  WGc.mapX(NsharpConstants.left) +240, y1, 0.0,
				NsharpConstants.color_cyan_md, 2);
    	// Compute and display effective helicity
    	topPF.setValue(0); // just a placeholder
    	botPF.setValue(0);
    	float helicity = nsharpNative.nsharpLib.helicity(aglBot,aglTop, smWindDir,smWindSpd, topPF, botPF);
    	String helicityStr = String.format("%4.0f m%cs%c", helicity,NsharpConstants.SQUARE_SYMBOL, NsharpConstants.SQUARE_SYMBOL);

    	//draw kelicity
    	target.drawString(font10,helicityStr, WGc.mapX(NsharpConstants.left)+190,
				y1-20, 0.0, TextStyle.NORMAL,
				NsharpConstants.color_cyan_md, HorizontalAlignment.LEFT,
				VerticalAlignment.MIDDLE, null);
    }
    
    @SuppressWarnings("deprecation")
	private void drawLclLine(IGraphicsTarget target) throws VizException{
    	NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
    	
    	WGraphics WGc = bkRsc.getSkewTBackground().getWorld();
    	//System.out.println("drawLclLine called define_parcel pType="+currentParcel+" pre="+ currentParcelLayerPressure);
		
    	nsharpNative.nsharpLib.define_parcel(currentParcel, currentParcelLayerPressure);
    	_lplvalues lpvls = new _lplvalues();
		nsharpNative.nsharpLib.get_lpvaluesData(lpvls);

		float sfctemp, sfcdwpt, sfcpres;
		sfctemp = lpvls.temp;
		sfcdwpt = lpvls.dwpt;
		sfcpres = lpvls.pres;
		// get parcel data by calling native nsharp parcel() API. value is returned in pcl 
		_parcel pcl = new _parcel();
		nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl);
    	//draw LCL line
    	float lcl = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lclpres ));
		if(lcl != NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA){
			double pressure = nsharpNative.nsharpLib.ipres(lcl+(int)(soundingLys.get(0).getGeoHeight()));
			//System.out.println("lcl= " + lcl + " lclpres ="+pcl.lclpres +" pressure="+ pressure);
        	double y = WGc.mapY(WxMath.getSkewTXY(pressure, 10).y);
        	target.drawLine( WGc.mapX(NsharpConstants.right) - 220, y, 0.0,  WGc.mapX(NsharpConstants.right) -180, y, 0.0,
        			NsharpConstants.color_green, 2);
        	target.drawString(font10, "LCL", WGc.mapX(NsharpConstants.right)-220,
        			y+15, 0.0, TextStyle.NORMAL,
        			NsharpConstants.color_green, HorizontalAlignment.LEFT,
        			VerticalAlignment.MIDDLE, null);
		}
		//draw LFC line
		if(pcl.lclpres!=pcl.lfcpres){
			float lfc = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lfcpres ));
			if(lfc != NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA){
				double pressure = nsharpNative.nsharpLib.ipres(lfc+(int)(soundingLys.get(0).getGeoHeight()));
				//System.out.println("lfcpres ="+pcl.lfcpres +" pressure="+ pressure);
				double y = WGc.mapY(WxMath.getSkewTXY(pressure, 10).y);
				target.drawLine( WGc.mapX(NsharpConstants.right) - 220, y, 0.0,  WGc.mapX(NsharpConstants.right) -180, y, 0.0,
						NsharpConstants.color_yellow, 2);
				target.drawString(font10, "LFC", WGc.mapX(NsharpConstants.right)-220,
						y, 0.0, TextStyle.NORMAL,
						NsharpConstants.color_yellow, HorizontalAlignment.RIGHT,
						VerticalAlignment.MIDDLE, null);
			}
		}
		// draw EL line
		if(pcl.lclpres!=pcl.elpres && pcl.elpres!=pcl.lfcpres){
			float el = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.elpres ));
			if(el != NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA){
				double pressure = nsharpNative.nsharpLib.ipres(el+(int)(soundingLys.get(0).getGeoHeight()));
				//System.out.println("elpres ="+pcl.elpres +" pressure="+ pressure);
				double y = WGc.mapY(WxMath.getSkewTXY(pressure, 10).y);
				target.drawLine( WGc.mapX(NsharpConstants.right) - 220, y, 0.0,  WGc.mapX(NsharpConstants.right) -180, y, 0.0,
						NsharpConstants.color_red, 2);
				target.drawString(font10, "EL", WGc.mapX(NsharpConstants.right)-220,
						y-22, 0.0, TextStyle.NORMAL,
						NsharpConstants.color_red, HorizontalAlignment.LEFT,
						VerticalAlignment.MIDDLE, null);
			}
		}
		// draw FZL line
		FloatByReference fValue= new FloatByReference(0);
		float fgzm = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(nsharpNative.nsharpLib.temp_lvl( 0, fValue )));
		float fgzft = nsharpNative.nsharpLib.mtof(fgzm);
		if(nsharpNative.nsharpLib.qc(fgzft)==1) {
			double pressure = nsharpNative.nsharpLib.ipres(fgzm+(int)(soundingLys.get(0).getGeoHeight()));
			//System.out.println("elpres ="+pcl.elpres +" pressure="+ pressure);
        	double y = WGc.mapY(WxMath.getSkewTXY(pressure, 10).y);
        	target.drawLine( WGc.mapX(NsharpConstants.right) - 220, y, 0.0,  WGc.mapX(NsharpConstants.right) -180, y, 0.0,
        			NsharpConstants.color_cyan, 2);
        	String textStr = "FGZ= %.0f'";
			textStr = String.format(textStr,fgzft);
        	target.drawString(font10, textStr, WGc.mapX(NsharpConstants.right)-220,
        			y-22, 0.0, TextStyle.NORMAL,
        			NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
        			VerticalAlignment.MIDDLE, null);
		}
		// draw -20Cline
		float h20m = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(nsharpNative.nsharpLib.temp_lvl( -20, fValue )));
		float h20ft = nsharpNative.nsharpLib.mtof(h20m);
		if(nsharpNative.nsharpLib.qc(h20ft)==1) {
			double pressure = nsharpNative.nsharpLib.ipres(h20m+(int)(soundingLys.get(0).getGeoHeight()));
			//System.out.println("elpres ="+pcl.elpres +" pressure="+ pressure);
        	double y = WGc.mapY(WxMath.getSkewTXY(pressure, -20).y);
        	//double x = WGc.mapX(WxMath.getSkewTXY(pressure, -20).x);
        	target.drawLine( WGc.mapX(NsharpConstants.right) - 220, y, 0.0,  WGc.mapX(NsharpConstants.right) -180, y, 0.0,
        			NsharpConstants.color_cyan, 2);
        	String textStr = "-20C= %.0f'";
			textStr = String.format(textStr,h20ft);
        	target.drawString(font10, textStr, WGc.mapX(NsharpConstants.right)-220,
        			y-22, 0.0, TextStyle.NORMAL,
        			NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
        			VerticalAlignment.MIDDLE, null);
		}
		// draw -30Cline
		float h30m = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(nsharpNative.nsharpLib.temp_lvl( -30, fValue )));
		float h30ft = nsharpNative.nsharpLib.mtof(h30m);
		if(nsharpNative.nsharpLib.qc(h30ft)==1) {
			double pressure = nsharpNative.nsharpLib.ipres(h30m+(int)(soundingLys.get(0).getGeoHeight()));
			//System.out.println("elpres ="+pcl.elpres +" pressure="+ pressure);
        	double y = WGc.mapY(WxMath.getSkewTXY(pressure, 10).y);
        	target.drawLine( WGc.mapX(NsharpConstants.right) - 220, y, 0.0,  WGc.mapX(NsharpConstants.right) -180, y, 0.0,
        			NsharpConstants.color_cyan, 2);
        	String textStr = "-30C= %.0f'";
			textStr = String.format(textStr,h30ft);
        	target.drawString(font10, textStr, WGc.mapX(NsharpConstants.right)-220,
        			y-22, 0.0, TextStyle.NORMAL,
        			NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
        			VerticalAlignment.MIDDLE, null);
		}
    }
    public void printHeightMark(WGraphics world, GC gc) throws VizException{
        //print feet  scales...
    	double vyMax = world.getViewYmax();
    	double vyMin = world.getViewYmin();
    	double vxMax = world.getViewXmax();
    	for (int j = 0; j < NsharpConstants.HEIGHT_LEVEL_FEET.length; j++) {
        	float meters = (float)NsharpConstants.feetToMeters.convert(NsharpConstants.HEIGHT_LEVEL_FEET[j]);
        			
        			double pressure = nsharpNative.nsharpLib.ipres(meters);
        			double y = world.mapY(WxMath.getSkewTXY(pressure, -50).y);

        			gc.drawString( Integer.toString(NsharpConstants.HEIGHT_LEVEL_FEET[j]/1000), (int)vxMax + 40,
        					(int)y,false);
        			

        			gc.drawLine( (int)vxMax + 50,(int) y,  (int) vxMax + 45,(int) y);
         } 
      //print meter  scales...
        for (int j = 0; j < NsharpConstants.HEIGHT_LEVEL_METERS.length; j++) {
        		int meters = NsharpConstants.HEIGHT_LEVEL_METERS[j];
        			
        			double pressure = nsharpNative.nsharpLib.ipres(meters);
        			double y = world.mapY(WxMath.getSkewTXY(pressure, -50).y);

        			gc.drawString( Integer.toString(meters/1000), (int)vxMax + 52,
        					(int)y,false);
        			

        			gc.drawLine( (int)vxMax + 50,(int) y,  (int) vxMax + 55,(int) y);
         } 
        // print surface level mark
        double y = world.mapY(WxMath.getSkewTXY(soundingLys.get(0).getPressure(), -50).y);
        gc.drawString("SFC("+Integer.toString((int)(soundingLys.get(0).getGeoHeight()))+"m)", (int)vxMax+ 50,
        (int)y, false);
		gc.drawLine((int) vxMax+ 50, (int)y, (int) vxMax + 55,(int) y);
		// top level mark at 100 mbar
        y = world.mapY(WxMath.getSkewTXY(100, -50).y);
        float hgt = nsharpNative.nsharpLib.ihght(100);
        gc.drawString(Float.toString(hgt/1000F), (int)vxMax+ 50,(int)y, false);
        gc.drawString("Kft  Km", (int) vxMax+35, (int)y -8);
        gc.drawString("MSL", (int) vxMax+45, (int)y -15);
		gc.drawLine((int) vxMax+40, (int)y, (int) vxMax + 60,(int) y);
		
		gc.drawLine((int) vxMax+50, (int)vyMin, (int) vxMax + 50,(int)vyMax);

    }
    
    /**
     * Prints the pressure lines number at left side out of skewT bkgd for printing job
     * 
     * @throws VizException
     */
    public void printNsharpPressureLinesNumber(WGraphics world, GC gc) throws VizException {
        String s = null;
        double vxMax = world.getViewXmax();
    	double vxMin = world.getViewXmin();
        for (int i = 0; i < NsharpConstants.PRESSURE_MAIN_LEVELS.length; i++) {
        	//we only care about pressure for this case, temp is no important  when calling getSkewTXY
        	Coordinate coor = WxMath.getSkewTXY(NsharpConstants.PRESSURE_MAIN_LEVELS[i],0);

        	gc.drawLine((int)vxMin, (int)world.mapY(coor.y),
        			(int)vxMax,
        			(int)world.mapY(coor.y));

        }
        for (int i = 0; i < NsharpConstants.PRESSURE_MARK_LEVELS.length; i++) {
        	//we only care about pressure for this case, temp is no important  when calling getSkewTXY
        	Coordinate coor = WxMath.getSkewTXY(NsharpConstants.PRESSURE_MARK_LEVELS[i],0);

        	gc.drawLine((int)vxMin, (int)world.mapY(coor.y),
        			(int)vxMin+10,
        			(int)world.mapY(coor.y));

        }
        for (int i = 0; i < NsharpConstants.PRESSURE_NUMBERING_LEVELS.length; i++) {
        	s = NsharpConstants.pressFormat.format(NsharpConstants.PRESSURE_NUMBERING_LEVELS[i]);
        	//we only care about pressure for this case, temp is no important  when calling getSkewTXY
        	Coordinate coor = WxMath.getSkewTXY(NsharpConstants.PRESSURE_NUMBERING_LEVELS[i],0);

        	gc.drawString( s, (int)vxMin-20,
        			(int)world.mapY(coor.y), false);
        }
    }

    /**
     * Print the temp number at bottom out of skewT bkgd for printing job
     * 
     * @throws VizException
     */
    public void printNsharpTempNumber(WGraphics world, GC gc) throws VizException {
        for (int i = 40; i > -50; i -= 10) {
            Coordinate coorStart = WxMath.getSkewTXY(1050, i);
            double startX = world.mapX(coorStart.x);
            double startY = world.mapY(coorStart.y);

            gc.drawString( Integer.toString(i), (int)startX,(int)startY+5,false);
        }
        for (int i = -60; i > -120; i -= 10) {
            Coordinate coorEnd = WxMath.getSkewTXY(100, i);

            //System.out.println("X = "+ startX + " Y = "+ startY);
            double endX = world.mapX(coorEnd.x);
            double endY = world.mapY(coorEnd.y);


            gc.drawString( Integer.toString(i),(int) endX,(int)endY-10,false);
        }
    }
    /**
     * Draws station id out of skewT bkgd.
     * 
     * @throws VizException
     */
    @SuppressWarnings("deprecation")
	private void drawNsharpStationId(IGraphicsTarget target, WGraphics world, Rectangle rect) throws VizException {
        PixelExtent extent = new PixelExtent(rect);
        RGB color;
        target.setupClippingPlane(extent);
        double x= NsharpConstants.STATION_ID_REC_X_ORIG + 5;
        double ly= NsharpConstants.STATION_ID_REC_Y_ORIG  + 5;
        for (int j = 0; j< stationIdList.size(); j++)
        {
        	String s = stationIdList.get(j).elementDescription;
        	
        	//System.out.println("stationId: "+ s);
        	ly = ly +  25;

        	//color = NsharpConstants.color_white;
        	
        	//mark user picked stn as green
        	color = elementColorMap.get(stationIdList.get(j).elementColor);
         	target.drawString(font10, s, x,
        			ly, 0.0,
        			IGraphicsTarget.TextStyle.NORMAL,
        			color,
        			HorizontalAlignment.LEFT,  
        			VerticalAlignment.BOTTOM, null);
        }
      /*plot notations:
        target.drawLine(NsharpConstants.STATION_ID_REC_X_ORIG, NsharpConstants.STATION_ID_NOTATION_Y_START, 0.0, 
    			NsharpConstants.STATION_ID_VIEW_X_END , NsharpConstants.STATION_ID_NOTATION_Y_START, 0.0, 
    			NsharpConstants.color_white,1, LineStyle.SOLID);
        double y = NsharpConstants.STATION_ID_NOTATION_Y_START+50;
        color = NsharpConstants.color_green;
        target.drawString(font10, "Enabled", x,
        		y, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);

        
        y=y+25;
        color = NsharpConstants.color_red;
        target.drawString(font10, "Disabled", x,
        		y, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);
	*/
    }
    
    /**
     * Draws the stn id title out of skewT bkgd.
     * 
     * @throws VizException
     */
    @SuppressWarnings("deprecation")
	private void drawNsharpStationIdTitle(IGraphicsTarget target) throws VizException {
    	//darw title first
        String s = stationIdList.size() + " stations";
        double x = NsharpConstants.STATION_ID_REC_X_ORIG;
        double y = NsharpConstants.STATION_ID_REC_Y_ORIG - 30;
        
        target.drawString(font10, s, x,
        		y, 0.0,
                IGraphicsTarget.TextStyle.NORMAL,
                NsharpConstants.color_white,
                HorizontalAlignment.LEFT,
                VerticalAlignment.MIDDLE, null);
        
    }
    @SuppressWarnings({ "deprecation", "unused" })
    //To be used later...
	private void drawHodoWindMotionBox(IGraphicsTarget target, Rectangle rect) throws VizException {
    	target.drawShadedRect(new PixelExtent(rect.x, rect.x+rect.width, rect.y, rect.y+rect.height), NsharpConstants.color_black, 1.0, null);
    	target.drawWireframeShape(hodoWindMotionBoxShape, NsharpConstants.color_cyan, commonLinewidth,commonLineStyle,font10);
    	RGB colorN, colorS, colorM;
    	switch(currentHodoWindMode){
    	case HODO_NORMAL:
    		colorN = NsharpConstants.color_white;
    		colorS = NsharpConstants.color_cyan;
    		colorM = NsharpConstants.color_cyan;
    		break;
    	case HODO_STORMRELATIVE:
    		colorN = NsharpConstants.color_cyan;
    		colorS = NsharpConstants.color_white;
    		colorM = NsharpConstants.color_cyan;
    		break;
    	case HODO_BNDRY:
    		colorN = NsharpConstants.color_cyan;
    		colorS = NsharpConstants.color_cyan;
    		colorM = NsharpConstants.color_cyan;
    		break;
    	case HODO_MEANWIND:
    	default:
    		colorN = NsharpConstants.color_cyan;
    		colorS = NsharpConstants.color_cyan;
    		colorM = NsharpConstants.color_white;
    		break;
    	}
    	double xOri = NsharpConstants.WIND_MOTION_REC_X_ORIG;
    	double yOri = NsharpConstants.WIND_MOTION_REC_Y_ORIG;
    	target.drawString(font10, "NORMAL   ", xOri+ NsharpConstants.WIND_MOTION_REC_WIDTH/2,
    			yOri+NsharpConstants.WIND_MOTION_REC_HEIGHT/3, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			colorN,
    			HorizontalAlignment.CENTER,  
    			VerticalAlignment.BOTTOM, null);
    	target.drawString(font10, "STORMREL ", xOri+ NsharpConstants.WIND_MOTION_REC_WIDTH/2,
    			yOri+ 2* NsharpConstants.WIND_MOTION_REC_HEIGHT/3, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			colorS,
    			HorizontalAlignment.CENTER,  
    			VerticalAlignment.BOTTOM, null);
    	target.drawString(font10, "MEAN WIND", xOri+ NsharpConstants.WIND_MOTION_REC_WIDTH/2,
    			yOri+ NsharpConstants.WIND_MOTION_REC_HEIGHT, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			colorM,
    			HorizontalAlignment.CENTER,  
    			VerticalAlignment.BOTTOM, null);
    }
    /**
     * Draws datatime box out of skewT bkgd.
     * 
     * @throws VizException
     */
    @SuppressWarnings("deprecation")
	private void drawNsharpDataTimelines(IGraphicsTarget target, WGraphics world, Rectangle rect) throws VizException {
        PixelExtent extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        //System.out.println("drawNsharpDataTimelines picked stn info: "+ pickedStnInfoStr);
        double x = NsharpConstants.DATA_TIMELINE_REC_X_ORIG + 5, x1 ;
        double nextPageY = NsharpConstants.DATA_TIMELINE_REC_Y_ORIG + NsharpConstants.CHAR_HEIGHT;
        RGB color = NsharpConstants.color_yellow;
        String s = "nextPage", s1;
        target.drawString(font10, s, x,
        		nextPageY, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);
        
        target.drawLine(NsharpConstants.DATA_TIMELINE_REC_X_ORIG, NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END, 0.0, 
    			NsharpConstants.DATA_TIMELINE_VIEW_X_END , NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END, 0.0, 
    			NsharpConstants.color_white,1, LineStyle.SOLID);
        
        int numTimeLineToShow = (NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END)/NsharpConstants.CHAR_HEIGHT;
    	int startIndex = (curTimeLinePage-1) * numTimeLineToShow;
        int i = 1, colorIndex=1;
        for (int j = startIndex; j< dataTimelineList.size(); j++)
        {
        	ElementStateProperty elm = dataTimelineList.get(j);
        	s = elm.elementDescription;
        	
        	//System.out.println("selectedTimeList: "+ s);
        	double ly = NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END + NsharpConstants.CHAR_HEIGHT * i;

        	if(!compareIsOn)
        		color = elementColorMap.get(elm.elementColor);
        	else {
        		if(elm.elementState == State.PICKED ){
    				color = NsharpConstants.COLOR_ARRAY[0];
    			} else if(elm.elementState == State.GROUPED){
    				color = NsharpConstants.COLOR_ARRAY[colorIndex];
    				
    			} else {
    				color = elementColorMap.get(elm.elementColor);;
    			}
        		colorIndex++;//always increase index, no matter ploting this elm or not
				if(colorIndex > NsharpConstants.COLOR_ARRAY.length-1)
					colorIndex =1;
        	}
        	//darw data time line, split string to make all lines in line vertically.
        	s = s.substring(0,s.length()-3);
        	String stok1, stok2, stok3;
        	StringTokenizer stoken = new StringTokenizer(s);
			if(sortByStn){
				stok1 = stoken.nextToken(); //stn name
				s = stok1;
				stok2 = stoken.nextToken(); //day
				stok2 = stok2.substring(2);
				stok3 = stoken.nextToken(); //hour
				s1 = stok2.concat(" "+stok3.substring(0, 2));
				x1 = x +130;
			}
			else{
				stok3 = stoken.nextToken(); //stn name
				s1 = stok3;
				stok1 = stoken.nextToken(); //day
				stok1 = stok1.substring(2);
				stok2 = stoken.nextToken(); //hour
				s = stok1.concat(" "+stok2.substring(0, 2));
				x1 = x+ 165;
			}
        	target.drawString(font10, s, x,
        			ly, 0.0,
        			IGraphicsTarget.TextStyle.NORMAL,
        			color,
        			HorizontalAlignment.LEFT,  
        			VerticalAlignment.BOTTOM, null);
        	target.drawString(font10, s1, x1,
        			ly, 0.0,
        			IGraphicsTarget.TextStyle.NORMAL,
        			color,
        			HorizontalAlignment.LEFT,  
        			VerticalAlignment.BOTTOM, null);
        	
        	i++;
        	if (ly >= NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.CHAR_HEIGHT)
        		//we dont show time line that extends below time line box
        		break;
        		
        }
        
        //plot sorting area
        x = NsharpConstants.DATA_TIMELINE_SORT_X_START;
        target.drawLine(x, NsharpConstants.DATA_TIMELINE_REC_Y_ORIG, 0.0, 
        		x, NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END, 0.0, 
    			NsharpConstants.color_white,1, LineStyle.SOLID);
        color = NsharpConstants.color_yellow;
        s = "by:";     
        target.drawString(font10, s, x+2,
        		nextPageY, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);
        
        x = NsharpConstants.DATA_TIMELINE_SORT_X_START + (NsharpConstants.DATA_TIMELINE_VIEW_X_END - NsharpConstants.DATA_TIMELINE_SORT_X_START)/3;
        s = "Stn";
        s1 = "Time";
        RGB color1;
        if(sortByStn){
        	
        	color = NsharpConstants.color_green;
        	color1 = NsharpConstants.color_orange;
        }
        else{
        	color = NsharpConstants.color_orange;
        	color1 = NsharpConstants.color_green;
        }
        target.drawString(font10, s, x+10,
        		nextPageY, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);
        target.drawString(font10, s1, x+60,
        		nextPageY, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color1,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);
    }
    /**
     * Draws the temp number at bottom out of skewT bkgd.
     * 
     * @throws VizException
     */
    @SuppressWarnings("deprecation")
	private void drawNsharpDataTimelineTitle(IGraphicsTarget target) throws VizException {
        String s = dataTimelineList.size() + " data time lines";
        double x = NsharpConstants.DATA_TIMELINE_REC_X_ORIG;
        double y = NsharpConstants.DATA_TIMELINE_REC_Y_ORIG - 60;
        
        target.drawString(font10, s, x,
        		y, 0.0,
                IGraphicsTarget.TextStyle.NORMAL,
                NsharpConstants.color_white,
                HorizontalAlignment.LEFT,
                VerticalAlignment.MIDDLE, null);
        y = y+NsharpConstants.CHAR_HEIGHT;
        s = "page " + curTimeLinePage+"/"+totalTimeLinePage;
        target.drawString(font10, s, x,
        		y, 0.0,
                IGraphicsTarget.TextStyle.NORMAL,
                NsharpConstants.color_green,
                HorizontalAlignment.LEFT,
                VerticalAlignment.MIDDLE, null);
    }

    /**
     * Draws color notations
     * 
     * @throws VizException
     */
    @SuppressWarnings("deprecation")
	private void drawNsharpColorNotation(IGraphicsTarget target, WGraphics world, Rectangle rect) throws VizException {
        PixelExtent extent = new PixelExtent(rect);
        RGB color;
        target.setupClippingPlane(extent);
      //plot notations:
        
        double x = NsharpConstants.COLOR_NOTATION_REC_X_ORIG+5;
        double y = NsharpConstants.COLOR_NOTATION_REC_Y_ORIG+25;
        color = NsharpConstants.color_white;
        target.drawString(font10, "Color Notations:", x,
        		y, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);

        y=y+25;
        color = NsharpConstants.color_green;
        target.drawString(font10, "Current", x,
        		y, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);

        x = x+180;
        color = NsharpConstants.color_cyan;
        target.drawString(font10, "Active", x,
        		y, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);
        
        
        
        x = NsharpConstants.COLOR_NOTATION_REC_X_ORIG+5;
        y=y+25;
        color = NsharpConstants.color_white;
        target.drawString(font10, "Not Active", x,
        		y, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);

        /*x = x+180;
        color = NsharpConstants.color_red;
        target.drawString(font10, "Disabled", x,
        		y, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);*/

        target.clearClippingPlane();

    }
    /**
     * 
     * Draws Wind barb and wind speed vs height into box
     * This function followed algorithm in plot_barbs (void) at xwvid1.c
     * to choose wind bulb for drawing around every 400m
     * 
     */
    private void drawNsharpWindBarb(IGraphicsTarget target, double zoomLevel,
            WGraphics world,  RGB icolor)throws VizException {
        ArrayList<List<LineStroke>> windList = new ArrayList<List<LineStroke>>();

        double windX = NsharpConstants.right - BARB_LENGTH;
        float lastHeight = -999;
        double windY;
        for (NcSoundingLayer layer : soundingLys) {
            float pressure = layer.getPressure();
            float spd = layer.getWindSpeed();
            float dir = layer.getWindDirection();
            if ( pressure < 100 || spd < 0 ) {
                continue;
            }
            if(spd > 140)
            	spd = 140;
            if ((layer.getGeoHeight() - lastHeight) < 400){
            	
            	continue;
            }

            // Get the vertical ordinate.
            windY = WxMath.getSkewTXY(pressure, 0).y;
            List<LineStroke> barb = WindBarbFactory.getWindGraphics((double) (spd), (double) dir);
            if (barb != null) {
                WindBarbFactory.scaleBarb(barb, zoomLevel);
                WindBarbFactory.translateBarb(barb, windX, windY);
                windList.add(barb);
            }
           
            
            lastHeight = layer.getGeoHeight();
        }

        for (List<LineStroke> barb : windList) {
        	for (LineStroke stroke : barb) {
        		//System.out.println("p1x="+(int)stroke.getPoint().x+" p1y="+(int)stroke.getPoint().y);
        		stroke.render(target, world, icolor);
        	}
        }
    	
    }

    /**
     * 
     * Print Wind barb for printing job
     * This function followed algorithm in plot_barbs (void) at xwvid1.c
     * to choose wind bulb for drawing around every 400m
     * 
     */
    public void printNsharpWind(WGraphics world, GC gc)throws VizException {
        ArrayList<List<LineStroke>> windList = new ArrayList<List<LineStroke>>();

        double windX = world.getViewXmax() + 6*BARB_LENGTH;
        //System.out.println("windX="+windX);
        float lastHeight = -999;
        double windY;
        for (NcSoundingLayer layer : soundingLys) {
            float pressure = layer.getPressure();
            float spd = layer.getWindSpeed();
            float dir = layer.getWindDirection();
            

            if ( pressure < 100) {
                continue;
            }
            
            if ((layer.getGeoHeight() - lastHeight) < 400){
            	
            	continue;
            }

            // Get the vertical ordinate.
            windY =  world.mapY(WxMath.getSkewTXY(pressure, 0).y);
            
            List<LineStroke> barb = WindBarbFactory.getWindGraphics(
                    /*metersPerSecondToKnots.convert*/(double) (spd), (double) dir);
            if (barb != null) {
                WindBarbFactory.scaleBarb(barb, -7);
                WindBarbFactory.translateBarb(barb, windX, windY);
                windList.add(barb);
            }
             
            lastHeight = layer.getGeoHeight();
        }
        Coordinate pt1=new Coordinate(0,0), pt2;
        for (List<LineStroke> barb : windList) {
            for (LineStroke stroke : barb) {
                //stroke render: rewrite stroke.render() for our printing purpose
                if(stroke.getType()=="M") {
                	pt1 = stroke.getPoint();
                	//change X coordinate by mirroring x coordinate at windX axis. AS we scaleBarb with -5 time. 
                	// It is easier to mirror at x-axis for this case.
                	pt1.x = windX - (pt1.x-windX);
                	//System.out.print("Myp1x="+(int)pt1.x+" p1y="+(int)pt1.y);
                    
                }
                else if(stroke.getType()=="D"){
                    pt2 = stroke.getPoint();
                    pt2.x = windX - (pt2.x-windX);
                    //System.out.println( " p2x="+(int)pt2.x+" p2y="+ (int)pt2.y);
                    gc.drawLine((int)pt1.x, (int)pt1.y, (int)pt2.x, (int)pt2.y);
                }
            }
        }
        gc.drawLine((int)windX,(int)world.mapY(WxMath.getSkewTXY(100, 0).y),
        		(int)windX,(int)world.mapY(WxMath.getSkewTXY(1000, 0).y));
    }

    /**
     * 
     * Print the wetbulb trace curve
     * 
     * @throws VizException
     */
    public void printNsharpWetbulbTraceCurve( 
            WGraphics world,  GC gc) throws VizException {
    	if((soundingLys == null) || (soundingLys.size()==0))
    			return;
    	float t1;
    
        Coordinate c2 =  null;
        Coordinate c1;
        // print trace
        for (NcSoundingLayer layer : this.soundingLys) {
        	if (layer.getDewpoint() > -200){
        		t1 = nsharpNative.nsharpLib.wetbulb(layer.getPressure(), layer.getTemperature(),
        				layer.getDewpoint());

        		c1 = WxMath.getSkewTXY(layer.getPressure(), t1);
        		c1.x = world.mapX(c1.x);
        		c1.y = world.mapY(c1.y);
        		if(c2!= null){
        			gc.drawLine((int)c1.x, (int)c1.y, (int)c2.x, (int)c2.y);
        		}
        		c2 =  c1;
        	}
        }
        
   }
     
    public void printNsharpParcelTraceCurve(WGraphics world, GC gc) throws VizException{
    	if(soundingLys.size() > 0){
    		for (ParcelData parData: parcelList){
    			//plotNsharpParcelTraceCurve(null, 0, world, NsharpConstants.color_white,parData.parcelType, parData.userPressure, gc, true);
    			nsharpNative.nsharpLib.define_parcel(parData.parcelType, parData.parcelLayerPressure);

    			_lplvalues lpvls = new _lplvalues();
    			nsharpNative.nsharpLib.get_lpvaluesData(lpvls);

    			float sfctemp, sfcdwpt, sfcpres;
    			sfctemp = lpvls.temp;
    			sfcdwpt = lpvls.dwpt;
    			sfcpres = lpvls.pres;

    			float vtemp = nsharpNative.nsharpLib.virtemp (sfcpres, sfctemp, sfcdwpt);
    			Coordinate c1 = WxMath.getSkewTXY(sfcpres, vtemp);
    			c1.x = world.mapX(c1.x);
    			c1.y = world.mapY(c1.y);
    			FloatByReference p2 = new FloatByReference(0), t2 = new FloatByReference(0);;
    			nsharpNative.nsharpLib.drylift (sfcpres, sfctemp, sfcdwpt, p2, t2);
    			vtemp = nsharpNative.nsharpLib.virtemp (p2.getValue(), t2.getValue(), t2.getValue());
    			Coordinate c2 = WxMath.getSkewTXY(p2.getValue(), vtemp);
    			c2.x = world.mapX(c2.x);
    			c2.y = world.mapY(c2.y);

    			gc.drawLine((int)c1.x, (int)c1.y, (int)c2.x, (int)c2.y);
    			c1 = c2;


    			float t3;
    			for (float i = p2.getValue() - 50; i >= 100; i = i - 50)
    			{
    				t3 = nsharpNative.nsharpLib.wetlift (p2.getValue(), t2.getValue(), i);
    				vtemp = nsharpNative.nsharpLib.virtemp (i, t3, t3);
    				c2 = WxMath.getSkewTXY(i, vtemp);
    				c2.x = world.mapX(c2.x);
    				c2.y = world.mapY(c2.y);

    				gc.drawLine((int)c1.x, (int)c1.y, (int)c2.x, (int)c2.y);
    				c1 = c2;
    			}

    			t3 = nsharpNative.nsharpLib.wetlift (p2.getValue(), t2.getValue(), 100);
    			vtemp = nsharpNative.nsharpLib.virtemp (100, t3, t3);
    			c2 = WxMath.getSkewTXY(100, vtemp);
    			c2.x = world.mapX(c2.x);
    			c2.y = world.mapY(c2.y);

    			gc.drawLine((int)c1.x, (int)c1.y, (int)c2.x, (int)c2.y);
    		}
		}
    }
    private void plotHodoEditPoints(IGraphicsTarget target, 
            WGraphics world, RGB color) throws VizException {
        
        Coordinate c1;
        for (NcSoundingLayer layer : this.soundingLys){
        	if(layer.getPressure() < 100 || layer.getWindSpeed() <0)
        		continue;
        	float wspd = layer.getWindSpeed();
        	float wdir = layer.getWindDirection();
        	c1 = WxMath.uvComp(wspd, wdir);
        	target.drawPoint(world
        			.mapX(c1.x), world.mapY(c1.y), 0.0, color, PointStyle.CIRCLE);

        	
        }
    }
    private void plotPressureTempEditPoints(IGraphicsTarget target, 
            WGraphics world, RGB color, int type, List<NcSoundingLayer> soundingLys) throws VizException {
    	double maxPressure = WxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmax())).y;
        double minPressure = WxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmin())).y;
        PointStyle ps = PointStyle.CIRCLE;
    	for (NcSoundingLayer layer : soundingLys) {
        	double t;
        	if(type == TEMP_TYPE)
        		t = layer.getTemperature();
        	else if (type == DEWPOINT_TYPE)
        		t = layer.getDewpoint();
        	else
        		break;
            double pressure = layer.getPressure();
            if (t != INVALID_DATA  && pressure >= minPressure
                    && pressure <= maxPressure) {

                Coordinate c1 = WxMath.getSkewTXY(pressure, t);
                
                c1.x = world.mapX(c1.x);
                c1.y = world.mapY(c1.y);
                
                target.drawPoint(c1.x, c1.y, 0.0, color, ps);
                
            }
        }
    }
    /**
     * 
     * Print the temperature curve when during overlap or compare mode
     * 
     * @throws VizException
     */
    
    public void printNsharpPressureTempCurve(WGraphics world, int type, GC gc,List<NcSoundingLayer> soundingLys) throws VizException {
    	if((soundingLys == null) || (soundingLys.size()==0))
			return;

        double maxPressure = WxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmax())).y;
        double minPressure = WxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmin())).y;
        Coordinate c0 = null;
       for (NcSoundingLayer layer : soundingLys) {
        	double t;
        	if(type == TEMP_TYPE)
        		t = layer.getTemperature();
        	else if (type == DEWPOINT_TYPE)
        		t = layer.getDewpoint();
        	else
        		break;
            double pressure = layer.getPressure();
            if (t != INVALID_DATA  && pressure >= minPressure
                    && pressure <= maxPressure) {

                Coordinate c1 = WxMath.getSkewTXY(pressure, t);
                
                c1.x = world.mapX(c1.x);
                c1.y = world.mapY(c1.y);
                //System.out.println("C.x "+ c1.x + " C.y "+ c1.y);
                if (c0 != null) {
                	gc.drawLine((int)c0.x, (int)c0.y, (int)c1.x, (int)c1.y);
                } 
                c0 = c1;
            }
        }
        
    }
    
    
     /**
     * 
     * Print the HODO
     * 
     *  
     * @throws VizException
     */

    public void printNsharpHodoWind(WGraphics world, GC gc, List<NcSoundingLayer> soundingLays) throws VizException {
        Coordinate c0 = null;
        Coordinate c1;
        for (NcSoundingLayer layer : soundingLays){
        	if(layer.getPressure() < 100 || layer.getWindSpeed() <0)
        		continue;
            float wspd = layer.getWindSpeed();
            float wdir = layer.getWindDirection();
             c1 = WxMath.uvComp(wspd, wdir);
            if (c0 != null) {
            		gc.setLineWidth(1);
            		gc.drawLine((int)world.mapX(c0.x),(int) world.mapY(c0.y),(int) world
                            .mapX(c1.x),(int) world.mapY(c1.y));
            }
            c0 = c1;
        }

       
    }
    
    /*
     * smvtype: 1: small circle, 2 large circle, 3: square
     */
    @SuppressWarnings("deprecation")
	public void plotNsharpHodoVectors(IGraphicsTarget target, double zoomLevel,
            WGraphics world, GC gc,  boolean printEvent) throws VizException {
    	double radiusUnit = 10;
    	NsharpGraphConfigDialog configD = NsharpGraphConfigDialog.getAccess();
    	Coordinate c;
    	FloatByReference value1= new FloatByReference(-999);
		FloatByReference value2= new FloatByReference(-999);
		FloatByReference wdir= new FloatByReference(-999);
		FloatByReference wspd= new FloatByReference(-999);
		FloatByReference Surfpressure = new FloatByReference(-999);
		String textStr;
		
    	//plot  Mean Wind Vector, yellow square, by default plot it
		if(((configD != null ) && (configD.isMeanWind()))||(configD == null)){
			nsharpNative.nsharpLib.mean_wind( -1, -1, value1, value2, wdir, wspd);
			if( nsharpNative.nsharpLib.qc(wdir.getValue())==1 && nsharpNative.nsharpLib.qc(wspd.getValue())==1) {
				c = WxMath.uvComp(wspd.getValue(),wdir.getValue());
				c= world.map(c);
				if(printEvent== true){
					gc.setLineWidth(1);
					gc.drawRectangle((int) (c.x-radiusUnit), (int)( c.y-radiusUnit), (int)(2*radiusUnit), (int)(2*radiusUnit));
				}
				else {
					PixelExtent pe = new PixelExtent( c.x-radiusUnit, c.x+radiusUnit, c.y-radiusUnit, c.y+radiusUnit );
					target.drawRect(pe, NsharpConstants.color_yellow, markerWidth*2, 1.0f);
					textStr= String.format("%.0f/%.0f MW",wdir.getValue(),wspd.getValue());
					target.drawString(font9, textStr, c.x-radiusUnit, c.y+radiusUnit+5, 0.0,
			                TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
			                VerticalAlignment.TOP, null);
				}
			}
		}
		//plot 15/85 and/or 30/75 SMV, by default dont plot it
		if((configD != null ) && (configD.isSmv1585() || configD.isSmv3075())){
			nsharpNative.nsharpLib.get_surface(Surfpressure, value1, value2);
			if(nsharpNative.nsharpLib.qc(Surfpressure.getValue()) == 1) {
				nsharpNative.nsharpLib.mean_wind(Surfpressure.getValue(), nsharpNative.nsharpLib.ipres (nsharpNative.nsharpLib.msl (6000.0F)),value1, value2, wdir, wspd);
				if( nsharpNative.nsharpLib.qc(wdir.getValue())==1 && nsharpNative.nsharpLib.qc(wspd.getValue())==1) {
					// ----- Plot 30/75 Storm Motion Vector -----small red circle 
					if(configD.isSmv3075()){
						//System.out.println("Plot 30/75 Storm Motion Vector 2");
						float dir = (wdir.getValue() + 30.0f)%360;
						float spd = wspd.getValue() * 0.75f;
						//System.out.println(spd + " "+ wspd.getValue());
						c = WxMath.uvComp(spd,dir);
						c= world.map(c);
						if(printEvent== true){			
							gc.drawOval((int) (c.x-radiusUnit), (int)( c.y-radiusUnit), (int)(2*radiusUnit), (int)(2*radiusUnit));
							gc.drawLine((int) (c.x - radiusUnit/2), (int)c.y,(int) ( c.x + radiusUnit/2),(int) c.y);
							gc.drawLine((int) (c.x), (int)(c.y - radiusUnit/2),(int) ( c.x ),(int) (c.y + radiusUnit/2));
						}
						else {
							RGB color = NsharpConstants.color_red;
							target.drawCircle(c.x, c.y, 0, radiusUnit,color, markerWidth*2);
							target.drawLine(c.x - radiusUnit/2, c.y, 0.0,c.x + radiusUnit/2, c.y, 0.0, color,
									markerWidth);
							target.drawLine(c.x, c.y- radiusUnit/2, 0.0,c.x, c.y + radiusUnit/2, 0.0, color,
									markerWidth);	
						}
					}
					//----- Plot 15/85 Storm Motion Vector ----- small green color circle
					if(configD.isSmv1585()){
						float dir = (wdir.getValue() + 15.0f)%360;
						float spd = wspd.getValue() * 0.85f;
						//System.out.println(spd + " "+ wspd.getValue());
						c = WxMath.uvComp(spd,dir);
						c= world.map(c);
						if(printEvent== true){
							gc.drawOval((int) (c.x-radiusUnit), (int)( c.y-radiusUnit), (int)(2*radiusUnit), (int)(2*radiusUnit));
							gc.drawLine((int) (c.x - radiusUnit/2), (int)c.y,(int) ( c.x + radiusUnit/2),(int) c.y);
							gc.drawLine((int) (c.x), (int)(c.y - radiusUnit/2),(int) ( c.x ),(int) (c.y + radiusUnit/2));					
						}
						else {
							RGB color = NsharpConstants.color_green;
							target.drawCircle(c.x, c.y, 0, radiusUnit,color, markerWidth*2);
							target.drawLine(c.x - radiusUnit/2, c.y, 0.0,c.x + radiusUnit/2, c.y, 0.0, color,
									markerWidth);
							target.drawLine(c.x, c.y- radiusUnit/2, 0.0,c.x, c.y + radiusUnit/2, 0.0, color,
									markerWidth);
						}
					}
				}
			}
		}
		//plot Corfidi Vectors, color_stellblue small circles, by default Not plot it
		if((configD != null ) && configD.isCorfidiV()){
			//Upwind-Propagating MCS motion vector
			FloatByReference upwdir= new FloatByReference(-999);
			FloatByReference upwspd= new FloatByReference(-999);
			//Downwind-Propagating MCS motion vector
			FloatByReference dpwdir= new FloatByReference(-999);
			FloatByReference dpwspd= new FloatByReference(-999);
			FloatByReference value5= new FloatByReference(-999);
			FloatByReference value6= new FloatByReference(-999);
			
			nsharpNative.nsharpLib.corfidi_MCS_motion(value1, value2, dpwdir, dpwspd, value5, value6, upwdir, upwspd);
			c = WxMath.uvComp(dpwspd.getValue(),dpwdir.getValue());
			c= world.map(c);
			RGB color = NsharpConstants.color_lightblue;
			target.drawCircle(c.x, c.y, 0, radiusUnit/2,color, markerWidth);
			//target.drawLine(c.x - radiusUnit/2, c.y, 0.0,c.x + radiusUnit/2, c.y, 0.0, color,
			//		markerWidth);
			//target.drawLine(c.x, c.y- radiusUnit/2, 0.0,c.x, c.y + radiusUnit/2, 0.0, color,
			//		markerWidth);
			textStr = String.format("DP= %.0f/%.0f",dpwdir.getValue(), dpwspd.getValue());
			target.drawString(font9, textStr, c.x, c.y+10, 0.0,
	                TextStyle.NORMAL, color, HorizontalAlignment.RIGHT,
	                VerticalAlignment.TOP, null);
			
			c = WxMath.uvComp(upwspd.getValue(),upwdir.getValue());
			c= world.map(c);			
			target.drawCircle(c.x, c.y, 0, radiusUnit/2,color, markerWidth);
			//target.drawLine(c.x - radiusUnit/2, c.y, 0.0,c.x + radiusUnit/2, c.y, 0.0, color,
			//		markerWidth);
			//target.drawLine(c.x, c.y- radiusUnit/2, 0.0,c.x, c.y + radiusUnit/2, 0.0, color,
			//		markerWidth);
			textStr = String.format("UP= %.0f/%.0f",upwdir.getValue(), upwspd.getValue());
			target.drawString(font9, textStr, c.x, c.y+10, 0.0,
	                TextStyle.NORMAL, color, HorizontalAlignment.LEFT,
	                VerticalAlignment.BOTTOM, null);
		}
		//plot Bunkers Vector,by default plot them
		if(((configD != null ) && configD.isSmvBunkersR())||(configD == null)){
			FloatByReference bwdir= new FloatByReference(-999);
			FloatByReference bwspd= new FloatByReference(-999);
			nsharpNative.nsharpLib.bunkers_storm_motion(value1, value2, bwdir, bwspd);
			c = WxMath.uvComp(bwspd.getValue(),bwdir.getValue());
			c= world.map(c);
			RGB color = NsharpConstants.color_firebrick;
			target.drawCircle(c.x, c.y, 0, radiusUnit,color, markerWidth);
			target.drawLine(c.x - radiusUnit, c.y, 0.0,c.x + radiusUnit, c.y, 0.0, color,
					markerWidth);
			target.drawLine(c.x, c.y- radiusUnit, 0.0,c.x, c.y + radiusUnit, 0.0, color,
					markerWidth);
			textStr = String.format("%.0f/%.0f RM",bwdir.getValue(), bwspd.getValue());
			target.drawString(font9, textStr, c.x, c.y+10, 0.0,
	                TextStyle.NORMAL, color, HorizontalAlignment.RIGHT,
	                VerticalAlignment.TOP, null);
		}
		if(((configD != null ) && configD.isSmvBunkersL())||(configD == null)){
			FloatByReference bwdir= new FloatByReference(-999);
			FloatByReference bwspd= new FloatByReference(-999);
			nsharpNative.nsharpLib.bunkers_left_motion(value1, value2, bwdir, bwspd);
			c = WxMath.uvComp(bwspd.getValue(),bwdir.getValue());
			c= world.map(c);
			RGB color = NsharpConstants.color_skyblue;
			target.drawCircle(c.x, c.y, 0, radiusUnit,color, markerWidth);
			target.drawLine(c.x - radiusUnit, c.y, 0.0,c.x + radiusUnit, c.y, 0.0, color,
					markerWidth);
			target.drawLine(c.x, c.y- radiusUnit, 0.0,c.x, c.y + radiusUnit, 0.0, color,
					markerWidth);
			textStr = String.format("%.0f/%.0f LM",bwdir.getValue(), bwspd.getValue());
			target.drawString(font9, textStr, c.x, c.y-10, 0.0,
	                TextStyle.NORMAL, color, HorizontalAlignment.LEFT,
	                VerticalAlignment.BOTTOM, null);
		}
		
		//plot current storm motion vector (mouse click) marker				
		target.drawCircle(hodoHouseC.x, hodoHouseC.y, 0, 15,
				NsharpConstants.color_white, markerWidth);
		target.drawLine(hodoHouseC.x - 7.5, hodoHouseC.y, 0.0,hodoHouseC.x + 7.5, hodoHouseC.y, 0.0, NsharpConstants.color_white,
				markerWidth);
		target.drawLine(hodoHouseC.x, hodoHouseC.y- 7.5, 0.0,hodoHouseC.x, hodoHouseC.y + 7.5, 0.0, NsharpConstants.color_white,
				markerWidth);
		textStr = String.format("%.0f/%.0f",smWindDir, smWindSpd);
		target.drawString(font9, textStr, hodoHouseC.x,hodoHouseC.y+15, 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.CENTER,
                VerticalAlignment.TOP, null);

		//plot critical angel
		float ca = nsharpNative.nsharpLib.cave_criticalAngel();
		if(ca != -9999){
			c = WxMath.uvComp(90,45); //90/45 is location picked visually from hodo grapg to draw crtical angel
			c= world.map(c);
			textStr = String.format("Critical Angel = %.0f",ca);
			target.drawString(font9, textStr, c.x, c.y, 0.0,
	                TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
	                VerticalAlignment.BOTTOM, null);
		}
		//draw lines from storm motion to top and bottom of effective layer 
		FloatByReference topPF= new FloatByReference(0);
		FloatByReference botPF= new FloatByReference(0);
    	nsharpNative.nsharpLib.get_effectLayertopBotPres(topPF, botPF);
    	if(botPF.getValue() >=1 ){
    		// interpolate wind direction and speed at top and bottom of effective layer 
    		float bot_spd = nsharpNative.nsharpLib.iwspd(botPF.getValue());
    		float bot_dir = nsharpNative.nsharpLib.iwdir(botPF.getValue());
    		float top_spd = nsharpNative.nsharpLib.iwspd(topPF.getValue());
    		float top_dir = nsharpNative.nsharpLib.iwdir(topPF.getValue());
    		c = WxMath.uvComp(bot_spd,bot_dir); 
    		c= world.map(c);
    		target.drawLine(hodoHouseC.x, hodoHouseC.y, 0.0,c.x, c.y, 0.0, NsharpConstants.color_skyblue,
    				markerWidth);
    		c = WxMath.uvComp(top_spd,top_dir); 
    		c= world.map(c);
    		target.drawLine(hodoHouseC.x, hodoHouseC.y, 0.0,c.x, c.y, 0.0, NsharpConstants.color_skyblue,
    				markerWidth);
    	}
    }
    
    
    @SuppressWarnings("deprecation")
	private void plotNsharpSRWindVectors(IGraphicsTarget target, double zoomLevel,
            WGraphics world, Rectangle rect) throws VizException {
    	/*
    	 * Chin:: NOTE:::
    	 *
    	 * This plotting function is based on the algorithm of plot_storminflow()
    	 *  at xwvid5.c of legacy nsharp source code
    	 * 
    	 * This function is dynamically chnaged when user change "storm relative" 
    	 * (by click on Hodo graph). Also it uses target.drawArc. 
    	 * With these 2 reasons, wire frame shape is not used.
    	 */
    	double dispX, dispY;
    	double vyMax = world.getViewYmax();
    	double vyMin = world.getViewYmin();
    	double vxMax = world.getViewXmax();
    	double vxMin = world.getViewXmin();
    	int width = getCapability(OutlineCapability.class).getOutlineWidth();
    	
        PixelExtent extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        /* ----- Plot Label ----- */
        dispX = vxMin + 30;
    	dispY =  vyMin + 30;
		target.drawString(font9, "SR Wind Vectors", dispX, dispY, 0.0,
                TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
                VerticalAlignment.BOTTOM, null);
    	//plot X axis 
		//xScaleAdjustRatio = (world.getViewXmax()-vxMin)/50;
		dispY =  vyMin + (vyMax - vyMin )/2;
		target.drawLine(vxMin , dispY,  0.0, vxMax,dispY, 0.0, NsharpConstants.color_white,
				width/3F);
		//plot Y axis 
		dispX =  vxMin + (vxMax - vxMin )/2;
		target.drawLine(dispX, vyMax, 0.0, dispX,vyMin, 0.0, NsharpConstants.color_white,
				width/3F);
		
		//plot speed rings
		
		for(int i=40; i<=NsharpConstants.INSET_REC_WIDTH/2; i+=40) {
			target.drawArc(dispX,dispY, 0, i,NsharpConstants.color_stellblue, markerWidth, 0, 360, LineStyle.DASH_DOTTED, true);
		}
		
		FloatByReference smdir= new FloatByReference(0);
		FloatByReference smspd= new FloatByReference(0);
		FloatByReference wdir= new FloatByReference(0); 
		FloatByReference wspd= new FloatByReference(0);
		FloatByReference mnu= new FloatByReference(0);
		FloatByReference mnv= new FloatByReference(0);
		nsharpNative.nsharpLib.get_storm(smspd, smdir);	
		int scaleFactor = 2;
		int gap = 10;
		/* ----- Plot 0-2km Inflow Vector ----- */
		nsharpNative.nsharpLib.sr_wind( -1, 
				nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(2000)), smdir.getValue(), smspd.getValue(), 
				mnu, mnv, wdir, wspd);
		target.drawLine(dispX , dispY,  0.0, dispX + mnu.getValue() * scaleFactor,dispY- mnv.getValue()* scaleFactor, 0.0, NsharpConstants.color_red,
				width*2);
		
		target.drawString(font12, "L", dispX+ mnu.getValue()* scaleFactor, dispY- mnv.getValue()* scaleFactor+2*gap, 0.0,
                TextStyle.NORMAL, NsharpConstants.color_red, HorizontalAlignment.CENTER,
                VerticalAlignment.MIDDLE, null);
		/* ----- Plot 4-6km Inflow Vector ----- */
		nsharpNative.nsharpLib.sr_wind( nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(4000)), 
				nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(6000)), smdir.getValue(), smspd.getValue(), 
				mnu, mnv, wdir, wspd);
		target.drawLine(dispX , dispY,  0.0, dispX + mnu.getValue()* scaleFactor,dispY- mnv.getValue()* scaleFactor, 0.0, NsharpConstants.color_cyan,
				width*2);
		
		target.drawString(font12, "M", dispX+ mnu.getValue()* scaleFactor, dispY- mnv.getValue()* scaleFactor+2*gap, 0.0,
                TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.CENTER,
                VerticalAlignment.MIDDLE, null);
		/* ----- Plot 9-11km Inflow Vector ----- */
		nsharpNative.nsharpLib.sr_wind( nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(9000)), 
				nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(11000)), smdir.getValue(), smspd.getValue(), 
				mnu, mnv, wdir, wspd);
		target.drawLine(dispX , dispY,  0.0, dispX + mnu.getValue()* scaleFactor,dispY- mnv.getValue()* scaleFactor, 0.0, NsharpConstants.color_hotpink,
				width*2);
		
		target.drawString(font12, "H", dispX+ mnu.getValue()* scaleFactor, dispY- mnv.getValue()* scaleFactor+ 2*gap, 0.0,
                TextStyle.NORMAL, NsharpConstants.color_hotpink, HorizontalAlignment.CENTER,
                VerticalAlignment.MIDDLE, null);
		
    }
    @SuppressWarnings("deprecation")
	private void plotNsharpStormSlinky(IGraphicsTarget target, double zoomLevel,
            WGraphics world, Rectangle rect) throws VizException {
    	/*
    	 * Chin:: NOTE:::
    	 *
    	 * This plotting function is based on the algorithm of plot_vis(),visual1(), vis_xy()
    	 *  at xwvid5.c of legacy nsharp source code
    	 * 
    	 * This function is dynamically chnaged when user change "storm relative" 
    	 * (by click on Hodo graph). Also it uses target.drawCircle. 
    	 * With these 2 reasons, wire frame shape is not used.
    	 */
    	double dispX, dispY, dispX1, dispY1;
    	double vyMax = world.getViewYmax();
    	double vyMin = world.getViewYmin();
    	double vxMax = world.getViewXmax();
    	double vxMin = world.getViewXmin();
    	
        PixelExtent extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        /* ----- Plot Label ----- */
        dispX = vxMin + 15;
    	dispY =  vyMax -20;
		target.drawString(font9, "Storm Slinky", dispX, dispY, 0.0,
                TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
                VerticalAlignment.BOTTOM, null);
    	//plot X axis 
		//xScaleAdjustRatio = (world.getViewXmax()-vxMin)/50;
		dispY =  vyMin + (vyMax - vyMin )/2;
		target.drawLine(vxMin , dispY,  0.0, vxMax,dispY, 0.0, NsharpConstants.color_stellblue,
				commonLinewidth/4F);
		//plot Y axis 
		dispX =  vxMin + (vxMax - vxMin )/2;
		target.drawLine(dispX, vyMax, 0.0, dispX,vyMin, 0.0, NsharpConstants.color_stellblue,
				commonLinewidth/4F);
   	
		
		// ----- Plot storm motion - white line -----
		dispX = vxMin + (vxMax - vxMin )/2;
		dispY = vyMin + (vyMax - vyMin )/2;
		FloatByReference smdir=new FloatByReference(0), smspd = new FloatByReference(0);
		nsharpNative.nsharpLib.get_storm(smspd, smdir);
		dispX1 = dispX + nsharpNative.nsharpLib.ucomp( smdir.getValue(), 30.0F) * 2;
		dispY1 = dispY - nsharpNative.nsharpLib.vcomp( smdir.getValue(), 30.0F) * 2;
		target.drawLine(dispX, dispY, 0.0, dispX1,dispY1, 0.0, NsharpConstants.color_white,
				commonLinewidth*2);
		
		/// ----- Calculate Parcel Data ----- 
    	_lplvalues lpvls = new _lplvalues();
		nsharpNative.nsharpLib.get_lpvaluesData(lpvls);	
		float sfctemp, sfcdwpt, sfcpres;
		sfctemp = lpvls.temp;
		sfcdwpt = lpvls.dwpt;
		sfcpres = lpvls.pres;
		// get parcel data by calling native nsharp parcel() API. value is returned in pcl 
	    _parcel pcl = new _parcel();
		nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl);
		/* ----- Run Visualization Routine to plot storm ----- */
		NsharpNative.NsharpLibrary.StormSlinkyStr stmSky = new NsharpNative.NsharpLibrary.StormSlinkyStr();
		//call cave_visual1 to get all points that to be plotted
		nsharpNative.nsharpLib.cave_visual1( pcl.lfcpres, pcl.elpres, sfcpres, sfctemp, sfcdwpt,stmSky);
		//System.out.println(" size = "+ stmSky.getSize()+ " tim ="+ stmSky.getTottim()+ " ang = "+ stmSky.getAngl());
		float tsuv[] = stmSky.getTsuv();
		int colors[] = stmSky.getColor();
		for(int i=0; i < stmSky.getSize(); i++){
			float xfactor, xs, ys;

			xfactor=.015F;// was .01F at legacy code

			xs = (float)dispX + (-tsuv[i*2] * xfactor);
			ys = (float)dispY + (-tsuv[i*2+1] * xfactor);
			RGB color = stormSlinkyColorMap.get(colors[i]);
			
			target.drawCircle(xs, ys, 0, 12,
					color, markerWidth);
		}
		//write ang and tottim
		dispX = vxMin + 15;
    	dispY =  vyMin + 30;
    	String s = (int)stmSky.getTottim()+ " s      " + (int)stmSky.getAngl() + " deg";
    	target.drawString(font11, s, dispX, dispY, 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.BOTTOM, null);
     }
    

        
    public void addSounding(DataTime dataTime, VerticalSounding sounding) {

        this.dataTimes.add(dataTime);
        soundingMap.put(dataTime.getValidTime().getTime(), new SoundingParams(
                sounding));
        Collections.sort(this.dataTimes);
    }

    public void setSoundingMap(Map<DataTime, SoundingParams> map) {
        this.soundingMap = new HashMap<Date, SoundingParams>();
        for (DataTime dt : map.keySet()) {
            soundingMap.put(dt.getValidTime().getTime(), map.get(dt));
        }
        this.dataTimes.clear();
        this.dataTimes.addAll(map.keySet());
        Collections.sort(this.dataTimes);

        if (this.soundingMap == null) {
            this.soundingMap = new HashMap<Date, SoundingParams>();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.ITimeSeqResource#setDisplayedDate
     * (java.util.Date)
     */
    public DataTime getDisplayedDate() {
        return displayedSounding;
    }

    protected SoundingParams retrieveVerticalSounding(DataTime time) {
        if (time == null) {
            return null;
        }
        return soundingMap.get(time.getValidTime().getTime());
    }

    @Override
    public String getName() {
        String name = "NO DATA";
        SoundingParams sp = this.getSoundingParameters();

        if (sp != null) {
            VerticalSounding vs = sp.getInterleavedData();
            name = vs.getName() + " Skewt";
            if (vs.getDataTime().getLegendString() != null) {
                name += " " + vs.getDataTime().getLegendString();
            }
        }

        return name;
    }

    @Override
    public void setDescriptor(NsharpSkewTDescriptor descriptor) {
        super.setDescriptor(descriptor);
        RGB rgb = ColorUtil.getNewColor(descriptor);
        getCapability(ColorableCapability.class).setColor(rgb);
        //System.out.println("screwT Rsc  setDescriptor called");
    }

    public void setSoundingDate(DataTime dt) {
        if (dt == null && this.displayedSounding == null) {
            return;
        }
        if (dt != null && dt.equals(this.displayedSounding)) {
            return;
        }
        this.displayedSounding = dt;
        fireListeners();
    }

    public SoundingParams getSoundingParameters() {
        SoundingParams sp = this
                .retrieveVerticalSounding(this.displayedSounding);
        return sp;
    }
    
    /**
     * Allows you to grab a sounding from a different time
     * 
     * @param dt
     * @return
     */
    public SoundingParams getSoundingByDataTime(DataTime dt) {
        // find a close time to 24 hours, give 4 hour lee way
        SoundingParams sp = this.retrieveVerticalSounding(dt);
        return sp;
    }

 
    @Override
    public String inspect(ReferencedCoordinate rCoord) throws VizException {
        String s = "";
        try {
         } catch (Exception e) {
            UFStatus.getHandler().handle(Priority.PROBLEM,  "Exception translating coordinate", e);
        }

        return s;
    }

    /**
     * Draw an edit handle
     * 
     * @param target
     * @param world
     * @param zoomLevel
     * @param x
     * @param y
     * @param color
     * @throws VizException
     
    private void drawEditHandle(IGraphicsTarget target, WGraphics world,
            double zoomLevel, double x, double y, RGB color)
            throws VizException {
        double radius = NsharpConstants.endpointRadius * zoomLevel;

        target.drawShadedRect(new PixelExtent(x - radius, x + radius, y
                - radius, y + radius), color, 1.0, null);
    }*/

    public void addListener(ISkewTDataChangedListener listener) {
        listenerList.add(listener);
    }

    public void removeListener(ISkewTDataChangedListener listener) {
        listenerList.remove(listener);
    }

    private void fireListeners() {
        for (Object listener : listenerList.getListeners()) {
            ((ISkewTDataChangedListener) listener).skewTResourceChanged(this);
        }
    }

    public static interface ISkewTDataChangedListener {
        public abstract void skewTResourceChanged(NsharpSkewTResource rsc);
    }

	public boolean isPlotInteractiveTemp() {
		return plotInteractiveTemp;
	}


	public void setPlotInteractiveTemp(boolean plotInteractiveTemp) {
		this.plotInteractiveTemp = plotInteractiveTemp;
	}
	public void setInteractiveTempPointCoordinate(
			Coordinate interactiveTempPointCoordinate) {
		//System.out.println("setInteractiveTempPointCoordinate called");
		this.interactiveTempPointCoordinate = interactiveTempPointCoordinate;
		plotInteractiveTemp = true;
		//Chin fixing BUG
		/*
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		
		Coordinate inC = WxMath.reverseSkewTXY(bkRsc.getSkewTBackground().getWorld().unMap(interactiveTempPointCoordinate));
		double inPressure = inC.y;
		
		if(inPressure <= 100) {
			currentSoundingLayerIndex = soundingLys.size()-1;
		}
		else {
			for (NcSoundingLayer layer : this.soundingLys) {
				int pressure =(int) layer.getPressure(); 
				//Note::use int for pressure comparison to avoid nano difference on pressure after
				//unMap() computation.
				//System.out.println("layer index = " +soundingLys.indexOf(layer));
				if(  pressure >= 100 && pressure <=  inPressure){
					currentSoundingLayerIndex = soundingLys.indexOf(layer); 
					break;
				}
			}
		}*/

	}
	public void setInteractiveHodoPointCoordinate(Coordinate c){
		try {
			NcSoundingLayer hodoLayer = soundingLys.get(hodoEditingSoundingLayerIndex);
			if(hodoLayer != null){
				NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
				Coordinate c1 = bkRsc.getHodoBackground().getWorld().unMap(c.x, c.y);
				//System.out.println("picked pt after unmap CX "+ c1.x + " CY "+ c1.y);
				c1 = WxMath.speedDir((float) c1.x, (float) c1.y);
				hodoLayer.setWindSpeed((float)c1.x);
				hodoLayer.setWindDirection((float)c1.y);
				createRscHodoWindShapeAll();
			}
		}
		catch(Exception e)  {
			
		}
	}
	public void applyInteractiveTempPoint(){
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		
		Coordinate inC = WxMath.reverseSkewTXY(bkRsc.getSkewTBackground().getWorld().unMap(interactiveTempPointCoordinate));
		double inTemp = inC.x;
		//System.out.println("applyInteractiveTempPoint called pressure " + inC.y + " temp "+ inTemp +
		//		" currentTempCurveType " + currentTempCurveType + " currentSoundingLayer index="+currentSoundingLayer);
		NcSoundingLayer layer = this.soundingLys.get(currentSoundingLayerIndex);
		if(currentTempCurveType == TEMP_TYPE){
			if(inTemp < layer.getDewpoint())
				// temp can not be lower than dew point
				layer.setTemperature(layer.getDewpoint());
			else
				layer.setTemperature((float)inTemp);
		}
		else{
			if(inTemp > layer.getTemperature())
				// dew point can not be higher than temp
				layer.setDewpoint(layer.getTemperature());
			else
				layer.setDewpoint((float)inTemp);
		}
		
		
		this.dataTimelineSndLysListMap.put(pickedStnInfoStr, this.soundingLys);
		//re-populate snd data to nsharp native code lib for later calculating
		nsharpNative.populateSndgData(soundingLys);
		//get storm motion wind data after populate sounding from NsharpLib
	//  #10438 FloatByReference stwdir= new FloatByReference(-999);
		//FloatByReference stwspd= new FloatByReference(-999);
		//nsharpNative.nsharpLib.get_storm(stwspd, stwdir);
		//bkRsc.setSmDir(stwdir.getValue());
		//bkRsc.setSmSpd(stwspd.getValue());
		
		createRscPressTempCurveShapeAll();	
			
	}
	
	public void applySfcEditing(float tp, float dp, float ws, float wd, float pressure){
		//NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		
		currentSoundingLayerIndex =nsharpNative.nsharpLib.sfc();
		NcSoundingLayer layer = this.soundingLys.get(currentSoundingLayerIndex);
		layer.setTemperature(tp);
		layer.setDewpoint(dp);
		layer.setWindDirection(wd);
		layer.setWindSpeed(ws);
		layer.setPressure(pressure);
		this.dataTimelineSndLysListMap.put(pickedStnInfoStr, this.soundingLys);
		//re-populate snd data to nsharp native code lib for later calculating
		nsharpNative.populateSndgData(soundingLys);
		//get storm motion wind data after populate sounding from NsharpLib
		/*//  #10438 FloatByReference stwdir= new FloatByReference(-999);
		FloatByReference stwspd= new FloatByReference(-999);
		nsharpNative.nsharpLib.get_storm(stwspd, stwdir);
		bkRsc.setSmDir(stwdir.getValue());
		bkRsc.setSmSpd(stwspd.getValue());	*/
	}
	public void updateLayer(int layerIndex, float tp, float dp, float ws, float wd, float pressure){
		if(layerIndex <0 || layerIndex >= soundingLys.size())
			return;
		//NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		
		currentSoundingLayerIndex =layerIndex;
		NcSoundingLayer layer = soundingLys.get(currentSoundingLayerIndex);
		layer.setGeoHeight(nsharpNative.nsharpLib.ihght(pressure));
		layer.setTemperature(tp);
		layer.setDewpoint(dp);
		layer.setWindDirection(wd);
		layer.setWindSpeed(ws);
		layer.setPressure(pressure);
		this.dataTimelineSndLysListMap.put(pickedStnInfoStr, soundingLys);
		//re-populate snd data to nsharp native code lib for later calculating
		Collections.sort(soundingLys,NsharpDataHandling.reversePressureHeightWindComparator());
		nsharpNative.populateSndgData(soundingLys);
		//get storm motion wind data after populate sounding from NsharpLib
	/*/  #10438 FloatByReference stwdir= new FloatByReference(-999);
		FloatByReference stwspd= new FloatByReference(-999);
		nsharpNative.nsharpLib.get_storm(stwspd, stwdir);
		bkRsc.setSmDir(stwdir.getValue());
		bkRsc.setSmSpd(stwspd.getValue());*/
		//refresh test area if it is shown now
		NsharpShowTextDialog textarea =  NsharpShowTextDialog.getAccess();
		if(textarea != null){
			textarea.refreshTextData();
		}
		createRscWireFrameShapes();
	}
	public void addNewLayer(float tp, float dp, float ws, float wd, float pressure){		
		//NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();		
		//currentSoundingLayer =layerIndex;
		NcSoundingLayer layer = new NcSoundingLayer();
		layer.setGeoHeight(nsharpNative.nsharpLib.ihght(pressure));
		layer.setTemperature(tp);
		layer.setDewpoint(dp);
		layer.setWindDirection(wd);
		layer.setWindSpeed(ws);
		layer.setPressure(pressure);
		soundingLys.add(layer);
		this.dataTimelineSndLysListMap.put(pickedStnInfoStr, soundingLys);
		//re-populate snd data to nsharp native code lib for later calculating
		Collections.sort(soundingLys,NsharpDataHandling.reversePressureHeightWindComparator());
		nsharpNative.populateSndgData(soundingLys);
		//get storm motion wind data after populate sounding from NsharpLib
		/* //  #10438 FloatByReference stwdir= new FloatByReference(-999);
		FloatByReference stwspd= new FloatByReference(-999);
		nsharpNative.nsharpLib.get_storm(stwspd, stwdir);
		bkRsc.setSmDir(stwdir.getValue());
		bkRsc.setSmSpd(stwspd.getValue());*/
		//refresh text area if it is shown now
		NsharpShowTextDialog textarea =  NsharpShowTextDialog.getAccess();
		if(textarea != null){
			textarea.refreshTextData();
		}
		createRscWireFrameShapes();
	}
	
	
	private void disposeAllWireFrameShapes(){
		disposeRscWireFrameShapes();
		//also dispose static background shpae
		if(omegaBkgShape!=null)
			omegaBkgShape.dispose();
		
		if(windBoxBkgShape!=null)
			windBoxBkgShape.dispose();
		
		if(hodoWindMotionBoxShape!=null)
			hodoWindMotionBoxShape.dispose();
	}
	private void disposeRscWireFrameShapes(){
		if(heightMarkRscShape!=null)
			heightMarkRscShape.dispose();
		if(omegaRscShape!=null)
			omegaRscShape.dispose();
		if(wetBulbTraceRscShape!=null)
			wetBulbTraceRscShape.dispose();
		if(vtempTraceCurveRscShape!=null)
			vtempTraceCurveRscShape.dispose();
		if(thetaEPressureYRscShape!=null)
			thetaEPressureYRscShape.dispose();
		if(thetaEPressureRRscShape!=null)
			thetaEPressureRRscShape.dispose();
		if(thetaEPressureWRscShape!=null)
			thetaEPressureWRscShape.dispose();
		if(thetaEHeightYRscShape!=null)
			thetaEHeightYRscShape.dispose();
		if(thetaEHeightWRscShape!=null)
			thetaEHeightWRscShape.dispose();
		if(thetaEHeightRRscShape!=null)
			thetaEHeightRRscShape.dispose();
		if(srWindBRscShape!=null)
			srWindBRscShape.dispose();
		if(srWindWRscShape!=null)
			srWindWRscShape.dispose();
		if(srWindRRscShape!=null)
			srWindRRscShape.dispose();
		if(srWindGRscShape!=null)
			srWindGRscShape.dispose();
		if(srWindMRscShape!=null)
			srWindMRscShape.dispose();
		if(psblWatchTypeBkgShape!=null)
			psblWatchTypeBkgShape.dispose();
		if(verticalWindSbShape!=null)
			verticalWindSbShape.dispose();
		if(verticalWindLabelShape!=null)
			verticalWindLabelShape.dispose();
		if(verticalWindRShape!=null)
			verticalWindRShape.dispose();
		if(parcelTraceRscShapeList.size()>0){
			for(IWireframeShape shape: parcelTraceRscShapeList){
				shape.dispose();
			}
			parcelTraceRscShapeList.clear();
			
		}
		if(hodoWindRscShapeList.size()>0){
			for(ShapeAndColor shapeColor: hodoWindRscShapeList){
				shapeColor.shape.dispose();
			}
			hodoWindRscShapeList.clear();
			
		}
		if(pressureTempRscShapeList.size()>0){
			for(ShapeAndColor shapeColor: pressureTempRscShapeList){
				shapeColor.shape.dispose();
			}
			pressureTempRscShapeList.clear();
			
		}
		if(windBoxWindRscShapeList.size()>0){
			for(ShapeAndColor shapeColor: windBoxWindRscShapeList){
				shapeColor.shape.dispose();
			}
			windBoxWindRscShapeList.clear();
			
		}
	}
	private void createRscVTempTraceShape(WGraphics world){
		if((soundingLys == null) || (soundingLys.size()==0))
			return;
    	float t1;
        
     
        Coordinate c2 =  null;
        Coordinate c1;
        // draw trace
        vtempTraceCurveRscShape = target.createWireframeShape(false,descriptor );
        vtempTraceCurveRscShape.allocate(this.soundingLys.size() * 2);
        for (NcSoundingLayer layer : this.soundingLys) {
        	if ((layer.getTemperature() != INVALID_DATA) && (layer.getDewpoint() != INVALID_DATA) && layer.getPressure()>= 100){
        		t1 = nsharpNative.nsharpLib.ivtmp(layer.getPressure());

        		c1 = WxMath.getSkewTXY(layer.getPressure(), t1);
        		c1.x = world.mapX(c1.x);
        		c1.y = world.mapY(c1.y);
        		if(c2!= null){
        			//target.drawLine(c1.x, c1.y, 0.0, c2.x, c2.y, 0.0, color,
        	  		//	lineWidth, LineStyle.DASHED);
        			double [][] lines = {{c1.x, c1.y},{c2.x, c2.y}};
        			vtempTraceCurveRscShape.addLineSegment(lines);
        		}
        		c2 =  c1;
        	}
        }
        vtempTraceCurveRscShape.compile();
	}
	/*
	 * Chin:: NOTE:::
	 * This plotting function is based on the algorithm of plot_advectionprofile() at xwvid5.c of Bignsharp source code
	 * 
	 */
	private void createRscVerticalWindShape(WGraphics world){
        //double origX = NsharpConstants.VERTICAL_WIND_X_ORIG;
        double y1, y2, x1,x2;//,origY;
        //float xRatio =  ((float)NsharpConstants.WIND_BOX_WIDTH) / 140.00F;
        verticalWindLabelShape = target.createWireframeShape(false,descriptor );
        verticalWindLabelShape.allocate(2);
        // add a virtual line to make wire frame shape worked when there is only strings in shape       
    	double [][] lines = {{0, 0},{0,0}}; 
    	verticalWindLabelShape.addLineSegment(lines);
    	double [] lblXy = { NsharpConstants.VERTICAL_WIND_X_ORIG+NsharpConstants.VERTICAL_WIND_WIDTH/2, NsharpConstants.VERTICAL_WIND_Y_ORIG+35};
    	verticalWindLabelShape.addLabel("Inferred Temp", lblXy);
    	double [] lblXy1 = { NsharpConstants.VERTICAL_WIND_X_ORIG+NsharpConstants.VERTICAL_WIND_WIDTH/2, NsharpConstants.VERTICAL_WIND_Y_ORIG+65};
    	verticalWindLabelShape.addLabel("Advection", lblXy1);
    	double [] lblXy2 = { NsharpConstants.VERTICAL_WIND_X_ORIG+NsharpConstants.VERTICAL_WIND_WIDTH/2, NsharpConstants.VERTICAL_WIND_Y_ORIG+95};
    	verticalWindLabelShape.addLabel("(C/hr)", lblXy2);
    	
        verticalWindSbShape = target.createWireframeShape(false,descriptor );
        verticalWindSbShape.allocate(72);
        verticalWindRShape = target.createWireframeShape(false,descriptor );
        verticalWindRShape.allocate(72);
        FloatByReference dummy1= new FloatByReference(-999);
		FloatByReference dummy2= new FloatByReference(-999);
		FloatByReference Surfpressure = new FloatByReference(-999);
		nsharpNative.nsharpLib.get_surface(Surfpressure, dummy1, dummy2);
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();   	
		WGraphics WGc = bkRsc.getSkewTBackground().getWorld();
		if(nsharpNative.nsharpLib.qc(Surfpressure.getValue()) == 1) {
			float advt;
			x1 = NsharpConstants.VERTICAL_WIND_X_ORIG+ (NsharpConstants.VERTICAL_WIND_WIDTH/2);
			for (float pressure=Surfpressure.getValue(); pressure>=200; pressure-=100) {
				y1 = WxMath.getSkewTXY(pressure, 0).y;
				y1=WGc.mapY(y1);
	            y2 = WxMath.getSkewTXY(pressure-100, 0).y;	
	            y2=WGc.mapY(y2);
				advt = nsharpNative.nsharpLib.advection_layer(dummy1, pressure, pressure - 100);
				//System.out.println("advt="+advt);
				if(advt <= NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA)
					continue;
				
				x2 = x1+advt*15;
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
					double [] advtLblXy = { x1+40, y2+ (y1-y2)/2};
					verticalWindRShape.addLabel(advtStr, advtLblXy);
				}
				else {
					verticalWindSbShape.addLineSegment(lines1);
					verticalWindSbShape.addLineSegment(lines2);
					verticalWindSbShape.addLineSegment(lines3);
					verticalWindSbShape.addLineSegment(lines4);
					double [] advtLblXy = { x1-40,y2+ (y1-y2)/2};
					verticalWindSbShape.addLabel(advtStr, advtLblXy);
				}
					
			}
		}
        verticalWindSbShape.compile();
        verticalWindRShape.compile();
        verticalWindLabelShape.compile();
	}
	private void createRscWindBoxWindShape(WGraphics world){
        double windBoxX = NsharpConstants.WIND_BOX_X_ORIG;
        //float lastHeight = -999;
        double windY, windBoxY;
        float xRatio =  ((float)NsharpConstants.WIND_BOX_WIDTH) / 140.00F;
        ShapeAndColor shNcolor = new ShapeAndColor();
        IWireframeShape shapeR = shNcolor.shape = target.createWireframeShape(false,descriptor );
        shapeR.allocate(soundingLys.size()*2);
        shNcolor.color = NsharpConstants.color_red;
        windBoxWindRscShapeList.add(shNcolor);
        shNcolor = new ShapeAndColor();
        IWireframeShape shapeG= shNcolor.shape = target.createWireframeShape(false,descriptor );
        shapeG.allocate(soundingLys.size()*2);
        shNcolor.color = NsharpConstants.color_green;
        windBoxWindRscShapeList.add(shNcolor);
        shNcolor = new ShapeAndColor();
        IWireframeShape shapeY= shNcolor.shape = target.createWireframeShape(false,descriptor );
        shapeY.allocate(soundingLys.size()*2);
        shNcolor.color = NsharpConstants.color_yellow;
        windBoxWindRscShapeList.add(shNcolor);
        shNcolor = new ShapeAndColor();
        IWireframeShape shapeC= shNcolor.shape = target.createWireframeShape(false,descriptor );
        shapeC.allocate(soundingLys.size()*2);
        shNcolor.color = NsharpConstants.color_cyan;
        windBoxWindRscShapeList.add(shNcolor);
        shNcolor = new ShapeAndColor();
        IWireframeShape shapeV = shNcolor.shape = target.createWireframeShape(false,descriptor );
        shapeV.allocate(soundingLys.size()*2);
        shNcolor.color = NsharpConstants.color_violet;
        windBoxWindRscShapeList.add(shNcolor);
        for (NcSoundingLayer layer : soundingLys) {
            float pressure = layer.getPressure();
            float spd = layer.getWindSpeed();
            if ( pressure < 100 || spd < 0 ) {
                continue;
            }
            if(spd > 140)
            	spd = 140;
            //if ((layer.getGeoHeight() - lastHeight) < 400){
            	
            //	continue;
            //}

            // Get the vertical ordinate.
            windY = WxMath.getSkewTXY(pressure, 0).y;
            
            //plot wind speed vs height in the wind box
            windBoxY = world.mapY(windY);
            float geoHt = layer.getGeoHeight();
            double [][] lines = {{windBoxX, windBoxY},{windBoxX + (spd) * xRatio,windBoxY}};
            if(geoHt <3000)
            	shapeR.addLineSegment(lines);
            else if(geoHt < 6000)
            	shapeG.addLineSegment(lines);
            else if(geoHt < 9000)
            	shapeY.addLineSegment(lines);
            else if(geoHt < 12000)
            	shapeC.addLineSegment(lines);
            else
            	shapeV.addLineSegment(lines);
            if(spd > 140)
            	spd = 140;
            
            //lastHeight = layer.getGeoHeight();
        }
        shapeR.compile();
    	shapeG.compile();
    	shapeY.compile();
    	shapeV.compile();
    	shapeC.compile();
	}
	private void createRscHodoWindShape(WGraphics world, List<NcSoundingLayer> soundingLays, RGB incolor){

        Coordinate c0 = null;
        Coordinate c1;
        ShapeAndColor shNcolor;
        IWireframeShape shapeR=null, shapeG=null, shapeY=null, shapeC=null, shapeV=null, shapeIn=null;
		if(incolor == null){
			//creating regular Hodo shape with 5 colors
			shNcolor = new ShapeAndColor();
			shapeR = shNcolor.shape = target.createWireframeShape(false,descriptor );
			shapeR.allocate(soundingLys.size()*2);
	        shNcolor.color = NsharpConstants.color_red;
	        hodoWindRscShapeList.add(shNcolor);
	        shNcolor = new ShapeAndColor();
	        shapeG= shNcolor.shape = target.createWireframeShape(false,descriptor );
	        shapeG.allocate(soundingLys.size()*2);
	        shNcolor.color = NsharpConstants.color_green;
	        hodoWindRscShapeList.add(shNcolor);
	        shNcolor = new ShapeAndColor();
	        shapeY= shNcolor.shape = target.createWireframeShape(false,descriptor );
	        shapeY.allocate(soundingLys.size()*2);
	        shNcolor.color = NsharpConstants.color_yellow;
	        hodoWindRscShapeList.add(shNcolor);
	        shNcolor = new ShapeAndColor();
	        shapeC= shNcolor.shape = target.createWireframeShape(false,descriptor );
	        shapeC.allocate(soundingLys.size()*2);
	        shNcolor.color = NsharpConstants.color_cyan;
	        hodoWindRscShapeList.add(shNcolor);
	        shNcolor = new ShapeAndColor();
	        shapeV = shNcolor.shape = target.createWireframeShape(false,descriptor );
	        shapeV.allocate(soundingLys.size()*2);
	        shNcolor.color = NsharpConstants.color_violet;
	        hodoWindRscShapeList.add(shNcolor);
		}
		else{
			shNcolor = new ShapeAndColor();
			shapeIn = shNcolor.shape = target.createWireframeShape(false,descriptor );
	        shapeIn.allocate(soundingLys.size()*2);
			shNcolor.color = incolor;
	        hodoWindRscShapeList.add(shNcolor);
		}
		
        
        for (NcSoundingLayer layer : soundingLays){
        	if(layer.getPressure() < 100 || layer.getWindSpeed() <0)
        		continue;
            float wspd = layer.getWindSpeed();
            float wdir = layer.getWindDirection();
             c1 = WxMath.uvComp(wspd, wdir);
            if (c0 != null) {
            	double [][] lines = {{world.mapX(c0.x), world.mapY(c0.y)},{world
                    .mapX(c1.x), world.mapY(c1.y)}};
            	if(incolor == null){
            		if(layer.getGeoHeight() <nsharpNative.nsharpLib.msl(3000)){
            			//red
            			shapeR.addLineSegment(lines);
            		}
                	else if(layer.getGeoHeight() < nsharpNative.nsharpLib.msl(6000))
                		shapeG.addLineSegment(lines);
                	else if(layer.getGeoHeight() < nsharpNative.nsharpLib.msl(9000))
                		shapeY.addLineSegment(lines);
                	else if(layer.getGeoHeight() < nsharpNative.nsharpLib.msl(12000))
                		shapeC.addLineSegment(lines);
                	else
                		shapeV.addLineSegment(lines);            	
            	}
            	else
            		shapeIn.addLineSegment(lines);
            }
            
            c0 = c1;
        }
        if(incolor == null){
        	shapeR.compile();
        	shapeG.compile();
        	shapeY.compile();
        	shapeV.compile();
        	shapeC.compile();
        }
        else
        	shapeIn.compile();

	}
	public void createRscPressTempCurveShapeAll(){
		if(pressureTempRscShapeList.size()>0){
			for(ShapeAndColor shapeColor: pressureTempRscShapeList){
				shapeColor.shape.dispose();
			}
			pressureTempRscShapeList.clear();
		}
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource(); 
		WGraphics WGc=  bkRsc.getSkewTBackground().getWorld();
		if(!compareIsOn){
			createRscPressTempCurveShape(WGc, this.soundingLys, null);
			if(overlayIsOn == true && this.previousSoundingLys!=null)
				createRscPressTempCurveShape(WGc, this.previousSoundingLys, NsharpConstants.color_violet);
		}
		else{
			int colorIndex =1;
			for(ElementStateProperty elm: dataTimelineList) {
	    		
	    		List<NcSoundingLayer> soundingLayeys = dataTimelineSndLysListMap.get(elm.elementDescription);
	    		RGB color=null;
				if(elm.elementState == State.PICKED ){
					color = NsharpConstants.COLOR_ARRAY[0];
				} else if(elm.elementState == State.GROUPED){
					color = NsharpConstants.COLOR_ARRAY[colorIndex];
					//System.out.println("COLORINDEX "+colorIndex);
				} 
				colorIndex++; //always increase index, no matter ploting this elm or not
				if(colorIndex > NsharpConstants.COLOR_ARRAY.length-1)
					colorIndex =1;
				if(elm.elementState != State.PICKED && elm.elementState != State.GROUPED)	
					continue;
				createRscPressTempCurveShape(WGc, soundingLayeys, color);
				
			}
		}
	}
	
	private void createRscHodoWindShapeAll(){
		if(hodoWindRscShapeList.size()>0){
			for(ShapeAndColor shapeColor: hodoWindRscShapeList){
				shapeColor.shape.dispose();
			}
			hodoWindRscShapeList.clear();
		}
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource(); 
		WGraphics WGc=  bkRsc.getHodoBackground().getWorld();
		if(!compareIsOn){
			createRscHodoWindShape(WGc, this.soundingLys, null);
			if(overlayIsOn == true && this.previousSoundingLys!=null)
				createRscHodoWindShape(WGc, this.previousSoundingLys, NsharpConstants.color_violet);
		}
		else{
			int colorIndex =1;
			for(ElementStateProperty elm: dataTimelineList) {
	    		
	    		List<NcSoundingLayer> soundingLayeys = dataTimelineSndLysListMap.get(elm.elementDescription);
	    		RGB color=null;
				if(elm.elementState == State.PICKED ){
					color = NsharpConstants.COLOR_ARRAY[0];
				} else if(elm.elementState == State.GROUPED){
					color = NsharpConstants.COLOR_ARRAY[colorIndex];
					//System.out.println("COLORINDEX "+colorIndex);
				} 
				colorIndex++; //always increase index, no matter ploting this elm or not
				if(colorIndex > NsharpConstants.COLOR_ARRAY.length-1)
					colorIndex =1;
				if(elm.elementState != State.PICKED && elm.elementState != State.GROUPED)	
					continue;
				createRscHodoWindShape(WGc, soundingLayeys, color);
				
			}
		}
	}
	private void createRscParcelTraceShape(WGraphics world, short parcelType, float userPre){
		//System.out.println("createRscParcelTraceShape called defoine_parcel pType="+parcelType+" pre="+ userPre);
		IWireframeShape shape = target.createWireframeShape(false,descriptor );
		shape.allocate(40);
		//call native define_parcel() with parcel type and user defined pressure (if user defined it)
		nsharpNative.nsharpLib.define_parcel(parcelType,userPre);

		_lplvalues lpvls = new _lplvalues();
		nsharpNative.nsharpLib.get_lpvaluesData(lpvls);

		float sfctemp, sfcdwpt, sfcpres;
		sfctemp = lpvls.temp;
		sfcdwpt = lpvls.dwpt;
		sfcpres = lpvls.pres;

		float vtemp = nsharpNative.nsharpLib.virtemp (sfcpres, sfctemp, sfcdwpt);
		Coordinate c1 = WxMath.getSkewTXY(sfcpres, vtemp);
		c1.x = world.mapX(c1.x);
		c1.y = world.mapY(c1.y);
		FloatByReference p2 = new FloatByReference(0), t2 = new FloatByReference(0);;
		nsharpNative.nsharpLib.drylift (sfcpres, sfctemp, sfcdwpt, p2, t2);
		vtemp = nsharpNative.nsharpLib.virtemp (p2.getValue(), t2.getValue(), t2.getValue());
		Coordinate c2 = WxMath.getSkewTXY(p2.getValue(), vtemp);
		c2.x = world.mapX(c2.x);
		c2.y = world.mapY(c2.y);

		double [][] lines = {{c1.x, c1.y},{c2.x, c2.y}};
		shape.addLineSegment(lines);

		c1 = c2;


		float t3;
		for (float i = p2.getValue() - 50; i >= 100; i = i - 50)
		{
			t3 = nsharpNative.nsharpLib.wetlift (p2.getValue(), t2.getValue(), i);
			vtemp = nsharpNative.nsharpLib.virtemp (i, t3, t3);
			c2 = WxMath.getSkewTXY(i, vtemp);
			c2.x = world.mapX(c2.x);
			c2.y = world.mapY(c2.y);

			double [][] lines1 = {{c1.x, c1.y},{c2.x, c2.y}};
			shape.addLineSegment(lines1);

			c1 = c2;
		}

		t3 = nsharpNative.nsharpLib.wetlift (p2.getValue(), t2.getValue(), 100);
		vtemp = nsharpNative.nsharpLib.virtemp (100, t3, t3);
		c2 = WxMath.getSkewTXY(100, vtemp);
		c2.x = world.mapX(c2.x);
		c2.y = world.mapY(c2.y);

		double [][] lines2 = {{c1.x, c1.y},{c2.x, c2.y}};
		shape.addLineSegment(lines2);

		shape.compile();

		parcelTraceRscShapeList.add(shape);

	}
	private void createBkgPsblWatchShape(WGraphics world){
		String wwtypeStr;
		//System.out.println("createBkgPsblWatchShape called");
		
    	double dispX, dispY;
    	//int width = getCapability(OutlineCapability.class).getOutlineWidth();
        /* ----- Plot Label ----- */
        dispX = world.getViewXmin() + 130;
    	dispY =  world.getViewYmin() + 30;
    	if(psblWatchTypeBkgShape!=null){
			psblWatchTypeBkgShape.dispose();
			psblWatchTypeBkgShape = null;
    	}
    	psblWatchTypeBkgShape = target.createWireframeShape(false,descriptor );
    	psblWatchTypeBkgShape.allocate(4);
    	double [] lblXy = { dispX, dispY};
		psblWatchTypeBkgShape.addLabel("Psbl Watch Type", lblXy);
        double [] lblXy1 = {dispX, dispY+150};
        if(soundingLys == null){
			wwtypeStr = "NONE";
			wwTypeColor = NsharpConstants.color_gold;
		}
        else {
        	int wwtype = nsharpNative.nsharpLib.cave_ww_type();
        	
        	//System.out.println("ww type="+ wwtype);
        	//See nsharpNative.nsharpLib.cave_ww_type() for returned wwtype definitions
        	switch(wwtype){
        	case 1: 
        		wwtypeStr = "MRGL SVR";
        		wwTypeColor = NsharpConstants.color_skyblue;
        		break;
        	case 2: 
        		wwtypeStr = "SVR";
        		wwTypeColor = NsharpConstants.color_cyan;
        		break;
        	case 3: 
        		wwtypeStr = "MRGL TOR";
        		wwTypeColor = NsharpConstants.color_red;
        		break;
        	case 4: 
        		wwtypeStr = "TOR";
        		wwTypeColor = NsharpConstants.color_red;
        		break;
        	case 5: 
        		wwtypeStr = "PDS TOR";
        		wwTypeColor = NsharpConstants.color_magenta;
        		break;
        	default: 
        		wwtypeStr = "NONE";
        		wwTypeColor = NsharpConstants.color_gold;
        		break;
        	}
        }
        psblWatchTypeBkgShape.addLabel(wwtypeStr, lblXy1);
		double [][] lines = {{world.getViewXmin(), dispY+10},{world.getViewXmax(), dispY+10}};
		psblWatchTypeBkgShape.addLineSegment(lines);
		psblWatchTypeBkgShape.compile();
	}
	private void createHodoWindMotionBoxShape(/*WGraphics world*/){
		hodoWindMotionBoxShape = target.createWireframeShape(false,descriptor );
		hodoWindMotionBoxShape.allocate(12);
		double xOri = NsharpConstants.WIND_MOTION_REC_X_ORIG;
    	double yOri = NsharpConstants.WIND_MOTION_REC_Y_ORIG;
    	double xEnd= NsharpConstants.WIND_MOTION_REC_X_ORIG + NsharpConstants.WIND_MOTION_REC_WIDTH;
    	double yEnd = NsharpConstants.WIND_MOTION_REC_Y_ORIG + NsharpConstants.WIND_MOTION_REC_HEIGHT;
    	double ygap = NsharpConstants.WIND_MOTION_REC_HEIGHT/3;
    	double [][] lines1 = {{xOri, yOri},{xEnd, yOri}};
    	hodoWindMotionBoxShape.addLineSegment(lines1);
    	double [][] lines2 = {{xOri, yOri+ygap},{xEnd, yOri+ygap}};
    	hodoWindMotionBoxShape.addLineSegment(lines2);
    	double [][] lines3 = {{xOri, yOri+2*ygap},{xEnd, yOri+2*ygap}};
    	hodoWindMotionBoxShape.addLineSegment(lines3);
    	double [][] lines4 = {{xOri, yEnd},{xEnd, yEnd}};
    	hodoWindMotionBoxShape.addLineSegment(lines4);
    	double [][] lines5 = {{xOri, yOri},{xOri, yEnd}};
    	hodoWindMotionBoxShape.addLineSegment(lines5);
    	double [][] lines6 = {{xEnd, yOri},{xEnd, yEnd}};
    	hodoWindMotionBoxShape.addLineSegment(lines6);
	}
	private void createBkgWindBoxShape(WGraphics world){
		windBoxBkgShape = target.createWireframeShape(false,descriptor );
		windBoxBkgShape.allocate(30);
		double xOri = NsharpConstants.WIND_BOX_X_ORIG;
    	double yOri = NsharpConstants.WIND_BOX_Y_ORIG;
        int tickLength = 5;
        double [][] lines = {{xOri - tickLength, yOri},{xOri+NsharpConstants.WIND_BOX_WIDTH, yOri}};
        windBoxBkgShape.addLineSegment(lines);
        double [][] lines1 = {{xOri - tickLength, yOri + NsharpConstants.WIND_BOX_HEIGHT},{xOri+NsharpConstants.WIND_BOX_WIDTH, yOri+ NsharpConstants.WIND_BOX_HEIGHT}};
        windBoxBkgShape.addLineSegment(lines1);
        double [] lblXy = {NsharpConstants.WIND_BOX_X_ORIG+130, yOri+35};
        windBoxBkgShape.addLabel("wind(kt) ", lblXy);
        double [] lblXy1 = {NsharpConstants.WIND_BOX_X_ORIG+130, yOri+65};
        windBoxBkgShape.addLabel("vs Ht ", lblXy1);
        double y0 = 0;
 
                            
        //draw lowest layer's height in meter and feet,  line
         y0 = world.mapY(WxMath.getSkewTXY(1050, 0).y);
        
 
        // draw wind speed vs height box
        double xtemp;
        for (int i = 20; i < 140 ; i= i+20){
        	xtemp = xOri + (NsharpConstants.WIND_BOX_WIDTH/7) * (i /20); 
        	double [][] lines2 = {{xtemp, yOri},{xtemp, yOri+NsharpConstants.WIND_BOX_HEIGHT}};
            windBoxBkgShape.addLineSegment(lines2);
            if( (i==20) || (i==60) || (i==100)){
            	double [] lblXy2 = {xtemp, y0+ 20};
            	windBoxBkgShape.addLabel(String.format("%d",i), lblXy2);
            }
            
        }
        windBoxBkgShape.compile();
	}
	private void createBkgOmegaShape(){
		double xAxisOrigin=NsharpConstants.OMEGA_X_TOP, yTop=NsharpConstants.OMEGA_Y_TOP, yBot=NsharpConstants.OMEGA_Y_BOT;
        //draw label and vertical lines
		omegaBkgShape = target.createWireframeShape(false,descriptor );
		omegaBkgShape.allocate(6);
		//Note: at X-axis, 40 units on skewT map will equal to 10 units of Omega length
        //we dont really care about temp, as we use pressure for Y axis. 
		// For X-axis, we will convert it propotionally.
        //left dash line, +10 omega line
        double [][] lines = {{xAxisOrigin-40, yTop},{xAxisOrigin-40, yBot}};
        omegaBkgShape.addLineSegment(lines);
        //center line
        double [][] lines1 = {{xAxisOrigin, yTop},{xAxisOrigin, yBot}};
        omegaBkgShape.addLineSegment(lines1);
        //right dash line, -10 omega line
        double [][] lines2 = {{xAxisOrigin+40, yTop},{xAxisOrigin+40, yBot}};
        omegaBkgShape.addLineSegment(lines2);
        double [] lblXy = {xAxisOrigin, yTop-40};
        omegaBkgShape.addLabel("OMEGA", lblXy);
		double [] lblXy1 = {xAxisOrigin-55, yTop-5};
		omegaBkgShape.addLabel("+10", lblXy1);
		double [] lblXy2 = {xAxisOrigin+40, yTop-5};
		omegaBkgShape.addLabel("-10", lblXy2);
		omegaBkgShape.compile();

	}
	private void createRscSrWindShape(WGraphics world){
    	/*
    	 * Chin:: NOTE:::
    	 *
    	 * This plotting function is based on the algorithm of plot_vertsrw() at xwvid5.c of native nsharp source code
    	 * 
    	 */
    	double dispX, dispY, dispX1, dispY1;
    	double xScaleAdjustRatio, yScaleAdjustRatio;
    	double  bothgt,tophgt;
    	double vyMax = world.getViewYmax();
    	double vyMin = world.getViewYmin();
    	double vxMax = world.getViewXmax();
    	double vxMin = world.getViewXmin();
    	/* ----- Set Layer (AGL) ----- */
    	bothgt = 0;
    	tophgt = 16000;
    	if(srWindBRscShape!=null)
			srWindBRscShape.dispose();
		if(srWindWRscShape!=null)
			srWindWRscShape.dispose();
		if(srWindRRscShape!=null)
			srWindRRscShape.dispose();
		if(srWindGRscShape!=null)
			srWindGRscShape.dispose();
		if(srWindMRscShape!=null)
			srWindMRscShape.dispose();
    	srWindBRscShape = target.createWireframeShape(false,descriptor );    	
    	srWindBRscShape.allocate(2);
    	srWindRRscShape = target.createWireframeShape(false,descriptor );
    	srWindRRscShape.allocate(250);
    	srWindWRscShape= target.createWireframeShape(false,descriptor );
    	srWindWRscShape.allocate(250);
    	srWindGRscShape = target.createWireframeShape(false,descriptor );
    	srWindGRscShape.allocate(2);
    	srWindMRscShape= target.createWireframeShape(false,descriptor );
    	srWindMRscShape.allocate(250);
    	/* ----- Plot Label ----- */
        dispX = vxMin + 50;
    	dispY =  vyMin+30;
		double [] lblXy = { vxMin + (vxMax-vxMin)/2, dispY};
		srWindWRscShape.addLabel("SR Winds (kt) vs Height", lblXy);

    	/* ----- Plot height legend ----- */
		// 2 = 2000 m
		int yNum = 0;
		for(double h=0; h<=vyMax-vyMin; h += (vyMax-vyMin)/8)
		{
			dispY =  vyMax - h;
			dispX =  vxMin;
			double [][] lines = {{dispX, dispY},{dispX+20,dispY}};
			srWindWRscShape.addLineSegment(lines);
			if(h> 0 && h<vyMax-vyMin)
			{
				double [] lblXy1 = { dispX+20, dispY};
				srWindWRscShape.addLabel(String.valueOf(yNum), lblXy1);
			}
			yNum = yNum +2;
		}

		/* ----- Plot horizontal legend ----- */
		int k=0;
		for(float h=0; h<= vxMax-vxMin; h += (vxMax-vxMin)/8)
		{
			//each segment is scale of 10Kt in BigNsharp
			//therefore, totoal x axis is 80 kt
			dispY =  vyMax;
			dispX =  vxMin + h;
			double [][] lines = {{dispX, dispY},{dispX,dispY-20}};
			srWindWRscShape.addLineSegment(lines);
			if(h>0 && h <vxMax-vxMin){
				double [] lblXy1 = { dispX, dispY-20};
				k=k+10;
				srWindWRscShape.addLabel(String.valueOf(k), lblXy1);
			}
		}
	    /* ----- Plot vertical dashed line at 15kt ----- */
		xScaleAdjustRatio = (vxMax-vxMin)/80;
		//dispY =  vyMax;
		{
			dispX =  vxMin + (xScaleAdjustRatio * 15);
			for(int i=0; i < (vyMax-vyMin); i=i+10){
				double [][] lines1 = {{dispX, vyMin+ i},{dispX, vyMin+ i+5}};
				srWindWRscShape.addLineSegment(lines1);
			}
		}
        /* ----- Plot 1/2 vertical dashed line at 40kt ----- */
		{
			dispX =  vxMin + (xScaleAdjustRatio * 40);
			for(int i=0; i < (vyMax-vyMin)/2; i=i+10){
				double [][] lines1 = {{dispX, vyMin+ i},{dispX, vyMin+ i+5}};
				srWindMRscShape.addLineSegment(lines1);
			}
			
		}
		/* ----- Plot 1/2 vertical dashed line at 70kt ----- */
		{
			dispX =  vxMin + (xScaleAdjustRatio * 70);
			for(int i=0; i < (vyMax-vyMin)/2; i=i+10){
				double [][] lines1 = {{dispX, vyMin+ i},{dispX, vyMin+ i+5}};
				srWindMRscShape.addLineSegment(lines1);
			}
			
		}
		/* ----- Plot vertical srw profile ----- */
		dispX1 = dispY1 = -999;
		//NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		
		//Coordinate c = bkRsc.getHodoBackground().getWorld().unMap(hodoHouseC.x, hodoHouseC.y);
		//c = WxMath.speedDir((float) c.x, (float) c.y);
		FloatByReference smdirf= new FloatByReference(0);
		FloatByReference smspdf= new FloatByReference(0);
		nsharpNative.nsharpLib.get_storm(smspdf, smdirf);	
		float smdir = (float) smdirf.getValue();
		float smspd = (float)smspdf.getValue();
		//System.out.println("Rel Wind Spd " + smspd + " dir " + smdir);
		yScaleAdjustRatio = (vyMax-vyMin)/tophgt;
		FloatByReference mnu= new FloatByReference(0);
		FloatByReference mnv= new FloatByReference(0);
		FloatByReference wdir= new FloatByReference(0);
		FloatByReference wspd= new FloatByReference(0);
		
		for(float h= (float)bothgt; h<= tophgt; h += 250){
			nsharpNative.nsharpLib.sr_wind(nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h)), 
					nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h)),
					smdir, smspd, mnu, mnv, wdir, wspd);
			dispX = vxMin + wspd.getValue() * xScaleAdjustRatio;
			dispY = vyMax - (yScaleAdjustRatio * h);
			if(dispX1 == -999) {
    			dispX1 = dispX;
    			dispY1 = dispY;
    		}
    		double [][] lines1 = {{dispX, dispY},{dispX1,dispY1}};
            srWindRRscShape.addLineSegment(lines1);
    		dispX1 = dispX;
			dispY1 = dispY;
		}
		/* ----- Plot Mean-Layer SRW value (Sfc-2km) ----- */
		float h1 = 0.0F;
		float h2 = 2000.0F;
		nsharpNative.nsharpLib.sr_wind(nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h1)), 
				nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h2)),
				smdir, smspd, mnu, mnv, wdir, wspd);
		if(wspd.getValue() != INVALID_DATA)
		{
			dispX = vxMin + wspd.getValue() * xScaleAdjustRatio;
			dispY = vyMax - (yScaleAdjustRatio * h1);
			dispY1 = vyMax - (yScaleAdjustRatio * h2);
			//target.drawLine(dispX, dispY, 0.0, dispX,dispY1, 0.0, NsharpConstants.color_green,
           	//		commonLinewidth*2, commonLineStyle);
			double [][] lines1 = {{dispX, dispY},{dispX,dispY1}};
			srWindGRscShape.addLineSegment(lines1);
		}
		/* ----- Plot Mean-Layer SRW value (4-6km) ----- */
		h1 = 4000.0F;
		h2 = 6000.0F;
		nsharpNative.nsharpLib.sr_wind(nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h1)), 
				nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h2)),
				smdir, smspd, mnu, mnv, wdir, wspd);
		if(wspd.getValue() != INVALID_DATA)
		{
			dispX = vxMin + wspd.getValue() * xScaleAdjustRatio;
			dispY = vyMax - (yScaleAdjustRatio * h1);
			dispY1 = vyMax - (yScaleAdjustRatio * h2);
			double [][] lines1 = {{dispX, dispY},{dispX,dispY1}};
			srWindBRscShape.addLineSegment(lines1);
		}
		/* ----- Plot Mean-Layer SRW value (9-11km) ----- */
		h1 = 9000.0F;
		h2 = 11000.0F;
		nsharpNative.nsharpLib.sr_wind(nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h1)), 
				nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h2)),
				smdir, smspd, mnu, mnv, wdir, wspd);
		if(wspd.getValue() != INVALID_DATA)
		{
			dispX = vxMin + wspd.getValue() * xScaleAdjustRatio;
			dispY = vyMax - (yScaleAdjustRatio * h1);
			dispY1 = vyMax - (yScaleAdjustRatio * h2);
			//target.drawLine(dispX, dispY, 0.0, dispX,dispY1, 0.0, NsharpConstants.color_violet,
           	//		commonLinewidth*2, commonLineStyle);
			double [][] lines1 = {{dispX, dispY},{dispX,dispY1}};
			srWindMRscShape.addLineSegment(lines1);
		}
		srWindBRscShape.compile();
        srWindWRscShape.compile();
        srWindRRscShape.compile();
        srWindGRscShape.compile();
        srWindMRscShape.compile();
       
	}
	private void createRscThetaEHeightShape(WGraphics world){
    	/*
    	 * Chin:: NOTE:::
    	 * This plotting function is based on the algorithm of plot_thetae() at xwvid5.c of native nsharp source code
    	 * 
    	 */
		thetaEHeightYRscShape = target.createWireframeShape(false,descriptor );
		thetaEHeightYRscShape.allocate(2);
		thetaEHeightWRscShape = target.createWireframeShape(false,descriptor );
		thetaEHeightWRscShape.allocate(100);
		thetaEHeightRRscShape = target.createWireframeShape(false,descriptor );
		thetaEHeightRRscShape.allocate(soundingLys.size() * 2);
		double dispX, dispY, dispX1, dispY1;
    	double xAxisCenter, xScaleAdjustRatio, thetaEDispWidth;
    	double yScaleAdjustRatio;
    	float t700, t850, d700, d850, ct1, ct2, ct3, cthe, nct, tophgt;
    	double vyMax = world.getViewYmax();
    	double vyMin = world.getViewYmin();
    	double vxMax = world.getViewXmax();
    	double vxMin = world.getViewXmin();
    	// add a virtual line to make wire fram shape works when only string in shape       
    	double [][] lines = {{0, 0},{0,0}}; 
    	thetaEHeightYRscShape.addLineSegment(lines);
          	
        /* ----- Set Layer (AGL) ----- */
        // set max height to at pressure 500
    	tophgt = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(500.0F));
        if(nsharpNative.nsharpLib.qc(tophgt) == 0) 
        	tophgt = 5500.0F;
        yScaleAdjustRatio = (vyMax-vyMin)/tophgt;
        //----- Plot Label ----- 
        dispX = vxMin + 50;
    	dispY =  vyMin+30;
		double [] lblXy = { dispX+80, dispY};
		thetaEHeightYRscShape.addLabel("Theta-E vs Height", lblXy);
		// ----- Plot horizontal legend ----- 
		
		FloatByReference surfpressure= new FloatByReference(0);
		FloatByReference surfTemp= new FloatByReference(0);
		FloatByReference surfDewpt= new FloatByReference(0);
		t700 = nsharpNative.nsharpLib.itemp(700.0F);
		if (nsharpNative.nsharpLib.qc(t700) == 0)
		     return;
		t850 = nsharpNative.nsharpLib.itemp(850.0F);
		if (nsharpNative.nsharpLib.qc(t850) == 0)
		     return;
		d700 = nsharpNative.nsharpLib.idwpt(700.0F);
		if (nsharpNative.nsharpLib.qc(d700) == 0)
		     return;
		d850 = nsharpNative.nsharpLib.idwpt(850.0F);
		if (nsharpNative.nsharpLib.qc(d850) == 0)
		     return;
        ct1 = nsharpNative.nsharpLib.thetae( 850.0F, t850, d850);
        ct2 = nsharpNative.nsharpLib.thetae( 700.0F, t700, d700);
        nsharpNative.nsharpLib.get_surface(surfpressure, surfTemp, surfDewpt);		
        ct3 = nsharpNative.nsharpLib.thetae( surfpressure.getValue(), surfTemp.getValue(), surfDewpt.getValue() );
        cthe = 0.0F; nct = 0.0F;
        if(nsharpNative.nsharpLib.qc(ct1) != 0)
        {
        	cthe = cthe + ct1;
        	nct++;
        }
        if(nsharpNative.nsharpLib.qc(ct2) != 0)
        {
        	cthe = cthe + ct2;
        	nct++;
        }
        if(nsharpNative.nsharpLib.qc(ct3)!= 0)
        {
        	cthe = cthe + ct3;
        	nct++;
        }
        if(nct < 1.0F) return;
        cthe = cthe / nct;
       //Adjust Theta-E axis scale with real view X scale
        thetaEDispWidth = 60;
        xAxisCenter = ((vxMax-vxMin ) / 2 ) + vxMin;
        xScaleAdjustRatio = (vxMax-vxMin)/thetaEDispWidth;
        for(double h= -(thetaEDispWidth/2); h<=(thetaEDispWidth/2); h += 10.0F)
        {
        	int iThetaE = (int)(h+cthe); //cthe sits in the middle of x-axis
        	dispX = xAxisCenter + (h* xScaleAdjustRatio);
        	dispY =  vyMax; 
           	double [][] lines1 = {{dispX, dispY},{dispX,dispY-20}};
            thetaEHeightWRscShape.addLineSegment(lines1);
           	
           	if((h != -(thetaEDispWidth/2))&&(h != (thetaEDispWidth/2)) ){
           		//draw scale with kelvin degree
          		double [] lblXy1 = { dispX, dispY-20};
        		thetaEHeightYRscShape.addLabel(String.valueOf(iThetaE+273), lblXy1);
           	}
        
        }

        // ----- Plot vertical theta-e profile -----     
        //use soundingLys directly
        float thetaE;
        dispX1 = dispY1 = -999;
        for( NcSoundingLayer lys:soundingLys)
        {	//plot only at pressure greater than or equal to 500
        	if((lys.getPressure() >= 500) && (nsharpNative.nsharpLib.qc(lys.getDewpoint()) !=0 )){
        		thetaE = nsharpNative.nsharpLib.thetae( lys.getPressure(), lys.getTemperature(),lys.getDewpoint())-cthe;
        		dispX = xAxisCenter + ( thetaE * xScaleAdjustRatio);
        		dispY =  vyMax - (nsharpNative.nsharpLib.agl(lys.getGeoHeight()) * yScaleAdjustRatio) ;
        		
        		if(dispX1 == -999) {
        			dispX1 = dispX;
        			dispY1 = dispY;
        		}
        		double [][] lines1 = {{dispX, dispY},{dispX1,dispY1}};
                thetaEHeightRRscShape.addLineSegment(lines1);
        		dispX1 = dispX;
    			dispY1 = dispY;
    			
        	}
        }
    	/* ----- Plot Vertical Legend ----- */
        dispX = vxMin;
    	for(float pres=1000; pres > 500; pres -= 100)
    	{
    		int iPres;
    		dispY =  vyMax - (nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pres)) * yScaleAdjustRatio) ;
    		double [][] lines1 = {{dispX, dispY},{dispX+20,dispY}};
            thetaEHeightWRscShape.addLineSegment(lines1);
    		iPres = (((int)nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pres)))/100)*100;
    		double [] lblXy1 = { dispX+30, dispY};
    		thetaEHeightYRscShape.addLabel(String.valueOf(iPres), lblXy1);
    	}
		thetaEHeightYRscShape.compile();
        thetaEHeightWRscShape.compile();
        thetaEHeightRRscShape.compile();
 
	}
	private void createRscThetaEPressureShape(WGraphics world){
		/*
    	 * Chin:: NOTE:::
    	 * This plotting function is based on the algorithm of plot_thetae() at xwvid5.c of native nsharp source code
    	 * 
    	 */
		thetaEPressureYRscShape = target.createWireframeShape(false,descriptor );
		thetaEPressureYRscShape.allocate(2);
		thetaEPressureRRscShape = target.createWireframeShape(false,descriptor );
		thetaEPressureRRscShape.allocate(soundingLys.size() * 2);
		thetaEPressureWRscShape = target.createWireframeShape(false,descriptor );
		thetaEPressureWRscShape.allocate(100);
		double dispX, dispY, dispX1, dispY1;
    	double xAxisCenter, xScaleAdjustRatio, thetaEDispWidth;
    	double yScaleAdjustRatio;
    	float t700, t850, d700, d850, ct1, ct2, ct3, cthe, nct;
    	double vyMax = world.getViewYmax();
    	double vyMin = world.getViewYmin();
    	double vxMax = world.getViewXmax();
    	double vxMin = world.getViewXmin();
    	
    	// add a virtual line to make wire fram shape worked when only strings are added to shape       
    	double [][] lines = {{0, 0},{0,0}}; 
    	thetaEPressureYRscShape.addLineSegment(lines);   	
        /* ----- Set Layer (AGL) ----- */
        
        //----- Plot Label ----- 
        dispX = vxMin + 20;
    	dispY =  vyMin+30;
    	
		//target.drawString(font9, "Theta-E vs Pressure", dispX, dispY, 0.0,
         //       TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
        //        VerticalAlignment.BOTTOM, null);
		double [] lblXy = { dispX+90, dispY};
		thetaEPressureYRscShape.addLabel("Theta-E vs Pressure", lblXy);
		
		//plot theta E difference
		double [] lblXy2= {dispX+150,dispY+100};
		FloatByReference tempF= new FloatByReference(0);
		@SuppressWarnings("deprecation")
		float thetaEDiff= nsharpNative.nsharpLib.ThetaE_diff(tempF);
		if(nsharpNative.nsharpLib.qc(thetaEDiff) == 1){
			String thetaDiffStr = String.format("TEI = %.0f", thetaEDiff);
			thetaEPressureYRscShape.addLabel(thetaDiffStr, lblXy2);
		}
		// ----- Plot horizontal legend ----- 
		
		FloatByReference surfpressure= new FloatByReference(0);
		FloatByReference surfTemp= new FloatByReference(0);
		FloatByReference surfDewpt= new FloatByReference(0);
		t700 = nsharpNative.nsharpLib.itemp(700.0F);
		if (nsharpNative.nsharpLib.qc(t700) == 0)
		     return;
		t850 = nsharpNative.nsharpLib.itemp(850.0F);
		if (nsharpNative.nsharpLib.qc(t850) == 0)
		     return;
		d700 = nsharpNative.nsharpLib.idwpt(700.0F);
		if (nsharpNative.nsharpLib.qc(d700) == 0)
		     return;
		d850 = nsharpNative.nsharpLib.idwpt(850.0F);
		if (nsharpNative.nsharpLib.qc(d850) == 0)
		     return;
        ct1 = nsharpNative.nsharpLib.thetae( 850.0F, t850, d850);
        ct2 = nsharpNative.nsharpLib.thetae( 700.0F, t700, d700);
        nsharpNative.nsharpLib.get_surface(surfpressure, surfTemp, surfDewpt);		
        ct3 = nsharpNative.nsharpLib.thetae( surfpressure.getValue(), surfTemp.getValue(), surfDewpt.getValue() );
        cthe = 0.0F; nct = 0.0F;
        if(nsharpNative.nsharpLib.qc(ct1) != 0)
        {
        	cthe = cthe + ct1;
        	nct++;
        }
        if(nsharpNative.nsharpLib.qc(ct2) != 0)
        {
        	cthe = cthe + ct2;
        	nct++;
        }
        if(nsharpNative.nsharpLib.qc(ct3)!= 0)
        {
        	cthe = cthe + ct3;
        	nct++;
        }
        if(nct < 1.0F) return;
        cthe = cthe / nct;
        //Adjust Theta-E axis scale with real view X scale
        thetaEDispWidth = 60;
        xAxisCenter = ((vxMax-vxMin ) / 2 ) + vxMin;
        xScaleAdjustRatio = (vxMax-vxMin)/thetaEDispWidth;
        for(double h= -(thetaEDispWidth/2); h<=(thetaEDispWidth/2); h += 10.0F)
        {
        	int iThetaE = (int)(h+cthe); //cthe sits in the middle of x-axis
        	dispX = xAxisCenter + (h* xScaleAdjustRatio);
        	dispY =  vyMax; 
        	//System.out.println("cthe = " + cthe+"xmax = " + vxMax + ", Ymax = " +vyMax + " x1 ="+ x1 + "y1 = " + y1 + " h = " + h );
           	double [][] lines1 = {{dispX, dispY},{dispX,dispY-20}};
            thetaEPressureWRscShape.addLineSegment(lines1);
           	if((h != -(thetaEDispWidth/2))&&(h != (thetaEDispWidth/2)) ){
           		//draw scale with kelvin degree
           		double [] lblXy1= { dispX, dispY-20};
        		thetaEPressureYRscShape.addLabel(String.valueOf(iThetaE+273), lblXy1);
           	}
        
        }

        // ----- Plot vertical theta-e profile -----     
        //use soundingLys directly
        float thetaE, highPressure=1000;
        dispX1 = dispY1 = -999;
        //find highest pressure available on sounding data. But should be <= 1000 and > 500
        for( NcSoundingLayer lys:soundingLys)
        {
        	if((lys.getPressure() >= 500) && (lys.getPressure() <= 1000)){
        		highPressure = lys.getPressure();
        		break;
        	}
        }
    	yScaleAdjustRatio = (vyMax-vyMin)/(highPressure-500);
        for( NcSoundingLayer lys:soundingLys)
        {	//plot only at pressure greater than or equal to 500
        	if((lys.getPressure() >= 500) && (lys.getPressure() <= 1000) && (nsharpNative.nsharpLib.qc(lys.getDewpoint()) !=0 )){
        		thetaE = nsharpNative.nsharpLib.thetae( lys.getPressure(), lys.getTemperature(),lys.getDewpoint())-cthe;
        		dispX = xAxisCenter + ( thetaE * xScaleAdjustRatio);
        		dispY =  vyMin + (lys.getPressure()-500) * yScaleAdjustRatio ;
        		
        		if(dispX1 == -999) {
        			dispX1 = dispX;
        			dispY1 = dispY;
        		}
        		//System.out.println("dispX="+dispX+" dispY="+dispY+" geoH="+lys.getGeoHeight());
       		double [][] lines1 = {{dispX, dispY},{dispX1,dispY1}};
                thetaEPressureRRscShape.addLineSegment(lines1);
        		dispX1 = dispX;
    			dispY1 = dispY;
    			
        	}
        }
    	/* ----- Plot Vertical (pressure) Legend ----- */
        dispX = vxMin;
    	for(int pres=900; pres > 500; pres -= 100)
    	{
    		dispY =  vyMin +(pres-500) * yScaleAdjustRatio ;
    		double [][] lines1 = {{dispX, dispY},{dispX+20,dispY}};
            thetaEPressureWRscShape.addLineSegment(lines1);
            double [] lblXy1= { dispX+25, dispY};
    		thetaEPressureYRscShape.addLabel(String.valueOf(pres), lblXy1);
    	}
		thetaEPressureYRscShape.compile();
        thetaEPressureWRscShape.compile();
        thetaEPressureRRscShape.compile();
    }
	private void createRscPressTempCurveShape(WGraphics WGc, List<NcSoundingLayer> soundingLays, RGB incolor){
		IWireframeShape shapeT = target.createWireframeShape(false,descriptor );
		shapeT.allocate(soundingLays.size() * 2);
		IWireframeShape shapeD = target.createWireframeShape(false,descriptor );
		shapeD.allocate(soundingLays.size() * 2);
		ShapeAndColor shNcolorT = new ShapeAndColor(), shNcolorD=new ShapeAndColor();
        double maxPressure = WxMath.reverseSkewTXY(new Coordinate(0, WGc
                .getWorldYmax())).y;
        double minPressure = WxMath.reverseSkewTXY(new Coordinate(0, WGc
                .getWorldYmin())).y;
        boolean drawTemp=true, drawDew=true;
        NsharpGraphConfigDialog configD = NsharpGraphConfigDialog.getAccess();
        if(configD!=null){
        	drawTemp = configD.isTemp();
        	drawDew = configD.isDewp();
        }
        Coordinate c0 = null, c01=null;
        for (NcSoundingLayer layer : soundingLays) {
        	double t, d;
        	t = layer.getTemperature();
        	d = layer.getDewpoint();
        	
            double pressure = layer.getPressure();
            if (t != INVALID_DATA  && pressure >= minPressure
                    && pressure <= maxPressure) {

                Coordinate c1 = WxMath.getSkewTXY(pressure, t);
                
                c1.x = WGc.mapX(c1.x);
                c1.y = WGc.mapY(c1.y);
                if (c0 != null) {
                		double [][] lines = {{c0.x, c0.y},{c1.x, c1.y}};
                		shapeT.addLineSegment(lines);
                 }
                c0 = c1;
            }
            if (d > -999  && pressure >= minPressure
                    && pressure <= maxPressure) {

                Coordinate c11 = WxMath.getSkewTXY(pressure, d);
                
                c11.x = WGc.mapX(c11.x);
                c11.y = WGc.mapY(c11.y);
                if (c01 != null) {
                		double [][] lines = {{c01.x, c01.y},{c11.x, c11.y}};
                		shapeD.addLineSegment(lines);
                 }
                c01 = c11;
            }
        }
        
        shapeT.compile();
        shapeD.compile();
        
        shNcolorT.shape = shapeT;
        shNcolorD.shape = shapeD;
        if(incolor == null){
        	//use default color 
        	shNcolorT.color = NsharpConstants.color_red;
        	shNcolorD.color = NsharpConstants.color_green;           
        }
        else
        {
        	shNcolorT.color = incolor;
        	shNcolorD.color = incolor;
        }
        //check draw temp and dew here. It is easier to do this way, otherwise, we have to check it every wghere
        if(drawTemp)
        	pressureTempRscShapeList.add(shNcolorT);
        else
        	shNcolorT.shape.dispose();
        if(drawDew)
        	pressureTempRscShapeList.add(shNcolorD);
        else
        	shNcolorD.shape.dispose();
	}
	private void createRscwetBulbTraceShape(WGraphics WGc){
		wetBulbTraceRscShape = target.createWireframeShape(false,descriptor );
		wetBulbTraceRscShape.allocate(soundingLys.size() * 2);
		float t1;
	    
        Coordinate c2 =  null;
        Coordinate c1;
        // draw trace
        for (NcSoundingLayer layer : this.soundingLys) {
        	if (layer.getDewpoint() > -200 && layer.getPressure() >= 100){
        		t1 = nsharpNative.nsharpLib.wetbulb(layer.getPressure(), layer.getTemperature(),
        				layer.getDewpoint());

        		c1 = WxMath.getSkewTXY(layer.getPressure(), t1);
        		c1.x = WGc.mapX(c1.x);
        		c1.y = WGc.mapY(c1.y);
        		if(c2!= null){

        			double [][] lines = {{c1.x, c1.y},{c2.x, c2.y}};
        			wetBulbTraceRscShape.addLineSegment(lines);

        		}
        		c2 =  c1;
        	}
        }
        wetBulbTraceRscShape.compile();
	}
	private void createRscOmegaShape(WGraphics WGc){
		omegaRscShape = target.createWireframeShape(false,descriptor );
		omegaRscShape.allocate(soundingLys.size() * 2);
		float p, omega, t;
		double xAxisOrigin=NsharpConstants.OMEGA_X_TOP;
		for (NcSoundingLayer layer : this.soundingLys) {
        	p = layer.getPressure();
        	t = layer.getTemperature();
        	omega = layer.getOmega()* 10 ;//to have same scale as legacy Nsharp 
        	if (p > 140 && p < 1000 && omega > -10){
        		//since 40 units of skewT X-axis is 10 unit of Omega, we have to convert it by 
        		// multiply 4 to omega unit for plotting
        		Coordinate c1 = WxMath.getSkewTXY(p, t);
        		c1.y = WGc.mapY(c1.y); // what we need here is only pressure for Y-axix, 
         		//target.drawLine(xAxisOrigin, c1.y, 0.0, xAxisOrigin + omega* 4, c1.y, 0.0, NsharpConstants.color_cyan,
        	  	//		commonLinewidth);
         		double [][] lines = {{xAxisOrigin, c1.y},{xAxisOrigin + omega* 4, c1.y}};
         		omegaRscShape.addLineSegment(lines);
         	}
        }
        omegaRscShape.compile();
	}
	private void createRscHeightMarkShape(WGraphics WGc){
		heightMarkRscShape = target.createWireframeShape(false,descriptor );
		heightMarkRscShape.allocate(20);
		//plot meter  scales...
        for (int j = 0; j < NsharpConstants.HEIGHT_LEVEL_METERS.length; j++) {
        	int meters = NsharpConstants.HEIGHT_LEVEL_METERS[j];
        	// plot the meters scale
        	double pressure = nsharpNative.nsharpLib.ipres(meters+(int)(soundingLys.get(0).getGeoHeight()));
        	double y = WGc.mapY(WxMath.getSkewTXY(pressure, -50).y);

        	double [][] lines = {{WGc.mapX(NsharpConstants.left) + 20, y},{WGc.mapX(NsharpConstants.left) + 40, y}};
            heightMarkRscShape.addLineSegment(lines);
            double [] lblXy = {WGc.mapX(NsharpConstants.left) + 50,y-5};
            heightMarkRscShape.addLabel(Integer.toString(meters/1000)+" km", lblXy);
        } 
        // plot surface level mark{
        if(soundingLys.get(0).getGeoHeight() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)
        {
        	double y = WGc.mapY(WxMath.getSkewTXY(soundingLys.get(0).getPressure(), -50).y);
        	double [][] lines = {{WGc.mapX(NsharpConstants.left) + 20, y},{WGc.mapX(NsharpConstants.left) + 40, y}};
            heightMarkRscShape.addLineSegment(lines);
            double [] lblXy = {WGc.mapX(NsharpConstants.left) + 50,y-5};
            heightMarkRscShape.addLabel("SFC("+Integer.toString((int)(soundingLys.get(0).getGeoHeight()))+"m)", lblXy);
        }
        heightMarkRscShape.compile();
	}
	/**
	 * Create all wire frame shapes at one place.
	 * Should be used only when a new resource is becoming Current active resource to be displayed.
	 *  
	 */
	private void createAllWireFrameShapes(){
		//System.out.println("createAllWireFrameShapes called");
		disposeAllWireFrameShapes();
		createRscWireFrameShapes();;
		//create static shape
		createBkgOmegaShape();
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();  
		WGraphics WGc = bkRsc.getSkewTBackground().getWorld();
		createBkgWindBoxShape(WGc);
		createHodoWindMotionBoxShape();
	}
	private void createRscWireFrameShapes(){
		//System.out.println("createRscWireFrameShapes called");
		disposeRscWireFrameShapes();
		if(soundingLys != null){
			NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();   	
			WGraphics WGc = bkRsc.getSkewTBackground().getWorld();
			createRscHeightMarkShape(WGc);
			createRscOmegaShape(WGc);
			createRscwetBulbTraceShape(WGc);
			createRscPressTempCurveShapeAll();
			createRscVTempTraceShape(WGc);
			createRscWindBoxWindShape(WGc);
			for (ParcelData parData: parcelList){
				createRscParcelTraceShape( WGc, parData.parcelType,parData.parcelLayerPressure);
			}

			createRscHodoWindShapeAll();

			WGc=  bkRsc.getThetaEPresureBackground().getWorld();		
			createRscThetaEPressureShape(WGc);
			WGc=  bkRsc.getThetaEHeightBackground().getWorld();		
			createRscThetaEHeightShape(WGc);
			WGc=  bkRsc.getSrWindsBackground().getWorld();	
			createRscSrWindShape(WGc);
			
			// Chin: Note: delay create possible watch type to when first time paintInterl() called.
			// To fix an initial watch type not correct issue: TTR6191.
			//WGc = bkRsc.getPsblWatchTypeBackground().getWorld();
			//createBkgPsblWatchShape(WGc); 
			if(psblWatchTypeBkgShape != null){
				psblWatchTypeBkgShape.dispose();
				psblWatchTypeBkgShape = null;
			}
			
			WGc = bkRsc.getVerticalWindBackground().getWorld();
			createRscVerticalWindShape(WGc);
		}
	}
	
}

