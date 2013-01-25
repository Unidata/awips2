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

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigStore;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGraphProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpLineProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpSoundingElementStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpTimeLineStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpWxMath;
import gov.noaa.nws.ncep.ui.nsharp.maprsc.NsharpMapMouseHandler;
import gov.noaa.nws.ncep.ui.nsharp.maprsc.NsharpMapResource;
import gov.noaa.nws.ncep.ui.nsharp.menu.NsharpLoadDialog;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpDataHandling;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNativeConstants;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative.NsharpLibrary._lplvalues;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative.NsharpLibrary._parcel;
import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpPaletteWindow;
import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpParcelDialog;
import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpShowTextDialog;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDescriptor;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDisplay;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTEditor;
import gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd.NsharpIcingBackground;
import gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd.NsharpTurbulenceBackground;

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
import com.raytheon.uf.viz.core.drawables.IShadedShape;
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
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
@SuppressWarnings("deprecation")
public class NsharpSkewTResource extends AbstractVizResource<AbstractResourceData, NsharpSkewTDescriptor> {
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
	private NsharpConfigManager configMgr;
	private NsharpConfigStore configStore;
	private NsharpGraphProperty graphConfigProperty;
	private HashMap<String, NsharpLineProperty> linePropertyMap; 
	private int parcelLinesInPhysicalPanelNumber = 1;
	private boolean overlayIsOn = false;
	private boolean interpolateIsOn = false;
	private boolean compareStnIsOn = false;
	private boolean compareTmIsOn = false;
	private boolean editGraphOn=false;
	private boolean getTimeMatcher=false;
	private static int CURSER_FONT_INC_STEP = 3;
	private static int CURSER_FONT_10 =10;
	private static int CURSER_STRING_OFF =CURSER_FONT_10+ 5*CURSER_FONT_INC_STEP;
	private int curseToggledFontLevel= CURSER_FONT_10; //0:default 1:large 2:turn off display
	private IGraphicsTarget target=null;
	private RGB wwTypeColor;
	private boolean cursorInSkewT = false, cursorInHodo=false;
	private Coordinate cursorCor;
	private double currentZoomLevel=1;
	private int currentCanvasBoundWidth= NsharpConstants.DEFAULT_CANVAS_WIDTH;
	private int currentCanvasBoundHeight= NsharpConstants.DEFAULT_CANVAS_HEIGHT;
	public int TEMP_TYPE = 1;
	public int DEWPOINT_TYPE = 2;
	private int currentTempCurveType;
	private int currentSoundingLayerIndex =0;
	private int hodoEditingSoundingLayerIndex =0;
	private boolean plotInteractiveTemp= false;
	private Coordinate interactiveTempPointCoordinate;
	private List<NcSoundingLayer> intpSndLst = new ArrayList<NcSoundingLayer>();
	public static final float INVALID_DATA = NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;
    protected static final double BARB_LENGTH = 3.5;
    
    private String soundingType= null;

    protected Map<Date, SoundingParams> soundingMap;

    protected DataTime displayedSounding;
    
    private int currentGraphMode= NsharpConstants.GRAPH_SKEWT;
	protected IFont font9=null;
    protected IFont font10=null;
    protected IFont font11=null;
    protected IFont font12=null;
    private float currentFont10Size=10;
    
    protected ListenerList listenerList = new ListenerList();
    
    private List<List<NcSoundingLayer>> soundingLysList = null;
    
    //current active sounding layer list
	private List<NcSoundingLayer> soundingLys = null;
	private List<NcSoundingLayer> previousSoundingLys = null;
	private String pickedStnInfoStr; // current picked stn info with time line, e.g. "ATLH 2010-12-12 12:00:00"
	private NsharpStationInfo pickedStnInfo = null;
	private FrameChangeOperation currentOpDirection = FrameChangeOperation.NEXT; // next =forward
	private int commonLinewidth;
	private LineStyle commonLineStyle;
	
	private HashMap<Integer, RGB> stormSlinkyColorMap = new HashMap<Integer, RGB>();
    //list of sounding layer list for each data time line
    private HashMap<String, List<NcSoundingLayer>> dataTimelineSndLysListMap = new HashMap<String, List<NcSoundingLayer>>();
    private HashMap<String, List<NcSoundingLayer>> originalDataTimelineSndLysListMap= new HashMap<String, List<NcSoundingLayer>>();
	//public enum State {
	//	CURRENT, ACTIVE, INACTIVE,NOTAVAIL ,OVERLAY, AVAIL//was , DISABLED
	//}
	//dataTimelineList: time line selected by user, but is updated based on available time line at DB at setRsc()
	// this is derived from dataTimelineSndLysListMap. It has stn info + sounding time line info
	// used field is used to identify if this time line is picked by user. user could pick multiple time lines for comparison
	private List<ElementStateProperty> dataTimelineList= new ArrayList<ElementStateProperty>();
	public List<ElementStateProperty> getDataTimelineList() {
		System.out.println("getDataTimelineList called");
		for(List<NsharpSoundingElementStateProperty> stnList: stnTimeTable){
			for(NsharpSoundingElementStateProperty sndProp: stnList){
				ElementStateProperty elm = new ElementStateProperty();
				elm.elementDescription = sndProp.elementDescription;
				elm.stnInfo = sndProp.stnInfo;
				elm.elementState = sndProp.elementState;
				dataTimelineList.add(elm);
			}
		}
		Collections.sort(dataTimelineList, new ElementComparatorNameTime());
		return dataTimelineList;
	}
	
	
	// Chin Note: Store all sounding profiles property for GUI display control
	// 1st index points to stnId and 2nd index points to time line.
	// IN other words, the outer list is listed by stnId and inner list is listed by time line.
	// It is same as [][] 2D matrix. When a new Stn with a time line loaded, 
	// We create a new stn list (in outer list) and fill in all (currently available) time lines (in inner list), even only one element is loaded. 
	// All unloaded element is marked as "NOTAVAIL" state. Only when user load new sounding at this stn at this time line, then change
	// this element state out of "NOTAVAIL".
	//  stn1---> stn2--->stn3->...
	//   |         |      |
	//   V         V      V
	//  T1         T1     T1
	//   |         |      |
	//   V         V      V
	//  T2         T2     T2
	//   |         |      |
	//   V         V      V
	//  T3         T3     T3
	//   |         |      |
	//   V         V      V
	// Its outer list index should be in sync with stnStateList, and its inner list index should be in sync with timeLineStateList
	// elementState should be AVAIL or NOTAVAIL only
	private List<List <NsharpSoundingElementStateProperty>>stnTimeTable = new ArrayList< List <NsharpSoundingElementStateProperty>>();
	//timeLineStateList store time line administration state. Active, Inactive 
	private List<NsharpTimeLineStateProperty> timeLineStateList = new ArrayList<NsharpTimeLineStateProperty>();
	//stnStateList store stn administration states, Active, Inactive And stn info
	private List<NsharpStationStateProperty> stnStateList = new ArrayList<NsharpStationStateProperty>();
	private NsharpSoundingElementStateProperty curSndProfileProp = null;
	private NsharpSoundingElementStateProperty preSndProfileProp = null;
	private int curTimeLinePage=1;
	private int totalTimeLinePage=1;
	private int curStnIdPage=1;
	private int totalStnIdPage=1;
	private int currentStnStateListIndex;
	private int currentTimeLineStateListIndex;
	private int previousTimeLineStateListIndex;
		
	public List<List<NsharpSoundingElementStateProperty>> getStnTimeTable() {
		return stnTimeTable;
	}
	

	public List<NsharpTimeLineStateProperty> getTimeLineStateList() {
		return timeLineStateList;
	}

	public List<NsharpStationStateProperty> getStnStateList() {
		return stnStateList;
	}


	private HashMap<NsharpConstants.State, RGB> elementColorMap = new HashMap<NsharpConstants.State, RGB>();
	public class ElementStateProperty {
		String elementDescription;
		NsharpConstants.State elementState;
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
		public NsharpConstants.State getElementState() {
			return elementState;
		}
	}
	
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
	private Integer     	markerWidth = 1;
	private Coordinate hodoHouseC = new Coordinate(NsharpConstants.HODO_CENTER_X, NsharpConstants.HODO_CENTER_Y);
	private float smWindDir, smWindSpd;
	//shape and color storage
	public class ShapeAndLineProperty {
		IWireframeShape shape;
		NsharpLineProperty lp;
		public ShapeAndLineProperty() {
			super();
			lp = new NsharpLineProperty();
		}
		
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
	private IShadedShape cloudFMShape = null;
	private IWireframeShape cloudFMLabelShape = null;
	private IShadedShape cloudCEShape = null;
	//ICING wireframe shape
	private IWireframeShape icingTempShape = null;
	private IWireframeShape icingRHShape = null;
	private IWireframeShape icingEPIShape = null;
	//Turbulence wireframe shape
	private IWireframeShape turbLnShape = null;
	private IWireframeShape turbWindShearShape = null;

	private List<IWireframeShape> parcelTraceRscShapeList = new ArrayList<IWireframeShape>();
	private List<ShapeAndLineProperty>hodoWindRscShapeList  = new ArrayList<ShapeAndLineProperty>();
	private List<ShapeAndLineProperty>pressureTempRscShapeList  = new ArrayList<ShapeAndLineProperty>();
	private List<ShapeAndLineProperty>windBoxWindRscShapeList  = new ArrayList<ShapeAndLineProperty>();
	//static bk shape that not handled at background resource class
	private IWireframeShape omegaBkgShape = null;
	private IWireframeShape psblWatchTypeBkgShape = null;
	private IWireframeShape windBoxBkgShape = null;
	private IWireframeShape hodoWindMotionBoxShape = null;
	public HashMap<String, NsharpLineProperty> getLinePropertyMap() {
		return linePropertyMap;
	}

	public void toggleCurseDisplay() {
		curseToggledFontLevel = curseToggledFontLevel + CURSER_FONT_INC_STEP;
		if(curseToggledFontLevel > CURSER_STRING_OFF)
			curseToggledFontLevel = CURSER_FONT_10;
		NsharpSkewTEditor editor = NsharpSkewTEditor.getActiveNsharpEditor();
		if (editor != null) {
			editor.refresh();
		}
	}

	public boolean isCompareStnIsOn() {
		return compareStnIsOn;
	}
	public boolean isCompareTmIsOn() {
		return compareTmIsOn;
	}
	
	public boolean isOverlayIsOn() {
		return overlayIsOn;
	}

	public void setCursorInSkewT(boolean cursorInSkewT, Coordinate c) {
		this.cursorInSkewT = cursorInSkewT;
		this.cursorCor = c;
	}
	public void setCursorInHodo(boolean cursorInHodo) {
		this.cursorInHodo = cursorInHodo;
		
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
	
	
	public void setOverlayIsOn(boolean overlay) {
		previousSoundingLys=null;
		previousTimeLineStateListIndex=-1;
		preSndProfileProp = null;
		this.overlayIsOn = overlay;
		
		createRscHodoWindShapeAll();
		createRscPressTempCurveShapeAll();
	}
	public void setInterpolateIsOn(boolean interpolateIsOn) {
		this.interpolateIsOn = interpolateIsOn;
		
	}
	

	public boolean isInterpolateIsOn() {
		return interpolateIsOn;
	}
	public void setCompareStnIsOn(boolean compareIsOn) {
		this.compareStnIsOn = compareIsOn;
		/*if(compareStnIsOn){
			currentTimeLineStateListIndexBackup = currentTimeLineStateListIndex;
			currentStnStateListIndexBackup = currentStnStateListIndex;
		}
		else{
			currentTimeLineStateListIndex = currentTimeLineStateListIndexBackup;
			currentStnStateListIndex = currentStnStateListIndexBackup;
		}*/
		//make sure soundingLys is not null
		if(soundingLys==null && compareIsOn && currentTimeLineStateListIndex>=0){
			//find a new available stn for current time line
			boolean found = false;
			for(int i =0; i< stnStateList.size(); i++){
				if(stnStateList.get(i).stnState == NsharpConstants.State.ACTIVE 
						&& stnTimeTable.get(i).get(currentTimeLineStateListIndex).elementState == NsharpConstants.State.AVAIL){
					found = true;
					currentStnStateListIndex = i;
				}
				if(found)
					break;
			}
		}
		int numTimeLinePerPage = (NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END)/NsharpConstants.CHAR_HEIGHT;
		curStnIdPage = totalStnIdPage/numTimeLinePerPage + 1;
		setCurSndProfileProp();
		setCurrentSoundingLayerInfo();
		resetData();
		if(soundingLys!=null){
			createRscHodoWindShapeAll();
			createRscPressTempCurveShapeAll();
		}
	}
	public void setCompareTmIsOn(boolean compareIsOn) {
		this.compareTmIsOn = compareIsOn;
		/*if(compareTmIsOn){
			currentTimeLineStateListIndexBackup = currentTimeLineStateListIndex;
			currentStnStateListIndexBackup = currentStnStateListIndex;
		}
		else{
			currentTimeLineStateListIndex = currentTimeLineStateListIndexBackup;
			currentStnStateListIndex = currentStnStateListIndexBackup;
		}*/
		//make sure soundingLys is not null
		if(soundingLys==null && compareIsOn){
			//find a new available time line for current stn
			boolean found = false;
			for(int i =0; i< timeLineStateList.size(); i++){
				if(timeLineStateList.get(i).timeState == NsharpConstants.State.ACTIVE 
					&& stnTimeTable.get(currentStnStateListIndex).get(i).elementState == NsharpConstants.State.AVAIL){
					found = true;
					previousTimeLineStateListIndex = currentTimeLineStateListIndex;
		 			currentTimeLineStateListIndex = i;
				}
				if(found)
					break;
			}
		}
		int numTimeLinePerPage = (NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END)/NsharpConstants.CHAR_HEIGHT;
		curTimeLinePage = currentTimeLineStateListIndex/numTimeLinePerPage + 1;
		setCurSndProfileProp();
		setCurrentSoundingLayerInfo();
		resetData();	 			
		if(soundingLys!=null){
			createRscHodoWindShapeAll();
			createRscPressTempCurveShapeAll();
		}		
	}

	public void setEditGraphOn(boolean editGraphOn) {
		this.editGraphOn = editGraphOn;
	}

	public boolean isEditGraphOn() {
		return editGraphOn;
	}

    
    public int getCurrentGraphMode() {
		return currentGraphMode;
	}

	public void setCurrentGraphMode(int currentGraphMode) {
		this.currentGraphMode = currentGraphMode;
		NsharpSkewTEditor editor = NsharpSkewTEditor.getActiveNsharpEditor();
		if (editor != null) {
			editor.refresh();
		}
	}


	public short getCurrentParcel() {
		return currentParcel;
	}

	//native nsharp c/fortran lib
	//NsharpNative is native nsharp lib awips/lib/libnsharp.so wrapper class
	
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
		this.dataTimelineSndLysListMap.clear();
		//this.dataTimelineSndLysListMap = new HashMap<String, List<NcSoundingLayer>>();
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
		compareStnIsOn = false;
		editGraphOn = false;
		resetData();

	}
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
			desc.setFrame(0); //NEW CODE
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
	//NOTE: this comparator is coded only for dataTimelineList and stationIdList to use
	//Typical time line string: e.g. KNJX 110810/00V000 (NAMS) meaning stnId=KNJX date=2011/Aug/10 Hour:00
	// we dont care about string after "V"
	//compare station name first, then day/hour
	//if only sta name available, then just compare sta name
	//e.g. stationIdList only contain stn name in its element
	public class ElementComparatorNameTime  implements Comparator<ElementStateProperty>{
		@Override
		public int compare(ElementStateProperty o1, ElementStateProperty o2) {
			
			String s1tok1="", s1tok2="";//, s1tok3="";
			String s2tok1="", s2tok2="";//, s2tok3="";
			StringTokenizer st1 = new StringTokenizer(o1.elementDescription);
			int tkCount1 = st1.countTokens();
			//System.out.println("ElementComparatorNDH o1="+o1.elementDescription+"c1 = "+tkCount1);
			if(tkCount1 < 2)
			{
				//stationIdList only contain stn name in its element
				s1tok1 = st1.nextToken(); //stn name
			}
			else{
				s1tok1 = st1.nextToken(); //stn name
				s1tok2 = st1.nextToken(); //date/hour
				//int end = Math.min(9,s1tok2.length());
				//s1tok2 = s1tok2.substring(0,end); // get rind of Vxxx part if there is one
				
			}
			//System.out.println("t1="+s1tok1+" t2="+s1tok2);
			StringTokenizer st2 = new StringTokenizer(o2.elementDescription);
			int tkCount2 = st2.countTokens();
			//System.out.println("ElementComparatorNDH o2="+o2.elementDescription+"c2 = "+tkCount2);
			if(tkCount2 < 2)
			{
				s2tok1 = st2.nextToken(); //stn name
			}
			else{
				s2tok1 = st2.nextToken(); //stn name
				s2tok2 = st2.nextToken(); //date/Hour
				//int end = Math.min(9,s2tok2.length());
				//s2tok2 = s2tok2.substring(0,end);// get rind of Vxxx part if there is one
			}
			//System.out.println("t1="+s2tok1+" t2="+s2tok2);
			if(s1tok1.compareTo(s2tok1) == 0){
				//same station name
				if(s1tok2.compareTo(s2tok2) == 0){
					//same day
					return 0;
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
	public class SndPropElementComparatorStnId  implements Comparator<List<NsharpSoundingElementStateProperty>>{
		@Override
		public int compare(List<NsharpSoundingElementStateProperty> o1, List<NsharpSoundingElementStateProperty> o2) {
			
			String s1tok1="";
			String s2tok1="";
			StringTokenizer st1 = new StringTokenizer(o1.get(0).stnDescription);
			int tkCount1 = st1.countTokens();
			//System.out.println("ElementComparatorNDH o1="+o1.elementDescription+"c1 = "+tkCount1);
			if(tkCount1 < 2)
			{
				//stationIdList only contain stn name in its element
				s1tok1 = st1.nextToken(); //stn name
			}
			else{
				return 0;
			}
			//System.out.println("t1="+s1tok1+" t2="+s1tok2);
			StringTokenizer st2 = new StringTokenizer(o2.get(0).stnDescription);
			int tkCount2 = st2.countTokens();
			//System.out.println("ElementComparatorNDH o2="+o2.elementDescription+"c2 = "+tkCount2);
			if(tkCount2 < 2)
			{
				s2tok1 = st2.nextToken(); //stn name
			}
			else{
				return 0;
			}
			//System.out.println("t1="+s2tok1+" t2="+s2tok2);
			if(s1tok1.compareTo(s2tok1) == 0){
				//same station name
				return 0;
				
			} else if (s1tok1.compareTo(s2tok1) < 0){
				return -1;
			} else if (s1tok1.compareTo(s2tok1) > 0) {
				return 1;
			}
			return 0;
		}
	}
	public class StationStatePropertyComparator  implements Comparator<NsharpStationStateProperty>{
		@Override
		public int compare(NsharpStationStateProperty o1, NsharpStationStateProperty o2) {
			
			String s1tok1="";
			String s2tok1="";
			StringTokenizer st1 = new StringTokenizer(o1.stnDescription);
			int tkCount1 = st1.countTokens();
			//System.out.println("ElementComparatorNDH o1="+o1.elementDescription+"c1 = "+tkCount1);
			if(tkCount1 < 2)
			{
				//stationIdList only contain stn name in its element
				s1tok1 = st1.nextToken(); //stn name
			}
			else{
				return 0;
			}
			//System.out.println("t1="+s1tok1+" t2="+s1tok2);
			StringTokenizer st2 = new StringTokenizer(o2.stnDescription);
			int tkCount2 = st2.countTokens();
			//System.out.println("ElementComparatorNDH o2="+o2.elementDescription+"c2 = "+tkCount2);
			if(tkCount2 < 2)
			{
				s2tok1 = st2.nextToken(); //stn name
			}
			else{
				return 0;
			}
			//System.out.println("t1="+s2tok1+" t2="+s2tok2);
			if(s1tok1.compareTo(s2tok1) == 0){
				//same station name
				return 0;
				
			} else if (s1tok1.compareTo(s2tok1) < 0){
				return -1;
			} else if (s1tok1.compareTo(s2tok1) > 0) {
				return 1;
			}
			return 0;
		}
	}
	//NOTE: this comparator is coded only for dataTimelineList to use
	//compare  day/hour, then station name
	//Typical time line string: e.g. KNJX 110810/00V000 (NAMS)
	/*public class ElementComparatorTimeName  implements Comparator<T>{
		@Override
		public int compare(T o1, T o2) {
			
			String s1tok1="", s1tok2="";
			String s2tok1="", s2tok2="";
			StringTokenizer st1 = new StringTokenizer(o1.elementDescription);
			int tkCount1 = st1.countTokens();
			//System.out.println("ElementComparatorDHN o1="+o1.elementDescription+"c1 = "+tkCount1);
			if(tkCount1 < 2)
			{
				if(tkCount1==0)
					s1tok1= o2.elementDescription;
				else
					s1tok1 = st1.nextToken();
			}
			else{
				s1tok1 = st1.nextToken(); //stn name
				s1tok2 = st1.nextToken(); //
				
				
			}
			//System.out.println("t1="+s1tok1+" t2="+s1tok2);
			StringTokenizer st2 = new StringTokenizer(o2.elementDescription);
			int tkCount2 = st2.countTokens();
			//System.out.println("ElementComparatorDHN o2="+o2.elementDescription+"c2 = "+tkCount2);
			if(tkCount2 < 2)
			{
				//return -1;
				if(tkCount2==0)
					s2tok1= o2.elementDescription;
				else
					s2tok1 = st2.nextToken();
			}
			else{
				s2tok1 = st2.nextToken(); //stn name
				s2tok2 = st2.nextToken(); //
				
				
			}
			//System.out.println("t1="+s2tok1+" t2="+s2tok2);
			if(s1tok2.compareTo(s2tok2) == 0){ //compare day/hour
				//same 
				if(s1tok1.compareTo(s2tok1) == 0){//compare stn
					return 0;
				}else if (s1tok1.compareTo(s2tok1) < 0){
					return -1;
				} else if (s1tok1.compareTo(s2tok1) > 0) {
					return 1;
				}
				
			} else if (s1tok2.compareTo(s2tok2) < 0){
				return 1;
			} else if (s1tok2.compareTo(s2tok2) > 0) {
				return -1;
			}
			return 0;
		}
	}*/
	//NOTE: this comparator is coded only for curAggregateTimeLineList to use
	//compare  day/hour at first tokne only
	//Typical time line string: e.g. 110810/00V000 (NAMS)
	public class ElementComparatorTimeLine  implements Comparator<ElementStateProperty>{
		@Override
		public int compare(ElementStateProperty o1, ElementStateProperty o2) {
			
			String s1tok1="";//, s1tok2="";
			String s2tok1="";//, s2tok2="";
			StringTokenizer st1 = new StringTokenizer(o1.elementDescription);
			int tkCount1 = st1.countTokens();
			//System.out.println("ElementComparatorTimeLine o1="+o1.elementDescription+"c1 = "+tkCount1);
			if(tkCount1 >= 1)
			{
				s1tok1 = st1.nextToken();
			}
			else{
				return 0;

			}			
			//System.out.println("t1="+s1tok1+" t2="+s1tok2);
			StringTokenizer st2 = new StringTokenizer(o2.elementDescription);
			int tkCount2 = st2.countTokens();
			//System.out.println("ElementComparatorTimeLine o2="+o2.elementDescription+"c2 = "+tkCount2);
			if(tkCount2 >= 1)
			{
				s2tok1 = st2.nextToken();
			}
			else{
				return 0;

			}
			//System.out.println("t1="+s2tok1+" t2="+s2tok2);
			if(s1tok1.compareTo(s2tok1) == 0){
				return 0;
			}else if (s1tok1.compareTo(s2tok1) < 0){
				return 1;
			} else if (s1tok1.compareTo(s2tok1) > 0) {
				return -1;
			}
			return 0;
		}
	}
	public class SndPropElementComparatorTimeLine  implements Comparator<NsharpSoundingElementStateProperty>{
		@Override
		public int compare(NsharpSoundingElementStateProperty o1, NsharpSoundingElementStateProperty o2) {
			
			String s1tok1="";//, s1tok2="";
			String s2tok1="";//, s2tok2="";
			StringTokenizer st1 = new StringTokenizer(o1.timeDescription);
			int tkCount1 = st1.countTokens();
			//System.out.println("ElementComparatorTimeLine o1="+o1.elementDescription+"c1 = "+tkCount1);
			if(tkCount1 >= 1)
			{
				s1tok1 = st1.nextToken();
			}
			else{
				return 0;

			}			
			//System.out.println("t1="+s1tok1+" t2="+s1tok2);
			StringTokenizer st2 = new StringTokenizer(o2.timeDescription);
			int tkCount2 = st2.countTokens();
			//System.out.println("ElementComparatorTimeLine o2="+o2.elementDescription+"c2 = "+tkCount2);
			if(tkCount2 >= 1)
			{
				s2tok1 = st2.nextToken();
			}
			else{
				return 0;

			}
			//System.out.println("t1="+s2tok1+" t2="+s2tok2);
			if(s1tok1.compareTo(s2tok1) == 0){
				return 0;
			}else if (s1tok1.compareTo(s2tok1) < 0){
				return 1;
			} else if (s1tok1.compareTo(s2tok1) > 0) {
				return -1;
			}
			return 0;
		}
	}
	public class TimeLineStateComparator  implements Comparator<NsharpTimeLineStateProperty>{
		@Override
		public int compare(NsharpTimeLineStateProperty o1, NsharpTimeLineStateProperty o2) {
			
			String s1tok1="";//, s1tok2="";
			String s2tok1="";//, s2tok2="";
			StringTokenizer st1 = new StringTokenizer(o1.timeDescription);
			int tkCount1 = st1.countTokens();
			//System.out.println("ElementComparatorTimeLine o1="+o1.elementDescription+"c1 = "+tkCount1);
			if(tkCount1 >= 1)
			{
				s1tok1 = st1.nextToken();
			}
			else{
				return 0;

			}			
			//System.out.println("t1="+s1tok1+" t2="+s1tok2);
			StringTokenizer st2 = new StringTokenizer(o2.timeDescription);
			int tkCount2 = st2.countTokens();
			//System.out.println("ElementComparatorTimeLine o2="+o2.elementDescription+"c2 = "+tkCount2);
			if(tkCount2 >= 1)
			{
				s2tok1 = st2.nextToken();
			}
			else{
				return 0;

			}
			//System.out.println("t1="+s2tok1+" t2="+s2tok2);
			if(s1tok1.compareTo(s2tok1) == 0){
				return 0;
			}else if (s1tok1.compareTo(s2tok1) < 0){
				return 1;
			} else if (s1tok1.compareTo(s2tok1) > 0) {
				return -1;
			}
			return 0;
		}
	}
	
	private int getTmLineIndexFromTimeLineStateList(String tmLine){
		for(NsharpTimeLineStateProperty sndProp: timeLineStateList){
			if(sndProp.timeDescription.equals(tmLine) )
				return timeLineStateList.indexOf(sndProp); 
		}

		return -1;
	}
	private int getStnIndexFromStnStateList(String stnId){
		for(NsharpStationStateProperty sndProp: stnStateList){
			if(sndProp.stnDescription.equals(stnId) )
				return stnStateList.indexOf(sndProp); 

		}
		return -1;
	}
	private void setCurrentTimeLineStateIndexForCurrentState(){
		boolean found = false;
		for(NsharpTimeLineStateProperty tl:timeLineStateList){
			if(tl.timeState  == NsharpConstants.State.ACTIVE && 
					stnTimeTable.get(currentStnStateListIndex).get(timeLineStateList.indexOf(tl)).elementState == NsharpConstants.State.AVAIL){
				currentTimeLineStateListIndex = timeLineStateList.indexOf(tl);
				found = true;
				break; // find a good time line state index
			}
		}
		if(!found)
			currentTimeLineStateListIndex = -1;
	}
	private void setCurrentStnStateIndexForCurrentTime(){
		boolean found = false;
		for(NsharpStationStateProperty tl:stnStateList){
			if(tl.stnState  == NsharpConstants.State.ACTIVE && 
					stnTimeTable.get(stnStateList.indexOf(tl)).get(currentTimeLineStateListIndex).elementState == NsharpConstants.State.AVAIL){
				currentStnStateListIndex = stnStateList.indexOf(tl);
				found = true;
				break; // find a good stn state index
			}
		}
		if(!found)
			currentStnStateListIndex=-1;
	}
	
	private void findCurrentElementIndexesAfterConfig(){
		if(stnStateList.size()<=0 || timeLineStateList.size() <=0){
			currentTimeLineStateListIndex=-1;
			currentStnStateListIndex=-1;
			return;
		}
		if(currentTimeLineStateListIndex <0 || currentTimeLineStateListIndex>=timeLineStateList.size()){
			//reset it
			currentTimeLineStateListIndex = 0; 
		}
		if(currentStnStateListIndex < 0 || currentStnStateListIndex >= stnStateList.size()){
			currentStnStateListIndex = 0;
		}
		if(timeLineStateList.get(currentTimeLineStateListIndex).timeState == NsharpConstants.State.ACTIVE && 
				stnStateList.get(currentStnStateListIndex).stnState == NsharpConstants.State.ACTIVE )
			return;
		else if(timeLineStateList.get(currentTimeLineStateListIndex).timeState == NsharpConstants.State.INACTIVE && 
				stnStateList.get(currentStnStateListIndex).stnState == NsharpConstants.State.ACTIVE ){
			//find first active time line from list
			for(NsharpTimeLineStateProperty sndProp: timeLineStateList){
				if(sndProp.timeState == NsharpConstants.State.ACTIVE ){
					currentTimeLineStateListIndex = timeLineStateList.indexOf(sndProp);
					//current stn  may not be available for this time line
					if(stnTimeTable.get(currentStnStateListIndex).get(currentTimeLineStateListIndex).elementState ==
								NsharpConstants.State.AVAIL)
						return;
					else {
						//need to find an available time line for this time line
						setCurrentStnStateIndexForCurrentTime();
						return;
					}
				}
			}
			//come to here means nothing found,
			currentTimeLineStateListIndex = -1;
		}
		else if(timeLineStateList.get(currentTimeLineStateListIndex).timeState == NsharpConstants.State.ACTIVE && 
				stnStateList.get(currentStnStateListIndex).stnState == NsharpConstants.State.INACTIVE ){
			for(NsharpStationStateProperty sndProp: stnStateList){
				if(sndProp.stnState == NsharpConstants.State.ACTIVE) {
					currentStnStateListIndex = stnStateList.indexOf(sndProp);
					//current time line may not be available for this stn
					if(stnTimeTable.get(currentStnStateListIndex).get(currentTimeLineStateListIndex).elementState ==
								NsharpConstants.State.AVAIL)
						return;
					else {
						//need to find an available time line for this stn
						setCurrentTimeLineStateIndexForCurrentState();
						return;
					}
				}
			}
			//come to here means nothing found 
			currentStnStateListIndex = -1;
		}
		else{
			boolean found = false;
			for(NsharpTimeLineStateProperty sndProp: timeLineStateList){
				if(sndProp.timeState == NsharpConstants.State.ACTIVE ){
					currentTimeLineStateListIndex = timeLineStateList.indexOf(sndProp);
					found = true;
					break;
				}
				if(!found)
					currentTimeLineStateListIndex = -1;
			}
			for(NsharpStationStateProperty sndProp: stnStateList){
				if(sndProp.stnState == NsharpConstants.State.ACTIVE) {
					currentStnStateListIndex = stnStateList.indexOf(sndProp);
					return;
				}
			}
			//come to here means nothing found 
			currentStnStateListIndex = -1;
		}
		
		
	}
	//NEW CODE
	private void findCurrentElementIndexesAfterDelete(){
		//find new indexes
		for(NsharpTimeLineStateProperty tl:timeLineStateList){
			if(tl.timeState == NsharpConstants.State.ACTIVE){
				for(NsharpStationStateProperty stn: stnStateList){
					if(stn.stnState == NsharpConstants.State.ACTIVE && 
							stnTimeTable.get(stnStateList.indexOf(stn)).get(timeLineStateList.indexOf(tl)).elementState ==
								NsharpConstants.State.AVAIL){
						currentStnStateListIndex = stnStateList.indexOf(stn);
						currentTimeLineStateListIndex = timeLineStateList.indexOf(tl);
						return;
					}
				}
			}
		}
		currentStnStateListIndex = -1;
		currentTimeLineStateListIndex = -1;

	}
	
	private void addElementToTableAndLists(String elementDesc,String stnId, String tmLine, NsharpStationInfo stnInfo){
		System.out.println("stn to be added "+ stnId + " timeline "+tmLine);
		NsharpSoundingElementStateProperty newSndPropElem=null;
		NsharpSoundingElementStateProperty dummySndPropElem=null;
		int tmIndex = getTmLineIndexFromTimeLineStateList(tmLine);//getTmLineIndexByTimeLine(tmLine);
		int stnIndex = getStnIndexFromStnStateList(stnId);//getStnIndexByStnId(stnId);
		if(tmIndex>=0 && stnIndex>=0){
			if(stnTimeTable.get(stnIndex).get(tmIndex).elementState == NsharpConstants.State.AVAIL)
				// this sounding element is already loaded
				return;
			else {
				// this element is previous created but marked as NOTAVAIL state.
				newSndPropElem = stnTimeTable.get(stnIndex).get(tmIndex);
				newSndPropElem.setElementState(NsharpConstants.State.AVAIL);
				newSndPropElem.setElementDescription(elementDesc);
				currentTimeLineStateListIndex = tmIndex;
				currentStnStateListIndex = stnIndex;
			}
		}
		else if (tmIndex >=0 && stnIndex < 0){
			// a new stnId with existing time line
			// Based on our design rules, we will create a new stn with a time line list same as timeLineStateList
			// All time lines will marked as "NOTAVAIL" except this been loaded one
			// need a new stn time line list in stnTimeTable
			List<NsharpSoundingElementStateProperty> newList = new ArrayList<NsharpSoundingElementStateProperty>();
			for(NsharpTimeLineStateProperty sndProp: timeLineStateList){
				if(tmLine.equals(sndProp.timeDescription)){
					//stnId state should be active, but time line state should based on its peer in other stn
					newSndPropElem = 
						new NsharpSoundingElementStateProperty(elementDesc, NsharpConstants.State.AVAIL,stnId, tmLine,  stnInfo);
					
					newList.add(newSndPropElem);
				}
				else{
					//create time line for  not avail sounding profiles
					String elmDes= stnId+" "+sndProp.timeDescription;
					dummySndPropElem = 
						new NsharpSoundingElementStateProperty(elmDes, NsharpConstants.State.NOTAVAIL,stnId,  sndProp.timeDescription,  stnInfo);
					newList.add(dummySndPropElem);
				}
				
			}
			//finally, add this new stn list to table
			stnTimeTable.add(newList);
			//newList is created according to existing list, so, sorting on time line is not necessary.
			//However, we have to sort stnId in stnTimeTable (outer list)
			Collections.sort(stnTimeTable,  new SndPropElementComparatorStnId());
			//add new stn to stnStateList
			NsharpStationStateProperty stn = new NsharpStationStateProperty(stnId,NsharpConstants.State.ACTIVE,stnInfo);
			stnStateList.add(stn);
			Collections.sort(stnStateList,  new StationStatePropertyComparator());
			currentStnStateListIndex = stnStateList.indexOf(stn);
			currentTimeLineStateListIndex = tmIndex;
		}else if (tmIndex < 0 && stnIndex >= 0){
			//  a new time line with existing stnId
			//  Based on our design rules, we will add a new time line to each existing  stns' time line list
			//  Time line element added to a stn which is not the new "coming stn" will marked as "NOTAVAIL"
			for(List<NsharpSoundingElementStateProperty> stnList: stnTimeTable){
				for(NsharpSoundingElementStateProperty sndProp: stnList){
					if(sndProp.stnDescription.equals(stnId) ){
						//time line state should be active, but stnId  state should based on its peer in other  time line
						newSndPropElem = 
							new NsharpSoundingElementStateProperty(elementDesc, NsharpConstants.State.AVAIL,stnId, tmLine,  stnInfo);
						//newSndPropElem.elementState = decideNewLoadElementState(newSndPropElem);
						stnList.add(newSndPropElem);
					}
					else{
						String elmDes= sndProp.stnDescription+" "+tmLine;
						dummySndPropElem = 
							new NsharpSoundingElementStateProperty(elmDes, NsharpConstants.State.NOTAVAIL,sndProp.stnDescription, tmLine,  sndProp.stnInfo);
						stnList.add(dummySndPropElem);
					}
					
					break; // only add one element for each list, break out of here.
				}
				// we have to sort time line
				Collections.sort(stnList,  new SndPropElementComparatorTimeLine());

			}
			//add new time line to NsharpTimeLineStateProperty
			NsharpTimeLineStateProperty tl = new NsharpTimeLineStateProperty(tmLine, NsharpConstants.State.ACTIVE);
			timeLineStateList.add(tl);
			Collections.sort(timeLineStateList,  new TimeLineStateComparator());
			currentTimeLineStateListIndex = timeLineStateList.indexOf(tl);
			currentStnStateListIndex = stnIndex;
		} else {
			//  an element with new time line and new stnId
			//  need to do both cases above
			//add new time line to NsharpTimeLineStateProperty
			NsharpTimeLineStateProperty tl = new NsharpTimeLineStateProperty(tmLine, NsharpConstants.State.ACTIVE);
			timeLineStateList.add(tl);
			Collections.sort(timeLineStateList,  new TimeLineStateComparator());
			currentTimeLineStateListIndex = timeLineStateList.indexOf(tl);
			//add new stn to stnStateList
			NsharpStationStateProperty stn = new NsharpStationStateProperty(stnId,NsharpConstants.State.ACTIVE,stnInfo);
			stnStateList.add(stn);
			Collections.sort(stnStateList,  new StationStatePropertyComparator());
			currentStnStateListIndex = stnStateList.indexOf(stn);
			
			// need a new stn time line list to stnTimeTable
			List<NsharpSoundingElementStateProperty> newList = new ArrayList<NsharpSoundingElementStateProperty>();	
			if(stnTimeTable.size()>0){
				// do new stn id case first, add element for each existing time line
				for(NsharpTimeLineStateProperty sndProp: timeLineStateList){
					if(tmLine.equals(sndProp.timeDescription)){
						//stnId state should be active, but time line state should based on its peer in other stn
						newSndPropElem = 
							new NsharpSoundingElementStateProperty(elementDesc, NsharpConstants.State.AVAIL,stnId, tmLine,  stnInfo);
						
						newList.add(newSndPropElem);
					}
					else{
						//create time line for  not avail sounding profiles
						String elmDes= stnId+" "+sndProp.timeDescription;
						dummySndPropElem = 
							new NsharpSoundingElementStateProperty(elmDes, NsharpConstants.State.NOTAVAIL,stnId,  sndProp.timeDescription,  stnInfo);
						newList.add(dummySndPropElem);
					}
					
				}
				// we do not have to sort time line as we add it according to newly sorted timeLineStateList
				//now add new time line element for each existing stns, except the new one, 
				//Note that we have NOT add "newList" to stnTimeTable yet, so just loop through "current" table.
				for(List<NsharpSoundingElementStateProperty> stnList: stnTimeTable){
					for(NsharpSoundingElementStateProperty sndProp: stnList){
						String elmDes= sndProp.stnDescription+" "+tmLine;
						dummySndPropElem = 
							new NsharpSoundingElementStateProperty(elmDes, NsharpConstants.State.NOTAVAIL,sndProp.stnDescription, tmLine,  sndProp.stnInfo);
						stnList.add(dummySndPropElem);
						break; // only add one element for each list, break out of here.
					}
					// we have to sort time line
					Collections.sort(stnList,  new SndPropElementComparatorTimeLine());
				}
				//finally, add this new stn list to table
				stnTimeTable.add(newList);
				//we have to sort stnId in stnTimeTable (outer list)
				Collections.sort(stnTimeTable,  new SndPropElementComparatorStnId());
			}
			else{
				//this is the case, we are adding first element to stnTimeTable
				newSndPropElem = 
					new NsharpSoundingElementStateProperty(elementDesc, NsharpConstants.State.AVAIL,stnId, tmLine,stnInfo);
				newList.add(newSndPropElem);
				stnTimeTable.add(newList);
				curSndProfileProp=newSndPropElem;	
				return;
			}
		}
		findCurrentElementIndexesAfterConfig();
		setCurSndProfileProp();
	}
	private void updateStnTimeTableAndList(){
		List<Integer> removeIntLst = new ArrayList<Integer>();
		if(stnTimeTable.size() > 0 && stnTimeTable.get(0).size()>0){
			
			int timeLinePerStn = stnTimeTable.get(0).size();
			for(int i=0; i< timeLinePerStn; i++){
				boolean found =false;
				for(List<NsharpSoundingElementStateProperty> stnList: stnTimeTable){
					NsharpSoundingElementStateProperty elm= stnList.get(i);
					if(elm.getElementState() !=NsharpConstants.State.NOTAVAIL){
						found = true;
						break;
					}
				}
				if(!found){
					//remove this time line from all stns' time list
					removeIntLst.add(i);
				}
			}
			//remove timeline backward from list
			for(int i = removeIntLst.size()-1; i >=0; i--){
				for(List<NsharpSoundingElementStateProperty> stnTimeList: stnTimeTable){
					stnTimeList.remove(stnTimeList.get(removeIntLst.get(i)));
					
				}
				//update timeLineStateList as well
				timeLineStateList.remove(timeLineStateList.get(removeIntLst.get(i)));
			}
		}
		removeIntLst.clear();
		//List<List<NsharpSoundingElementStateProperty>> removeLst = new ArrayList<List<NsharpSoundingElementStateProperty>> ();
		for(List<NsharpSoundingElementStateProperty> stnList: stnTimeTable){
			if(stnList.size() <=0 ){
				//after remove time line above, it is possible that stn does not have any time line
				//removeLst.add(stnList);
				removeIntLst.add(stnTimeTable.indexOf(stnList));
				continue;
			}
			boolean found =false;
			for(NsharpSoundingElementStateProperty elm: stnList){
				if(elm.getElementState() !=NsharpConstants.State.NOTAVAIL){
					found = true;
					break;
				}
			}
			if(!found){
				//all stns time lines are unloaded, remove this stn
				//removeLst.add(stnList);
				removeIntLst.add(stnTimeTable.indexOf(stnList));
			}
		}
		for(int i = removeIntLst.size()-1; i >=0; i--){
			stnTimeTable.remove(stnTimeTable.get(removeIntLst.get(i)));
			//update stnStateList as well
			stnStateList.remove(stnStateList.get(removeIntLst.get(i)));
			
		}
	}
	
	private void setCurSndProfileProp() {	
		if(currentTimeLineStateListIndex <0 || currentTimeLineStateListIndex >= timeLineStateList.size()||
				currentStnStateListIndex < 0 || currentStnStateListIndex >= stnStateList.size()){
			curSndProfileProp = null;
			preSndProfileProp = null;
		}
		else {
			preSndProfileProp = curSndProfileProp;
			curSndProfileProp = stnTimeTable.get(currentStnStateListIndex).get(currentTimeLineStateListIndex);
		}		
	}
	/*
	 * Note: each deletingTimeList's element (a String) is used to mapped to elementDescription (a String)
	 * of dataTimelineList's elements, which has same value as key (a String) of dataTimelineSndLysListMap
	 */
	public boolean deleteRsc(List<String> deletingDataTimeList){
		boolean curSndDeleted = false;
		for(String dataTmLine: deletingDataTimeList){
			if(curSndProfileProp.elementDescription.equals(dataTmLine)){
				curSndDeleted = true;
			}
			dataTimelineSndLysListMap.remove(dataTmLine);
			//set state to NOTAVAIL from stnTimeTable
			boolean setdone = false;
			for(List<NsharpSoundingElementStateProperty> stnList: stnTimeTable){
				for(NsharpSoundingElementStateProperty elm: stnList){
					if(dataTmLine.equals(elm.elementDescription)){
						elm.setElementState(NsharpConstants.State.NOTAVAIL);
						updateStnTimeTableAndList();						
						setdone = true;
						break;
					}
				}
				if(setdone)
					break;
			}
		}
		if(curSndDeleted){
			//this is the case that we are deleting current snd, so, a new current snd should be selected
			curSndProfileProp = null;
			findCurrentElementIndexesAfterDelete();
			setCurSndProfileProp();
		}
		else{
			// currentTimeLineStateListIndex and currentStnStateListIndex may be not point to right element
			//after some elements are deleted.
			// re-find them
			currentStnStateListIndex = -1;
			currentTimeLineStateListIndex = -1;
			if(curSndProfileProp!= null){
				for(List<NsharpSoundingElementStateProperty> stnList: stnTimeTable){
					for(NsharpSoundingElementStateProperty elm: stnList){
						if(curSndProfileProp.elementDescription.equals(elm.elementDescription)){
							currentTimeLineStateListIndex = stnList.indexOf(elm);
							currentStnStateListIndex = stnTimeTable.indexOf(stnList);
							break;
						}
					}
				}
			}
		}
		
		setCurrentSoundingLayerInfo();
		return curSndDeleted;
		// anything more to do?
	}
	public void deleteRscAll(){
		NsharpMapResource nsharpMapResource = NsharpMapResource.getOrCreateNsharpMapResource();
		nsharpMapResource.setPoints(null);
		cleanUpRsc();
		//System.out.println("NsharpSkewTResource deleteRscAll() called");
	}
	
	private void setCurrentSoundingLayerInfo() {
		if(currentTimeLineStateListIndex >=0 && currentStnStateListIndex>=0 ){
			pickedStnInfoStr = stnTimeTable.get( currentStnStateListIndex).get( currentTimeLineStateListIndex).getElementDescription();
			pickedStnInfo = stnTimeTable.get( currentStnStateListIndex).get( currentTimeLineStateListIndex).getStnInfo();

			if(stnTimeTable.get( currentStnStateListIndex).get( currentTimeLineStateListIndex).elementState == NsharpConstants.State.AVAIL){

				if(overlayIsOn){ 
					previousSoundingLys = soundingLys;
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
					soundingLys = dataTimelineSndLysListMap.get(pickedStnInfoStr);

				}

			}
			else{
				previousSoundingLys = null;
				soundingLys = null;
			}
		}
		else{
			pickedStnInfoStr= null;
			previousSoundingLys = null;
			soundingLys = null;
			pickedStnInfo= null;
		}
	}
	public void addRsc(Map<String, List<NcSoundingLayer>> soundMap,  NsharpStationInfo stnInfo){
		//make sure not adding duplicated sounding data
		System.out.println("NsharpSkewTResource addRsc called");
		Set<String> duplicateKeys = new HashSet<String>();
		for(String key: soundMap.keySet()) {
			if(dataTimelineSndLysListMap.containsKey(key)==true)
				duplicateKeys.add(key);
		}
		for(String key: duplicateKeys) {
			soundMap.remove(key);
		}
		if(soundMap.size() <=0){
			return;
		}
		//add new data 
		dataTimelineSndLysListMap.putAll(soundMap);
		Set<String> dataTimelineSet = soundMap.keySet();	
		String[] tempTimeLineArr = dataTimelineSet.toArray(new String[dataTimelineSet.size()]);
		Arrays.sort(tempTimeLineArr);
		// ADD new time line(s) to dataTimelineList, stationIdList,curAggregateTimeLineList
		for (int i=0; i< tempTimeLineArr.length; i++){
			//System.out.println(newElm.elementDescription);
			String elmDesc = tempTimeLineArr[i].toString();
			String stnId = elmDesc.substring(0,elmDesc.indexOf(" "));
			String timeLine= elmDesc.substring(elmDesc.indexOf(" ")+1);
			//add time line to stnTimeTable and set its index
			addElementToTableAndLists(elmDesc,stnId,timeLine,stnInfo);
		}
		//Set default parcel trace data
		parcelList.clear();
		ParcelData defParcel = new ParcelData();
		defParcel.setParcelType(NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE );
		defParcel.setParcelLayerPressure(NsharpNativeConstants.MU_LAYER);
		parcelList.add(defParcel);
		setCurrentSoundingLayerInfo();
		resetData();

		//set total time line group and stn id list page  number
		int numTimeLinePerPage = (NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END)/NsharpConstants.CHAR_HEIGHT;
		
		totalTimeLinePage = timeLineStateList.size()/numTimeLinePerPage + 1; //NEW CODE
		curTimeLinePage = currentTimeLineStateListIndex/numTimeLinePerPage + 1; //NEW CODE
		totalStnIdPage = stnStateList.size()/numTimeLinePerPage + 1; //NEW CODE
		curStnIdPage= currentStnStateListIndex/numTimeLinePerPage + 1; //NEW CODE
		
		//System.out.println("totalTimeGroupPage ="+totalTimeGroupPage+ " curTimeGroupPage:"+ this.curTimeGroupPage);
		NsharpSkewTDisplay renderableDisplay = (NsharpSkewTDisplay) descriptor.getRenderableDisplay();
		if(renderableDisplay != null) {
		    IDisplayPaneContainer editor = renderableDisplay.getContainer();
		    if(editor instanceof NsharpSkewTEditor) {
		       int editorNum = ((NsharpSkewTEditor) editor).getEditorNum();
		       renderableDisplay.setEditorNum(editorNum);
		    }
		}
		//set data time to descriptor
		//this is necessary for looping	
        if ((descriptor.getFramesInfo().getFrameCount() == 0)&& !getTimeMatcher) {
            //DataTime[] dataTimes = new DataTime[dataTimelineList.size()];
        	//Chin Note: we just have to do this once and set dataTimes size bigger than 1. 
        	//Nsharp handles changing frame itself. It just need system to send change frame notice. 
        	//That is happened at NsharpSkewTDescriptor.checkDrawTime().
            DataTime[] dataTimes = new DataTime[2/*stnTimeTable.size()*/];
            Date now = new Date();
            for(int k =0; k < 2/*stnTimeTable.size()*/ ; k++){
                dataTimes[k]= new DataTime(now, k);
            }
            //no need to get a descriptor from a renderableDispaly since we have a descriptor
            descriptor.setDataTimes(dataTimes);
            getTimeMatcher=true;
        }
        
		NsharpShowTextDialog textarea =  NsharpShowTextDialog.getAccess();
		if(textarea != null){
			textarea.refreshTextData();
		}
		currentGraphMode=NsharpPaletteWindow.getCurrentGraphMode();
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		if(bkRsc!=null)
			bkRsc.setCurrentGraphMode(currentGraphMode);
		deepCopyDataMap(this.dataTimelineSndLysListMap,this.originalDataTimelineSndLysListMap);
		NsharpSkewTEditor editor = NsharpSkewTEditor.getActiveNsharpEditor();
		if (editor != null) {
			editor.refresh();
		}
		
	}

 	public String getPickedStnInfoStr() {
		return pickedStnInfoStr;
	}


	public void handleUserClickOnStationId(Coordinate c) {
		int numStnIdPerPage = (NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END)/NsharpConstants.CHAR_HEIGHT;
		//first to find if it is for change to next page, or change sorting
		//System.out.println("numTimeLinePerPage="+numTimeLinePerPage+"gap="+(NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END));
		int index =((int)(c.y - NsharpConstants.DATA_TIMELINE_REC_Y_ORIG))/ NsharpConstants.CHAR_HEIGHT;
		if(index == 0 ){
			if( totalStnIdPage == 1) 
				return;

			curStnIdPage++;
			if(curStnIdPage>totalStnIdPage)
				curStnIdPage=1;

			return;
		}
		// recalculate index for time line
		index =((int)(c.y - NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END))/ NsharpConstants.CHAR_HEIGHT +
		 			(curStnIdPage-1)* numStnIdPerPage ;	
		
		if( index  < this.stnStateList.size() ){
			switch(stnStateList.get(index).stnState){
			
			case INACTIVE:
				stnStateList.get(index).stnState = NsharpConstants.State.ACTIVE;
				break;
			case ACTIVE:
				stnStateList.get(index).stnState = NsharpConstants.State.INACTIVE;
				
				break;
			default:
				return;
			}
			findCurrentElementIndexesAfterConfig();
			setCurSndProfileProp();
			setCurrentSoundingLayerInfo();
			resetData();

		}
	}
	
	public void handleUserClickOnTimeLine(Coordinate c) {
		int numTimeLinePerPage = (NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END)/NsharpConstants.CHAR_HEIGHT;
		//first to find if it is for change to next page, or change sorting
		//System.out.println("numTimeLinePerPage="+numTimeLinePerPage+"gap="+(NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END));
		int index =((int)(c.y - NsharpConstants.DATA_TIMELINE_REC_Y_ORIG))/ NsharpConstants.CHAR_HEIGHT;
		if(index == 0 ){
			if( totalTimeLinePage == 1) //
				return;

			curTimeLinePage++;
			if(curTimeLinePage>totalTimeLinePage)
				curTimeLinePage=1;
			return;
		}
		// recalculate index for time line
		index =((int)(c.y - NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END))/ NsharpConstants.CHAR_HEIGHT +
		 			(curTimeLinePage-1)* numTimeLinePerPage ;	
		
		if( index  < timeLineStateList.size() ){
			switch(timeLineStateList.get(index).timeState){
			case INACTIVE:
				timeLineStateList.get(index).timeState = NsharpConstants.State.ACTIVE;
				break;
			case ACTIVE:
				timeLineStateList.get(index).timeState = NsharpConstants.State.INACTIVE;
				break;
			default:
				return;

			}
		}
		//
		findCurrentElementIndexesAfterConfig();
		setCurSndProfileProp();
		setCurrentSoundingLayerInfo();
		resetData();
		
		if(compareStnIsOn){
			createRscHodoWindShapeAll();
			createRscPressTempCurveShapeAll();
		}
	}
	
		
	private void moveTimeLineIndexBackward(){
		previousTimeLineStateListIndex = currentTimeLineStateListIndex;
		int counter=0;
		while(true){
			currentTimeLineStateListIndex++;
			currentTimeLineStateListIndex = currentTimeLineStateListIndex % this.timeLineStateList.size();
			counter++;
			if(counter > timeLineStateList.size())
				break;
			if(timeLineStateList.get(currentTimeLineStateListIndex).getTimeState() == NsharpConstants.State.ACTIVE &&
					stnTimeTable.get(currentStnStateListIndex).get(currentTimeLineStateListIndex).elementState == NsharpConstants.State.AVAIL){
				break;//out of while loop
			}
			
		}
	}
	private void moveTimeLineIndexForward (){
		previousTimeLineStateListIndex = currentTimeLineStateListIndex;
		int counter=0;
		while(true){
			currentTimeLineStateListIndex= currentTimeLineStateListIndex + this.timeLineStateList.size();// so, we wont get a negative number
			currentTimeLineStateListIndex--;
			currentTimeLineStateListIndex = currentTimeLineStateListIndex % this.timeLineStateList.size();
			counter++;
			if(counter > timeLineStateList.size())
				break;
			if(timeLineStateList.get(currentTimeLineStateListIndex).getTimeState() == NsharpConstants.State.ACTIVE &&
					stnTimeTable.get(currentStnStateListIndex).get(currentTimeLineStateListIndex).elementState == NsharpConstants.State.AVAIL){
				break;//out of while loop
			}
		}
		//System.out.println("timeline="+timeLineStateList.get(pickedTimeGroupIndex).getElementDescription());
	}
	private void moveTimeLineIndexCycle (){
		previousTimeLineStateListIndex = currentTimeLineStateListIndex;
		//Note: direction should only be NEXT or PREVIOUS
		int counter=0;
		while(true){
			counter++;
			if(counter > timeLineStateList.size()){
				currentTimeLineStateListIndex = previousTimeLineStateListIndex;
				break;
			}
			if(currentOpDirection == FrameChangeOperation.NEXT ){
				currentTimeLineStateListIndex--;
				if(currentTimeLineStateListIndex <= 0){
					//the end of forward direction, change direction to backward
					currentOpDirection = FrameChangeOperation.PREVIOUS;
					currentTimeLineStateListIndex=0;
				}

			}
			else{ // direction is FrameChangeOperation.PREVIOUS
				currentTimeLineStateListIndex++;
				if(currentTimeLineStateListIndex >= timeLineStateList.size()-1){
					//the end of backward direction, change direction to forward
					currentOpDirection = FrameChangeOperation.NEXT;
					currentTimeLineStateListIndex = timeLineStateList.size()-1;
				}
			}
			if(timeLineStateList.get(currentTimeLineStateListIndex).getTimeState() == NsharpConstants.State.ACTIVE &&
				stnTimeTable.get(currentStnStateListIndex).get(currentTimeLineStateListIndex).elementState == NsharpConstants.State.AVAIL){
						break;//out of while loop
			}
		}

	}
	/*
	 * Note: looping only apply to curAggregateTimeLineList NOT stationIdList
	 */
	public void setLoopingDataTimeLine(LoopProperties loopProperties) {
		//System.out.println("setLoopingDataTimeLine loopmode ="+loopProperties.getMode().toString());
		if( this.timeLineStateList.size()>0) { 
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
			
			int numTimeLinePerPage = (NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END)/NsharpConstants.CHAR_HEIGHT;
			curTimeLinePage = currentTimeLineStateListIndex/numTimeLinePerPage + 1;
			setCurSndProfileProp();
 			setCurrentSoundingLayerInfo();
 			resetData();
 			issueRefresh();
			//NsharpSkewTEditor editor = NsharpSkewTEditor.getActiveNsharpEditor();
			//if (editor != null) {
			//	editor.refresh();
			//}
		}
				
	}
	public enum LoopMode {
        Forward, Backward, Cycle
    };

 	private int getActiveTimeLineNumber(){
 		int n=0;
 		for (NsharpTimeLineStateProperty tm:timeLineStateList){
 			if(tm.timeState == NsharpConstants.State.ACTIVE){
 				n++;
 			}
 		}
 		return n;
 	}
 	private int getActiveStnNumber(){
 		int n=0;
 		for (NsharpStationStateProperty stn:stnStateList){
 			if(stn.stnState == NsharpConstants.State.ACTIVE){
 				n++;
 			}
 		}
 		return n;
 	}
 	public void setSteppingTimeLine(FrameChangeOperation operation, FrameChangeMode mode) {
 		if( this.timeLineStateList.size() > 0 && getActiveTimeLineNumber()>1/* && getAvailTimeLineNumber(currentStnStateListIndex)>1*/) { 
 			int targetIndex = currentTimeLineStateListIndex;
 			//previousTimeLineStateListIndex = currentTimeLineStateListIndex;
 			//preset index for LAST and FIRST operation
 			switch(operation){
 			case LAST: //the future-est time, at top of time line shown. set to -1, so in while loop, it starts from 0
 				targetIndex=-1;//
 				break;
 			case FIRST: //the oldest time, set to dataTimelineList.length, so in while loop, it starts from dataTimelineList.length-1
 				targetIndex = timeLineStateList.size(); 
 				break;
 			}

 			int counter=0;
 			while(true){
 				switch(operation){
 				case LAST: //the future-est time, at top of time line shown
 					targetIndex++;
 					break;
 				case FIRST: //the oldest time
 					targetIndex--; 
 					break;
 				case PREVIOUS:
 					targetIndex++;
 					targetIndex = targetIndex % this.timeLineStateList.size(); 
 					break;
 				case NEXT:
 					// so, we wont get a negative number
 					targetIndex= targetIndex + this.timeLineStateList.size();
 					targetIndex--;
 					targetIndex = targetIndex % this.timeLineStateList.size();	
 					break;
 				}
 				counter++;
 				if(counter >= timeLineStateList.size())
 					return; // looped through whole list already, and index back to original
 				if(timeLineStateList.get(targetIndex).getTimeState() == NsharpConstants.State.ACTIVE) {
 					if(compareTmIsOn 
 					&& stnTimeTable.get(currentStnStateListIndex).get(targetIndex).elementState == NsharpConstants.State.NOTAVAIL){
 						continue;
 					}
 					else if(compareStnIsOn){
 						boolean found = false;
 						//find an active and available stn for this timeline and set is as current
 						for (int i=0; i < stnStateList.size(); i++){
 							if(stnStateList.get(i).stnState==NsharpConstants.State.ACTIVE &&
 									stnTimeTable.get(i).get(targetIndex).elementState == NsharpConstants.State.AVAIL){
 								currentStnStateListIndex = i;
 								found = true;
 								break;
 							}
 						}
 						if(!found){
 							currentStnStateListIndex = -1;
 						}
 						//no matter we find current stn or not
 						//we should get out of here
 						break;						
 					}
 					else{
 						break;
 					}
 				}
 			}
 			previousTimeLineStateListIndex = currentTimeLineStateListIndex;
 			currentTimeLineStateListIndex = targetIndex;
 			int numTimeLinePerPage = (NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END)/NsharpConstants.CHAR_HEIGHT;
 			curTimeLinePage = currentTimeLineStateListIndex/numTimeLinePerPage + 1;
 			setCurSndProfileProp();
 			setCurrentSoundingLayerInfo();
 			resetData();
 			
 			NsharpSkewTEditor editor = NsharpSkewTEditor.getActiveNsharpEditor();
 			if (editor != null) {
 				editor.refresh();
 			}
 		}
 	}
 	
 	/*
 	 * Stn index stepping is only controlled by up/down arrow keys, down key = NEXT operation, up key = PREVIOUS operation
 	 */
 	public void setSteppingStnIdList(FrameChangeOperation operation) {
 		if( this.stnStateList.size() > 0 && getActiveStnNumber()>1/* && getAvailStnNumber(currentTimeLineStateListIndex) > 1*/){ 
 			
 			int counter=0;
 			while(true){
 				switch(operation){
  				case PREVIOUS:
  					currentStnStateListIndex= currentStnStateListIndex + this.stnStateList.size();
 					currentStnStateListIndex--;
 					currentStnStateListIndex = currentStnStateListIndex % this.stnStateList.size();	
 					break;
 				case NEXT:
 					// so, we wont get a negative number					
 					currentStnStateListIndex++;
 					currentStnStateListIndex = currentStnStateListIndex % this.stnStateList.size(); 
 					break;
 				}
 				counter++;
 				//System.out.println("counter = "+ counter);
 				if(counter >= stnStateList.size())
 					return; // looped through whole list already, and index back to original
 				if(stnStateList.get(currentStnStateListIndex).stnState == NsharpConstants.State.ACTIVE){
 					if (compareStnIsOn 
 							&& stnTimeTable.get(currentStnStateListIndex).get(currentTimeLineStateListIndex).elementState == NsharpConstants.State.NOTAVAIL){
 						continue;
 					}
 					else if(compareTmIsOn){
 						boolean found = false;
 						//find an active and available timeline for this stn and set is as current
 						for (int i=0; i < timeLineStateList.size(); i++){
 							if(timeLineStateList.get(i).timeState==NsharpConstants.State.ACTIVE &&
 									stnTimeTable.get(currentStnStateListIndex).get(i).elementState == NsharpConstants.State.AVAIL){
 								currentTimeLineStateListIndex = i;
 								found = true;
 								break;
 							}
 						}
 						if(!found){
 							currentTimeLineStateListIndex = -1;
 						}
 						//no matter we find current time line for this stn or not
 						//we should get out of here
 						break;				
 					}
 					else
 						break;
 				}
 			}
 			int numTimeLinePerPage = (NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END)/NsharpConstants.CHAR_HEIGHT;
 			curStnIdPage = currentStnStateListIndex/numTimeLinePerPage + 1;
 			setCurSndProfileProp();
 			setCurrentSoundingLayerInfo();
 			resetData();
 			
 			NsharpSkewTEditor editor = NsharpSkewTEditor.getActiveNsharpEditor();
 			if (editor != null) {
 				editor.refresh();
 			}
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
		Coordinate inC = NsharpWxMath.reverseSkewTXY(WGc.unMap(c));
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
					closeptC =  NsharpWxMath.getSkewTXY(pickedPressure, pickedTemp);
					closeptC = WGc.map(closeptC);
					currentTempCurveType = TEMP_TYPE;	
					//System.out.println("picked pressure "+ pickedPressure + " temp " +pickedTemp);
				}
				else {
					closeptC =  NsharpWxMath.getSkewTXY(pickedPressure, pickedDewpoint);
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
		Coordinate c1 = NsharpWxMath.getSkewTXY(aboveLayerPressure, plotAboveT);                
		c1.x = world.mapX(c1.x);
		c1.y = world.mapY(c1.y);
		target.drawLine(c1.x, c1.y, 0.0, interactiveTempPointCoordinate.x, interactiveTempPointCoordinate.y, 0.0, color,
				commonLinewidth, LineStyle.DASHED);
		c1 = NsharpWxMath.getSkewTXY(belowLayerPressure,plotBelowT);            
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
		nsharpNative.populateSndgData(rawSndLysLst);		

		List<NcSoundingLayer> mySndLst = new ArrayList<NcSoundingLayer>();
		// add top layer
		try {
			//here a shallowCopy is enough
			newLayer = (NcSoundingLayer) rawSndLysLst.get(0).clone();
			mySndLst.add(newLayer);
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		} 
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
    	System.out.println("NsharpSkewTResource constructed");
        this.dataTimes = new ArrayList<DataTime>();
        this.soundingMap = new HashMap<Date, SoundingParams>();
        elementColorMap.put(NsharpConstants.State.CURRENT,NsharpConstants.color_green); //green
        elementColorMap.put(NsharpConstants.State.ACTIVE,NsharpConstants.color_yellow);//cyan
        elementColorMap.put(NsharpConstants.State.INACTIVE,NsharpConstants.color_white);//white
        elementColorMap.put(NsharpConstants.State.NOTAVAIL,NsharpConstants.color_red);//white
        elementColorMap.put(NsharpConstants.State.OVERLAY,NsharpConstants.color_red);//red
        elementColorMap.put(NsharpConstants.State.AVAIL,NsharpConstants.color_yellow);//white
        nsharpNative = new NsharpNative();
		//based on BigNsharp storm slinky color used and gempak color definition
		stormSlinkyColorMap.put(new Integer(3),NsharpConstants.color_green); //green
		stormSlinkyColorMap.put(new Integer(7),NsharpConstants.color_magenta);
		stormSlinkyColorMap.put(new Integer(6),NsharpConstants.color_cyan);
		stormSlinkyColorMap.put(new Integer(13),NsharpConstants.color_violet_md);
		stormSlinkyColorMap.put(new Integer(20),NsharpConstants.color_yellow_DK);
		stormSlinkyColorMap.put(new Integer(27),NsharpConstants.color_cyan_md);
		currentGraphMode=NsharpPaletteWindow.getCurrentGraphMode();
		
		// new for configMgr
		configMgr = NsharpConfigManager.getInstance();
		configStore = configMgr.retrieveNsharpConfigStoreFromFs();
		graphConfigProperty = configStore.getGraphProperty();
		int tempOffset = graphConfigProperty.getTempOffset();
		NsharpWxMath.setTempOffset(tempOffset);
		linePropertyMap = configStore.getLinePropertyMap();
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
    	if(intpSndLst!= null)
    		intpSndLst.clear();
    	if(stnTimeTable != null){
    		for(List<NsharpSoundingElementStateProperty> stnList: stnTimeTable){
    			stnList.clear();
    		}
    		stnTimeTable.clear();
    	}
    	if(timeLineStateList != null)
    		timeLineStateList.clear();
    	if(stnStateList != null)
    		stnStateList.clear();
    	curSndProfileProp = null;
    	preSndProfileProp = null;
    	currentTextPage = 1;
    	currentInsetPage = 1;
    	currentParcel = NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;
    	currentParcelLayerPressure = NsharpNativeConstants.MU_LAYER;
    }
	@Override
	protected void disposeInternal() {
		System.out.println("NsharpSkewTResource disposeInternal called");
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
		intpSndLst = null;
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
		System.out.println("NsharpSkewTResource initInternal called");
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
	private void paintIcing(double zoomLevel,IGraphicsTarget target) throws VizException{
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		if(bkRsc!= null)
		{
			NsharpIcingBackground icingBk = bkRsc.getIcingBackground();

			WGraphics plotWorld = icingBk.getWorld();
			target.setupClippingPlane(icingBk.getPe());
			try {
				
				if((graphConfigProperty!=null && graphConfigProperty.isWindBarb() == true) || graphConfigProperty== null) {
					plotWorld.setWorldCoordinates(NsharpConstants.ICING_RELATIVE_HUMIDITY_LEFT, icingBk.toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM),        		
							NsharpConstants.ICING_RELATIVE_HUMIDITY_RIGHT, icingBk.toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_TOP));
					NsharpLineProperty lp =linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_WIND_BARB]);
					
					drawNsharpWindBarb(target, zoomLevel, plotWorld,lp.getLineColor() /*NsharpConstants.color_yellow*/, this.soundingLys, 90, NsharpConstants.ICING_PRESSURE_LEVEL_TOP);
				}
			} catch (VizException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			//Chin NOTE: icining wireframeshapes are created dynamically ONLY when icing display is to be shown
			//However, Skewt wireframeshapes are created when new sounding is loaded.
			if(icingRHShape==null){
				// current WorldCoordinates for RH already loaded
				plotWorld.setWorldCoordinates(NsharpConstants.ICING_RELATIVE_HUMIDITY_LEFT, icingBk.toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_TOP),        		
						NsharpConstants.ICING_RELATIVE_HUMIDITY_RIGHT, icingBk.toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM));
				
				createIcingRHShape(plotWorld);
			}
			if(icingRHShape != null){
				NsharpLineProperty lp =linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_ICING_RH]);			
				target.drawWireframeShape(icingRHShape,lp.getLineColor(), lp.getLineWidth(),lp.getLineStyle(),font10);
			}
			if(icingTempShape==null){
				plotWorld.setWorldCoordinates(NsharpConstants.ICING_TEMPERATURE_LEFT, icingBk.toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_TOP),        		
						NsharpConstants.ICING_TEMPERATURE_RIGHT, icingBk.toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM));
				createIcingTempShape(plotWorld);
			}
			if(icingTempShape != null){
				NsharpLineProperty lp =linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_ICING_TEMP]);			
				target.drawWireframeShape(icingTempShape,lp.getLineColor(), lp.getLineWidth(),lp.getLineStyle(),font10);
			}
			if(icingEPIShape==null){
				plotWorld.setWorldCoordinates(NsharpConstants.ICING_TEMPERATURE_LEFT, icingBk.toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_TOP),        		
						NsharpConstants.ICING_TEMPERATURE_RIGHT, icingBk.toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM));
				createIcingEPIShape(plotWorld);
			}
			if(icingEPIShape != null){
				NsharpLineProperty lp =linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_ICING_EPI]);			
				target.drawWireframeShape(icingEPIShape,lp.getLineColor(), lp.getLineWidth(),lp.getLineStyle(),font10);
			}
			
			target.clearClippingPlane();
		}
	}
	private void paintTurbulence(double zoomLevel,IGraphicsTarget target) throws VizException{
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		if(bkRsc!= null)
		{
			NsharpTurbulenceBackground turbBk = bkRsc.getTurbBackground();

			WGraphics plotWorld = turbBk.getWorld();
			target.setupClippingPlane(turbBk.getPe());
			//Chin NOTE: turbulence wireframeshapes are created dynamically ONLY when turbulence display is to be shown
			//However, Skewt wireframeshapes are created when new sounding is loaded.
			try {
				//Chin:NOTE: LN Richardson number is plotted with positive number increase to left and netagive number decrease to its roght side.
				// Therefore, we have to set its world X coordintion in a reverse way as plotting Icing wind barb.
				if((graphConfigProperty!=null && graphConfigProperty.isWindBarb() == true) || graphConfigProperty== null) {
					plotWorld.setWorldCoordinates(NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_RIGHT, turbBk.toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_BOTTOM),        		
							NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_LEFT, turbBk.toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_TOP));
					NsharpLineProperty lp =linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_WIND_BARB]);
					drawNsharpWindBarb(target, zoomLevel, plotWorld, lp.getLineColor(), this.soundingLys, 7, NsharpConstants.TURBULENCE_PRESSURE_LEVEL_TOP);
				} 
			}catch (VizException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if(turbLnShape==null || turbWindShearShape==null){
				createTurbulenceShapes(plotWorld);
			}
			if(turbLnShape != null){
				NsharpLineProperty lp =linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_TURBULENCE_LN]);	
				target.drawWireframeShape(turbLnShape, lp.getLineColor(), lp.getLineWidth(),lp.getLineStyle(),font10);
			
			}
			if(turbWindShearShape != null){
				NsharpLineProperty lp =linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_TURBULENCE_WS]);	
				target.drawWireframeShape(turbWindShearShape, lp.getLineColor(), lp.getLineWidth(),lp.getLineStyle(),font10);
			}
			
			target.clearClippingPlane(); 
		}
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
    		bkRsc.getIcingBackground().setCurrentFont(currentFont10Size);
    	}
    	if(zoomLevel != currentZoomLevel)
    	{
    		currentZoomLevel = zoomLevel;
    		magnifyFont(zoomLevel);
    		bkRsc.getSkewTBackground().magnifyFont(zoomLevel);
    		bkRsc.getHodoBackground().magnifyFont(zoomLevel);
    		bkRsc.getIcingBackground().magnifyFont(zoomLevel);
    	}
    	WGraphics plotWorld;
    	PixelExtent extent;
    	//configD = NsharpParametersSelectionConfigDialog.getAccess();
    	if((soundingLys != null) && (soundingLys.size()>= 4))
    	{
    		if(currentGraphMode== NsharpConstants.GRAPH_SKEWT){
    			plotWorld = bkRsc.getSkewTBackground().getWorld();


    			extent = new PixelExtent(bkRsc.getSkewTBackground()
    					.getRectangle());
    			target.setupClippingPlane(extent);
    			//plot temp curve, when constructing pressureTempRscShapeList, it already considered 
    			// comparison, overlay, etc..so, just draw it.
    			for(ShapeAndLineProperty shapeNColor: pressureTempRscShapeList){
    				target.drawWireframeShape(shapeNColor.shape, shapeNColor.lp.getLineColor(), shapeNColor.lp.getLineWidth(), shapeNColor.lp.getLineStyle(),font10);//commonLinewidth*2,commonLineStyle,font10);
    			}
  			if(graphConfigProperty != null ){        		        		
    				if(graphConfigProperty.isTemp() == true && !compareStnIsOn){
    					if(editGraphOn)
    						plotPressureTempEditPoints(target, plotWorld, NsharpConstants.color_red, TEMP_TYPE, this.soundingLys);
    				}
    				// dew point curve
    				if(graphConfigProperty.isDewp() == true && !compareStnIsOn){
    					if(editGraphOn)
    						plotPressureTempEditPoints(target, plotWorld, NsharpConstants.color_green, DEWPOINT_TYPE, this.soundingLys);
    				}
    				//plot wetbulb trace
    				if(graphConfigProperty.isWetBulb() == true && !compareStnIsOn){
    					NsharpLineProperty lp =linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_WETBULB]);
    					target.drawWireframeShape(wetBulbTraceRscShape, lp.getLineColor(),lp.getLineWidth(),lp.getLineStyle(),font10);//NsharpConstants.color_cyan, commonLinewidth,commonLineStyle,font10);
    				}
    				//plot virtual temp trace
    				if(graphConfigProperty.isVTemp() == true && !compareStnIsOn){		
    					NsharpLineProperty lp =linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_VIRTUAL_TEMP]);
    					target.drawWireframeShape(vtempTraceCurveRscShape,lp.getLineColor(),lp.getLineWidth(),lp.getLineStyle(),font10);//NsharpConstants.color_red, commonLinewidth*2, LineStyle.DASHED,font10);
    				}
    				// parcel trace curve
    				if(graphConfigProperty.isParcel() == true && !compareStnIsOn){
    					if(soundingLys.size() > 0){
    						for (IWireframeShape shape: parcelTraceRscShapeList){
    							NsharpLineProperty lp =linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_PARCEL]);
    	    					target.drawWireframeShape(shape, lp.getLineColor(),lp.getLineWidth(),lp.getLineStyle(),font10);//NsharpConstants.color_white, commonLinewidth,LineStyle.DASHED,font10);
    						}
    					}
    				}
    				if(graphConfigProperty.isEffLayer() == true && !compareStnIsOn)
    					//draw effective layer lines
    					drawEffectiveLayerLines(target);

    				//cloud
    				if(graphConfigProperty.isCloud() == true && !compareStnIsOn){
    					if(cloudFMShape!= null)
    						target.drawShadedShape(cloudFMShape, 1f);
    					if(cloudFMLabelShape!= null)
    						target.drawWireframeShape(cloudFMLabelShape, NsharpConstants.color_chocolate, commonLinewidth*3,
    								commonLineStyle,font9);
    					if(cloudCEShape!= null)
    						target.drawShadedShape(cloudCEShape, 1f);
    				}

    				if(graphConfigProperty.isOmega() == true){
    					if(NsharpLoadDialog.getAccess()!= null && 
    							(NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.MODEL_SND ||
    									NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.PFC_SND )){
    						//plot omega
    						target.clearClippingPlane();
    						target.drawWireframeShape(omegaBkgShape, NsharpConstants.color_violet_red, commonLinewidth,
    								LineStyle.DASHED,font10);
    						target.drawWireframeShape(omegaRscShape, NsharpConstants.color_cyan, commonLinewidth,
    								commonLineStyle,font10);
    					}
    				}
    			}
    			else{
    				//by default, draw everything
    				if(!compareStnIsOn){
    					if(editGraphOn)
    						plotPressureTempEditPoints(target, plotWorld, NsharpConstants.color_red, TEMP_TYPE, this.soundingLys);
    					// dew point curve
    					if(editGraphOn)
    						plotPressureTempEditPoints(target, plotWorld, NsharpConstants.color_green, DEWPOINT_TYPE, this.soundingLys);
    					//plot wetbulb trace
    					NsharpLineProperty lp =linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_WETBULB]);
    					target.drawWireframeShape(wetBulbTraceRscShape, lp.getLineColor(),lp.getLineWidth(),lp.getLineStyle(),font10);//NsharpConstants.color_cyan, commonLinewidth,commonLineStyle,font10);
    					//plot virtual temp trace
    					lp =linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_VIRTUAL_TEMP]);
    					target.drawWireframeShape(vtempTraceCurveRscShape,lp.getLineColor(),lp.getLineWidth(),lp.getLineStyle(),font10);//NsharpConstants.color_red, commonLinewidth*2, LineStyle.DASHED,font10);

    					// parcel trace curve
    					if(soundingLys.size() > 0){
    						for (IWireframeShape shape: parcelTraceRscShapeList){
    							lp =linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_PARCEL]);
    	    					target.drawWireframeShape(shape, lp.getLineColor(),lp.getLineWidth(),lp.getLineStyle(),font10);//NsharpConstants.color_white, commonLinewidth,LineStyle.DASHED,font10);
     						}
    					}
    					//draw effective layer lines
    					drawEffectiveLayerLines(target);
    				}
    				if(NsharpLoadDialog.getAccess()!= null && 
    						(NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.MODEL_SND ||
    								NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.PFC_SND )){
    					//plot omega
    					target.clearClippingPlane();
    					target.drawWireframeShape(omegaBkgShape, NsharpConstants.color_violet_red, commonLinewidth,
    							LineStyle.DASHED,font10);
    					target.drawWireframeShape(omegaRscShape, NsharpConstants.color_cyan, commonLinewidth,
    							commonLineStyle,font10);
    				}
    			}
    			if(plotInteractiveTemp == true ){
    				plotNsharpInteractiveTemp( target,  zoomLevel,
    						plotWorld,  NsharpConstants.color_white);
    			}
    			target.clearClippingPlane();


    			// Wind Barb
    			if((graphConfigProperty!=null && graphConfigProperty.isWindBarb() == true) || graphConfigProperty== null) {
    				
    				if(overlayIsOn == true && this.previousSoundingLys!=null){
    					drawNsharpWindBarb(target, zoomLevel, plotWorld, linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_OVERLAY1]).getLineColor(), this.soundingLys, NsharpConstants.right - BARB_LENGTH,100);
    					if(!previousSoundingLys.equals(soundingLys))
    						drawNsharpWindBarb(target, zoomLevel, plotWorld,  linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_OVERLAY2]).getLineColor(), this.previousSoundingLys, NsharpConstants.right - BARB_LENGTH * 2.5,100);
    				}
    				else{
    					if(!compareStnIsOn ){
    						NsharpLineProperty lp =linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_WIND_BARB]);
        					drawNsharpWindBarb(target, zoomLevel, plotWorld, lp.getLineColor()/*NsharpConstants.color_yellow*/, this.soundingLys, NsharpConstants.right - BARB_LENGTH,100);
    					}
    				}

    			}
    			//drawHeightMark(target);
    			target.drawWireframeShape(heightMarkRscShape, NsharpConstants.color_red, 1, LineStyle.SOLID, font10);


    			//if(!compareStnIsOn){
    				//draw EL, LFC, LCL, FZL, -20C, -30C lines
    				drawLclLine(target);

    				// draw cursor data
    				if(cursorInSkewT== true){
    					if(curseToggledFontLevel < CURSER_STRING_OFF)
    						drawNsharpSkewtCursorData(target);
    					//draw dynamic temp, theta, height     	    		
    					drawNsharpSkewtDynamicData(target, zoomLevel, plotWorld);

    				}
    			//}

    		}// end of currentGraphMode= NsharpConstants.GRAPH_SKEWT
    		else if(currentGraphMode == NsharpConstants.GRAPH_ICING){
    			paintIcing( zoomLevel, target);
    		}
    		else if(currentGraphMode == NsharpConstants.GRAPH_TURB){
    			paintTurbulence( zoomLevel, target);
    		}
    		
    		//wind box background and wind
    		target.drawWireframeShape(windBoxBkgShape, NsharpConstants.color_white,
                    0.5F, LineStyle.DOTS, font10);
    		for(ShapeAndLineProperty shapeNColor: windBoxWindRscShapeList){
				target.drawWireframeShape(shapeNColor.shape, shapeNColor.lp.getLineColor(), commonLinewidth,commonLineStyle,font10);
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
    		drawNsharpTimelineTitle(target);
    		drawNsharpStationIdTitle(target);
    		drawNsharpDataFilelabel(target, zoomLevel);
    		
    		//plot HODO
    		plotWorld = bkRsc.getHodoBackground().getWorld();
    		extent = new PixelExtent(bkRsc.getHodoBackground()
    				.getRectangle());
    		target.setupClippingPlane(extent);
    		if(((graphConfigProperty != null )&& graphConfigProperty.isHodo())|| (graphConfigProperty == null)){
    		for(ShapeAndLineProperty shapeNColor: hodoWindRscShapeList){
				target.drawWireframeShape(shapeNColor.shape, shapeNColor.lp.getLineColor(), commonLinewidth*2,commonLineStyle,font10);
    		}
    		}
    		if(editGraphOn && !compareStnIsOn)
    			plotHodoEditPoints( target,plotWorld, NsharpConstants.color_white);
    		if(!compareStnIsOn){
    			plotNsharpHodoVectors(target, zoomLevel, plotWorld, null, false);
    		} 
    		if(cursorInHodo){
    			target.clearClippingPlane();
    			drawHodoDynamicData(target, zoomLevel, plotWorld);
    		}
    		
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
    		//drawNsharpDataTimelines(target, plotWorld, bkRsc.getDataTimelineBackground().getRectangle());

    		drawNsharpTimelinBox(target, plotWorld, bkRsc.getDataTimelineBackground().getRectangle());
    		
    		//plot station id
    		plotWorld = bkRsc.getStationIdBackground().getWorld();
    		drawNsharpStationIdBox(target, plotWorld, bkRsc.getStationIdBackground().getRectangle());
    		
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
    		
    		//data time title
    		drawNsharpDataUnavailMessage(target);
    		drawNsharpTimelineTitle(target);
    		drawNsharpStationIdTitle(target);
    		drawNsharpDataFilelabel(target, zoomLevel);
    		//plot data time line
    		plotWorld = bkRsc.getDataTimelineBackground().getWorld();
    		//drawNsharpDataTimelines(target, plotWorld, bkRsc.getDataTimelineBackground().getRectangle());

    		drawNsharpTimelinBox(target, plotWorld, bkRsc.getDataTimelineBackground().getRectangle());
    		
    		//plot station id
    		plotWorld = bkRsc.getStationIdBackground().getWorld();
    		drawNsharpStationIdBox(target, plotWorld, bkRsc.getStationIdBackground().getRectangle());
    		
    		//plot color notations
    		plotWorld = bkRsc.getColorNotationsBackground().getWorld();
    		drawNsharpColorNotation(target, plotWorld, bkRsc.getColorNotationsBackground().getRectangle());
    	}
    }
    public static BufferedImage crop(BufferedImage src, int width, int height) {//throws IOException { 
        int x = src.getWidth()/2 - width/2;
        int y = src.getHeight()/2 - height/2;
        
        System.out.println("---" + src.getWidth() + " - " + src.getHeight() + " - " + x + " - " + y);
        
        BufferedImage clipping = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);//src.getType());  
        Graphics2D area = (Graphics2D) clipping.getGraphics().create();  
        //area.drawImage(src, 0, 0, clipping.getWidth(), clipping.getHeight(), x, y, x + clipping.getWidth(),  
        //    y + clipping.getHeight(), null);
        area.drawImage(src, 0, 0, clipping.getWidth(), clipping.getHeight(), 0, 0, x + clipping.getWidth(),  
                y + clipping.getHeight(), null);
        area.dispose(); 
        
        return clipping;
    }  
    
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
    private void drawNsharpDataUnavailMessage(IGraphicsTarget target) throws VizException{
    	double X = NsharpConstants.WIND_BOX_X_ORIG;
    	double Y = 80;
    	target.drawString(font12, "Sounding Data is NOT loaded for Current Station and Current Time Line", X, Y, 0.0,
                TextStyle.BOXED, NsharpConstants.color_red, HorizontalAlignment.LEFT,
                VerticalAlignment.MIDDLE, null);
    }
    private void drawNsharpDataFilelabel(IGraphicsTarget target, double zoomLevel)
            throws VizException {
    	double X;
    	double Y = 15;;
    	RGB pickedStnColor = NsharpConstants.color_green;   	
    	if(overlayIsOn){
    		if(preSndProfileProp!= null){
    			X = NsharpConstants.SKEWT_VIEW_X_END;
    			pickedStnColor = linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_OVERLAY2]).getLineColor();
    			String stnInfoStr = preSndProfileProp.elementDescription;
    			String latlonStr = Math.rint(preSndProfileProp.stnInfo.getLatitude()*100)/100 + "," + Math.rint(preSndProfileProp.stnInfo.getLongitude()*100)/100;
    			target.drawString(font10, stnInfoStr, X, Y, 0.0,
    	                TextStyle.NORMAL, pickedStnColor, HorizontalAlignment.LEFT,
    	                VerticalAlignment.MIDDLE, null);
    			target.drawString(font10, latlonStr, X, 3*Y, 0.0,
        				TextStyle.NORMAL, pickedStnColor, HorizontalAlignment.LEFT,
        				VerticalAlignment.MIDDLE, null);
    		}
    		pickedStnColor = linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_OVERLAY1]).getLineColor();
    	}
    	X = NsharpConstants.SKEWT_REC_X_ORIG+20;
    	if(pickedStnInfoStr!= null)
    		target.drawString(font10, pickedStnInfoStr, X, Y, 0.0,
                TextStyle.NORMAL, pickedStnColor, HorizontalAlignment.LEFT,
                VerticalAlignment.MIDDLE, null);
    	//Also draw stn lat/lon info string and sounding type string
    	if(pickedStnInfo != null){
    		String latlonStr = Math.rint(pickedStnInfo.getLatitude()*100)/100 + "," + Math.rint(pickedStnInfo.getLongitude()*100)/100;
    		target.drawString(font10, latlonStr, X, 3*Y, 0.0,
    				TextStyle.NORMAL, pickedStnColor, HorizontalAlignment.LEFT,
    				VerticalAlignment.MIDDLE, null);
    		
    		/*target.drawString(font10, "Sounding Type", NsharpConstants.OMEGA_X_TOP, Y, 0.0,
    				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
    				VerticalAlignment.MIDDLE, null);
    		String sndTypeStr = pickedStnInfo.getSndType();
    		int len = Math.min(12, sndTypeStr.length());
    		sndTypeStr = sndTypeStr.substring(0, len);
    		target.drawString(font10, sndTypeStr, NsharpConstants.OMEGA_X_TOP,3* Y, 0.0,
    				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
    				VerticalAlignment.MIDDLE, null);*/
    	}
    	
    	
    }
    public void drawNsharpSkewtCursorData(IGraphicsTarget target) throws VizException{
    	IFont myFont;
    	myFont = target.initializeFont("Monospace", curseToggledFontLevel, null);
    	myFont.setSmoothing(false);
    	myFont.setScaleFont(false);
    	NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
    	
    	WGraphics WGc = bkRsc.getSkewTBackground().getWorld();
    	Coordinate c = NsharpWxMath.reverseSkewTXY(WGc.unMap(cursorCor.x, cursorCor.y));
    	//System.out.println("Cusrso.x="+cursorCor.x+" Cusrso.y="+cursorCor.y);
    	//System.out.println("Skewt.x="+c.x+" Skewt.y="+c.y);
		double p_mb = c.y;
		double temp = c.x;
		float htFt, htM, relh=-1;
		String curStrFormat, curStrFormat1;
		String curStr3,curStr, curStr1, curStr2;
		VerticalAlignment vAli;
		HorizontalAlignment hAli;
		
		curStr3 = pickedStnInfoStr+"\n";

		htM = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght((float)p_mb));
		htFt= nsharpNative.nsharpLib.mtof(htM);
		if (nsharpNative.nsharpLib.itemp((float)p_mb) > -9998.0 && nsharpNative.nsharpLib.idwpt((float)p_mb) > -9998.0){
			FloatByReference parm= new FloatByReference(0);
			relh= nsharpNative.nsharpLib.relh((float)p_mb, parm);
			curStrFormat= "%4.0f/%.0fkt %4.0fmb  %5.0fft/%.0fm agl  %2.0f%%\n";
			curStr = String.format(curStrFormat, nsharpNative.nsharpLib.iwdir((float)p_mb),
					nsharpNative.nsharpLib.iwspd((float)p_mb),p_mb,htFt,htM,relh);
		}
		else{
			curStrFormat= "%4.0f/%.0fkt %4.0fmb  %5.0fft/%.0fm agl\n";
			curStr = String.format(curStrFormat,nsharpNative.nsharpLib.iwdir((float)p_mb),
					nsharpNative.nsharpLib.iwspd((float)p_mb), p_mb,htFt,htM);
		}
		/*curStrFormat1 = "%4.1f %4.1f/%4.1f%cC  %4.0f/%.0f kt\n";
		curStr1 = String.format(curStrFormat1,temp,  nsharpNative.nsharpLib.itemp((float)p_mb),
				nsharpNative.nsharpLib.idwpt((float)p_mb),NsharpConstants.DEGREE_SYMBOL, nsharpNative.nsharpLib.iwdir((float)p_mb),
				nsharpNative.nsharpLib.iwspd((float)p_mb));*/

		curStrFormat1 = "%s(%s) %4.1f/%4.1f%cF(%4.1f/%4.1f%cC)\n";
		temp =  nsharpNative.nsharpLib.itemp((float)p_mb);
		UnitConverter celciusToFahrenheit = SI.CELSIUS.getConverterTo(NonSI.FAHRENHEIT);
		double tempF= celciusToFahrenheit.convert(temp);
		double dp = nsharpNative.nsharpLib.idwpt((float)p_mb);
		double dpF= celciusToFahrenheit.convert(dp);
		curStr1 = String.format(curStrFormat1,bkRsc.getSTemperatureF(),bkRsc.getSTemperatureC(),  tempF,dpF, NsharpConstants.DEGREE_SYMBOL,temp,
				dp,NsharpConstants.DEGREE_SYMBOL);
		
		//String tempS= String.format("%5.1f%cC ",temp,NsharpConstants.DEGREE_SYMBOL);
		//curStr2 =bkRsc.getSThetaInK()+" "+bkRsc.getSWThetaInK()+" "+bkRsc.getSEThetaInK()+"\n";

		//Adjust string plotting position
		if(cursorCor.x < NsharpConstants.SKEWT_REC_X_ORIG + 200){
			hAli = HorizontalAlignment.LEFT;
		} 
		//else if(cursorCor.x > NsharpConstants.SKEWT_VIEW_X_END - 200){
		//	hAli = HorizontalAlignment.RIGHT;
		//}
		else {
			hAli = HorizontalAlignment.RIGHT;
		}
		vAli = VerticalAlignment.BOTTOM;
		target.drawString(myFont,curStr+curStr1+curStr3, cursorCor.x,
				cursorCor.y, 0.0, TextStyle.NORMAL,
				NsharpConstants.color_yellow, hAli,
				vAli, null);
		myFont.dispose();
    }
    
    /*
     * This function mostly follow display_effective_layer() of xwvid1.c     */
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
    	double y = WGc.mapY(NsharpWxMath.getSkewTXY(botPF.getValue(), 10).y);
    	target.drawLine( WGc.mapX(NsharpConstants.left) +200, y, 0.0,  WGc.mapX(NsharpConstants.left) +280, y, 0.0,
				NsharpConstants.color_cyan_md, 2);
		target.drawString(font10,botStr, WGc.mapX(NsharpConstants.left)+300,
				y, 0.0, TextStyle.NORMAL,
				NsharpConstants.color_cyan_md, HorizontalAlignment.LEFT,
				VerticalAlignment.MIDDLE, null);
    	// Draw effective top level		
    	topStr = String.format( "%.0fm", aglTop);
    	double y1 = WGc.mapY(NsharpWxMath.getSkewTXY(topPF.getValue(), 10).y);
    	target.drawLine( WGc.mapX(NsharpConstants.left) +200, y1, 0.0,  WGc.mapX(NsharpConstants.left) +280, y1, 0.0,
				NsharpConstants.color_cyan_md, 2);
    	if(aglTop > aglBot){
    		target.drawString(font10,topStr, WGc.mapX(NsharpConstants.left)+300,
    				y1, 0.0, TextStyle.NORMAL,
    				NsharpConstants.color_cyan_md, HorizontalAlignment.LEFT,
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
        	double y = WGc.mapY(NsharpWxMath.getSkewTXY(pressure, 10).y);
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
				double y = WGc.mapY(NsharpWxMath.getSkewTXY(pressure, 10).y);
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
				double y = WGc.mapY(NsharpWxMath.getSkewTXY(pressure, 10).y);
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
        	double y = WGc.mapY(NsharpWxMath.getSkewTXY(pressure, 10).y);
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
        	double y = WGc.mapY(NsharpWxMath.getSkewTXY(pressure, -20).y);
        	//double x = WGc.mapX(NsharpWxMath.getSkewTXY(pressure, -20).x);
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
        	double y = WGc.mapY(NsharpWxMath.getSkewTXY(pressure, 10).y);
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
        			double y = world.mapY(NsharpWxMath.getSkewTXY(pressure, -50).y);

        			gc.drawString( Integer.toString(NsharpConstants.HEIGHT_LEVEL_FEET[j]/1000), (int)vxMax + 40,
        					(int)y,false);
        			

        			gc.drawLine( (int)vxMax + 50,(int) y,  (int) vxMax + 45,(int) y);
         } 
      //print meter  scales...
        for (int j = 0; j < NsharpConstants.HEIGHT_LEVEL_METERS.length; j++) {
        		int meters = NsharpConstants.HEIGHT_LEVEL_METERS[j];
        			
        			double pressure = nsharpNative.nsharpLib.ipres(meters);
        			double y = world.mapY(NsharpWxMath.getSkewTXY(pressure, -50).y);

        			gc.drawString( Integer.toString(meters/1000), (int)vxMax + 52,
        					(int)y,false);
        			

        			gc.drawLine( (int)vxMax + 50,(int) y,  (int) vxMax + 55,(int) y);
         } 
        // print surface level mark
        double y = world.mapY(NsharpWxMath.getSkewTXY(soundingLys.get(0).getPressure(), -50).y);
        gc.drawString("SFC("+Integer.toString((int)(soundingLys.get(0).getGeoHeight()))+"m)", (int)vxMax+ 50,
        (int)y, false);
		gc.drawLine((int) vxMax+ 50, (int)y, (int) vxMax + 55,(int) y);
		// top level mark at 100 mbar
        y = world.mapY(NsharpWxMath.getSkewTXY(100, -50).y);
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
        	Coordinate coor = NsharpWxMath.getSkewTXY(NsharpConstants.PRESSURE_MAIN_LEVELS[i],0);

        	gc.drawLine((int)vxMin, (int)world.mapY(coor.y),
        			(int)vxMax,
        			(int)world.mapY(coor.y));

        }
        for (int i = 0; i < NsharpConstants.PRESSURE_MARK_LEVELS.length; i++) {
        	//we only care about pressure for this case, temp is no important  when calling getSkewTXY
        	Coordinate coor = NsharpWxMath.getSkewTXY(NsharpConstants.PRESSURE_MARK_LEVELS[i],0);

        	gc.drawLine((int)vxMin, (int)world.mapY(coor.y),
        			(int)vxMin+10,
        			(int)world.mapY(coor.y));

        }
        for (int i = 0; i < NsharpConstants.PRESSURE_NUMBERING_LEVELS.length; i++) {
        	s = NsharpConstants.pressFormat.format(NsharpConstants.PRESSURE_NUMBERING_LEVELS[i]);
        	//we only care about pressure for this case, temp is no important  when calling getSkewTXY
        	Coordinate coor = NsharpWxMath.getSkewTXY(NsharpConstants.PRESSURE_NUMBERING_LEVELS[i],0);

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
            Coordinate coorStart = NsharpWxMath.getSkewTXY(1050, i);
            double startX = world.mapX(coorStart.x);
            double startY = world.mapY(coorStart.y);

            gc.drawString( Integer.toString(i), (int)startX,(int)startY+5,false);
        }
        for (int i = -60; i > -120; i -= 10) {
            Coordinate coorEnd = NsharpWxMath.getSkewTXY(100, i);

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

    private void drawNsharpStationIdBox(IGraphicsTarget target, WGraphics world, Rectangle rect) throws VizException {
        PixelExtent extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        double x= NsharpConstants.STATION_ID_REC_X_ORIG + 5;
        double nextPageY = NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END;
        RGB color = NsharpConstants.color_yellow;
        String s = "nextPage";
        target.drawString(font10, s, x,
        		nextPageY, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);
        target.drawLine(NsharpConstants.STATION_ID_REC_X_ORIG, NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END, 0.0, 
    			NsharpConstants.STATION_ID_VIEW_X_END , NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END, 0.0, 
    			NsharpConstants.color_white,1, LineStyle.SOLID);
        
        
        int numStnToShow = (NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END)/NsharpConstants.CHAR_HEIGHT;
    	int startIndex = (curStnIdPage-1) * numStnToShow;
        int i = 1;
        int compIndex= 1;
    	int colorIndex;
        for (int j = startIndex; j< stnStateList.size(); j++)
        {
        	boolean avail=false;
         	NsharpStationStateProperty elm = stnStateList.get(j);
    		NsharpConstants.State sta ;
    		if(elm.stnState == NsharpConstants.State.ACTIVE && j == currentStnStateListIndex)
    			sta = NsharpConstants.State.CURRENT;
    		else 
    			sta = elm.stnState; // set its state based on stn state
    		
    		double ly = NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END + NsharpConstants.CHAR_HEIGHT * i;
    		if (currentTimeLineStateListIndex>=0){
    			if(stnTimeTable.get(j).get(currentTimeLineStateListIndex).elementState == NsharpConstants.State.AVAIL ){
    				avail =true;
    			}
    			if(avail){
    				color = NsharpConstants.color_red;
    				s = "*";
    			}
    			else {
    				color = NsharpConstants.color_cyan;
    				s = "*";
    			}
    			//System.out.println("selectedTimeList: "+ s);

    			target.drawString(font10, s, x,
    					ly, 0.0,
    					IGraphicsTarget.TextStyle.NORMAL,
    					color,
    					HorizontalAlignment.LEFT,  
    					VerticalAlignment.BOTTOM, null);
    		}
    		
        	String stnId = elm.stnDescription;        	
        	
        	color = elementColorMap.get(sta);
         	target.drawString(font10, stnId, x+10,
        			ly, 0.0,
        			IGraphicsTarget.TextStyle.NORMAL,
        			color,
        			HorizontalAlignment.LEFT,  
        			VerticalAlignment.BOTTOM, null);
         	if(compareStnIsOn && elm.stnState == NsharpConstants.State.ACTIVE && avail){
         		colorIndex = (compIndex-1)%(NsharpConstants.LINE_COMP10-NsharpConstants.LINE_COMP1+1)+ NsharpConstants.LINE_COMP1;
    			s ="Cp "+ compIndex;
    			target.drawString(font10,s, x+230,
						ly, 0.0,
						IGraphicsTarget.TextStyle.NORMAL,
						linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor(),
						HorizontalAlignment.LEFT,  
						VerticalAlignment.BOTTOM, null);
    			compIndex++;
    			
         	}//else if(compareTmIsOn){
    			//anything to do? 
    		//}
         	
         	
         	i++;
        	if (ly >= NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.CHAR_HEIGHT)
        		//we dont show stn id that extends below  box
        		break;
        }
    }
    
    /**
     * Draws the stn id title out of skewT bkgd.
     * 
     * @throws VizException
     */
    private void drawNsharpStationIdTitle(IGraphicsTarget target) throws VizException {
    	//darw title first
        String s;
        s = stnStateList.size() + " stations";
        double x = NsharpConstants.STATION_ID_REC_X_ORIG;
        double y = NsharpConstants.STATION_ID_REC_Y_ORIG - 60;
        
        target.drawString(font10, s, x,
        		y, 0.0,
                IGraphicsTarget.TextStyle.NORMAL,
                NsharpConstants.color_white,
                HorizontalAlignment.LEFT,
                VerticalAlignment.MIDDLE, null);
        y = y+NsharpConstants.CHAR_HEIGHT;
        s = "page " + curStnIdPage+"/"+totalStnIdPage;
        target.drawString(font10, s, x,
        		y, 0.0,
                IGraphicsTarget.TextStyle.NORMAL,
                NsharpConstants.color_green,
                HorizontalAlignment.LEFT,
                VerticalAlignment.MIDDLE, null);
        
    }
   //To be used later...
	@SuppressWarnings("unused")
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
    private void drawNsharpTimelinBox(IGraphicsTarget target, WGraphics world, Rectangle rect) throws VizException {
        PixelExtent extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        //System.out.println("drawNsharpDataTimelines picked stn info: "+ pickedStnInfoStr);
        double x = NsharpConstants.DATA_TIMELINE_REC_X_ORIG + 5;//, x1 ;
        double nextPageY = NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END;//DATA_TIMELINE_REC_Y_ORIG + NsharpConstants.CHAR_HEIGHT;
        RGB color = NsharpConstants.color_yellow;
        String s = "nextPage";
        target.drawString(font10, s, x,
        		nextPageY, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);
        target.drawLine(NsharpConstants.DATA_TIMELINE_REC_X_ORIG, NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END, 0.0, 
    			NsharpConstants.DATA_TIMELINE_VIEW_X_END , NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END, 0.0, 
    			NsharpConstants.color_white,1, LineStyle.SOLID);
        
        
        int numTimeLineToShowPerPage = (NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END)/NsharpConstants.CHAR_HEIGHT;
    	int startIndex = (curTimeLinePage-1) * numTimeLineToShowPerPage;
        int i = 1;
        //List<NsharpSoundingElementStateProperty>  curStnTimeList = getStnTimeLineListByTimeLine(curSndProfileProp);
        if(timeLineStateList!= null){
        	int compIndex= 1;
        	int colorIndex;
        	//List<ElementStateProperty> stnTmList = stnTimeLineListMap.get(stationIdList.get(curStnIndex).elementDescription);
        	for (int j = startIndex; j< timeLineStateList.size(); j++)
        	{
        		boolean avail=false;
        		NsharpTimeLineStateProperty elm = timeLineStateList.get(j);
        		NsharpConstants.State sta  = elm.timeState;
        		double ly = NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END + NsharpConstants.CHAR_HEIGHT * i;
        		if(elm.timeState == NsharpConstants.State.ACTIVE && j == currentTimeLineStateListIndex )
    				sta = NsharpConstants.State.CURRENT;
        		if(currentStnStateListIndex>=0){
        			

        			if ( stnTimeTable.get(currentStnStateListIndex).get(j).elementState == NsharpConstants.State.AVAIL ){
        				avail = true;
        			}
        			if(avail){
        				color = NsharpConstants.color_red;
        				s = "*";
        			}
        			else {
        				color = NsharpConstants.color_cyan;
        				s = "*";
        			}
        			//System.out.println("selectedTimeList: "+ s);

        			target.drawString(font10, s, x,
        					ly, 0.0,
        					IGraphicsTarget.TextStyle.NORMAL,
        					color,
        					HorizontalAlignment.LEFT,  
        					VerticalAlignment.BOTTOM, null);
        		}
        		
        		color = elementColorMap.get(sta);
        		s = elm.timeDescription;
        		target.drawString(font10, s, x+10,
        				ly, 0.0,
        				IGraphicsTarget.TextStyle.NORMAL,
        				color,
        				HorizontalAlignment.LEFT,  
        				VerticalAlignment.BOTTOM, null);
        		
        		i++;
        		if(compareTmIsOn && elm.timeState == NsharpConstants.State.ACTIVE && avail){
        			colorIndex = (compIndex-1)%(NsharpConstants.LINE_COMP10-NsharpConstants.LINE_COMP1+1)+ NsharpConstants.LINE_COMP1;
        			s ="Cp "+ compIndex;
        			target.drawString(font10,s, x+270,
    						ly, 0.0,
    						IGraphicsTarget.TextStyle.NORMAL,
    						linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor(),
    						HorizontalAlignment.LEFT,  
    						VerticalAlignment.BOTTOM, null);
        			compIndex++;
        		}
        		/*else if(overlayIsOn){
        			if(j == currentTimeLineStateListIndex)
        			{
        				target.drawString(font10,"Ol 1" , x+270,
        						ly, 0.0,
        						IGraphicsTarget.TextStyle.NORMAL,
        						linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_OVERLAY1]).getLineColor(),
        						HorizontalAlignment.LEFT,  
        						VerticalAlignment.BOTTOM, null);
        			}
        			if(j == previousTimeLineStateListIndex && previousTimeLineStateListIndex != currentTimeLineStateListIndex)
        			{
        				target.drawString(font10, "Ol 2", x+270,
        						ly, 0.0,
        						IGraphicsTarget.TextStyle.NORMAL,
        						linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_OVERLAY2]).getLineColor(),
        						HorizontalAlignment.LEFT,  
        						VerticalAlignment.BOTTOM, null);
        			}
        		}         	*/	
        		if (ly >= NsharpConstants.DATA_TIMELINE_NOTATION_Y_START-NsharpConstants.CHAR_HEIGHT)
        			//we dont show time line that extends below time line box
        			break;
        	}
        }

        
    }
    /* 
     *  
     * Chin NOte:
     * drawNsharpDataTimelines() draws dataTimelineList. But since this list is not shown in GUI now,
     * so, it is not used for now.But keep it for possibly used again in the future.
     * 
     *
    private void drawNsharpDataTimelines(IGraphicsTarget target, WGraphics world, Rectangle rect) throws VizException {
        PixelExtent extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        //System.out.println("drawNsharpDataTimelines picked stn info: "+ pickedStnInfoStr);
        double x = NsharpConstants.DATA_TIMELINE_REC_X_ORIG + 5;//, x1 ;
        double nextPageY = NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END;//DATA_TIMELINE_REC_Y_ORIG + NsharpConstants.CHAR_HEIGHT;
        RGB color = NsharpConstants.color_yellow;
        String s = "nextPage";
        String s1="", s2="";
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
        int i = 1, colorIndex=NsharpConstants.LINE_COMP1;//1;
        for (int j = startIndex; j< dataTimelineList.size(); j++)
        {
        	ElementStateProperty elm = dataTimelineList.get(j);
        	s = elm.elementDescription;
        	
        	//System.out.println("selectedTimeList: "+ s);
        	double ly = NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END + NsharpConstants.CHAR_HEIGHT * i;

        	if(!compareIsOn){
        		if(overlayIsOn == true && this.previousSoundingLys!=null){
        			if(elm.elementState == NsharpConstants.State.PICKED ){
        				color = linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_OVERLAY1]).getLineColor();
        				
        			}
        			else if(elm.elementState == NsharpConstants.State.OVERLAY)
        				color = linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_OVERLAY2]).getLineColor();
        			
        			else
        				color = elementColorMap.get(elm.elementColor);
        			
        		}
        		else {
        			color = elementColorMap.get(elm.elementColor);
         		}
        	}
        	else {
        		if(elm.elementState == NsharpConstants.State.PICKED || elm.elementState == NsharpConstants.State.GROUPED) { 
        			color = linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor();
        			
    			}  
    			else 
    			{
    				color = elementColorMap.get(elm.elementColor);
    			}
        		
        		colorIndex++; //always increase index, no matter ploting this elm or not
				if(colorIndex > NsharpConstants.LINE_COMP10)//COLOR_ARRAY.length-1)
					colorIndex =NsharpConstants.LINE_COMP1;//1;
        	}
        	if(elm.elementState == NsharpConstants.State.PICKED){
        		pickedStnColor = color;
        	}
        	
        	StringTokenizer stoken = new StringTokenizer(s);
        	if(sortByStn){
        		s1 = stoken.nextToken(); //stn name, display upto 4 char
        		s2 = s.substring(s1.length());
				int nameLen= Math.min(4, s1.length());
				s1 = s1.substring(0,nameLen);
				
        	}else{
        		s2 = stoken.nextToken(); //stn name
        		s1 = s.substring(s2.length()+1)+ " ";
				int nameLen= Math.min(4, s2.length());
				s2 = s2.substring(0,nameLen);
        	}
        	//System.out.println("drawNsharpDataTimelines color " + color);
        	s= s1+ s2;
        	target.drawString(font10, s, x,
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
        s = "list by:";     
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
        	color1 = NsharpConstants.color_white;
        }
        else{
        	color = NsharpConstants.color_white;
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
    }*/
    /**
     * Draws the temp number at bottom out of skewT bkgd.
     * 
     * @throws VizException
     */
    private void drawNsharpTimelineTitle(IGraphicsTarget target) throws VizException {
        String s = timeLineStateList.size() + " time lines";
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
    private void drawNsharpColorNotation(IGraphicsTarget target, WGraphics world, Rectangle rect) throws VizException {
        PixelExtent extent = new PixelExtent(rect);
        RGB color;
        target.setupClippingPlane(extent);
      //plot notations:
        
        double x = NsharpConstants.COLOR_NOTATION_REC_X_ORIG+5;
        double y = NsharpConstants.COLOR_NOTATION_REC_Y_ORIG+25;
        color = NsharpConstants.color_white;
        target.drawString(font10, "TimeLine/Station State Color Notations:", x,
        		y, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);

        y=y+25;
        color = NsharpConstants.color_green;
        target.drawString(font10, "Current:green", x,
        		y, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);

        x = x+180;
        color = NsharpConstants.color_yellow;
        target.drawString(font10, "Active:yellow", x,
        		y, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);
        

        x = x+180;
        color = NsharpConstants.color_white;
        target.drawString(font10, "InActive:white", x,
        		y, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);

        x = NsharpConstants.COLOR_NOTATION_REC_X_ORIG+5;
        y=y+25;
        
        color = NsharpConstants.color_white;
        target.drawString(font10, "Current TimeLine/Station Data Loading Status:", x,
        		y, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);
        
        x = NsharpConstants.COLOR_NOTATION_REC_X_ORIG+5;
        y=y+25;
        color = NsharpConstants.color_red;
        target.drawString(font10, "* :Data Loaded", x,
        		y, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,   
    			VerticalAlignment.BOTTOM, null);
        
        x = x+180;
        color = NsharpConstants.color_cyan;
        target.drawString(font10, "* :Data Not Loaded", x,
        		y, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			color,
    			HorizontalAlignment.LEFT,  
    			VerticalAlignment.BOTTOM, null);

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
            WGraphics world,  RGB icolor, List<NcSoundingLayer> sndLys, double xPosition, double botPress)throws VizException {
        ArrayList<List<LineStroke>> windList = new ArrayList<List<LineStroke>>();

        double windX = xPosition;
        float lastHeight = -999;
        double windY=0;
        double barbScaleFactorx, barbScaleFactory;
        barbScaleFactorx = zoomLevel;
        barbScaleFactory = zoomLevel;
        //System.out.println("zoom="+zoomLevel +"world viewYmin="+world.getViewYmin()+" viewYmax="+world.getViewYmax()+" wolrdYmin="+ world.getWorldYmin()+" wolrdYmax="+ world.getWorldYmax()
        //		+"world viewXmin="+world.getViewXmin()+" viewXmax="+world.getViewXmax()+" wolrdXmin="+ world.getWorldXmin()+" wolrdXmax="+ world.getWorldXmax());
        for (NcSoundingLayer layer : sndLys) {
            float pressure = layer.getPressure();
            float spd = layer.getWindSpeed();
            float dir = layer.getWindDirection();
            if ( pressure < botPress || spd < 0 ) {
                continue;
            }
            if(spd > 140)
            	spd = 140;
            if ((layer.getGeoHeight() - lastHeight) < graphConfigProperty.getWindBarbDistance()*zoomLevel){
            	
            	continue;
            }

            // Get the vertical ordinate.
            if(currentGraphMode== NsharpConstants.GRAPH_SKEWT)
            	windY = NsharpWxMath.getSkewTXY(pressure, 0).y;
            else if(currentGraphMode== NsharpConstants.GRAPH_ICING ){
            	NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
            	//Chin:Y axis (pressure) is scaled using log scale and increaing downward
            	//WorldYmin= at pressure 1000,its value actually is 1000 (max), wolrdYmax = at pressure 300, its value is 825 (min)
            	windY = world.getWorldYmax() + (world.getWorldYmin()-bkRsc.getIcingBackground().toLogScale(pressure));
            	barbScaleFactorx = 2.5*zoomLevel;
            	barbScaleFactory= 3.5*zoomLevel;//experimental value: depends on the world coordinate size set
            }else if( currentGraphMode== NsharpConstants.GRAPH_TURB){
            	NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
            	//Chin:Y axis (pressure) is scaled using log scale and increaing downward
            	//WorldYmin= at pressure 1000,its value actually is 1000 (max), wolrdYmax = at pressure 300, its value is 825 (min)
            	windY = world.getWorldYmax() + (world.getWorldYmin()-bkRsc.getTurbBackground().toLogScale(pressure));
            	barbScaleFactorx = .23*zoomLevel;//experimental value: depends on the world coordinate size set
            	barbScaleFactory=5.5*zoomLevel;
            }
            else
            	continue;
            		
            List<LineStroke> barb = WindBarbFactory.getWindGraphics((double) (spd), (double) dir);
            if (barb != null) {
               // WindBarbFactory.scaleBarb(barb, zoomLevel*barbScaleFactor);
                for (LineStroke stroke : barb) {
                    stroke.scale(barbScaleFactorx, barbScaleFactory);
                }
                //System.out.println("pressure="+pressure+" windX="+windX+" windY="+windY);
                WindBarbFactory.translateBarb(barb, windX, windY);
                windList.add(barb);
            }
             lastHeight = layer.getGeoHeight();
        }

        for (List<LineStroke> barb : windList) {
        	//System.out.println("barb");
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
            windY =  world.mapY(NsharpWxMath.getSkewTXY(pressure, 0).y);
            
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
        gc.drawLine((int)windX,(int)world.mapY(NsharpWxMath.getSkewTXY(100, 0).y),
        		(int)windX,(int)world.mapY(NsharpWxMath.getSkewTXY(1000, 0).y));
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

        		c1 = NsharpWxMath.getSkewTXY(layer.getPressure(), t1);
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
    			Coordinate c1 = NsharpWxMath.getSkewTXY(sfcpres, vtemp);
    			c1.x = world.mapX(c1.x);
    			c1.y = world.mapY(c1.y);
    			FloatByReference p2 = new FloatByReference(0), t2 = new FloatByReference(0);;
    			nsharpNative.nsharpLib.drylift (sfcpres, sfctemp, sfcdwpt, p2, t2);
    			vtemp = nsharpNative.nsharpLib.virtemp (p2.getValue(), t2.getValue(), t2.getValue());
    			Coordinate c2 = NsharpWxMath.getSkewTXY(p2.getValue(), vtemp);
    			c2.x = world.mapX(c2.x);
    			c2.y = world.mapY(c2.y);

    			gc.drawLine((int)c1.x, (int)c1.y, (int)c2.x, (int)c2.y);
    			c1 = c2;


    			float t3;
    			for (float i = p2.getValue() - 50; i >= 100; i = i - 50)
    			{
    				t3 = nsharpNative.nsharpLib.wetlift (p2.getValue(), t2.getValue(), i);
    				vtemp = nsharpNative.nsharpLib.virtemp (i, t3, t3);
    				c2 = NsharpWxMath.getSkewTXY(i, vtemp);
    				c2.x = world.mapX(c2.x);
    				c2.y = world.mapY(c2.y);

    				gc.drawLine((int)c1.x, (int)c1.y, (int)c2.x, (int)c2.y);
    				c1 = c2;
    			}

    			t3 = nsharpNative.nsharpLib.wetlift (p2.getValue(), t2.getValue(), 100);
    			vtemp = nsharpNative.nsharpLib.virtemp (100, t3, t3);
    			c2 = NsharpWxMath.getSkewTXY(100, vtemp);
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
    	double maxPressure = NsharpWxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmax())).y;
        double minPressure = NsharpWxMath.reverseSkewTXY(new Coordinate(0, world
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

                Coordinate c1 = NsharpWxMath.getSkewTXY(pressure, t);
                
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

        double maxPressure = NsharpWxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmax())).y;
        double minPressure = NsharpWxMath.reverseSkewTXY(new Coordinate(0, world
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

                Coordinate c1 = NsharpWxMath.getSkewTXY(pressure, t);
                
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
    public void plotNsharpHodoVectors(IGraphicsTarget target, double zoomLevel,
            WGraphics world, GC gc,  boolean printEvent) throws VizException {
    	double radiusUnit = 10;
    	//NsharpParametersSelectionConfigDialog configD = NsharpParametersSelectionConfigDialog.getAccess();
    	Coordinate c;
    	FloatByReference value1= new FloatByReference(-999);
		FloatByReference value2= new FloatByReference(-999);
		FloatByReference wdir= new FloatByReference(-999);
		FloatByReference wspd= new FloatByReference(-999);
		FloatByReference Surfpressure = new FloatByReference(-999);
		String textStr;
		
    	//plot  Mean Wind Vector, yellow square, by default plot it
		if(((graphConfigProperty != null ) && (graphConfigProperty.isMeanWind()))||(graphConfigProperty == null)){
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
		if((graphConfigProperty != null ) && (graphConfigProperty.isSmv1585() || graphConfigProperty.isSmv3075())){
			nsharpNative.nsharpLib.get_surface(Surfpressure, value1, value2);
			if(nsharpNative.nsharpLib.qc(Surfpressure.getValue()) == 1) {
				nsharpNative.nsharpLib.mean_wind(Surfpressure.getValue(), nsharpNative.nsharpLib.ipres (nsharpNative.nsharpLib.msl (6000.0F)),value1, value2, wdir, wspd);
				if( nsharpNative.nsharpLib.qc(wdir.getValue())==1 && nsharpNative.nsharpLib.qc(wspd.getValue())==1) {
					// ----- Plot 30/75 Storm Motion Vector -----small red circle 
					if(graphConfigProperty.isSmv3075()){
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
					if(graphConfigProperty.isSmv1585()){
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
		if((graphConfigProperty != null ) && graphConfigProperty.isCorfidiV()){
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
		if(((graphConfigProperty != null ) && graphConfigProperty.isSmvBunkersR())||(graphConfigProperty == null)){
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
		if(((graphConfigProperty != null ) && graphConfigProperty.isSmvBunkersL())||(graphConfigProperty == null)){
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
	}
	public void setInteractiveHodoPointCoordinate(Coordinate c){
		try {
			NcSoundingLayer hodoLayer = soundingLys.get(hodoEditingSoundingLayerIndex);
			if(hodoLayer != null){
				//TTR575
				this.dataTimelineSndLysListMap.put(pickedStnInfoStr, this.soundingLys);
				nsharpNative.populateSndgData(soundingLys);
				//end TTR575
				NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
				Coordinate c1 = bkRsc.getHodoBackground().getWorld().unMap(c.x, c.y);
				//System.out.println("picked pt after unmap CX "+ c1.x + " CY "+ c1.y);
				c1 = WxMath.speedDir((float) c1.x, (float) c1.y);
				hodoLayer.setWindSpeed((float)c1.x);
				hodoLayer.setWindDirection((float)c1.y);
				createRscHodoWindShapeAll();
				//createTurbulenceShapes(bkRsc.getTurbBackground().getWorld());
			}
		}
		catch(Exception e)  {
			
		}
	}
	public void applyInteractiveTempPoint(){
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		
		Coordinate inC = NsharpWxMath.reverseSkewTXY(bkRsc.getSkewTBackground().getWorld().unMap(interactiveTempPointCoordinate));
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
		createRscPressTempCurveShapeAll();	
		WGraphics WGc = bkRsc.getSkewTBackground().getWorld();
		createRscwetBulbTraceShape(WGc);
		createRscVTempTraceShape(WGc);
		if(parcelTraceRscShapeList.size()>0){
			for(IWireframeShape shape: parcelTraceRscShapeList){
				shape.dispose();
			}
			parcelTraceRscShapeList.clear();
			
		}
		for (ParcelData parData: parcelList){
			createRscParcelTraceShape( WGc, parData.parcelType,parData.parcelLayerPressure);
		}
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
		if(heightMarkRscShape!=null){
			heightMarkRscShape.dispose();
			heightMarkRscShape=null;
		}
		if(omegaRscShape!=null){
			omegaRscShape.dispose();
			omegaRscShape=null;
		}
		if(wetBulbTraceRscShape!=null){
			wetBulbTraceRscShape.dispose();
			wetBulbTraceRscShape=null;
		}
		if(vtempTraceCurveRscShape!=null){
			vtempTraceCurveRscShape.dispose();
			vtempTraceCurveRscShape=null;
		}
		if(thetaEPressureYRscShape!=null){
			thetaEPressureYRscShape.dispose();
			thetaEPressureYRscShape=null;
		}
		if(thetaEPressureRRscShape!=null){
			thetaEPressureRRscShape.dispose();
			thetaEPressureRRscShape=null;
		}
		if(thetaEPressureWRscShape!=null){
			thetaEPressureWRscShape.dispose();
			thetaEPressureWRscShape=null;
		}
		if(thetaEHeightYRscShape!=null){
			thetaEHeightYRscShape.dispose();
			thetaEHeightYRscShape=null;
		}
		if(thetaEHeightWRscShape!=null){
			thetaEHeightWRscShape.dispose();
			thetaEHeightWRscShape=null;
		}
		if(thetaEHeightRRscShape!=null){
			thetaEHeightRRscShape.dispose();
			thetaEHeightRRscShape=null;
		}
		if(srWindBRscShape!=null){
			srWindBRscShape.dispose();
			srWindBRscShape=null;
		}
		if(srWindWRscShape!=null){
			srWindWRscShape.dispose();
			srWindWRscShape=null;
		}
		if(srWindRRscShape!=null){
			srWindRRscShape.dispose();
			srWindRRscShape=null;
		}
		if(srWindGRscShape!=null){
			srWindGRscShape.dispose();
			srWindGRscShape=null;
		}
		if(srWindMRscShape!=null){
			srWindMRscShape.dispose();
			srWindMRscShape=null;
		}
		if(psblWatchTypeBkgShape!=null){
			psblWatchTypeBkgShape.dispose();
			psblWatchTypeBkgShape=null;
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
		if(cloudFMShape!=null){
			cloudFMShape.dispose();
			cloudFMShape=null;
		}
		if(cloudCEShape!=null){
			cloudCEShape.dispose();
			cloudCEShape=null;
		}
		if(cloudFMLabelShape!=null){
			cloudFMLabelShape.dispose();
			cloudFMLabelShape=null;
		}
		if(icingTempShape!=null){
			icingTempShape.dispose();
			icingTempShape= null;
		}
		if(icingRHShape!=null){
			icingRHShape.dispose();
			icingRHShape=null;
		}
		if(icingEPIShape!=null){
			icingEPIShape.dispose();
			icingEPIShape=null;
		}
		if(turbWindShearShape!=null){
			turbWindShearShape.dispose();
			turbWindShearShape=null;
		}
		if(turbLnShape!=null){
			turbLnShape.dispose();
			turbLnShape=null;
		}
		if(parcelTraceRscShapeList.size()>0){
			for(IWireframeShape shape: parcelTraceRscShapeList){
				shape.dispose();
			}
			parcelTraceRscShapeList.clear();
			
		}
		if(hodoWindRscShapeList.size()>0){
			for(ShapeAndLineProperty shapeColor: hodoWindRscShapeList){
				shapeColor.shape.dispose();
			}
			hodoWindRscShapeList.clear();
			
		}
		if(pressureTempRscShapeList.size()>0){
			for(ShapeAndLineProperty shapeColor: pressureTempRscShapeList){
				shapeColor.shape.dispose();
			}
			pressureTempRscShapeList.clear();
			
		}
		if(windBoxWindRscShapeList.size()>0){
			for(ShapeAndLineProperty shapeColor: windBoxWindRscShapeList){
				shapeColor.shape.dispose();
			}
			windBoxWindRscShapeList.clear();
			
		}
	}
	private void createRscVTempTraceShape(WGraphics world){
		if((soundingLys == null) || (soundingLys.size()==0))
			return;
    	float t1;
    	if(vtempTraceCurveRscShape!=null){
			vtempTraceCurveRscShape.dispose();
			vtempTraceCurveRscShape=null;
		}
     
        Coordinate c2 =  null;
        Coordinate c1;
        // draw trace
        vtempTraceCurveRscShape = target.createWireframeShape(false,descriptor );
        vtempTraceCurveRscShape.allocate(this.soundingLys.size() * 2);
        for (NcSoundingLayer layer : this.soundingLys) {
        	if ((layer.getTemperature() != INVALID_DATA) && (layer.getDewpoint() != INVALID_DATA) && layer.getPressure()>= 100){
        		t1 = nsharpNative.nsharpLib.ivtmp(layer.getPressure());

        		c1 = NsharpWxMath.getSkewTXY(layer.getPressure(), t1);
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
	 * This plotting function is based on the algorithm of draw_Clouds() at xwvid1.c of AWC Nsharp source code
	 * Using Fred Mosher's Algorithm & Chernykh and Eskridge Algorithm 
	 * 
	 */
	private void createCloudsShape(WGraphics world) {
    	NsharpNative.NsharpLibrary.CloudInfoStr cloudInfo = new NsharpNative.NsharpLibrary.CloudInfoStr();
    	nsharpNative.nsharpLib.draw_Clouds(cloudInfo);
    	// draw FM model: Fred Mosher's Algorithm
    	if(cloudInfo.getSizeFM() > 0){
    		cloudFMShape = target.createShadedShape(false, descriptor, false);
    		cloudFMLabelShape = target.createWireframeShape(false,descriptor );
    		cloudFMLabelShape.allocate(2);
    		double [][] lines = {{0, 0},{0,0}}; 
    		cloudFMLabelShape.addLineSegment(lines);
    		for (int i=0; i < cloudInfo.getSizeFM() ; i++){
    			double lowY = world.mapY(NsharpWxMath.getSkewTXY(cloudInfo.getPreStartFM()[i], -50).y);
    			double highY = world.mapY(NsharpWxMath.getSkewTXY(cloudInfo.getPreEndFM()[i], -50).y);
    			Coordinate[] coords = new Coordinate[4];
    			coords[0] = new Coordinate(NsharpConstants.SKEWT_REC_X_ORIG+150, lowY);
    			coords[1] = new Coordinate(NsharpConstants.SKEWT_REC_X_ORIG+200, lowY);
    			coords[2] = new Coordinate(NsharpConstants.SKEWT_REC_X_ORIG+200, highY);
    			coords[3] = new Coordinate(NsharpConstants.SKEWT_REC_X_ORIG+150, highY);
    			 			
    			/*
    			 * Create LineString[] from Coordinates[]
    			 */
    			GeometryFactory gf = new GeometryFactory();
    			LineString[] ls = new LineString[] { gf.createLineString(coords) };
    			
    			cloudFMShape.addPolygonPixelSpace(ls, NsharpConstants.color_yellow);      	
    			double [] lblXy = { NsharpConstants.SKEWT_REC_X_ORIG+175, (lowY+highY)/2};
    			cloudFMLabelShape.addLabel(NsharpNative.NsharpLibrary.CLOUD_TYPE[cloudInfo.cloudTypeFM[i]], lblXy);
    		}
    		cloudFMShape.compile();
    		cloudFMLabelShape.compile();
    	}
    	// draw CE model :  Chernykh and Eskridge Algorithm 
    	if(cloudInfo.getSizeCE() > 0){
    		cloudCEShape = target.createShadedShape(false, descriptor, false);
    		
    		for (int i=0; i < cloudInfo.getSizeCE() ; i++){
    			double lowY = world.mapY(NsharpWxMath.getSkewTXY(cloudInfo.getPreStartCE()[i], -50).y);
    			double highY = world.mapY(NsharpWxMath.getSkewTXY(cloudInfo.getPreEndCE()[i], -50).y);
    			Coordinate[] coords = new Coordinate[4];
    			coords[0] = new Coordinate(NsharpConstants.SKEWT_REC_X_ORIG+100, lowY);
    			coords[1] = new Coordinate(NsharpConstants.SKEWT_REC_X_ORIG+150, lowY);
    			coords[2] = new Coordinate(NsharpConstants.SKEWT_REC_X_ORIG+150, highY);
    			coords[3] = new Coordinate(NsharpConstants.SKEWT_REC_X_ORIG+100, highY);
    			 			
    			/*
    			 * Create LineString[] from Coordinates[]
    			 */
    			GeometryFactory gf = new GeometryFactory();
    			LineString[] ls = new LineString[] { gf.createLineString(coords) };
    			
    			cloudCEShape.addPolygonPixelSpace(ls, NsharpConstants.color_red);      	
    			
    		}
    		cloudCEShape.compile();
    		
    	}
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
				y1 = NsharpWxMath.getSkewTXY(pressure, 0).y;
				y1=WGc.mapY(y1);
	            y2 = NsharpWxMath.getSkewTXY(pressure-100, 0).y;	
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
        ShapeAndLineProperty shNcolor = new ShapeAndLineProperty();
        IWireframeShape shapeR = shNcolor.shape = target.createWireframeShape(false,descriptor );
        shapeR.allocate(soundingLys.size()*2);
        shNcolor.lp.setLineColor(NsharpConstants.color_red);
        windBoxWindRscShapeList.add(shNcolor);
        shNcolor = new ShapeAndLineProperty();
        IWireframeShape shapeG= shNcolor.shape = target.createWireframeShape(false,descriptor );
        shapeG.allocate(soundingLys.size()*2);
        shNcolor.lp.setLineColor(NsharpConstants.color_green);
        windBoxWindRscShapeList.add(shNcolor);
        shNcolor = new ShapeAndLineProperty();
        IWireframeShape shapeY= shNcolor.shape = target.createWireframeShape(false,descriptor );
        shapeY.allocate(soundingLys.size()*2);
        shNcolor.lp.setLineColor(NsharpConstants.color_yellow);
        windBoxWindRscShapeList.add(shNcolor);
        shNcolor = new ShapeAndLineProperty();
        IWireframeShape shapeC= shNcolor.shape = target.createWireframeShape(false,descriptor );
        shapeC.allocate(soundingLys.size()*2);
        shNcolor.lp.setLineColor(NsharpConstants.color_cyan);
        windBoxWindRscShapeList.add(shNcolor);
        shNcolor = new ShapeAndLineProperty();
        IWireframeShape shapeV = shNcolor.shape = target.createWireframeShape(false,descriptor );
        shapeV.allocate(soundingLys.size()*2);
        shNcolor.lp.setLineColor(NsharpConstants.color_violet);
        windBoxWindRscShapeList.add(shNcolor);
        for (NcSoundingLayer layer : soundingLys) {
            float pressure = layer.getPressure();
            float spd = layer.getWindSpeed();
            if ( pressure < 100 || spd < 0 ) {
                continue;
            }
            if(spd > 140)
            	spd = 140;
             // Get the vertical ordinate.
            windY = NsharpWxMath.getSkewTXY(pressure, 0).y;
            
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
        ShapeAndLineProperty shNcolor;
        IWireframeShape shapeR=null, shapeG=null, shapeY=null, shapeC=null, shapeV=null, shapeIn=null;
		if(incolor == null){
			//creating regular Hodo shape with 5 colors
			shNcolor = new ShapeAndLineProperty();
			shapeR = shNcolor.shape = target.createWireframeShape(false,descriptor );
			shapeR.allocate(soundingLays.size()*2);
	        shNcolor.lp.setLineColor(NsharpConstants.color_red);
	        hodoWindRscShapeList.add(shNcolor);
	        shNcolor = new ShapeAndLineProperty();
	        shapeG= shNcolor.shape = target.createWireframeShape(false,descriptor );
	        shapeG.allocate(soundingLays.size()*2);
	        shNcolor.lp.setLineColor(NsharpConstants.color_green);
	        hodoWindRscShapeList.add(shNcolor);
	        shNcolor = new ShapeAndLineProperty();
	        shapeY= shNcolor.shape = target.createWireframeShape(false,descriptor );
	        shapeY.allocate(soundingLays.size()*2);
	        shNcolor.lp.setLineColor(NsharpConstants.color_yellow);
	        hodoWindRscShapeList.add(shNcolor);
	        shNcolor = new ShapeAndLineProperty();
	        shapeC= shNcolor.shape = target.createWireframeShape(false,descriptor );
	        shapeC.allocate(soundingLays.size()*2);
	        shNcolor.lp.setLineColor(NsharpConstants.color_cyan);
	        hodoWindRscShapeList.add(shNcolor);
	        shNcolor = new ShapeAndLineProperty();
	        shapeV = shNcolor.shape = target.createWireframeShape(false,descriptor );
	        shapeV.allocate(soundingLays.size()*2);
	        shNcolor.lp.setLineColor(NsharpConstants.color_violet);
	        hodoWindRscShapeList.add(shNcolor);
		}
		else{
			shNcolor = new ShapeAndLineProperty();
			shapeIn = shNcolor.shape = target.createWireframeShape(false,descriptor );
	        shapeIn.allocate(soundingLays.size()*2);
			shNcolor.lp.setLineColor(incolor);
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
			for(ShapeAndLineProperty shapeColor: pressureTempRscShapeList){
				shapeColor.shape.dispose();
			}
			pressureTempRscShapeList.clear();
		}
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource(); 
		WGraphics WGc=  bkRsc.getSkewTBackground().getWorld();
		
		if(compareStnIsOn && currentTimeLineStateListIndex >=0){
			int colorIndex =NsharpConstants.LINE_COMP1;
			for(NsharpStationStateProperty elm: stnStateList) {
				if(elm.stnState == NsharpConstants.State.ACTIVE && 
						stnTimeTable.get(stnStateList.indexOf(elm)).get(currentTimeLineStateListIndex).elementState == NsharpConstants.State.AVAIL){
					List<NcSoundingLayer> soundingLayeys = dataTimelineSndLysListMap.get(stnTimeTable.get(stnStateList.indexOf(elm)).get(currentTimeLineStateListIndex).elementDescription);
					NsharpLineProperty lp = linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]);
					colorIndex++;
					if(colorIndex > NsharpConstants.LINE_COMP10)
						colorIndex =NsharpConstants.LINE_COMP1;
					createRscPressTempCurveShape(WGc, soundingLayeys, lp);
				}
			}
		}
		else if(compareTmIsOn && currentStnStateListIndex >=0 ){
			int colorIndex =NsharpConstants.LINE_COMP1;
			for(NsharpTimeLineStateProperty elm: timeLineStateList) {
				if(elm.timeState == NsharpConstants.State.ACTIVE && 
						stnTimeTable.get(currentStnStateListIndex).get(timeLineStateList.indexOf(elm)).elementState == NsharpConstants.State.AVAIL){
					List<NcSoundingLayer> soundingLayeys = dataTimelineSndLysListMap.get(stnTimeTable.get(currentStnStateListIndex).get(timeLineStateList.indexOf(elm)).elementDescription);
					NsharpLineProperty lp = linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]);
					colorIndex++;
					if(colorIndex > NsharpConstants.LINE_COMP10)
						colorIndex =NsharpConstants.LINE_COMP1;
					createRscPressTempCurveShape(WGc, soundingLayeys, lp);
				}
			}
		}
		else if(overlayIsOn == true ){
			createRscPressTempCurveShape(WGc, this.soundingLys, linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_OVERLAY1]));
			if(this.previousSoundingLys!=null && !previousSoundingLys.equals(soundingLys)){
				createRscPressTempCurveShape(WGc, this.previousSoundingLys, linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_OVERLAY2]));
			}

		}
		else {
			createRscPressTempCurveShape(WGc, this.soundingLys, null);
		}
	}
	
	public void createRscHodoWindShapeAll(){
		if(hodoWindRscShapeList.size()>0){
			for(ShapeAndLineProperty shapeColor: hodoWindRscShapeList){
				shapeColor.shape.dispose();
			}
			hodoWindRscShapeList.clear();
		}
		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource(); 
		WGraphics WGc=  bkRsc.getHodoBackground().getWorld();
		if(compareStnIsOn && currentTimeLineStateListIndex >=0){
			int colorIndex =NsharpConstants.LINE_COMP1;
			for(NsharpStationStateProperty elm: stnStateList) {
				if(elm.stnState == NsharpConstants.State.ACTIVE && 
						stnTimeTable.get(stnStateList.indexOf(elm)).get(currentTimeLineStateListIndex).elementState == NsharpConstants.State.AVAIL){
					List<NcSoundingLayer> soundingLayeys = dataTimelineSndLysListMap.get(stnTimeTable.get(stnStateList.indexOf(elm)).get(currentTimeLineStateListIndex).elementDescription);
					RGB color = linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor();
					colorIndex++; 
					if(colorIndex > NsharpConstants.LINE_COMP10)
						colorIndex =NsharpConstants.LINE_COMP1;
					createRscHodoWindShape(WGc, soundingLayeys, color);
				}
			}
		}
		else if(compareTmIsOn && currentStnStateListIndex >=0 ){
			int colorIndex =NsharpConstants.LINE_COMP1;
			for(NsharpTimeLineStateProperty elm: timeLineStateList) {
				if(elm.timeState == NsharpConstants.State.ACTIVE && 
						stnTimeTable.get(currentStnStateListIndex).get(timeLineStateList.indexOf(elm)).elementState == NsharpConstants.State.AVAIL){
					List<NcSoundingLayer> soundingLayeys = dataTimelineSndLysListMap.get(stnTimeTable.get(currentStnStateListIndex).get(timeLineStateList.indexOf(elm)).elementDescription);
					RGB color = linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor();
					colorIndex++;
					if(colorIndex > NsharpConstants.LINE_COMP10)
						colorIndex =NsharpConstants.LINE_COMP1;
					createRscHodoWindShape(WGc, soundingLayeys, color);
				}
			}
		}
		else if(overlayIsOn == true && this.previousSoundingLys!=null){
			createRscHodoWindShape(WGc, this.soundingLys, linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_OVERLAY1]).getLineColor());
			if(!previousSoundingLys.equals(soundingLys))
				createRscHodoWindShape(WGc, this.previousSoundingLys, linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_OVERLAY2]).getLineColor());
		}
		else {
			createRscHodoWindShape(WGc, this.soundingLys, null);
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
		Coordinate c1 = NsharpWxMath.getSkewTXY(sfcpres, vtemp);
		c1.x = world.mapX(c1.x);
		c1.y = world.mapY(c1.y);
		FloatByReference p2 = new FloatByReference(0), t2 = new FloatByReference(0);;
		nsharpNative.nsharpLib.drylift (sfcpres, sfctemp, sfcdwpt, p2, t2);
		vtemp = nsharpNative.nsharpLib.virtemp (p2.getValue(), t2.getValue(), t2.getValue());
		Coordinate c2 = NsharpWxMath.getSkewTXY(p2.getValue(), vtemp);
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
			c2 = NsharpWxMath.getSkewTXY(i, vtemp);
			c2.x = world.mapX(c2.x);
			c2.y = world.mapY(c2.y);

			double [][] lines1 = {{c1.x, c1.y},{c2.x, c2.y}};
			shape.addLineSegment(lines1);

			c1 = c2;
		}

		t3 = nsharpNative.nsharpLib.wetlift (p2.getValue(), t2.getValue(), 100);
		vtemp = nsharpNative.nsharpLib.virtemp (100, t3, t3);
		c2 = NsharpWxMath.getSkewTXY(100, vtemp);
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
         y0 = world.mapY(NsharpWxMath.getSkewTXY(1050, 0).y);
        
 
        // draw wind speed vs height box
        double xtemp;
        for (int i = 0; i < 140 ; i= i+20){
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
       // double [][] lines = {{xAxisOrigin, yTop},{xAxisOrigin, yBot}};
        //omegaBkgShape.addLineSegment(lines);
        //center line
        double [][] lines1 = {{xAxisOrigin+40, yTop},{xAxisOrigin+40, yBot}};
        omegaBkgShape.addLineSegment(lines1);
        //right dash line, -10 omega line
        //double [][] lines2 = {{xAxisOrigin+80, yTop},{xAxisOrigin+80, yBot}};
       // omegaBkgShape.addLineSegment(lines2);
        double [] lblXy = {xAxisOrigin+40, yTop-40};
        omegaBkgShape.addLabel("OMEGA", lblXy);
		double [] lblXy1 = {xAxisOrigin, yTop-5};
		omegaBkgShape.addLabel("+10", lblXy1);
		double [] lblXy2 = {xAxisOrigin+80, yTop-5};
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
		double [] lblXy = { dispX+150, dispY};
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
          		double [] lblXy1 = { dispX+20, dispY-20};
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
    		double [] lblXy1 = { dispX+40, dispY};
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
    	
		double [] lblXy = { dispX+ 150, dispY};
		thetaEPressureYRscShape.addLabel("Theta-E vs Pressure", lblXy);
		
		//plot theta E difference
		double [] lblXy2= {dispX+150,dispY+100};
		FloatByReference tempF= new FloatByReference(0);
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
           		double [] lblXy1= { dispX+20, dispY-20};
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
            double [] lblXy1= { dispX+40, dispY};
    		thetaEPressureYRscShape.addLabel(String.valueOf(pres), lblXy1);
    	}
		thetaEPressureYRscShape.compile();
        thetaEPressureWRscShape.compile();
        thetaEPressureRRscShape.compile();
    }
	/******************************************************************************************************************
	 * createTurbulenceShapes()
	 * Chin:: NOTE:::
	 * This plotting function is based on the algorithm of draw_TURB() at xwvid1.c of AWC Nsharp source code
	 * by LARRY J. HINSON AWC/KCMO    
	 * Original C code
	g=9.8; 
    for (i=0;i<numlvl-2 && sndg[i][1]>=100;i++) {
      s1=i;
      s2=i+1;
      if (sndg[s1][3] > -900 & sndg[s2][3]> -900.0) {
        u1=-sndg[s1][6]*sin(sndg[s1][5]*PI/180);
        v1=-sndg[s1][6]*cos(sndg[s1][5]*PI/180);
        u2=-sndg[s2][6]*sin(sndg[s2][5]*PI/180);
        v2=-sndg[s2][6]*cos(sndg[s2][5]*PI/180);
        u=u2-u1;v=v2-v1;
        windshear=sqrt(u*u+v*v)*.51479/(sndg[s2][2]-sndg[s1][2]);
        midPres=(sndg[s1][1]+sndg[s2][1])/2;
        theta1=theta(sndg[s1][1],sndg[s1][3],1000)+273.16;
        theta2=theta(sndg[s2][1],sndg[s2][3],1000)+273.16;
        meanTheta=(theta1+theta2)/2.0;
        dz=sndg[s2][2]-sndg[s1][2];
        dthetadz=(theta2-theta1)/dz;
        if (windshear != 0.0) {
      	  windshearsqrd=(windshear*windshear);
      	  Ri=(g/meanTheta)*(dthetadz/windshearsqrd);
          if (! first) {
            setcolor(7);
            setwindow(X0,fnP(Y0),XM,fnP(YM));
            line_w(lastptx,lastpty,log(Ri),fnP(midPres));
            setcolor(11);
            setwindow(nx1w,fnP(Y0),nx2w,fnP(YM));
            tke_windshear_prod=0.54*dz*windshearsqrd;
           
            line_w(lastpts,lastpty,tke_windshear_prod*100,fnP(midPres));
          }
          lastptx=log(Ri);
          lastpty=fnP(midPres);
          lastpts=0.54*dz*windshearsqrd*100;
          first=0;
        }
      }
    }
	 **********************************************************************************************************************/    
	private void createTurbulenceShapes(WGraphics world){		

		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		if(bkRsc==null){
			return;
		}
		if(turbLnShape!=null)
			turbLnShape.dispose();
		NsharpTurbulenceBackground tBk= bkRsc.getTurbBackground();
		turbLnShape = target.createWireframeShape(false,descriptor );
		turbLnShape.allocate(this.soundingLys.size() * 2);
		turbWindShearShape  = target.createWireframeShape(false,descriptor );
		turbWindShearShape.allocate(this.soundingLys.size() * 2);
		Coordinate pointALn = null;
		Coordinate pointBLn=null;
		Coordinate pointAWsh = null;
		Coordinate pointBWsh=null;
		double g= 9.8f, Ri;
		double t0,t1,v0,v1, u0,u1, windshear0, windshearsqrd,tke_windshear_prod;
		double pressure0=0, pressure1,midpressure0, p, high0=0,high1;
		double theta1=0, theta0=0,dthetadz0,meanTheta;
		boolean first=true;
		NcSoundingLayer layer0, layer1;
		for (int i=0; i< soundingLys.size()-1; i++) {
			layer0 = soundingLys.get(i);
			pressure0= layer0.getPressure();
			t0= layer0.getTemperature();
			high0= layer0.getGeoHeight();
			layer1 = soundingLys.get(i+1);
			t1= layer1.getTemperature();
			high1= layer1.getGeoHeight();
			if( t0<= NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA || t1<= NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA||
				pressure0 <= NsharpConstants.TURBULENCE_PRESSURE_LEVEL_TOP )
				continue;
			pressure1= layer1.getPressure();
			v0=nsharpNative.nsharpLib.iwndv((float)pressure0);
			v1=nsharpNative.nsharpLib.iwndv((float)pressure1);
			u0=nsharpNative.nsharpLib.iwndu((float)pressure0);
			u1=nsharpNative.nsharpLib.iwndu((float)pressure1);
			windshear0=Math.sqrt((u1-u0)*(u1-u0)+(v1-v0)*(v1-v0))*.51479/(high1-high0);
			midpressure0 = (pressure1+pressure0)/2;
			theta0=WxMath.theta(pressure0,t0, 1000)+273.15;
			theta1=WxMath.theta(pressure1,t1, 1000)+273.15;
			meanTheta=(theta1+theta0)/2.0f;
			dthetadz0=(theta1-theta0)/(high1-high0);
			if (windshear0 != 0.0 ) {
				windshearsqrd=(windshear0*windshear0);
				Ri=(g/meanTheta)*(dthetadz0/windshearsqrd);
				world.setWorldCoordinates(NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_LEFT, tBk.toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_TOP),        		
						NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_RIGHT, tBk.toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_BOTTOM));
				//System.out.println("world viewYmin="+world.getViewYmin()+" viewYmax="+world.getViewYmax()+" wolrdYmin="+ world.getWorldYmin()+" wolrdYmax="+ world.getWorldYmax()
		        //		+" viewXmin="+world.getViewXmin()+" viewXmax="+world.getViewXmax()+" wolrdXmin="+ world.getWorldXmin()+" wolrdXmax="+ world.getWorldXmax());
				
				pointALn = new Coordinate();
				p = tBk.toLogScale(midpressure0);
				pointALn.x = world.mapX(Math.log(Ri));
				pointALn.y = world.mapY(p);
				world.setWorldCoordinates(NsharpConstants.TURBULENCE_WIND_SHEAR_TKE_LEFT, tBk.toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_TOP),        		
						NsharpConstants.TURBULENCE_WIND_SHEAR_TKE_RIGHT, tBk.toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_BOTTOM));
				pointAWsh = new Coordinate();
				tke_windshear_prod=0.54*(high1-high0)*windshearsqrd;
				pointAWsh.x = world.mapX( tke_windshear_prod*100);
				pointAWsh.y = world.mapY(p);
				//System.out.println("P0="+pressure0+" dthetadz0="+dthetadz0+" theta0="+theta0+" log(Ri)="+Math.log(Ri)+ " pointAx="+pointALn.x+ " y="+pointALn.y);
				if (! first) {
					double [][] linesLn = {{pointALn.x, pointALn.y},{pointBLn.x, pointBLn.y}};
					double [][] linesWsh = {{pointAWsh.x, pointAWsh.y},{pointBWsh.x, pointBWsh.y}};
					turbLnShape.addLineSegment(linesLn);
					turbWindShearShape.addLineSegment(linesWsh);
					
		          }
		          else{
		        	  first=false;
		          }
		          pointBLn = pointALn;
		          pointBWsh = pointAWsh;
			}
		}
        
        turbLnShape.compile();
        turbWindShearShape.compile();
	}
	
	/*
	 * Chin:: NOTE:::
	 * This plotting function is based on the algorithm of draw_ICG() at xwvid1.c of AWC Nsharp source code
	 * by LARRY J. HINSON AWC/KCMO    
	 * 
	 */    
	private void createIcingRHShape(WGraphics world){		
		//System.out.println("world viewYmin="+world.getViewYmin()+" viewYmax="+world.getViewYmax()+" wolrdYmin="+ world.getWorldYmin()+" wolrdYmax="+ world.getWorldYmax()
		//        		+" viewXmin="+world.getViewXmin()+" viewXmax="+world.getViewXmax()+" wolrdXmin="+ world.getWorldXmin()+" wolrdXmax="+ world.getWorldXmax());

		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		if(bkRsc==null){
			return;
		}
		icingRHShape = target.createWireframeShape(false,descriptor );
		icingRHShape.allocate(this.soundingLys.size() * 2);
		Coordinate c0 = null;
		
        for (NcSoundingLayer layer : soundingLys) {
        	double pressure = layer.getPressure();
            if (pressure >= NsharpConstants.ICING_PRESSURE_LEVEL_TOP
                    && pressure <= NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM) {

            	FloatByReference parm= new FloatByReference(0);
				float relh= nsharpNative.nsharpLib.relh((float)pressure, parm);
				Coordinate c1 = new Coordinate();
                double p = bkRsc.getIcingBackground().toLogScale(pressure);
                c1.x = world.mapX(relh);
                c1.y = world.mapY(p);
                //System.out.println("RH="+relh+ " p="+pressure+ " x="+c1.x+ " y="+c1.y);
                if (c0 != null) {
                		double [][] lines = {{c0.x, c0.y},{c1.x, c1.y}};
                		icingRHShape.addLineSegment(lines);
                 }
                c0 = c1;
            }
        }
        
        icingRHShape.compile();
	}

	/******************************************************************************************************************
	 * createIcingEPIShape()
	 * Chin:: NOTE:::
	 * This plotting function is based on the algorithm of draw_ICG() at xwvid1.c of AWC Nsharp source code
	 * by LARRY J. HINSON AWC/KCMO    
	 *  original c ode********
        first=-1;
        for (i=0;i<numlvl-1 && sndg[i][1]>=100;i++) {
          if (sndg[i][3] > -900.0 && sndg[i+1][3]>-900) {
            theta1=theta(sndg[i][1],sndg[i][3],1000)+273.15;
            thetase1=theta1*exp(const1*mixratio(sndg[i][1],sndg[i][3])*.001/(sndg[i][3]+273.15));
            theta2=theta(sndg[i+1][1],sndg[i+1][3],1000)+273.15;
            thetase2=theta2*exp(const1*mixratio(sndg[i+1][1],sndg[i+1][3])*.001/(sndg[i][3]+273.15));
            //Do D-Theta-se/dz
            dthetasedz=(thetase2-thetase1)/(sndg[i+1][2]-sndg[i][2]);
            midpres=(sndg[i][1]+sndg[i+1][1])/2;
            if (first) {
              moveto_w(dthetasedz*1E3,fnP(midpres));
              first=0;
            }
            else
              lineto_w(dthetasedz*1E3,fnP(midpres));
          }
        }
        end of c code *******
	 * 
	 **********************************************************************************************************************/    
    private void createIcingEPIShape(WGraphics world){		
		//System.out.println("world viewYmin="+world.getViewYmin()+" viewYmax="+world.getViewYmax()+" wolrdYmin="+ world.getWorldYmin()+" wolrdYmax="+ world.getWorldYmax()
		//        		+" viewXmin="+world.getViewXmin()+" viewXmax="+world.getViewXmax()+" wolrdXmin="+ world.getWorldXmin()+" wolrdXmax="+ world.getWorldXmax());

		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		if(bkRsc==null){
			return;
		}
		icingEPIShape = target.createWireframeShape(false,descriptor );
		icingEPIShape.allocate(this.soundingLys.size() * 2);
		Coordinate pointA = null;
		Coordinate pointB=null;
		boolean firstround=true;
		double t0,t1;
		double pressure0=0, pressure1,midpressure0, p, high0=0,high1;
		double const1=2500000.0/1004.0;
		double theta1=0,thetase1, theta0=0,thetase0=0,mixratio0,mixratio1,dthetasedz0;
		NcSoundingLayer layer0, layer1;
		for (int i=0; i< soundingLys.size()-1; i++) {
			layer0 = soundingLys.get(i);
			layer1 = soundingLys.get(i+1);
			t0= layer0.getTemperature();     
			t1= layer1.getTemperature();     
			pressure0 = layer0.getPressure();
			pressure1 = layer1.getPressure();
			if( t0<= NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA || t1<= NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA||
				(pressure0 < NsharpConstants.ICING_PRESSURE_LEVEL_TOP && pressure1 < NsharpConstants.ICING_PRESSURE_LEVEL_TOP))
				continue;
			theta1=WxMath.theta(pressure1,t1, 1000)+273.15;
			mixratio1 = WxMath.mixingRatio(pressure1, t1);
			thetase1 = theta1*Math.exp(const1*mixratio1*.001/(t1+273.15));
			high1= layer1.getGeoHeight();
			theta0=WxMath.theta(pressure0,t0, 1000)+273.15;
			mixratio0 = WxMath.mixingRatio(pressure0, t0);
			thetase0 = theta0*Math.exp(const1*mixratio0*.001/(t0+273.15));
			high0= layer0.getGeoHeight();
			//Do D-Theta-se/dz
			dthetasedz0=(thetase1-thetase0)/(high1-high0)*1E3;
			midpressure0 = (pressure1+pressure0)/2;
			pointA = new Coordinate();
			p = bkRsc.getIcingBackground().toLogScale(midpressure0);
			pointA.x = world.mapX(dthetasedz0);
			pointA.y = world.mapY(p);
			if(!firstround){
				//System.out.println("Temp="+t0+ " p="+pressure0+ "pointAx="+pointA.x+ " y="+pointA.y+ " pointBx="+pointB.x+ " y="+pointB.y);
				double [][] lines = {{pointA.x, pointA.y},{pointB.x, pointB.y}};
				icingEPIShape.addLineSegment(lines);

			}
			else
			{//this is first round, we need two pints for a line segment. We only have first point now.
				firstround= false;
			}
			pointB = pointA;

		}        
        icingEPIShape.compile();
	}
	/*
	 * Chin:: NOTE:::
	 * This plotting function is based on the algorithm of draw_ICG() at xwvid1.c of AWC Nsharp source code
	 * by LARRY J. HINSON AWC/KCMO    
	 * 
	 */    
    private void createIcingTempShape(WGraphics world){		
		//System.out.println("world viewYmin="+world.getViewYmin()+" viewYmax="+world.getViewYmax()+" wolrdYmin="+ world.getWorldYmin()+" wolrdYmax="+ world.getWorldYmax()
		//        		+" viewXmin="+world.getViewXmin()+" viewXmax="+world.getViewXmax()+" wolrdXmin="+ world.getWorldXmin()+" wolrdXmax="+ world.getWorldXmax());

		NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
		if(bkRsc==null){
			return;
		}
		icingTempShape = target.createWireframeShape(false,descriptor );
		icingTempShape.allocate(this.soundingLys.size() * 2);
		Coordinate c0 = null;
		
        for (NcSoundingLayer layer : soundingLys) {
        	double t= layer.getTemperature();     
        	//if( t > NsharpConstants.ICING_TEMPERATURE_RIGHT || t< NsharpConstants.ICING_TEMPERATURE_LEFT)
        	//	continue;
            double pressure = layer.getPressure();
            if (pressure >= NsharpConstants.ICING_PRESSURE_LEVEL_TOP
                    && pressure <= NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM) {

                Coordinate c1 = new Coordinate();
                double p = bkRsc.getIcingBackground().toLogScale(pressure);
                c1.x = world.mapX(t);
                c1.y = world.mapY(p);
                //System.out.println("Temp="+t+ " p="+pressure+ " x="+c1.x+ " y="+c1.y);
                if (c0 != null) {
                		double [][] lines = {{c0.x, c0.y},{c1.x, c1.y}};
                		icingTempShape.addLineSegment(lines);
                 }
                c0 = c1;
            }
        }
        
        icingTempShape.compile();
	}
    private void createRscPressTempCurveShape(WGraphics WGc, List<NcSoundingLayer> soundingLays, NsharpLineProperty lineP){
		IWireframeShape shapeT = target.createWireframeShape(false,descriptor );
		shapeT.allocate(soundingLays.size() * 2);
		IWireframeShape shapeD = target.createWireframeShape(false,descriptor );
		shapeD.allocate(soundingLays.size() * 2);
		ShapeAndLineProperty shNcolorT = new ShapeAndLineProperty(), shNcolorD=new ShapeAndLineProperty();
        double maxPressure = NsharpWxMath.reverseSkewTXY(new Coordinate(0, WGc
                .getWorldYmax())).y;
        double minPressure = NsharpWxMath.reverseSkewTXY(new Coordinate(0, WGc
                .getWorldYmin())).y;
        boolean drawTemp=true, drawDew=true;
        //NsharpParametersSelectionConfigDialog configD = NsharpParametersSelectionConfigDialog.getAccess();
        if(graphConfigProperty!=null){
        	drawTemp = graphConfigProperty.isTemp();
        	drawDew = graphConfigProperty.isDewp();
        }
        Coordinate c0 = null, c01=null;
        for (NcSoundingLayer layer : soundingLays) {
        	double t, d;
        	t = layer.getTemperature();
        	d = layer.getDewpoint();
        	
            double pressure = layer.getPressure();
            if (t != INVALID_DATA  && pressure >= minPressure
                    && pressure <= maxPressure) {

                Coordinate c1 = NsharpWxMath.getSkewTXY(pressure, t);
                
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

                Coordinate c11 = NsharpWxMath.getSkewTXY(pressure, d);
                
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
        if(!overlayIsOn && !compareStnIsOn && !compareTmIsOn){
        	//use default color 
           	shNcolorT.lp = linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_TEMP]);//chin new config NsharpConstants.color_red;
        	shNcolorD.lp = linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_DEWP]);//NsharpConstants.color_green;           
        }
        else
        {
        	shNcolorT.lp = lineP;
        	shNcolorD.lp = lineP;
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
		if(wetBulbTraceRscShape!=null){
			wetBulbTraceRscShape.dispose();
			wetBulbTraceRscShape=null;
		}
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

        		c1 = NsharpWxMath.getSkewTXY(layer.getPressure(), t1);
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
        	omega = layer.getOmega()* -10 ;//to have same scale as legacy Nsharp 
        	if (p > 140 && p < 1000 && omega > -10){
        		//since 40 units of skewT X-axis is 10 unit of Omega, we have to convert it by 
        		// multiply 4 to omega unit for plotting
        		Coordinate c1 = NsharpWxMath.getSkewTXY(p, t);
        		c1.y = WGc.mapY(c1.y); // what we need here is only pressure for Y-axix, 
         		//target.drawLine(xAxisOrigin, c1.y, 0.0, xAxisOrigin + omega* 4, c1.y, 0.0, NsharpConstants.color_cyan,
        	  	//		commonLinewidth);
         		double [][] lines = {{xAxisOrigin+40, c1.y},{xAxisOrigin+40 + (omega* 4), c1.y}};
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
        	double y = WGc.mapY(NsharpWxMath.getSkewTXY(pressure, -50).y);
        	//System.out.println("WGc.mapX(NsharpConstants.left) + 20 =" + (WGc.mapX(NsharpConstants.left) + 20));
        	double [][] lines = {{WGc.mapX(NsharpConstants.left) + 20, y},{WGc.mapX(NsharpConstants.left) + 40, y}};
            heightMarkRscShape.addLineSegment(lines);
            double [] lblXy = {WGc.mapX(NsharpConstants.left) + 50,y-5};
            heightMarkRscShape.addLabel(Integer.toString(meters/1000)+" km", lblXy);
        } 
        // plot surface level mark{
        if(soundingLys.get(0).getGeoHeight() != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA)
        {
        	double y = WGc.mapY(NsharpWxMath.getSkewTXY(soundingLys.get(0).getPressure(), -50).y);
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
			createCloudsShape(WGc);

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
	public void setGraphConfigProperty(NsharpGraphProperty graphConfigProperty) {
		this.graphConfigProperty = graphConfigProperty;
		int tempOffset = graphConfigProperty.getTempOffset();
		NsharpWxMath.setTempOffset(tempOffset);
	}

	public void setLinePropertyMap(
			HashMap<String, NsharpLineProperty> linePropertyMap) {
		this.linePropertyMap = linePropertyMap;
	}
	
	public void handleTimeLineActConfig(List<String>  tlList, NsharpConstants.State actSt){
		for(String tlStr: tlList){
			for(NsharpTimeLineStateProperty tl: timeLineStateList){	
				if(tlStr.equals(tl.timeDescription)){
					tl.timeState = actSt;
					break;
				}
			}
		}
		findCurrentElementIndexesAfterConfig();
		setCurSndProfileProp();
		setCurrentSoundingLayerInfo();
		resetData();
		if(compareStnIsOn){
			createRscHodoWindShapeAll();
			createRscPressTempCurveShapeAll();
		}
	}
	public void handleStationActConfig(List<String>  stnList, NsharpConstants.State actSt){
		for(String tlStr: stnList){
			for(NsharpStationStateProperty stn: stnStateList){	
				if(tlStr.equals(stn.stnDescription)){
					stn.stnState = actSt;
					break;
				}
			}
		}
		findCurrentElementIndexesAfterConfig();
		setCurSndProfileProp();
		setCurrentSoundingLayerInfo();
		resetData();
	}
}

