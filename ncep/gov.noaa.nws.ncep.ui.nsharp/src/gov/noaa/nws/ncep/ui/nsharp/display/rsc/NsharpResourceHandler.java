/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler
 * 
 * This java class performs the NSHARP NsharpSkewTResource functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/30/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.display.rsc;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.ElementStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigStore;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpDataPageProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGraphProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpLineProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpSoundingElementStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpTimeLineStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpWxMath;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpMapResource;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpDataHandling;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative.NsharpLibrary._lplvalues;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNativeConstants;
import gov.noaa.nws.ncep.ui.nsharp.view.NsharpPaletteWindow;
import gov.noaa.nws.ncep.ui.nsharp.view.NsharpParcelDialog;
import gov.noaa.nws.ncep.ui.nsharp.view.NsharpShowTextDialog;

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

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.sounding.WxMath;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.sounding.SoundingParams;
import com.raytheon.viz.core.graphing.LineStroke;
import com.raytheon.viz.core.graphing.WGraphics;
import com.raytheon.viz.core.graphing.WindBarbFactory;
import com.sun.jna.ptr.FloatByReference;
import com.vividsolutions.jts.geom.Coordinate;
@SuppressWarnings("deprecation")
public class NsharpResourceHandler {
	private IRenderableDisplay[] displayArray=null;
	private NsharpPartListener.PartEvent editorPartStatus =NsharpPartListener.PartEvent.partClosed;
	private NsharpSkewTPaneResource skewtPaneRsc;
	private NsharpWitoPaneResource witoPaneRsc;
	private NsharpHodoPaneResource hodoPaneRsc;
	private NsharpTimeStnPaneResource timeStnPaneRsc;
	private NsharpInsetPaneResource insetPaneRsc;
	private NsharpDataPaneResource dataPaneRsc;
	private NsharpSpcGraphsPaneResource spcGraphsPaneRsc;
	private NsharpAbstractPaneResource futurePaneRsc;
	//private Coordinate hodoStmCenter; //hodo storm motion center
	NsharpNative nsharpNative=null;
	private static final int  DATAPAGEMAX = NsharpConstants.PAGE_MAX_NUMBER/ 2;
	private static final int  INSETPAGEMAX =2;
	private int currentTextChapter= 1;
	private int currentInsetPage= 1;
	private int cnYOrig = NsharpConstants.COLOR_NOTATION_Y_ORIG;
	private int dtNextPageEnd = NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END_;
	private int charHeight = NsharpConstants.CHAR_HEIGHT_;
	private int dtXOrig = NsharpConstants.DATA_TIMELINE_X_ORIG;
	private int dtYOrig = NsharpConstants.DATA_TIMELINE_Y_ORIG;
	private int dtWidth = NsharpConstants.DATA_TIMELINE_WIDTH;
	private String paneConfigurationName;
	/* Hodograph Modes - definition is based on definitions in globals_xw.h of BigNsharp  */
	private static final int HODO_NORMAL	=		0;
	//private static int HODO_EFFECTIVE=		1; not used in BigNsharp source code
	private static final int HODO_STORMRELATIVE=	2;
	@SuppressWarnings("unused")
	private static final int HODO_BNDRY=			3;
	private static final int HODO_MEANWIND=		4;
	@SuppressWarnings("unused")
	private int currentHodoWindMode = HODO_MEANWIND;
	private NsharpConfigManager configMgr;
	private NsharpConfigStore configStore;
	private NsharpGraphProperty graphConfigProperty;
	private HashMap<String, NsharpLineProperty> linePropertyMap; 
	private NsharpDataPageProperty dataPageProperty;
	private int[] pageDisplayOrderNumberArray = new int[NsharpConstants.PAGE_MAX_NUMBER+1]; //index is the real page defined in NsharpConstants to be shown, value is the order number of this page. index 0 point to a dummy.
	private boolean overlayIsOn = false;
	private boolean interpolateIsOn = false;
	private boolean compareStnIsOn = false;
	private boolean compareTmIsOn = false;
	private boolean editGraphOn=false;
	private boolean getTimeMatcher=false;
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
	    
    protected ListenerList listenerList = new ListenerList();
    
    private List<List<NcSoundingLayer>> soundingLysList = null;
    
    //current active sounding layer list
	private List<NcSoundingLayer> soundingLys = null;
	private List<NcSoundingLayer> previousSoundingLys = null;
	private String pickedStnInfoStr; // current picked stn info with time line, e.g. "ATLH 2010-12-12 12:00:00"
	private NsharpStationInfo pickedStnInfo = null;
	private IFrameCoordinator.FrameChangeOperation currentOpDirection = IFrameCoordinator.FrameChangeOperation.NEXT; // next =forward
	
	
	private HashMap<Integer, RGB> stormSlinkyColorMap = new HashMap<Integer, RGB>();
    //list of sounding layer list for each data time line
    private HashMap<String, List<NcSoundingLayer>> dataTimelineSndLysListMap = new HashMap<String, List<NcSoundingLayer>>();
    private HashMap<String, List<NcSoundingLayer>> originalDataTimelineSndLysListMap= new HashMap<String, List<NcSoundingLayer>>();
	
	//dataTimelineList: time line selected by user, but is updated based on available time line at DB at setRsc()
	// this is derived from dataTimelineSndLysListMap. It has stn info + sounding time line info
	// used field is used to identify if this time line is picked by user. user could pick multiple time lines for comparison
	private List<ElementStateProperty> dataTimelineList= new ArrayList<ElementStateProperty>();
	public List<ElementStateProperty> getDataTimelineList() {
		System.out.println("getDataTimelineList called");
		for(List<NsharpSoundingElementStateProperty> stnList: stnTimeTable){
			for(NsharpSoundingElementStateProperty sndProp: stnList){
				ElementStateProperty elm = new ElementStateProperty();
				elm.setElementDescription(sndProp.getElementDescription());
				elm.setStnInfo(sndProp.getStnInfo());
				elm.setElementState(sndProp.getElementState());
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
	private int currentStnStateListIndex=-1;
	private int currentTimeLineStateListIndex=-1;
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
	
	//private List<ParcelData> parcelList = new ArrayList<ParcelData>(); 
	private short currentParcel = NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;
	private float currentParcelLayerPressure = NsharpNativeConstants.MU_LAYER; 
	//private Coordinate hodoHouseC = new Coordinate(NsharpConstants.HODO_CENTER_X, NsharpConstants.HODO_CENTER_Y);
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
	public HashMap<String, NsharpLineProperty> getLinePropertyMap() {
		return linePropertyMap;
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

	
	public NsharpNative getNsharpNative() {
		return nsharpNative;
	}
	
	

	public void setNextTextChapter(){
		if(currentTextChapter == DATAPAGEMAX){
			currentTextChapter = 1;
		}
		else
			currentTextChapter++;
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
		
		if(hodoPaneRsc!=null)
			hodoPaneRsc.createRscHodoWindShapeAll();
		if(skewtPaneRsc!=null)
			skewtPaneRsc.handleResize();
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
				if(stnStateList.get(i).getStnState() == NsharpConstants.State.ACTIVE 
						&& stnTimeTable.get(i).get(currentTimeLineStateListIndex).getElementState() == NsharpConstants.State.AVAIL){
					found = true;
					currentStnStateListIndex = i;
				}
				if(found)
					break;
			}
		}
		int numTimeLinePerPage = (cnYOrig-dtNextPageEnd)/charHeight;
		curStnIdPage = totalStnIdPage/numTimeLinePerPage + 1;
		setCurSndProfileProp();
		setCurrentSoundingLayerInfo();
		resetData();
		if(soundingLys!=null){
			if(hodoPaneRsc!=null)
				hodoPaneRsc.createRscHodoWindShapeAll();
			if(skewtPaneRsc!=null)
				skewtPaneRsc.handleResize();
		}
	}
	public void setCompareTmIsOn(boolean compareIsOn) {
		this.compareTmIsOn = compareIsOn;
		//make sure soundingLys is not null
		if(soundingLys==null && compareIsOn){
			//find a new available time line for current stn
			boolean found = false;
			for(int i =0; i< timeLineStateList.size(); i++){
				if(timeLineStateList.get(i).getTimeState() == NsharpConstants.State.ACTIVE 
					&& stnTimeTable.get(currentStnStateListIndex).get(i).getElementState() == NsharpConstants.State.AVAIL){
					found = true;
					previousTimeLineStateListIndex = currentTimeLineStateListIndex;
		 			currentTimeLineStateListIndex = i;
				}
				if(found)
					break;
			}
		}
		int numTimeLinePerPage = (cnYOrig-dtNextPageEnd)/charHeight;
		curTimeLinePage = currentTimeLineStateListIndex/numTimeLinePerPage + 1;
		setCurSndProfileProp();
		setCurrentSoundingLayerInfo();
		resetData();	 			
		if(soundingLys!=null){
			if(hodoPaneRsc!=null)
				hodoPaneRsc.createRscHodoWindShapeAll();
			if(skewtPaneRsc!=null)
				skewtPaneRsc.handleResize();
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
		if(skewtPaneRsc!=null)
			skewtPaneRsc.setCurrentGraphMode(currentGraphMode);
		
		/*NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
		//getWitoPaneRsc().recreateShapes();
		if (editor != null) {
			editor.refresh();
		}*/
		refreshPane();
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

	/*public static NsharpSkewTResource createSkewtResource() {
        LoadProperties loadProperties1 = new LoadProperties();
        ColorableCapability colorable1 = new ColorableCapability();
        colorable1.setColor(NsharpConstants.backgroundColor);
        loadProperties1.getCapabilities().addCapability(colorable1);
        return new NsharpSkewTResource(new NsharpSkewTResourceData(),
                loadProperties1);
    }*/
	
    public List<List<NcSoundingLayer>> getSoundingLysList() {
		return soundingLysList;
	}
    
    public void setCurrentParcel(short currentParcel) {
		this.currentParcel = currentParcel;
		currentParcelLayerPressure=NsharpNativeConstants.parcelToLayerMap.get(currentParcel);
		//inform draw panel as well
		if(dataPaneRsc!=null){
			dataPaneRsc.setCurrentParcel(currentParcel);
		}
		if(skewtPaneRsc!=null){
			if(currentParcel == NsharpNativeConstants.PARCELTYPE_USER_DEFINED)
				currentParcelLayerPressure = NsharpParcelDialog.getUserDefdParcelMb();
			skewtPaneRsc.createRscParcelTraceShapes(currentParcel,currentParcelLayerPressure);
			skewtPaneRsc.createLCLEtcLinesShape();
		}
	}
    public void setCurrentParcelData(short currentParcel, float pressure) {
		this.currentParcel = currentParcel;
		currentParcelLayerPressure=pressure;
		//inform draw panel as well
		if(dataPaneRsc!=null){
			dataPaneRsc.setCurrentParcel(currentParcel);
		}
	}
	/*
     * NOTE:::ONly one parcel will be in parcel list as current design changed to only show one configured parcel
     * This is how BigNsharp does.
     * Todo: replace List<ParcelData>  with just one ParcelData
     */
	/*public void setParcelList(List<ParcelData> parcelList) {
		this.parcelList = parcelList;
		if(skewtPaneRsc!=null)
			skewtPaneRsc.createParcelShapes(parcelList);
	}*/
	public void updateParcelFromPanel(short currentParcel){
		this.currentParcel = currentParcel;
		currentParcelLayerPressure=NsharpNativeConstants.parcelToLayerMap.get(currentParcel);
		if(skewtPaneRsc!=null){
			skewtPaneRsc.createRscParcelTraceShapes(currentParcel,currentParcelLayerPressure);
			skewtPaneRsc.createLCLEtcLinesShape();
		}
		//update parcel shape
		/*List<ParcelData> parcelList = new ArrayList<ParcelData>(); 
		ParcelData pd= new ParcelData();
		pd.setParcelType(currentParcel);
		pd.setParcelLayerPressure(currentParcelLayerPressure);
		parcelList.add(pd);
		setParcelList(parcelList);*/
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
	

	public void setHodoStmCenter(Coordinate hodoHouseC) {
		if(hodoPaneRsc==null)
			return;
		//hodoPaneRsc.setHodoHouseC(hodoHouseC);
		Coordinate c = hodoPaneRsc.getHodoBackground().getWorld().unMap(hodoHouseC.x, hodoHouseC.y);
		c = WxMath.speedDir((float) c.x, (float) c.y);
		smWindDir = (float) c.y;
		smWindSpd = (float)c.x;
		nsharpNative.nsharpLib.set_storm(smWindSpd, smWindDir);
		if(insetPaneRsc!=null){
			WGraphics WGc = insetPaneRsc.getPsblWatchTypeBackground().getWorld();
			insetPaneRsc.createBkgPsblWatchShape(WGc);
		
			//Sr wind vs Height graph shape need to recreate
			WGc=  insetPaneRsc.getSrWindsBackground().getWorld();	
			insetPaneRsc.createRscSrWindShape(WGc); 
		}
		if(skewtPaneRsc!=null){
			skewtPaneRsc.createEffectiveLayerLinesShape();
			skewtPaneRsc.updatePsblWatchColor();
		}
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
		
	
		nsharpNative.populateSndgData(soundingLys);
		if(skewtPaneRsc!=null)
			skewtPaneRsc.resetData(soundingLys,previousSoundingLys);
		if(hodoPaneRsc!=null)
			hodoPaneRsc.resetData(soundingLys,previousSoundingLys);
		if(witoPaneRsc!=null)
			witoPaneRsc.resetData(soundingLys, previousSoundingLys);
		if(dataPaneRsc!=null)
			dataPaneRsc.resetData(soundingLys, previousSoundingLys);
		if(insetPaneRsc!=null)
			insetPaneRsc.resetData(soundingLys, previousSoundingLys);
		
		//re-create shape
		if(skewtPaneRsc!=null)
			skewtPaneRsc.handleResize();
		if(hodoPaneRsc!=null)
			hodoPaneRsc.createRscHodoWindShapeAll();
		if(insetPaneRsc!=null)
			insetPaneRsc.createInsetWireFrameShapes();
		if(witoPaneRsc!=null)
			witoPaneRsc.createRscWireFrameShapes();
		
		
	}
	
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
	public void handleNsharpEditorPartEvent(NsharpPartListener.PartEvent pStatus){
		switch(pStatus){
		case partActivated:
			if(editorPartStatus != NsharpPartListener.PartEvent.partDeactivated){
				//repopulateSndgData();
				//resetRsc();
				resetData();
			}
			
			break;
		default:
			break;
		}
		editorPartStatus = pStatus;
	}
	public void resetRsc() {
		//System.out.println("resetRsc called");
		this.dataTimelineSndLysListMap.clear();
		//this.dataTimelineSndLysListMap = new HashMap<String, List<NcSoundingLayer>>();
		deepCopyDataMap(this.originalDataTimelineSndLysListMap,this.dataTimelineSndLysListMap);
		this.soundingLys = this.dataTimelineSndLysListMap.get(pickedStnInfoStr);
		//Set default parcel trace data
		currentParcel = NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;
		currentParcelLayerPressure = NsharpNativeConstants.MU_LAYER; 
		setSoundingInfo(dataTimelineSndLysListMap.get(pickedStnInfoStr));
		currentTextChapter = 1;
		overlayIsOn = false;
		interpolateIsOn = false;
		compareStnIsOn = false;
		editGraphOn = false;
		if(skewtPaneRsc!=null)
			skewtPaneRsc.setCurrentSkewTEditMode(NsharpConstants.SKEWT_EDIT_MODE_EDITPOINT);
		resetData();

	}
	public synchronized void resetData(){
		//System.out.println("resetData called, rscHdr="+this.toString() + " pickedStnInfoStr="+pickedStnInfoStr+ " nsharpNative="+nsharpNative.toString());
		
		//update active sounding layer and picked stn info						
		//re-populate snd data to nsharp native code lib for later calculating
		nsharpNative.populateSndgData(soundingLys);
		if(skewtPaneRsc!=null)
			skewtPaneRsc.resetData(soundingLys,previousSoundingLys);
		if(hodoPaneRsc!=null)
			hodoPaneRsc.resetData(soundingLys,previousSoundingLys);
		if(insetPaneRsc!=null)
			insetPaneRsc.resetData(soundingLys, previousSoundingLys);
		if(dataPaneRsc!=null)
			dataPaneRsc.resetData(soundingLys, previousSoundingLys);
		if(witoPaneRsc!=null)
			witoPaneRsc.resetData(soundingLys, previousSoundingLys);
		
		NsharpShowTextDialog textarea =  NsharpShowTextDialog.getAccess();
		if(textarea != null){
			textarea.refreshTextData();
		}
		//if soundingLys is null, then we stop here, after reset data.
		if(soundingLys == null)
			return;
		if(soundingLys.size() >0){
			//set initial hodohouseC
			
			// ----- set hodo circle at Bunkers Right, Chin according to TTR6065 or RaytheonTicket#10438
			FloatByReference dummy1= new FloatByReference(-999);
			FloatByReference dummy2= new FloatByReference(-999);
			FloatByReference bwdir= new FloatByReference(-999);
			FloatByReference bwspd= new FloatByReference(-999);
			nsharpNative.nsharpLib.bunkers_storm_motion(dummy1, dummy2, bwdir, bwspd);
			//System.out.println("resetData windspd="+  bwspd.getValue()+ " dir="+bwdir.getValue());
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
			/* TBD 
			//reset draw panel as well
			if(drawPanel!=null){
				drawPanel.resetCurrentParcel();
			}*/
		}
		//Chin: TBD remove handle resize here to fix sizing issue when swapped nsharp from side pane back to main pane 
		// but, may cause other problem?
		//if(skewtPaneRsc!=null)
			//skewtPaneRsc.handleResize();
		if(skewtPaneRsc!=null)
			skewtPaneRsc.createRscWireFrameShapes();
		if(hodoPaneRsc!=null)
			hodoPaneRsc.createRscHodoWindShapeAll();
		if(insetPaneRsc!=null)
			insetPaneRsc.createInsetWireFrameShapes();	
		if(witoPaneRsc!=null)
			witoPaneRsc.createAllWireFrameShapes();
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
			StringTokenizer st1 = new StringTokenizer(o1.getElementDescription());
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
			StringTokenizer st2 = new StringTokenizer(o2.getElementDescription());
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
			StringTokenizer st1 = new StringTokenizer(o1.get(0).getStnDescription());
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
			StringTokenizer st2 = new StringTokenizer(o2.get(0).getStnDescription());
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
			StringTokenizer st1 = new StringTokenizer(o1.getStnDescription());
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
			StringTokenizer st2 = new StringTokenizer(o2.getStnDescription());
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
			StringTokenizer st1 = new StringTokenizer(o1.getElementDescription());
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
			StringTokenizer st2 = new StringTokenizer(o2.getElementDescription());
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
			StringTokenizer st1 = new StringTokenizer(o1.getTimeDescription());
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
			StringTokenizer st2 = new StringTokenizer(o2.getTimeDescription());
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
			StringTokenizer st1 = new StringTokenizer(o1.getTimeDescription());
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
			StringTokenizer st2 = new StringTokenizer(o2.getTimeDescription());
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
			if(sndProp.getTimeDescription().equals(tmLine) )
				return timeLineStateList.indexOf(sndProp); 
		}

		return -1;
	}
	private int getStnIndexFromStnStateList(String stnId){
		for(NsharpStationStateProperty sndProp: stnStateList){
			if(sndProp.getStnDescription().equals(stnId) )
				return stnStateList.indexOf(sndProp); 

		}
		return -1;
	}
	private void setCurrentTimeLineStateIndexForCurrentState(){
		boolean found = false;
		for(NsharpTimeLineStateProperty tl:timeLineStateList){
			if(tl.getTimeState()  == NsharpConstants.State.ACTIVE && 
					stnTimeTable.get(currentStnStateListIndex).get(timeLineStateList.indexOf(tl)).getElementState() == NsharpConstants.State.AVAIL){
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
			if(tl.getStnState()  == NsharpConstants.State.ACTIVE && 
					stnTimeTable.get(stnStateList.indexOf(tl)).get(currentTimeLineStateListIndex).getElementState() == NsharpConstants.State.AVAIL){
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
		if(timeLineStateList.get(currentTimeLineStateListIndex).getTimeState() == NsharpConstants.State.ACTIVE && 
				stnStateList.get(currentStnStateListIndex).getStnState() == NsharpConstants.State.ACTIVE )
			return;
		else if(timeLineStateList.get(currentTimeLineStateListIndex).getTimeState() == NsharpConstants.State.INACTIVE && 
				stnStateList.get(currentStnStateListIndex).getStnState() == NsharpConstants.State.ACTIVE ){
			//find first active time line from list
			for(NsharpTimeLineStateProperty sndProp: timeLineStateList){
				if(sndProp.getTimeState() == NsharpConstants.State.ACTIVE ){
					currentTimeLineStateListIndex = timeLineStateList.indexOf(sndProp);
					//current stn  may not be available for this time line
					if(stnTimeTable.get(currentStnStateListIndex).get(currentTimeLineStateListIndex).getElementState() ==
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
		else if(timeLineStateList.get(currentTimeLineStateListIndex).getTimeState() == NsharpConstants.State.ACTIVE && 
				stnStateList.get(currentStnStateListIndex).getStnState() == NsharpConstants.State.INACTIVE ){
			for(NsharpStationStateProperty sndProp: stnStateList){
				if(sndProp.getStnState() == NsharpConstants.State.ACTIVE) {
					currentStnStateListIndex = stnStateList.indexOf(sndProp);
					//current time line may not be available for this stn
					if(stnTimeTable.get(currentStnStateListIndex).get(currentTimeLineStateListIndex).getElementState() ==
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
				if(sndProp.getTimeState() == NsharpConstants.State.ACTIVE ){
					currentTimeLineStateListIndex = timeLineStateList.indexOf(sndProp);
					found = true;
					break;
				}
				if(!found)
					currentTimeLineStateListIndex = -1;
			}
			for(NsharpStationStateProperty sndProp: stnStateList){
				if(sndProp.getStnState() == NsharpConstants.State.ACTIVE) {
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
			if(tl.getTimeState() == NsharpConstants.State.ACTIVE){
				for(NsharpStationStateProperty stn: stnStateList){
					if(stn.getStnState() == NsharpConstants.State.ACTIVE && 
							stnTimeTable.get(stnStateList.indexOf(stn)).get(timeLineStateList.indexOf(tl)).getElementState() ==
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
		//System.out.println("stn to be added "+ stnId + " timeline "+tmLine);
		NsharpSoundingElementStateProperty newSndPropElem=null;
		NsharpSoundingElementStateProperty dummySndPropElem=null;
		int tmIndex = getTmLineIndexFromTimeLineStateList(tmLine);//getTmLineIndexByTimeLine(tmLine);
		int stnIndex = getStnIndexFromStnStateList(stnId);//getStnIndexByStnId(stnId);
		if(tmIndex>=0 && stnIndex>=0){
			if(stnTimeTable.get(stnIndex).get(tmIndex).getElementState() == NsharpConstants.State.AVAIL)
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
				if(tmLine.equals(sndProp.getTimeDescription())){
					//stnId state should be active, but time line state should based on its peer in other stn
					newSndPropElem = 
						new NsharpSoundingElementStateProperty(elementDesc, NsharpConstants.State.AVAIL,stnId, tmLine,  stnInfo);
					
					newList.add(newSndPropElem);
				}
				else{
					//create time line for  not avail sounding profiles
					String elmDes= stnId+" "+sndProp.getTimeDescription();
					dummySndPropElem = 
						new NsharpSoundingElementStateProperty(elmDes, NsharpConstants.State.NOTAVAIL,stnId,  sndProp.getTimeDescription(),  stnInfo);
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
					if(sndProp.getStnDescription().equals(stnId) ){
						//time line state should be active, but stnId  state should based on its peer in other  time line
						newSndPropElem = 
							new NsharpSoundingElementStateProperty(elementDesc, NsharpConstants.State.AVAIL,stnId, tmLine,  stnInfo);
						//newSndPropElem.getElementState() = decideNewLoadElementState(newSndPropElem);
						stnList.add(newSndPropElem);
					}
					else{
						String elmDes= sndProp.getStnDescription()+" "+tmLine;
						dummySndPropElem = 
							new NsharpSoundingElementStateProperty(elmDes, NsharpConstants.State.NOTAVAIL,sndProp.getStnDescription(), tmLine,  sndProp.getStnInfo());
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
					if(tmLine.equals(sndProp.getTimeDescription())){
						//stnId state should be active, but time line state should based on its peer in other stn
						newSndPropElem = 
							new NsharpSoundingElementStateProperty(elementDesc, NsharpConstants.State.AVAIL,stnId, tmLine,  stnInfo);
						
						newList.add(newSndPropElem);
					}
					else{
						//create time line for  not avail sounding profiles
						String elmDes= stnId+" "+sndProp.getTimeDescription();
						dummySndPropElem = 
							new NsharpSoundingElementStateProperty(elmDes, NsharpConstants.State.NOTAVAIL,stnId,  sndProp.getTimeDescription(),  stnInfo);
						newList.add(dummySndPropElem);
					}
					
				}
				// we do not have to sort time line as we add it according to newly sorted timeLineStateList
				//now add new time line element for each existing stns, except the new one, 
				//Note that we have NOT add "newList" to stnTimeTable yet, so just loop through "current" table.
				for(List<NsharpSoundingElementStateProperty> stnList: stnTimeTable){
					for(NsharpSoundingElementStateProperty sndProp: stnList){
						String elmDes= sndProp.getStnDescription()+" "+tmLine;
						dummySndPropElem = 
							new NsharpSoundingElementStateProperty(elmDes, NsharpConstants.State.NOTAVAIL,sndProp.getStnDescription(), tmLine,  sndProp.getStnInfo());
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
			if(curSndProfileProp.getElementDescription().equals(dataTmLine)){
				curSndDeleted = true;
			}
			dataTimelineSndLysListMap.remove(dataTmLine);
			originalDataTimelineSndLysListMap.remove(dataTmLine);
			
			//set state to NOTAVAIL from stnTimeTable
			boolean setdone = false;
			for(List<NsharpSoundingElementStateProperty> stnList: stnTimeTable){
				for(NsharpSoundingElementStateProperty elm: stnList){
					if(dataTmLine.equals(elm.getElementDescription())){
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
		if(curSndDeleted || soundingLys == null){
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
						if( curSndProfileProp.getElementDescription().equals(elm.getElementDescription())){
							currentTimeLineStateListIndex = stnList.indexOf(elm);
							currentStnStateListIndex = stnTimeTable.indexOf(stnList);
							break;
						}
					}
				}
			}
		}
		
		setCurrentSoundingLayerInfo();
		resetData();
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

			if(stnTimeTable.get( currentStnStateListIndex).get( currentTimeLineStateListIndex).getElementState() == NsharpConstants.State.AVAIL){

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
	public void addRsc(Map<String, List<NcSoundingLayer>> soundMap,  NsharpStationInfo stnInfo, boolean displayNewData){
		deepCopyDataMap(this.originalDataTimelineSndLysListMap,this.dataTimelineSndLysListMap);
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
		if(soundMap.size() <=0 || (skewtPaneRsc==null)){
			return;
		}
		if(timeLineStateList.isEmpty() || stnStateList.isEmpty() || 
				currentTimeLineStateListIndex < 0 || currentStnStateListIndex < 0){
			//if no data was loaded since, then display this data any way
			displayNewData = true;
		}
		//save current timeline and stn state properties if we are NOT loading new data
		NsharpTimeLineStateProperty currentTL=null;
		NsharpStationStateProperty currentStn=null;
		NsharpSoundingElementStateProperty currentPreSndProfileProp=null;
		if(!displayNewData){
			currentTL = timeLineStateList.get(currentTimeLineStateListIndex);
			currentStn = stnStateList.get(currentStnStateListIndex);
			currentPreSndProfileProp = preSndProfileProp;
		}
		//add new data to map/lists
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
		if(displayNewData){
		//Set default parcel trace data
			currentParcel = NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;
			currentParcelLayerPressure = NsharpNativeConstants.MU_LAYER; 
		setCurrentSoundingLayerInfo();
		resetData();
		}
		else {
			//Not display new data. Reset current "parameter"s after adding data to map/lists
			currentStnStateListIndex= stnStateList.indexOf(currentStn);
			currentTimeLineStateListIndex = timeLineStateList.indexOf(currentTL);
			preSndProfileProp = currentPreSndProfileProp;
			curSndProfileProp = stnTimeTable.get(currentStnStateListIndex).get(currentTimeLineStateListIndex);
		}

		//set total time line group and stn id list page  number
		int numTimeLinePerPage = (cnYOrig-dtNextPageEnd)/charHeight;
		//System.out.println("numTimeLinePerPage="+numTimeLinePerPage);
		totalTimeLinePage = timeLineStateList.size()/numTimeLinePerPage + 1; //NEW CODE
		curTimeLinePage = currentTimeLineStateListIndex/numTimeLinePerPage + 1; //NEW CODE
		totalStnIdPage = stnStateList.size()/numTimeLinePerPage + 1; //NEW CODE
		curStnIdPage= currentStnStateListIndex/numTimeLinePerPage + 1; //NEW CODE
		
		/* Chin: TBD: do we need these code?
		NsharpSkewTPaneDisplay renderableDisplay = (NsharpSkewTPaneDisplay) skewtPaneRsc.getDescriptor().getRenderableDisplay();
		if(renderableDisplay != null) {
		    IDisplayPaneContainer editor = renderableDisplay.getContainer();
		    if(editor instanceof NsharpEditor) {
		       int editorNum = ((NsharpEditor) editor).getEditorNum();
		       renderableDisplay.setEditorNum(editorNum);
		    }
		}*/
		//set data time to descriptor
		//this is necessary for looping	
        if (( skewtPaneRsc.getDescriptor().getFramesInfo().getFrameCount() == 0)&& !getTimeMatcher) {
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
            skewtPaneRsc.getDescriptor().setDataTimes(dataTimes);
            getTimeMatcher=true;
        }
        
		NsharpShowTextDialog textarea =  NsharpShowTextDialog.getAccess();
		if(textarea != null){
			textarea.refreshTextData();
		}
		NsharpPaletteWindow win = NsharpPaletteWindow.getInstance() ;
		if(win!=null)
			currentGraphMode=win.getCurrentGraphMode();
		
		deepCopyDataMap(this.dataTimelineSndLysListMap,this.originalDataTimelineSndLysListMap);
		/*NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
		if (editor != null) {
			editor.refresh();
		}*/
		refreshPane();
		
	}

	public void addRsc(Map<String, List<NcSoundingLayer>> soundMap,  NsharpStationInfo stnInfo){
		//by default, display new data 
		this.addRsc(soundMap, stnInfo,true);
		return;
	}
 	public String getPickedStnInfoStr() {
		return pickedStnInfoStr;
	}


	public void handleUserClickOnStationId(Coordinate c) {
		int numStnIdPerPage = (cnYOrig-dtNextPageEnd)/charHeight;
		//first to find if it is for change to next page, or change sorting
		//System.out.println("numTimeLinePerPage="+numTimeLinePerPage+"gap="+(cnYOrig-dtNextPageEnd));
		int index =((int)(c.y - dtYOrig))/ charHeight;
		
		if(index == 0 ){
			//change to next/previous page
			if( totalStnIdPage == 1) 
				return;
			if((c.x - (dtXOrig+dtWidth)) < (dtWidth/2)){
			curStnIdPage++;
			if(curStnIdPage>totalStnIdPage)
				curStnIdPage=1;
			} else {
				curStnIdPage--;
				if(curStnIdPage <=0)
					curStnIdPage = totalStnIdPage;
			}
			return;
		}
		// recalculate index for time line
		index =((int)(c.y - dtNextPageEnd))/ charHeight +
		 			(curStnIdPage-1)* numStnIdPerPage ;	
		
		if( index  < this.stnStateList.size() ){
			switch(stnStateList.get(index).getStnState()){
			
			case INACTIVE:
				stnStateList.get(index).setStnState( NsharpConstants.State.ACTIVE);
				break;
			case ACTIVE:
				stnStateList.get(index).setStnState( NsharpConstants.State.INACTIVE);
				
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
		int numTimeLinePerPage = (cnYOrig-dtNextPageEnd)/charHeight;
		//first to find if it is for change to next/prev page
		//System.out.println("numTimeLinePerPage="+numTimeLinePerPage+"gap="+(cnYOrig-dtNextPageEnd));
		int index =((int)(c.y - dtYOrig))/ charHeight;
		if(index == 0 ){
			//change to next/previous page
			if( totalTimeLinePage == 1) 
				return;
			if((c.x - dtXOrig) < (dtWidth/2)){
			curTimeLinePage++;
			if(curTimeLinePage>totalTimeLinePage)
				curTimeLinePage=1;
			} else {
				curTimeLinePage--;
				if(curTimeLinePage <=0)
					curTimeLinePage = totalTimeLinePage;
			}
			return;
		}
		// recalculate index for time line
		index =((int)(c.y - dtNextPageEnd))/ charHeight +
		 			(curTimeLinePage-1)* numTimeLinePerPage ;	
		
		if( index  < timeLineStateList.size() ){
			switch(timeLineStateList.get(index).getTimeState()){
			case INACTIVE:
				timeLineStateList.get(index).setTimeState( NsharpConstants.State.ACTIVE);
				break;
			case ACTIVE:
				timeLineStateList.get(index).setTimeState( NsharpConstants.State.INACTIVE);
				break;
			default:
				return;

			}

			findCurrentElementIndexesAfterConfig();
			setCurSndProfileProp();
			setCurrentSoundingLayerInfo();
			resetData();

			if(compareStnIsOn){
				if(hodoPaneRsc!=null)
					hodoPaneRsc.createRscHodoWindShapeAll();
				if(skewtPaneRsc!=null)
					skewtPaneRsc.handleResize();
			}
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
					stnTimeTable.get(currentStnStateListIndex).get(currentTimeLineStateListIndex).getElementState() == NsharpConstants.State.AVAIL){
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
					stnTimeTable.get(currentStnStateListIndex).get(currentTimeLineStateListIndex).getElementState() == NsharpConstants.State.AVAIL){
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
			if(currentOpDirection == IFrameCoordinator.FrameChangeOperation.NEXT ){
				currentTimeLineStateListIndex--;
				if(currentTimeLineStateListIndex <= 0){
					//the end of forward direction, change direction to backward
					currentOpDirection = IFrameCoordinator.FrameChangeOperation.PREVIOUS;
					currentTimeLineStateListIndex=0;
				}

			}
			else{ // direction is FrameChangeOperation.PREVIOUS
				currentTimeLineStateListIndex++;
				if(currentTimeLineStateListIndex >= timeLineStateList.size()-1){
					//the end of backward direction, change direction to forward
					currentOpDirection = IFrameCoordinator.FrameChangeOperation.NEXT;
					currentTimeLineStateListIndex = timeLineStateList.size()-1;
				}
			}
			if(timeLineStateList.get(currentTimeLineStateListIndex).getTimeState() == NsharpConstants.State.ACTIVE &&
				stnTimeTable.get(currentStnStateListIndex).get(currentTimeLineStateListIndex).getElementState() == NsharpConstants.State.AVAIL){
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
			
			int numTimeLinePerPage = (cnYOrig-dtNextPageEnd)/charHeight;
			curTimeLinePage = currentTimeLineStateListIndex/numTimeLinePerPage + 1;
			setCurSndProfileProp();
 			setCurrentSoundingLayerInfo();
 			resetData();
			/*NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
			if (editor != null) {
				editor.refresh();
			}*/
 			refreshPane();
		}
				
	}
	public enum LoopMode {
        Forward, Backward, Cycle
    };

 	private int getActiveTimeLineNumber(){
 		int n=0;
 		for (NsharpTimeLineStateProperty tm:timeLineStateList){
 			if(tm.getTimeState() == NsharpConstants.State.ACTIVE){
 				n++;
 			}
 		}
 		return n;
 	}
 	private int getActiveStnNumber(){
 		int n=0;
 		for (NsharpStationStateProperty stn:stnStateList){
 			if(stn.getStnState() == NsharpConstants.State.ACTIVE){
 				n++;
 			}
 		}
 		return n;
 	}
 	public void setSteppingTimeLine(IFrameCoordinator.FrameChangeOperation operation, IFrameCoordinator.FrameChangeMode mode) {
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
 					&& stnTimeTable.get(currentStnStateListIndex).get(targetIndex).getElementState() == NsharpConstants.State.NOTAVAIL){
 						continue;
 					}
 					else if(compareStnIsOn){
 						boolean found = false;
 						//find an active and available stn for this timeline and set is as current
 						for (int i=0; i < stnStateList.size(); i++){
 							if(stnStateList.get(i).getStnState()==NsharpConstants.State.ACTIVE &&
 									stnTimeTable.get(i).get(targetIndex).getElementState() == NsharpConstants.State.AVAIL){
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
 			int numTimeLinePerPage = (cnYOrig-dtNextPageEnd)/charHeight;
 			curTimeLinePage = currentTimeLineStateListIndex/numTimeLinePerPage + 1;
 			setCurSndProfileProp();
 			setCurrentSoundingLayerInfo();
 			resetData();
 			
 			/*NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
 			if (editor != null) {
 				editor.refresh();
 			}*/
 			refreshPane();
 		}
 	}
 	
 	/*
 	 * Stn index stepping is only controlled by up/down arrow keys, down key = PREVIOUS operation, up key = NEXT operation
 	 */
 	public void setSteppingStnIdList(IFrameCoordinator.FrameChangeOperation operation) {
 		if( this.stnStateList.size() > 0 && getActiveStnNumber()>1/* && getAvailStnNumber(currentTimeLineStateListIndex) > 1*/){ 
 			
 			int counter=0;
 			while(true){
 				switch(operation){
  				case NEXT:
  					currentStnStateListIndex= currentStnStateListIndex + this.stnStateList.size();
 					currentStnStateListIndex--;
 					currentStnStateListIndex = currentStnStateListIndex % this.stnStateList.size();	
 					break;
 				case PREVIOUS:
 					// so, we wont get a negative number					
 					currentStnStateListIndex++;
 					currentStnStateListIndex = currentStnStateListIndex % this.stnStateList.size(); 
 					break;
 				}
 				counter++;
 				//System.out.println("counter = "+ counter);
 				if(counter >= stnStateList.size())
 					return; // looped through whole list already, and index back to original
 				if(stnStateList.get(currentStnStateListIndex).getStnState() == NsharpConstants.State.ACTIVE){
 					if (compareStnIsOn 
 							&& stnTimeTable.get(currentStnStateListIndex).get(currentTimeLineStateListIndex).getElementState() == NsharpConstants.State.NOTAVAIL){
 						continue;
 					}
 					else if(compareTmIsOn){
 						boolean found = false;
 						//find an active and available timeline for this stn and set is as current
 						for (int i=0; i < timeLineStateList.size(); i++){
 							if(timeLineStateList.get(i).getTimeState()==NsharpConstants.State.ACTIVE &&
 									stnTimeTable.get(currentStnStateListIndex).get(i).getElementState() == NsharpConstants.State.AVAIL){
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
 			int numTimeLinePerPage = (cnYOrig-dtNextPageEnd)/charHeight;
 			curStnIdPage = currentStnStateListIndex/numTimeLinePerPage + 1;
 			setCurSndProfileProp();
 			setCurrentSoundingLayerInfo();
 			resetData();
 			
 			/*NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
 			if (editor != null) {
 				editor.refresh();
 			}*/
 			refreshPane();
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
	 *    
	*/
	public Coordinate getClosestHodoPoint(Coordinate inputC){
		//System.out.println("picked pt  CX "+ inputC.x + " CY "+ inputC.y);
		Coordinate closeptC = new Coordinate(0,0);
		if(hodoPaneRsc==null)
			return closeptC;
		
		double curSmallestDist=10000; // picked a impossible big number to start with
		double distance;
		boolean ptFound = false;
		NcSoundingLayer layer;
		//
		// Note: soundingLys list sorted with highest pressure as first element
		//		
		for (int i=0; i< this.soundingLys.size(); i++) {
			layer = this.soundingLys.get(i);
			double curS, curD;
			curS = layer.getWindSpeed();
			curD = layer.getWindDirection();
			closeptC = WxMath.uvComp((float)curS, (float)curD);
			closeptC = hodoPaneRsc.getHodoBackground().getWorld().map(closeptC);
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
			closeptC = hodoPaneRsc.getHodoBackground().getWorld().map(closeptC);
		}
		
		//System.out.println("picked closeptCx " + closeptC.x+ " closeptCy "+ closeptC.y);
		return closeptC;
	}

	public List<NcSoundingLayer> getSoundingLys() {
		return soundingLys;
	}

	public List<NcSoundingLayer> getPreviousSoundingLys() {
		return previousSoundingLys;
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
	public void updateDisplay(IRenderableDisplay[] displayArray, String paneConfigurationName) {
		skewtPaneRsc = null;
		witoPaneRsc = null;
		hodoPaneRsc = null;
		timeStnPaneRsc = null;
		insetPaneRsc = null;
		dataPaneRsc = null;
		spcGraphsPaneRsc = null;
		futurePaneRsc = null;
		for(IRenderableDisplay disp: displayArray) {
			ResourcePair rscP = disp.getDescriptor().getResourceList().get(0);
			NsharpAbstractPaneResource absPaneRsc = (NsharpAbstractPaneResource)rscP.getResource();
			if (absPaneRsc instanceof NsharpSkewTPaneResource){
				skewtPaneRsc = (NsharpSkewTPaneResource)absPaneRsc ;
			skewtPaneRsc.setLinePropertyMap(linePropertyMap);
			skewtPaneRsc.setGraphConfigProperty(graphConfigProperty);
			skewtPaneRsc.setNsharpNative(nsharpNative);
		}
			else if (absPaneRsc instanceof NsharpDataPaneResource){
				dataPaneRsc = (NsharpDataPaneResource)absPaneRsc;
			dataPaneRsc.setLinePropertyMap(linePropertyMap);
			dataPaneRsc.setGraphConfigProperty(graphConfigProperty);
			dataPaneRsc.setNsharpNative(nsharpNative);
			dataPaneRsc.setPageDisplayOrderNumberArray(pageDisplayOrderNumberArray);
		}
			else if (absPaneRsc instanceof NsharpHodoPaneResource){
				hodoPaneRsc = (NsharpHodoPaneResource)absPaneRsc;
			hodoPaneRsc.setLinePropertyMap(linePropertyMap);
			hodoPaneRsc.setGraphConfigProperty(graphConfigProperty);
			hodoPaneRsc.setNsharpNative(nsharpNative);
		}
			else if (absPaneRsc instanceof NsharpWitoPaneResource &&
					(paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)|| 
				paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_1_STR)||
							paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_2_STR))){
			
				witoPaneRsc = (NsharpWitoPaneResource)absPaneRsc;
				witoPaneRsc.setLinePropertyMap(linePropertyMap);
				witoPaneRsc.setGraphConfigProperty(graphConfigProperty);
				witoPaneRsc.setNsharpNative(nsharpNative);

			}
			else if (absPaneRsc instanceof NsharpInsetPaneResource &&
					(paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)|| 
							paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_1_STR)||
							paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_2_STR))){

				insetPaneRsc = (NsharpInsetPaneResource)absPaneRsc;
				insetPaneRsc.setLinePropertyMap(linePropertyMap);
				insetPaneRsc.setGraphConfigProperty(graphConfigProperty);
				insetPaneRsc.setNsharpNative(nsharpNative);
		
			}
			else if (absPaneRsc instanceof NsharpSpcGraphsPaneResource && paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)){
				spcGraphsPaneRsc = (NsharpSpcGraphsPaneResource)absPaneRsc;
				spcGraphsPaneRsc.setLinePropertyMap(linePropertyMap);
				spcGraphsPaneRsc.setGraphConfigProperty(graphConfigProperty);
				spcGraphsPaneRsc.setNsharpNative(nsharpNative);
			}
			else if(absPaneRsc instanceof NsharpAbstractPaneResource && paneConfigurationName.equals(NsharpConstants.PANE_SIMPLE_D2D_CFG_STR)){
				futurePaneRsc = (NsharpAbstractPaneResource)absPaneRsc;
				futurePaneRsc.setLinePropertyMap(linePropertyMap);
				futurePaneRsc.setGraphConfigProperty(graphConfigProperty);
				futurePaneRsc.setNsharpNative(nsharpNative);

			}
			else if (absPaneRsc instanceof NsharpTimeStnPaneResource && 
					(paneConfigurationName.equals(NsharpConstants.PANE_SIMPLE_D2D_CFG_STR)|| 
				paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_1_STR)||
							paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_2_STR))){
				timeStnPaneRsc = (NsharpTimeStnPaneResource)absPaneRsc ;
				timeStnPaneRsc.setLinePropertyMap(linePropertyMap);
				timeStnPaneRsc.setGraphConfigProperty(graphConfigProperty);
				timeStnPaneRsc.setNsharpNative(nsharpNative);
			}
		}
		this.displayArray = displayArray;
	}
	public void resetRscSoundingData(){
		if(skewtPaneRsc!=null)
			skewtPaneRsc.resetData(soundingLys,previousSoundingLys);
		if(hodoPaneRsc!=null) 
			hodoPaneRsc.resetData(soundingLys,previousSoundingLys);
		if(witoPaneRsc!=null)
			witoPaneRsc.resetData(soundingLys, previousSoundingLys);
		if(dataPaneRsc!=null) 
			dataPaneRsc.resetData(soundingLys, previousSoundingLys);
		if(insetPaneRsc!=null)
			insetPaneRsc.resetData(soundingLys, previousSoundingLys);
		if(spcGraphsPaneRsc!=null)
			spcGraphsPaneRsc.resetData(soundingLys, previousSoundingLys);
	}


	public NsharpResourceHandler(IRenderableDisplay[] displayArray, NsharpEditor editor) {
    	//System.out.println("NsharpResourceHandler constructed");
		//myNsharpEditor = editor;
        this.soundingMap = new HashMap<Date, SoundingParams>();
        elementColorMap.put(NsharpConstants.State.CURRENT,NsharpConstants.color_green); //green
        elementColorMap.put(NsharpConstants.State.ACTIVE,NsharpConstants.color_yellow);//cyan
        elementColorMap.put(NsharpConstants.State.INACTIVE,NsharpConstants.color_white);//white
        elementColorMap.put(NsharpConstants.State.NOTAVAIL,NsharpConstants.color_red);//white
        elementColorMap.put(NsharpConstants.State.OVERLAY,NsharpConstants.color_red);//red
        elementColorMap.put(NsharpConstants.State.AVAIL,NsharpConstants.color_yellow);//white
        nsharpNative = new NsharpNative();
        //System.out.println("NsharpResourceHandler constructed"+this.toString() + " nsharpNative="+nsharpNative.toString());
		//based on BigNsharp storm slinky color used and gempak color definition
		stormSlinkyColorMap.put(new Integer(3),NsharpConstants.color_green); //green
		stormSlinkyColorMap.put(new Integer(7),NsharpConstants.color_magenta);
		stormSlinkyColorMap.put(new Integer(6),NsharpConstants.color_cyan);
		stormSlinkyColorMap.put(new Integer(13),NsharpConstants.color_violet_md);
		stormSlinkyColorMap.put(new Integer(20),NsharpConstants.color_yellow_DK);
		stormSlinkyColorMap.put(new Integer(27),NsharpConstants.color_cyan_md);
		NsharpPaletteWindow win = NsharpPaletteWindow.getInstance() ;
		if(win!=null)
			currentGraphMode=win.getCurrentGraphMode();
			
		
		// new for configMgr
		configMgr = NsharpConfigManager.getInstance();
		configStore = configMgr.retrieveNsharpConfigStoreFromFs();
		graphConfigProperty = configStore.getGraphProperty();
		paneConfigurationName = graphConfigProperty.getPaneConfigurationName();
		
		int tempOffset = graphConfigProperty.getTempOffset();
		NsharpWxMath.setTempOffset(tempOffset);
		linePropertyMap = configStore.getLinePropertyMap();
		dataPageProperty = configStore.getDataPageProperty();
		updatePageOrderArray();
		updateDisplay(displayArray,paneConfigurationName);
		//pspLsner = new NsharpPerspectiveListener();
		//pspLsner.setRscHandler(this);
		//pspLsner.setMyPerspectiveId(VizPerspectiveListener.getCurrentPerspectiveManager().getPerspectiveId());
		//System.out.println("perspective id = " + VizPerspectiveListener.getCurrentPerspectiveManager().getPerspectiveId());
		//PlatformUI.getWorkbench().getActiveWorkbenchWindow().addPerspectiveListener(pspLsner);
		
		
    }
    public void cleanUpRsc(){
    	if(dataTimelineSndLysListMap!= null)
    		dataTimelineSndLysListMap.clear();
    	if(originalDataTimelineSndLysListMap!=null)
    		originalDataTimelineSndLysListMap.clear();
    	if(soundingLysList!=null)
    		soundingLysList.clear();
    	if(soundingLys!=null){
    		soundingLys.clear();
    		soundingLys=null;
    	}
    	if(previousSoundingLys!=null){
    		previousSoundingLys.clear();
    		previousSoundingLys = null;
    	}
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
    	currentTextChapter = 1;
    	currentInsetPage = 1;
    	currentParcel = NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;
    	currentParcelLayerPressure = NsharpNativeConstants.MU_LAYER;
    	resetData();
    }
	public void disposeInternal() {
		//System.out.println("NsharpSkewTResource disposeInternal called");
	    soundingMap= null;
	    //parcelList= null;
	    listenerList=null;
	    soundingLysList=null;
	    soundingLys = null;
		previousSoundingLys = null;
		dataTimelineSndLysListMap = null;
		originalDataTimelineSndLysListMap= null;
		intpSndLst = null;
		stormSlinkyColorMap = null;
		elementColorMap= null;
		
		
		if(NsharpParcelDialog.getAccess()!= null){
			NsharpParcelDialog.getAccess().reset();
		}
		
		nsharpNative = null;
		//PlatformUI.getWorkbench().getActiveWorkbenchWindow().removePerspectiveListener(pspLsner);
		//pspLsner = null;
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
    		//for (ParcelData parData: parcelList){
    			//plotNsharpParcelTraceCurve(null, 0, world, NsharpConstants.color_white,parData.parcelType, parData.userPressure, gc, true);
    			//nsharpNative.nsharpLib.define_parcel(parData.parcelType, parData.parcelLayerPressure);
    			nsharpNative.nsharpLib.define_parcel(currentParcel,currentParcelLayerPressure);
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
    		//}
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
    

	public boolean isPlotInteractiveTemp() {
		return plotInteractiveTemp;
	}


	public void setPlotInteractiveTemp(boolean plotInteractiveTemp) {
		this.plotInteractiveTemp = plotInteractiveTemp;
		if(skewtPaneRsc!=null)
			skewtPaneRsc.setPlotInteractiveTemp(plotInteractiveTemp);
	}
	public void setInteractiveTempPointCoordinate(
			Coordinate interactiveTempPointCoordinate) {
		//System.out.println("setInteractiveTempPointCoordinate called");
		this.interactiveTempPointCoordinate = interactiveTempPointCoordinate;
		plotInteractiveTemp = true;
		if(skewtPaneRsc!=null){
			skewtPaneRsc.setPlotInteractiveTemp(plotInteractiveTemp);
			skewtPaneRsc.setInteractiveTempPointCoordinate(interactiveTempPointCoordinate);
		}
	}
	public void setInteractiveHodoPointCoordinate(Coordinate c){
		if(hodoPaneRsc == null)
			return;
		try {
			NcSoundingLayer hodoLayer = soundingLys.get(hodoEditingSoundingLayerIndex);
			if(hodoLayer != null){
				//TTR575
				this.dataTimelineSndLysListMap.put(pickedStnInfoStr, this.soundingLys);
				nsharpNative.populateSndgData(soundingLys);
				//end TTR575
				Coordinate c1 = hodoPaneRsc.getHodoBackground().getWorld().unMap(c.x, c.y);
				//System.out.println("picked pt after unmap CX "+ c1.x + " CY "+ c1.y);
				c1 = WxMath.speedDir((float) c1.x, (float) c1.y);
				hodoLayer.setWindSpeed((float)c1.x);
				hodoLayer.setWindDirection((float)c1.y);
				hodoPaneRsc.createRscHodoWindShapeAll();
				if(witoPaneRsc!=null)
					witoPaneRsc.createAllWireFrameShapes();
				if(insetPaneRsc!=null)
					insetPaneRsc.createInsetWireFrameShapes();
				if(skewtPaneRsc!=null)
					skewtPaneRsc.handleResize();
			}
		}
		catch(Exception e)  {
			
		}
	}
	public void applyMovingTempLine(){
		if(skewtPaneRsc==null)
			return;
		Coordinate inC = NsharpWxMath.reverseSkewTXY(skewtPaneRsc.getWorld().unMap(interactiveTempPointCoordinate));
		float inTemp = (float) inC.x;
		currentSoundingLayerIndex = skewtPaneRsc.getCurrentSoundingLayerIndex();
		currentTempCurveType = skewtPaneRsc.getCurrentTempCurveType();
		float currentLayerTemp, currentLayerDewP;
		float smallestGap = skewtPaneRsc.getTempDewPtSmallestGap();
		float tempShiftedDist;
    	currentLayerTemp = soundingLys.get(currentSoundingLayerIndex).getTemperature();
    	currentLayerDewP = soundingLys.get(currentSoundingLayerIndex).getDewpoint();
		if(currentTempCurveType == TEMP_TYPE){
    		if(inTemp < currentLayerTemp){
    			// shift to left, tempShiftedDist should be a negative number
    			if((currentLayerTemp - inTemp)> smallestGap){
    				tempShiftedDist = -smallestGap;
    			}
    			else {
    				tempShiftedDist = inTemp - currentLayerTemp;
    			}
    		}
    		else {
    			// shift to right, tempShiftedDist should be a positive number
    			tempShiftedDist = inTemp - currentLayerTemp;
    		}
    	}
    	else {
    		if(inTemp < currentLayerDewP){
    			// shift to left, tempShiftedDist should be a negative number
    			tempShiftedDist = inTemp - currentLayerDewP;
    		}
    		else {
    			// shift to right, tempShiftedDist should be a positive number
    			if((inTemp - currentLayerDewP)> smallestGap){
    				tempShiftedDist = smallestGap;
    			}
    			else {
    				tempShiftedDist = inTemp - currentLayerDewP;
    			}
    		}
    	}
		for (NcSoundingLayer layer : soundingLys) {
			float t;
			if(currentTempCurveType == TEMP_TYPE){
				t = layer.getTemperature();
				if (t != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA  ) {
					layer.setTemperature(t+tempShiftedDist);
				}
			}
			else{
				t = layer.getDewpoint();
				if (t != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA  ) {
					layer.setDewpoint(t+tempShiftedDist);
				}
			}
		}
		this.dataTimelineSndLysListMap.put(pickedStnInfoStr, this.soundingLys);
		//re-populate snd data to nsharp native code lib for later calculating
		nsharpNative.populateSndgData(soundingLys);
		//get storm motion wind data after populate sounding from NsharpLib		
		skewtPaneRsc.setSoundingLys(soundingLys);
		skewtPaneRsc.handleResize();
		if(hodoPaneRsc!=null){
			hodoPaneRsc.setSoundingLys(soundingLys);
			hodoPaneRsc.createRscHodoWindShapeAll();
		}
	}
	public void applyInteractiveTempPoint(){
		if(skewtPaneRsc==null)
			return;
		Coordinate inC = NsharpWxMath.reverseSkewTXY(skewtPaneRsc.getWorld().unMap(interactiveTempPointCoordinate));
		double inTemp = inC.x;
		currentSoundingLayerIndex = skewtPaneRsc.getCurrentSoundingLayerIndex();
		NcSoundingLayer layer = this.soundingLys.get(currentSoundingLayerIndex);
		currentTempCurveType = skewtPaneRsc.getCurrentTempCurveType();
		//System.out.println("applyInteractiveTempPoint called pressure " + inC.y + " temp "+ inTemp +
		//		" currentTempCurveType " + currentTempCurveType );
		
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
		skewtPaneRsc.setSoundingLys(soundingLys);
		skewtPaneRsc.handleResize();
		//skewtPaneRsc.createRscPressTempCurveShapeAll();	
		//skewtPaneRsc.createRscwetBulbTraceShape();
		//skewtPaneRsc.createRscVTempTraceShape();
		//skewtPaneRsc.createParcelShapes(parcelList);
		if(hodoPaneRsc!=null){
			hodoPaneRsc.setSoundingLys(soundingLys);
			hodoPaneRsc.createRscHodoWindShapeAll();
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
		if(skewtPaneRsc!=null)
			skewtPaneRsc.setSoundingLys(soundingLys);
		if(hodoPaneRsc!=null)
			hodoPaneRsc.setSoundingLys(soundingLys);
		if(insetPaneRsc!=null)
			insetPaneRsc.setSoundingLys(soundingLys);
		if(witoPaneRsc!=null)
			witoPaneRsc.setSoundingLys(soundingLys);
		if(dataPaneRsc!=null)
			dataPaneRsc.setSoundingLys(soundingLys);
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
		if(skewtPaneRsc!=null){
			skewtPaneRsc.setSoundingLys(soundingLys);
			skewtPaneRsc.handleResize();
		}
		if(hodoPaneRsc!=null){
			hodoPaneRsc.setSoundingLys(soundingLys);
			hodoPaneRsc.createRscHodoWindShapeAll();
		}
		if(insetPaneRsc!=null){
			insetPaneRsc.setSoundingLys(soundingLys);
			insetPaneRsc.createInsetWireFrameShapes();
		}
		if(witoPaneRsc!=null) {
			witoPaneRsc.setSoundingLys(soundingLys);
			witoPaneRsc.createAllWireFrameShapes();
		}
		if(dataPaneRsc!=null)
			dataPaneRsc.setSoundingLys(soundingLys);
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
		if(skewtPaneRsc!=null){
			skewtPaneRsc.setSoundingLys(soundingLys);
			skewtPaneRsc.handleResize();
		}
		if(hodoPaneRsc!=null){
			hodoPaneRsc.setSoundingLys(soundingLys);
			hodoPaneRsc.createRscHodoWindShapeAll();
		}
		if(insetPaneRsc!=null){
			insetPaneRsc.setSoundingLys(soundingLys);
			insetPaneRsc.createInsetWireFrameShapes();
		}
		if(witoPaneRsc!=null) {
			witoPaneRsc.setSoundingLys(soundingLys);
			witoPaneRsc.createAllWireFrameShapes();
		}
		if(dataPaneRsc!=null) 
			dataPaneRsc.setSoundingLys(soundingLys);
	}
	
	


	public void setGraphConfigProperty(NsharpGraphProperty graphConfigProperty) {
		this.graphConfigProperty = graphConfigProperty;
		int tempOffset = graphConfigProperty.getTempOffset();
		NsharpWxMath.setTempOffset(tempOffset);
		if(skewtPaneRsc!=null){
			skewtPaneRsc.setGraphConfigProperty(graphConfigProperty);
			skewtPaneRsc.handleResize();
			skewtPaneRsc.getSkewTBackground().setGraphConfigProperty(graphConfigProperty);
		}
		if(hodoPaneRsc!=null) {
			hodoPaneRsc.setGraphConfigProperty(graphConfigProperty);
			hodoPaneRsc.createRscHodoWindShapeAll();
		}
		if(witoPaneRsc!=null) {
			witoPaneRsc.setGraphConfigProperty(graphConfigProperty);
			witoPaneRsc.createAllWireFrameShapes();
		}
		if(insetPaneRsc!=null){
			insetPaneRsc.setGraphConfigProperty(graphConfigProperty);
			insetPaneRsc.createInsetWireFrameShapes();
		}
	}
	
	
	public NsharpGraphProperty getGraphConfigProperty() {
		return graphConfigProperty;
	}


	public void setLinePropertyMap(
			HashMap<String, NsharpLineProperty> linePropertyMap) {
		this.linePropertyMap = linePropertyMap;
		if(skewtPaneRsc!=null){
			skewtPaneRsc.setLinePropertyMap(linePropertyMap);
			skewtPaneRsc.handleResize();
		}
		if(hodoPaneRsc!=null){
			hodoPaneRsc.setLinePropertyMap(linePropertyMap);
			hodoPaneRsc.createRscHodoWindShapeAll();
		}
		if(timeStnPaneRsc!=null)
			timeStnPaneRsc.setLinePropertyMap(linePropertyMap);
	}
	private void updatePageOrderArray(){
		pageDisplayOrderNumberArray[NsharpConstants.PAGE_SUMMARY1 ] = dataPageProperty.getSummary1Page();
		pageDisplayOrderNumberArray[NsharpConstants.PAGE_SUMMARY2 ] = dataPageProperty.getSummary2Page();
		pageDisplayOrderNumberArray[NsharpConstants.PAGE_PARCEL_DATA ] = dataPageProperty.getParcelDataPage();
		pageDisplayOrderNumberArray[NsharpConstants.PAGE_THERMODYNAMIC_DATA ]  = dataPageProperty.getThermodynamicDataPage();
		pageDisplayOrderNumberArray[NsharpConstants.PAGE_OPC_DATA ]  = dataPageProperty.getOpcDataPage();
		pageDisplayOrderNumberArray[NsharpConstants.PAGE_MIXING_HEIGHT]  = dataPageProperty.getMixingHeightPage();
		pageDisplayOrderNumberArray[NsharpConstants.PAGE_STORM_RELATIVE ]  = dataPageProperty.getStormRelativePage();
		pageDisplayOrderNumberArray[NsharpConstants.PAGE_MEAN_WIND ]  = dataPageProperty.getMeanWindPage();
		pageDisplayOrderNumberArray[NsharpConstants.PAGE_CONVECTIVE_INITIATION ]  = dataPageProperty.getConvectiveInitiationPage();
		pageDisplayOrderNumberArray[NsharpConstants.PAGE_SEVERE_POTENTIAL ]  = dataPageProperty.getSeverePotentialPage();                    		
	}
	public void setDataPageProperty(NsharpDataPageProperty dataPageProperty) {
		this.dataPageProperty = dataPageProperty;
		updatePageOrderArray();
		if(dataPaneRsc!=null)
			dataPaneRsc.setPageDisplayOrderNumberArray(pageDisplayOrderNumberArray);
	}


	public void handleTimeLineActConfig(List<String>  tlList, NsharpConstants.State actSt){
		for(String tlStr: tlList){
			for(NsharpTimeLineStateProperty tl: timeLineStateList){	
				if(tlStr.equals(tl.getTimeDescription())){
					tl.setTimeState( actSt);
					break;
				}
			}
		}
		findCurrentElementIndexesAfterConfig();
		setCurSndProfileProp();
		setCurrentSoundingLayerInfo();
		resetData();
		if(compareStnIsOn){
			if(hodoPaneRsc!=null) 
				hodoPaneRsc.createRscHodoWindShapeAll();
			if(skewtPaneRsc!=null)
				skewtPaneRsc.handleResize();
		}
	}
	public void handleStationActConfig(List<String>  stnList, NsharpConstants.State actSt){
		for(String tlStr: stnList){
			for(NsharpStationStateProperty stn: stnStateList){	
				if(tlStr.equals(stn.getStnDescription())){
					stn.setStnState( actSt);
					break;
				}
			}
		}
		findCurrentElementIndexesAfterConfig();
		setCurSndProfileProp();
		setCurrentSoundingLayerInfo();
		resetData();
	}


	public int getCurrentTextChapter() {
		return currentTextChapter;
	}


	public int getCurrentTimeLineStateListIndex() {
		return currentTimeLineStateListIndex;
	}
	public int getTimeLineStateListSize() {
		return this.timeLineStateList.size();
	}

	public int getCurrentStnStateListIndex() {
		return currentStnStateListIndex;
	}


	public HashMap<String, List<NcSoundingLayer>> getDataTimelineSndLysListMap() {
		return dataTimelineSndLysListMap;
	}


	//public List<ParcelData> getParcelList() {
	//	return parcelList;
	//}


	public float getCurrentParcelLayerPressure() {
		return currentParcelLayerPressure;
	}


	public NsharpSkewTPaneResource getSkewtPaneRsc() {
		return skewtPaneRsc;
	}


	public NsharpHodoPaneResource getHodoPaneRsc() {
		return hodoPaneRsc;
	}


	public NsharpSoundingElementStateProperty getPreSndProfileProp() {
		return preSndProfileProp;
	}
	
	


	public int getCurTimeLinePage() {
		return curTimeLinePage;
	}


	public int getCurrentInsetPage() {
		return currentInsetPage;
	}


	public int getCurrentSoundingLayerIndex() {
		return currentSoundingLayerIndex;
	}


	public IFrameCoordinator.FrameChangeOperation getCurrentOpDirection() {
		return currentOpDirection;
	}


	public NsharpSoundingElementStateProperty getCurSndProfileProp() {
		return curSndProfileProp;
	}


	public int getCurStnIdPage() {
		return curStnIdPage;
	}


	public HashMap<NsharpConstants.State, RGB> getElementColorMap() {
		return elementColorMap;
	}


	public int getTotalTimeLinePage() {
		return totalTimeLinePage;
	}


	public int getTotalStnIdPage() {
		return totalStnIdPage;
	}
	
	public void repopulateSndgData(){
		nsharpNative.populateSndgData(soundingLys);
	}


	public NsharpWitoPaneResource getWitoPaneRsc() {
		return witoPaneRsc;
	}


	public NsharpInsetPaneResource getInsetPaneRsc() {
		return insetPaneRsc;
	}
	


	public NsharpSpcGraphsPaneResource getSpcGraphsPaneRsc() {
		return spcGraphsPaneRsc;
	}


	/*public void setCnYOrig(int cnYOrig) {
		this.cnYOrig = cnYOrig;
	}


	public void setDtNextPageEnd(int dtNextPage_end) {
		this.dtNextPageEnd = dtNextPage_end;
	}


	public void setDtYOrig(int dtYOrig) {
		this.dtYOrig = dtYOrig;
	}*/
	public void setTimeStnBoxData(int cnYOrig,int dtNextPage_end, int dtYOrig , int dtXOrig, int dtWidth, int charHeight){
		this.charHeight = charHeight;
		this.dtYOrig = dtYOrig;
		this.dtXOrig = dtXOrig;
		this.dtWidth = dtWidth;
		this.cnYOrig = cnYOrig;
		this.dtNextPageEnd = dtNextPage_end;
		int numTimeLinePerPage = (cnYOrig-dtNextPageEnd)/charHeight;
		if(numTimeLinePerPage<=0)
			numTimeLinePerPage=1;
		//System.out.println("numTimeLinePerPage="+numTimeLinePerPage);
		totalTimeLinePage = timeLineStateList.size()/numTimeLinePerPage ;
		if(timeLineStateList.size()%numTimeLinePerPage != 0)
			totalTimeLinePage= totalTimeLinePage+1;
		curTimeLinePage = currentTimeLineStateListIndex/numTimeLinePerPage+1;
		totalStnIdPage = stnStateList.size()/numTimeLinePerPage;
		if(stnStateList.size()%numTimeLinePerPage != 0)
			totalStnIdPage++;
		curStnIdPage= currentStnStateListIndex/numTimeLinePerPage + 1; //NEW CODE
	}

	/*public void setCharHeight(int charHeight) {
		this.charHeight = charHeight;
	}*/
	public void refreshPane(){
		for(int i =0; i< displayArray.length ; i++){
			displayArray[i].refresh();
		}
	}


	public String getPaneConfigurationName() {
		return paneConfigurationName;
	}

}

