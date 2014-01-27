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
 * 01/13/2014               Chin Chen   TTR829- when interpolation, edit graph is allowed
 * 08/20/2013   2259        bsteffen    Delete old skewt plugin.
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.display.rsc;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigStore;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpDataPageProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGraphProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpLineProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpOperationElement;
import gov.noaa.nws.ncep.ui.nsharp.NsharpSoundingElementStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
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

import java.text.DateFormatSymbols;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
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
import com.raytheon.viz.core.graphing.LineStroke;
import com.raytheon.viz.core.graphing.WGraphics;
import com.raytheon.viz.core.graphing.WindBarbFactory;
import com.sun.jna.ptr.FloatByReference;
import com.vividsolutions.jts.geom.Coordinate;

@SuppressWarnings("deprecation")
public class NsharpResourceHandler {
    private IRenderableDisplay[] displayArray = null;

    private NsharpPartListener.PartEvent editorPartStatus = NsharpPartListener.PartEvent.partClosed;

    private NsharpSkewTPaneResource skewtPaneRsc;

    private NsharpWitoPaneResource witoPaneRsc;

    private NsharpHodoPaneResource hodoPaneRsc;

    private NsharpTimeStnPaneResource timeStnPaneRsc;

    private NsharpInsetPaneResource insetPaneRsc;

    private NsharpDataPaneResource dataPaneRsc;

    private NsharpSpcGraphsPaneResource spcGraphsPaneRsc;

    private NsharpAbstractPaneResource futurePaneRsc;

    private String[] defaultDays;

    NsharpNative nsharpNative = null;

    private int displayDataPageMax;

    private static final int INSETPAGEMAX = 2;

    private int currentTextChapter = 1;

    private int currentInsetPage = 1;

    private int cnYOrig = NsharpConstants.COLOR_NOTATION_Y_ORIG;

    private int dtNextPageEnd = NsharpConstants.DATA_TIMELINE_NEXT_PAGE_END_;

    private int charHeight = NsharpConstants.CHAR_HEIGHT_;

    private int dtXOrig = NsharpConstants.DATA_TIMELINE_X_ORIG;

    private int dtYOrig = NsharpConstants.DATA_TIMELINE_Y_ORIG;

    private int dtWidth = NsharpConstants.DATA_TIMELINE_WIDTH;

    private String paneConfigurationName;

    private int numTimeLinePerPage = 1;

    /*
     * Hodograph Modes - definition is based on definitions in globals_xw.h of
     * BigNsharp
     */
    private static final int HODO_NORMAL = 0;

    // private static int HODO_EFFECTIVE= 1; not used in BigNsharp source code
    private static final int HODO_STORMRELATIVE = 2;

    @SuppressWarnings("unused")
    private static final int HODO_BNDRY = 3;

    private static final int HODO_MEANWIND = 4;

    @SuppressWarnings("unused")
    private int currentHodoWindMode = HODO_MEANWIND;

    private NsharpConfigManager configMgr;

    private NsharpConfigStore configStore;

    private NsharpGraphProperty graphConfigProperty;

    private HashMap<String, NsharpLineProperty> linePropertyMap;

    private NsharpDataPageProperty dataPageProperty;

    private int[] pageDisplayOrderNumberArray = new int[NsharpConstants.PAGE_MAX_NUMBER + 1]; // index
                                                                                              // is
                                                                                              // the
                                                                                              // real
                                                                                              // page
                                                                                              // defined
                                                                                              // in
                                                                                              // NsharpConstants
                                                                                              // to
                                                                                              // be
                                                                                              // shown,
                                                                                              // value
                                                                                              // is
                                                                                              // the
                                                                                              // order
                                                                                              // number
                                                                                              // of
                                                                                              // this
                                                                                              // page.
                                                                                              // index
                                                                                              // 0
                                                                                              // point
                                                                                              // to
                                                                                              // a
                                                                                              // dummy.

    private boolean overlayIsOn = false;

    private boolean interpolateIsOn = false;

    private boolean compareSndIsOn = false;

    private boolean compareStnIsOn = false;

    private boolean compareTmIsOn = false;

    private boolean editGraphOn = false;

    private boolean getTimeMatcher = false;

    public int TEMP_TYPE = 1;

    public int DEWPOINT_TYPE = 2;

    private int currentTempCurveType;

    private int currentSoundingLayerIndex = 0;

    private int hodoEditingSoundingLayerIndex = 0;

    private boolean plotInteractiveTemp = false;

    private Coordinate interactiveTempPointCoordinate;

    public static final float INVALID_DATA = NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;

    protected static final double BARB_LENGTH = 3.5;

    private String soundingType = null;

    protected DataTime displayedSounding;

    private int currentGraphMode = NsharpConstants.GRAPH_SKEWT;

    protected ListenerList listenerList = new ListenerList();

    // current active sounding layer list
    private List<NcSoundingLayer> soundingLys = null;

    private List<NcSoundingLayer> previousSoundingLys = null;

    private String pickedStnInfoStr; // current picked stn info with time line,
                                     // e.g. "ATLH 101209/03(Thu)V003"

    private NsharpStationInfo pickedStnInfo = null;

    private IFrameCoordinator.FrameChangeOperation currentOpDirection = IFrameCoordinator.FrameChangeOperation.NEXT; // next
                                                                                                                     // =forward

    private HashMap<Integer, RGB> stormSlinkyColorMap = new HashMap<Integer, RGB>();

    private List<List<List<NsharpSoundingElementStateProperty>>> stnTimeSndTable = new ArrayList<List<List<NsharpSoundingElementStateProperty>>>();

    // stnTimeSndTable:
    // Store all sounding profiles property for GUI display control
    // 1st index refer to stnId, 2nd index refer to time line and 3rd point to
    // sndType.
    // It is same as [][][] 3d array.
    // We dynamically expand this 3D array based on newly added
    // stnid/timeline/sndType When a new sounding data is loaded,
    // All unloaded element is null. Only when user load new sounding with this
    // stnId/this time line/this sndType, then
    // the element allocated.
    //
    // stn3-> T1--->T2--->T3->...
    // ^
    // /
    // stn2-> T1--->T2--->T3->...
    // ^
    // /
    // stn1-> T1--->T2--->T3->...
    // | | |
    // V V V
    // snd1 snd1 snd1
    // | | |
    // V V V
    // snd2 snd2 snd2
    // | | |
    // V V V
    // stnTimeSndTable first dimension (station id) should be in sync with
    // stnElementList,
    // 2nd dimension (time line) should be in sync with timeElementList, and
    // 3rd dimension (sounding type) should be in sync with sndTypeElementList
    // NULL element in stnTimeSndTable indicates that sounding data is not
    // loaded yet.

    private List<NsharpOperationElement> stnElementList = new ArrayList<NsharpOperationElement>();

    private List<NsharpOperationElement> timeElementList = new ArrayList<NsharpOperationElement>();

    private List<NsharpOperationElement> sndElementList = new ArrayList<NsharpOperationElement>();

    private NsharpSoundingElementStateProperty curSndProfileProp = null;

    private NsharpSoundingElementStateProperty preSndProfileProp = null;

    private int curTimeLinePage = 1;

    private int totalTimeLinePage = 1;

    private int curStnIdPage = 1;

    private int totalStnIdPage = 1;

    private int curSndPage = 1;

    private int totalSndPage = 1;

    private int previousTimeLineStateListIndex;

    private int currentStnElementListIndex = -1; // index to first dim of
                                                 // stnTimeSndTable and index to
                                                 // stnElementList

    private int currentTimeElementListIndex = -1;// index to 2nd dim of
                                                 // stnTimeSndTable and index to
                                                 // timeElementList

    private int currentSndElementListIndex = -1;// index to 3rd dim of
                                                // stnTimeSndTable and index to
                                                // sndElementList

    // use element state, NsharpConstants.LoadState or NsharpConstants.ActState,
    // as key to set color for drawing
    private HashMap<String, RGB> elementColorMap = new HashMap<String, RGB>();

    private short currentParcel = NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;

    private float currentParcelLayerPressure = NsharpNativeConstants.MU_LAYER;

    private float smWindDir, smWindSpd;

    public List<List<List<NsharpSoundingElementStateProperty>>> getStnTimeSndTable() {
        return stnTimeSndTable;
    }

    public List<String> getAllLoadedSndDesciptionList() {
        List<String> strLst = new ArrayList<String>();
        for (List<List<NsharpSoundingElementStateProperty>> tlListList : stnTimeSndTable) {
            // add a new element for the new sndType to each existing sndlist of
            // each existing time of each existing stnId
            for (List<NsharpSoundingElementStateProperty> sndtyList : tlListList) {
                for (NsharpSoundingElementStateProperty elem : sndtyList) {
                    if (elem != null)
                        strLst.add(elem.getElementDescription());
                }
            }
        }
        return strLst;
    }

    public List<NsharpOperationElement> getStnElementList() {
        return stnElementList;
    }

    public List<NsharpOperationElement> getTimeElementList() {
        return timeElementList;
    }

    public List<NsharpOperationElement> getSndElementList() {
        return sndElementList;
    }

    public int getCurrentStnElementListIndex() {
        return currentStnElementListIndex;
    }

    public int getCurrentTimeElementListIndex() {
        return currentTimeElementListIndex;
    }

    public int getTimeElementListSize() {
        return timeElementList.size();
    }

    public int getCurrentSndElementListIndex() {
        return currentSndElementListIndex;
    }

    // shape and color storage
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

    public boolean isCompareSndIsOn() {
        return compareSndIsOn;
    }

    public boolean isOverlayIsOn() {
        return overlayIsOn;
    }

    public NsharpNative getNsharpNative() {
        return nsharpNative;
    }

    public void setNextTextChapter() {
        if (currentTextChapter == displayDataPageMax) {
            currentTextChapter = 1;
        } else
            currentTextChapter++;
    }

    public void setNextInsetPage() {
        if (currentInsetPage == INSETPAGEMAX) {
            currentInsetPage = 1;
        } else
            currentInsetPage++;
    }

    public void setOverlayIsOn(boolean overlay) {
        previousSoundingLys = null;
        previousTimeLineStateListIndex = -1;
        preSndProfileProp = null;
        this.overlayIsOn = overlay;

        if (hodoPaneRsc != null)
            hodoPaneRsc.createRscHodoWindShapeAll();
        if (skewtPaneRsc != null)
            skewtPaneRsc.handleResize();
    }

    public boolean isInterpolateIsOn() {
        return interpolateIsOn;
    }

    /*
     * When compareStnIsOn is changed,
     */
    public void setCompareStnIsOn(boolean compareIsOn) {
        this.compareStnIsOn = compareIsOn;

        // This is the case when sounding data is not available at
        // currentTimeElementListIndex/currentSndElementListIndex/currentStnElementListIndex
        // and user set
        // compare stn on.
        if (compareStnIsOn) {
            if (soundingLys == null && currentTimeElementListIndex >= 0
                    && currentSndElementListIndex >= 0) {
                // find a new available stn for current time and sndType
                boolean found = false;
                for (int i = 0; i < stnElementList.size(); i++) {
                    if (stnElementList.get(i).getActionState() == NsharpConstants.ActState.ACTIVE
                            && stnTimeSndTable.get(i)
                                    .get(currentTimeElementListIndex)
                                    .get(currentSndElementListIndex) != null) {
                        found = true;
                        currentStnElementListIndex = i;
                    }

                    if (found)
                        break;
                }
                if (!found)
                    return;
            }

            int colorIndex = NsharpConstants.LINE_COMP1;
            for (NsharpOperationElement elm : stnElementList) {
                int stnIndex = stnElementList.indexOf(elm);
                NsharpSoundingElementStateProperty stnTmElm = stnTimeSndTable
                        .get(stnIndex).get(currentTimeElementListIndex)
                        .get(currentSndElementListIndex);
                if (stnTmElm != null) {
                    stnTmElm.setCompColorIndex(colorIndex);
                    // if(colorIndex > NsharpConstants.LINE_COMP10)
                    // colorIndex = NsharpConstants.LINE_COMP1;
                }
                colorIndex++;
                if (colorIndex > NsharpConstants.LINE_COMP10)
                    colorIndex = NsharpConstants.LINE_COMP1;
            }
        }
        setCurSndProfileProp();
        setCurrentSoundingLayerInfo();
        resetData();
    }

    public void setCompareSndIsOn(boolean compareSndIsOn) {
        this.compareSndIsOn = compareSndIsOn;
        // This is the case when sounding data is not available at
        // currentTimeElementListIndex/currentSndElementListIndex/currentStnElementListIndex
        // and user set
        // compSnd on
        if (compareSndIsOn) {
            if (soundingLys == null && currentStnElementListIndex >= 0
                    && currentTimeElementListIndex >= 0) {
                // find a new available snd type for current time and stn
                boolean found = false;
                for (int i = 0; i < sndElementList.size(); i++) {
                    if (sndElementList.get(i).getActionState() == NsharpConstants.ActState.ACTIVE
                            && stnTimeSndTable.get(currentStnElementListIndex)
                                    .get(currentTimeElementListIndex).get(i) != null) {
                        found = true;
                        currentSndElementListIndex = i;
                    }
                    if (found)
                        break;
                }
                if (!found)
                    return;
            }
            int colorIndex = NsharpConstants.LINE_COMP1;
            for (NsharpOperationElement elm : sndElementList) {
                int sndIndex = sndElementList.indexOf(elm);
                NsharpSoundingElementStateProperty stnTmElm = stnTimeSndTable
                        .get(currentStnElementListIndex)
                        .get(currentTimeElementListIndex).get(sndIndex);
                if (stnTmElm != null) {
                    stnTmElm.setCompColorIndex(colorIndex);
                    // if(colorIndex > NsharpConstants.LINE_COMP10)
                    // colorIndex = NsharpConstants.LINE_COMP1;
                }
                colorIndex++;
                if (colorIndex > NsharpConstants.LINE_COMP10)
                    colorIndex = NsharpConstants.LINE_COMP1;
            }
        }
        setCurSndProfileProp();
        setCurrentSoundingLayerInfo();
        resetData();

    }

    public void setCompareTmIsOn(boolean compareIsOn) {
        this.compareTmIsOn = compareIsOn;
        // This is the case when sounding data is not available at
        // currentTimeElementListIndex/currentSndElementListIndex/currentStnElementListIndex
        // and user set
        // compTm on
        if (compareIsOn) {
            if (soundingLys == null && currentStnElementListIndex >= 0
                    && currentSndElementListIndex >= 0) {
                // find a new available time line for current snd type and stn
                boolean found = false;
                for (int i = 0; i < timeElementList.size(); i++) {
                    if (timeElementList.get(i).getActionState() == NsharpConstants.ActState.ACTIVE
                            && stnTimeSndTable.get(currentStnElementListIndex)
                                    .get(i).get(currentSndElementListIndex) != null) {
                        found = true;
                        currentTimeElementListIndex = i;
                    }
                    if (found)
                        break;
                }
                if (!found)
                    return;
            }
            int colorIndex = NsharpConstants.LINE_COMP1;
            for (NsharpOperationElement elm : timeElementList) {
                int tmIndex = timeElementList.indexOf(elm);
                NsharpSoundingElementStateProperty stnTmElm = stnTimeSndTable
                        .get(currentStnElementListIndex).get(tmIndex)
                        .get(currentSndElementListIndex);
                if (stnTmElm != null) {
                    stnTmElm.setCompColorIndex(colorIndex);
                    // if(colorIndex > NsharpConstants.LINE_COMP10)
                    // colorIndex = NsharpConstants.LINE_COMP1;
                }
                colorIndex++;
                if (colorIndex > NsharpConstants.LINE_COMP10)
                    colorIndex = NsharpConstants.LINE_COMP1;
            }
        }
        setCurSndProfileProp();
        setCurrentSoundingLayerInfo();
        resetData();
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
        if (skewtPaneRsc != null)
            skewtPaneRsc.setCurrentGraphMode(currentGraphMode);
        refreshPane();
    }

    public short getCurrentParcel() {
        return currentParcel;
    }

    // native nsharp c/fortran lib
    // NsharpNative is native nsharp lib awips/lib/libnsharp.so wrapper class

    public float getSmWindDir() {
        return smWindDir;
    }

    public float getSmWindSpd() {
        return smWindSpd;
    }

    public void setCurrentHodoWindMode(int cursorPositionIndex) {
        switch (cursorPositionIndex) {
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

    public void setCurrentParcel(short currentParcel) {
        this.currentParcel = currentParcel;
        currentParcelLayerPressure = NsharpNativeConstants.parcelToLayerMap
                .get(currentParcel);
        // inform data/skewT panel as well
        if (dataPaneRsc != null) {
            dataPaneRsc.setCurrentParcel(currentParcel);
        }
        if (skewtPaneRsc != null) {
            if (currentParcel == NsharpNativeConstants.PARCELTYPE_USER_DEFINED)
                currentParcelLayerPressure = NsharpParcelDialog
                        .getUserDefdParcelMb();
            skewtPaneRsc.createRscParcelTraceShapes(currentParcel,
                    currentParcelLayerPressure);
            skewtPaneRsc.createRscParcelRtTraceShapesList(currentParcel,
                    currentParcelLayerPressure);
            skewtPaneRsc.createLCLEtcLinesShape();
        }
    }

    public void updateParcelFromPanel(short currentParcel) {
        this.currentParcel = currentParcel;
        currentParcelLayerPressure = NsharpNativeConstants.parcelToLayerMap
                .get(currentParcel);
        if (skewtPaneRsc != null) {
            skewtPaneRsc.createRscParcelTraceShapes(currentParcel,
                    currentParcelLayerPressure);
            skewtPaneRsc.createRscParcelRtTraceShapesList(currentParcel,
                    currentParcelLayerPressure);
            skewtPaneRsc.createLCLEtcLinesShape();
        }
    }

    public void setHodoStmCenter(Coordinate hodoHouseC) {
        if (hodoPaneRsc == null)
            return;
        // hodoPaneRsc.setHodoHouseC(hodoHouseC);
        Coordinate c = hodoPaneRsc.getHodoBackground().getWorld()
                .unMap(hodoHouseC.x, hodoHouseC.y);
        c = WxMath.speedDir((float) c.x, (float) c.y);
        smWindDir = (float) c.y;
        smWindSpd = (float) c.x;
        nsharpNative.nsharpLib.set_storm(smWindSpd, smWindDir);
        if (insetPaneRsc != null) {
            WGraphics WGc = insetPaneRsc.getPsblWatchTypeBackground()
                    .getWorld();
            insetPaneRsc.createBkgPsblWatchShape(WGc);

            // Sr wind vs Height graph shape need to recreate
            WGc = insetPaneRsc.getSrWindsBackground().getWorld();
            insetPaneRsc.createRscSrWindShape(WGc);
        }
        if (skewtPaneRsc != null) {
            skewtPaneRsc.createEffectiveLayerLinesShape();
            skewtPaneRsc.updatePsblWatchColor();
        }
    }

    // This function is called only when interpolation "on/off" is changed by
    // user
    public void resetInfoOnInterpolate(boolean interpolateIsOn)
            throws CloneNotSupportedException {
        // We dont want to assume previous interpolation on/off state. So, reset
        // soundingLys any how.
        this.interpolateIsOn = interpolateIsOn;
        NsharpSoundingElementStateProperty elem = getCurSoundingElementStateProperty();
        if (elem != null) {
            if (interpolateIsOn == false) {
                soundingLys = elem.getSndLyLst();

            } else {
                /*
                 * //TTR829 //interpolation is on //we dont want to change
                 * original raw data, so use a deep copy List<NcSoundingLayer>
                 * intpSndLst = new ArrayList<NcSoundingLayer>();
                 * for(NcSoundingLayer layer: elem.getSndLyLst() ) {
                 * intpSndLst.add((NcSoundingLayer) layer.clone()); }
                 * 
                 * // interpolation intpSndLst =
                 * performInterpolation(intpSndLst);
                 * 
                 * soundingLys = intpSndLst;
                 */
                // end TTR829
                soundingLys = performInterpolation(soundingLys);
            }

            nsharpNative.populateSndgData(soundingLys);
            if (skewtPaneRsc != null)
                skewtPaneRsc.resetData(soundingLys, previousSoundingLys);
            if (hodoPaneRsc != null)
                hodoPaneRsc.resetData(soundingLys, previousSoundingLys);
            if (witoPaneRsc != null)
                witoPaneRsc.resetData(soundingLys, previousSoundingLys);
            if (dataPaneRsc != null)
                dataPaneRsc.resetData(soundingLys, previousSoundingLys);
            if (insetPaneRsc != null)
                insetPaneRsc.resetData(soundingLys, previousSoundingLys);

            // re-create shape
            if (skewtPaneRsc != null)
                skewtPaneRsc.handleResize();
            if (hodoPaneRsc != null)
                hodoPaneRsc.createRscHodoWindShapeAll();
            if (insetPaneRsc != null)
                insetPaneRsc.createInsetWireFrameShapes();
            if (witoPaneRsc != null)
                witoPaneRsc.createRscWireFrameShapes();

        }

    }

    public void handleNsharpEditorPartEvent(NsharpPartListener.PartEvent pStatus) {
        switch (pStatus) {
        case partActivated:
            if (editorPartStatus != NsharpPartListener.PartEvent.partDeactivated) {
                // repopulateSndgData();
                // resetRsc();
                resetData();
            }

            break;
        default:
            break;
        }
        editorPartStatus = pStatus;
    }

    public void resetRsc() {
        restoreAllSoundingData();
        NsharpSoundingElementStateProperty elem = getCurSoundingElementStateProperty();
        if (elem != null) {
            this.soundingLys = elem.getSndLyLst();
            // Set default parcel trace data
            currentParcel = NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;
            currentParcelLayerPressure = NsharpNativeConstants.MU_LAYER;
            setSoundingInfo(this.soundingLys);
            currentTextChapter = 1;
            overlayIsOn = false;
            interpolateIsOn = false;
            compareStnIsOn = false;
            compareSndIsOn = false;
            compareTmIsOn = false;
            editGraphOn = false;
            if (skewtPaneRsc != null)
                skewtPaneRsc
                        .setCurrentSkewTEditMode(NsharpConstants.SKEWT_EDIT_MODE_EDITPOINT);
            resetData();
        }

    }

    public synchronized void resetData() {
        // System.out.println("resetData called, rscHdr="+this.toString() +
        // " pickedStnInfoStr="+pickedStnInfoStr+
        // " nsharpNative="+nsharpNative.toString());

        // update active sounding layer and picked stn info
        // re-populate snd data to nsharp native code lib for later calculating
        nsharpNative.populateSndgData(soundingLys);
        if (skewtPaneRsc != null)
            skewtPaneRsc.resetData(soundingLys, previousSoundingLys);
        if (hodoPaneRsc != null)
            hodoPaneRsc.resetData(soundingLys, previousSoundingLys);
        if (insetPaneRsc != null)
            insetPaneRsc.resetData(soundingLys, previousSoundingLys);
        if (dataPaneRsc != null)
            dataPaneRsc.resetData(soundingLys, previousSoundingLys);
        if (witoPaneRsc != null)
            witoPaneRsc.resetData(soundingLys, previousSoundingLys);

        NsharpShowTextDialog textarea = NsharpShowTextDialog.getAccess();
        if (textarea != null) {
            textarea.refreshTextData();
        }
        // if soundingLys is null, then we stop here, after reset data.
        if (soundingLys == null)
            return;
        if (soundingLys.size() > 0) {
            // set initial hodohouseC

            // ----- set hodo circle at Bunkers Right, Chin according to TTR6065
            // or RaytheonTicket#10438
            FloatByReference dummy1 = new FloatByReference(-999);
            FloatByReference dummy2 = new FloatByReference(-999);
            FloatByReference bwdir = new FloatByReference(-999);
            FloatByReference bwspd = new FloatByReference(-999);
            nsharpNative.nsharpLib.bunkers_storm_motion(dummy1, dummy2, bwdir,
                    bwspd);
            // System.out.println("resetData windspd="+ bwspd.getValue()+
            // " dir="+bwdir.getValue());
            smWindSpd = bwspd.getValue();
            smWindDir = bwdir.getValue();
            nsharpNative.nsharpLib.set_storm(smWindSpd, smWindDir);

            // reset parcel
            currentParcel = NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;
            currentParcelLayerPressure = NsharpNativeConstants.MU_LAYER;
            // reset parcel dialog as well
            if (NsharpParcelDialog.getAccess() != null) {
                NsharpParcelDialog.getAccess().resetUserDefParcel();
            }
            /*
             * Chin::: This api is called in many scenarios. User may want to
             * keep his previous picked parcel type. Therefore,
             * thdataPaneRsc.resetCurrentParcel should be called from other area
             * that really meant to reset parcel type.
             */
        }
        // Chin: TBD remove handle resize here to fix sizing issue when swapped
        // nsharp from side pane back to main pane
        // but, may cause other problem?
        // if(skewtPaneRsc!=null)
        // skewtPaneRsc.handleResize();
        if (skewtPaneRsc != null)
            skewtPaneRsc.createRscWireFrameShapes();
        if (hodoPaneRsc != null)
            hodoPaneRsc.createRscHodoWindShapeAll();
        if (insetPaneRsc != null)
            insetPaneRsc.createInsetWireFrameShapes();
        if (witoPaneRsc != null)
            witoPaneRsc.createAllWireFrameShapes();
    }

    private class NsharpOperationElementComparator implements
            Comparator<NsharpOperationElement> {
        @Override
        public int compare(NsharpOperationElement o1, NsharpOperationElement o2) {

            String s1tok1 = "";// , s1tok2="";
            String s2tok1 = "";// , s2tok2="";
            StringTokenizer st1 = new StringTokenizer(
                    o1.getElementDescription());
            int tkCount1 = st1.countTokens();
            // System.out.println("ElementComparatorTimeLine o1="+o1.elementDescription+"c1 = "+tkCount1);
            if (tkCount1 >= 1) {
                s1tok1 = st1.nextToken();
            } else {
                return 0;

            }
            // System.out.println("t1="+s1tok1+" t2="+s2tok1);
            StringTokenizer st2 = new StringTokenizer(
                    o2.getElementDescription());
            int tkCount2 = st2.countTokens();
            // System.out.println("ElementComparatorTimeLine o2="+o2.elementDescription+"c2 = "+tkCount2);
            if (tkCount2 >= 1) {
                s2tok1 = st2.nextToken();
            } else {
                return 0;

            }
            // System.out.println("t1="+s2tok1+" t2="+s2tok2);
            if (s1tok1.compareTo(s2tok1) == 0) {
                return 0;
            } else if (s1tok1.compareTo(s2tok1) < 0) {
                return 1;
            } else if (s1tok1.compareTo(s2tok1) > 0) {
                return -1;
            }
            return 0;
        }
    }

    private int getIndexFromElementList(String targetDescription,
            List<NsharpOperationElement> elemLst) {
        for (NsharpOperationElement sndProp : elemLst) {
            if (sndProp.getElementDescription().equals(targetDescription))
                return elemLst.indexOf(sndProp);

        }
        return -1;
    }

    private void restoreAllSoundingData() {
        for (List<List<NsharpSoundingElementStateProperty>> tlListList : stnTimeSndTable) {
            // add a new element for the new sndType to each existing sndlist of
            // each existing time of each existing stnId
            for (List<NsharpSoundingElementStateProperty> sndtyList : tlListList) {
                for (NsharpSoundingElementStateProperty elem : sndtyList) {
                    if (elem != null)
                        elem.restoreSndLyLstFromBackup();
                }
            }
        }
    }

    private int addElemToElemList(String elemDesc,
            List<NsharpOperationElement> elemList) {
        NsharpOperationElement elem = new NsharpOperationElement(elemDesc,
                NsharpConstants.ActState.ACTIVE);
        elemList.add(elem);
        Collections.sort(elemList, new NsharpOperationElementComparator());
        return elemList.indexOf(elem);
    }

    private void addNewSndToStnTimeSndTable(int sndIndex) {
        for (List<List<NsharpSoundingElementStateProperty>> tlListList : stnTimeSndTable) {
            // add a new element for the new sndType to each existing sndlist of
            // each existing time of each existing stnId
            for (List<NsharpSoundingElementStateProperty> sndtyList : tlListList) {
                sndtyList.add(sndIndex, null);
            }
        }
    }

    private void addNewStnToStnTimeSndTable(int stnIndex) {
        // Add new stnid to outer list of stnTimeSndTable
        List<List<NsharpSoundingElementStateProperty>> listListForNewStn = new ArrayList<List<NsharpSoundingElementStateProperty>>();
        // based on new stn id, add list for each existing time line
        for (int i = 0; i < timeElementList.size(); i++) {
            // based on each time line, add element for each existing sndType
            List<NsharpSoundingElementStateProperty> sndListForTm = new ArrayList<NsharpSoundingElementStateProperty>();
            for (int j = 0; j < sndElementList.size(); j++) {
                sndListForTm.add(null);
            }
            listListForNewStn.add(sndListForTm);
        }
        stnTimeSndTable.add(stnIndex, listListForNewStn);
    }

    private void addNewTimeToStnTimeSndTable(int timeIndex) {
        for (List<List<NsharpSoundingElementStateProperty>> tlListList : stnTimeSndTable) {
            // based on sndTypeElementList
            // create sndlist for the new time line for each existing stnid
            List<NsharpSoundingElementStateProperty> newSndList = new ArrayList<NsharpSoundingElementStateProperty>();
            for (int i = 0; i < sndElementList.size(); i++) {
                newSndList.add(null);
            }
            // add sndlist for the new time line to stn list
            tlListList.add(timeIndex, newSndList);
        }
    }

    private void addElementToTableAndLists(String stnId_timeLine_sndType,
            String stnId, String tmLine, String sndType,
            NsharpStationInfo stnInfo, List<NcSoundingLayer> sndLyLst) {
        // System.out.println("stn to be added "+ stnId + " timeline "+tmLine);
        NsharpSoundingElementStateProperty newSndPropElem = null;
        int tmIndex = getIndexFromElementList(tmLine, timeElementList);
        int stnIndex = getIndexFromElementList(stnId, stnElementList);
        int sndTpyeIndex = getIndexFromElementList(sndType, sndElementList);
        currentTimeElementListIndex = tmIndex;
        currentStnElementListIndex = stnIndex;
        currentSndElementListIndex = sndTpyeIndex;
        // based on these 3 indexes, we have 8 cases to handle.
        if (tmIndex >= 0 && stnIndex >= 0 && sndTpyeIndex >= 0) {
            // CASE1: All 3 index are good (>=0)
            if (stnTimeSndTable.get(stnIndex).get(tmIndex).get(sndTpyeIndex) != null)
                // this sounding element is already loaded
                return;
            else {
                // replace previously added "null" object with real
                // NsharpSoundingElementStateProperty object
                newSndPropElem = new NsharpSoundingElementStateProperty(
                        stnId_timeLine_sndType, stnId, tmLine, stnInfo,
                        sndLyLst);
                stnTimeSndTable.get(stnIndex).get(tmIndex)
                        .set(sndTpyeIndex, newSndPropElem);
            }
        } else if (tmIndex >= 0) {
            if (stnIndex >= 0) {
                // CASE2 : tmIndex/stnIndex are good (>=0), sndTpyeIndex is bad
                // (<0), a new snd type
                // add new sndType to sndTypeElementList
                currentSndElementListIndex = addElemToElemList(sndType,
                        sndElementList);
                // Add new snd type to each snd type list of stnTimeSndTable
                addNewSndToStnTimeSndTable(currentSndElementListIndex);
                // replace previously added "null" object with real
                // NsharpSoundingElementStateProperty object
                newSndPropElem = new NsharpSoundingElementStateProperty(
                        stnId_timeLine_sndType, stnId, tmLine, stnInfo,
                        sndLyLst);
                stnTimeSndTable.get(currentStnElementListIndex)
                        .get(currentTimeElementListIndex)
                        .set(currentSndElementListIndex, newSndPropElem);
            } else {
                if (sndTpyeIndex >= 0) {
                    // CASE3 : tmIndex/sndTpyeIndex are good, stnIndex is bad
                    // (<0), a new stnId
                    // add new stn to stnElementList
                    currentStnElementListIndex = addElemToElemList(stnId,
                            stnElementList);
                    // Add new stnid to outer list of stnTimeSndTable
                    addNewStnToStnTimeSndTable(currentStnElementListIndex);
                    // replace previously added "null" object with real
                    // NsharpSoundingElementStateProperty object
                    newSndPropElem = new NsharpSoundingElementStateProperty(
                            stnId_timeLine_sndType, stnId, tmLine, stnInfo,
                            sndLyLst);
                    stnTimeSndTable.get(currentStnElementListIndex)
                            .get(currentTimeElementListIndex)
                            .set(currentSndElementListIndex, newSndPropElem);

                } else {
                    // CASE4 : tmIndex is good, stnIndex/sndTpyeIndex are bad
                    // (<0), new stnId and new snd type
                    // add new stn to stnElementList
                    currentStnElementListIndex = addElemToElemList(stnId,
                            stnElementList);
                    // add new sndType to sndTypeElementList
                    currentSndElementListIndex = addElemToElemList(sndType,
                            sndElementList);
                    // Add new snd type to each snd type list of stnTimeSndTable
                    addNewSndToStnTimeSndTable(currentSndElementListIndex);
                    // Add new stnid to outer list of stnTimeSndTable
                    addNewStnToStnTimeSndTable(currentStnElementListIndex);
                    // replace previously added "null" object with real
                    // NsharpSoundingElementStateProperty object
                    newSndPropElem = new NsharpSoundingElementStateProperty(
                            stnId_timeLine_sndType, stnId, tmLine, stnInfo,
                            sndLyLst);
                    stnTimeSndTable.get(currentStnElementListIndex)
                            .get(currentTimeElementListIndex)
                            .set(currentSndElementListIndex, newSndPropElem);

                }
            }
        } else {
            if (stnIndex >= 0) {
                if (sndTpyeIndex >= 0) {
                    // CASE5 : stnIndex/sndTpyeIndex are good, tmIndex is bad
                    // (<0)
                    // add new time line to timeElementList
                    currentTimeElementListIndex = addElemToElemList(tmLine,
                            timeElementList);
                    // add new time line to StnTimeSndTable
                    addNewTimeToStnTimeSndTable(currentTimeElementListIndex);
                    // replace previously added "null" object with real
                    // NsharpSoundingElementStateProperty object
                    newSndPropElem = new NsharpSoundingElementStateProperty(
                            stnId_timeLine_sndType, stnId, tmLine, stnInfo,
                            sndLyLst);
                    stnTimeSndTable.get(currentStnElementListIndex)
                            .get(currentTimeElementListIndex)
                            .set(currentSndElementListIndex, newSndPropElem);

                } else {
                    // CASE6 : stnIndex is good, tmIndex/sndTpyeIndex are bad
                    // (<0)
                    // add new time line to timeElementList
                    currentTimeElementListIndex = addElemToElemList(tmLine,
                            timeElementList);
                    // add new sndType to sndTypeElementList
                    currentSndElementListIndex = addElemToElemList(sndType,
                            sndElementList);
                    // Add new snd type to each snd type list of stnTimeSndTable
                    addNewSndToStnTimeSndTable(currentSndElementListIndex);
                    // add new time line to StnTimeSndTable
                    addNewTimeToStnTimeSndTable(currentTimeElementListIndex);
                    // replace previously added "null" object with real
                    // NsharpSoundingElementStateProperty object
                    newSndPropElem = new NsharpSoundingElementStateProperty(
                            stnId_timeLine_sndType, stnId, tmLine, stnInfo,
                            sndLyLst);
                    stnTimeSndTable.get(currentStnElementListIndex)
                            .get(currentTimeElementListIndex)
                            .set(currentSndElementListIndex, newSndPropElem);

                }

            } else {
                if (sndTpyeIndex >= 0) {
                    // CASE7 : sndTpyeIndex is good, tmIndex/stnIndex are bad
                    // (<0)
                    // add new time line to timeElementList
                    currentTimeElementListIndex = addElemToElemList(tmLine,
                            timeElementList);
                    // add new stn to stnElementList
                    currentStnElementListIndex = addElemToElemList(stnId,
                            stnElementList);
                    // add new time line to StnTimeSndTable
                    addNewTimeToStnTimeSndTable(currentTimeElementListIndex);
                    // Add new stnid to outer list of stnTimeSndTable
                    addNewStnToStnTimeSndTable(currentStnElementListIndex);
                    // replace previously added "null" object with real
                    // NsharpSoundingElementStateProperty object
                    newSndPropElem = new NsharpSoundingElementStateProperty(
                            stnId_timeLine_sndType, stnId, tmLine, stnInfo,
                            sndLyLst);
                    stnTimeSndTable.get(currentStnElementListIndex)
                            .get(currentTimeElementListIndex)
                            .set(currentSndElementListIndex, newSndPropElem);

                } else {
                    // CASE8 : All are bad (<0)
                    // an element with new time line, new stnId and new sndType
                    // add new time line to timeElementList
                    currentTimeElementListIndex = addElemToElemList(tmLine,
                            timeElementList);
                    // add new stn to stnElementList
                    currentStnElementListIndex = addElemToElemList(stnId,
                            stnElementList);
                    // add new sndType to sndTypeElementList
                    currentSndElementListIndex = addElemToElemList(sndType,
                            sndElementList);

                    // Construct stnTimeSndTable
                    if (stnTimeSndTable.size() > 0) {
                        List<List<NsharpSoundingElementStateProperty>> listListForNewStn = new ArrayList<List<NsharpSoundingElementStateProperty>>();
                        // based on new stn id, add list for each existing time
                        // line
                        for (NsharpOperationElement tmElem : timeElementList) {
                            // based on each time line, add element for each
                            // existing sndType
                            List<NsharpSoundingElementStateProperty> sndlistForTm = new ArrayList<NsharpSoundingElementStateProperty>();
                            for (NsharpOperationElement sndElem : sndElementList) {
                                if (tmLine.equals(tmElem
                                        .getElementDescription())
                                        && sndType.equals(sndElem
                                                .getElementDescription())) {
                                    // only one case falls in this route as only
                                    // one new loaded sounding data
                                    newSndPropElem = new NsharpSoundingElementStateProperty(
                                            stnId_timeLine_sndType, stnId,
                                            tmLine, stnInfo, sndLyLst);

                                    sndlistForTm.add(newSndPropElem);
                                } else {
                                    // create for not avail sounding profiles
                                    sndlistForTm.add(null);
                                }
                            }
                            listListForNewStn.add(sndlistForTm);
                        }

                        // Now update stnTimeSndTable by adding "dummy"
                        // NsharpSoundingElementStateProperty to all exiting stn
                        // listList and time list
                        // Note that we have NOT added "listListForNewStn" to
                        // stnTimeSndTable yet.
                        // we have to update current table now
                        for (List<List<NsharpSoundingElementStateProperty>> tlListList : stnTimeSndTable) {
                            // add a new element for the new sndType to each
                            // existing sndlist of each existing time of each
                            // existing stnId
                            for (List<NsharpSoundingElementStateProperty> sndtyList : tlListList) {
                                sndtyList.add(currentSndElementListIndex, null);
                            }
                            // based on sndTypeElementList
                            // add a new sndlist for the new time line for each
                            // existing stnid
                            List<NsharpSoundingElementStateProperty> newSndList = new ArrayList<NsharpSoundingElementStateProperty>();
                            for (int i = 0; i < sndElementList.size(); i++) {
                                {
                                    newSndList.add(null);
                                }
                            }
                            tlListList.add(currentTimeElementListIndex,
                                    newSndList);
                        }
                        // finally, add this new stn list to table
                        stnTimeSndTable.add(currentStnElementListIndex,
                                listListForNewStn);
                    } else {
                        // this is the case, we are adding first element to
                        // stnTimeSndTable
                        // need a new stn time line list to stnTimeSndTable
                        List<NsharpSoundingElementStateProperty> newList = new ArrayList<NsharpSoundingElementStateProperty>();
                        List<List<NsharpSoundingElementStateProperty>> newListList = new ArrayList<List<NsharpSoundingElementStateProperty>>();

                        newSndPropElem = new NsharpSoundingElementStateProperty(
                                stnId_timeLine_sndType, stnId, tmLine, stnInfo,
                                sndLyLst);
                        newList.add(newSndPropElem);
                        newListList.add(newList);
                        stnTimeSndTable.add(newListList);
                        curSndProfileProp = newSndPropElem;
                        return;
                    }

                }
            }
        }
        setCurSndProfileProp();

    }

    private void setCurSndProfileProp() {
        if (currentTimeElementListIndex < 0
                || currentTimeElementListIndex >= timeElementList.size()
                || currentStnElementListIndex < 0
                || currentStnElementListIndex >= stnElementList.size()
                || currentSndElementListIndex < 0
                || currentSndElementListIndex >= sndElementList.size()) {
            curSndProfileProp = null;
            preSndProfileProp = null;
        } else {
            preSndProfileProp = curSndProfileProp;
            curSndProfileProp = stnTimeSndTable.get(currentStnElementListIndex)
                    .get(currentTimeElementListIndex)
                    .get(currentSndElementListIndex);
        }
    }

    private void cleanUpStnTimeSndTable(int stni, int tmi, int sndi) {
        boolean found = false;
        // find if this station is no longer in use
        List<List<NsharpSoundingElementStateProperty>> tlListList = stnTimeSndTable
                .get(stni);
        for (List<NsharpSoundingElementStateProperty> sndtyList : tlListList) {
            for (NsharpSoundingElementStateProperty elem : sndtyList) {
                if (elem != null) {
                    found = true;
                    break;
                }
            }
            if (found)
                break;
        }
        if (!found) {
            // This stn is no longer in use...delete it from stnTimeSndTable and
            // stnElementList
            stnElementList.remove(stni);
            tlListList = stnTimeSndTable.remove(stni);
            tlListList.clear();
        }
        // find if this time line is no longer in use
        found = false;
        for (List<List<NsharpSoundingElementStateProperty>> tmListListForStn : stnTimeSndTable) {
            List<NsharpSoundingElementStateProperty> sndtyListForTm = tmListListForStn
                    .get(tmi);
            for (NsharpSoundingElementStateProperty elem : sndtyListForTm) {
                if (elem != null) {
                    found = true;
                    break;
                }
            }
            if (found)
                break;
        }
        if (!found) {
            // This time line is no longer in use...delete it from
            // stnTimeSndTable and timeElementList
            timeElementList.remove(tmi);
            for (List<List<NsharpSoundingElementStateProperty>> tmListListForStn : stnTimeSndTable) {
                List<NsharpSoundingElementStateProperty> sndtyListForTm = tmListListForStn
                        .remove(tmi);
                sndtyListForTm.clear();
            }
        }
        // find if this sounding type is no longer in use
        found = false;
        for (List<List<NsharpSoundingElementStateProperty>> tmListListForStn : stnTimeSndTable) {
            for (List<NsharpSoundingElementStateProperty> sndtyListForTm : tmListListForStn) {
                NsharpSoundingElementStateProperty elem = sndtyListForTm
                        .get(sndi);
                if (elem != null) {
                    found = true;
                    break;
                }
            }
            if (found)
                break;
        }
        if (!found) {
            // This sounding type is no longer in use...delete it from
            // stnTimeSndTable and sndElementList
            sndElementList.remove(sndi);
            for (List<List<NsharpSoundingElementStateProperty>> tmListListForStn : stnTimeSndTable) {
                for (List<NsharpSoundingElementStateProperty> sndtyListForTm : tmListListForStn) {
                    sndtyListForTm.remove(sndi);
                }
            }
        }

    }

    public boolean deleteRsc(List<String> deletingDataTimeList) {
        boolean curSndDeleted = false;
        for (String dataTmLine : deletingDataTimeList) {
            if (curSndProfileProp != null
                    && curSndProfileProp.getElementDescription().equals(
                            dataTmLine)) {
                curSndDeleted = true;
            }
            // find deleting element from stnTimeSndTable and null it
            boolean setdone = false;
            for (List<List<NsharpSoundingElementStateProperty>> tlListList : stnTimeSndTable) {
                for (List<NsharpSoundingElementStateProperty> sndtyList : tlListList) {
                    for (NsharpSoundingElementStateProperty elem : sndtyList) {
                        if (elem != null
                                && dataTmLine.equals(elem
                                        .getElementDescription())) {
                            int sndi = sndtyList.indexOf(elem);
                            int tmi = tlListList.indexOf(sndtyList);
                            int stni = stnTimeSndTable.indexOf(tlListList);
                            sndtyList.set(sndtyList.indexOf(elem), null);
                            setdone = true;
                            // clean up stnTimeSndTable if a stn/timeline/snd is
                            // no longer in use
                            cleanUpStnTimeSndTable(stni, tmi, sndi);
                            break;
                        }
                    }
                    if (setdone)
                        break;
                }
                if (setdone)
                    break;
            }
        }

        if (curSndDeleted || soundingLys == null) {
            // this is the case that we are deleting current snd, so, a new
            // current snd should be selected
            curSndProfileProp = null;
            // find CurrentElementIndexes After Delete current snding
            boolean found = false;
            int stni = 0;
            for (List<List<NsharpSoundingElementStateProperty>> tlListList : stnTimeSndTable) {
                int timei = 0;
                for (List<NsharpSoundingElementStateProperty> sndtyList : tlListList) {
                    int sndi = 0;
                    for (NsharpSoundingElementStateProperty elem : sndtyList) {
                        if (elem != null
                                && stnElementList.get(stni).getActionState() == NsharpConstants.ActState.ACTIVE
                                && timeElementList.get(timei).getActionState() == NsharpConstants.ActState.ACTIVE
                                && sndElementList.get(sndi).getActionState() == NsharpConstants.ActState.ACTIVE) {
                            currentStnElementListIndex = stni;
                            currentSndElementListIndex = sndi;
                            currentTimeElementListIndex = timei;
                            found = true;
                            break;
                        }
                        sndi++;
                    }
                    if (found)
                        break;
                    timei++;
                }
                if (found)
                    break;
                stni++;
            }
            if (!found) {
                currentStnElementListIndex = -1;
                currentSndElementListIndex = -1;
                currentTimeElementListIndex = -1;
            }
            setCurSndProfileProp();
        } else {
            // currentStnElementListIndex, currentSndElementListIndex and
            // currentTimeElementListIndex may not point to right element
            // after some elements are deleted.
            currentStnElementListIndex = -1;
            currentTimeElementListIndex = -1;
            currentSndElementListIndex = -1;
            if (curSndProfileProp != null) {
                boolean found = false;
                for (List<List<NsharpSoundingElementStateProperty>> tmListListForStn : stnTimeSndTable) {
                    for (List<NsharpSoundingElementStateProperty> sndtyListForTm : tmListListForStn) {
                        for (NsharpSoundingElementStateProperty elem : sndtyListForTm) {
                            if (elem != null
                                    && curSndProfileProp
                                            .getElementDescription()
                                            .equals(elem
                                                    .getElementDescription())) {
                                currentSndElementListIndex = sndtyListForTm
                                        .indexOf(elem);
                                currentTimeElementListIndex = tmListListForStn
                                        .indexOf(sndtyListForTm);
                                currentStnElementListIndex = stnTimeSndTable
                                        .indexOf(tmListListForStn);
                                found = true;
                                break;
                            }
                        }
                        if (found)
                            break;
                    }
                    if (found)
                        break;
                }
            }
        }
        setCurrentSoundingLayerInfo();
        resetData();
        System.out.println("num=" + getFrameCount());
        return curSndDeleted;
        // anything more to do?
    }

    public void deleteRscAll() {
        NsharpMapResource nsharpMapResource = NsharpMapResource
                .getOrCreateNsharpMapResource();
        nsharpMapResource.setPoints(null);
        if (soundingLys != null) {
            soundingLys.clear();
            soundingLys = null;
        }
        if (previousSoundingLys != null) {
            previousSoundingLys.clear();
            previousSoundingLys = null;
        }
        if (stnTimeSndTable != null) {
            for (List<List<NsharpSoundingElementStateProperty>> stnListList : stnTimeSndTable) {
                for (List<NsharpSoundingElementStateProperty> timeList : stnListList) {
                    timeList.clear();
                }
                stnListList.clear();
            }
            stnTimeSndTable.clear();
        }
        if (timeElementList != null)
            timeElementList.clear();
        if (stnElementList != null)
            stnElementList.clear();
        if (sndElementList != null)
            sndElementList.clear();
        curSndProfileProp = null;
        preSndProfileProp = null;
        currentTextChapter = 1;
        currentInsetPage = 1;
        currentParcel = NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;
        currentParcelLayerPressure = NsharpNativeConstants.MU_LAYER;
        currentTimeElementListIndex = -1;
        currentStnElementListIndex = -1;
        currentSndElementListIndex = -1;
        resetData();
    }

    private NsharpSoundingElementStateProperty getCurSoundingElementStateProperty() {
        if (currentTimeElementListIndex >= 0
                && currentStnElementListIndex >= 0
                && currentSndElementListIndex >= 0
                && stnTimeSndTable.get(currentStnElementListIndex)
                        .get(currentTimeElementListIndex)
                        .get(currentSndElementListIndex) != null) {
            return stnTimeSndTable.get(currentStnElementListIndex)
                    .get(currentTimeElementListIndex)
                    .get(currentSndElementListIndex);
        }
        return null;
    }

    private void setCurrentSoundingLayerInfo() {
        NsharpSoundingElementStateProperty elem = getCurSoundingElementStateProperty();
        if (elem != null) {
            pickedStnInfoStr = elem.getElementDescription();
            pickedStnInfo = elem.getStnInfo();

            if (overlayIsOn) {
                previousSoundingLys = soundingLys;
            } else {
                previousSoundingLys = null;
            }

            if (interpolateIsOn == true) {
                /*
                 * TTR829 deep copy is not necessary as performInterpolation()
                 * will re-allocate new layer for each layer //we dont want to
                 * change original raw data, so use a new copy NcSoundingLayer
                 * newLayer; List<NcSoundingLayer> mySndLst = new
                 * ArrayList<NcSoundingLayer>(); for(NcSoundingLayer
                 * layer:elem.getSndLyLst()){ newLayer = new NcSoundingLayer();
                 * try { newLayer = (NcSoundingLayer) layer.clone();
                 * 
                 * } catch (CloneNotSupportedException e) { e.printStackTrace();
                 * } //here a shallowCopy is enough mySndLst.add(newLayer); } //
                 * interpolation mySndLst = performInterpolation(mySndLst);
                 * 
                 * soundingLys = mySndLst;
                 */

                soundingLys = performInterpolation(elem.getSndLyLst());
            } else {
                soundingLys = elem.getSndLyLst();
            }
        } else {
            previousSoundingLys = null;
            soundingLys = null;
        }

    }

    public void addRsc(Map<String, List<NcSoundingLayer>> soundMap,
            NsharpStationInfo stnInfo, boolean fromNCP) {
        if (fromNCP) {
            // this is from NCP do nothing now
            this.addRsc(true, soundMap, stnInfo);
            // NCP case:
            // Key String format will be like this for NCUAIR
            // KGRI 100616/03(Wed)-NCUAIR
            // and for PFC/Grid sounding will be like this
            // KGRI 100616/03(Wed)V001-GFSSND
        } else {
            // D2D case::::
            // this is from D2D, edit display and time line string to add short
            // day-of-week and
            // also add sounding type to string to solve an issue that data with
            // same stn, same time line but different
            // sounding type will not be loaded.
            // D2D's key string is like this: "KGRI 2010-06-16 03:00:00"
            // will change it to "KGRI 100616/03(Wed)-GFSSND"
            Set<String> dataTimelineSet = soundMap.keySet();
            SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            // DateFormatSymbols dfs= new DateFormatSymbols();
            // String[] defaultDays = dfs.getShortWeekdays();
            Calendar cal = Calendar.getInstance();
            Date date;
            Map<String, List<NcSoundingLayer>> newmap = new HashMap<String, List<NcSoundingLayer>>();
            String sndType = stnInfo.getSndType();
            for (String timeline : dataTimelineSet) {
                System.out.println("D2D sending timeline Str:" + timeline);
                String dateStr = timeline.substring(timeline.indexOf(' ') + 1);
                String stnId = timeline.substring(0, timeline.indexOf(' '));
                try {
                    date = df.parse(dateStr);
                    cal.setTime(date);
                    String dayOfWeek = defaultDays[cal
                            .get(Calendar.DAY_OF_WEEK)];
                    // dateStr = timeline.substring(0,
                    // timeline.indexOf(':'))+dayOfWeek+stnInfo.getSndType();
                    // System.out.println("New Str:"+ dateStr);
                    String finalTimeStr = String.format(
                            "%4$s %1$ty%1$tm%1$td/%1$tH(%2$s) %3$s", cal,
                            dayOfWeek, sndType, stnId);
                    System.out.println("D2D (modified) timeline Str:"
                            + finalTimeStr);
                    // put newTimeStr to new map with original value
                    newmap.put(finalTimeStr, soundMap.get(timeline));
                } catch (ParseException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                    continue;
                }
            }
            // this is from D2D, and it does not want to display new data right
            // away.
            this.addRsc(false, newmap, stnInfo);
        }
    }

    /*
     * public void loadSoundingData( boolean displayNewData,
     * List<NcSoundingLayer> soundDataLst, String stnId, String sndType,
     * Timestamp rangeStartTime,double latitude,double longitude){ //* testing
     * code //stnInfo.setStnId("KFSD"); { Set<String> keyset= new
     * HashSet<String>(soundMap.keySet()); for(String key: keyset) {
     * List<NcSoundingLayer> sndLy = soundMap.remove(key); String newkey=
     * key.replace("NAMS", "yahoo"); //String newkey= key.replace("GFSS",
     * "NAMS"); //newkey = newkey.replace("KSLN", "KFSD"); soundMap.put(newkey,
     * sndLy); } stnInfo.setSndType(stnInfo.getSndType().replace("NAMS",
     * "yahoo")); //stnInfo.setSndType(stnInfo.getSndType().replace("GFSS",
     * "NAMS")); }//
     * 
     * //need to format reftime to GMT time string. Timestamp.toString produce a
     * local time Not GMT time Calendar cal =
     * Calendar.getInstance(TimeZone.getTimeZone("GMT"));
     * cal.setTimeInMillis(rangeStartTime.getTime()); String dayOfWeek =
     * defaultDays[cal.get(Calendar.DAY_OF_WEEK)]; String timeLine =
     * String.format("%1$ty%1$tm%1$td/%1$tH(%2$s)", cal, dayOfWeek);
     * if(stnId.indexOf(" ")>=0){ //take care stnId with SPACE case. stnId =
     * stnId.replace(" ", "_"); } if(timeElementList.isEmpty() ||
     * stnElementList.isEmpty() || currentSndElementListIndex < 0
     * ||sndElementList.isEmpty() || currentTimeElementListIndex < 0 ||
     * currentStnElementListIndex < 0){ //if no data was loaded since, then
     * display this data any way displayNewData = true; } //save current
     * timeline and stn state properties if we are NOT loading new data
     * NsharpOperationElement currentTL=null; NsharpOperationElement
     * currentStn=null; NsharpOperationElement currentSnd=null;
     * NsharpSoundingElementStateProperty currentPreSndProfileProp=null;
     * if(!displayNewData){ currentTL =
     * timeElementList.get(currentTimeElementListIndex); currentStn =
     * stnElementList.get(currentStnElementListIndex); currentSnd =
     * sndElementList.get(currentSndElementListIndex); currentPreSndProfileProp
     * = preSndProfileProp; } //add new data to table //add time line to
     * stnTimeTable and set its index
     * addElementToTableAndLists((stnId+" "+timeLine
     * +" "+sndType),stnId,timeLine,sndType,stnInfo, soundDataLst);
     * 
     * if(displayNewData){ //Set default parcel trace data currentParcel =
     * NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;
     * currentParcelLayerPressure = NsharpNativeConstants.MU_LAYER;
     * setCurrentSoundingLayerInfo(); resetData(); } else { //Not display new
     * data. Reset current "parameter"s after adding data to map/lists
     * currentStnElementListIndex= stnElementList.indexOf(currentStn);
     * currentTimeElementListIndex = timeElementList.indexOf(currentTL);
     * currentSndElementListIndex = sndElementList.indexOf(currentSnd);
     * preSndProfileProp = currentPreSndProfileProp; curSndProfileProp =
     * stnTimeSndTable
     * .get(currentStnElementListIndex).get(currentTimeElementListIndex
     * ).get(currentSndElementListIndex); }
     * 
     * //set total time line group and stn id list page number
     * calculateTimeStnBoxData();
     * 
     * //set data time to descriptor //this is necessary for looping // starting
     * 13.2.1, this line is changed by Raytheon if ((
     * skewtPaneRsc.getDescriptor().getFramesInfo().getFrameCount() == 0)&&
     * !getTimeMatcher) { //was this line before 13.2.1 if ((
     * skewtPaneRsc.getDescriptor().getTimeMatcher() == null ||
     * skewtPaneRsc.getDescriptor().getTimeMatcher().getTimeMatchBasis() ==
     * null)&& !getTimeMatcher) { //DataTime[] dataTimes = new
     * DataTime[dataTimelineList.size()]; //Chin Note: we just have to do this
     * once and set dataTimes size bigger than 1. //Nsharp handles changing
     * frame itself. It just need system to send change frame notice. //That is
     * happened at NsharpSkewTDescriptor.checkDrawTime(). DataTime[] dataTimes =
     * new DataTime[2]; Date now = new Date(); for(int k =0; k < 2 ; k++){
     * dataTimes[k]= new DataTime(now, k); } //no need to get a descriptor from
     * a renderableDispaly since we have a descriptor
     * skewtPaneRsc.getDescriptor().setDataTimes(dataTimes);
     * getTimeMatcher=true; }
     * 
     * NsharpShowTextDialog textarea = NsharpShowTextDialog.getAccess();
     * if(textarea != null){ textarea.refreshTextData(); } NsharpPaletteWindow
     * win = NsharpPaletteWindow.getInstance() ; if(win!=null)
     * currentGraphMode=win.getCurrentGraphMode();
     * 
     * refreshPane();
     * 
     * }
     */

    private void addRsc(boolean displayNewData,
            Map<String, List<NcSoundingLayer>> soundMap,
            NsharpStationInfo stnInfo) {
        /*
         * testing code //stnInfo.setStnId("KFSD"); { Set<String> keyset= new
         * HashSet<String>(soundMap.keySet()); for(String key: keyset) {
         * List<NcSoundingLayer> sndLy = soundMap.remove(key); String newkey=
         * key.replace("NCUAIR", "gpduair"); //String newkey=
         * key.replace("GFSS", "NAMS"); //String newkey = key.replace("OAK",
         * "KFSD"); soundMap.put(newkey, sndLy); }
         * stnInfo.setSndType(stnInfo.getSndType().replace("NCUAIR",
         * "gpduair"));
         * //stnInfo.setSndType(stnInfo.getSndType().replace("GFSS", "NAMS"));
         * }//
         */

        if (stnInfo.getStnId() != null && stnInfo.getStnId().indexOf(" ") >= 0) {
            // take care stnId with SPACE case.
            String stnId = stnInfo.getStnId();
            String newStnId = stnId.replace(" ", "_");
            stnInfo.setStnId(newStnId);
            String dspInfo = stnInfo.getStnDisplayInfo();
            stnInfo.setStnDisplayInfo(dspInfo.replace(stnId, newStnId));
            Set<String> keyset = new HashSet<String>(soundMap.keySet());
            for (String key : keyset) {
                List<NcSoundingLayer> sndLy = soundMap.remove(key);
                String newkey = key.replace(stnId, newStnId);
                soundMap.put(newkey, sndLy);
            }

        }

        if (soundMap.size() <= 0 || (skewtPaneRsc == null)) {
            return;
        }
        if (timeElementList.isEmpty() || stnElementList.isEmpty()
                || currentSndElementListIndex < 0 || sndElementList.isEmpty()
                || currentTimeElementListIndex < 0
                || currentStnElementListIndex < 0) {
            // if no data was loaded since, then display this data any way
            displayNewData = true;
        }
        // save current timeline and stn state properties if we are NOT loading
        // new data
        NsharpOperationElement currentTL = null;
        NsharpOperationElement currentStn = null;
        NsharpOperationElement currentSnd = null;
        NsharpSoundingElementStateProperty currentPreSndProfileProp = null;
        if (!displayNewData) {
            currentTL = timeElementList.get(currentTimeElementListIndex);
            currentStn = stnElementList.get(currentStnElementListIndex);
            currentSnd = sndElementList.get(currentSndElementListIndex);
            currentPreSndProfileProp = preSndProfileProp;
        }
        // add new data to table
        Set<String> dataTimelineSet = soundMap.keySet();
        String[] tempTimeLineArr = dataTimelineSet
                .toArray(new String[dataTimelineSet.size()]);
        Arrays.sort(tempTimeLineArr);
        for (int i = 0; i < tempTimeLineArr.length; i++) {
            // based on this KEY string format "KGRI 100616/03(Wed) GFSSND"
            String stnId_timeLine_sndType = tempTimeLineArr[i].toString();
            String stnId = stnId_timeLine_sndType.substring(0,
                    stnId_timeLine_sndType.indexOf(" "));
            String timeLine_sndType = stnId_timeLine_sndType
                    .substring(stnId_timeLine_sndType.indexOf(" ") + 1);
            String timeLine = timeLine_sndType.substring(0,
                    timeLine_sndType.indexOf(" "));
            String sndType = timeLine_sndType.substring(timeLine_sndType
                    .indexOf(" ") + 1);
            // No more needed? timeLine = timeLine.replace(" ", "-"); //fixed
            // DR15325 - sorting time line issue in D2D
            // add time line to stnTimeTable and set its index
            addElementToTableAndLists(stnId_timeLine_sndType, stnId, timeLine,
                    sndType, stnInfo, soundMap.get(stnId_timeLine_sndType));
        }
        if (displayNewData) {
            // Set default parcel trace data
            currentParcel = NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;
            currentParcelLayerPressure = NsharpNativeConstants.MU_LAYER;
            setCurrentSoundingLayerInfo();
            resetData();
        } else {
            // Not display new data. Reset current "parameter"s after adding
            // data to map/lists
            currentStnElementListIndex = stnElementList.indexOf(currentStn);
            currentTimeElementListIndex = timeElementList.indexOf(currentTL);
            currentSndElementListIndex = sndElementList.indexOf(currentSnd);
            preSndProfileProp = currentPreSndProfileProp;
            curSndProfileProp = stnTimeSndTable.get(currentStnElementListIndex)
                    .get(currentTimeElementListIndex)
                    .get(currentSndElementListIndex);
        }

        // set total time line group and stn id list page number
        calculateTimeStnBoxData();

        // set data time to descriptor
        // this is necessary for looping
        // starting 13.2.1, this line is changed by Raytheon
        if ((skewtPaneRsc.getDescriptor().getFramesInfo().getFrameCount() == 0)
                && !getTimeMatcher) {
            // was this line before 13.2.1 if ((
            // skewtPaneRsc.getDescriptor().getTimeMatcher() == null ||
            // skewtPaneRsc.getDescriptor().getTimeMatcher().getTimeMatchBasis()
            // == null)&& !getTimeMatcher) {
            // DataTime[] dataTimes = new DataTime[dataTimelineList.size()];
            // Chin Note: we just have to do this once and set dataTimes size
            // bigger than 1.
            // Nsharp handles changing frame itself. It just need system to send
            // change frame notice.
            // That is happened at NsharpSkewTDescriptor.checkDrawTime().
            DataTime[] dataTimes = new DataTime[2/* stnTimeTable.size() */];
            Date now = new Date();
            for (int k = 0; k < 2/* stnTimeTable.size() */; k++) {
                dataTimes[k] = new DataTime(now, k);
            }
            // no need to get a descriptor from a renderableDispaly since we
            // have a descriptor
            skewtPaneRsc.getDescriptor().setDataTimes(dataTimes);
            getTimeMatcher = true;
        }

        NsharpShowTextDialog textarea = NsharpShowTextDialog.getAccess();
        if (textarea != null) {
            textarea.refreshTextData();
        }
        NsharpPaletteWindow win = NsharpPaletteWindow.getInstance();
        if (win != null)
            currentGraphMode = win.getCurrentGraphMode();

        refreshPane();

    }

    public void addRsc(Map<String, List<NcSoundingLayer>> soundMap,
            NsharpStationInfo stnInfo) {
        // by default, display new data
        // NCP always call from this route.
        this.addRsc(true, soundMap, stnInfo);
        return;
    }

    public String getPickedStnInfoStr() {
        return pickedStnInfoStr;
    }

    public void handleUserClickOnStationId(Coordinate c) {

        // first to find if it is for change to next page, or change sorting
        // System.out.println("numTimeLinePerPage="+numTimeLinePerPage+"gap="+(cnYOrig-dtNextPageEnd));
        int index = ((int) (c.y - dtYOrig)) / charHeight;

        if (index == 0) {
            // change to next/previous page
            if (totalStnIdPage == 1)
                return;
            if ((c.x - (dtXOrig + dtWidth)) < (dtWidth / 2)) {
                curStnIdPage++;
                if (curStnIdPage > totalStnIdPage)
                    curStnIdPage = 1;
            } else {
                curStnIdPage--;
                if (curStnIdPage <= 0)
                    curStnIdPage = totalStnIdPage;
            }
            return;
        }
        // recalculate index for time line
        index = ((int) (c.y - dtNextPageEnd)) / charHeight + (curStnIdPage - 1)
                * numTimeLinePerPage;

        if (index < this.stnElementList.size()) {
            switch (stnElementList.get(index).getActionState()) {

            case INACTIVE:
                stnElementList.get(index).setActionState(
                        NsharpConstants.ActState.ACTIVE);
                break;
            case ACTIVE:
                // do not allow deactivate current stn
                if (index == currentStnElementListIndex)
                    return;
                stnElementList.get(index).setActionState(
                        NsharpConstants.ActState.INACTIVE);
                break;
            default:
                return;
            }
            // findCurrentElementIndexesAfterConfig();
            // setCurSndProfileProp();
            // setCurrentSoundingLayerInfo();
            // resetData(); try these
            if (skewtPaneRsc != null)
                skewtPaneRsc.createRscWireFrameShapes();
            if (hodoPaneRsc != null)
                hodoPaneRsc.createRscHodoWindShapeAll();

        }
    }

    public void handleUserClickOnTimeLine(Coordinate c) {

        // first to find if it is for change to next/prev page
        // System.out.println("numTimeLinePerPage="+numTimeLinePerPage+"gap="+(cnYOrig-dtNextPageEnd));
        int index = ((int) (c.y - dtYOrig)) / charHeight;
        if (index == 0) {
            // change to next/previous page
            if (totalTimeLinePage == 1)
                return;
            if ((c.x - dtXOrig) < (dtWidth / 2)) {
                curTimeLinePage++;
                if (curTimeLinePage > totalTimeLinePage)
                    curTimeLinePage = 1;
            } else {
                curTimeLinePage--;
                if (curTimeLinePage <= 0)
                    curTimeLinePage = totalTimeLinePage;
            }
            return;
        }
        // recalculate index for time line
        index = ((int) (c.y - dtNextPageEnd)) / charHeight
                + (curTimeLinePage - 1) * numTimeLinePerPage;

        if (index < timeElementList.size() && index >= 0) {
            switch (timeElementList.get(index).getActionState()) {
            case INACTIVE:
                timeElementList.get(index).setActionState(
                        NsharpConstants.ActState.ACTIVE);
                break;
            case ACTIVE:
                if (index == currentTimeElementListIndex)
                    // dont allow to deactive current time line
                    return;
                timeElementList.get(index).setActionState(
                        NsharpConstants.ActState.INACTIVE);
                break;

            default:
                return;

            }

            // findCurrentElementIndexesAfterConfig();
            // setCurSndProfileProp();
            // setCurrentSoundingLayerInfo();
            // resetData(); try these
            if (skewtPaneRsc != null)
                skewtPaneRsc.createRscWireFrameShapes();
            if (hodoPaneRsc != null)
                hodoPaneRsc.createRscHodoWindShapeAll();

        }
    }

    public void handleUserClickOnSndLine(Coordinate c) {

        // first to find if it is for change to next/prev page
        // System.out.println("numTimeLinePerPage="+numTimeLinePerPage+"gap="+(cnYOrig-dtNextPageEnd));
        int index = ((int) (c.y - dtYOrig)) / charHeight;
        if (index == 0) {
            // change to next/previous page
            if (totalSndPage == 1)
                return;
            if ((c.x - dtXOrig) < (dtWidth / 2)) {
                curSndPage++;
                if (curSndPage > totalSndPage)
                    curSndPage = 1;
            } else {
                curSndPage--;
                if (curSndPage <= 0)
                    curSndPage = totalSndPage;
            }
            return;
        }
        // recalculate index for time line
        index = ((int) (c.y - dtNextPageEnd)) / charHeight + (curSndPage - 1)
                * numTimeLinePerPage;

        if (index < sndElementList.size() && index >= 0) {
            switch (sndElementList.get(index).getActionState()) {
            case INACTIVE:
                sndElementList.get(index).setActionState(
                        NsharpConstants.ActState.ACTIVE);
                break;
            case ACTIVE:
                if (index == currentSndElementListIndex)
                    // dont allow to deactive current time line
                    return;
                sndElementList.get(index).setActionState(
                        NsharpConstants.ActState.INACTIVE);
                break;

            default:
                return;

            }
            if (skewtPaneRsc != null)
                skewtPaneRsc.createRscWireFrameShapes();
            if (hodoPaneRsc != null)
                hodoPaneRsc.createRscHodoWindShapeAll();

        }
    }

    private void moveTimeLineIndexBackward() {
        previousTimeLineStateListIndex = currentTimeElementListIndex;
        int counter = 0;
        while (true) {
            currentTimeElementListIndex++;
            currentTimeElementListIndex = currentTimeElementListIndex
                    % this.timeElementList.size();
            counter++;
            if (counter > timeElementList.size())
                break;
            if (timeElementList.get(currentTimeElementListIndex)
                    .getActionState() == NsharpConstants.ActState.ACTIVE
                    && stnTimeSndTable.get(currentStnElementListIndex)
                            .get(currentTimeElementListIndex)
                            .get(currentSndElementListIndex) != null) {
                break;// out of while loop
            }

        }
    }

    private void moveTimeLineIndexForward() {
        previousTimeLineStateListIndex = currentTimeElementListIndex;
        int counter = 0;
        while (true) {
            currentTimeElementListIndex = currentTimeElementListIndex
                    + this.timeElementList.size();// so, we wont get a negative
                                                  // number
            currentTimeElementListIndex--;
            currentTimeElementListIndex = currentTimeElementListIndex
                    % this.timeElementList.size();
            counter++;
            if (counter > timeElementList.size())
                break;
            if (timeElementList.get(currentTimeElementListIndex)
                    .getActionState() == NsharpConstants.ActState.ACTIVE
                    && stnTimeSndTable.get(currentStnElementListIndex)
                            .get(currentTimeElementListIndex)
                            .get(currentSndElementListIndex) != null) {
                break;// out of while loop
            }
        }
        // System.out.println("timeline="+timeLineStateList.get(pickedTimeGroupIndex).getElementDescription());
    }

    private void moveTimeLineIndexCycle() {
        previousTimeLineStateListIndex = currentTimeElementListIndex;
        // Note: direction should only be NEXT or PREVIOUS
        int counter = 0;
        while (true) {
            counter++;
            if (counter > timeElementList.size()) {
                currentTimeElementListIndex = previousTimeLineStateListIndex;
                break;
            }
            if (currentOpDirection == IFrameCoordinator.FrameChangeOperation.NEXT) {
                currentTimeElementListIndex--;
                if (currentTimeElementListIndex <= 0) {
                    // the end of forward direction, change direction to
                    // backward
                    currentOpDirection = IFrameCoordinator.FrameChangeOperation.PREVIOUS;
                    currentTimeElementListIndex = 0;
                }

            } else { // direction is FrameChangeOperation.PREVIOUS
                currentTimeElementListIndex++;
                if (currentTimeElementListIndex >= timeElementList.size() - 1) {
                    // the end of backward direction, change direction to
                    // forward
                    currentOpDirection = IFrameCoordinator.FrameChangeOperation.NEXT;
                    currentTimeElementListIndex = timeElementList.size() - 1;
                }
            }
            if (timeElementList.get(currentTimeElementListIndex)
                    .getActionState() == NsharpConstants.ActState.ACTIVE
                    && stnTimeSndTable.get(currentStnElementListIndex)
                            .get(currentTimeElementListIndex)
                            .get(currentSndElementListIndex) != null) {
                break;// out of while loop
            }
        }

    }

    /*
     * Note: looping only apply to curAggregateTimeLineList NOT stationIdList
     */
    public void setLoopingDataTimeLine(LoopProperties loopProperties) {
        // System.out.println("setLoopingDataTimeLine loopmode ="+loopProperties.getMode().toString());
        if (this.timeElementList.size() > 0) {
            switch (loopProperties.getMode()) {
            case Forward:
                moveTimeLineIndexForward();
                break;
            case Backward:
                moveTimeLineIndexBackward();
                break;
            case Cycle:
                moveTimeLineIndexCycle();

                break;
            }

            curTimeLinePage = currentTimeElementListIndex / numTimeLinePerPage
                    + 1;
            setCurSndProfileProp();
            setCurrentSoundingLayerInfo();
            resetData();
            refreshPane();
        }

    }

    public enum LoopMode {
        Forward, Backward, Cycle
    };

    private int getElemlistActiveNumber(List<NsharpOperationElement> elemlist) {
        int n = 0;
        for (NsharpOperationElement elem : elemlist) {
            if (elem.getActionState() == NsharpConstants.ActState.ACTIVE) {
                n++;
            }
        }
        return n;
    }

    public void setSteppingTimeLine(
            IFrameCoordinator.FrameChangeOperation operation,
            IFrameCoordinator.FrameChangeMode mode) {
        if (this.timeElementList.size() > 0
                && getElemlistActiveNumber(timeElementList) > 1/*
                                                                * &&
                                                                * getAvailTimeLineNumber
                                                                * (
                                                                * currentStnStateListIndex
                                                                * )>1
                                                                */) {
            int targetIndex = currentTimeElementListIndex;
            // previousTimeLineStateListIndex = currentTimeLineStateListIndex;
            // preset index for LAST and FIRST operation
            switch (operation) {
            case LAST: // the future-est time, at top of time line shown. set to
                       // -1, so in while loop, it starts from 0
                targetIndex = -1;//
                break;
            case FIRST: // the oldest time, set to dataTimelineList.length, so
                        // in while loop, it starts from
                        // dataTimelineList.length-1
                targetIndex = timeElementList.size();
                break;
            default:
                break;

            }

            int counter = 0;
            while (true) {
                switch (operation) {
                case LAST: // the future-est time, at top of time line shown
                    targetIndex++;
                    break;
                case FIRST: // the oldest time
                    targetIndex--;
                    break;
                case PREVIOUS:
                    targetIndex++;
                    targetIndex = targetIndex % this.timeElementList.size();
                    break;
                case NEXT:
                    // so, we wont get a negative number
                    targetIndex = targetIndex + this.timeElementList.size();
                    targetIndex--;
                    targetIndex = targetIndex % this.timeElementList.size();
                    break;
                default:
                    break;
                }
                counter++;
                if (counter >= timeElementList.size())
                    return; // looped through whole list already, and index back
                            // to original

                if (timeElementList.get(targetIndex).getActionState() == NsharpConstants.ActState.ACTIVE) {
                    if (compareTmIsOn
                            && currentStnElementListIndex >= 0
                            && currentSndElementListIndex >= 0
                            && stnTimeSndTable.get(currentStnElementListIndex)
                                    .get(targetIndex)
                                    .get(currentSndElementListIndex) == null) {
                        continue;
                    } else if (compareStnIsOn) {
                        boolean found = false;
                        if (currentStnElementListIndex >= 0
                                && currentSndElementListIndex >= 0
                                && stnTimeSndTable
                                        .get(currentStnElementListIndex)
                                        .get(targetIndex)
                                        .get(currentSndElementListIndex) != null) {
                            found = true;
                        } else {
                            // find an active and available stn for this
                            // timeline and set is as current
                            for (int i = 0; i < stnElementList.size(); i++) {
                                if (stnElementList.get(i).getActionState() == NsharpConstants.ActState.ACTIVE
                                        && stnTimeSndTable
                                                .get(i)
                                                .get(targetIndex)
                                                .get(currentSndElementListIndex) != null) {
                                    currentStnElementListIndex = i;
                                    found = true;
                                    break;
                                }
                            }
                        }
                        if (!found) {
                            currentStnElementListIndex = -1;
                        } else {
                            int colorIndex = NsharpConstants.LINE_COMP1;
                            for (NsharpOperationElement elm : stnElementList) {
                                int stnIndex = stnElementList.indexOf(elm);
                                NsharpSoundingElementStateProperty stnTmElm = stnTimeSndTable
                                        .get(stnIndex).get(targetIndex)
                                        .get(currentSndElementListIndex);
                                if (stnTmElm != null) {
                                    stnTmElm.setCompColorIndex(colorIndex);
                                    // if(colorIndex >
                                    // NsharpConstants.LINE_COMP10)
                                    // colorIndex = NsharpConstants.LINE_COMP1;
                                }
                                colorIndex++;
                                if (colorIndex > NsharpConstants.LINE_COMP10)
                                    colorIndex = NsharpConstants.LINE_COMP1;
                            }
                        }
                        // no matter we find current stn or not
                        // we should get out of here
                        break;
                    } else if (compareSndIsOn) {
                        boolean found = false;
                        if (currentStnElementListIndex >= 0
                                && currentSndElementListIndex >= 0
                                && stnTimeSndTable
                                        .get(currentStnElementListIndex)
                                        .get(targetIndex)
                                        .get(currentSndElementListIndex) != null) {
                            found = true;
                        } else {
                            // find an active and available snd for this stn and
                            // set is as current
                            for (int i = 0; i < sndElementList.size(); i++) {
                                if (sndElementList.get(i).getActionState() == NsharpConstants.ActState.ACTIVE
                                        && stnTimeSndTable
                                                .get(currentStnElementListIndex)
                                                .get(targetIndex).get(i) != null) {
                                    currentSndElementListIndex = i;
                                    found = true;
                                    break;
                                }
                            }
                        }
                        if (!found) {
                            currentSndElementListIndex = -1;
                        } else {
                            int colorIndex = NsharpConstants.LINE_COMP1;
                            for (NsharpOperationElement elm : sndElementList) {
                                if (elm.getActionState() == NsharpConstants.ActState.INACTIVE)
                                    continue;
                                int sndIndex = sndElementList.indexOf(elm);
                                NsharpSoundingElementStateProperty stnTmElm = stnTimeSndTable
                                        .get(currentStnElementListIndex)
                                        .get(targetIndex).get(sndIndex);
                                if (stnTmElm != null) {
                                    stnTmElm.setCompColorIndex(colorIndex);
                                }
                                colorIndex++;
                                if (colorIndex > NsharpConstants.LINE_COMP10)
                                    colorIndex = NsharpConstants.LINE_COMP1;
                            }
                        }
                        // no matter we find current snd type for this stn or
                        // not
                        // we should get out of here
                        break;
                    } else {
                        break;
                    }
                }
            }
            previousTimeLineStateListIndex = currentTimeElementListIndex;
            currentTimeElementListIndex = targetIndex;
            curTimeLinePage = currentTimeElementListIndex / numTimeLinePerPage
                    + 1;
            setCurSndProfileProp();
            setCurrentSoundingLayerInfo();
            resetData();
            refreshPane();
        }
    }

    /*
     * Stn index stepping is only controlled by up/down arrow keys, down key =
     * PREVIOUS operation, up key = NEXT operation
     */
    public void setSteppingStnIdList(
            IFrameCoordinator.FrameChangeOperation operation) {
        if (this.stnElementList.size() > 0
                && getElemlistActiveNumber(stnElementList) > 1) {

            int counter = 0;
            while (true) {
                switch (operation) {
                case NEXT:
                    currentStnElementListIndex = currentStnElementListIndex
                            + this.stnElementList.size();
                    currentStnElementListIndex--;
                    currentStnElementListIndex = currentStnElementListIndex
                            % this.stnElementList.size();
                    break;
                case PREVIOUS:
                    // so, we wont get a negative number
                    currentStnElementListIndex++;
                    currentStnElementListIndex = currentStnElementListIndex
                            % this.stnElementList.size();
                    break;
                default:
                    break;

                }
                counter++;
                // System.out.println("counter = "+ counter);
                if (counter >= stnElementList.size())
                    return; // looped through whole list already, and index back
                            // to original
                if (stnElementList.get(currentStnElementListIndex)
                        .getActionState() == NsharpConstants.ActState.ACTIVE) {
                    if (compareStnIsOn
                            && currentTimeElementListIndex >= 0
                            && currentSndElementListIndex >= 0
                            && stnTimeSndTable.get(currentStnElementListIndex)
                                    .get(currentTimeElementListIndex)
                                    .get(currentSndElementListIndex) == null) {// .getElementState()
                                                                               // ==
                                                                               // NsharpConstants.LoadState.NOTAVAIL){
                        continue;
                    } else if (compareTmIsOn) {
                        boolean found = false;
                        if (currentTimeElementListIndex >= 0
                                && currentSndElementListIndex >= 0
                                && stnTimeSndTable
                                        .get(currentStnElementListIndex)
                                        .get(currentTimeElementListIndex)
                                        .get(currentSndElementListIndex) != null) {
                            // use currentTimeLineStateListIndex
                            found = true;
                        } else {
                            // find an active and available timeline for this
                            // stn and set is as current
                            for (int i = 0; i < timeElementList.size(); i++) {
                                if (timeElementList.get(i).getActionState() == NsharpConstants.ActState.ACTIVE
                                        && stnTimeSndTable
                                                .get(currentStnElementListIndex)
                                                .get(i)
                                                .get(currentSndElementListIndex) != null) {
                                    currentTimeElementListIndex = i;
                                    found = true;
                                    break;
                                }
                            }
                        }
                        if (!found) {
                            currentTimeElementListIndex = -1;
                        } else {
                            int colorIndex = NsharpConstants.LINE_COMP1;
                            for (NsharpOperationElement elm : timeElementList) {
                                if (elm.getActionState() == NsharpConstants.ActState.INACTIVE)
                                    continue;
                                int tmIndex = timeElementList.indexOf(elm);
                                NsharpSoundingElementStateProperty stnTmElm = stnTimeSndTable
                                        .get(currentStnElementListIndex)
                                        .get(tmIndex)
                                        .get(currentSndElementListIndex);
                                if (stnTmElm != null) {
                                    stnTmElm.setCompColorIndex(colorIndex);
                                }
                                colorIndex++;
                                if (colorIndex > NsharpConstants.LINE_COMP10)
                                    colorIndex = NsharpConstants.LINE_COMP1;

                            }
                        }
                        // no matter we find current time line for this stn or
                        // not
                        // we should get out of here
                        break;
                    } else if (compareSndIsOn) {
                        boolean found = false;
                        if (currentTimeElementListIndex >= 0
                                && currentSndElementListIndex >= 0
                                && stnTimeSndTable
                                        .get(currentStnElementListIndex)
                                        .get(currentTimeElementListIndex)
                                        .get(currentSndElementListIndex) != null) {
                            found = true;
                        } else {
                            // find an active and available snd for this stn and
                            // set is as current
                            for (int i = 0; i < sndElementList.size(); i++) {
                                if (sndElementList.get(i).getActionState() == NsharpConstants.ActState.ACTIVE
                                        && stnTimeSndTable
                                                .get(currentStnElementListIndex)
                                                .get(currentTimeElementListIndex)
                                                .get(i) != null) {
                                    currentSndElementListIndex = i;
                                    found = true;
                                    break;
                                }
                            }
                        }
                        if (!found) {
                            currentSndElementListIndex = -1;
                        } else {
                            int colorIndex = NsharpConstants.LINE_COMP1;
                            for (NsharpOperationElement elm : sndElementList) {
                                if (elm.getActionState() == NsharpConstants.ActState.INACTIVE)
                                    continue;
                                int sndIndex = sndElementList.indexOf(elm);
                                NsharpSoundingElementStateProperty stnTmElm = stnTimeSndTable
                                        .get(currentStnElementListIndex)
                                        .get(currentTimeElementListIndex)
                                        .get(sndIndex);
                                if (stnTmElm != null) {
                                    stnTmElm.setCompColorIndex(colorIndex);
                                }
                                colorIndex++;
                                if (colorIndex > NsharpConstants.LINE_COMP10)
                                    colorIndex = NsharpConstants.LINE_COMP1;
                            }
                        }
                        // no matter we find current snd type for this stn or
                        // not
                        // we should get out of here
                        break;
                    } else
                        break;
                }
            }
            curStnIdPage = currentStnElementListIndex / numTimeLinePerPage + 1;

            setCurSndProfileProp();
            setCurrentSoundingLayerInfo();
            resetData();

            refreshPane();
        }

    }

    /*
     * Snd Type index stepping is only controlled by shift + up/down arrow keys,
     * shift+down key = PREVIOUS operation, shift+up key = NEXT operation
     */
    public void setSteppingSndTypeList(
            IFrameCoordinator.FrameChangeOperation operation) {
        if (this.sndElementList.size() > 0
                && getElemlistActiveNumber(sndElementList) > 1) {

            int counter = 0;
            while (true) {
                switch (operation) {
                case NEXT:
                    currentSndElementListIndex = currentSndElementListIndex
                            + this.sndElementList.size();
                    currentSndElementListIndex--;
                    currentSndElementListIndex = currentSndElementListIndex
                            % this.sndElementList.size();
                    break;
                case PREVIOUS:
                    // so, we wont get a negative number
                    currentSndElementListIndex++;
                    currentSndElementListIndex = currentSndElementListIndex
                            % this.sndElementList.size();
                    break;
                default:
                    break;

                }
                counter++;
                // System.out.println("counter = "+ counter);
                if (counter >= sndElementList.size())
                    return; // looped through whole list already, and index back
                            // to original
                if (sndElementList.get(currentSndElementListIndex)
                        .getActionState() == NsharpConstants.ActState.ACTIVE) {
                    if (compareSndIsOn
                            && currentTimeElementListIndex >= 0
                            && currentStnElementListIndex >= 0
                            && stnTimeSndTable.get(currentStnElementListIndex)
                                    .get(currentTimeElementListIndex)
                                    .get(currentSndElementListIndex) == null) {
                        continue;
                    } else if (compareTmIsOn) {
                        boolean found = false;
                        if (currentTimeElementListIndex >= 0
                                && currentStnElementListIndex >= 0
                                && stnTimeSndTable
                                        .get(currentStnElementListIndex)
                                        .get(currentTimeElementListIndex)
                                        .get(currentSndElementListIndex) != null) {
                            // use currentTimeLineStateListIndex
                            found = true;
                        } else {
                            // find an active and available timeline for this
                            // stn and set is as current
                            for (int i = 0; i < timeElementList.size(); i++) {
                                if (timeElementList.get(i).getActionState() == NsharpConstants.ActState.ACTIVE
                                        && stnTimeSndTable
                                                .get(currentStnElementListIndex)
                                                .get(i)
                                                .get(currentSndElementListIndex) != null) {
                                    currentTimeElementListIndex = i;
                                    found = true;
                                    break;
                                }
                            }
                        }
                        if (!found) {
                            currentTimeElementListIndex = -1;
                        } else {
                            int colorIndex = NsharpConstants.LINE_COMP1;
                            for (NsharpOperationElement elm : timeElementList) {
                                if (elm.getActionState() == NsharpConstants.ActState.INACTIVE)
                                    continue;
                                int tmIndex = timeElementList.indexOf(elm);
                                NsharpSoundingElementStateProperty stnTmElm = stnTimeSndTable
                                        .get(currentStnElementListIndex)
                                        .get(tmIndex)
                                        .get(currentSndElementListIndex);
                                if (stnTmElm != null) {
                                    stnTmElm.setCompColorIndex(colorIndex);
                                }
                                colorIndex++;
                                if (colorIndex > NsharpConstants.LINE_COMP10)
                                    colorIndex = NsharpConstants.LINE_COMP1;
                            }
                        }
                        // no matter we find current time line for this stn or
                        // not
                        // we should get out of here
                        break;
                    } else if (compareStnIsOn) {
                        boolean found = false;
                        // find an active and available stn for this timeline
                        // and set is as current
                        if (currentTimeElementListIndex >= 0
                                && currentStnElementListIndex >= 0
                                && stnTimeSndTable
                                        .get(currentStnElementListIndex)
                                        .get(currentTimeElementListIndex)
                                        .get(currentSndElementListIndex) != null) {
                            found = true;
                        } else {
                            for (int i = 0; i < stnElementList.size(); i++) {
                                if (stnElementList.get(i).getActionState() == NsharpConstants.ActState.ACTIVE
                                        && stnTimeSndTable
                                                .get(i)
                                                .get(currentTimeElementListIndex)
                                                .get(currentSndElementListIndex) != null) {
                                    currentStnElementListIndex = i;
                                    found = true;
                                    break;
                                }
                            }
                        }
                        if (!found) {
                            currentStnElementListIndex = -1;
                        } else {
                            int colorIndex = NsharpConstants.LINE_COMP1;
                            for (NsharpOperationElement elm : stnElementList) {
                                int stnIndex = stnElementList.indexOf(elm);
                                NsharpSoundingElementStateProperty stnTmElm = stnTimeSndTable
                                        .get(stnIndex)
                                        .get(currentTimeElementListIndex)
                                        .get(currentSndElementListIndex);
                                if (stnTmElm != null) {
                                    stnTmElm.setCompColorIndex(colorIndex);
                                }
                                colorIndex++;
                                if (colorIndex > NsharpConstants.LINE_COMP10)
                                    colorIndex = NsharpConstants.LINE_COMP1;

                            }
                        }
                        // no matter we find current stn or not
                        // we should get out of here
                        break;
                    } else
                        break;
                }
            }
            curStnIdPage = currentSndElementListIndex / numTimeLinePerPage + 1;

            setCurSndProfileProp();
            setCurrentSoundingLayerInfo();
            resetData();

            refreshPane();
        }

    }

    // used for sorting
    public class tempPoint implements Comparable<tempPoint> {
        double diff;

        double temp;

        double pressure;

        int type; // 1= temp, 2 = dewpoint

        tempPoint(double diff, double temp, double pressure, int type) {
            this.diff = diff;
            this.temp = temp;
            this.pressure = pressure;
            this.type = type;
        }

        @Override
        public int compareTo(tempPoint o) {
            if (this.diff >= o.diff)
                return 1;
            else
                return 0;
        }
    }

    /*
     * Return the closest point to the input point on Hodo graph
     */
    public Coordinate getClosestHodoPoint(Coordinate inputC) {
        // System.out.println("picked pt  CX "+ inputC.x + " CY "+ inputC.y);
        Coordinate closeptC = new Coordinate(0, 0);
        if (hodoPaneRsc == null)
            return closeptC;

        double curSmallestDist = 10000; // picked a impossible big number to
                                        // start with
        double distance;
        boolean ptFound = false;
        NcSoundingLayer layer;
        //
        // Note: soundingLys list sorted with highest pressure as first element
        //
        for (int i = 0; i < this.soundingLys.size(); i++) {
            layer = this.soundingLys.get(i);
            double curS, curD;
            curS = layer.getWindSpeed();
            curD = layer.getWindDirection();
            closeptC = WxMath.uvComp((float) curS, (float) curD);
            closeptC = hodoPaneRsc.getHodoBackground().getWorld().map(closeptC);
            // System.out.println("closeptCx " + closeptC.x+ " closeptCy "+
            // closeptC.y);
            distance = inputC.distance(closeptC);
            // System.out.println("closeptCx " + closeptC.x+ " closeptCy "+
            // closeptC.y+" distance "+ distance + " curSmallestDist "
            // +curSmallestDist);
            if (distance < curSmallestDist) {
                curSmallestDist = distance;
                hodoEditingSoundingLayerIndex = i;
                ptFound = true;
            }
        }
        if (ptFound == false) {
            closeptC.x = closeptC.y = 0;
        } else {
            layer = this.soundingLys.get(hodoEditingSoundingLayerIndex);
            closeptC = WxMath.uvComp((float) layer.getWindSpeed(),
                    (float) layer.getWindDirection());
            closeptC = hodoPaneRsc.getHodoBackground().getWorld().map(closeptC);
        }

        // System.out.println("picked closeptCx " + closeptC.x+ " closeptCy "+
        // closeptC.y);
        return closeptC;
    }

    public List<NcSoundingLayer> getSoundingLys() {
        return soundingLys;
    }

    public List<NcSoundingLayer> getPreviousSoundingLys() {
        return previousSoundingLys;
    }

    /*
     * This interpolation is to smooth data out with a pressure gap of 25 mb per
     * layer and also keep original lowest and highest layers.
     */
    private List<NcSoundingLayer> performInterpolation(
            List<NcSoundingLayer> rawSndLysLst) {
        NcSoundingLayer newLayer = new NcSoundingLayer();
        // System.out.println(" performInterpolation");
        nsharpNative.populateSndgData(rawSndLysLst);

        List<NcSoundingLayer> mySndLst = new ArrayList<NcSoundingLayer>();
        // add top layer
        try {
            // here a shallowCopy is enough
            newLayer = (NcSoundingLayer) rawSndLysLst.get(0).clone();
            mySndLst.add(newLayer);
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
        }
        // The first layer has highest pressure
        // get a pressure value below first layer and can be divided by 25
        // exactly
        int p = (int) (rawSndLysLst.get(0).getPressure() / 25) * 25;
        float interpolatedValue;
        if (p == (int) rawSndLysLst.get(0).getPressure()) { // if p is same as
                                                            // first layer, then
                                                            // this layer is
                                                            // added already,
                                                            // start from 2nd
                                                            // layer
            p = p - 25;
        }
        for (; p >= 50; p = p - 25) {
            newLayer = new NcSoundingLayer();
            newLayer.setPressure(p);
            interpolatedValue = nsharpNative.nsharpLib.itemp(p);
            if (interpolatedValue == NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA)
                // this is not good layer data, usually happened when lowest
                // layer pressure is
                // more than 50, then when interpolate layer for pressure 50,
                // will return unvalid value
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
        if (overlayIsOn) {
            previousSoundingLys = soundingLys;
        } else {
            previousSoundingLys = null;
        }

        if (interpolateIsOn == true) {
            /*
             * TTR829 deep copy is not necessary as performInterpolation() will
             * re-allocate new layer for each layer //interpolation is on //we
             * dont want to change original raw data, so use a deep copy
             * List<NcSoundingLayer> intpSndLst = new
             * ArrayList<NcSoundingLayer>(); for(NcSoundingLayer layer: sndLys )
             * { try { intpSndLst.add((NcSoundingLayer) layer.clone()); } catch
             * (CloneNotSupportedException e) { // TODO Auto-generated catch
             * block e.printStackTrace(); } }
             * 
             * // interpolation intpSndLst = performInterpolation(intpSndLst);
             * 
             * soundingLys = intpSndLst;
             */

            soundingLys = performInterpolation(sndLys);
        } else {
            soundingLys = sndLys;

        }
    }

    public void updateDisplay(IRenderableDisplay[] displayArray,
            String paneConfigurationName) {
        skewtPaneRsc = null;
        witoPaneRsc = null;
        hodoPaneRsc = null;
        timeStnPaneRsc = null;
        insetPaneRsc = null;
        dataPaneRsc = null;
        spcGraphsPaneRsc = null;
        futurePaneRsc = null;
        for (IRenderableDisplay disp : displayArray) {
            ResourcePair rscP = disp.getDescriptor().getResourceList().get(0);
            NsharpAbstractPaneResource absPaneRsc = (NsharpAbstractPaneResource) rscP
                    .getResource();
            if (absPaneRsc instanceof NsharpSkewTPaneResource) {
                skewtPaneRsc = (NsharpSkewTPaneResource) absPaneRsc;
                skewtPaneRsc.setLinePropertyMap(linePropertyMap);
                skewtPaneRsc.setGraphConfigProperty(graphConfigProperty);
                skewtPaneRsc.setNsharpNative(nsharpNative);
            } else if (absPaneRsc instanceof NsharpDataPaneResource) {
                dataPaneRsc = (NsharpDataPaneResource) absPaneRsc;
                dataPaneRsc.setLinePropertyMap(linePropertyMap);
                dataPaneRsc.setGraphConfigProperty(graphConfigProperty);
                dataPaneRsc.setNsharpNative(nsharpNative);
                dataPaneRsc.setPageDisplayOrderNumberArray(
                        pageDisplayOrderNumberArray,
                        dataPageProperty.getNumberPagePerDisplay());
            } else if (absPaneRsc instanceof NsharpHodoPaneResource) {
                hodoPaneRsc = (NsharpHodoPaneResource) absPaneRsc;
                hodoPaneRsc.setLinePropertyMap(linePropertyMap);
                hodoPaneRsc.setGraphConfigProperty(graphConfigProperty);
                hodoPaneRsc.setNsharpNative(nsharpNative);
            } else if (absPaneRsc instanceof NsharpWitoPaneResource
                    && (paneConfigurationName
                            .equals(NsharpConstants.PANE_SPCWS_CFG_STR)
                            || paneConfigurationName
                                    .equals(NsharpConstants.PANE_DEF_CFG_1_STR) || paneConfigurationName
                                .equals(NsharpConstants.PANE_DEF_CFG_2_STR))) {

                witoPaneRsc = (NsharpWitoPaneResource) absPaneRsc;
                witoPaneRsc.setLinePropertyMap(linePropertyMap);
                witoPaneRsc.setGraphConfigProperty(graphConfigProperty);
                witoPaneRsc.setNsharpNative(nsharpNative);

            } else if (absPaneRsc instanceof NsharpInsetPaneResource
                    && (paneConfigurationName
                            .equals(NsharpConstants.PANE_SPCWS_CFG_STR)
                            || paneConfigurationName
                                    .equals(NsharpConstants.PANE_DEF_CFG_1_STR) || paneConfigurationName
                                .equals(NsharpConstants.PANE_DEF_CFG_2_STR))) {

                insetPaneRsc = (NsharpInsetPaneResource) absPaneRsc;
                insetPaneRsc.setLinePropertyMap(linePropertyMap);
                insetPaneRsc.setGraphConfigProperty(graphConfigProperty);
                insetPaneRsc.setNsharpNative(nsharpNative);

            } else if (absPaneRsc instanceof NsharpSpcGraphsPaneResource
                    && paneConfigurationName
                            .equals(NsharpConstants.PANE_SPCWS_CFG_STR)) {
                spcGraphsPaneRsc = (NsharpSpcGraphsPaneResource) absPaneRsc;
                spcGraphsPaneRsc.setLinePropertyMap(linePropertyMap);
                spcGraphsPaneRsc.setGraphConfigProperty(graphConfigProperty);
                spcGraphsPaneRsc.setNsharpNative(nsharpNative);
            } else if (absPaneRsc instanceof NsharpAbstractPaneResource
                    && paneConfigurationName
                            .equals(NsharpConstants.PANE_SIMPLE_D2D_CFG_STR)) {
                futurePaneRsc = (NsharpAbstractPaneResource) absPaneRsc;
                futurePaneRsc.setLinePropertyMap(linePropertyMap);
                futurePaneRsc.setGraphConfigProperty(graphConfigProperty);
                futurePaneRsc.setNsharpNative(nsharpNative);

            } else if (absPaneRsc instanceof NsharpTimeStnPaneResource
                    && (paneConfigurationName
                            .equals(NsharpConstants.PANE_SIMPLE_D2D_CFG_STR)
                            || paneConfigurationName
                                    .equals(NsharpConstants.PANE_DEF_CFG_1_STR) || paneConfigurationName
                                .equals(NsharpConstants.PANE_DEF_CFG_2_STR))) {
                timeStnPaneRsc = (NsharpTimeStnPaneResource) absPaneRsc;
                timeStnPaneRsc.setLinePropertyMap(linePropertyMap);
                timeStnPaneRsc.setGraphConfigProperty(graphConfigProperty);
                timeStnPaneRsc.setNsharpNative(nsharpNative);
            }
        }
        this.displayArray = displayArray;
    }

    public void resetRscSoundingData() {
        if (skewtPaneRsc != null)
            skewtPaneRsc.resetData(soundingLys, previousSoundingLys);
        if (hodoPaneRsc != null)
            hodoPaneRsc.resetData(soundingLys, previousSoundingLys);
        if (witoPaneRsc != null)
            witoPaneRsc.resetData(soundingLys, previousSoundingLys);
        if (dataPaneRsc != null)
            dataPaneRsc.resetData(soundingLys, previousSoundingLys);
        if (insetPaneRsc != null)
            insetPaneRsc.resetData(soundingLys, previousSoundingLys);
        if (spcGraphsPaneRsc != null)
            spcGraphsPaneRsc.resetData(soundingLys, previousSoundingLys);
    }

    public NsharpResourceHandler(IRenderableDisplay[] displayArray,
            NsharpEditor editor) {
        // System.out.println("NsharpResourceHandler constructed");
        // myNsharpEditor = editor;
        elementColorMap.put(NsharpConstants.ActState.CURRENT.name(),
                NsharpConstants.color_green);
        elementColorMap.put(NsharpConstants.ActState.ACTIVE.name(),
                NsharpConstants.color_yellow);
        elementColorMap.put(NsharpConstants.ActState.INACTIVE.name(),
                NsharpConstants.color_white);
        // elementColorMap.put(NsharpConstants.LoadState.NOTAVAIL.name(),NsharpConstants.color_red);
        // elementColorMap.put(NsharpConstants.ActState.OVERLAY,NsharpConstants.color_red);
        // elementColorMap.put(NsharpConstants.LoadState.AVAIL.name(),NsharpConstants.color_yellow);
        nsharpNative = new NsharpNative();
        // System.out.println("NsharpResourceHandler constructed"+this.toString()
        // + " nsharpNative="+nsharpNative.toString());
        // based on BigNsharp storm slinky color used and gempak color
        // definition
        stormSlinkyColorMap.put(new Integer(3), NsharpConstants.color_green); // green
        stormSlinkyColorMap.put(new Integer(7), NsharpConstants.color_magenta);
        stormSlinkyColorMap.put(new Integer(6), NsharpConstants.color_cyan);
        stormSlinkyColorMap.put(new Integer(13),
                NsharpConstants.color_violet_md);
        stormSlinkyColorMap.put(new Integer(20),
                NsharpConstants.color_yellow_DK);
        stormSlinkyColorMap.put(new Integer(27), NsharpConstants.color_cyan_md);
        NsharpPaletteWindow win = NsharpPaletteWindow.getInstance();
        if (win != null)
            currentGraphMode = win.getCurrentGraphMode();

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
        updateDisplay(displayArray, paneConfigurationName);
        displayDataPageMax = NsharpConstants.PAGE_MAX_NUMBER
                / dataPageProperty.getNumberPagePerDisplay();

        DateFormatSymbols dfs = new DateFormatSymbols();
        defaultDays = dfs.getShortWeekdays();
    }

    public void disposeInternal() {
        // System.out.println("NsharpResourceHandler disposeInternal called");
        listenerList = null;
        soundingLys = null;
        previousSoundingLys = null;
        stormSlinkyColorMap = null;
        elementColorMap = null;

        if (NsharpParcelDialog.getAccess() != null) {
            NsharpParcelDialog.getAccess().reset();
        }

        nsharpNative = null;
    }

    public void printHeightMark(WGraphics world, GC gc) throws VizException {
        // print feet scales...
        double vyMax = world.getViewYmax();
        double vyMin = world.getViewYmin();
        double vxMax = world.getViewXmax();
        for (int j = 0; j < NsharpConstants.HEIGHT_LEVEL_FEET.length; j++) {
            float meters = (float) NsharpConstants.feetToMeters
                    .convert(NsharpConstants.HEIGHT_LEVEL_FEET[j]);

            double pressure = nsharpNative.nsharpLib.ipres(meters);
            double y = world.mapY(NsharpWxMath.getSkewTXY(pressure, -50).y);

            gc.drawString(Integer
                    .toString(NsharpConstants.HEIGHT_LEVEL_FEET[j] / 1000),
                    (int) vxMax + 40, (int) y, false);

            gc.drawLine((int) vxMax + 50, (int) y, (int) vxMax + 45, (int) y);
        }
        // print meter scales...
        for (int j = 0; j < NsharpConstants.HEIGHT_LEVEL_METERS.length; j++) {
            int meters = NsharpConstants.HEIGHT_LEVEL_METERS[j];

            double pressure = nsharpNative.nsharpLib.ipres(meters);
            double y = world.mapY(NsharpWxMath.getSkewTXY(pressure, -50).y);

            gc.drawString(Integer.toString(meters / 1000), (int) vxMax + 52,
                    (int) y, false);

            gc.drawLine((int) vxMax + 50, (int) y, (int) vxMax + 55, (int) y);
        }
        // print surface level mark
        double y = world.mapY(NsharpWxMath.getSkewTXY(soundingLys.get(0)
                .getPressure(), -50).y);
        gc.drawString(
                "SFC("
                        + Integer.toString((int) (soundingLys.get(0)
                                .getGeoHeight())) + "m)", (int) vxMax + 50,
                (int) y, false);
        gc.drawLine((int) vxMax + 50, (int) y, (int) vxMax + 55, (int) y);
        // top level mark at 100 mbar
        y = world.mapY(NsharpWxMath.getSkewTXY(100, -50).y);
        float hgt = nsharpNative.nsharpLib.ihght(100);
        gc.drawString(Float.toString(hgt / 1000F), (int) vxMax + 50, (int) y,
                false);
        gc.drawString("Kft  Km", (int) vxMax + 35, (int) y - 8);
        gc.drawString("MSL", (int) vxMax + 45, (int) y - 15);
        gc.drawLine((int) vxMax + 40, (int) y, (int) vxMax + 60, (int) y);

        gc.drawLine((int) vxMax + 50, (int) vyMin, (int) vxMax + 50,
                (int) vyMax);

    }

    /**
     * Prints the pressure lines number at left side out of skewT bkgd for
     * printing job
     * 
     * @throws VizException
     */
    public void printNsharpPressureLinesNumber(WGraphics world, GC gc)
            throws VizException {
        String s = null;
        double vxMax = world.getViewXmax();
        double vxMin = world.getViewXmin();
        for (int i = 0; i < NsharpConstants.PRESSURE_MAIN_LEVELS.length; i++) {
            // we only care about pressure for this case, temp is no important
            // when calling getSkewTXY
            Coordinate coor = NsharpWxMath.getSkewTXY(
                    NsharpConstants.PRESSURE_MAIN_LEVELS[i], 0);

            gc.drawLine((int) vxMin, (int) world.mapY(coor.y), (int) vxMax,
                    (int) world.mapY(coor.y));

        }
        for (int i = 0; i < NsharpConstants.PRESSURE_MARK_LEVELS.length; i++) {
            // we only care about pressure for this case, temp is no important
            // when calling getSkewTXY
            Coordinate coor = NsharpWxMath.getSkewTXY(
                    NsharpConstants.PRESSURE_MARK_LEVELS[i], 0);

            gc.drawLine((int) vxMin, (int) world.mapY(coor.y),
                    (int) vxMin + 10, (int) world.mapY(coor.y));

        }
        for (int i = 0; i < NsharpConstants.PRESSURE_NUMBERING_LEVELS.length; i++) {
            s = NsharpConstants.pressFormat
                    .format(NsharpConstants.PRESSURE_NUMBERING_LEVELS[i]);
            // we only care about pressure for this case, temp is no important
            // when calling getSkewTXY
            Coordinate coor = NsharpWxMath.getSkewTXY(
                    NsharpConstants.PRESSURE_NUMBERING_LEVELS[i], 0);

            gc.drawString(s, (int) vxMin - 20, (int) world.mapY(coor.y), false);
        }
    }

    /**
     * Print the temp number at bottom out of skewT bkgd for printing job
     * 
     * @throws VizException
     */
    public void printNsharpTempNumber(WGraphics world, GC gc)
            throws VizException {
        for (int i = 40; i > -50; i -= 10) {
            Coordinate coorStart = NsharpWxMath.getSkewTXY(1050, i);
            double startX = world.mapX(coorStart.x);
            double startY = world.mapY(coorStart.y);

            gc.drawString(Integer.toString(i), (int) startX, (int) startY + 5,
                    false);
        }
        for (int i = -60; i > -120; i -= 10) {
            Coordinate coorEnd = NsharpWxMath.getSkewTXY(100, i);

            // System.out.println("X = "+ startX + " Y = "+ startY);
            double endX = world.mapX(coorEnd.x);
            double endY = world.mapY(coorEnd.y);

            gc.drawString(Integer.toString(i), (int) endX, (int) endY - 10,
                    false);
        }
    }

    /**
     * 
     * Print Wind barb for printing job This function followed algorithm in
     * plot_barbs (void) at xwvid1.c to choose wind bulb for drawing around
     * every 400m
     * 
     */
    public void printNsharpWind(WGraphics world, GC gc) throws VizException {
        ArrayList<List<LineStroke>> windList = new ArrayList<List<LineStroke>>();

        double windX = world.getViewXmax() + 6 * BARB_LENGTH;
        // System.out.println("windX="+windX);
        float lastHeight = -999;
        double windY;
        for (NcSoundingLayer layer : soundingLys) {
            float pressure = layer.getPressure();
            float spd = layer.getWindSpeed();
            float dir = layer.getWindDirection();

            if (pressure < 100) {
                continue;
            }

            if ((layer.getGeoHeight() - lastHeight) < 400) {

                continue;
            }

            // Get the vertical ordinate.
            windY = world.mapY(NsharpWxMath.getSkewTXY(pressure, 0).y);

            List<LineStroke> barb = WindBarbFactory.getWindGraphics(
            /* metersPerSecondToKnots.convert */(double) (spd), (double) dir);
            if (barb != null) {
                WindBarbFactory.scaleBarb(barb, -7);
                WindBarbFactory.translateBarb(barb, windX, windY);
                windList.add(barb);
            }

            lastHeight = layer.getGeoHeight();
        }
        Coordinate pt1 = new Coordinate(0, 0), pt2;
        for (List<LineStroke> barb : windList) {
            for (LineStroke stroke : barb) {
                // stroke render: rewrite stroke.render() for our printing
                // purpose
                if (stroke.getType() == "M") {
                    pt1 = stroke.getPoint();
                    // change X coordinate by mirroring x coordinate at windX
                    // axis. AS we scaleBarb with -5 time.
                    // It is easier to mirror at x-axis for this case.
                    pt1.x = windX - (pt1.x - windX);
                    // System.out.print("Myp1x="+(int)pt1.x+" p1y="+(int)pt1.y);

                } else if (stroke.getType() == "D") {
                    pt2 = stroke.getPoint();
                    pt2.x = windX - (pt2.x - windX);
                    // System.out.println( " p2x="+(int)pt2.x+" p2y="+
                    // (int)pt2.y);
                    gc.drawLine((int) pt1.x, (int) pt1.y, (int) pt2.x,
                            (int) pt2.y);
                }
            }
        }
        gc.drawLine((int) windX,
                (int) world.mapY(NsharpWxMath.getSkewTXY(100, 0).y),
                (int) windX,
                (int) world.mapY(NsharpWxMath.getSkewTXY(1000, 0).y));
    }

    /**
     * 
     * Print the wetbulb trace curve
     * 
     * @throws VizException
     */
    public void printNsharpWetbulbTraceCurve(WGraphics world, GC gc)
            throws VizException {
        if ((soundingLys == null) || (soundingLys.size() == 0))
            return;
        float t1;

        Coordinate c2 = null;
        Coordinate c1;
        // print trace
        for (NcSoundingLayer layer : this.soundingLys) {
            if (layer.getDewpoint() > -200) {
                t1 = nsharpNative.nsharpLib.wetbulb(layer.getPressure(),
                        layer.getTemperature(), layer.getDewpoint());

                c1 = NsharpWxMath.getSkewTXY(layer.getPressure(), t1);
                c1.x = world.mapX(c1.x);
                c1.y = world.mapY(c1.y);
                if (c2 != null) {
                    gc.drawLine((int) c1.x, (int) c1.y, (int) c2.x, (int) c2.y);
                }
                c2 = c1;
            }
        }

    }

    public void printNsharpParcelTraceCurve(WGraphics world, GC gc)
            throws VizException {
        if (soundingLys.size() > 0) {
            // for (ParcelData parData: parcelList){
            // plotNsharpParcelTraceCurve(null, 0, world,
            // NsharpConstants.color_white,parData.parcelType,
            // parData.userPressure, gc, true);
            // nsharpNative.nsharpLib.define_parcel(parData.parcelType,
            // parData.parcelLayerPressure);
            nsharpNative.nsharpLib.define_parcel(currentParcel,
                    currentParcelLayerPressure);
            _lplvalues lpvls = new _lplvalues();
            nsharpNative.nsharpLib.get_lpvaluesData(lpvls);

            float sfctemp, sfcdwpt, sfcpres;
            sfctemp = lpvls.temp;
            sfcdwpt = lpvls.dwpt;
            sfcpres = lpvls.pres;

            float vtemp = nsharpNative.nsharpLib.virtemp(sfcpres, sfctemp,
                    sfcdwpt);
            Coordinate c1 = NsharpWxMath.getSkewTXY(sfcpres, vtemp);
            c1.x = world.mapX(c1.x);
            c1.y = world.mapY(c1.y);
            FloatByReference p2 = new FloatByReference(0), t2 = new FloatByReference(
                    0);
            ;
            nsharpNative.nsharpLib.drylift(sfcpres, sfctemp, sfcdwpt, p2, t2);
            vtemp = nsharpNative.nsharpLib.virtemp(p2.getValue(),
                    t2.getValue(), t2.getValue());
            Coordinate c2 = NsharpWxMath.getSkewTXY(p2.getValue(), vtemp);
            c2.x = world.mapX(c2.x);
            c2.y = world.mapY(c2.y);

            gc.drawLine((int) c1.x, (int) c1.y, (int) c2.x, (int) c2.y);
            c1 = c2;

            float t3;
            for (float i = p2.getValue() - 50; i >= 100; i = i - 50) {
                t3 = nsharpNative.nsharpLib.wetlift(p2.getValue(),
                        t2.getValue(), i);
                vtemp = nsharpNative.nsharpLib.virtemp(i, t3, t3);
                c2 = NsharpWxMath.getSkewTXY(i, vtemp);
                c2.x = world.mapX(c2.x);
                c2.y = world.mapY(c2.y);

                gc.drawLine((int) c1.x, (int) c1.y, (int) c2.x, (int) c2.y);
                c1 = c2;
            }

            t3 = nsharpNative.nsharpLib.wetlift(p2.getValue(), t2.getValue(),
                    100);
            vtemp = nsharpNative.nsharpLib.virtemp(100, t3, t3);
            c2 = NsharpWxMath.getSkewTXY(100, vtemp);
            c2.x = world.mapX(c2.x);
            c2.y = world.mapY(c2.y);

            gc.drawLine((int) c1.x, (int) c1.y, (int) c2.x, (int) c2.y);
            // }
        }
    }

    /**
     * 
     * Print the temperature curve when during overlap or compare mode
     * 
     * @throws VizException
     */

    public void printNsharpPressureTempCurve(WGraphics world, int type, GC gc,
            List<NcSoundingLayer> soundingLys) throws VizException {
        if ((soundingLys == null) || (soundingLys.size() == 0))
            return;

        double maxPressure = NsharpWxMath.reverseSkewTXY(new Coordinate(0,
                world.getWorldYmax())).y;
        double minPressure = NsharpWxMath.reverseSkewTXY(new Coordinate(0,
                world.getWorldYmin())).y;
        Coordinate c0 = null;
        for (NcSoundingLayer layer : soundingLys) {
            double t;
            if (type == TEMP_TYPE)
                t = layer.getTemperature();
            else if (type == DEWPOINT_TYPE)
                t = layer.getDewpoint();
            else
                break;
            double pressure = layer.getPressure();
            if (t != INVALID_DATA && pressure >= minPressure
                    && pressure <= maxPressure) {

                Coordinate c1 = NsharpWxMath.getSkewTXY(pressure, t);

                c1.x = world.mapX(c1.x);
                c1.y = world.mapY(c1.y);
                // System.out.println("C.x "+ c1.x + " C.y "+ c1.y);
                if (c0 != null) {
                    gc.drawLine((int) c0.x, (int) c0.y, (int) c1.x, (int) c1.y);
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

    public void printNsharpHodoWind(WGraphics world, GC gc,
            List<NcSoundingLayer> soundingLays) throws VizException {
        Coordinate c0 = null;
        Coordinate c1;
        for (NcSoundingLayer layer : soundingLays) {
            if (layer.getPressure() < 100 || layer.getWindSpeed() < 0)
                continue;
            float wspd = layer.getWindSpeed();
            float wdir = layer.getWindDirection();
            c1 = WxMath.uvComp(wspd, wdir);
            if (c0 != null) {
                gc.setLineWidth(1);
                gc.drawLine((int) world.mapX(c0.x), (int) world.mapY(c0.y),
                        (int) world.mapX(c1.x), (int) world.mapY(c1.y));
            }
            c0 = c1;
        }

    }

    public boolean isPlotInteractiveTemp() {
        return plotInteractiveTemp;
    }

    public void setPlotInteractiveTemp(boolean plotInteractiveTemp) {
        this.plotInteractiveTemp = plotInteractiveTemp;
        if (skewtPaneRsc != null)
            skewtPaneRsc.setPlotInteractiveTemp(plotInteractiveTemp);
    }

    public void setInteractiveTempPointCoordinate(
            Coordinate interactiveTempPointCoordinate) {
        // System.out.println("setInteractiveTempPointCoordinate called");
        this.interactiveTempPointCoordinate = interactiveTempPointCoordinate;
        plotInteractiveTemp = true;
        if (skewtPaneRsc != null) {
            skewtPaneRsc.setPlotInteractiveTemp(plotInteractiveTemp);
            skewtPaneRsc
                    .setInteractiveTempPointCoordinate(interactiveTempPointCoordinate);
        }
    }

    public void setInteractiveHodoPointCoordinate(Coordinate c) {
        if (hodoPaneRsc == null)
            return;
        try {
            NcSoundingLayer hodoLayer = soundingLys
                    .get(hodoEditingSoundingLayerIndex);
            if (hodoLayer != null) {
                // TTR575
                nsharpNative.populateSndgData(soundingLys);
                // end TTR575
                Coordinate c1 = hodoPaneRsc.getHodoBackground().getWorld()
                        .unMap(c.x, c.y);
                // System.out.println("picked pt after unmap CX "+ c1.x +
                // " CY "+ c1.y);
                c1 = WxMath.speedDir((float) c1.x, (float) c1.y);
                hodoLayer.setWindSpeed((float) c1.x);
                hodoLayer.setWindDirection((float) c1.y);
                hodoPaneRsc.createRscHodoWindShapeAll();
                if (witoPaneRsc != null)
                    witoPaneRsc.createAllWireFrameShapes();
                if (insetPaneRsc != null)
                    insetPaneRsc.createInsetWireFrameShapes();
                if (skewtPaneRsc != null)
                    // CHIN:::fix edit zoom issue skewtPaneRsc.handleResize();
                    skewtPaneRsc.createRscWireFrameShapes();
            }
        } catch (Exception e) {

        }
    }

    public void applyMovingTempLine() {
        if (skewtPaneRsc == null)
            return;
        Coordinate inC = NsharpWxMath.reverseSkewTXY(skewtPaneRsc.getWorld()
                .unMap(interactiveTempPointCoordinate));
        float inTemp = (float) inC.x;
        currentSoundingLayerIndex = skewtPaneRsc.getCurrentSoundingLayerIndex();
        currentTempCurveType = skewtPaneRsc.getCurrentTempCurveType();
        float currentLayerTemp, currentLayerDewP;
        float smallestGap = skewtPaneRsc.getTempDewPtSmallestGap();
        float tempShiftedDist;
        currentLayerTemp = soundingLys.get(currentSoundingLayerIndex)
                .getTemperature();
        currentLayerDewP = soundingLys.get(currentSoundingLayerIndex)
                .getDewpoint();
        if (currentTempCurveType == TEMP_TYPE) {
            if (inTemp < currentLayerTemp) {
                // shift to left, tempShiftedDist should be a negative number
                if ((currentLayerTemp - inTemp) > smallestGap) {
                    tempShiftedDist = -smallestGap;
                } else {
                    tempShiftedDist = inTemp - currentLayerTemp;
                }
            } else {
                // shift to right, tempShiftedDist should be a positive number
                tempShiftedDist = inTemp - currentLayerTemp;
            }
        } else {
            if (inTemp < currentLayerDewP) {
                // shift to left, tempShiftedDist should be a negative number
                tempShiftedDist = inTemp - currentLayerDewP;
            } else {
                // shift to right, tempShiftedDist should be a positive number
                if ((inTemp - currentLayerDewP) > smallestGap) {
                    tempShiftedDist = smallestGap;
                } else {
                    tempShiftedDist = inTemp - currentLayerDewP;
                }
            }
        }
        for (NcSoundingLayer layer : soundingLys) {
            float t;
            if (currentTempCurveType == TEMP_TYPE) {
                t = layer.getTemperature();
                if (t != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA) {
                    layer.setTemperature(t + tempShiftedDist);
                }
            } else {
                t = layer.getDewpoint();
                if (t != NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA) {
                    layer.setDewpoint(t + tempShiftedDist);
                }
            }
        }
        // re-populate snd data to nsharp native code lib for later calculating
        nsharpNative.populateSndgData(soundingLys);
        // get storm motion wind data after populate sounding from NsharpLib
        skewtPaneRsc.setSoundingLys(soundingLys);
        // CHIN:::fix edit zoom issue skewtPaneRsc.handleResize();
        skewtPaneRsc.createRscWireFrameShapes();
        if (hodoPaneRsc != null) {
            hodoPaneRsc.setSoundingLys(soundingLys);
            hodoPaneRsc.createRscHodoWindShapeAll();
        }
    }

    public void applyInteractiveTempPoint() {
        if (skewtPaneRsc == null)
            return;
        Coordinate inC = NsharpWxMath.reverseSkewTXY(skewtPaneRsc.getWorld()
                .unMap(interactiveTempPointCoordinate));
        double inTemp = inC.x;
        currentSoundingLayerIndex = skewtPaneRsc.getCurrentSoundingLayerIndex();
        NcSoundingLayer layer = this.soundingLys.get(currentSoundingLayerIndex);
        currentTempCurveType = skewtPaneRsc.getCurrentTempCurveType();
        // System.out.println("applyInteractiveTempPoint called pressure " +
        // inC.y + " temp "+ inTemp +
        // " currentTempCurveType " + currentTempCurveType );

        if (currentTempCurveType == TEMP_TYPE) {
            if (inTemp < layer.getDewpoint())
                // temp can not be lower than dew point
                layer.setTemperature(layer.getDewpoint());
            else
                layer.setTemperature((float) inTemp);
        } else {
            if (inTemp > layer.getTemperature())
                // dew point can not be higher than temp
                layer.setDewpoint(layer.getTemperature());
            else
                layer.setDewpoint((float) inTemp);
        }

        // re-populate snd data to nsharp native code lib for later calculating
        nsharpNative.populateSndgData(soundingLys);
        // get storm motion wind data after populate sounding from NsharpLib
        skewtPaneRsc.setSoundingLys(soundingLys);
        // CHIN:::fix edit zoom issue skewtPaneRsc.handleResize();
        skewtPaneRsc.createRscWireFrameShapes();

        if (hodoPaneRsc != null) {
            hodoPaneRsc.setSoundingLys(soundingLys);
            hodoPaneRsc.createRscHodoWindShapeAll();
        }
    }

    public void applySfcEditing(float tp, float dp, float ws, float wd,
            float pressure) {
        currentSoundingLayerIndex = nsharpNative.nsharpLib.sfc();
        NcSoundingLayer layer = this.soundingLys.get(currentSoundingLayerIndex);
        layer.setTemperature(tp);
        layer.setDewpoint(dp);
        layer.setWindDirection(wd);
        layer.setWindSpeed(ws);
        layer.setPressure(pressure);
        // re-populate snd data to nsharp native code lib for later calculating
        nsharpNative.populateSndgData(soundingLys);
        if (skewtPaneRsc != null)
            skewtPaneRsc.setSoundingLys(soundingLys);
        if (hodoPaneRsc != null)
            hodoPaneRsc.setSoundingLys(soundingLys);
        if (insetPaneRsc != null)
            insetPaneRsc.setSoundingLys(soundingLys);
        if (witoPaneRsc != null)
            witoPaneRsc.setSoundingLys(soundingLys);
        if (dataPaneRsc != null)
            dataPaneRsc.setSoundingLys(soundingLys);
    }

    public void updateLayer(int layerIndex, float tp, float dp, float ws,
            float wd, float pressure) {
        if (layerIndex < 0 || layerIndex >= soundingLys.size())
            return;
        currentSoundingLayerIndex = layerIndex;
        NcSoundingLayer layer = soundingLys.get(currentSoundingLayerIndex);
        layer.setGeoHeight(nsharpNative.nsharpLib.ihght(pressure));
        layer.setTemperature(tp);
        layer.setDewpoint(dp);
        layer.setWindDirection(wd);
        layer.setWindSpeed(ws);
        layer.setPressure(pressure);
        // re-populate snd data to nsharp native code lib for later calculating
        Collections.sort(soundingLys,
                NsharpDataHandling.reversePressureHeightWindComparator());
        nsharpNative.populateSndgData(soundingLys);
        // get storm motion wind data after populate sounding from NsharpLib
        // refresh test area if it is shown now
        NsharpShowTextDialog textarea = NsharpShowTextDialog.getAccess();
        if (textarea != null) {
            textarea.refreshTextData();
        }
        if (skewtPaneRsc != null) {
            skewtPaneRsc.setSoundingLys(soundingLys);
            // CHIN:::fix edit zoom issue skewtPaneRsc.handleResize();
            skewtPaneRsc.createRscWireFrameShapes();
        }
        if (hodoPaneRsc != null) {
            hodoPaneRsc.setSoundingLys(soundingLys);
            hodoPaneRsc.createRscHodoWindShapeAll();
        }
        if (insetPaneRsc != null) {
            insetPaneRsc.setSoundingLys(soundingLys);
            insetPaneRsc.createInsetWireFrameShapes();
        }
        if (witoPaneRsc != null) {
            witoPaneRsc.setSoundingLys(soundingLys);
            witoPaneRsc.createAllWireFrameShapes();
        }
        if (dataPaneRsc != null)
            dataPaneRsc.setSoundingLys(soundingLys);
    }

    public void addNewLayer(float tp, float dp, float ws, float wd,
            float pressure) {
        // NsharpBackgroundResource bkRsc = descriptor.getSkewTBkGResource();
        // currentSoundingLayer =layerIndex;
        NcSoundingLayer layer = new NcSoundingLayer();
        layer.setGeoHeight(nsharpNative.nsharpLib.ihght(pressure));
        layer.setTemperature(tp);
        layer.setDewpoint(dp);
        layer.setWindDirection(wd);
        layer.setWindSpeed(ws);
        layer.setPressure(pressure);
        soundingLys.add(layer);
        // re-populate snd data to nsharp native code lib for later calculating
        Collections.sort(soundingLys,
                NsharpDataHandling.reversePressureHeightWindComparator());
        nsharpNative.populateSndgData(soundingLys);
        // get storm motion wind data after populate sounding from NsharpLib
        // refresh text area if it is shown now
        NsharpShowTextDialog textarea = NsharpShowTextDialog.getAccess();
        if (textarea != null) {
            textarea.refreshTextData();
        }
        if (skewtPaneRsc != null) {
            skewtPaneRsc.setSoundingLys(soundingLys);
            // CHIN:::fix edit zoom issue skewtPaneRsc.handleResize();
            skewtPaneRsc.createRscWireFrameShapes();
        }
        if (hodoPaneRsc != null) {
            hodoPaneRsc.setSoundingLys(soundingLys);
            hodoPaneRsc.createRscHodoWindShapeAll();
        }
        if (insetPaneRsc != null) {
            insetPaneRsc.setSoundingLys(soundingLys);
            insetPaneRsc.createInsetWireFrameShapes();
        }
        if (witoPaneRsc != null) {
            witoPaneRsc.setSoundingLys(soundingLys);
            witoPaneRsc.createAllWireFrameShapes();
        }
        if (dataPaneRsc != null)
            dataPaneRsc.setSoundingLys(soundingLys);
    }

    public void setGraphConfigProperty(NsharpGraphProperty graphConfigProperty) {
        this.graphConfigProperty = graphConfigProperty;
        int tempOffset = graphConfigProperty.getTempOffset();
        NsharpWxMath.setTempOffset(tempOffset);
        if (skewtPaneRsc != null) {
            skewtPaneRsc.setGraphConfigProperty(graphConfigProperty);
            skewtPaneRsc.handleResize();
            skewtPaneRsc.getSkewTBackground().setGraphConfigProperty(
                    graphConfigProperty);
        }
        if (hodoPaneRsc != null) {
            hodoPaneRsc.setGraphConfigProperty(graphConfigProperty);
            hodoPaneRsc.createRscHodoWindShapeAll();
        }
        if (witoPaneRsc != null) {
            witoPaneRsc.setGraphConfigProperty(graphConfigProperty);
            witoPaneRsc.createAllWireFrameShapes();
        }
        if (insetPaneRsc != null) {
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
        if (skewtPaneRsc != null) {
            skewtPaneRsc.setLinePropertyMap(linePropertyMap);
            skewtPaneRsc.handleResize();
        }
        if (hodoPaneRsc != null) {
            hodoPaneRsc.setLinePropertyMap(linePropertyMap);
            hodoPaneRsc.createRscHodoWindShapeAll();
        }
        if (timeStnPaneRsc != null)
            timeStnPaneRsc.setLinePropertyMap(linePropertyMap);
    }

    private void updatePageOrderArray() {
        pageDisplayOrderNumberArray[NsharpConstants.PAGE_SUMMARY1] = dataPageProperty
                .getSummary1Page();
        pageDisplayOrderNumberArray[NsharpConstants.PAGE_SUMMARY2] = dataPageProperty
                .getSummary2Page();
        pageDisplayOrderNumberArray[NsharpConstants.PAGE_PARCEL_DATA] = dataPageProperty
                .getParcelDataPage();
        pageDisplayOrderNumberArray[NsharpConstants.PAGE_THERMODYNAMIC_DATA] = dataPageProperty
                .getThermodynamicDataPage();
        pageDisplayOrderNumberArray[NsharpConstants.PAGE_OPC_DATA] = dataPageProperty
                .getOpcDataPage();
        pageDisplayOrderNumberArray[NsharpConstants.PAGE_MIXING_HEIGHT] = dataPageProperty
                .getMixingHeightPage();
        pageDisplayOrderNumberArray[NsharpConstants.PAGE_STORM_RELATIVE] = dataPageProperty
                .getStormRelativePage();
        pageDisplayOrderNumberArray[NsharpConstants.PAGE_MEAN_WIND] = dataPageProperty
                .getMeanWindPage();
        pageDisplayOrderNumberArray[NsharpConstants.PAGE_CONVECTIVE_INITIATION] = dataPageProperty
                .getConvectiveInitiationPage();
        pageDisplayOrderNumberArray[NsharpConstants.PAGE_SEVERE_POTENTIAL] = dataPageProperty
                .getSeverePotentialPage();
    }

    public void setDataPageProperty(NsharpDataPageProperty dataPageProperty) {
        this.dataPageProperty = dataPageProperty;
        updatePageOrderArray();
        if (dataPaneRsc != null)
            dataPaneRsc.setPageDisplayOrderNumberArray(
                    pageDisplayOrderNumberArray,
                    dataPageProperty.getNumberPagePerDisplay());
    }

    public void handleTimeLineActConfig(List<String> tlList,
            NsharpConstants.ActState actSt) {
        for (String tlStr : tlList) {
            for (NsharpOperationElement tl : timeElementList) {
                if (tlStr.equals(tl.getElementDescription())) {
                    tl.setActionState(actSt);
                    break;
                }
            }
        }
    }

    public void handleStationActConfig(List<String> stnList,
            NsharpConstants.ActState actSt) {
        for (String tlStr : stnList) {
            for (NsharpOperationElement stn : stnElementList) {
                if (tlStr.equals(stn.getElementDescription())) {
                    stn.setActionState(actSt);
                    break;
                }
            }
        }
    }

    public void handleSndTypeActConfig(List<String> sndTypeList,
            NsharpConstants.ActState actSt) {
        for (String tlStr : sndTypeList) {
            for (NsharpOperationElement sndType : sndElementList) {
                if (tlStr.equals(sndType.getElementDescription())) {
                    sndType.setActionState(actSt);
                    break;
                }
            }
        }
    }

    public int getCurrentTextChapter() {
        return currentTextChapter;
    }

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

    public int getCurSndPage() {
        return curSndPage;
    }

    public HashMap<String, RGB> getElementColorMap() {
        return elementColorMap;
    }

    public int getTotalTimeLinePage() {
        return totalTimeLinePage;
    }

    public int getTotalStnIdPage() {
        return totalStnIdPage;
    }

    public int getTotalSndPage() {
        return totalSndPage;
    }

    public void repopulateSndgData() {
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

    public NsharpDataPaneResource getDataPaneRsc() {
        return dataPaneRsc;
    }

    public void setTimeStnBoxData(int cnYOrig, int dtNextPage_end, int dtYOrig,
            int dtXOrig, int dtWidth, int charHeight) {
        this.charHeight = charHeight;
        this.dtYOrig = dtYOrig;
        this.dtXOrig = dtXOrig;
        this.dtWidth = dtWidth;
        this.cnYOrig = cnYOrig;
        this.dtNextPageEnd = dtNextPage_end;
        calculateTimeStnBoxData();
    }

    public void refreshPane() {
        for (int i = 0; i < displayArray.length; i++) {
            displayArray[i].refresh();
        }
    }

    public String getPaneConfigurationName() {
        return paneConfigurationName;
    }

    private void calculateTimeStnBoxData() {
        // set total time line group and stn id list page number
        numTimeLinePerPage = (cnYOrig - dtNextPageEnd) / charHeight;
        if (numTimeLinePerPage <= 0)
            numTimeLinePerPage = 1;
        // System.out.println("numTimeLinePerPage="+numTimeLinePerPage);
        totalTimeLinePage = timeElementList.size() / numTimeLinePerPage;
        if (timeElementList.size() % numTimeLinePerPage != 0)
            totalTimeLinePage = totalTimeLinePage + 1;
        curTimeLinePage = currentTimeElementListIndex / numTimeLinePerPage + 1;
        totalStnIdPage = stnElementList.size() / numTimeLinePerPage;
        if (stnElementList.size() % numTimeLinePerPage != 0)
            totalStnIdPage++;
        curStnIdPage = currentStnElementListIndex / numTimeLinePerPage + 1;
        totalSndPage = sndElementList.size() / numTimeLinePerPage;
        if (sndElementList.size() % numTimeLinePerPage != 0)
            totalSndPage++;
        curSndPage = currentSndElementListIndex / numTimeLinePerPage + 1;
    }

    /*
     * Return size of stnTimeSndTable Note that not all elements in this table
     * has sounding data loaded. Therefore, returned size may be much bigger
     * than the number of actual loaded sounding data frame.
     */
    public int getFrameCount() {
        return timeElementList.size() * sndElementList.size()
                * stnElementList.size();
    }

    /*
     * return "accumulated" index to stnTimeSndTable of current displayed
     * sounding data
     */
    public int getCurrentIndex() {
        int index = 0;
        if (currentSndElementListIndex >= 0 && currentStnElementListIndex >= 0
                && currentTimeElementListIndex >= 0) {
            index = currentSndElementListIndex + currentTimeElementListIndex
                    * sndElementList.size() + currentStnElementListIndex
                    * timeElementList.size() * sndElementList.size();
        }
        return index;
    }

    /*
     * set current displayed sounding data using the input "accumulated" index
     * (to stnTimeSndTable) return false: If input index is not valid or
     * sounding data is not loaded for input index return true: if set
     * successfully
     */
    public boolean setCurrentIndex(int index) {
        if (index < 0
                || index >= (timeElementList.size() * sndElementList.size() * stnElementList
                        .size())) {
            // System.out.println("invalid index="+index);
            return false;
        }
        int tempStni, tempSndi, tempTmi;
        tempStni = index / (timeElementList.size() * sndElementList.size());
        tempTmi = (index % (timeElementList.size() * sndElementList.size()))
                / sndElementList.size();
        tempSndi = (index % (timeElementList.size() * sndElementList.size()))
                % sndElementList.size();
        if (timeElementList.get(tempTmi).getActionState() == NsharpConstants.ActState.INACTIVE
                || stnElementList.get(tempStni).getActionState() == NsharpConstants.ActState.INACTIVE
                || sndElementList.get(tempSndi).getActionState() == NsharpConstants.ActState.INACTIVE
                || stnTimeSndTable.get(tempStni).get(tempTmi).get(tempSndi) == null) {
            // System.out.println("invalid element="+index);
            return false;
        }
        previousTimeLineStateListIndex = currentTimeElementListIndex;
        currentStnElementListIndex = tempStni;
        currentTimeElementListIndex = tempTmi;
        currentSndElementListIndex = tempSndi;
        setCurSndProfileProp();
        curTimeLinePage = currentTimeElementListIndex / numTimeLinePerPage + 1;
        curSndPage = currentSndElementListIndex / numTimeLinePerPage + 1;
        curStnIdPage = currentStnElementListIndex / numTimeLinePerPage + 1;
        setCurrentSoundingLayerInfo();
        resetData();
        refreshPane();
        return true;
    }
}
