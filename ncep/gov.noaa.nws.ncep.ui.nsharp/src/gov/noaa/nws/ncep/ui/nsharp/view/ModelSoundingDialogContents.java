/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.view.ModelSoundingDialogContents
 * 
 * This java class performs the NSHARP NsharpLoadDialog functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 01/2011	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.view;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingTimeLines;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigStore;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGraphProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGridInventory;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.SurfaceStationPointData;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpMapResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpDataHandling;
import gov.noaa.nws.ncep.viz.common.soundingQuery.NcSoundingQuery;

import java.io.File;
import java.sql.Timestamp;
import java.text.DateFormatSymbols;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.vividsolutions.jts.geom.Coordinate;

public class ModelSoundingDialogContents {
	private  Composite parent;
	private org.eclipse.swt.widgets.List modelTypeList=null, availableFileList=null, sndTimeList=null;
	//timeLineToFileMap maps time line (rangeStart time in sndTimeList) to available file (reftime in availableFileList)
	private Map<String, String> timeLineToFileMap = new HashMap<String, String>();
	//soundingLysLstMap maps "lat;lon timeline" string to its queried sounding layer list
	private Map<String, List<NcSoundingLayer>> soundingLysLstMap = new HashMap<String, List<NcSoundingLayer>> ();
	private Group  modelTypeGp, bottomGp, availableFileGp, sndTimeListGp,  topGp, locationMainGp;
	private Button timeBtn, latlonBtn, stationBtn, loadBtn;
	private Text locationText;
	private Label locationLbl;
	private boolean timeLimit = false;
	private NsharpLoadDialog ldDia;
	private Font newFont;
	private List<String> selectedFileList = new ArrayList<String>(); 
	private List<String> selectedTimeList = new ArrayList<String>(); 
	//private NcSoundingProfile.MdlSndType currentSndType = NcSoundingProfile.MdlSndType.NONE;
	private float lat, lon;
	private String stnStr="";
	private final String GOOD_LATLON_STR = " A good input looked like this:\n 38.95;-77.45 or 38.95,-77.45";
	private final String GOOD_STN_STR = " A good input looked like this:\n GAI or gai";
	String gribDecoderName = "grid";//NcSoundingQuery.NCGRIB_PLUGIN_NAME;
	private String selectedModel="";	
	//private DBType currentDb = DBType.NCGRIB;
	private static final String SND_TIMELINE_NOT_AVAIL_STRING = "No Sounding Time for Nsharp";
	
	public enum LocationType {
		LATLON, STATION
	}
	public enum DBType {
		GRIB, NCGRIB
	}
	private LocationType currentLocType=LocationType.LATLON;

	public LocationType getCurrentLocType() {
		return currentLocType;
	}

	public Text getLocationText() {
		return locationText;
	}
	
	
	public ModelSoundingDialogContents (Composite parent) {
		this.parent = parent;
		ldDia = NsharpLoadDialog.getAccess();
		newFont = ldDia.getNewFont();
		/*if( VizPerspectiveListener.getCurrentPerspectiveManager()!= null){
			if(VizPerspectiveListener.getCurrentPerspectiveManager().getPerspectiveId().equals(NmapCommon.NatlCntrsPerspectiveID))
				gribDecoderName = NcSoundingQuery.NCGRIB_PLUGIN_NAME;
			else
				gribDecoderName = NcSoundingQuery.GRIB_PLUGIN_NAME;
			
			
			//for testing
			//gribDecoderName = NcSoundingQuery.GRIB_PLUGIN_NAME;
			//System.out.println("perspective id = " + VizPerspectiveListener.getCurrentPerspectiveManager().getPerspectiveId());
		}*/
	}
	
	private void createMDLAvailableFileList() {
		if(sndTimeList!=null)
			sndTimeList.removeAll();
		if(availableFileList!=null)
			availableFileList.removeAll();
		HashMap<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
		rcMap.put( "info.datasetId", new RequestConstraint(selectedModel) );
		ldDia.startWaitCursor();
    	ArrayList<String> queryRsltsList1 = 
    		NsharpGridInventory.getInstance().searchInventory( 
    			rcMap, "dataTime");//.refTime" ); 
    	/*
    	 * Chin Note: with this query, the returned string has this format, "grid/ruc13/2012-01-17_16:00:00.0(6)xxxxx"
    	 * We will have to strip off "ncgrib/ruc13/" and ":00:00.0(6)xxxxx", also replace "_" with space, to get 
    	 * grid file name like this "2012-01-17 16".
    	 */
    	char fileSep =  File.pathSeparatorChar;
    	String header = "grid"+fileSep+  selectedModel +fileSep;
    	if( queryRsltsList1 != null && !queryRsltsList1.isEmpty() ) {
    		Collections.sort(queryRsltsList1, String.CASE_INSENSITIVE_ORDER);
    		Collections.reverse(queryRsltsList1);
    		
			for(String queryRslt : queryRsltsList1 ) {
				//System.out.println("ref time:"+queryRslt );
				queryRslt = queryRslt.substring(header.length());
				String refTime = queryRslt.substring(0, queryRslt.indexOf('_'));
				refTime = refTime + " "+ queryRslt.substring(queryRslt.indexOf('_')+1,queryRslt.indexOf(':'));
				//System.out.println("ret for disp="+refTime );
				//Chin: a same refTime may be returned more than once. 
				int index = availableFileList.indexOf(refTime);
				if(index  == -1) // index = -1 means it is not in the list
					availableFileList.add(refTime);
			}
		}
    	ldDia.stopWaitCursor();
		
	}
	/*
	private void createMDLAvailableFileListOld() {
		if(sndTimeList!=null)
			sndTimeList.removeAll();
		if(availableFileList!=null)
			availableFileList.removeAll();
		
		//query using NcSoundingQuery class to query
		NcSoundingTimeLines timeLines = NcSoundingQuery.mdlSoundingTimeLineQuery(selectedModel, gribDecoderName);
		if(timeLines!= null && timeLines.getTimeLines() != null){
			ldDia.startWaitCursor();
			for(Object timeLine : timeLines.getTimeLines()){
				Timestamp reftime = (Timestamp)timeLine;
				if(reftime != null){
					//need to format reftime to GMT time string.  Timestamp.toString produce a local time Not GMT time
					Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
					cal.setTimeInMillis(reftime.getTime());
					String gmtTimeStr = String.format("%1$tY-%1$tm-%1$td %1$tH",  cal);
					//System.out.println("GMT time " + gmtTimeStr);
					availableFileList.add(gmtTimeStr);
					
				}
				
			}
			ldDia.stopWaitCursor();
		}
		else
			System.out.println("SQL: query return null");	
	}*/
    private void createMDLSndTimeList(List<String> selectedFlLst) {
    	if(selectedFlLst.size() <=0 )
    		return;
    	if(sndTimeList!=null)
    		sndTimeList.removeAll();
    	if(timeLineToFileMap!=null)
    		timeLineToFileMap.clear();
    	int nameLen= Math.min(6, selectedModel.length());
    	String modelName= selectedModel.substring(0,nameLen);
		//query using NcSoundingQuery to query
    	DateFormatSymbols dfs= new DateFormatSymbols();
		String[] defaultDays = dfs.getShortWeekdays();
		Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		ldDia.startWaitCursor();
    	for(int i=0; i<  selectedFlLst.size(); i++){	
			String fl = selectedFlLst.get(i);
			long reftimeMs= NcSoundingQuery.convertRefTimeStr(fl);
			NcSoundingTimeLines timeLines = NcSoundingQuery.mdlSoundingRangeTimeLineQuery(selectedModel, fl, gribDecoderName);
			if(timeLines != null && timeLines.getTimeLines().length >0){
				for(Object obj : timeLines.getTimeLines()){
					Timestamp rangestart = (Timestamp)obj;
					//need to format rangestart to GMT time string.  Timestamp.toString produce a local time Not GMT time
					cal.setTimeInMillis(rangestart.getTime());
					long vHour = (cal.getTimeInMillis()- reftimeMs)/3600000;
					String dayOfWeek = defaultDays[cal.get(Calendar.DAY_OF_WEEK)];
					//String gmtTimeStr = String.format("%1$ty%1$tm%1$td/%1$tH%1$tMV%2$03d %3$s",  cal, vHour,modelName);
					String gmtTimeStr = String.format("%1$ty%1$tm%1$td/%1$tH(%4$s)V%2$03d %3$s",  cal, vHour,modelName,dayOfWeek);
					if(sndTimeList.indexOf(gmtTimeStr) != -1){
						// this indicate that gmtTimeStr is already in the sndTimeList, then we dont need to add it to list again.
						continue;
					}

					//System.out.println("GMT time " + gmtTimeStr);
					if(!timeLimit){
						sndTimeList.add(gmtTimeStr);	
						timeLineToFileMap.put(gmtTimeStr, fl);
					}
					else {
						int hour = cal.get(Calendar.HOUR_OF_DAY);
						if((hour == 0) || (hour == 12)){
							sndTimeList.add(gmtTimeStr);
							timeLineToFileMap.put(gmtTimeStr, fl);
						}
					}
				}
			}
    	}
    	if(sndTimeList!=null && sndTimeList.getItemCount()<=0){
    		sndTimeList.add(SND_TIMELINE_NOT_AVAIL_STRING);
    	}
    	ldDia.stopWaitCursor();
	}
    
    private void queryAndLoadData(boolean stnQuery) {
    	soundingLysLstMap.clear();
    	ldDia.startWaitCursor();
    	//Chin Note: Since NcGrib/Grib HDF5 data file is created based on a forecast time line, we can not query 
    	// more than one time line at one time as Edex server just could not support such query at one shot.
    	//This is not the case of PFC sounding (modelsounding db). It has all time lines of one forecast report
    	// saved in one file. Therefore, PFC query is much faster.
    	for(String timeLine: selectedTimeList){
    		// avail file, ie. its refTime
    		String selectedFileStr = timeLineToFileMap.get(timeLine); 
    		String rangeStartStr = NcSoundingQuery.convertSoundTimeDispStringToRangeStartTimeFormat(timeLine);
    		float[][] latLon = {{lat, lon}};
    		NcSoundingCube cube = NcSoundingQuery.mdlSoundingQueryByLatLon(selectedFileStr+":00:00",rangeStartStr, latLon, gribDecoderName,selectedModel, false, "-1");
    		if(cube != null && cube.getRtnStatus()== NcSoundingCube.QueryStatus.OK){
    			//System.out.println("mdlSoundingQueryByLatLon returnd ok");
        		
    			NcSoundingProfile sndPf = cube.getSoundingProfileList().get(0);

    			List<NcSoundingLayer> rtnSndLst = sndPf.getSoundingLyLst();
    			if(rtnSndLst != null &&  rtnSndLst.size() > 1){  
    				//Remove sounding layers that not used by NSHARP
    				//System.out.println("numbe of layer returned from query ="+ rtnSndLst.size());
    				rtnSndLst = NsharpDataHandling.organizeSoundingDataForShow(rtnSndLst, sndPf.getStationElevation());
    				//minimum rtnSndList size will be 2 (50 & 75 mb layers), but that is not enough
					// We need at least 2 regular layers for plotting
					if(rtnSndLst != null &&  rtnSndLst.size() > 4)
    				{ //after organized, if size is still good
    					if(!stnQuery){
    						soundingLysLstMap.put(lat+"/"+lon+" "+timeLine, rtnSndLst);
    						//System.out.println(lat+";"+lon+" "+timeLine);
    					}
    					else{
    						// replaced space to _ in stnStr.
    						String stnStrPacked = stnStr.replace(" ", "_");
    						soundingLysLstMap.put(stnStrPacked+" "+timeLine, rtnSndLst);
    						//System.out.println(stnStr+" "+timeLine);
    					}
    					continue;
    				}
    				
    			}
    			//code to this point means query result is not good
    			NsharpLoadDialog ldDia = NsharpLoadDialog.getAccess();
				if(!stnQuery){
					ldDia.setAndOpenMb("Sounding query with lat/lon ("+lat+"/"+lon+") at "+timeLine+": Returned\n But without vlaid data");
				}
				else{
					ldDia.setAndOpenMb("Sounding query with stn "+ stnStr+ "at lat/lon ("+lat+"/"+lon+") at "+timeLine+": Returned\n But without vlaid data");
				}
				//return;
    		}
    		else
    		{
    			if(!stnQuery){
    				System.out.println("mdlsoundingQueryByLatLon failed");
    				NsharpLoadDialog ldDia = NsharpLoadDialog.getAccess();
    				if(cube != null)
    					ldDia.setAndOpenMb("Sounding query with lat/lon ("+lat+"/"+lon+") at "+timeLine+": failed\nError status:"+ cube.getRtnStatus().toString());
    				else
    					ldDia.setAndOpenMb("Sounding query with lat/lon ("+lat+"/"+lon+") at "+timeLine+": failed\nError status: NULL returned");
    			}
    			else{
    				System.out.println("mdlsoundingQueryByStn failed");
    				NsharpLoadDialog ldDia = NsharpLoadDialog.getAccess();
    				if(cube != null)
    					ldDia.setAndOpenMb("Sounding query with stn "+ stnStr+ "at lat/lon ("+lat+"/"+lon+") at "+timeLine+": failed\nError status:"+ cube.getRtnStatus().toString());
    				else
    					ldDia.setAndOpenMb("Sounding query with stn "+ stnStr+ "at lat/lon ("+lat+"/"+lon+") at "+timeLine+": failed\nError status: NULL returned");
    			}
    			//return;
    		}
    	}
    	ldDia.stopWaitCursor();
    	NsharpEditor skewtEdt = NsharpEditor.createOrOpenEditor();
    	NsharpResourceHandler skewRsc = skewtEdt.getRscHandler();
    	//create station info structure
    	NsharpStationInfo stnInfo = new NsharpStationInfo();
    	stnInfo.setSndType(selectedModel);
    	stnInfo.setLatitude(lat);
    	stnInfo.setLongitude(lon);
    	stnInfo.setStnId(stnStr);
    	skewRsc.addRsc(soundingLysLstMap, stnInfo);
		skewRsc.setSoundingType(selectedModel);
		NsharpEditor.bringEditorToTop();
    }
    private void createModelTypeList(){
    	if(modelTypeList!=null)
    		modelTypeList.removeAll();
    	if(sndTimeList!=null)
    		sndTimeList.removeAll();
    	if(availableFileList!=null)
    		availableFileList.removeAll();
    	ldDia.startWaitCursor();
    	List<String> cfgList=null;
    	NsharpConfigManager configMgr = NsharpConfigManager.getInstance();
		NsharpConfigStore configStore = configMgr.retrieveNsharpConfigStoreFromFs();
		NsharpGraphProperty graphConfigProperty = configStore.getGraphProperty();
    	cfgList = graphConfigProperty.getGribModelTypeList();
		HashMap<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
		rcMap.put( "pluginName", new RequestConstraint("grid") );

    	ArrayList<String> queryRsltsList = 
    		NsharpGridInventory.getInstance().searchInventory( 
    			rcMap, "info.datasetId" );
    	
    	/*
    	 * Chin Note: with this query, the returned string has this format, "ncgrib/gfsP5"
    	 * We will have to strip off "ncgrib/" to get model name like this "gfsP5".
    	 */

    	if( queryRsltsList != null && !queryRsltsList.isEmpty() ) {
    		Collections.sort(queryRsltsList, String.CASE_INSENSITIVE_ORDER);
			for(String queryRslt : queryRsltsList ) {
				System.out.println("model name:"+queryRslt );
				String modelName = queryRslt.substring( "grid/".length() );
				if(cfgList!=null && cfgList.size()>0){
					if(cfgList.contains(modelName))
						modelTypeList.add(modelName);
				}
				else
					modelTypeList.add(modelName);
			}
		}
		ldDia.stopWaitCursor();
		
    }
    private void handleAvailFileListSelection(){
    	String selectedFile=null;	
    	if (availableFileList.getSelectionCount() > 0 ) {
			selectedFileList.clear();
			for(int i=0; i < availableFileList.getSelectionCount(); i++) {
				selectedFile = availableFileList.getSelection()[i];
				//System.out.println("selected sounding file is " + selectedFile);
				selectedFileList.add(selectedFile);
			}	
			createMDLSndTimeList(selectedFileList);
		}
    }
    private void handleSndTimeSelection(){
    	String selectedSndTime=null;
    	if (sndTimeList.getSelectionCount() > 0 && sndTimeList.getSelection()[0].equals(SND_TIMELINE_NOT_AVAIL_STRING)== false) {
			
			selectedTimeList.clear();
			for(int i=0; i < sndTimeList.getSelectionCount(); i++) {
				selectedSndTime = sndTimeList.getSelection()[i];
				//System.out.println("selected sounding time is " + selectedSndTime);
				selectedTimeList.add(selectedSndTime);
			}
			NsharpMapResource.bringMapEditorToTop();
		}
    }
	public void createMdlDialogContents(){
		topGp = new Group(parent,SWT.SHADOW_ETCHED_IN);
		topGp.setLayout( new GridLayout( 2, false ) );
		selectedModel = ldDia.getActiveMdlSndMdlType();
		ldDia.createSndTypeList(topGp);
		
		modelTypeGp = new Group(topGp, SWT.SHADOW_ETCHED_IN);
		modelTypeGp.setText("Model Type");
		modelTypeGp.setFont(newFont);
		modelTypeList = new org.eclipse.swt.widgets.List(modelTypeGp, SWT.BORDER  | SWT.V_SCROLL  );
		modelTypeList.setBounds(modelTypeGp.getBounds().x, modelTypeGp.getBounds().y + NsharpConstants.labelGap , NsharpConstants.filelistWidth, NsharpConstants.listHeight );
		//query to get and add available sounding models from DB
		modelTypeList.setFont(newFont);
		createModelTypeList();
		
		//create a selection listener to handle user's selection on list		
		modelTypeList.addListener ( SWT.Selection, new Listener () {
			public void handleEvent (Event e) {   			
				if (modelTypeList.getSelectionCount() > 0 ) {
					selectedModel = modelTypeList.getSelection()[0];
					ldDia.setActiveMdlSndMdlType(selectedModel);
					//System.out.println("selected sounding model is " + selectedModel);
					createMDLAvailableFileList();
				}
			}
		} );
		
		
		availableFileGp = new Group(topGp,SWT.SHADOW_ETCHED_IN);
		availableFileGp.setText("Available Grid files:");
		availableFileGp.setFont(newFont);
		availableFileList = new org.eclipse.swt.widgets.List(availableFileGp, SWT.BORDER  | SWT.MULTI| SWT.V_SCROLL  );
		availableFileList.setBounds(availableFileGp.getBounds().x, availableFileGp.getBounds().y + NsharpConstants.labelGap , NsharpConstants.filelistWidth, NsharpConstants.listHeight);//*32/5 );
		availableFileList.setFont(newFont);
		//create a selection listener to handle user's selection on list		
		availableFileList.addListener ( SWT.Selection, new Listener () {
			public void handleEvent (Event e) {   			
				handleAvailFileListSelection();
			}
		} );
		
		 //create Sounding Times widget list 
		sndTimeListGp = new Group(topGp,SWT.SHADOW_ETCHED_IN);
		sndTimeListGp.setText("Sounding Times:");
		sndTimeListGp.setFont(newFont);
		sndTimeList = new org.eclipse.swt.widgets.List(sndTimeListGp, SWT.BORDER  | SWT.MULTI| SWT.V_SCROLL  );
		sndTimeList.removeAll();
		sndTimeList.setFont(newFont);
		sndTimeList.setBounds(sndTimeListGp.getBounds().x, sndTimeListGp.getBounds().y + NsharpConstants.labelGap, NsharpConstants.listWidth, NsharpConstants.listHeight );//*32/5);
		sndTimeList.addListener ( SWT.Selection, new Listener () {
			public void handleEvent (Event e) {   			
				handleSndTimeSelection();
    		}
    	});
		timeBtn = new Button(topGp, SWT.CHECK | SWT.BORDER);
		timeBtn.setText("00Z and 12Z only");
		timeBtn.setEnabled( true );
		timeBtn.setFont(newFont);
		timeBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(timeLimit)
					timeLimit = false;
				else
					timeLimit = true;
				
				//refresh sounding list if file type is selected already
				if(selectedModel!=null && selectedFileList.size() > 0){
					createMDLSndTimeList(selectedFileList);
				}

			}          		            	 	
		} );  
		locationMainGp= new Group(parent,SWT.SHADOW_ETCHED_IN);
		locationMainGp.setLayout( new GridLayout( 5, false ) );
		locationMainGp.setText("Location");
		locationMainGp.setFont(newFont);
		latlonBtn = new Button(locationMainGp, SWT.RADIO | SWT.BORDER);
		latlonBtn.setText("Lat/Lon");
		latlonBtn.setFont(newFont);
		latlonBtn.setEnabled(true);
		latlonBtn.setSelection(true);
		latlonBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				currentLocType = LocationType.LATLON;
				locationText.setText("");
			}          		            	 	
		} ); 
		stationBtn = new Button(locationMainGp, SWT.RADIO | SWT.BORDER);
		stationBtn.setText("Station");
		stationBtn.setEnabled(true);
		stationBtn.setFont(newFont);
		stationBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				currentLocType = LocationType.STATION;
				locationText.setText("");
			}          		            	 	
		} ); 
		//locationInputGp = new Group(locationMainGp,SWT.SHADOW_ETCHED_IN);
		locationLbl = new Label(locationMainGp, SWT.NONE | SWT.BORDER);
		//locationLbl.setBounds(latlonBtn.getBounds().x, latlonBtn.getBounds().y + latlonBtn.getBounds().height+ NsharpConstants.btnGapY, bottomGp.getBounds().width/2,NsharpConstants.btnHeight);
		locationLbl.setText("Location:");
		locationLbl.setFont(newFont);
		locationText = new Text(locationMainGp, SWT.BORDER | SWT.SINGLE);
		GridData data1 = new GridData (SWT.FILL,SWT.FILL, true, true);
		locationText.setLayoutData (data1);
		//locationText.setBounds(stationBtn.getBounds().x, locationLbl.getBounds().y,450,NsharpConstants.btnHeight);
		locationText.setTextLimit(15);
		locationText.setFont(newFont);
		locationText.addListener (SWT.Verify, new Listener () {
			public void handleEvent (Event e) {
				String userInputStr = e.text;
				if(userInputStr.length()>0){

					if(currentLocType == LocationType.LATLON){
						//to make sure user enter digits and separated by ";" or ","only, if lat/lon is used
						//System.out.println("user input str" + userInputStr);
						if(userInputStr.length() ==1){
							char  inputChar = userInputStr.charAt(0);
							if (!('0' <= inputChar && inputChar <= '9') && inputChar != ';' && inputChar != ',' && inputChar != '-'&& inputChar != '.') {
								e.doit = false;
								return;
							}
						}
					} else {
						//do nothing when station type

					}
				}
			}
		});
		
		loadBtn = new Button(locationMainGp, SWT.PUSH);
		loadBtn.setText("Load ");
		loadBtn.setFont(newFont);
		loadBtn.setEnabled( true );
		loadBtn.setBounds(locationMainGp.getBounds().x+ NsharpConstants.btnGapX, locationLbl.getBounds().y + locationLbl.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);	
		loadBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {  
				NsharpLoadDialog ldDia = NsharpLoadDialog.getAccess();
				if(selectedTimeList != null && selectedTimeList.size() == 0){
					ldDia.setAndOpenMb("Time line(s) is not selected!\n Can not load data!");
					return;
				}
				String textStr = locationText.getText();
				if((textStr != null) && !(textStr.isEmpty())){
					//textStr = textStr.trim();
					if(currentLocType == LocationType.LATLON){
						//to make sure user enter digits and separated by ";" or ","only, if lat/lon is used
						int dividerIndex = textStr.indexOf(';');
						boolean indexFound = false;
						if(dividerIndex != -1 )
							indexFound = true;				
						if(indexFound == false){
							dividerIndex = textStr.indexOf(',');
							if(dividerIndex != -1 )
								indexFound = true;
						}
						if(indexFound == true) {
							try{
								lat = Float.parseFloat(textStr.substring(0, dividerIndex));
								lon = Float.parseFloat(textStr.substring(dividerIndex+1));
								if(lat >90 || lat < -90 || lon >180 || lon <-180){
									//System.out.println("bad lat/lon entered =" + textStr);
									ldDia.setAndOpenMb("lat/lon out of range ("+textStr+") entered!\n"+GOOD_LATLON_STR);
									locationText.setText("");
									return;
								}
								//System.out.println("user enter lat " + lat+ " lon " + lon);
								queryAndLoadData(false);

							} catch (Exception e) {
								System.out.println("queryAndLoadData failed at " + textStr);
								//ldDia.setAndOpenMb("Bad lat/lon ("+textStr+") entered!\n"+GOOD_LATLON_STR);
								//locationText.setText("");
								return;
							}
							
						}
						else {
							//System.out.println("2 bad lat/lon entered =" + textStr);
							ldDia.setAndOpenMb("Bad lat/lon ("+textStr+") entered!\n"+GOOD_LATLON_STR);
							locationText.setText("");
							return;
						}
					} else if(currentLocType == LocationType.STATION){
						//query station lat /lon
						textStr = textStr.trim(); // user may start with a space before enter a station id
						stnStr = textStr.toUpperCase(Locale.getDefault()); 
						Coordinate co = SurfaceStationPointData.getStnCoordinate(stnStr);
						lat = (float) co.y;
						lon = (float) co.x;
						//System.out.println("user enter station ="+ stnStr+" length="+ stnStr.length()+" lat " + lat+ " lon " + lon);
						if(lat == SurfaceStationPointData.DEFAULT_LATLON){
							//System.out.println("bad stn id entered =" + textStr);
							ldDia.setAndOpenMb("Bad station id ("+textStr+") entered!\n"+GOOD_STN_STR);
							locationText.setText("");
							return;
						}
						queryAndLoadData(true);
					}
					
					//ldDia.close();
				}
			}          		            	 	
		} ); 
		
		if(selectedModel != null && selectedModel.equals("")== false){
			String[] selectedModelArray = {selectedModel};
			modelTypeList.setSelection(selectedModelArray);
			createMDLAvailableFileList();
			selectedFileList = ldDia.getMdlSelectedFileList();
			Object[] selFileObjectArray = selectedFileList.toArray();
			String[] selFileStringArray = Arrays.copyOf(selFileObjectArray, selFileObjectArray.length, String[].class);
			availableFileList.setSelection(selFileStringArray);
			handleAvailFileListSelection();
			
			selectedTimeList = ldDia.getMdlSelectedTimeList();
			Object[] selTimeObjectArray = selectedTimeList.toArray();
			String[] selTimeStringArray = Arrays.copyOf(selTimeObjectArray, selTimeObjectArray.length, String[].class);
			sndTimeList.setSelection(selTimeStringArray);
			handleSndTimeSelection();
			
				
		}
	}
	
	public void cleanup(){
		/*
		if(gfsBtn != null){
			gfsBtn.removeListener(SWT.MouseUp, gfsBtn.getListeners(SWT.MouseUp)[0]);
			gfsBtn.dispose();
			gfsBtn = null;
		}
		if(namBtn != null){
			namBtn.removeListener(SWT.MouseUp, namBtn.getListeners(SWT.MouseUp)[0]);
			namBtn.dispose();
			namBtn = null;
		}		
		if(ngmBtn != null){
			ngmBtn.removeListener(SWT.MouseUp, ngmBtn.getListeners(SWT.MouseUp)[0]);
			ngmBtn.dispose();
			ngmBtn = null;
		}		
		if(ruc2Btn != null){
			ruc2Btn.removeListener(SWT.MouseUp, ruc2Btn.getListeners(SWT.MouseUp)[0]);
			ruc2Btn.dispose();
			ruc2Btn = null;
		}		
		if(ukmetBtn != null){
			ukmetBtn.removeListener(SWT.MouseUp, ukmetBtn.getListeners(SWT.MouseUp)[0]);
			ukmetBtn.dispose();
			ukmetBtn = null;	
		}	*/	
		if(modelTypeList!=null){
			if(modelTypeList.getListeners(SWT.Selection).length >0)
				modelTypeList.removeListener(SWT.Selection, modelTypeList.getListeners(SWT.Selection)[0]);
			modelTypeList.dispose();
			modelTypeList = null;
		}
		if(modelTypeGp!= null){
			modelTypeGp.dispose();
			modelTypeGp = null;
		}
		if(timeBtn != null){
		timeBtn.removeListener(SWT.MouseUp, timeBtn.getListeners(SWT.MouseUp)[0]);
		timeBtn.dispose();
		timeBtn = null;
		}
	
		NsharpLoadDialog ldDia = NsharpLoadDialog.getAccess();
		ldDia.cleanSndTypeList();
		
		
		
		
		if(availableFileList!= null){
			availableFileList.removeListener(SWT.Selection, availableFileList.getListeners(SWT.Selection)[0]);
			availableFileList.dispose();
			availableFileList = null;
		}
		
		if(availableFileGp!= null){
			availableFileGp.dispose();
			availableFileGp = null;
		}
		if(sndTimeList!= null){
			sndTimeList.removeListener(SWT.Selection, sndTimeList.getListeners(SWT.Selection)[0]);
			sndTimeList.dispose();
			sndTimeList = null;
		}
		if(sndTimeListGp!= null){
			sndTimeListGp.dispose();
			sndTimeListGp = null;
		}
		if(bottomGp!= null){
			bottomGp.dispose();
			bottomGp = null;
		}
		if(topGp!= null){
			topGp.dispose();
			topGp = null;
		}

		if(loadBtn != null){
			loadBtn.removeListener(SWT.MouseUp, loadBtn.getListeners(SWT.MouseUp)[0]);
			loadBtn.dispose();
			loadBtn = null;
		}
		if(stationBtn != null){
			stationBtn.removeListener(SWT.MouseUp, stationBtn.getListeners(SWT.MouseUp)[0]);
			stationBtn.dispose();
			stationBtn = null;
		}
		if(latlonBtn != null){
			latlonBtn.removeListener(SWT.MouseUp, latlonBtn.getListeners(SWT.MouseUp)[0]);
			latlonBtn.dispose();
			latlonBtn = null;
		}
		if(locationText != null){
			locationText.removeListener(SWT.Verify, locationText.getListeners(SWT.Verify)[0]);
			locationText.dispose();
			locationText = null;
		}
		
		if(locationLbl!= null){
			locationLbl.dispose();
			locationLbl = null;
		}
		if(locationMainGp!= null){
			locationMainGp.dispose();
			locationMainGp = null;
		}
		/*if(newTabBtn != null){
			newTabBtn.removeListener(SWT.MouseUp, newTabBtn.getListeners(SWT.MouseUp)[0]);
			newTabBtn.dispose();
			newTabBtn = null;
		}*/

	}
}
