/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.view.PfcSoundingDialogContents
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

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfo;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfoCollection;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingTimeLines;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpMapResource;
import gov.noaa.nws.ncep.viz.common.soundingQuery.NcSoundingQuery;

import java.sql.Timestamp;
import java.text.DateFormatSymbols;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;

public class PfcSoundingDialogContents {
	private  Composite parent;
	private org.eclipse.swt.widgets.List availablefileList, sndTimeList;
	private Group  topGp, fileTypeGp, bottomGp, availableFileGp, sndTimeListGp;
	private Button namBtn, gfsBtn, timeBtn;
	private boolean timeLimit = false;
	private List<String> selectedFileList = new ArrayList<String>(); 
	private List<String> selectedTimeList = new ArrayList<String>(); 
	private List<NsharpStationInfo> stnPoints = new ArrayList<NsharpStationInfo>();
	private NcSoundingProfile.PfcSndType currentSndType = NcSoundingProfile.PfcSndType.NONE;
	private NsharpLoadDialog ldDia;
	private Font newFont;
	public NcSoundingProfile.PfcSndType getCurrentSndType() {
		return currentSndType;
	}
	public PfcSoundingDialogContents() {
	}
	public PfcSoundingDialogContents (Composite parent) {
		this.parent = parent;
		ldDia = NsharpLoadDialog.getAccess();
		newFont = ldDia.getNewFont();
	}
	private void createPFCAvailableFileList() {
		sndTimeList.removeAll();;
		availablefileList.removeAll();
		//query using NcSoundingQuery class to query
		NcSoundingTimeLines timeLines = NcSoundingQuery.soundingTimeLineQuery(currentSndType.toString());
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
					availablefileList.add(gmtTimeStr);
					
				}
				
			}
			ldDia.stopWaitCursor();
		}
		else
			System.out.println("SQL: query return null");	
	}
    private void createPFCSndTimeList(List<String> selectedFlLst) {
    	if(selectedFlLst.size() <=0 )
    		return;
		//currentDBTblName = MODELSOUNDING_TBL_NAME;
		sndTimeList.removeAll();
		//query using NcSoundingQuery to query
		ldDia.startWaitCursor();
		String sndStr = currentSndType.toString();
		int endIndex= Math.min(3, sndStr.length());
		String dispSndStr = sndStr.substring(0, endIndex);
		DateFormatSymbols dfs= new DateFormatSymbols();
		String[] defaultDays = dfs.getShortWeekdays();
		Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		
    	for(int i=0; i<  selectedFlLst.size(); i++){	
			String fl = selectedFlLst.get(i);
			long reftimeMs= NcSoundingQuery.convertRefTimeStr(fl);
			//System.out.println("reftime="+fl + " in ms="+reftimeMs);
			NcSoundingTimeLines timeLines = NcSoundingQuery.soundingRangeTimeLineQuery(sndStr, fl);
			if(timeLines != null && timeLines.getTimeLines().length >0) {
				for(Object obj : timeLines.getTimeLines()){
					Timestamp rangestart = (Timestamp)obj;
					
						//need to format rangestart to GMT time string.  Timestamp.toString produce a local time Not GMT time
						cal.setTimeInMillis(rangestart.getTime());
						long vHour = (cal.getTimeInMillis()- reftimeMs)/3600000;
						String dayOfWeek = defaultDays[cal.get(Calendar.DAY_OF_WEEK)];
						//String gmtTimeStr = String.format("%1$ty%1$tm%1$td/%1$tH%1$tMV%2$03d %3$s",  cal, vHour,dispSndStr);
						String gmtTimeStr = String.format("%1$ty%1$tm%1$td/%1$tH(%4$s)V%2$03d %3$s",  cal, vHour,dispSndStr,dayOfWeek);
						if(sndTimeList.indexOf(gmtTimeStr) != -1){
							// this indicate that gmtTimeStr is already in the sndTimeList, then we dont need to add it to list again.
							continue;
						}
						
						//System.out.println("GMT time " + gmtTimeStr);
						if(!timeLimit)
							sndTimeList.add(gmtTimeStr);
						else {
							int hour = cal.get(Calendar.HOUR_OF_DAY);
							if((hour == 0) || (hour == 12))
								sndTimeList.add(gmtTimeStr);
						}
					
				}
			}
    	}
    	ldDia.stopWaitCursor();
	}
    private void handleAvailFileListSelection(){
    	String selectedFile=null;	
    	if (availablefileList.getSelectionCount() > 0 ) {
			selectedFileList.clear();
			for(int i=0; i < availablefileList.getSelectionCount(); i++) {
				selectedFile = availablefileList.getSelection()[i];
				//System.out.println("selected sounding file is " + selectedFile);
				selectedFileList.add(selectedFile);
			}	
			ldDia.setPfcSelectedFileList(selectedFileList);
			createPFCSndTimeList(selectedFileList);
		}
    }
    private void handleSndTimeSelection(){
		String selectedSndTime=null;	
    	if (sndTimeList.getSelectionCount() > 0 ) {
			NsharpMapResource nsharpMapResource = NsharpMapResource.getOrCreateNsharpMapResource();//NsharpLoadDialog.getAccess().getNsharpMapResource();
			nsharpMapResource.setPoints(null);
			selectedTimeList.clear();
			ldDia.startWaitCursor();
			List<String> queriedTimeList = new ArrayList<String>(); 
			for(int i=0; i < sndTimeList.getSelectionCount(); i++) {
				selectedSndTime = sndTimeList.getSelection()[i];
				selectedTimeList.add(selectedSndTime);
				int endIndex = selectedSndTime.indexOf(" ");
				String querySndTime = selectedSndTime.substring(0, endIndex);
				//System.out.println("selected sounding time is " + selectedSndTime);
				//refTimeStr is same as "PFC file" name in Load dialog display 
				String refTimeStr=NcSoundingQuery.convertSoundTimeDispStringToForecastTime(querySndTime);
				//while rangeStartStr is same as "sounding Times
				String rangeStartStr = NcSoundingQuery.convertSoundTimeDispStringToRangeStartTimeFormat(querySndTime);
				if(queriedTimeList.contains(refTimeStr)== true){
					addStnPtWithoutQuery(refTimeStr,rangeStartStr,querySndTime);
				}
				else {
					queriedTimeList.add(refTimeStr);
					queryAndMarkStn(refTimeStr,rangeStartStr,querySndTime);
				}
			}
			
			ldDia.stopWaitCursor();
			
			nsharpMapResource.setPoints(stnPoints);
			NsharpMapResource.bringMapEditorToTop();
		}
    }
	public void createPfcDialogContents(){
		topGp = new Group(parent,SWT.SHADOW_ETCHED_IN);
		topGp.setLayout( new GridLayout( 2, false ) );
		
		currentSndType = ldDia.getActivePfcSndType();
		ldDia.createSndTypeList(topGp);
		
		fileTypeGp = new Group(topGp, SWT.SHADOW_ETCHED_IN);
		fileTypeGp.setText("File Type");
		fileTypeGp.setFont(newFont);
		namBtn = new Button(fileTypeGp, SWT.RADIO | SWT.BORDER);
		namBtn.setText("NAMSND");
		namBtn.setEnabled( true );
		namBtn.setBounds(fileTypeGp.getBounds().x+ NsharpConstants.btnGapX, fileTypeGp.getBounds().y + NsharpConstants.labelGap, NsharpConstants.btnWidth,NsharpConstants.btnHeight);
		namBtn.setFont(newFont);
		namBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				currentSndType = NcSoundingProfile.PfcSndType.NAMSND;
				createPFCAvailableFileList();
				ldDia.setActivePfcSndType(currentSndType);
			}          		            	 	
		} );  
		gfsBtn = new Button(fileTypeGp, SWT.RADIO | SWT.BORDER);
		gfsBtn.setText("GFSSND");
		gfsBtn.setEnabled( true );
		gfsBtn.setBounds(fileTypeGp.getBounds().x+ NsharpConstants.btnGapX, namBtn.getBounds().y + namBtn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);
		gfsBtn.setFont(newFont);
		gfsBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				currentSndType = NcSoundingProfile.PfcSndType.GFSSND;
				createPFCAvailableFileList();
				ldDia.setActivePfcSndType(currentSndType);
			}          		            	 	
		} );  
		
		
		timeBtn = new Button(parent, SWT.CHECK | SWT.BORDER);
		timeBtn.setText("00Z and 12Z only");
		timeBtn.setEnabled( true );
		//timeBtn.setBounds(fileTypeGp.getBounds().x+ NsharpConstants.btnGapX, browseBtn.getBounds().y + browseBtn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);
		timeBtn.setFont(newFont);
		timeBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(timeLimit)
					timeLimit = false;
				else
					timeLimit = true;
				
				//refresh sounding list if file type is selected already
				if(!currentSndType.equals("NA") && selectedFileList.size() > 0 ){
					createPFCSndTimeList(selectedFileList);
				}

			}          		            	 	
		} );  
		
		bottomGp = new Group(parent,SWT.SHADOW_ETCHED_IN);
		bottomGp.setLayout( new GridLayout( 2, false ) );
		
		availableFileGp = new Group(bottomGp,SWT.SHADOW_ETCHED_IN);
		availableFileGp.setText("Available PFC files:");
		availableFileGp.setFont(newFont);
		availablefileList = new org.eclipse.swt.widgets.List(availableFileGp, SWT.BORDER  | SWT.MULTI| SWT.V_SCROLL  );
		availablefileList.setBounds(availableFileGp.getBounds().x, availableFileGp.getBounds().y + NsharpConstants.labelGap , NsharpConstants.filelistWidth, NsharpConstants.listHeight *36/5);
		//create a selection listener to handle user's selection on list
		availablefileList.setFont(newFont);
		availablefileList.addListener ( SWT.Selection, new Listener () {
			public void handleEvent (Event e) {   	
				handleAvailFileListSelection();
			}
		} );
		
		 //create Sounding Times widget list 
		sndTimeListGp = new Group(bottomGp,SWT.SHADOW_ETCHED_IN);
		sndTimeListGp.setText("Sounding Times:");
		sndTimeListGp.setFont(newFont);
		sndTimeList = new org.eclipse.swt.widgets.List(sndTimeListGp, SWT.BORDER  | SWT.MULTI| SWT.V_SCROLL  );
		sndTimeList.removeAll();
		sndTimeList.setFont(newFont);
		sndTimeList.setBounds(sndTimeListGp.getBounds().x, sndTimeListGp.getBounds().y + NsharpConstants.labelGap, NsharpConstants.listWidth, NsharpConstants.listHeight*36/5 );
		sndTimeList.addListener ( SWT.Selection, new Listener () {
		//	private String selectedSndTime=null;
    		public void handleEvent (Event e) {  
    			handleSndTimeSelection();
    		}
    		
    	});
		if(currentSndType==NcSoundingProfile.PfcSndType.GFSSND || currentSndType==NcSoundingProfile.PfcSndType.NAMSND){
			if(currentSndType==NcSoundingProfile.PfcSndType.GFSSND)
				gfsBtn.setSelection(true);
			else
				namBtn.setSelection(true);
			createPFCAvailableFileList();
			selectedFileList = ldDia.getPfcSelectedFileList();
			Object[] selFileObjectArray = selectedFileList.toArray();
			String[] selFileStringArray = Arrays.copyOf(selFileObjectArray, selFileObjectArray.length, String[].class);
			availablefileList.setSelection(selFileStringArray);
			handleAvailFileListSelection();
			
			selectedTimeList = ldDia.getPfcSelectedTimeList();
			Object[] selTimeObjectArray = selectedTimeList.toArray();
			String[] selTimeStringArray = Arrays.copyOf(selTimeObjectArray, selTimeObjectArray.length, String[].class);
			sndTimeList.setSelection(selTimeStringArray);
			handleSndTimeSelection();
			
				
		}
	}
	
	private void addStnPtWithoutQuery(String refTimeStr,String rangeStartStr, String selectedSndTime) {
		long reftimeMs= NcSoundingQuery.convertRefTimeStr(refTimeStr);
		Timestamp refTime = new Timestamp(reftimeMs);
		for(NsharpStationInfo stn: stnPoints){
			if(refTime.equals(stn.getReftime())== true){
				long rangetimeMs= NcSoundingQuery.convertRefTimeStr(rangeStartStr);
				Timestamp rangeStartTime = new Timestamp(rangetimeMs);
				NsharpStationInfo.timeLineSpecific timeLinsSpc =  stn.new timeLineSpecific();
				String sndTypeStr = currentSndType.toString();
				int endIndex= Math.min(4, sndTypeStr.length());
				String dispInfo = stn.getStnId()+ " " + selectedSndTime+" "+sndTypeStr.substring(0,endIndex);
				timeLinsSpc.setDisplayInfo(dispInfo);
				timeLinsSpc.setTiemLine(rangeStartTime);
				stn.addToTimeLineSpList(timeLinsSpc);
			}
		}		
		//System.out.println("addStnPtWithoutQuery stn num ="+ stnPoints.size()+ " for pfc refTime(file) "+refTimeStr);
	} 
	private void queryAndMarkStn(String refTimeStr,String rangeStartStr, String selectedSndTime) {
		//use NcSoundingQuery to query stn info
		String sndTypeStr = currentSndType.toString();
		NcSoundingStnInfoCollection sndStnInfoCol = NcSoundingQuery.soundingStnInfoQuery(sndTypeStr,rangeStartStr, refTimeStr);
		if(sndStnInfoCol != null && sndStnInfoCol.getStationInfo() != null){

			NcSoundingStnInfo[] stnInfoAry = sndStnInfoCol.getStationInfo();
			//System.out.println("queryAndMarkStn stn num ="+ stnInfoAry.length+ " for pfc refTime(file) "+refTimeStr);
			for(int i=0; i < stnInfoAry.length; i++){
				NcSoundingStnInfo stnInfo = stnInfoAry[i];
				NsharpStationInfo stn = new NsharpStationInfo();
				NsharpStationInfo.timeLineSpecific timeLinsSpc =  stn.new timeLineSpecific();
				
				int endIndex= Math.min(4, sndTypeStr.length());
				String packedStnIdStr= stnInfo.getStnId().replace(" ", "_");
				String dispInfo = packedStnIdStr + " " + selectedSndTime+" "+sndTypeStr.substring(0,endIndex);
				timeLinsSpc.setDisplayInfo(dispInfo);
				timeLinsSpc.setTiemLine(stnInfo.getRangeStartTime());
				stn.addToTimeLineSpList(timeLinsSpc);
				stn.setLongitude(stnInfo.getStationLongitude());
				stn.setLatitude(stnInfo.getStationLatitude());
				stn.setReftime(stnInfo.getSynopTime());
				stn.setStnId(stnInfo.getStnId());
				stn.setSndType(sndTypeStr);
				//if(i <10)
				//	System.out.println( "disP="+dispInfo+" refT= "+stnInfo.getSynopTime()+ " rangSt="+stnInfo.getRangeStartTime());
				stnPoints.add(stn);
			}
			
			
		}
	}
	public void cleanup(){
		if(namBtn != null&& namBtn.isDisposed()== false){
			namBtn.removeListener(SWT.MouseUp, namBtn.getListeners(SWT.MouseUp)[0]);
			namBtn.dispose();
			namBtn = null;
		}
		if(gfsBtn != null){
			gfsBtn.removeListener(SWT.MouseUp, gfsBtn.getListeners(SWT.MouseUp)[0]);
			gfsBtn.dispose();
			gfsBtn = null;
		}		
		/*if(ruc2Btn != null){
			ruc2Btn.removeListener(SWT.MouseUp, ruc2Btn.getListeners(SWT.MouseUp)[0]);
			ruc2Btn.dispose();
			ruc2Btn = null;
		}		
		if(rucpBtn != null){
			rucpBtn.removeListener(SWT.MouseUp, rucpBtn.getListeners(SWT.MouseUp)[0]);
			rucpBtn.dispose();
			rucpBtn = null;
		}		
		if(browseBtn != null){
			browseBtn.removeListener(SWT.MouseUp, browseBtn.getListeners(SWT.MouseUp)[0]);
			browseBtn.dispose();
			browseBtn = null;
			
		}	*/
		NsharpLoadDialog ldDia = NsharpLoadDialog.getAccess();
		ldDia.cleanSndTypeList();
		if(topGp!= null){
			topGp.dispose();
			topGp = null;
		}
		
		if(timeBtn != null){
			timeBtn.removeListener(SWT.MouseUp, timeBtn.getListeners(SWT.MouseUp)[0]);
			timeBtn.dispose();
			timeBtn = null;
		}
		
		if(fileTypeGp!= null){
			fileTypeGp.dispose();
			fileTypeGp = null;
		}
		
		if(availablefileList!= null){
			availablefileList.removeListener(SWT.Selection, availablefileList.getListeners(SWT.Selection)[0]);
			availablefileList.dispose();
			availablefileList = null;
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
		/*if(newTabBtn != null){
			newTabBtn.removeListener(SWT.MouseUp, newTabBtn.getListeners(SWT.MouseUp)[0]);
			newTabBtn.dispose();
			newTabBtn = null;
		}*/
	}
}
