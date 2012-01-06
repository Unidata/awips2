/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.menu.PfcSoundingDialogContents
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
package gov.noaa.nws.ncep.ui.nsharp.menu;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfo;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfoCollection;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingTimeLines;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.maprsc.NsharpMapResource;
import gov.noaa.nws.ncep.viz.common.soundingQuery.NcSoundingQuery;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
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
	private Button namBtn, gfsBtn, ruc2Btn, rucpBtn, browseBtn,timeBtn;
	private boolean timeLimit = false;
	private List<String> selectedFileList = new ArrayList<String>(); 
	private List<String> selectedTimeList = new ArrayList<String>(); 
	private NcSoundingProfile.PfcSndType currentSndType = NcSoundingProfile.PfcSndType.NONE;

	public NcSoundingProfile.PfcSndType getCurrentSndType() {
		return currentSndType;
	}
	public PfcSoundingDialogContents() {
	}
	public PfcSoundingDialogContents (Composite parent) {
		this.parent = parent;
		
	}
	private void createPFCAvailableFileList() {
		sndTimeList.removeAll();;
		availablefileList.removeAll();
		//query using NcSoundingQuery class to query
		NcSoundingTimeLines timeLines = NcSoundingQuery.soundingTimeLineQuery(currentSndType.toString());
		if(timeLines!= null && timeLines.getTimeLines() != null){
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
    	for(int i=0; i<  selectedFlLst.size(); i++){	
			String fl = selectedFlLst.get(i);
			NcSoundingTimeLines timeLines = NcSoundingQuery.soundingRangeTimeLineQuery(currentSndType.toString(), fl);
			if(timeLines != null && timeLines.getTimeLines().length >0) {
				for(Object obj : timeLines.getTimeLines()){
					Timestamp rangestart = (Timestamp)obj;
					
						//need to format rangestart to GMT time string.  Timestamp.toString produce a local time Not GMT time
						Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
						cal.setTimeInMillis(rangestart.getTime());
						
						String gmtTimeStr = String.format("%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS",  cal);
						if(sndTimeList.indexOf(gmtTimeStr) != -1){
							// this indicate that gmtTimeStr is laready in the sndTimeList, then we dont need to add it to list again.
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
		
	}
	public void createPfcDialogContents(){
		topGp = new Group(parent,SWT.SHADOW_ETCHED_IN);
		topGp.setLayout( new GridLayout( 2, false ) );
		NsharpLoadDialog ldDia = NsharpLoadDialog.getAccess();
		ldDia.setShellSize(false);
		ldDia.createSndTypeList(topGp);
		
		fileTypeGp = new Group(topGp, SWT.SHADOW_ETCHED_IN);
		fileTypeGp.setText("File Type");

		namBtn = new Button(fileTypeGp, SWT.RADIO | SWT.BORDER);
		namBtn.setText("NAMSND");
		namBtn.setEnabled( true );
		namBtn.setBounds(fileTypeGp.getBounds().x+ NsharpConstants.btnGapX, fileTypeGp.getBounds().y + NsharpConstants.labelGap, NsharpConstants.btnWidth,NsharpConstants.btnHeight);

		namBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				currentSndType = NcSoundingProfile.PfcSndType.NAMSND;
				createPFCAvailableFileList();
			}          		            	 	
		} );  
		gfsBtn = new Button(fileTypeGp, SWT.RADIO | SWT.BORDER);
		gfsBtn.setText("GFSSND");
		gfsBtn.setEnabled( true );
		gfsBtn.setBounds(fileTypeGp.getBounds().x+ NsharpConstants.btnGapX, namBtn.getBounds().y + namBtn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);

		gfsBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				currentSndType = NcSoundingProfile.PfcSndType.GFSSND;
				createPFCAvailableFileList();
			}          		            	 	
		} );  

		ruc2Btn = new Button(fileTypeGp, SWT.RADIO | SWT.BORDER);
		ruc2Btn.setText("RUC2SND");
		ruc2Btn.setEnabled( false );
		ruc2Btn.setBounds(fileTypeGp.getBounds().x+ NsharpConstants.btnGapX, gfsBtn.getBounds().y + gfsBtn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);

		ruc2Btn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				currentSndType = NcSoundingProfile.PfcSndType.RUC2SND;
				createPFCAvailableFileList();
			}          		            	 	
		} );  
		rucpBtn = new Button(fileTypeGp, SWT.RADIO | SWT.BORDER);
		rucpBtn.setText("RUCPTYPSND");
		rucpBtn.setEnabled( false );
		rucpBtn.setBounds(fileTypeGp.getBounds().x+ NsharpConstants.btnGapX, ruc2Btn.getBounds().y + ruc2Btn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);

		rucpBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				currentSndType = NcSoundingProfile.PfcSndType.RUCPTYPSND;
				createPFCAvailableFileList();
			}          		            	 	
		} );  
		browseBtn = new Button(fileTypeGp, SWT.RADIO | SWT.BORDER);
		browseBtn.setText("BROWSE");
		browseBtn.setEnabled( false );
		browseBtn.setBounds(fileTypeGp.getBounds().x+ NsharpConstants.btnGapX, rucpBtn.getBounds().y + rucpBtn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);

		browseBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				currentSndType = NcSoundingProfile.PfcSndType.BROWSE;
			}          		            	 	
		} );  
		
		
		timeBtn = new Button(parent, SWT.CHECK | SWT.BORDER);
		timeBtn.setText("00Z and 12Z only");
		timeBtn.setEnabled( true );
		timeBtn.setBounds(fileTypeGp.getBounds().x+ NsharpConstants.btnGapX, browseBtn.getBounds().y + browseBtn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);

		timeBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(timeLimit)
					timeLimit = false;
				else
					timeLimit = true;
				
				//refresh sounding list if file type is selected already
				if(!currentSndType.equals("NA") && selectedFileList.size() > 0){
					createPFCSndTimeList(selectedFileList);
				}

			}          		            	 	
		} );  
		
		bottomGp = new Group(parent,SWT.SHADOW_ETCHED_IN);
		bottomGp.setLayout( new GridLayout( 2, false ) );
		
		availableFileGp = new Group(bottomGp,SWT.SHADOW_ETCHED_IN);
		availableFileGp.setText("Available PFC files:");
		availablefileList = new org.eclipse.swt.widgets.List(availableFileGp, SWT.BORDER  | SWT.MULTI| SWT.V_SCROLL  );
		availablefileList.setBounds(availableFileGp.getBounds().x, availableFileGp.getBounds().y + NsharpConstants.labelGap , NsharpConstants.filelistWidth, NsharpConstants.filelistHeight );
		//create a selection listener to handle user's selection on list		
		availablefileList.addListener ( SWT.Selection, new Listener () {
			private String selectedFile=null;	
			
			public void handleEvent (Event e) {   			
				if (availablefileList.getSelectionCount() > 0 ) {
					selectedFileList.clear();
					for(int i=0; i < availablefileList.getSelectionCount(); i++) {
						selectedFile = availablefileList.getSelection()[i];
						//System.out.println("selected sounding file is " + selectedFile);
						selectedFileList.add(selectedFile);
					}	
					createPFCSndTimeList(selectedFileList);
				}
			}
		} );
		
		 //create Sounding Times widget list 
		sndTimeListGp = new Group(bottomGp,SWT.SHADOW_ETCHED_IN);
		sndTimeListGp.setText("Sounding Times:");
		sndTimeList = new org.eclipse.swt.widgets.List(sndTimeListGp, SWT.BORDER  | SWT.MULTI| SWT.V_SCROLL  );
		sndTimeList.removeAll();
		sndTimeList.setBounds(sndTimeListGp.getBounds().x, sndTimeListGp.getBounds().y + NsharpConstants.labelGap, NsharpConstants.listWidth, NsharpConstants.listHeight );
		sndTimeList.addListener ( SWT.Selection, new Listener () {
			private String selectedSndTime=null;
    		public void handleEvent (Event e) {   			
    			if (sndTimeList.getSelectionCount() > 0 ) {
    				NsharpMapResource nsharpMapResource = NsharpMapResource.getOrCreateNsharpMapResource();//NsharpLoadDialog.getAccess().getNsharpMapResource();
    				nsharpMapResource.setPoints(null);
    				selectedTimeList.clear();
    				for(int i=0; i < sndTimeList.getSelectionCount(); i++) {
    					selectedSndTime = sndTimeList.getSelection()[i];
    					//System.out.println("selected sounding time is " + selectedSndTime);
    					queryAndMarkStn(selectedSndTime);
    					selectedTimeList.add(selectedSndTime);
    				}
    				//NsharpMapMouseHandler.getAccess().setSelectedTimeList(selectedTimeList);
    			}
    		}
    	});

	}
	private void queryAndMarkStn(String selectedSndTime) {
		//use NcSoundingQuery to query stn info
		NcSoundingStnInfoCollection sndStnInfoCol = NcSoundingQuery.soundingStnInfoQuery(currentSndType.toString(),selectedSndTime);
		if(sndStnInfoCol != null && sndStnInfoCol.getStationInfo() != null){

			NcSoundingStnInfo[] stnInfoAry = sndStnInfoCol.getStationInfo();
			NsharpMapResource nsharpMapResource = NsharpMapResource.getOrCreateNsharpMapResource();//NsharpLoadDialog.getAccess().getNsharpMapResource();
			
			//System.out.println("queryAndMarkStn stn num ="+ stnInfoAry.length);
			//Note: A same station may have many reports
			for(int i=0; i < stnInfoAry.length; i++){
				NcSoundingStnInfo stnInfo = stnInfoAry[i];
				NsharpStationInfo stn = new NsharpStationInfo();
				stn.setStnDisplayInfo(stnInfo.getStnId() + " " + selectedSndTime);
				stn.setLongitude(stnInfo.getStationLongitude());
				stn.setLatitude(stnInfo.getStationLatitude());
				stn.setReftime(stnInfo.getSynopTime());
				stn.setRangestarttime(stnInfo.getRangeStartTime());
				stn.setSndType(currentSndType.toString());
				//System.out.println( " currentSndType "+currentSndType);
				//stn.setElevation(stnInfo.getStationElevation());
				nsharpMapResource.addPoint(stn);
			}
			NsharpMapResource.bringMapEditorToTop();
			/* Chin test if(NsharpMapResource.getMapEditor() != null){
				
				
				NsharpMapResource.getMapEditor().refresh();
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().bringToTop(NsharpMapResource.getMapEditor());
			}
			else{
				//bring the MAP editor back to top 
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().bringToTop(NmapUiUtils.findDisplayByName("Map"));
			}*/
			
			//NsharpMapModalTool.setModal();
			
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
		if(ruc2Btn != null){
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
			
		}	
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
	}
}
