/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.menu.ObservedSoundingDialogContents
 * 
 * This java class performs the NSHARP NsharpLoadDialog functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 01/2011	    229			Chin Chen	Initial coding
 * 09/14/2011   457         S. Gurung   Renamed H5UAIR to NCUAIR
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
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;

public class ObservedSoundingDialogContents {
	private  Composite parent;
	private   org.eclipse.swt.widgets.List sndTimeList;
	private Group  btnGp,  sndTimeListGp, topGp, midGp;
	private   boolean timeLimit = false, newtab=false;	
	private   boolean rawData = false;	
	private   Button timeBtn,  browseBtn, tamBtn,  bufruaBtn, uairBtn, rawBtn, newTabBtn;
	private  String FILE_UAIR = "UAIR";
	private  String FILE_BUFRUA = "BUFRUA";
	//private  String FILE_DROP = "DROP";
	private  String FILE_TAMDAR = "TAMDAR";
	private  String FILE_BROWSE = "BROWSE";
	private  NcSoundingProfile.ObsSndType currentSndType = NcSoundingProfile.ObsSndType.NONE;
	private NsharpLoadDialog ldDia;
	private  List<String> selectedTimeList = new ArrayList<String>(); 
	private Font newFont;
	public boolean isRawData() {
		return rawData;
	}
	public NcSoundingProfile.ObsSndType getCurrentSndType() {
		return currentSndType;
	}
	public ObservedSoundingDialogContents(Composite parent) {
		this.parent = parent;
		ldDia = NsharpLoadDialog.getAccess();
		newFont = ldDia.getNewFont();
	}
	private void createObsvdSndUairList() {
		sndTimeList.removeAll();
		
		// use NcSoundingQuery to query
		NcSoundingTimeLines timeLines = NcSoundingQuery.soundingTimeLineQuery(currentSndType.toString());
		
		if(timeLines!= null && timeLines.getTimeLines() != null){
			ldDia.startWaitCursor();
			for(Object timeLine : timeLines.getTimeLines()){
				Timestamp synoptictime = (Timestamp)timeLine;
				if(synoptictime != null){
					//need to format synoptictime to GMT time string.  Timestamp.toString produce a local time Not GMT time
					Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
					cal.setTimeInMillis(synoptictime.getTime());
					String gmtTimeStr = String.format("%1$ty%1$tm%1$td/%1$tH",  cal);
					//String gmtTimeStr = String.format("%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS",  cal);
					//System.out.println("GMT time " + gmtTimeStr + " msec="+ synoptictime.getTime() + " local time "+synoptictime.toString());
					if(!timeLimit){
						//System.out.println("not 00z and 12z only");
						sndTimeList.add(gmtTimeStr);
					}
					else {
						int hour = cal.get(Calendar.HOUR_OF_DAY);
						//System.out.println("00z and 12z only hour = "+ hour);
						if((hour == 0) || (hour == 12))
							sndTimeList.add(gmtTimeStr);
					}
				}
			}
			ldDia.stopWaitCursor();
		}
		else
			System.out.println("EDEX timeline query return null");

	}
	private void queryAndMarkStn(String selectedSndTime) {
		String selectTimetr = NcSoundingQuery.convertSoundTimeDispStringToRangeStartTimeFormat(selectedSndTime);
		NsharpMapResource nsharpMapResource = NsharpMapResource.getOrCreateNsharpMapResource();
		//Chin float lat, lon;
		double lat, lon;
		String stnInfoStr;
		
		//use NcSoundingQuery to query stn info
		NcSoundingStnInfoCollection sndStnInfoCol = NcSoundingQuery.soundingStnInfoQuery(currentSndType.toString(),selectTimetr);
		if(sndStnInfoCol != null && sndStnInfoCol.getStationInfo() != null){

			NcSoundingStnInfo[] stnInfoAry = sndStnInfoCol.getStationInfo();
			//System.out.println("queryAndMarkStn called mapresource = "+ nsharpMapResource.toString());
			//Note: A same station may have many reports
			for(int i=0; i < stnInfoAry.length; i++){
				NcSoundingStnInfo stnInfo = stnInfoAry[i];
				Timestamp synoptictime=null;
				stnInfoStr= stnInfo.getStnId();
				if(stnInfoStr== null || stnInfoStr.length() < 1)
					stnInfoStr = "*";
				lat = stnInfo.getStationLatitude();
				lon = stnInfo.getStationLongitude();
				//elv = stnInfo.getStationElevation();
				synoptictime = (Timestamp)stnInfo.getSynopTime();

				//convert to Nsharp's own station info struct
				NsharpStationInfo stn = new NsharpStationInfo();
				stn.setStnDisplayInfo(stnInfoStr + " " + selectedSndTime);
				stn.setLongitude(lon);
				stn.setLatitude(lat);
				//stn.setElevation(elv);
				stn.setReftime(synoptictime);
				stn.setRangestarttime(synoptictime);
				stn.setSndType(currentSndType.toString());
				//System.out.println("sndType= "+currentSndType);
				//System.out.println("stn  lat ="+stn.getLatitude() + " lon="+stn.getLongitude());
				nsharpMapResource.addPoint(stn);
			}

			NsharpMapResource.bringMapEditorToTop();
			/*  Chin test if(NsharpMapResource.getMapEditor() != null){

				//NmapUiUtils.getActiveNatlCntrsEditor().refresh();
				NsharpMapResource.getMapEditor().refresh();
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().bringToTop(NsharpMapResource.getMapEditor());
			}
			else{
				//bring the MAP editor back to top 
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().bringToTop(NmapUiUtils.findDisplayByName("Map"));

			}*/
		}
		//NsharpMapModalTool.setModal();
	}
	public void createObsvdDialogContents(){
		currentSndType = NcSoundingProfile.ObsSndType.NONE;
		timeLimit =false;
		rawData = false;
		topGp = new Group(parent,SWT.SHADOW_ETCHED_IN);
		topGp.setLayout( new GridLayout( 2, false ) );
		
		//ldDia.setShellSize(false);
		ldDia.createSndTypeList(topGp);
		
		btnGp = new Group(topGp, SWT.SHADOW_ETCHED_IN);
		btnGp.setText("File Type");
		btnGp.setFont(newFont);
		uairBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		uairBtn.setText(FILE_UAIR);
		uairBtn.setEnabled( true );
		uairBtn.setBounds(btnGp.getBounds().x+ NsharpConstants.btnGapX, btnGp.getBounds().y + NsharpConstants.labelGap, NsharpConstants.btnWidth,NsharpConstants.btnHeight);
		uairBtn.setFont(newFont);
		uairBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {   
				sndTimeList.removeAll();
				currentSndType = NcSoundingProfile.ObsSndType.NCUAIR;
				createObsvdSndUairList();
				//System.out.println("new obvSnd dialog uair btn");
			}          		            	 	
		} ); 
		
		bufruaBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		bufruaBtn.setText(FILE_BUFRUA);
		bufruaBtn.setEnabled( true );
		bufruaBtn.setBounds(btnGp.getBounds().x+ NsharpConstants.btnGapX, uairBtn.getBounds().y + uairBtn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);
		bufruaBtn.setFont(newFont);
		bufruaBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				sndTimeList.removeAll();
				currentSndType = NcSoundingProfile.ObsSndType.BUFRUA;
				createObsvdSndUairList();
			}          		            	 	
		} );  
		/*/NCUAIR 
		ncuairBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		ncuairBtn.setText("NCUAIR");
		ncuairBtn.setEnabled( true );
		ncuairBtn.setBounds(btnGp.getBounds().x+ NsharpConstants.btnGapX, bufruaBtn.getBounds().y + bufruaBtn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);
		
		ncuairBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {   
				sndTimeList.removeAll();
				//CHIN HDF5 test
				currentSndType = NcSoundingProfile.ObsSndType.NCUAIR;
				createObsvdSndUairList();
				//System.out.println("new obvSnd dialog uair btn");
			}          		            	 	
		} ); 
		
		dropBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		dropBtn.setText(FILE_DROP);
		dropBtn.setEnabled( false );
		dropBtn.setBounds(btnGp.getBounds().x+ NsharpConstants.btnGapX, bufruaBtn.getBounds().y + bufruaBtn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);

		dropBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				sndTimeList.removeAll();
				currentSndType = NcSoundingProfile.ObsSndType.DROP;
			}          		            	 	
		} ); 
		tamBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		tamBtn.setText(FILE_TAMDAR);
		tamBtn.setEnabled( false );
		tamBtn.setBounds(btnGp.getBounds().x+ NsharpConstants.btnGapX, bufruaBtn.getBounds().y + bufruaBtn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);
		//tamBtn.setBounds(btnGp.getBounds().x+ NsharpConstants.btnGapX, ncuairBtn.getBounds().y + ncuairBtn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);
		tamBtn.setFont(newFont);
		tamBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {  
				sndTimeList.removeAll();
				currentSndType = NcSoundingProfile.ObsSndType.TAMDAR;
			}          		            	 	
		} );  
		browseBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		browseBtn.setText(FILE_BROWSE);
		browseBtn.setEnabled( false );
		browseBtn.setBounds(btnGp.getBounds().x+ NsharpConstants.btnGapX, tamBtn.getBounds().y + tamBtn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);
		browseBtn.setFont(newFont);
		browseBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				sndTimeList.removeAll();
				currentSndType = NcSoundingProfile.ObsSndType.BROWSE;
			}          		            	 	
		} );  */
		midGp = new Group(parent,SWT.SHADOW_ETCHED_IN);
		midGp.setLayout( new GridLayout( 2, false ) );
		timeBtn = new Button(midGp, SWT.CHECK | SWT.BORDER);
		timeBtn.setText("00Z and 12Z only");
		timeBtn.setEnabled( true );
		//timeBtn.setBounds(btnGp.getBounds().x+ NsharpConstants.btnGapX, browseBtn.getBounds().y + browseBtn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);
		timeBtn.setFont(newFont);
		timeBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(timeBtn.getSelection())
					timeLimit = true;
				else
					timeLimit = false;
				
				//refresh sounding list if file type is selected already
				if(currentSndType== NcSoundingProfile.ObsSndType.NCUAIR /*|| currentSndType == NcSoundingProfile.ObsSndType.UAIR*/|| currentSndType == NcSoundingProfile.ObsSndType.BUFRUA){
					createObsvdSndUairList();
				}
					
				
			}          		            	 	
		} );  
		rawBtn = new Button(midGp, SWT.CHECK | SWT.BORDER);
		rawBtn.setText("raw data");
		rawBtn.setEnabled( true );
		rawBtn.setBounds(timeBtn.getBounds().x+timeBtn.getBounds().width, timeBtn.getBounds().y,
				timeBtn.getBounds().width, timeBtn.getBounds().height);
		rawBtn.setFont(newFont);
		rawBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(rawBtn.getSelection())
					rawData  = true;
				else
					rawData = false;;
			}          		            	 	
		} );  
		 //create file widget list 
		sndTimeListGp = new Group(parent,SWT.SHADOW_ETCHED_IN);
		sndTimeListGp.setText("Sounding Times:");
		sndTimeListGp.setFont(newFont);
		sndTimeList = new org.eclipse.swt.widgets.List(sndTimeListGp, SWT.BORDER | SWT.MULTI| SWT.V_SCROLL  );
		sndTimeList.setBounds(btnGp.getBounds().x+ NsharpConstants.btnGapX, sndTimeListGp.getBounds().y + NsharpConstants.labelGap, NsharpConstants.listWidth, NsharpConstants.listHeight*7 );
		sndTimeList.setFont(newFont);
		//create a selection listener to handle user's selection on list		
		sndTimeList.addListener ( SWT.Selection, new Listener () {
        	private String selectedSndTime=null;	
    		public void handleEvent (Event e) {   			
    			if (sndTimeList.getSelectionCount() > 0 ) {  	
    				NsharpMapResource nsharpMapResource = NsharpMapResource.getOrCreateNsharpMapResource();//NsharpLoadDialog.getAccess().getNsharpMapResource();
    				nsharpMapResource.setPoints(null);
    				selectedTimeList.clear();
    				ldDia.startWaitCursor();
    				for(int i=0; i < sndTimeList.getSelectionCount(); i++) {
    					selectedSndTime = sndTimeList.getSelection()[i];
    					//System.out.println("selected sounding time is " + selectedSndTime);
    					queryAndMarkStn(selectedSndTime);
    					selectedTimeList.add(selectedSndTime);
    				}
					ldDia.stopWaitCursor();

    				//NsharpMapMouseHandler.getAccess().setSelectedTimeList(selectedTimeList);
    			}
    		}
    	});
		/*newTabBtn = new Button(parent, SWT.CHECK | SWT.BORDER);
		newTabBtn.setText("new skewT editor");
		newTabBtn.setEnabled( true );
		//newTabBtn.setBounds(btnGp.getBounds().x+ NsharpConstants.btnGapX, browseBtn.getBounds().y + browseBtn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);
		newTabBtn.setFont(newFont);
		newTabBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(newTabBtn.getSelection())
					newtab = true;
				else
					newtab = false;
			}          		            	 	
		} ); */ 
	}
	public void cleanup(){
		if(sndTimeList != null){
			sndTimeList.removeListener(SWT.Selection, sndTimeList.getListeners(SWT.Selection)[0]);
			sndTimeList.dispose();
			sndTimeList = null;
		}
		if(sndTimeListGp!= null){
			sndTimeListGp.dispose();
			sndTimeListGp = null;
		}
		if(timeBtn != null){
			timeBtn.removeListener(SWT.MouseUp, timeBtn.getListeners(SWT.MouseUp)[0]);
			timeBtn.dispose();
			timeBtn= null;
		}
		if(rawBtn != null){
			rawBtn.removeListener(SWT.MouseUp, rawBtn.getListeners(SWT.MouseUp)[0]);
			rawBtn.dispose();
			rawBtn= null;
		}
		if(midGp!= null){
			midGp.dispose();
			midGp = null;
		}
		/*if(browseBtn != null){
			browseBtn.removeListener(SWT.MouseUp, browseBtn.getListeners(SWT.MouseUp)[0]);
			browseBtn.dispose();
			browseBtn = null;
		}
		
		
		if(tamBtn != null){
			tamBtn.removeListener(SWT.MouseUp, tamBtn.getListeners(SWT.MouseUp)[0]);
			tamBtn.dispose();
			tamBtn = null;
		}
		*/
		if(bufruaBtn != null){
			bufruaBtn.removeListener(SWT.MouseUp, bufruaBtn.getListeners(SWT.MouseUp)[0]);
			bufruaBtn.dispose();
			bufruaBtn = null;
		}
		if(uairBtn != null){
			uairBtn.removeListener(SWT.MouseUp, uairBtn.getListeners(SWT.MouseUp)[0]);
			uairBtn.dispose();
			uairBtn = null;
		}
		if(btnGp!= null){
			btnGp.dispose();
			btnGp = null;
		}
		/*if(newTabBtn != null){
			newTabBtn.removeListener(SWT.MouseUp, newTabBtn.getListeners(SWT.MouseUp)[0]);
			newTabBtn.dispose();
			newTabBtn = null;
		}*/
		NsharpLoadDialog ldDia = NsharpLoadDialog.getAccess();
		ldDia.cleanSndTypeList();
		if(topGp!= null){
			topGp.dispose();
			topGp = null;
		}
	}
}
