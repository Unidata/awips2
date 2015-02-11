package gov.noaa.nws.ncep.ui.nsharp;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;

import java.util.ArrayList;
import java.util.List;


/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSoundingElementStateProperty
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
 * 01/27/2015   DR#17006,
 *              Task#5929   Chin Chen   NSHARP freezes when loading a sounding from MDCRS products 
 *                                      in Volume Browser
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

public class NsharpSoundingElementStateProperty {
	private String elementDescription; //stnId_timeLine_sndType
	//private NsharpConstants.LoadState elementState; //  possible values are AVAIL,NOTAVAIL
						// NOTAVAIL is set when there is no sounding data loaded for this stn at this time line.
	private String stnDescription;
	private String timeDescription;
	private String sndType;
	private NsharpStationInfo stnInfo;
	private int compColorIndex;
	private List<NcSoundingLayer> sndLyLst;
	private List<NcSoundingLayer> sndLyLstBk;
	private boolean goodData = true; //#5929
	public NsharpSoundingElementStateProperty(String elementDescription,
			String stnDescription, 
			String timeDescription,  NsharpStationInfo stnInfo, List<NcSoundingLayer> sndLyLst, boolean goodData) {
		super();
		this.elementDescription = elementDescription;
		//this.elementState = elementState;
		this.stnDescription = stnDescription;
		this.timeDescription = timeDescription;
		this.stnInfo = stnInfo;
		this.sndType =  stnInfo.getSndType();
		this.compColorIndex = NsharpConstants.LINE_COMP1;;
		this.sndLyLst = sndLyLst;
		this.goodData = goodData; //#5929
		sndLyLstBk= new ArrayList<NcSoundingLayer>(sndLyLst.size());
		for(NcSoundingLayer ly : sndLyLst){
			try {
				sndLyLstBk.add((NcSoundingLayer)ly.clone());
			} catch (CloneNotSupportedException e) {
				e.printStackTrace();
			}
		}
	}
	
	public NsharpSoundingElementStateProperty() {
		super();
		// TODO Auto-generated constructor stub
	}

	public String getElementDescription() {
		return elementDescription;
	}
	public void setElementDescription(String elementDescription) {
		this.elementDescription = elementDescription;
	}
	
	public String getStnDescription() {
		return stnDescription;
	}
	public void setStnDescription(String stnDescription) {
		this.stnDescription = stnDescription;
	}
	
	public String getTimeDescription() {
		return timeDescription;
	}
	public void setTimeDescription(String timeDescription) {
		this.timeDescription = timeDescription;
	}
	
	public NsharpStationInfo getStnInfo() {
		return stnInfo;
	}
	public void setStnInfo(NsharpStationInfo stnInfo) {
		this.stnInfo = stnInfo;
	}

	
	public int getCompColorIndex() {
		return compColorIndex;
	}

	public void setCompColorIndex(int compColorIndex) {
		this.compColorIndex = compColorIndex;
	}

	public String getSndType() {
		return sndType;
	}

	public void setSndType(String sndType) {
		this.sndType = sndType;
	}

	public List<NcSoundingLayer> getSndLyLst() {
		return sndLyLst;
	}

	public void setSndLyLst(List<NcSoundingLayer> sndLyLst) {
		this.sndLyLst = sndLyLst;
	}

	public List<NcSoundingLayer> getSndLyLstBk() {
		return sndLyLstBk;
	}

	public void setSndLyLstBk(List<NcSoundingLayer> sndLyLstBk) {
		this.sndLyLstBk = sndLyLstBk;
	}
	public void restoreSndLyLstFromBackup(){
		sndLyLst.clear();
		for(NcSoundingLayer ly : sndLyLstBk){
			try {
				sndLyLst.add((NcSoundingLayer)ly.clone());
			} catch (CloneNotSupportedException e) {
				e.printStackTrace();
			}
		}
	}

	public boolean isGoodData() {
		return goodData;
	}

	public void setGoodData(boolean goodData) {
		this.goodData = goodData;
	}
	
}