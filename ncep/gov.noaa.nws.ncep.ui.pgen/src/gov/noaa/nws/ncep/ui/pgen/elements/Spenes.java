/*
 * Spenes
 * 
 * Date created: May 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.common.staticdata.Cwa;
import gov.noaa.nws.ncep.common.staticdata.Rfc;
import gov.noaa.nws.ncep.common.staticdata.USState;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import java.awt.Color;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Software History
 * Date    Ticket       Engineer            Description
 * **                   B. Yin              Initial Creating
 * 05/04   734          J. Zeng             Add elements
 * 
 * @author jzeng
 *
 */

@ElementOperations ( {Operation.CONNECT, Operation.COPY_MOVE, Operation.EXTRAPOLATE,
    Operation.DELETE_PART, Operation.DELETE_POINT, Operation.MODIFY, 
    Operation.INTERPOLATE, Operation.ADD_POINT} )
public class Spenes extends Line {
	
	private ArrayList<USState> states;
	private ArrayList<Rfc> rfcs;
	private ArrayList<Cwa> cwas;
	
	private String stateZ000 = null;
	private String initDateTime = null;
	private String latestDataUsed = null;
	private int obsHr = -1 ;
	private String forecasters = null;
    private String location = null;
    private String attnWFOs = null;
    private String attnRFCs = null;
	private String event = null;
	private String satAnalysisTrend = null;
	private int shortTermBegin = -1;
	private int shortTermEnd = -1;
    private String outlookLevel = null;
    private String addlInfo = null;
    private String latLon = null;
    
	public Spenes(){
		super();
	}
	
	public Spenes(Coordinate[] range, Color[] colors,
			float lineWidth, double sizeScale, boolean closed, boolean filled,
			List<Coordinate> linePoints, int smoothFactor, FillPattern fillPattern,
			String pgenCategory, String pgenType) {
		super(range, colors, lineWidth, sizeScale, closed, filled,
				linePoints, smoothFactor, fillPattern, pgenCategory, pgenType);
	}

	/**
	 * Creates a copy of this object.  This is a deep copy and new objects are
	 * created so that we are not just copying references of objects
	 */
	@Override
	public DrawableElement copy() {
		/*
		 * create a new Line object and initially set its attributes to this one's
		 */
		Spenes newEl = new Spenes();
		
		/*
		 * new Strings are created for Type and LinePattern
		 */
		newEl.setPgenCategory(new String(this.getPgenCategory()));
		newEl.setPgenType(new String(this.getPgenType()));
		newEl.setParent(this.getParent());
		
		newEl.update(this);
		
		/*
		 * new Coordinates points are created and set, so we don't just set 
		 * references
		 */
		ArrayList<Coordinate> ptsCopy = new ArrayList<Coordinate>();
		for (int i=0; i < this.getPoints().size(); i++) {
			ptsCopy.add(new Coordinate(this.getPoints().get(i)));
		}
		newEl.setPoints(ptsCopy);
		
		/*
		 * new colors are created and set, so we don't just set 
		 * references
		 */
		Color[] colorCopy = new Color[this.getColors().length];
		for (int i=0; i<this.getColors().length; i++) {
			colorCopy[i] = new Color(this.getColors()[i].getRed(),
					                 this.getColors()[i].getGreen(),
					                 this.getColors()[i].getBlue() );
		}
		newEl.setColors(colorCopy);
		newEl.setLineWidth(this.getLineWidth());
		newEl.setSmoothFactor(this.getSmoothFactor());
		
		newEl.setInitDateTime(this.getInitDateTime());
		newEl.setLatestData(this.getLatestDataUsed());
		newEl.setObsHr(this.getObsHr());
		newEl.setForecasters(this.getForecasters());
		newEl.setStateZ000(this.getStateZ000());
		newEl.setLocation(this.getLocation());
		newEl.setAttnWFOs(this.getAttnWFOs());
		newEl.setAttnRFCs(this.getAttnRFCs());
		newEl.setEvent(this.getEvent());
		newEl.setSatAnalysisTrend(this.getSatAnalysisTrend());
		newEl.setShortTermBegin(this.getShortTermBegin());
		newEl.setShortTermEnd(this.getShortTermEnd());
		newEl.setOutlookLevel(this.getOutlookLevel());
		newEl.setAddlInfo(this.getAddlInfo());
		newEl.setLatLon(this.getLatLon());
		
		return newEl;
	}
	
	/**
	 * Gets the name of the line pattern associated with this object
	 */
	@Override
	public String getPatternName() {
		return "LINE_SOLID";
	}	
	
	/**
	 * Sets whether the object should be closed.
	 */
	public Boolean getClosed() {
		return true;
	}
	
	/**
	 * Generate information of states, wfos and rfcs.
	 */
	public void generateStatesWfosRfcs(){
		
		Polygon poly = this.toJTSPolygon();
		setStates(PgenStaticDataProvider.getProvider().statesInGeometry(poly));
		setRfcs(PgenStaticDataProvider.getProvider().rfcsInGeometry(poly));
		setCwas(PgenStaticDataProvider.getProvider().cwasInGeometry(poly));
	}
	
	/**
	 * Set states
	 */
	public void setStates(ArrayList<USState> states) {
		this.states = states;
	}
	
	/**
	 * Get states
	 */
	public ArrayList<USState> getStates() {
		return states;
	}
	
	/**
	 * Set Rfcs
	 */
	public void setRfcs(ArrayList<Rfc> rfcs) {
		this.rfcs = rfcs;
	}

	/**
	 * Get Rfcs
	 */
	public ArrayList<Rfc> getRfcs() {
		return rfcs;
	}
	
	/**
	 * Set Cwas
	 */
	public void setCwas(ArrayList<Cwa> cwas) {
		this.cwas = cwas;
	}
	
	/**
	 * Get Cwas
	 */
	public ArrayList<Cwa> getCwas() {
		return cwas;
	}
	
	/**
	 * get states abbreviation.
	 */
	public String getStateZ000(){
    	return stateZ000;
    }
    
	/**
	 * set state abbreviation from a string
	 */
	public void setStateZ000(String statesZ000){
    	this.stateZ000 = statesZ000;
    }
    
	/**
	 * set state abbreviation from a list
	 */
	public void setStateZ000(ArrayList<USState> states)  {
		int ii = 0;
		StringBuilder sb = new StringBuilder();
		for ( USState st : states) {
			sb.append(st.getStateAbrv());
			sb.append("Z000-");
			ii++;
			if(ii%9 == 0) sb.append("\n");
		}
		
    	this.stateZ000 = sb.toString();
    }
    
	/**
	 * get initial time
	 */
	public String getInitDateTime(){
    	return initDateTime;
    }
    
	/**
	 * initialize time
	 * @return
	 */
	public void setInitTime(){
	
		Calendar init = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
		if (init !=  null) {
			initDateTime = String.format("%1$tm/%1$td/%1$ty %1$tH%1$tMZ", init);
		}		
	}
    
	/**
	 * set initialization time from string
	 */
	public void setInitDateTime(String initDateTime){
		if (initDateTime == null ) setInitTime();
    	this.initDateTime = initDateTime;
    }
        
	/**
	 * get lastest data type used
	 */
	public String getLatestDataUsed(){
    	return latestDataUsed;
    }
    
	/**
	 * set latest data type
	 */
	public void setLatestData(String latestDataUsed){
    	this.latestDataUsed = latestDataUsed;
    	
    }
    
	/**
	 * get observation hour
	 */
	public int getObsHr(){
    	return obsHr;
    }
    
	/**
	 * set observation hour
	 */
	public void setObsHr(int obsHr){
    	this.obsHr = obsHr;
    }
    
	/**
	 * get forecaster's name
	 */
	public String getForecasters(){
		return forecasters;
	}
	
	/**
	 * set forecaster's name
	 */
	public void setForecasters(String forecasters){
		this.forecasters = forecasters;
	}   
	
	/**
	 * get states information
	 */
	public String getLocation(){
		return location;
	}
	
	/**
	 * set states information from string
	 */
	public void setLocation(String statesName){
		this.location = statesName;
	}
	
	/**
	 * set state information from list
	 */
	public void setLocation(ArrayList<USState> states) {
		int ii = 0;
		StringBuilder sb = new StringBuilder();
		for ( USState st : states) {
			sb.append(st.getName());
			sb.append("...");
			ii++;
			if(ii%6 == 0) sb.append("\n\n");
		}
		this.location = sb.toString();
	}
    
	/**
	 * get WFOs information
	 */
	public String getAttnWFOs(){
		return attnWFOs;
	}
	
	/**
	 * set WFOs information from a string
	 */
	public void setAttnWFOs(String cwas){
		this.attnWFOs = cwas;
	}
	
	/**
	 * set WFOs information from a list
	 */
	public void setAttnWFOs(ArrayList<Cwa> cwas) {
		int ii = 0;
		StringBuilder sb = new StringBuilder();
		for ( Cwa cwa : cwas) {
			sb.append(cwa.getWfoName());
			sb.append("...");
			ii++;
			if(ii%11 == 0) sb.append("\n\n");
		}
		this.attnWFOs = sb.toString();
	}
	
	/**
	 * get RFCs information
	 */
	public String getAttnRFCs(){
		return attnRFCs;
	}
	
	/**
	 * set RFCs information from a string
	 */
	public void setAttnRFCs(String rfcs) {
		this.attnRFCs = rfcs;
	}
	
	/**
	 * set RFCs information from a list
	 */
	public void setAttnRFCs(ArrayList<Rfc> rfcs){
		int ii = 0;
		StringBuilder sb = new StringBuilder();
		for ( Rfc rfc : rfcs) {
			sb.append(rfc.getBasinId());
			sb.append("...");
			ii++;
			if(ii%6 == 0) sb.append("\n\n");
		}
		this.attnRFCs = sb.toString();
	}
	
	/**
	 * set an event title
	 */
	public void setEvent(String event){
		this.event = event;
	}
	
	/**
	 * get an event title 
	 */
	public String getEvent(){
		return event;
	}
	
	/**
	 * set info from analysis
	 */
	public void setSatAnalysisTrend(String satAnalysisTrend){
		this.satAnalysisTrend = satAnalysisTrend;
	}
	
	/**
	 * set info from analysis
	 */
	public String getSatAnalysisTrend(){
		return satAnalysisTrend;
	}
	
	/**
	 * get the beginning time of the short term precipitation
	 */
	public int getShortTermBegin(){
		return shortTermBegin;
	}
	
	/**
	 * set the beginning time of the short term precipitation
	 */
	public void setShortTermBegin(int shortTermBegin){
		this.shortTermBegin = shortTermBegin;
	}
	
	/**
	 * get the ending time of the precipitation
	 */
	public int getShortTermEnd(){
		return shortTermEnd;
	}
	
	/**
	 * set the ending time of the precipitation
	 */
	public void setShortTermEnd(int shortTermEnd){
		this.shortTermEnd = shortTermEnd;
	}
	
	/**
	 * get outlook level
	 */
	public String getOutlookLevel(){
		return outlookLevel;
	}
	
	/**
	 * set outlook level
	 */
	public void setOutlookLevel(String outlookLevel){
		this.outlookLevel = outlookLevel;
	}
	
	/**
	 * set additional information
	 */
	public void setAddlInfo(String addlInfo){
		this.addlInfo = addlInfo;
	}
	
	/**
	 * get additional information
	 */
	public String getAddlInfo(){
		return addlInfo;
	}
	
	/**
	 * get latlon
	 */
	public String getLatLon(){
		return latLon;
	}
		
	/**
	 * set latlon from a list
	 */
	public void setLatLon(Coordinate[] coords){
		int ii = 0;
		StringBuilder sb = new StringBuilder();
		for ( Coordinate coord : coords) {
			sb.append((int)(coord.y*100));
			sb.append(" ");
			sb.append((int)(coord.x*100));
			sb.append(" ");
			ii++;
			if(ii%4 == 0) sb.append("\n\n");
		}
		this.latLon = sb.toString();
	}
	
	/**
	 * set latlon from a string
	 */
	public void setLatLon(String latlon){
		this.latLon = latlon;
	}
}
