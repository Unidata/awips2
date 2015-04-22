/*
 * gov.noaa.nws.ncep.ui.pgen.sigmet
 * 
 * Janurary 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.sigmet;

import java.awt.Color;
import java.util.*;

import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.elements.*;
import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;

import com.vividsolutions.jts.geom.Coordinate;


/**
 * The class for Volcano element
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		#165		G. Zhang   	Initial Creation.
 * 01/11        #137        Q. Zhou     Create getConverterVolcPoints(). Instead of getPoints for converter. 
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * </pre>
 * 
 * @author	G. Zhang
 */
@ElementOperations ( {Operation.DELETE_POINT} )
public class Volcano extends SinglePointElement implements ISigmet{
	
	public static final String NIL_STRING = "NIL";
	
	/**
	 * used for connecting user inputs and info from vaa.xml
	 * so one fields can store the two together. 
	 * 
	 */
	public static final String WORD_SPLITTER = SigmetInfo.LINE_SEPERATER;
	
	Coordinate[] locs = null;
	
	private String name = null;
	private String number = null;
	private String txtLoc = null;
	private String area = null;
	private String elev = null;
	
	private String origStnVAAC = null;
	private String wmoId = null;
	private String hdrNum = null;
	private String product = null; //TODO: for NORMAL/END/NEAR/QUICK
	private String year = null;
	private String advNum = null;
	private String corr = null;
	
	private String infoSource = null;
	private String addInfoSource = null;
	private String aviColorCode = null;
	private String erupDetails = null;
	
	private String obsAshDate = null;
	private String obsAshTime = null;
	private String nil = null;
	
	private String obsFcstAshCloudInfo = null;
	private String obsFcstAshCloudInfo6 = null;
	private String obsFcstAshCloudInfo12 = null;
	private String obsFcstAshCloudInfo18 = null;

	private String remarks = null;
	private String nextAdv = null;
	private String forecasters = null;
	
		
	public void setLinePoints(ArrayList<Coordinate> locs){
		this.locs = locs.toArray(new Coordinate[]{});
		if(locs != null && locs.size() > 0) 
			this.setLocation(locs.get(0));
	}
	
	public Coordinate[] getLinePoints(){
		return locs != null ? locs : new Coordinate[]{getLocation()};
	}
	
	
	public AbstractDrawableComponent copy(){
		//TODO: replace this!
		Volcano newVol = new Volcano();
		newVol.update(this);//see Symbol.java
		
		newVol.setColors(new Color[]{
				new Color(	getColors()[0].getRed(),
							getColors()[0].getGreen(),
							getColors()[0].getBlue() )  });
		newVol.setLocation(new Coordinate(getLocation()));
		newVol.setPgenCategory(pgenCategory);
		newVol.setPgenType(pgenType);
		newVol.setParent(getParent());		
		
		//TODO: set others
		
		return newVol;
	}

	@Override
	public String getPatternName() {
		
		return getPgenType();
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getNumber() {
		return number;
	}

	public void setNumber(String number) {
		this.number = number;
	}

	public String getArea() {
		return area;
	}

	public void setArea(String area) {
		this.area = area;
	}

	public String getElev() {
		return elev;
	}

	public void setElev(String elev) {
		this.elev = elev;
	}

	public String getTxtLoc() {
		return txtLoc;
	}

	public void setTxtLoc(String txtLoc) {
		this.txtLoc = txtLoc;
	}

	public String getOrigStnVAAC() {
		return origStnVAAC;
	}

	public void setOrigStnVAAC(String origStnVAAC) {
		this.origStnVAAC = origStnVAAC;
	}

	public String getWmoId() {
		return wmoId;
	}

	public void setWmoId(String wmoId) {
		this.wmoId = wmoId;
	}

	public String getHdrNum() {
		return hdrNum;
	}

	public void setHdrNum(String hdrNum) {
		this.hdrNum = hdrNum;
	}

	public String getProduct() {
		return product;
	}

	public void setProduct(String product) {
		this.product = product;
	}

	public String getYear() {
		return year;
	}

	public void setYear(String year) {
		this.year = year;
	}

	public String getAdvNum() {
		
		return advNum;
	}

	public void setAdvNum(String advNum) {
		this.advNum = advNum;
	}

	public String getCorr() {
		return corr;
	}

	public void setCorr(String corr) {
		this.corr = corr;
	}

	public String getInfoSource() {
		return infoSource;
	}

	public void setInfoSource(String infoSource) {
		this.infoSource = infoSource;
	}

	public String getAddInfoSource() {
		return addInfoSource;
	}

	public void setAddInfoSource(String addInfoSource) {
		this.addInfoSource = addInfoSource;
	}

	public String getAviColorCode() {
		return aviColorCode;
	}

	public void setAviColorCode(String aviColorCode) {
		this.aviColorCode = aviColorCode;
	}

	public String getErupDetails() {
		return erupDetails;
	}

	public void setErupDetails(String erupDetails) {
		this.erupDetails = erupDetails;
	}

	public String getObsAshDate() {
		return obsAshDate;
	}

	public void setObsAshDate(String obsAshDate) {
		this.obsAshDate = obsAshDate;
	}

	public String getObsAshTime() {
		return obsAshTime;
	}

	public void setObsAshTime(String obsAshTime) {
		this.obsAshTime = obsAshTime;
	}

	public String getObsFcstAshCloudInfo() {
		return obsFcstAshCloudInfo;
	}

	public void setObsFcstAshCloudInfo(String obsFcstAshCloudInfo) {
		this.obsFcstAshCloudInfo = obsFcstAshCloudInfo;
	}
	
	public String getObsFcstAshCloudInfo6() {
		return obsFcstAshCloudInfo6;
	}

	public void setObsFcstAshCloudInfo6(String obsFcstAshCloudInfo6) {
		this.obsFcstAshCloudInfo6 = obsFcstAshCloudInfo6;
	}

	public String getObsFcstAshCloudInfo12() {
		return obsFcstAshCloudInfo12;
	}

	public void setObsFcstAshCloudInfo12(String obsFcstAshCloudInfo12) {
		this.obsFcstAshCloudInfo12 = obsFcstAshCloudInfo12;
	}

	public String getObsFcstAshCloudInfo18() {
		return obsFcstAshCloudInfo18;
	}

	public void setObsFcstAshCloudInfo18(String obsFcstAshCloudInfo18) {
		this.obsFcstAshCloudInfo18 = obsFcstAshCloudInfo18;
	}

	public String getRemarks() {
		return remarks;
	}

	public void setRemarks(String remarks) {
		this.remarks = remarks;
	}

	public String getNextAdv() {
		return nextAdv;
	}

	public void setNextAdv(String nextAdv) {
		this.nextAdv = nextAdv;
	}

	public String getForecasters() {
		return forecasters;
	}

	public void setForecasters(String forecasters) {
		this.forecasters = forecasters;
	}

	public String getNil() {
		return nil;
	}

	public void setNil(String nil) {
		this.nil = nil;
	}
	
	/**
	 * special Volcanos(i.e.TEST) have NO points on the map.
	 * return an empty list to avoid drawing it when
	 * selecting: see PgenResource method
	 * getNearestElement(Coordinate, ElementFilter)
	 * 
	 */
	
	@Override 
	public ArrayList<Coordinate> getPoints(){
		if( VaaInfo.isNonDrawableVol(this))
			return new ArrayList<Coordinate>();
		
		return super.getPoints();		
	}
	
	//Create getConverterVolcPoints(). Instead of getPoints for converter
	public ArrayList<Coordinate> getConverterVolcPoints(){
		
		return super.getPoints();		
	}
	
	/**
	 * append info from vaa.xml to erupDetails 
	 * 
	 * @param: info: words from vaa.xml
	 */
	
	public void setExtraErupDetails(String info){		
		erupDetails = getNoNullTxt(erupDetails) + WORD_SPLITTER + info;
	}
	
	/**
	 * append info from vaa.xml to infoSource	 
	 * 
	 * @param: info: words from vaa.xml 
	 */
	
	public void setExtraInfoSource(String info){
		infoSource = getNoNullTxt(infoSource) + WORD_SPLITTER + info;
	}
	
	/**
	 * append info from vaa.xml to remarks
	 * 
	 * @param: info: words from vaa.xml
	 */
	
	public void setExtraRemarks(String info){
		remarks = getNoNullTxt(remarks) + WORD_SPLITTER + info;
	}
	
	/**
	 * append info from vaa.xml to nextAdv
	 * 
	 * @param: info: words from vaa.xml
	 */
	
	public void setExtraNextAdv(String info){
		nextAdv = getNoNullTxt(nextAdv) + WORD_SPLITTER + info;
	}
	
	/**
     * Text and other Widgets canNOT: setText(null). 
     * 
     * @param: text to be checked
     * @return non-null text
     */
	
	public static String getNoNullTxt(String s){
		return s == null ? "" : s;
	}
	
	/**
	 * Some fields contains both user inputs and info
     * from vaa.xml divided by Volcano.WORD_SPLITTER.
     * 
	 * @param word: user input with info from vaa.xml
	 * @return user input
	 */
	
	public static String getUserInputPart(String word){
		String input = "";
		
		if(word != null ){//&& word.contains(Volcano.WORD_SPLITTER)){
    		input = word.split(Volcano.WORD_SPLITTER)[0];
    	}
		
		return input;
	}

	@Override
	public int getSmoothFactor() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public Boolean isClosedLine() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Boolean isFilled() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public FillPattern getFillPattern() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getLineType() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public double getWidth() {
		// TODO Auto-generated method stub
		return 0;
	}
	
}
