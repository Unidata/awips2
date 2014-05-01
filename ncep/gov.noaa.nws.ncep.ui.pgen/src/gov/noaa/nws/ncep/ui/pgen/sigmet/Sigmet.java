/*
 * gov.noaa.nws.ncep.ui.pgen.sigmet.Sigmet
 * 
 * September 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.sigmet;

import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;

import java.awt.Color;
import java.util.ArrayList;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Element class for sigmet.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/09		160			Gang Zhang 	Initial Creation. 
 * 04/11		?			B. Yin		Re-factor IAttribute
 * 03/12        #676        Q. Zhou     Added Issue Office field.
 * </pre>
 * 
 * @author	gzhang
 */

@ElementOperations ( {Operation.COPY_MOVE, Operation.EXTRAPOLATE, Operation.DELETE_POINT,
    Operation.ADD_POINT, Operation.INTERPOLATE, Operation.MODIFY} )
public class Sigmet extends AbstractSigmet{
	
	public final static String SIGMET_PGEN_CATEGORY = "Sigmet"; 	 
	public final static String SIGMET_PGEN_TYPE = "INTL_SIGMET";
	public static final String AREA = "Area", LINE = "Line", ISOLATED = "Isolated";//public for DisplayElementFactory/others use

	
	private String editableAttrStatus;	//new,amend...	
	private String editableAttrStartTime;	//start valid
	private String editableAttrEndTime;	//end valid
	private String editableAttrRemarks;	
	private String editableAttrPhenom;
	private String editableAttrPhenom2;
	private String editableAttrPhenomName;
	private String editableAttrPhenomLat;
	private String editableAttrPhenomLon;
	private String editableAttrPhenomPressure;
	private String editableAttrPhenomMaxWind;
	private String editableAttrFreeText;
	private String editableAttrTrend;
	private String editableAttrMovement;
	private String editableAttrPhenomSpeed;
	private String editableAttrPhenomDirection;
	private String editableAttrLevel;
	private String editableAttrLevelInfo1;
	private String editableAttrLevelInfo2;
	private String editableAttrLevelText1;
	private String editableAttrLevelText2;	
	private String editableAttrFir;
	
	public Sigmet(){
		
	}
	
	public Sigmet(
			Coordinate[] range, 
			Color[] colors,
			float lineWidth, 
			double sizeScale, 
			boolean closed, 
			boolean filled,
			ArrayList<Coordinate> linePoints, 
			int smoothFactor, 
			FillPattern fillPattern,
			String pgenCategory, 
			String pgenType, 
			
			String type, 
			double width,
		
			String editableAttrArea,
			String editableAttrIssueOffice,
			String editableAttrStatus,	
			String editableAttrId,		
			String editableAttrSeqNum,	
			String editableAttrStartTime,	
			String editableAttrEndTime,	
			String editableAttrRemarks,	
			String editableAttrPhenom,
			String editableAttrPhenom2,
			String editableAttrPhenomName,
			String editableAttrPhenomLat,
			String editableAttrPhenomLon,
			String editableAttrPhenomPressure,
			String editableAttrPhenomMaxWind,
			String editableAttrFreeText,
			String editableAttrTrend,
			String editableAttrMovement,
			String editableAttrPhenomSpeed,
			String editableAttrPhenomDirection,
			String editableAttrLevel,
			String editableAttrLevelInfo1,
			String editableAttrLevelInfo2,
			String editableAttrLevelText1,
			String editableAttrLevelText2,
			String editableAttrFromLine,
			String editableAttrFir				){
		
		super(	range, 
				colors,
				lineWidth, 
				sizeScale, 
				closed, 
				filled,
				linePoints, 
				smoothFactor, 
				fillPattern,
				pgenCategory, 
				pgenType, 
				
				type, 
				width,
				
				editableAttrArea,
				editableAttrIssueOffice,
				editableAttrFromLine,
				editableAttrId,	
				editableAttrSeqNum	);
		
		this.editableAttrStatus = editableAttrStatus;		
		this.editableAttrStartTime = editableAttrStartTime;
		this.editableAttrEndTime = editableAttrEndTime;
		this.editableAttrRemarks = editableAttrRemarks;
		this.editableAttrPhenom = editableAttrPhenom;
		this.editableAttrPhenom2 = editableAttrPhenom2;
		this.editableAttrPhenomName = editableAttrPhenomName;
		this.editableAttrPhenomLat = editableAttrPhenomLat;
		this.editableAttrPhenomLon = editableAttrPhenomLon;
		this.editableAttrPhenomPressure = editableAttrPhenomPressure;
		this.editableAttrPhenomMaxWind = editableAttrPhenomMaxWind;
		this.editableAttrFreeText = editableAttrFreeText;
		this.editableAttrTrend = editableAttrTrend;
		this.editableAttrMovement = editableAttrMovement;
		this.editableAttrPhenomSpeed = editableAttrPhenomSpeed;
		this.editableAttrPhenomDirection = editableAttrPhenomDirection;
		this.editableAttrLevel = editableAttrLevel;
		this.editableAttrLevelInfo1 = editableAttrLevelInfo1;
		this.editableAttrLevelInfo2 = editableAttrLevelInfo2;
		this.editableAttrLevelText1 = editableAttrLevelText1;
		this.editableAttrLevelText2 = editableAttrLevelText2;		
		this.editableAttrFir = editableAttrFir;
		
	}
	
	

	
	@Override
	public DrawableElement copy() {
		/*
		 * create a new Line object and initially set its attributes to this one's
		 */
		Sigmet newSigmet = new Sigmet();
		newSigmet.update(this);
		
		/*
		 * new Coordinates points are created and set, so we don't just set 
		 * references
		 */
		ArrayList<Coordinate> ptsCopy = new ArrayList<Coordinate>();
		for (int i=0; i < this.getPoints().size(); i++) {
			ptsCopy.add(new Coordinate(this.getPoints().get(i)));
		}
		newSigmet.setPoints(ptsCopy);
		
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
		newSigmet.setColors(colorCopy);
		
		/*
		 * new Strings are created for Type and LinePattern
		 */
		newSigmet.setPgenCategory(new String(this.getPgenCategory()));
		newSigmet.setPgenType(new String(this.getPgenType()));
		newSigmet.setParent(this.getParent());
		newSigmet.setType(this.getType());
		newSigmet.setWidth(this.getWidth());
		
		newSigmet.setEditableAttrArea(this.getEditableAttrArea());
		newSigmet.setEditableAttrIssueOffice(this.getEditableAttrIssueOffice());
		newSigmet.setEditableAttrFromLine(this.getEditableAttrFromLine());
		newSigmet.setEditableAttrId(this.getEditableAttrId());
		newSigmet.setEditableAttrSeqNum(this.getEditableAttrSeqNum());
		
		//CCFP
		newSigmet.setEditableAttrFreeText(this.getEditableAttrFreeText());
		newSigmet.setEditableAttrFromLine(this.getEditableAttrFromLine());
		newSigmet.setEditableAttrStartTime(this.getEditableAttrStartTime());
		newSigmet.setEditableAttrEndTime(this.getEditableAttrEndTime());
		newSigmet.setEditableAttrPhenom(this.getEditableAttrPhenom());
		newSigmet.setEditableAttrPhenom2(this.getEditableAttrPhenom2());
		newSigmet.setEditableAttrPhenomLat(this.getEditableAttrPhenomLat());
		newSigmet.setEditableAttrPhenomLon(this.getEditableAttrPhenomLon());
		newSigmet.setEditableAttrPhenomSpeed(this.getEditableAttrPhenomSpeed());
		newSigmet.setEditableAttrPhenomDirection(this.getEditableAttrPhenomDirection());
		
		newSigmet.setEditableAttrArea(this.getEditableAttrArea());
		newSigmet.setEditableAttrRemarks(this.getEditableAttrRemarks());
		newSigmet.setEditableAttrPhenomName(this.getEditableAttrPhenomName());
		newSigmet.setEditableAttrPhenomPressure(this.getEditableAttrPhenomPressure());
		newSigmet.setEditableAttrPhenomMaxWind(this.getEditableAttrPhenomMaxWind());
		newSigmet.setEditableAttrTrend(this.getEditableAttrTrend());
		newSigmet.setEditableAttrMovement(this.getEditableAttrMovement());
		newSigmet.setEditableAttrLevel(this.getEditableAttrLevel());
		newSigmet.setEditableAttrLevelInfo1(this.getEditableAttrLevelInfo1());
		newSigmet.setEditableAttrLevelInfo2(this.getEditableAttrLevelInfo2());
		newSigmet.setEditableAttrLevelText1(this.getEditableAttrLevelText1());
		newSigmet.setEditableAttrLevelText2(this.getEditableAttrLevelText2());
		newSigmet.setEditableAttrFir(this.getEditableAttrFir());
		
		return newSigmet;
	}



	public String getEditableAttrStatus() {
		return editableAttrStatus;
	}



	public void setEditableAttrStatus(String editableAttrStatus) {
		this.editableAttrStatus = editableAttrStatus;
	}



	public String getEditableAttrStartTime() {
		return editableAttrStartTime;
	}



	public void setEditableAttrStartTime(String editableAttrStartTime) {
		this.editableAttrStartTime = editableAttrStartTime;
	}



	public String getEditableAttrEndTime() {
		return editableAttrEndTime;
	}



	public void setEditableAttrEndTime(String editableAttrEndTime) {
		this.editableAttrEndTime = editableAttrEndTime;
	}



	public String getEditableAttrRemarks() {
		return editableAttrRemarks;
	}



	public void setEditableAttrRemarks(String editableAttrRemarks) {
		this.editableAttrRemarks = editableAttrRemarks;
	}



	public String getEditableAttrPhenom() {
		return editableAttrPhenom;
	}



	public void setEditableAttrPhenom(String editableAttrPhenom) {
		this.editableAttrPhenom = editableAttrPhenom;
	}



	public String getEditableAttrPhenom2() {
		return editableAttrPhenom2;
	}



	public void setEditableAttrPhenom2(String editableAttrPhenom2) {
		this.editableAttrPhenom2 = editableAttrPhenom2;
	}



	public String getEditableAttrPhenomName() {
		return editableAttrPhenomName;
	}



	public void setEditableAttrPhenomName(String editableAttrPhenomName) {
		this.editableAttrPhenomName = editableAttrPhenomName;
	}



	public String getEditableAttrPhenomLat() {
		return editableAttrPhenomLat;
	}



	public void setEditableAttrPhenomLat(String editableAttrPhenomLat) {
		this.editableAttrPhenomLat = editableAttrPhenomLat;
	}



	public String getEditableAttrPhenomLon() {
		return editableAttrPhenomLon;
	}



	public void setEditableAttrPhenomLon(String editableAttrPhenomLon) {
		this.editableAttrPhenomLon = editableAttrPhenomLon;
	}



	public String getEditableAttrPhenomPressure() {
		return editableAttrPhenomPressure;
	}



	public void setEditableAttrPhenomPressure(String editableAttrPhenomPressure) {
		this.editableAttrPhenomPressure = editableAttrPhenomPressure;
	}



	public String getEditableAttrPhenomMaxWind() {
		return editableAttrPhenomMaxWind;
	}



	public void setEditableAttrPhenomMaxWind(String editableAttrPhenomMaxWind) {
		this.editableAttrPhenomMaxWind = editableAttrPhenomMaxWind;
	}



	public String getEditableAttrFreeText() {
		return editableAttrFreeText;
	}



	public void setEditableAttrFreeText(String editableAttrFreeText) {
		this.editableAttrFreeText = editableAttrFreeText;
	}



	public String getEditableAttrTrend() {
		return editableAttrTrend;
	}



	public void setEditableAttrTrend(String editableAttrTrend) {
		this.editableAttrTrend = editableAttrTrend;
	}



	public String getEditableAttrMovement() {
		return editableAttrMovement;
	}



	public void setEditableAttrMovement(String editableAttrMovement) {
		this.editableAttrMovement = editableAttrMovement;
	}



	public String getEditableAttrPhenomSpeed() {
		return editableAttrPhenomSpeed;
	}



	public void setEditableAttrPhenomSpeed(String editableAttrPhenomSpeed) {
		this.editableAttrPhenomSpeed = editableAttrPhenomSpeed;
	}



	public String getEditableAttrPhenomDirection() {
		return editableAttrPhenomDirection;
	}



	public void setEditableAttrPhenomDirection(String editableAttrPhenomDirection) {
		this.editableAttrPhenomDirection = editableAttrPhenomDirection;
	}



	public String getEditableAttrLevel() {
		return editableAttrLevel;
	}



	public void setEditableAttrLevel(String editableAttrLevel) {
		this.editableAttrLevel = editableAttrLevel;
	}



	public String getEditableAttrLevelInfo1() {
		return editableAttrLevelInfo1;
	}



	public void setEditableAttrLevelInfo1(String editableAttrLevelInfo1) {
		this.editableAttrLevelInfo1 = editableAttrLevelInfo1;
	}



	public String getEditableAttrLevelInfo2() {
		return editableAttrLevelInfo2;
	}



	public void setEditableAttrLevelInfo2(String editableAttrLevelInfo2) {
		this.editableAttrLevelInfo2 = editableAttrLevelInfo2;
	}



	public String getEditableAttrLevelText1() {
		return editableAttrLevelText1;
	}



	public void setEditableAttrLevelText1(String editableAttrLevelText1) {
		this.editableAttrLevelText1 = editableAttrLevelText1;
	}



	public String getEditableAttrLevelText2() {
		return editableAttrLevelText2;
	}



	public void setEditableAttrLevelText2(String editableAttrLevelText2) {
		this.editableAttrLevelText2 = editableAttrLevelText2;
	}



	public String getEditableAttrFir() {
		return editableAttrFir;
	}



	public void setEditableAttrFir(String editableAttrFir) {
		this.editableAttrFir = editableAttrFir;
	}

}
