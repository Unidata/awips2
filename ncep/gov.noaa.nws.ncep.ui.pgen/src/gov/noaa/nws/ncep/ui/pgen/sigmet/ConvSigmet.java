/*
 * gov.noaa.nws.ncep.ui.pgen.sigmet.ConvSigmet
 * 
 * December 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.sigmet;

import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import java.awt.Color;
import java.util.ArrayList;

import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;

/**
 * Element for ConvSigmet
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/09		182			G. Zhang 	Initial Creation. 
 *
 * </pre>
 * 
 * @author	gzhang
 */
@ElementOperations ( {Operation.COPY_MOVE, Operation.EXTRAPOLATE, Operation.DELETE_PART, Operation.DELETE_POINT,
	                  Operation.ADD_POINT, Operation.MODIFY} )
public class ConvSigmet extends Sigmet{ 
	
	public final static String SIGMET_PGEN_CATEGORY = "Sigmet"; 	 
	public final static String SIGMET_PGEN_TYPE = "CONV_SIGMET";

	
	@Override
	public DrawableElement copy() {
		/*
		 * create a new Line object and initially set its attributes to this one's
		 */
		ConvSigmet newSigmet = new ConvSigmet();
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
		newSigmet.setEditableAttrFromLine(this.getEditableAttrFromLine());
		newSigmet.setEditableAttrId(this.getEditableAttrId());
		newSigmet.setEditableAttrSeqNum(this.getEditableAttrSeqNum());
		
		//for CCFP 		
		newSigmet.setFillPattern(this.getFillPattern());
		
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

}
