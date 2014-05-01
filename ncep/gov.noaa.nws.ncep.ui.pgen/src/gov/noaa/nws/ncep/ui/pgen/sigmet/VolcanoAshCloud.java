/*
 * gov.noaa.nws.ncep.ui.pgen.sigmet
 * 
 * Janurary 2010
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
 * The class for Volcano Ash Cloud element 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		#165		G. Zhang   	Initial Creation.
 *
 * </pre>
 * 
 * @author	G. Zhang
 */

@ElementOperations ( {Operation.COPY_MOVE, Operation.EXTRAPOLATE, Operation.DELETE_PART, Operation.DELETE_POINT,
    Operation.ADD_POINT, Operation.INTERPOLATE, Operation.MODIFY} )
public class VolcanoAshCloud extends Sigmet {
	
	public static final String SIGMET_PGEN_TYPE = "VACL_SIGMET";



	@Override
	public DrawableElement copy() {
		/*
		 * create a new Line object and initially set its attributes to this one's
		 */
		VolcanoAshCloud newSigmet = new VolcanoAshCloud();
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
		String fhr = this.getEditableAttrFreeText(); 
		if(fhr==null && this.getAttr()!=null) 
			fhr = ((Sigmet)this.getAttr()).getEditableAttrFreeText();
		newSigmet.setEditableAttrFreeText(fhr);//20100818 workshop issue2
		newSigmet.setEditableAttrArea(this.getEditableAttrArea());
		newSigmet.setEditableAttrFromLine(this.getEditableAttrFromLine());
		newSigmet.setEditableAttrId(this.getEditableAttrId());
		newSigmet.setEditableAttrSeqNum(this.getEditableAttrSeqNum());
		
		return newSigmet;
	}
	
	@Override
	public FillPattern getFillPattern(){
		return FillPattern.FILL_PATTERN_2;
	}


}
