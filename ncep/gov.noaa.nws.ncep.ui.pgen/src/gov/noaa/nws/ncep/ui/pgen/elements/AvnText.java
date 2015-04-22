/*
 * AvnText
 * 
 * Date created: 29 JULY 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements;

import java.awt.Color;

import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.IAvnText;

/**
 * 
 * @author sgilbert
 *
 */
@ElementOperations ( {Operation.COPY_MOVE, Operation.EXTRAPOLATE} )
public class AvnText extends Text implements IAvnText {

	private AviationTextType avnTextType;
	private String symbolPatternName = null;
	private String topValue = null;
	private String bottomValue = null;
	
	public AvnText() {
		//default
	}
	
	public AvnText(Coordinate[] range, String fontName, float fontSize,
			TextJustification justification, Coordinate position,
			AviationTextType textType, String topValue, String bottomValue,
			FontStyle style, Color textColor, String symbolPatternName,
			String pgenCategory, String pgenType ) {
		
		super( range, fontName, fontSize, justification, position, 0.0, TextRotation.SCREEN_RELATIVE, null, 
				style, textColor, 0, 0, false, DisplayType.NORMAL, pgenCategory, pgenType );
				
		this.avnTextType = textType;
		this.topValue = topValue;
		this.bottomValue = bottomValue;
		this.symbolPatternName = symbolPatternName;
		
	}
	
	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IAvnText#getAvnTextType()
	 */
	@Override
	public AviationTextType getAvnTextType() {
		return avnTextType;
	}

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IAvnText#getBottomValue()
	 */
	@Override
	public String getBottomValue() {
		return bottomValue;
	}

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IAvnText#getSymbolPatternName()
	 */
	@Override
	public String getSymbolPatternName() {
		return symbolPatternName;
	}

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IAvnText#getTopValue()
	 */
	@Override
	public String getTopValue() {
		return topValue;
	}

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IAvnText#hasBottomValue()
	 */
	@Override
	public boolean hasBottomValue() {
		if ( bottomValue == null ) 
			return false;
		else 
			return true;
	}

	/**
	 * @param avnTextType the avnTextType to set
	 */
	public void setAvnTextType(AviationTextType avnTextType) {
		if (avnTextType != null  ){
			this.avnTextType = avnTextType;
		}
	}

	/**
	 * @param symbolPatternName the symbolPatternName to set
	 */
	public void setSymbolPatternName(String symbolPatternName) {
		if (symbolPatternName != null ){
			this.symbolPatternName = symbolPatternName;
		}
	}

	/**
	 * @param topValue the topValue to set
	 */
	public void setTopValue(String topValue) {
		if ( topValue != null ){
			this.topValue = topValue;
		}
	}

	/**
	 * @param bottomValue the bottomValue to set
	 */
	public void setBottomValue(String bottomValue) {
		if ( bottomValue != null ) {
			this.bottomValue = bottomValue;
		}
	}

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IAvnText#hasSymbolPattern()
	 */
	@Override
	public boolean hasSymbolPattern() {
		if ( symbolPatternName == null ) 
			return false;
		else 
			return true;
	}

	/**
	 * Update the attributes
	 */
	@Override
	public void update(IAttribute iattr) {
		if ( iattr instanceof IAvnText ){
			super.update(iattr);
			IAvnText attr = (IAvnText) iattr;
			this.setAvnTextType(attr.getAvnTextType());
			this.setTopValue(attr.getTopValue());
			this.setBottomValue(attr.getBottomValue());
			this.setSymbolPatternName(attr.getSymbolPatternName());
		}
	}
	
	/**
	 * @return the string
	 */
	public String toString() {
		StringBuilder	result = new StringBuilder( getClass().getSimpleName());
      
        result.append( "\nCategory:\t" + pgenCategory + "\n" );
        result.append( "Type:\t" + pgenType + "\n" );
        result.append("Text Type:\t" + avnTextType + "\n");
        result.append("Top Value:\t" + topValue + "\n");
        result.append("Bottom Value:\t" + bottomValue + "\n");
        result.append("Symbol Pattern:\t" + symbolPatternName + "\n");
        result.append("Color:\t" + colors[0] + "\n");
        result.append("FontName:\t" + getFontName() + "\n");
        result.append("FontSize:\t" +  getFontSize() + "\n");
        result.append("Justification:\t" + getJustification() + "\n");
        result.append("Style:\t" + getStyle() + "\n");
       
        result.append("Position:\t" + location.y + "\t" + location.x + "\n"); 
           	 	       		
		return result.toString();
	}	
	
	/**
	 * Creates a copy of this object.  This is a deep copy and new objects are
	 * created so that we are not just copying references of objects
	 */
	@Override
	public DrawableElement copy() {

		/*
		 * create a new Text object and initially set its attributes to this one's
		 */
		AvnText newText = new AvnText();
		newText.update(this);
		
		/*
		 * Set new Color, Strings and Coordinate so that we don't just set references to this
		 * object's attributes.
		 */
		newText.setColors(new Color[] { new Color(this.getColors()[0].getRed(),
	            this.getColors()[0].getGreen(),
	            this.getColors()[0].getBlue())   });
		newText.setLocation(new Coordinate(this.getLocation()) );
		newText.setFontName(new String(this.getFontName()));

		newText.setSymbolPatternName(new String(this.getSymbolPatternName()));
		newText.setTopValue(new String(this.getTopValue()));
		newText.setBottomValue(new String(this.getBottomValue()));
		
		newText.setPgenCategory(new String(this.getPgenCategory()));
		newText.setPgenType(new String(this.getPgenType()));
		newText.setParent(this.getParent());
		
		return newText;
	}
	
}
