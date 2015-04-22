/*
 * Text
 * 
 * Date created: 06 APRIL 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import java.awt.Color;

import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.IText;
	
/**
 * Class used to represent a Text drawable element.
 * @author sgilbert
 *
 * 05/09        #42         S. Gilbert  Added pgenType and pgenCategory to constructors and copy()
 * 03/10		#			M.Laryukhin	Operation.ROTATE is added.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 *
 */
@ElementOperations ( {Operation.COPY_MOVE, Operation.EXTRAPOLATE, Operation.ROTATE} )
public class Text extends SinglePointElement implements IText {

	/*
	 * name of Font
	 */
	private String fontName;
	
	/*
	 * size of Font
	 */
	private float fontSize;
	
	/*
	 * Justify text String Right, Left, or Center
	 */
	private TextJustification justification;
	
	/*
	 * angle of rotation to display String relative to + Xdirection
	 */
	private double rotation;
	
	/*
	 * Text should be rotated relative to geograpohic North or X direction of Screen coordinates
	 */
	private TextRotation rotationRelativity;
	
	/*
	 * Text Strings to Display:
	 * Each element of String[] is displayed on separate line.
	 */
	private String[] text;
	
	/*
	 * Regular, Bold, Italic, Bold-Italic
	 */
	private FontStyle style;
	
	/*
	 * Half-character offset in X-direction
	 */
	private int xOffset;
	
	/*
	 * Half-character offset in Y direction
	 */
	private int yOffset;
	
	/*
	 * Mask text background
	 */
	private Boolean mask;
	
	/*
	 * outline box around text
	 */
	private DisplayType displayType;

	/*
	 * Hide or display
	 */
	private Boolean hide;
	
	/*
	 * Auto-placed or manually-placed
	 */
	private Boolean auto;
	
	public Text() {
		//default
	}
	
	/**
	 * Constructor to set all attributes of the Text element
	 * @param fontName Name of the font to display
	 * @param fontSize Size of the font to display
	 * @param justification Specified where text is relative to position
	 *                       @see gov.noaa.nws.ncep.ui.pgen.display.IText.TextJustification
	 * @param position - Lat/lon location specifying where to display the text String
	 * @param rotation - display text at this rotation angle relative to +X direction.
	 * @param rotationRelativity - rotation angle is relative to North or Screen coordinates
	 *                       @see gov.noaa.nws.ncep.ui.pgen.display.IText.TextRotation
	 * @param text  The text Strings to display.  The text strings in each array element are displayed 
	 *               on a different line
	 * @param style  The font style to use.  @see gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle
	 * @param textColor  Color in which text string should be displayed
	 * @param offset Half-character offset in the X direction applied to @see #position
	 * @param offset2 Half-character offset in the Y direction applied to @see #position
	 * @param mask  Create a background mask behind the text Strings
	 * @param outline  type of display around the text Strings.
	 */
	public Text(Coordinate[] range, String fontName, float fontSize,
			TextJustification justification, Coordinate position,
			double rotation, TextRotation rotationRelativity, String[] text,
			FontStyle style, Color textColor, int offset, int offset2,
			boolean mask, DisplayType outline, String pgenCategory, String pgenType ) {
		super( range, new Color[]{textColor}, 1.0f, 0.0, false, position, pgenCategory, pgenType );
		this.fontName = fontName;
		this.fontSize = fontSize;
		this.justification = justification;
		//this.position = position;
		this.rotation = rotation;
		this.rotationRelativity = rotationRelativity;
		this.text = text;
		this.style = style;
		//this.textColor = textColor;
		xOffset = offset;
		yOffset = offset2;
		this.mask = mask;
		this.displayType = outline;
		
		this.hide = false;
		this.auto = false;
	}

	/**
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IText#getFontName()
	 */
	@Override
	public String getFontName() {
		return fontName;
	}

	/**
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IText#getFontSize()
	 */
	@Override
	public float getFontSize() {
		return fontSize;
	}

	/**
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IText#getJustification()
	 */
	@Override
	public TextJustification getJustification() {
		return justification;
	}

	/**
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IText#getPosition()
	 */
	@Override
	public Coordinate getPosition() {
		return getLocation();
	}

	/**
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IText#getRotation()
	 */
	@Override
	public double getRotation() {
		return rotation;
	}

	/**
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IText#getRotationRelativity()
	 */
	@Override
	public TextRotation getRotationRelativity() {
		return rotationRelativity;
	}

	/**
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IText#getString()
	 */
	@Override
	public String[] getString() {
		return text;
	}

	/**
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IText#getStyle()
	 */
	@Override
	public FontStyle getStyle() {
		return style;
	}

	/**
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IText#getTextColor()
	 */
	@Override
	public Color getTextColor() {
		return colors[0];
	}

	/**
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IText#getXOffset()
	 */
	@Override
	public int getXOffset() {
		return xOffset;
	}

	/**
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IText#getYOffset()
	 */
	@Override
	public int getYOffset() {
		return yOffset;
	}

	/**
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IText#maskText()
	 */
	@Override
	public Boolean maskText() {
		return mask;
	}

	/**
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IText#getDisplayType()
	 */
	@Override
	public DisplayType getDisplayType() {
		return displayType;
	}

	/**
	 * @param fontName the fontName to set
	 */
	public void setFontName(String fontName) {
		if ( fontName != null ){
			this.fontName = fontName;
		}
	}

	/**
	 * @param fontSize the fontSize to set
	 */
	public void setFontSize(float fontSize) {
		if ( !(new Float(fontSize).isNaN())){
			this.fontSize = fontSize;
		}
	}

	/**
	 * @param justification the justification to set
	 */
	public void setJustification(TextJustification justification) {
		if ( justification != null ){
			this.justification = justification;
		}
	}

	/**
	 * @param rotation the rotation to set
	 */
	public void setRotation(double rotation) {
		if ( !(new Double(rotation).isNaN()) ){
			this.rotation = rotation;
		}
	}

	/**
	 * @param rotationRelativity the rotationRelativity to set
	 */
	public void setRotationRelativity(TextRotation rotationRelativity) {
		if (rotationRelativity != null ){
			this.rotationRelativity = rotationRelativity;
		}
	}

	/**
	 * @param style the style to set
	 */
	public void setStyle(FontStyle style) {
		if ( style != null ){
			this.style = style;
		}
	}

	/**
	 * @param offset the xOffset to set
	 */
	public void setXOffset(int offset) {
		xOffset = offset;
	}

	/**
	 * @param offset the yOffset to set
	 */
	public void setYOffset(int offset) {
		yOffset = offset;
	}

	/**
	 * @param mask the mask to set
	 */
	public void setMask(Boolean mask) {
		if ( mask != null ){
			this.mask = mask;
		}
	}

	/**
	 * @param outline the outline to set
	 */
	public void setDisplayType(DisplayType outline) {
		if ( outline != null ) {
			this.displayType = outline;
		}
	}
	
	/**
	 * @param hide the hide to set
	 */
	public void setHide(Boolean hide) {
		this.hide = hide;
	}

	/**
	 * @return the hide
	 */
	public Boolean getHide() {
		return hide;
	}

	/**
	 * @param auto the auto to set
	 */
	public void setAuto(Boolean auto) {
		this.auto = auto;
	}

	/**
	 * @return the auto
	 */
	public Boolean getAuto() {
		return auto;
	}

	/**
	 * @return the text
	 */
	public String[] getText() {
		return text;
	}

	/**
	 * @param text the text to set
	 */
	public void setText(String[] text) {
		if ( text != null ){
			this.text = text;
		}
	}

	/**
	 * Update the attributes
	 */
	@Override
	public void update(IAttribute iattr) {
		if ( iattr instanceof IText ){
			super.update(iattr);
			IText attr = (IText)iattr;
			this.setFontName(attr.getFontName());
			this.setFontSize(attr.getFontSize());
			this.setJustification(attr.getJustification());
			this.setRotation(attr.getRotation());
			this.setRotationRelativity(attr.getRotationRelativity());
			this.setStyle(attr.getStyle());
			this.setXOffset(attr.getXOffset());
			this.setYOffset(attr.getYOffset());
			this.setMask(attr.maskText());
			this.setDisplayType(attr.getDisplayType());
			this.setHide(attr.getHide());
			this.setAuto(attr.getAuto());
			this.setText(attr.getString());
		}
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
		Text newText = new Text();
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
		
		/*
		 * new text Strings are created and set, so we don't just set 
		 * references
		 */
		String[] textCopy = new String[this.getText().length];
		for (int i=0; i<this.getText().length; i++) {
			textCopy[i] = new String(this.getText()[i]);
		}
		newText.setText(textCopy);
		
		newText.setPgenCategory(new String(this.getPgenCategory()));
		newText.setPgenType(new String(this.getPgenType()));
		
		newText.setHide(this.hide);
		newText.setAuto(this.auto);
		
		newText.setParent(this.getParent());
		
		return newText;
	}
	
	/**
	 * @return the string
	 */
	public String toString() {
		StringBuilder	result = new StringBuilder( getClass().getSimpleName());
      
        result.append( "\nCategory:\t" + pgenCategory + "\n" );
        result.append( "Type:\t" + pgenType + "\n" );
        if ( text != null ) {
            for ( String st : text ) {
                result.append( st +  "\n" );  
            }
        }
        result.append("Color:\t" + colors[0] + "\n");
        result.append("FontName:\t" + fontName + "\n");
        result.append("FontSize:\t" +  fontSize + "\n");
        result.append("Justification:\t" + justification + "\n");
        result.append("Rotation:\t" + rotation + "\n");
        result.append("RotationRelativity:\t" + rotationRelativity  + "\n");
        result.append("Style:\t" + style + "\n");
        result.append("XOffset:\t" + xOffset + "\n");
        result.append("YOffset:\t" + yOffset + "\n");
        result.append("Mask:\t" + mask + "\n");
        result.append("Outline:\t" + displayType + "\n");
        result.append("Hide:\t" + hide + "\n");
        result.append("Auto:\t" + hide + "\n");
        
        if ( location != null ) {
            result.append("Position:\t" + location.y + "\t" + location.x + "\n"); 
        }
        else {
            result.append("Position:\t not defined \n");        	
        }
           	 	       		
		return result.toString();
	}	
	
}
