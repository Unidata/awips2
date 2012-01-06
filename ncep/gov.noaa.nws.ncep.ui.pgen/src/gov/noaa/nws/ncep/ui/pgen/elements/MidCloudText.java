/*
 * MidCloudText
 * 
 * Date created: 14 JANUARY 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements;

import java.awt.Color;

import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.IMidCloudText;

/**
 * 
 * @author sgilbert
 *
 */
@ElementOperations ( {Operation.COPY_MOVE, Operation.EXTRAPOLATE} )
public class MidCloudText extends Text implements IMidCloudText {

	private String cloudTypes;
	//private String cloudAmounts = null;
	private String turbulencePattern;
	private String turbulenceLevels;
	private String icingPattern;
	private String icingLevels;
	private String tstormTypes;
	private String tstormLevels;
	private boolean twoColumns = true;
	
	public MidCloudText() {
		//default
	}
	
	public MidCloudText(Coordinate[] range, String fontName, float fontSize,
			TextJustification justification, Coordinate position,
			String cloudTypes, String cloudAmounts, String turbulencePattern,
			String turbulenceLevels, String icingPattern, String icingLevels,
			String tstormTypes, String tstormLevels, 
			FontStyle style, Color textColor, String pgenCategory, String pgenType ) {
		
		super( range, fontName, fontSize, justification, position, 0.0, TextRotation.SCREEN_RELATIVE, null, 
				style, textColor, 0, 0, false, DisplayType.NORMAL, pgenCategory, pgenType );
				
		this.cloudTypes = cloudTypes;
	//	this.cloudAmounts = cloudAmounts;
		this.turbulencePattern = turbulencePattern;
		this.turbulenceLevels = turbulenceLevels;
		this.icingPattern = icingPattern;
		this.icingLevels = icingLevels;
		this.tstormTypes = tstormTypes;
		this.tstormLevels = tstormLevels;
		
	}
	
	/**
	 * @return the cloudTypes
	 */
	public String getCloudTypes() {
		return cloudTypes;
	}

	/**
	 * @param cloudTypes the cloudTypes to set
	 */
	public void setCloudTypes(String cloudTypes) {
		this.cloudTypes = cloudTypes;
	}

	/**
	 * @return the cloudAmounts
	 */
	public String getCloudAmounts() {
		return null;
		//		return cloudAmounts;
	}

	/**
	 * @param cloudAmounts the cloudAmounts to set
	 */
/*	public void setCloudAmounts(String cloudAmounts) {
		this.cloudAmounts = cloudAmounts;
	}
*/
	/**
	 * @return the turbulencePattern
	 */
	public String getTurbulencePattern() {
		return turbulencePattern;
	}

	/**
	 * @param turbulencePattern the turbulencePattern to set
	 */
	public void setTurbulencePattern(String turbulencePattern) {
		this.turbulencePattern = turbulencePattern;
	}

	/**
	 * @return the turbulenceLevels
	 */
	public String getTurbulenceLevels() {
		return turbulenceLevels;
	}

	/**
	 * @param turbulenceLevels the turbulenceLevels to set
	 */
	public void setTurbulenceLevels(String turbulenceLevels) {
		this.turbulenceLevels = turbulenceLevels;
	}

	/**
	 * @return the icingPattern
	 */
	public String getIcingPattern() {
		return icingPattern;
	}

	/**
	 * @param icingPattern the icingPattern to set
	 */
	public void setIcingPattern(String icingPattern) {
		this.icingPattern = icingPattern;
	}

	/**
	 * @return the icingLevels
	 */
	public String getIcingLevels() {
		return icingLevels;
	}

	/**
	 * @param icingLevels the icingLevels to set
	 */
	public void setIcingLevels(String icingLevels) {
		this.icingLevels = icingLevels;
	}

	/**
	 * @return the tstormTypes
	 */
	public String getTstormTypes() {
		return tstormTypes;
	}

	/**
	 * @param tstormTypes the tstormTypes to set
	 */
	public void setTstormTypes(String tstormTypes) {
		this.tstormTypes = tstormTypes;
	}

	/**
	 * @return the tstormLevels
	 */
	public String getTstormLevels() {
		return tstormLevels;
	}

	/**
	 * @param tstormLevels the tstormLevels to set
	 */
	public void setTstormLevels(String tstormLevels) {
		this.tstormLevels = tstormLevels;
	}

	/**
	 * Update the attributes, overwrite only if new attribute is non-null
	 */
	@Override
	public void update(IAttribute attr) {
		super.update(attr);
		
		if ( attr instanceof IMidCloudText ) {
			IMidCloudText mid = (IMidCloudText)attr;
			if (mid.getCloudTypes()!=null)        this.setCloudTypes(mid.getCloudTypes());
//			if (mid.getCloudAmounts()!=null)      this.setCloudAmounts(mid.getCloudAmounts());
			if (mid.getTurbulencePattern()!=null) this.setTurbulencePattern(mid.getTurbulencePattern());
			if (mid.getTurbulenceLevels()!=null)  this.setTurbulenceLevels(mid.getTurbulenceLevels());
			if (mid.getIcingPattern()!=null)      this.setIcingPattern(mid.getIcingPattern());
			if (mid.getIcingLevels()!=null)       this.setIcingLevels(mid.getIcingLevels());
			if (mid.getTstormTypes()!=null)       this.setTstormTypes(mid.getTstormTypes());
			if (mid.getTstormLevels()!=null)      this.setTstormLevels(mid.getTstormLevels());
		}
	}
	
	/**
	 * @return the string
	 */
	public String toString() {
		StringBuilder	result = new StringBuilder( getClass().getSimpleName());
      
        result.append( "\nCategory:\t" + pgenCategory + "\n" );
        result.append( "Type:\t" + pgenType + "\n" );
        result.append("Cloud Types:\t" + cloudTypes + "\n");
  //      result.append("Cloud Amounts:\t" + cloudAmounts + "\n");
        result.append("Turb Pattern:\t" + turbulencePattern + "\n");
        result.append("Turb Levels:\t" + turbulenceLevels + "\n");
        result.append("Icing Pattern:\t" + icingPattern + "\n");
        result.append("Icing Levels:\t" + icingLevels + "\n");
        result.append("Tstorm Types:\t" + tstormTypes + "\n");
        result.append("Tstorm Levels:\t" + tstormLevels + "\n");
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
		MidCloudText newText = new MidCloudText();
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

		newText.setCloudTypes(new String(this.getCloudTypes()));
	//	newText.setCloudAmounts(new String(this.getCloudAmounts()));
		newText.setTurbulencePattern(new String(this.getTurbulencePattern()));
		newText.setTurbulenceLevels(new String(this.getTurbulenceLevels()));
		newText.setIcingPattern(new String(this.getIcingPattern()));
		newText.setIcingLevels(new String(this.getIcingLevels()));
		newText.setTstormTypes(new String(this.getTstormTypes()));
		newText.setTstormLevels(new String(this.getTstormLevels()));
		
		newText.setPgenCategory(new String(this.getPgenCategory()));
		newText.setPgenType(new String(this.getPgenType()));
		newText.setParent(this.getParent());
		
		newText.setTwoColumns(this.isTwoColumns());
		
		return newText;
	}

	@Override
	public boolean hasIcing() {
		if ( icingLevels == null || icingLevels.isEmpty() ) return false;
		else return true;
	}

	@Override
	public boolean hasTstorm() {
		if ( tstormTypes == null || tstormTypes.isEmpty() ) return false;
		else return true;
	}

	@Override
	public boolean hasTurbulence() {
		if ( turbulenceLevels == null || turbulenceLevels.isEmpty() ) return false;
		else return true;
	}

	public void setTwoColumns(boolean twoColumns) {
		this.twoColumns = twoColumns;
	}

	public boolean isTwoColumns() {
		return twoColumns;
	}
	
}
