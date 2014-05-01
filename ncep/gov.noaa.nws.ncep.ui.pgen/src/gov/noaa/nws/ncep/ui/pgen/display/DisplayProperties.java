/*
 * DisplayProperties
 * 
 * Date created: 08 DECEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.display;

import java.awt.Color;

/**
 * Contains properties determining how PGEN drawable Elements should be displayed, such as 
 * Layer attributes.
 * @author sgilbert
 *
 */
public class DisplayProperties {

	/**
	 * Color mode, color, and fill mode used to draw all elements in a layer
	 */
	private Boolean layerMonoColor = false;
	private Color   layerColor = null;	
	private Boolean layerFilled = false;
	/**
	 * 
	 */
	public DisplayProperties() {
		// TODO Auto-generated constructor stub
	}
	
	
	/**
	 * @param layerMonoColor
	 * @param layerColor
	 * @param layerFilled
	 */
	public DisplayProperties(Boolean layerMonoColor, Color layerColor,
			Boolean layerFilled) {
		this.layerMonoColor = layerMonoColor;
		this.layerColor = layerColor;
		this.layerFilled = layerFilled;
	}
	/**
	 * @return the layerMonoColor
	 */
	public Boolean getLayerMonoColor() {
		return layerMonoColor;
	}
	/**
	 * @param layerMonoColor the layerMonoColor to set
	 */
	public void setLayerMonoColor(Boolean layerMonoColor) {
		this.layerMonoColor = layerMonoColor;
	}
	/**
	 * @return the layerColor
	 */
	public Color getLayerColor() {
		return layerColor;
	}
	/**
	 * @param layerColor the layerColor to set
	 */
	public void setLayerColor(Color layerColor) {
		this.layerColor = layerColor;
	}
	/**
	 * @return the layerFilled
	 */
	public Boolean getLayerFilled() {
		return layerFilled;
	}
	/**
	 * @param layerFilled the layerFilled to set
	 */
	public void setLayerFilled(Boolean layerFilled) {
		this.layerFilled = layerFilled;
	}
	
	/*
	 * Two DisplayProperties are equal if their instance fields contain the
	 * same values.  The LayerColor field can be null.
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {

		if ( obj == null ) return false;
		if ( ! (obj instanceof DisplayProperties) ) return false;
		DisplayProperties dp = (DisplayProperties)obj;

		boolean sameColor;
		if ( (this.layerColor==null) && (dp.getLayerColor()==null) )
			sameColor = true;
		else if ( (this.layerColor==null) || (dp.getLayerColor()==null) )
			sameColor = false;
		else 
			sameColor = this.layerColor.equals( dp.getLayerColor() );
		
		if ( sameColor &&
				this.layerMonoColor.equals( dp.getLayerMonoColor() ) &&
				this.layerFilled.equals( dp.getLayerFilled() ) ) return true;
		else
			return false;
	}
	
	
}
