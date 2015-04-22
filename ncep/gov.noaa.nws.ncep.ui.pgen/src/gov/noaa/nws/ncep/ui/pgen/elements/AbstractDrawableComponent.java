/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenSinglePointDrawingTool
 * 
 * 20 June 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements;

import java.awt.Color;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Abstract super class for DrawableElement and DECollection
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/09		#116			B. Yin   	Initial Creation.
 * 06/09		#135			B. Yin   	Added a parent field.
 * 04/11		#422			B. Yin		added startTime and endTime
 * 
 * </pre>
 * 
 * @author	B. Yin
 */

public abstract class AbstractDrawableComponent {
	
	protected String          pgenCategory;
	protected String          pgenType;	
	protected AbstractDrawableComponent parent;
	
	protected Calendar		startTime;
	protected Calendar		endtime;

	public abstract void setColors(Color[] colors);
	public abstract List<Coordinate> getPoints();
	public abstract Iterator<DrawableElement> createDEIterator(); 
	public abstract AbstractDrawableComponent copy();
	public abstract DrawableElement getPrimaryDE();
	public abstract String getName();

	
	/**
	 * @return the pgenCategory
	 */
	public String getPgenCategory() {
		return pgenCategory;
	}

	/**
	 * @param pgenCategory the pgenCategory to set
	 */
	public void setPgenCategory(String pgenCategory) {
		this.pgenCategory = pgenCategory;
	}

	/**
	 * @return the pgenType
	 */
	public String getPgenType() {
		return pgenType;
	}

	/**
	 * @param pgenType the pgenType to set
	 */
	public void setPgenType(String pgenType) {
		this.pgenType = pgenType;
	}

	public AbstractDrawableComponent getParent(){
		return parent;
	}
	
	public void setParent(AbstractDrawableComponent parent ){
		this.parent = parent;
	}
	
	public boolean isEmpty(){
		return false;
	}
	
	public String getForecastHours(){
		return "";
	}
	public Calendar getStartTime() {
		return startTime;
	}
	public void setStartTime(Calendar startTime) {
		this.startTime = startTime;
	}
	public Calendar getEndtime() {
		return endtime;
	}
	public void setEndtime(Calendar endtime) {
		this.endtime = endtime;
	}
}
