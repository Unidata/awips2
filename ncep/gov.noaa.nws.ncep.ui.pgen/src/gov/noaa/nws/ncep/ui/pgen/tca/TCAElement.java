/*
 * TCAElement
 * 
 * Date created 03 NOVEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tca;

import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * @author sgilbert
 *
 */
public class TCAElement extends DrawableElement implements ITca {

	private int stormNumber;
	private String issueStatus;
	private String basin;
	private String advisoryNumber;
	private String stormName;
	private String stormType;
	private Calendar advisoryTime;
	private String timeZone;
	private Coordinate textLocation;
	private ArrayList<TropicalCycloneAdvisory> advisories;
 	
	/**
	 * 
	 */
	public TCAElement() {
		advisories = new ArrayList<TropicalCycloneAdvisory>();
		textLocation = new Coordinate(-80.4,25.8);
	}

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.ui.pgen.tca.ITca#getAdvisories()
	 */
	@Override
	public ArrayList<TropicalCycloneAdvisory> getAdvisories() {
		return advisories;
	}

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.ui.pgen.tca.ITca#getAdvisoryNumber()
	 */
	@Override
	public String getAdvisoryNumber() {
		return advisoryNumber;
	}

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.ui.pgen.tca.ITca#getStormName()
	 */
	@Override
	public String getStormName() {
		return stormName;
	}

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.ui.pgen.tca.ITca#getStormType()
	 */
	@Override
	public String getStormType() {
		return stormType;
	}

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.ui.pgen.tca.ITca#getTextLocation()
	 */
	@Override
	public Coordinate getTextLocation() {
		return textLocation;
	}

	@Override
	public void setPointsOnly(ArrayList<Coordinate> pts) {
		// TODO Auto-generated method stub
	}

	@Override
	public void update(IAttribute attr) {

		if ( attr instanceof ITca ) {
			ITca tca = (ITca)attr;
			this.setIssueStatus(tca.getIssuingStatus());
			this.setStormType(tca.getStormType());
			this.setBasin(tca.getBasin());
			this.setStormName(tca.getStormName());
			this.setStormNumber(tca.getStormNumber());
			this.setAdvisoryTime(tca.getAdvisoryTime());
			this.setAdvisoryNumber(tca.getAdvisoryNumber());
			this.setTimeZone(tca.getTimeZone());
			
			this.setAdvisories( tca.getAdvisories() );
			this.setTextLocation(tca.getTextLocation());
		}
	}

	/**
	 * Creates a copy of this object.  This is a deep copy and new objects are
	 * created so that we are not just copying references of objects
	 */
	@Override
	public AbstractDrawableComponent copy() {

		/*
		 * create a new Text object and initially set its attributes to this one's
		 */
		TCAElement newTca = new TCAElement();

		/*
		 * Set new Color, Strings and Coordinate so that we don't just set references to this
		 * object's attributes.
		 */
		newTca.setIssueStatus( new String(this.getIssuingStatus()) );
		newTca.setStormType( new String(this.getStormType()) );
		newTca.setBasin(new String(this.getBasin()) );
		newTca.setStormName( new String(this.getStormName()) );
		newTca.setStormNumber( this.getStormNumber() );
		newTca.setAdvisoryNumber( new String(this.getAdvisoryNumber()) );
		newTca.setTimeZone( new String (this.getTimeZone()) );
		newTca.setTextLocation( new Coordinate(this.getTextLocation()) );

		/*
		 * new text Strings are created and set, so we don't just set 
		 * references
		 */
		Calendar newTime = Calendar.getInstance();
		newTime.setTimeInMillis( this.getAdvisoryTime().getTimeInMillis() );
		newTca.setAdvisoryTime( newTime );
		
		for ( TropicalCycloneAdvisory adv : this.getAdvisories() ) {
			newTca.addAdvisory(adv.copy());
		}

		newTca.setPgenCategory(new String(this.getPgenCategory()));
		newTca.setPgenType(new String(this.getPgenType()));

		newTca.setParent(this.getParent());

		return newTca;

	}

	/**
	 * Returns the location of each breakpoint in each advisory
	 */
	@Override
	public List<Coordinate> getPoints() {

		List<Coordinate> points = new ArrayList<Coordinate>();
		
		if ( advisories.isEmpty() ) {
			points.add( textLocation );
		}
		else {
			for ( TropicalCycloneAdvisory tca : advisories ) {
				BPGeography segment = tca.getSegment();
				for ( Breakpoint bkpt : segment.getBreakpoints() ) {
					points.add( bkpt.getLocation() );
				}
			}
		}

		return points;
	}

	@Override
	public void setColors(Color[] colors) {
		// TODO Auto-generated method stub
	}

	@Override
	public String getBasin() {
		return basin;
	}

	@Override
	public String getIssuingStatus() {
		return issueStatus;
	}

	@Override
	public int getStormNumber() {
		return stormNumber;
	}

	@Override
	public String getTimeZone() {
		return timeZone;
	}

	/**
	 * @return the issueStatus
	 */
	public String getIssueStatus() {
		return issueStatus;
	}

	/**
	 * @param issueStatus the issueStatus to set
	 */
	public void setIssueStatus(String issueStatus) {
		this.issueStatus = issueStatus;
	}

	/**
	 * @return the advisoryTime
	 */
	public Calendar getAdvisoryTime() {
		return advisoryTime;
	}

	/**
	 * @param advisoryTime the advisoryTime to set
	 */
	public void setAdvisoryTime(Calendar advisoryTime) {
		this.advisoryTime = advisoryTime;
	}

	/**
	 * @param stormNumber the stormNumber to set
	 */
	public void setStormNumber(int stormNumber) {
		this.stormNumber = stormNumber;
	}

	/**
	 * @param basin the basin to set
	 */
	public void setBasin(String basin) {
		this.basin = basin;
	}

	/**
	 * @param advisoryNumber the advisoryNumber to set
	 */
	public void setAdvisoryNumber(String advisoryNumber) {
		this.advisoryNumber = advisoryNumber;
	}

	/**
	 * @param stormName the stormName to set
	 */
	public void setStormName(String stormName) {
		this.stormName = stormName;
	}

	/**
	 * @param stormType the stormType to set
	 */
	public void setStormType(String stormType) {
		this.stormType = stormType;
	}

	/**
	 * @param timeZone the timeZone to set
	 */
	public void setTimeZone(String timeZone) {
		this.timeZone = timeZone;
	}

	/**
	 * @param textLocation the textLocation to set
	 */
	public void setTextLocation(Coordinate textLocation) {
		this.textLocation = textLocation;
	}

	/**
	 * @param advisories the advisories to set
	 */
	public void setAdvisories(ArrayList<TropicalCycloneAdvisory> advisories) {
		//this.advisories = advisories;
		this.advisories.clear();
		for ( TropicalCycloneAdvisory adv : advisories ) {
			this.addAdvisory(adv.copy());
		}
	}

	public void addAdvisory(TropicalCycloneAdvisory advisory) {
		this.advisories.add(advisory);
	}
	
	public void replaceAdvisory(int index, TropicalCycloneAdvisory advisory) {
		this.advisories.set(index, advisory);
	}

	/**
	 * checks each advisory to see if it contains a breakpoint with the given name
	 * @param name
	 * @return true, if breakpoint name found
	 */
	public boolean containsBreakpoint(String name) {

		for ( TropicalCycloneAdvisory adv : advisories ) {
			for ( Breakpoint bkpt : adv.getSegment().getBreakpoints() ) {
				if ( bkpt.getName().equals(name) ) return true;
			}
		}
		return false;
	}
	
    /*
     * Returns the watch/warning advisory closest to the given lat/lon location
     */
	public TropicalCycloneAdvisory findClosestAdvisory(Coordinate loc) {

		TropicalCycloneAdvisory selectedAdv = null;
		GeometryFactory gf = new GeometryFactory();
		Point pt = gf.createPoint(loc);
		double min = Double.MAX_VALUE;
		Geometry geom = null;
		
		/*
		 * Compare distance of current location (loc) with each watch/warning advisory
		 * and save advisory with min distance
		 */
		for ( TropicalCycloneAdvisory adv : getAdvisories() ) {
			if ( adv.getSegment() instanceof BreakpointPair ) {
				/*
				 * create a Geometry for a line segment connecting the two breakpoints
				 */
				Coordinate[] coords = new Coordinate[] { adv.getSegment().getBreakpoints().get(0).getLocation(),
						adv.getSegment().getBreakpoints().get(1).getLocation() };
				
				geom = gf.createLineString(coords);
			}
			else {
				/*
				 * create a Geometry for the breakpoint location
				 */
				geom = gf.createPoint(adv.getSegment().getBreakpoints().get(0).getLocation());
			}
			// Calculate distance to advisory/breakpoint 
			double dist = pt.distance(geom);
			if ( dist < min ) {
				min = dist;
				selectedAdv = adv;
			}
		}
		
		return selectedAdv;
	}

}
