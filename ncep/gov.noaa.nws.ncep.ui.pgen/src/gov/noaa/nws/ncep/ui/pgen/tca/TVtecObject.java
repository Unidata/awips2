/*
 * TVtecObject
 * 
 * Date created 05 NOVEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tca;

import java.util.Calendar;
import java.util.HashMap;

//import com.raytheon.edex.vtec.api.PVtecObject;
//import com.raytheon.edex.vtec.util.VtecUtils;
import com.raytheon.viz.texteditor.util.VtecObject;
import com.raytheon.viz.texteditor.util.VtecUtil;

/**
 * Object representing the Primary Valid Time Event Code (VTEC).  Extends RTS's
 * PVtecObject to add functionality specific to the VTEC lines used in the TCV
 * message, such as allowing null ending event date/time.
 * Also, added comparator to determine priority between VTECs.
 * TCV message.
 * @author sgilbert
 *
 */
public class TVtecObject extends VtecObject implements Comparable<TVtecObject> {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final String vtecCreateFormatTCV = "/%s.%s.%s.%s.%s.%04d.%s-000000T0000Z/";
	private static final String COMPARE_FORMAT = "%s.%s.%s";
	
	private static final HashMap<String,Integer> priority = new HashMap<String,Integer>();
	
	/*
	 * Priority of possible VTEC codes to appear in TCV
	 */
	static {
		priority.put( "NEW.HU.W", new Integer(0));
		priority.put( "NEW.HU.A", new Integer(1));
		priority.put( "NEW.TR.W", new Integer(2));
		priority.put( "NEW.TR.A", new Integer(3));
		priority.put( "CON.HU.W", new Integer(4));
		priority.put( "CON.HU.A", new Integer(5));
		priority.put( "CON.TR.W", new Integer(6));
		priority.put( "CON.TR.A", new Integer(7));
		priority.put( "CAN.HU.W", new Integer(8));
		priority.put( "CAN.HU.A", new Integer(9));
		priority.put( "CAN.TR.W", new Integer(10));
		priority.put( "CAN.TR.A", new Integer(11));
	}
	
	/**
	 * 
	 */
	public TVtecObject() {
		// 
	}

	/**
	 * @param vtec
	 */
	public TVtecObject(String vtec) {
		super(vtec);
	}

	/**
	 * @param product
	 * @param action
	 * @param office
	 * @param phenomenon
	 * @param significance
	 * @param sequence
	 * @param startTime
	 * @param endTime
	 */
	public TVtecObject(String product, String action, String office,
			String phenomenon, String significance, int sequence,
			Calendar startTime, Calendar endTime) {
		super( action, office, phenomenon, significance, sequence );
		setProduct(product);
		setStartTime(startTime);
		setEndTime(endTime);
	}

	/**
	 * @param office
	 * @param phenomenon
	 * @param significance
	 * @param sequence
	 */
	public TVtecObject(String office, String phenomenon, String significance,
			int sequence) {
		super(office, phenomenon, significance, sequence);
	}

	/*
	 * need to override for 000000T0000Z
	 * @see com.raytheon.edex.vtec.api.PVtecObject#getVtecString()
	 */
    @Override
    public String getVtecString() {
    	String vtecStr;
    	
    	if ( getEndTime() == null )
    		vtecStr = String.format(vtecCreateFormatTCV, getProduct(), getAction(),
                getOffice(), getPhenomena(), getSignificance(), getSequence(),
                VtecUtil.formatVtecTime(getStartTime()) );
    	else
    		vtecStr = super.getVtecString();
    	
        return vtecStr;
    }

    /*
     * Compare to another PVTEC object based on a predefined priority
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
	@Override
	public int compareTo(TVtecObject o) {

		if ( o == null ) throw new NullPointerException("TVtecObject: Can't compare null");
		
		int thisPriority = getPriority(this);
		int thatPriority = getPriority(o);
		
		//System.out.println("thispri="+thisPriority+"            thatpri="+thatPriority);
		if ( thisPriority < thatPriority )   // this object has greater priority
			return -1;
		
		else if ( thisPriority > thatPriority )   // object o has greater priority
			return 1;
	
		else
			return 0;
			
	}

	/*
	 * returns an integer value representing this VTECs priority relative to others.
	 * Lower value indicates higher priority.
	 */
	private int getPriority(TVtecObject o) {
		
		String str = String.format(COMPARE_FORMAT, o.getAction(), o.getPhenomena(), o.getSignificance());

		if ( priority.containsKey(str) ) 
			return priority.get(str).intValue();
		else
			return 100;
	}

	/*
	 * two TVtecObjects are considered equal if they have the same priority.
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {

		if ( obj == null ) throw new NullPointerException("TVtecObject: Can't compare null");
		
		if ( obj instanceof TVtecObject ) {
			TVtecObject vtec = (TVtecObject)obj;
			if ( getPriority(this) == getPriority(vtec) ) return true;
		}
		
		return false;
	}

	

}
