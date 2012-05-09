package gov.noaa.nws.ncep.edex.util.grib2vars;

import java.util.Comparator;

/**
 * Comparator for Grib2Vars fields.
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/10		276  		L. Lin	   Initial Creation
 *                       
 * </pre>
 * 
 * @author llin
 * @version 1
 */

public class Grib2VarsComparator implements Comparator<Grib2Vars>, IGrib2VarsField {
	
	private Grib2VarsField field;
	
	public Grib2VarsComparator(Grib2VarsField f) {
		this.field = f;
	}
	
	public int compare(Grib2Vars o1, Grib2Vars o2) {
		switch (field) {
		case G2VARSID:
			return o1.getG2Varsid() - o2.getG2Varsid();
		case DISCIPLINE:
			return o1.getDiscipline() - o2.getDiscipline();
		case CATEGORY:
			return o1.getCategory() - o2.getCategory();
		case PID:
			return o1.getPid() - o2.getPid();
		case PDT:
			return o1.getPdt() - o2.getPdt();
		case NAME:
			return o1.getName().compareToIgnoreCase(o2.getName());
		case UNITS:
			return o1.getUnits().compareToIgnoreCase(o2.getUnits());
		case GNAM:
			return o1.getGnam().compareToIgnoreCase(o2.getGnam());
		case SCALE:
			return o1.getScale() - o2.getScale();
		case MISSING:
			return o1.getMissing().compareTo(o2.getMissing());
		default:
			return 0;
		}
		
	}
}