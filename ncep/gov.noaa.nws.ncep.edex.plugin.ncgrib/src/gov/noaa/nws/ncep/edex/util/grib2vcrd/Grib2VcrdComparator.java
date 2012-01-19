package gov.noaa.nws.ncep.edex.util.grib2vcrd;

import java.util.Comparator;

/**
 * Comparator for Grib2Vcrd fields.
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

public class Grib2VcrdComparator implements Comparator<Grib2Vcrd>, IGrib2VcrdField {
	
	private Grib2VcrdField field;
	
	public Grib2VcrdComparator(Grib2VcrdField f) {
		this.field = f;
	}
	
	public int compare(Grib2Vcrd o1, Grib2Vcrd o2) {
		switch (field) {
		case G2VCRDID:
			return o1.getG2Vcrdid() - o2.getG2Vcrdid();
		case VCRDID1:
			return o1.getVcrdid1() - o2.getVcrdid1();
		case VCRDID2:
			return o1.getVcrdid2() - o2.getVcrdid2();
		case NAME:
			return o1.getName().compareToIgnoreCase(o2.getName());
		case UNITS:
			return o1.getUnits().compareToIgnoreCase(o2.getUnits());
		case GNAM:
			return o1.getGnam().compareToIgnoreCase(o2.getGnam());
		case SCALE:
			return o1.getScale().compareToIgnoreCase(o2.getScale());
		default:
			return 0;
		}
		
	}
}