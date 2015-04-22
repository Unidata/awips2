package gov.noaa.nws.ncep.edex.common.stationTables;

import java.util.Comparator;

/**
 * Comparator for Station fields.
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/09		134  		M. Li	   Initial Creation
 *                       
 * </pre>
 * 
 * @author mli
 * @version 1
 */

public class StationComparator implements Comparator<Station>, IStationField {
	
	private StationField field;
	
	public StationComparator(StationField f) {
		this.field = f;
	}
	
	public int compare(Station o1, Station o2) {
		switch (field) {
		case STID:
			return o1.getStid().compareToIgnoreCase(o2.getStid());
		case STNM:
			return o1.getStnnum().compareToIgnoreCase(o2.getStnnum());
		case NAME:
			return o1.getStnnum().compareToIgnoreCase(o2.getStnnum());
		case ST:
			return o1.getState().compareToIgnoreCase(o2.getState());
		case CO:
			return o1.getCountry().compareToIgnoreCase(o2.getCountry());
		/*
		case LAT:
			return o1.getLatitude().compareTo(o2.getLatitude());
		case LON:
			return o1.getLongitude().compareTo(o2.getLongitude());
		case ELV:
			return o1.getElevation() - o2.getElevation();
		case PRI:
			return o1.getPriority() - o2.getPriority();
		*/	
		case WFO:
			return o1.getWfo().compareToIgnoreCase(o2.getWfo());
		case LOC:
			return o1.getLocation().compareToIgnoreCase(o2.getLocation());
			
		default:
			return 0;
		}
		
	}
}