package gov.noaa.nws.ncep.viz.common.staticPointDataSource;


/**
 * 
 * Class defines locator point data
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1/14/2009    #48        M. Li       Initial version
 * 07/04/2013   #1010      G. Hull     Use LabeldPoint to store name,lat/lon
 * 
 * 
 * * @author mli
 * * @version 1.0
 * 
 */
public class PointDirDist {

	private LabeledPoint namedPoint = null; // from STRTree

	private double direction  = Double.NaN;
	
	private int distanceInMeter = Integer.MAX_VALUE;

	public LabeledPoint getNamedPoint() {
		return namedPoint;
	}

//	public void setNamedPoint(NamedPoint np) {
//		this.namedPoint = np;
//	}

//	public String getStateID() {
//		return StateID;
//	}
//
//	public void setStateID(String stateID) {
//		StateID = stateID;
//	}

	public PointDirDist() {		
	}
	
	public PointDirDist( LabeledPoint np, int dist, double dir ) {
		namedPoint = new LabeledPoint( np.getName(), 
									   np.getLatitude(), np.getLongitude() );
		distanceInMeter = dist;
		direction = dir;
	}
	
	public String getName() {
		return namedPoint.getName();
	}

	public double getLat() {
		return namedPoint.getLatitude();
	}

	public double getLon() {
		return namedPoint.getLongitude();
	}

	public int getDistanceInMeter() {
		return distanceInMeter;
	}

	public void setDistanceInMeter(int distanceInMeter) {
		this.distanceInMeter = distanceInMeter;
	}
	
	public double getDirection() {
		return direction;
	}

	public void setDirection(double dir) {
		this.direction = dir;
	}
}
