package gov.dambreak.util;

/**
 * Logically encapsulates both a Cross Section Type and it's set of Topwidth-Elevation "pairs"  
 * Creation date: (7/28/2003 3:54:09 PM)
 * @author: 
 */
public class SectionGeometry {
	private String xsType = null;
	private float[][] elevations = new float[8][4];

	
	private int changeFlag = 0;		// 0 = no changes; 1 = updated;; 2 = inserted; 3 = deleted
	private int lastRowUsed = -1;	// used to indicate last Row in array with valid data
/**
 * Constructor
 */
public SectionGeometry() {
	
	xsType = "M1";
	
	for (int i=0; i<8; i++)
	{
		setElevationData(i,0,-999.0f);
		setElevationData(i,1,-999.0f);
		setElevationData(i,2,-999.0f);
		setElevationData(i,3,-999.0f);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (1/7/2004 2:08:15 PM)
 * @return int
 */
public int getChangeFlag() {
	return changeFlag;
}
/**
 * Insert the method's description here.
 * Creation date: (7/28/2003 4:02:59 PM)
 * @return float
 * @param elevation int
 * @param type int
 */
public float getElevationData(int elevation, int type) {
	return elevations[elevation][type];
}
/**
 * Insert the method's description here.
 * Creation date: (1/7/2004 3:35:26 PM)
 * @return int
 */
public int getLastRowUsed() {
	return lastRowUsed;
}
/**
 * Insert the method's description here.
 * Creation date: (7/28/2003 4:04:44 PM)
 */
public String getXSType() {
	
	return xsType;
	
}
/**
 * Insert the method's description here.
 * Creation date: (1/7/2004 2:08:15 PM)
 * @param newChangeFlag int
 */
public void setChangeFlag(int newChangeFlag) {
	changeFlag = newChangeFlag;
}
/**
 * Insert the method's description here.
 * Creation date: (7/28/2003 4:03:31 PM)
 * @param elevation int
 * @param type int
 * @param value float
 */
public void setElevationData(int elevation, int type, float value) {
	elevations[elevation][type] = value;
}
/**
 * Insert the method's description here.
 * Creation date: (1/7/2004 3:35:26 PM)
 * @param newLastRowUsed int
 */
public void setLastRowUsed(int newLastRowUsed) {
	lastRowUsed = newLastRowUsed;
}
/**
 * Insert the method's description here.
 * Creation date: (7/28/2003 4:05:11 PM)
 * @param value java.lang.String
 */
public void setXSType(String value) {
	xsType = value;
}
}
