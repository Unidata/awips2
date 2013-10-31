package gov.dambreak.util;

/**
 * Logical Model Representation Of Downstream Point
 * Creation date: (7/29/2003 8:01:02 AM)
 * @author: 
 */
import java.util.ArrayList;

public class DownstreamPoint {
	public String name;
	public String xsecBestType;
	public String comments;
	public float distanceToSection;
	public float floodDepth;
	public float floodWidth;
	public float floodFlow;
	public float latitude;
	public float longitude;
	public float elevation;
	public float mann_oc;
	public int bestXS;								// index pointing to best XS
	public int changeFlag = 0;						// 0 = no changes; 1 = updated; 2 = inserted; 3 = deleted
	public ArrayList xsections = new ArrayList(); 	// SectionGeometry
	
	public ArrayList deletedXsections = new ArrayList();	// SectionGeometry
/**
 * DownstreamPoint constructor comment.
 */
public DownstreamPoint() {
	name = "NotSpecified";
	distanceToSection = 100.0f;
	bestXS = -1;
}
/**
 * Method assumes that bestXS index points to proper instance in ArrayList 
 * Creation date: (7/31/2003 1:24:11 PM)
 * @return util.SectionGeometry
 */
public SectionGeometry getBestSection() {
	
	if (xsections.size() == 0)
		return null;
		
	if (bestXS == -1)
		return (SectionGeometry)xsections.get(0);

	if (bestXS >= xsections.size())
		return (SectionGeometry)xsections.get(0);
		
	return (SectionGeometry)xsections.get(bestXS);
	
}
}
