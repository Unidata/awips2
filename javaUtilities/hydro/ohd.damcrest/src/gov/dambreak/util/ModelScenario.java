package gov.dambreak.util;

/**
 * Insert the type's description here.
 * Creation date: (7/28/2003 10:39:30 AM)
 * @author: 
 */
public class ModelScenario {
	// public fields
	public String	source;
	public String	name;
	public int		damType;
	public float	HDE;
	public float	BME;
	public float	VOL;
	public float	SA;
	public float	BW;
	public float	TFM;
	public float	QO;
	public float	DISTTN;
	public float	CMS;

	public int		dbugType;
	public int 		changeFlag = 0;	// 0 = no changes; 1 = updated; 2 = inserted; 3 = deleted

	public ModelOutput	output;
	public boolean		bOutputAvailable;
/**
 * ModelScenario constructor comment.
 */
public ModelScenario() {
//	source = "DEF";
	source = "Not Specified";
	name = "NS";   //type of scenario -- set to not specified
	bOutputAvailable = false;
}
}
