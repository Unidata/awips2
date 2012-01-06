package gov.damcat.data;

/**
 * This class holds information from the damcat_out table.
 */
class OutputEntryInfo {
	
	public String src,scenario,down_name,comments,updated;
	public Float slope,max_flow,max_depth,time_max_depth,time_flood,time_deflood;
//	public java.sql.Date updated;
	
/**
 * DAMCAT_OutputEntryInfo constructor comment.
 */
public OutputEntryInfo() {
	super();
	src = "";
	scenario = "";
	down_name = "";
	comments = "";
	updated = "";
	slope = new Float(0.0f);
	max_flow = new Float(0.0f);
	max_depth = new Float(0.0f);
	time_max_depth = new Float(0.0f);
	time_flood = new Float(0.0f);
	time_deflood = new Float(0.0f);
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @return java.lang.String
 */
public java.lang.String getComments() {
	return comments;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @return java.lang.String
 */
public java.lang.String getDown_name() {
	return down_name;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @return java.lang.Float
 */
public java.lang.Float getMax_depth() {
	return max_depth;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @return java.lang.Float
 */
public java.lang.Float getMax_flow() {
	return max_flow;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @return java.lang.String
 */
public java.lang.String getScenario() {
	return scenario;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @return java.lang.Float
 */
public java.lang.Float getSlope() {
	return slope;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @return java.lang.String
 */
public java.lang.String getSrc() {
	return src;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @return java.lang.Float
 */
public java.lang.Float getTime_deflood() {
	return time_deflood;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @return java.lang.Float
 */
public java.lang.Float getTime_flood() {
	return time_flood;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @return java.lang.Float
 */
public java.lang.Float getTime_max_depth() {
	return time_max_depth;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @return java.lang.String
 */
public java.lang.String getUpdated() {
	return updated;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @param newComments java.lang.String
 */
public void setComments(java.lang.String newComments) {
	comments = newComments;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @param newDown_name java.lang.String
 */
public void setDown_name(java.lang.String newDown_name) {
	down_name = newDown_name;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @param newMax_depth java.lang.Float
 */
public void setMax_depth(java.lang.Float newMax_depth) {
	max_depth = newMax_depth;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @param newMax_flow java.lang.Float
 */
public void setMax_flow(java.lang.Float newMax_flow) {
	max_flow = newMax_flow;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @param newScenario java.lang.String
 */
public void setScenario(java.lang.String newScenario) {
	scenario = newScenario;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @param newSlope java.lang.Float
 */
public void setSlope(java.lang.Float newSlope) {
	slope = newSlope;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @param newSrc java.lang.String
 */
public void setSrc(java.lang.String newSrc) {
	src = newSrc;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @param newTime_deflood java.lang.Float
 */
public void setTime_deflood(java.lang.Float newTime_deflood) {
	time_deflood = newTime_deflood;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @param newTime_flood java.lang.Float
 */
public void setTime_flood(java.lang.Float newTime_flood) {
	time_flood = newTime_flood;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @param newTime_max_depth java.lang.Float
 */
public void setTime_max_depth(java.lang.Float newTime_max_depth) {
	time_max_depth = newTime_max_depth;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:02 PM)
 * @param newUpdated java.lang.String
 */
public void setUpdated(java.lang.String newUpdated) {
	updated = newUpdated;
}
}
