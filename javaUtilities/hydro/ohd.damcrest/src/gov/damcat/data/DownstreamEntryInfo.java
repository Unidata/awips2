package gov.damcat.data;

/**
 * This class holds information from the damcat_down table.
 */
class DownstreamEntryInfo {
	
	public String down_name,comments,xsec_best_type,updated;
	public Float longitude,latitude,elevation,distance_from_dam,flood_flow,flood_depth,flood_width,mann_oc;
	//public java.sql.Date updated;
	
/**
 * DAMCAT_DownstreamEntryInfo constructor comment.
 */
public DownstreamEntryInfo() {
	super();
	down_name = "";
	comments = "";
	xsec_best_type = "";
	updated = "";
	longitude = new Float(0.0f);
	latitude = new Float(0.0f);
	elevation = new Float(0.0f);
	distance_from_dam = new Float(0.0f);
	flood_flow = new Float(0.0f);
	flood_depth = new Float(0.0f);
	flood_width = new Float(0.0f);
	mann_oc = new Float(0.0f);
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @return java.lang.String
 */
public java.lang.String getComments() {
	return comments;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @return java.lang.Float
 */
public java.lang.Float getDistance_from_dam() {
	return distance_from_dam;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @return java.lang.String
 */
public java.lang.String getDown_name() {
	return down_name;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @return java.lang.Float
 */
public java.lang.Float getElevation() {
	return elevation;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @return java.lang.Float
 */
public java.lang.Float getFlood_depth() {
	return flood_depth;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @return java.lang.Float
 */
public java.lang.Float getFlood_flow() {
	return flood_flow;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @return java.lang.Float
 */
public java.lang.Float getFlood_width() {
	return flood_width;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @return java.lang.Float
 */
public java.lang.Float getLatitude() {
	return latitude;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @return java.lang.Float
 */
public java.lang.Float getLongitude() {
	return longitude;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @return java.lang.Float
 */
public java.lang.Float getMann_oc() {
	return mann_oc;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @return java.lang.String
 */
public java.lang.String getUpdated() {
	return updated;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @return java.lang.String
 */
public java.lang.String getXsec_best_type() {
	return xsec_best_type;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @param newComments java.lang.String
 */
public void setComments(java.lang.String newComments) {
	comments = newComments;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @param newDistance_from_dam java.lang.Float
 */
public void setDistance_from_dam(java.lang.Float newDistance_from_dam) {
	distance_from_dam = newDistance_from_dam;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @param newDown_name java.lang.String
 */
public void setDown_name(java.lang.String newDown_name) {
	down_name = newDown_name;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @param newElevation java.lang.Float
 */
public void setElevation(java.lang.Float newElevation) {
	elevation = newElevation;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @param newFlood_depth java.lang.Float
 */
public void setFlood_depth(java.lang.Float newFlood_depth) {
	flood_depth = newFlood_depth;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @param newFlood_flow java.lang.Float
 */
public void setFlood_flow(java.lang.Float newFlood_flow) {
	flood_flow = newFlood_flow;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @param newFlood_width java.lang.Float
 */
public void setFlood_width(java.lang.Float newFlood_width) {
	flood_width = newFlood_width;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @param newLatitude java.lang.Float
 */
public void setLatitude(java.lang.Float newLatitude) {
	latitude = newLatitude;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @param newLongitude java.lang.Float
 */
public void setLongitude(java.lang.Float newLongitude) {
	longitude = newLongitude;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @param newMann_oc java.lang.Float
 */
public void setMann_oc(java.lang.Float newMann_oc) {
	mann_oc = newMann_oc;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @param newUpdated java.lang.String
 */
public void setUpdated(java.lang.String newUpdated) {
	updated = newUpdated;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:48:49 PM)
 * @param newXsec_best_type java.lang.String
 */
public void setXsec_best_type(java.lang.String newXsec_best_type) {
	xsec_best_type = newXsec_best_type;
}
}
