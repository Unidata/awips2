package gov.damcat.data;

/**
 * This class holds information from the damcat_pair table.
 */
class CrossSectionEntryInfo {
	
	public String down_name,xsec_type,updated;
	public Float elev,tw,mann_n,inactive_width;
	public Integer pair_num;
	//public java.sql.Date updated;
	
/**
 * DAMCAT_CrossSectionEntryInfo constructor comment.
 */
public CrossSectionEntryInfo() {
	super();
	down_name = "";
	xsec_type = "";
	updated = "";
	elev = new Float(0.0f);
	inactive_width = new Float(0.0f);
	mann_n = new Float(0.0f);
	tw = new Float(0.0f);
	pair_num = new Integer(0);
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @return java.lang.String
 */
public java.lang.String getDown_name() {
	return down_name;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @return java.lang.Float
 */
public java.lang.Float getElev() {
	return elev;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @return java.lang.Float
 */
public java.lang.Float getInactive_width() {
	return inactive_width;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @return java.lang.Float
 */
public java.lang.Float getMann_n() {
	return mann_n;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @return java.lang.Integer
 */
public java.lang.Integer getPair_num() {
	return pair_num;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @return java.lang.Float
 */
public java.lang.Float getTw() {
	return tw;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @return java.lang.String
 */
public java.lang.String getUpdated() {
	return updated;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @return java.lang.String
 */
public java.lang.String getXsec_type() {
	return xsec_type;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @param newDown_name java.lang.String
 */
public void setDown_name(java.lang.String newDown_name) {
	down_name = newDown_name;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @param newElev java.lang.Float
 */
public void setElev(java.lang.Float newElev) {
	elev = newElev;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @param newInactive_width java.lang.Float
 */
public void setInactive_width(java.lang.Float newInactive_width) {
	inactive_width = newInactive_width;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @param newMann_n java.lang.Float
 */
public void setMann_n(java.lang.Float newMann_n) {
	mann_n = newMann_n;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @param newPair_num java.lang.Integer
 */
public void setPair_num(java.lang.Integer newPair_num) {
	pair_num = newPair_num;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @param newTw java.lang.Float
 */
public void setTw(java.lang.Float newTw) {
	tw = newTw;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @param newUpdated java.lang.String
 */
public void setUpdated(java.lang.String newUpdated) {
	updated = newUpdated;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 3:00:37 PM)
 * @param newXsec_type java.lang.String
 */
public void setXsec_type(java.lang.String newXsec_type) {
	xsec_type = newXsec_type;
}
}
