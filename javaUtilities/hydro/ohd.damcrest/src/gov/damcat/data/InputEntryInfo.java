package gov.damcat.data;

/**
 * This class holds information from the damcat_in table.
 */
class InputEntryInfo {
	
	public String src,scenario,comments, updated;
	public Float hde,bme,vol,sa,tfm,qo,bw;
	public Integer idam;
	//public java.sql.Date updated;
/**
 * DAMCAT_InputEntryInfo constructor comment.
 */
public InputEntryInfo() {
	super();
	src = "";
	scenario = "";
	comments = ""; 
	updated = "";
	hde = new Float(0.0f);
	bme = new Float(0.0f);
	vol = new Float(0.0f);
	sa = new Float(0.0f);
	tfm = new Float(0.0f);
	qo = new Float(0.0f);
	bw = new Float(0.0f);
	idam = new Integer(0);
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @return java.lang.Float
 */
public java.lang.Float getBme() {
	return bme;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @return java.lang.Float
 */
public java.lang.Float getBw() {
	return bw;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @return java.lang.String
 */
public java.lang.String getComments() {
	return comments;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @return java.lang.Float
 */
public java.lang.Float getHde() {
	return hde;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @return java.lang.Integer
 */
public java.lang.Integer getIdam() {
	return idam;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @return java.lang.Float
 */
public java.lang.Float getQo() {
	return qo;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @return java.lang.Float
 */
public java.lang.Float getSa() {
	return sa;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @return java.lang.String
 */
public java.lang.String getScenario() {
	return scenario;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @return java.lang.String
 */
public java.lang.String getSrc() {
	return src;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @return java.lang.Float
 */
public java.lang.Float getTfm() {
	return tfm;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @return java.lang.String
 */
public java.lang.String getUpdated() {
	return updated;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @return java.lang.Float
 */
public java.lang.Float getVol() {
	return vol;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @param newBme java.lang.Float
 */
public void setBme(java.lang.Float newBme) {
	bme = newBme;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @param newBw java.lang.Float
 */
public void setBw(java.lang.Float newBw) {
	bw = newBw;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @param newComments java.lang.String
 */
public void setComments(java.lang.String newComments) {
	comments = newComments;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @param newHde java.lang.Float
 */
public void setHde(java.lang.Float newHde) {
	hde = newHde;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @param newIdam java.lang.Integer
 */
public void setIdam(java.lang.Integer newIdam) {
	idam = newIdam;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @param newQo java.lang.Float
 */
public void setQo(java.lang.Float newQo) {
	qo = newQo;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @param newSa java.lang.Float
 */
public void setSa(java.lang.Float newSa) {
	sa = newSa;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @param newScenario java.lang.String
 */
public void setScenario(java.lang.String newScenario) {
	scenario = newScenario;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @param newSrc java.lang.String
 */
public void setSrc(java.lang.String newSrc) {
	src = newSrc;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @param newTfm java.lang.Float
 */
public void setTfm(java.lang.Float newTfm) {
	tfm = newTfm;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @param newUpdated java.lang.String
 */
public void setUpdated(java.lang.String newUpdated) {
	updated = newUpdated;
}

/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 2:59:02 PM)
 * @param newVol java.lang.Float
 */
public void setVol(java.lang.Float newVol) {
	vol = newVol;
}
}
