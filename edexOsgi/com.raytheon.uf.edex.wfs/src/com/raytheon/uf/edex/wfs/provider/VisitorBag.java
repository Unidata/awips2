/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.wfs.provider;

import java.util.Map;


/**
 * Visitor pattern carry-on that allows for plugin specific configuration to be
 * used by filter parser
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class VisitorBag {

	protected Class<?> rootEntity;

	protected String spatialField;

    protected String verticalField;

	protected Map<String, String> fieldMap;

    protected String idField;

	/**
	 * @param converter
	 * @param rootEntity
	 * @param spatialField
	 */
    public VisitorBag(Class<?> rootEntity, String spatialField,
            String verticalField, String idField) {
		super();
		this.rootEntity = rootEntity;
		this.spatialField = spatialField;
        this.verticalField = verticalField;
        this.idField = idField;
	}

	public String filterField(String field) {
		if (fieldMap == null || field == null ||
		        !fieldMap.containsKey(field)) {
			return field;
		}
		String rval = fieldMap.get(field);
		//return "" as null for consistency
		return (rval == null || "".equals(rval)) ? null : rval;
	}

	public Class<?> getRootEntity() {
		return rootEntity;
	}

	public void setRootEntity(Class<?> rootEntity) {
		this.rootEntity = rootEntity;
	}

	public String getSpatialField() {
		return spatialField;
	}

	public void setSpatialField(String spatialField) {
		this.spatialField = spatialField;
	}

	public Map<String, String> getFieldMap() {
		return fieldMap;
	}

	public void setFieldMap(Map<String, String> fieldMap) {
		this.fieldMap = fieldMap;
	}

    /**
     * @return the idField
     */
    public String getIdField() {
        return idField;
    }

    /**
     * @param idField
     *            the idField to set
     */
    public void setIdField(String idField) {
        this.idField = idField;
    }

    /**
     * @return the verticalField
     */
    public String getVerticalField() {
        return verticalField;
    }

    /**
     * @param verticalField
     *            the verticalField to set
     */
    public void setVerticalField(String verticalField) {
        this.verticalField = verticalField;
    }

}
