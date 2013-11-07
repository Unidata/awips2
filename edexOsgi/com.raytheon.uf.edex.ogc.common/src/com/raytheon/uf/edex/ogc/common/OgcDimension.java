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
/**
 * 
 */
package com.raytheon.uf.edex.ogc.common;

import java.util.List;


/**
 * Contains dimension metadata used to populate capability and description OGC
 * documents. Separate from OGC JAXB objects to support different versions of
 * OGC services.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class OgcDimension {

	protected String name;

	protected String units;

	protected String unitSymbol;

	protected List<String> values;

	protected String defaultVal;

	/**
	 * 
	 */
	public OgcDimension() {
	}

	public OgcDimension(String name, List<String> values) {
		this(name, "", values);
	}

	public OgcDimension(String name, String units, List<String> values) {
		this(name, units, null, values);
	}

	public OgcDimension(String name, String units, String unitSymbol,
			List<String> values) {
		this.name = name;
		this.units = units;
		this.unitSymbol = unitSymbol;
		this.values = values;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the units
	 */
	public String getUnits() {
		return units;
	}

	/**
	 * @param units
	 *            the units to set
	 */
	public void setUnits(String units) {
		this.units = units;
	}

	/**
	 * @return the unitSymbol
	 */
	public String getUnitSymbol() {
		return unitSymbol;
	}

	/**
	 * @param unitSymbol
	 *            the unitSymbol to set
	 */
	public void setUnitSymbol(String unitSymbol) {
		this.unitSymbol = unitSymbol;
	}

	/**
	 * @return the values
	 */
	public List<String> getValues() {
		return values;
	}

	/**
	 * @param values
	 *            the values to set
	 */
	public void setValues(List<String> values) {
		this.values = values;
	}

	/**
	 * @return the defaultVal
	 */
	public String getDefaultVal() {
		return defaultVal;
	}

	/**
	 * @param defaultVal
	 *            the defaultVal to set
	 */
	public void setDefaultVal(String defaultVal) {
		this.defaultVal = defaultVal;
	}

}
