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
package com.raytheon.uf.viz.derivparam.library;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.time.DataTime;

/**
 * The DerivedParameteRequest is the way for a class to communicate a request
 * for a derived parameter to the DerivedParameterGenerator thread. This class
 * works closely with <code>DerivedParameterGenerator</code> to get the
 * requested parameter back.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------ ----------  ----------- --------------------------
 * Jul 09, 2008             brockwoo    Initial creation
 * Jun 04, 2013 2041        bsteffen    Switch derived parameters to use
 *                                      concurrent python for threading.
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class DerivedParameterRequest {

    private String parameterAbbreviation;

    private String method;

    private Object[] argumentRecords;

    private List<Object> baseParam;

    private DataTime baseTime;

    public DerivedParameterRequest() {
        this.baseParam = new ArrayList<Object>();
    }

    public DerivedParameterRequest(DerivedParameterRequest that) {
        this();
        baseParam.addAll(that.baseParam);
        this.parameterAbbreviation = that.parameterAbbreviation;
        this.method = that.method;
        this.argumentRecords = that.argumentRecords;
        this.baseTime = that.baseTime;
    }

    public DataTime getBaseTime() {
        return baseTime;
    }

    public void setBaseTime(DataTime baseTime) {
        this.baseTime = baseTime;
    }

    /**
     * Returns the parameter abbreviation for this request.
     * 
     * @return the parameter abbreviation
     */
    public String getParameterAbbreviation() {
        return parameterAbbreviation;
    }

    /**
     * Adds a base parameter used by the derived parameter script to calculate
     * the final result. Presently, this can be a <code>PluginDataObject</code>
     * which signifies the DataCubeContainer should retrieve the product from
     * the HDF5 data store or it can be a String representing a constant of some
     * kind.
     * 
     * @param param
     *            One of the data types specified above
     */
    public void addBaseParam(Object param) {
        baseParam.add(param);
    }

    /**
     * Returns the list of all base parameters that have been found for this
     * derived parameter request.
     * 
     * @return a list of base parameters
     */
    public List<Object> getBaseParams() {
        return this.baseParam;
    }

    /**
     * Sets the parameter abbreviation to the passed in string
     * 
     * @param parameterAbbreviation
     *            The target abbreviation
     */
    public void setParameterAbbreviation(String parameterAbbreviation) {
        this.parameterAbbreviation = parameterAbbreviation;
    }

    /**
     * Returns the derived parameter method to be used with the included base
     * parameters.
     * 
     * @return the method name
     */
    public String getMethod() {
        return method;
    }

    /**
     * Sets the derived parameter method to be used with the included base
     * parameters.
     * 
     * @param method
     *            the method name to set
     */
    public void setMethod(String method) {
        this.method = method;
    }

    /**
     * Returns a list of argument records. This is the actual raw data used by
     * the derived parameter script to generate the derived data.
     * 
     * @return an array of objects containing the raw data
     */
    public Object[] getArgumentRecords() {
        return argumentRecords;
    }

    /**
     * Sets the raw data to the array passed in.
     * 
     * @param argumentRecords
     *            an array of objects containing the raw data
     */
    public void setArgumentRecords(Object[] argumentRecords) {
        this.argumentRecords = argumentRecords;
    }

}
