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
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.TimeUnit;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * The DerivedParameteRequest is the way for a class to communicate a request
 * for a derived parameter to the DerivedParameterGenerator thread. This class
 * works closely with <code>DerivedParameterGenerator</code> to get the
 * requested parameter back.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 9, 2008				brockwoo	Initial creation
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

    private SynchronousQueue<List<?>> queue;

    public DerivedParameterRequest() {
        this.queue = new SynchronousQueue<List<?>>();
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

    /**
     * This method will return the data calculated by the derived parameter
     * script. It is important to call this method right after the request has
     * been submitted to the derived parameter thread as it will block for 30
     * seconds waiting for the response.
     * 
     * @return the raw data calculated by the script
     */
    public List<?> getQueue() throws VizException {
        try {
            List<?> result = queue.poll(30, TimeUnit.SECONDS);
            if (result == null) {
                throw new VizException("Derived Parameter Timed Out");
            }
            return result;
        } catch (InterruptedException e) {
            throw new VizException(e);
        }
    }

    /**
     * Used by the derived parameter generator to return the calculated data to
     * the thread requesting it. This will block until <code>getQueue</code> is
     * called.
     * 
     * @param queue
     *            the data being returned to the thread making the request
     * @throws VizException
     */
    public void setQueue(List<?> queue) {
        try {
            this.queue.offer(queue, 15, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

}
