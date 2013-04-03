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
package com.raytheon.uf.common.dataaccess;

import java.util.Map;

import com.raytheon.uf.common.dataplugin.level.Level;

/**
 * A generic request for geospatial data to the Data Access Framework. All
 * requests must have a datatype set.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public interface IDataRequest<D extends IData> {

    /**
     * The datatype of the data, usually the pluginName. This value will be used
     * as a key to determine the corresponding IDataFactory that should process
     * the request. This is required to be non-null.
     * 
     * @param datatype
     *            the datatype of the data to request
     */
    public void setDatatype(String datatype);

    /**
     * Adds an identifier the factory can use to determine exactly what data is
     * desired. Note that sub-interfaces may add methods for specific
     * identifying characteristics.
     * 
     * @param key
     *            the name of the identifier
     * @param value
     *            the value desired on all the return data
     */
    public void addIdentifier(String key, Object value);

    /**
     * Sets the parameters to request of the data.
     * 
     * @param params
     *            the parameters to request
     */
    public void setParameters(String... params);

    /**
     * Sets the levels to request of the data. Some factories may ignore this or
     * throw an exception if all data is at one level.
     * 
     * @param levels
     *            the levels to request
     */
    public void setLevels(Level... levels);

    /**
     * Returns the datatype set on the request.
     * 
     * @return the datatype of the request
     */
    public String getDatatype();

    /**
     * Returns the identifiers added to the request.
     * 
     * @return the identifiers of the request
     */
    public Map<String, Object> getIdentifiers();

    /**
     * Returns the parameters set on the request.
     * 
     * @return the parameters of the request
     */
    public String[] getParameters();

    /**
     * Returns the levels set on the request.
     * 
     * @return the levels of the request
     */
    public Level[] getLevels();

}
