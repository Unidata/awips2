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
import com.vividsolutions.jts.geom.Envelope;

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
 * Feb 14, 2013 1614       bsteffen    Refactor data access framework to use
 *                                     single request.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public interface IDataRequest {

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
     * Sets a list of location names to limit what is returned. Each datatype
     * may have its own mapping of what a location is (e.g. ICAO vs stationId vs
     * radar name, etc). Possible location names can be retrieved by using the
     * method getAvailableLocationNames(IGeometryRequest) on the DataAccessLayer
     * or IGeometryDataFactory. Note that not all factories may support requests
     * by location names and instead may throw a IncompatibleRequestException or
     * ignore the location names.
     * 
     * @param locationNames
     */
    public void setLocationNames(String... locationNames);

    /**
     * Returns the datatype set on the request.
     * 
     * @return the datatype of the request
     */
    public String getDatatype();

    /**
     * Sets a bounding box on the request to limit the area of the request. The
     * envelope coordinates should be in Lat/Lon space. Note that not all
     * factories may support the envelope and instead may throw an
     * IncompatibleRequestException or ignore the envelope.
     * 
     * @param env
     *            the envelope to constrain the request
     */
    public void setEnvelope(Envelope env);

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

    /**
     * Returns the location names set on the request.
     * 
     * @return
     */
    public String[] getLocationNames();

    /**
     * Returns the envelope set on the request.
     * 
     * @return the envelope set on the request
     */
    public Envelope getEnvelope();

}
