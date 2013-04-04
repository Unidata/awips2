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
package com.raytheon.uf.common.dataaccess.geom;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.vividsolutions.jts.geom.Envelope;

/**
 * A request for any data type that is non-gridded and can be represented by a
 * geometry.
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

public interface IGeometryRequest extends IDataRequest<IGeometryData> {

    /**
     * Sets a bounding box on the request to limit the area of the request. The
     * envelope coordinates should be in Lat/Lon space. Note that not all
     * factories may support the envelope and instead may throw an
     * EnvelopeUnsupportedException or ignore the envelope.
     * 
     * @param env
     *            the envelope to constrain the request
     */
    public void setEnvelope(Envelope env);

    /**
     * Returns the envelope set on the request.
     * 
     * @return the envelope set on the request
     */
    public Envelope getEnvelope();

    /**
     * Sets a list of location names to limit what is returned. Each datatype
     * may have its own mapping of what a location is (e.g. ICAO vs stationId vs
     * radar name, etc). Possible location names can be retrieved by using the
     * method getAvailableLocationNames(IGeometryRequest) on the DataAccessLayer
     * or IGeometryDataFactory. Note that not all factories may support requests
     * by location names and instead may throw a
     * LocationNameUnsupportedException or ignore the location names.
     * 
     * @param locationNames
     */
    public void setLocationNames(String... locationNames);

    /**
     * Returns the location names set on the request.
     * 
     * @return
     */
    public String[] getLocationNames();

}
