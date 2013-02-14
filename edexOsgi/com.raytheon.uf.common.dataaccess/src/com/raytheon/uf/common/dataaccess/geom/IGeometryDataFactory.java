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

import com.raytheon.uf.common.dataaccess.IDataFactory;

/**
 * IDataFactory for any data that is non-gridded, for example points or
 * polygons.
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

public interface IGeometryDataFactory extends
        IDataFactory<IGeometryRequest, IGeometryData> {

    /**
     * Gets the available location names that match the request. Implementations
     * should throw LocationNameUnsupportedException if location names do not
     * apply to their datatype.
     * 
     * @param request
     *            the request to find matching location names for
     * @return the available location names that match the request
     */
    public String[] getAvailableLocationNames(IGeometryRequest request);

}
