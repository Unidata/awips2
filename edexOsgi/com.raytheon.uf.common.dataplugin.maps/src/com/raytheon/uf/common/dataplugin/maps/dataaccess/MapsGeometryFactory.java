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
package com.raytheon.uf.common.dataplugin.maps.dataaccess;

import java.util.Map;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.impl.AbstractGeometryTimeAgnosticDatabaseFactory;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

/**
 * A data factory for retrieving data from the maps database. Currently, the
 * name of the table to retrieve data from and the name of the geometry field of
 * interest are required identifiers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 28, 2013            bkowal     Initial creation
 * Feb 14, 2013 1614       bsteffen    Refactor data access framework to use
 *                                     single request.
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */
public class MapsGeometryFactory extends
        AbstractGeometryTimeAgnosticDatabaseFactory {
    private static final String[] REQUIRED_IDENTIFIERS = new String[] {
            MapsQueryAssembler.REQUIRED_IDENTIFIERS.IDENTIFIER_TABLE,
            MapsQueryAssembler.REQUIRED_IDENTIFIERS.IDENTIFIER_GEOM_FIELD };

    private static final String MAPS_DATABASE = "maps";

    private static final ThreadLocal<WKBReader> wkbReaderWrapper = new ThreadLocal<WKBReader>() {
        @Override
        protected WKBReader initialValue() {
            return new WKBReader();
        }
    };

    /**
     * Constructor
     */
    public MapsGeometryFactory() {
        super(MAPS_DATABASE, REQUIRED_IDENTIFIERS);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.impl.AbstractGeometryDatabaseFactory
     * #makeGeometry(java.lang.Object[], java.lang.String[], java.util.Map)
     */
    @Override
    protected IGeometryData makeGeometry(Object[] data, String[] paramNames,
            Map<String, Object> attrs) {
        // order selected geom field, location, and other parameters

        // build the geometry
        Geometry geometry = null;
        Object object = data[0];
        if ((object instanceof byte[]) == false) {
            throw new DataRetrievalException(
                    "Retrieved Geometry was not the expected type; was expecting byte[], received: "
                            + object.getClass().getName());
        }
        try {
            geometry = (wkbReaderWrapper.get()).read((byte[]) object);
        } catch (ParseException e) {
            throw new DataRetrievalException("Failed to parse the geometry.", e);
        }
        String location = (String) data[1];

        return super.buildGeometryData(null, null, geometry, location, attrs,
                2, data, paramNames);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.dataaccess.impl.
     * AbstractGeometryTimeAgnosticDatabaseFactory
     * #assembleGetData(com.raytheon.uf.common.dataaccess.geom.IDataRequest)
     */
    @Override
    protected String assembleGetData(IDataRequest request) {
        return MapsQueryAssembler.assembleGetData(request);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.impl.AbstractGeometryDatabaseFactory
     * #assembleGetAvailableLocationNames
     * (com.raytheon.uf.common.dataaccess.geom.IDataRequest)
     */
    @Override
    protected String assembleGetAvailableLocationNames(IDataRequest request) {
        return MapsQueryAssembler.assembleGetAvailableLocationNames(request);
    }
}