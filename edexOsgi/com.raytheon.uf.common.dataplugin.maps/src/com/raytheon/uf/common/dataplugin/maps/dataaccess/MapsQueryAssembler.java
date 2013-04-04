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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang.BooleanUtils;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataplugin.maps.dataaccess.util.MapsQueryUtil;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Constructs a query to retrieve information from the maps database based on
 * the supplied information.
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

public class MapsQueryAssembler {
    public static final class REQUIRED_IDENTIFIERS {
        /*
         * The table to retrieve the data from.
         */
        public static final String IDENTIFIER_TABLE = "table";

        /*
         * The first field that will be selected - the geometry that we would
         * like to retrieve. We will verify that it is not already in the column
         * list.
         */
        public static final String IDENTIFIER_GEOM_FIELD = "geomField";
    }

    /*
     * Other common identifiers we may encounter.
     */
    public static final class IDENTIFIERS {
        /*
         * Used to specify if the factory should look for information that is
         * within the specified locations or information that excludes the
         * specified locations. If this identifier is not specified, the default
         * will be to look for information within the specified location.
         */
        public static final String IDENTIFIER_IN_LOCATION = "inLocation";

        /*
         * The name of the location field, defaults to "name".
         */
        public static final String IDENTIFIER_LOCATION_FIELD = "locationField";
    }

    private static final List<String> RESERVED_IDENTIFIERS = Arrays.asList(
            REQUIRED_IDENTIFIERS.IDENTIFIER_TABLE,
            REQUIRED_IDENTIFIERS.IDENTIFIER_GEOM_FIELD,
            IDENTIFIERS.IDENTIFIER_IN_LOCATION,
            IDENTIFIERS.IDENTIFIER_LOCATION_FIELD);

    private static final String DEFAULT_LOCATION_FIELD = "name";

    /**
     * Constructor
     */
    private MapsQueryAssembler() {
    }

    /**
     * Retrieves a named identifier from the request
     * 
     * @param request
     *            the original request that we are processing
     * @param identifierName
     *            the name of the identifier to extract
     * @return the identifier
     */
    public static String extractIdentifier(IDataRequest request,
            String identifierName) {
        return request.getIdentifiers().get(identifierName).toString();
    }

    /**
     * Retrieves the table identifier
     * 
     * @param request
     *            the original request that we are processing
     * @return the table identifier
     */
    public static String extractTable(IDataRequest request) {
        return extractIdentifier(request, REQUIRED_IDENTIFIERS.IDENTIFIER_TABLE);
    }

    /**
     * Retrieves the geometry field identifier
     * 
     * @param request
     *            the original request that we are processing
     * @return the geometry identifier
     */
    public static String extractGeomField(IDataRequest request) {
        return extractIdentifier(request,
                REQUIRED_IDENTIFIERS.IDENTIFIER_GEOM_FIELD);
    }

    /**
     * Constructs a query to retrieve data from the maps database
     * 
     * @param request
     *            the original request that we are processing
     * @return the query
     */
    public static String assembleGetData(IDataRequest request) {
        return assembleQuery(request, Boolean.FALSE);
    }

    /**
     * Constructs a query to retrieve locations from the database
     * 
     * @param request
     *            the original request that we are processing
     * @return the query
     */
    public static String assembleGetAvailableLocationNames(
IDataRequest request) {
        return assembleQuery(request, Boolean.TRUE);
    }

    /**
     * Constructs a query to retrieve information from the database
     * 
     * @param request
     *            the original request that we are processing
     * @param locationQuery
     *            indicates whether or not this query will be used to retrieve
     *            location information
     * @return the query
     */
    private static String assembleQuery(IDataRequest request,
            boolean locationQuery) {
        Envelope envelope = request.getEnvelope();
        String table = extractTable(request);
        String geomField = extractGeomField(request);
        String locationField = DEFAULT_LOCATION_FIELD;
        if (request.getIdentifiers().containsKey(
                IDENTIFIERS.IDENTIFIER_LOCATION_FIELD)) {
            locationField = request.getIdentifiers()
                    .get(IDENTIFIERS.IDENTIFIER_LOCATION_FIELD).toString();
        }

        List<String> columns = new ArrayList<String>();
        if (locationQuery == false) {
            // the first column will always be the geometry.
            columns.add("AsBinary(" + geomField + ")");
        }
        // the second column will always be the location name
        columns.add(locationField);
        if (locationQuery == false) {
            // add any additional database columns the user has specified as
            // parameters
            // for additional information, refer to: http://tinyurl.com/arnayco
            for (String parameter : request.getParameters()) {
                columns.add(parameter);
            }
        }
        List<String> constraints = new ArrayList<String>();
        // add location constraint (ifdef)
        if ((request.getLocationNames() == null) == false
                && request.getLocationNames().length > 0) {
            boolean inLocation = Boolean.TRUE;
            if (request.getIdentifiers().containsKey(
                    IDENTIFIERS.IDENTIFIER_IN_LOCATION)) {
                inLocation = BooleanUtils.toBoolean(request.getIdentifiers()
                        .get(IDENTIFIERS.IDENTIFIER_IN_LOCATION).toString());
            }

            constraints.add(buildInConstraint(request.getLocationNames(),
                    locationField, inLocation));
        }
        // add remaining identifiers to constraints (ifdef)
        Iterator<String> identifiersIterator = request.getIdentifiers()
                .keySet().iterator();
        while (identifiersIterator.hasNext()) {
            String identifierKey = identifiersIterator.next();
            if (RESERVED_IDENTIFIERS.contains(identifierKey)) {
                continue;
            }
            constraints.add(buildEqualsConstraint(identifierKey, request
                    .getIdentifiers().get(identifierKey).toString()));
        }

        return MapsQueryUtil.assembleMapsTableQuery(envelope, columns,
                constraints, table, geomField);
    }

    /**
     * Constructs an equality constraint
     * 
     * @param key
     *            the operand
     * @param value
     *            the expected result
     * @return the equality constraint
     */
    private static String buildEqualsConstraint(String key, String value) {
        StringBuilder stringBuilder = new StringBuilder(key);
        stringBuilder.append(" = '");
        stringBuilder.append(value);
        stringBuilder.append("'");

        return stringBuilder.toString();
    }

    /**
     * Constructs an IN or NOT IN constraint
     * 
     * @param elements
     *            a list of elements to include in the constraint
     * @param fieldName
     *            the database column name
     * @param in
     *            indicates whether this is an IN constraint or a NOT IN
     *            constraint
     * @return the constraint
     */
    private static String buildInConstraint(Object[] elements,
            String fieldName, boolean in) {
        StringBuilder stringBuilder = new StringBuilder(fieldName);
        if (in) {
            stringBuilder.append(" IN ('");
        } else {
            stringBuilder.append(" NOT IN ('");
        }
        // add the 0th location
        stringBuilder.append(elements[0]);
        stringBuilder.append("'");
        for (int i = 1; i < elements.length; i++) {
            stringBuilder.append(", '");
            stringBuilder.append(elements[i]);
            stringBuilder.append("'");
        }
        stringBuilder.append(")");

        return stringBuilder.toString();
    }
}