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
package com.raytheon.uf.edex.registry.ebxml.services.query;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.BooleanValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DateTimeValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.IntegerValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ParameterType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryDefinitionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

/**
 * 
 * Utility class for querying
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/8/2013    1682        bphillip    Initial implementation
 * 11/7/2013    1678        bphillip    Added formatArrayString
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryQueryUtil {

    /** Valid parameter types for Query Definition Parameters */
    public enum PARAMETER_DATA_TYPE {
        STRING, TAXONOMYELEMENT, BOOLEAN, INTEGER, DATETIME
    }

    /**
     * Pattern used to replace the registry wildcard character of ? to the
     * database wildcard of _
     */
    private static final Pattern REPLACE_LIKE_CHARS_PATTERN = Pattern
            .compile("\\?");

    /**
     * Pattern used to format the string representation of arrays
     */
    private static final Pattern ARRAY_STRING_PATTERN = Pattern
            .compile("(\\[)(.*)(\\])");

    /**
     * Replaces the registry character wilcard (?) with the database wildcard
     * (_)
     * 
     * @param str
     *            The string to modify
     * @return The modified string
     */
    public static String substituteWildcardCharacters(String str) {
        return REPLACE_LIKE_CHARS_PATTERN.matcher(str).replaceAll("_");
    }

    /**
     * Gets the query parameters from a query definition in a format so it may
     * be passed to the SessionManagedDao executing the query
     * 
     * @param queryDef
     *            The Query Definition
     * @param query
     *            The query object
     * @return Array of parameter names and values to pass to the data access
     *         object
     * @throws MsgRegistryException
     *             If errors occur while converting the default value
     */
    public static Object[] getQueryParameters(QueryDefinitionType queryDef,
            QueryType query) throws MsgRegistryException {
        List<ParameterType> parameters = queryDef.getParameter();
        if (CollectionUtil.isNullOrEmpty(parameters)) {
            return new Object[] {};
        }
        List<Object> params = new ArrayList<Object>(parameters.size() * 2);

        for (ParameterType param : parameters) {
            String paramName = param.getParameterName();
            Object paramValue = query.getSlotValue(paramName);
            if (paramValue == null && param.getDefaultValue() != null) {
                paramValue = convertObject(getDataType(param.getDataType()),
                        param.getDefaultValue());
            }
            params.add(paramName);
            params.add(paramValue);
        }
        return params.toArray(new Object[params.size()]);
    }

    /**
     * Adds a slot to the query
     * 
     * @param value
     *            The value to add
     * @param parameter
     *            The parameter definition to determine method of adding slot to
     *            the query
     * @param query
     *            The query to add the parameter slot to
     * @throws MsgRegistryException
     *             If errors occur while creating calendar objects or if an
     *             unsupported data type is detected
     */
    public static void addSlotToQuery(String value, ParameterType parameter,
            QueryType query) throws MsgRegistryException {

        PARAMETER_DATA_TYPE paramType = getDataType(parameter.getDataType());
        String paramName = parameter.getParameterName();

        if (paramType == null) {
            throw EbxmlExceptionUtil.createUnsupportedCapabilityExceptionType(
                    "Unsupported parameter dataType!", "The data type ["
                            + parameter.getDataType()
                            + "] is currently unsupported");
        }
        switch (paramType) {
        case STRING:
        case TAXONOMYELEMENT:
            query.getSlot().add(
                    new SlotType(paramName, new StringValueType(value)));
            break;
        case BOOLEAN:
            query.getSlot().add(
                    new SlotType(paramName, new BooleanValueType(Boolean
                            .parseBoolean(value))));
            break;
        case INTEGER:
            query.getSlot().add(
                    new SlotType(paramName, new IntegerValueType(
                            new BigInteger(value))));
            break;
        case DATETIME:
            try {
                query.getSlot().add(
                        new SlotType(paramName, new DateTimeValueType(
                                DatatypeFactory.newInstance()
                                        .newXMLGregorianCalendar(value))));
            } catch (DatatypeConfigurationException e) {
                throw EbxmlExceptionUtil.createMsgRegistryException(
                        "Error creating dateTime slot!", e);
            }
            break;
        default:
            throw EbxmlExceptionUtil.createUnsupportedCapabilityExceptionType(
                    "Unsupported parameter dataType!", "The data type ["
                            + paramType + "] is currently unsupported");
        }

    }

    /**
     * Gets that enum data type from the string representation
     * 
     * @param dataType
     *            The String representation of the data type
     * @return The enum representing the data type
     */
    private static PARAMETER_DATA_TYPE getDataType(String dataType) {
        return PARAMETER_DATA_TYPE.valueOf(dataType.toUpperCase());
    }

    /**
     * Converts a string version of an object to the desired type
     * 
     * @param desiredType
     *            The desired object type
     * @param value
     *            THe value to convert
     * @return The converted value
     * @throws MsgRegistryException
     *             If errors occur while creating a XMLGregorianCalendar
     */
    private static Object convertObject(PARAMETER_DATA_TYPE desiredType,
            String value) throws MsgRegistryException {
        switch (desiredType) {
        case STRING:
        case TAXONOMYELEMENT:
            return value;
        case BOOLEAN:
            return Boolean.valueOf(value);
        case INTEGER:
            return Integer.valueOf(value);
        case DATETIME:
            try {
                return DatatypeFactory.newInstance().newXMLGregorianCalendar(
                        value);
            } catch (DatatypeConfigurationException e) {
                throw EbxmlExceptionUtil.createMsgRegistryException(
                        "Error creating XMLGregorianCalendar!", e);
            }
        default:
            throw EbxmlExceptionUtil.createUnsupportedCapabilityExceptionType(
                    "Unsupported parameter dataType!", "The data type ["
                            + desiredType + "] is currently unsupported");

        }
    }

    /**
     * Formats the string representation of an array as a comma separated list
     * 
     * @param arrayString
     *            The array string to format
     * @return The formatted string
     */
    public static String formatArrayString(Object[] arr) {
        String arrayString = Arrays.toString(arr);
        Matcher matcher = ARRAY_STRING_PATTERN.matcher(arrayString);
        if (matcher.find()) {
            return matcher.group(2);
        }
        return arrayString;
    }
}
