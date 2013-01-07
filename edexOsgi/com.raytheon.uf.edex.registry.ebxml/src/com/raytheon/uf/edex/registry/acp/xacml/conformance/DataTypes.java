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
package com.raytheon.uf.edex.registry.acp.xacml.conformance;

import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;

import javax.naming.InvalidNameException;
import javax.naming.ldap.LdapName;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Base64Binary;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Rfc822Name;

/**
 * 
 * Data types supported by the XACML 2.0 core spec. Defined in table 10.2.7 of
 * the XACML 2.0 core spec
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/17/2012    724          bphillip    Initial Coding
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class DataTypes {

    public static final String STRING = "http://www.w3.org/2001/XMLSchema#string";

    public static final String BOOLEAN = "http://www.w3.org/2001/XMLSchema#boolean";

    public static final String INTEGER = "http://www.w3.org/2001/XMLSchema#integer";

    public static final String DOUBLE = "http://www.w3.org/2001/XMLSchema#double";

    public static final String TIME = "http://www.w3.org/2001/XMLSchema#time";

    public static final String DATE = "http://www.w3.org/2001/XMLSchema#date";

    public static final String DATETIME = "http://www.w3.org/2001/XMLSchema#dateTime";

    public static final String DAY_TIME_DURATION = "http://www.w3.org/TR/2002/WD-xquery-operators-20020816#dayTimeDuration";

    public static final String YEAR_MONTH_DURATION = "http://www.w3.org/TR/2002/WD-xquery-operators-20020816#yearMonthDuration";

    public static final String ANY_URI = "http://www.w3.org/2001/XMLSchema#anyURI";

    public static final String HEX_BINARY = "http://www.w3.org/2001/XMLSchema#hexBinary";

    public static final String BASE_64_BINARY = "http://www.w3.org/2001/XMLSchema#base64Binary";

    public static final String RFC822_NAME = "urn:oasis:names:tc:xacml:1.0:data-type:rfc822Name";

    public static final String X500_NAME = "urn:oasis:names:tc:xacml:1.0:data-type:x500Name";

    private static DatatypeFactory dataTypeFactory;

    static {
        try {
            dataTypeFactory = DatatypeFactory.newInstance();
        } catch (DatatypeConfigurationException e) {
            e.printStackTrace();
        }
    }

    public static Object castDataType(String obj, String dataType)
            throws XACMLProcessingException {
        if (dataType.equals(STRING)) {
            return obj;
        } else if (dataType.equals(BOOLEAN)) {
            return Boolean.parseBoolean(obj);
        } else if (dataType.equals(INTEGER)) {
            return Integer.parseInt(obj);
        } else if (dataType.equals(DOUBLE)) {
            return Double.parseDouble(obj);
        } else if (dataType.equals(ANY_URI)) {
            try {
                return new URI(obj);
            } catch (URISyntaxException e) {
                throw new XACMLProcessingException("Error parsing URI", e);
            }
        } else if (dataType.equals(TIME) || dataType.equals(DATE)
                || dataType.equals(DATETIME)) {
            return dataTypeFactory.newXMLGregorianCalendar(obj);
        } else if (dataType.equals(DAY_TIME_DURATION)) {
            return dataTypeFactory.newDurationDayTime(obj);
        } else if (dataType.equals(YEAR_MONTH_DURATION)) {
            return dataTypeFactory.newDurationYearMonth(obj);
        } else if (dataType.equals(HEX_BINARY)) {
            return new BigInteger(obj, 16);
        } else if (dataType.equals(BASE_64_BINARY)) {
            return new Base64Binary(obj);
        } else if (dataType.equals(X500_NAME)) {
            try {
                return new LdapName(obj);
            } catch (InvalidNameException e) {
                throw new XACMLProcessingException("Error parsing x500 Name", e);
            }
        } else if (dataType.equals(RFC822_NAME)) {
            return new Rfc822Name(obj);
        } else {
            throw new UnsupportedOperationException("Unsupported Data Type: "
                    + dataType);
        }
    }
}
