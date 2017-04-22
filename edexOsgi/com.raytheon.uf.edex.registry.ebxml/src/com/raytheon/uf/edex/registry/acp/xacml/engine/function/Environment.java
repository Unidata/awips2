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
package com.raytheon.uf.edex.registry.acp.xacml.engine.function;

import java.util.Calendar;

import com.raytheon.uf.edex.registry.acp.xacml.conformance.DataTypes;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.util.XACMLObjectUtil;

/**
 * 
 * Utility class containing the environment functions
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
public class Environment {

    /** The id for the current time attribute */
    public static final String CURRENT_TIME = "urn:oasis:names:tc:xacml:1.0:environment:current-time";

    /** The id for the current date attribute */
    private static final String CURRENT_DATE = "urn:oasis:names:tc:xacml:1.0:environment:current-date";

    /** The id for the current date time attribute */
    private static final String CURRENT_DATE_TIME = "urn:oasis:names:tc:xacml:1.0:environment:current-dateTime";

    /** The singleton instance of the Envirnoment utility class */
    private static Environment instance = new Environment();

    /**
     * Private constructor
     */
    private Environment() {

    }

    /**
     * Gets the singleton instance of the Environment utility class
     * 
     * @return The singleton instance of the Environment utility class
     */
    public static synchronized Environment getInstance() {
        return instance;
    }

    /**
     * Gets the current time. The time is represented as a Calendar object with
     * the year, month, and day fields unset
     * 
     * @return The current time Calendar object
     */
    private Calendar getCurrentTime() {
        Calendar cal = Calendar.getInstance();
        cal.set(Calendar.YEAR, 2000);
        cal.set(Calendar.MONTH, 1);
        cal.set(Calendar.DAY_OF_MONTH, 1);
        cal.set(Calendar.MILLISECOND, 0);
        return cal;
    }

    /**
     * Gets the current date. The date is represented as a Calendar object with
     * the hour, minute, second and millisecond fields unset
     * 
     * @return The current date Calendar object
     */
    private Object getCurrentDate() {
        Calendar cal = Calendar.getInstance();
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        return cal;
    }

    /**
     * Gets the current date and time. The datetime is represented as a Calendar
     * object with the millisecond field unset.
     * 
     * @return
     */
    private Object getCurrentDateTime() {
        Calendar cal = Calendar.getInstance();
        cal.set(Calendar.MILLISECOND, 0);
        return cal;
    }

    /**
     * Gets an attribute from the environment
     * 
     * @param id
     *            The id of the attribute to get from the environment
     * @param dataType
     *            The expected data type of the attribute from the environment
     * @return The value of the environment attribute
     * @throws XACMLProcessingException
     *             If errors occur casting the result to the correct data type
     *             or if the returned attribute is null;
     */
    public Object getEnvironmentValue(String id, String dataType)
            throws XACMLProcessingException {
        Object retVal = null;
        if (id.equals(CURRENT_TIME)) {
            retVal = getCurrentTime();
        } else if (id.equals(CURRENT_DATE)) {
            retVal = getCurrentDate();
        } else if (id.equals(CURRENT_DATE_TIME)) {
            retVal = getCurrentDateTime();
        } else {
            String envAttr = XACMLObjectUtil.stripIdentifierPrefix(id);
            String value = System.getenv().get(envAttr);
            if (value != null) {
                retVal = DataTypes.castDataType(value, dataType);
            }
        }
        if (retVal == null) {
            throw new XACMLProcessingException("Value [" + id
                    + "] not present in environment");
        }
        return retVal;
    }
}
