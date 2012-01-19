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

package com.raytheon.edex.plugin;

import java.util.Calendar;
import java.util.TimeZone;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.edex.core.props.Properties;

/**
 * Base class for plugin decoders.
 * <p>
 * The AbstractDecoder class provides logging support and class variables used
 * in child classes. The {@link AbstractDecoder#getCalendar(String)} method
 * provides a consistent Calendar conversion from a string representation of a
 * date.
 * <p>
 * Use of inherited logging support and the getCalendar method is demonstrated
 * in the following example the decode method of FileNameDecoder
 * 
 * <pre>
 * <code>
 * if (product == null) {
 *     logger.error(&quot;Unable to locate a value for GOES product = &quot;
 *             + fileName.substring(4, 6) + &quot; and satellite = &quot; + satellite);
 *     throw new DecoderException(&quot;Unable to locate a value for GOES product = &quot;
 *             + fileName.substring(4, 6));
 * }
 * // parse the date
 * calendar = getCalendar(fileName.substring(7, 13));
 * </code>
 * &lt;pre&gt;
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/14/06                garmendariz Initial check-in
 * 11/02/06     #39        brockwoo    Added wmo header field
 * 
 * &lt;/pre&gt;
 * &#064;author garmendariz
 * &#064;version 1.0
 * 
 */
public abstract class AbstractDecoder {

    /** The logger */
    protected Log logger = LogFactory.getLog(getClass());

    /** The file name to parse */
    protected String fileName;

    /** The wmo header to parse */
    protected String wmoHeader;

    /** The plugin properties */
    protected Properties properties;

    /**
     * Retrieves a Calendar instance from a formatted date as a string
     * 
     * @param aString
     *            A formatted date as a string
     * @return An instance of the Calendar class set to the parameter date
     */
    protected Calendar getCalendar(String aString) {
        Calendar calendar = null;

        calendar = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        calendar.set(Calendar.DAY_OF_MONTH, Integer.parseInt(aString.substring(
                0, 2)));
        calendar.set(Calendar.HOUR_OF_DAY, Integer.parseInt(aString.substring(
                2, 4)));
        calendar
                .set(Calendar.MINUTE, Integer.parseInt(aString.substring(4, 6)));
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);

        return calendar;

    }

    public Properties getProperties() {
        return properties;
    }

    public void setProperties(Properties properties) {
        this.properties = properties;
    }

}
