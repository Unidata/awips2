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
package com.raytheon.uf.edex.decodertools.time;

import java.util.Calendar;

/**
 * ITimeService declares an interface that returns a calendar. The most simple
 * implementation would be
 * 
 * <pre><code>
 * public Calendar getCalendar() {
 *   return Calendar.getInstance(TimeZone.getTimeZone(&quot;GMT&quot;))};
 * }
 * </code>
 * Which would just return a calendar. Other implementations could modify the
 * calendar based on some offset, return a constant calendar for testing, etc.
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071019            391 jkorman     Initial Development
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public interface ITimeService {

    /**
     * Return a none null Calendar instance.
     * 
     * @return A Calendar instance.
     */
    public Calendar getCalendar();
}
