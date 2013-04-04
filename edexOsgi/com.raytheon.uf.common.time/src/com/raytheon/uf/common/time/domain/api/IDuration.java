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
package com.raytheon.uf.common.time.domain.api;

import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.datatype.Duration;

import com.raytheon.uf.common.time.domain.IDurationTypeAdapter;

/**
 * Interface for a duration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 11, 2013 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlJavaTypeAdapter(value = IDurationTypeAdapter.class)
public interface IDuration {

    /**
     * Retrieve the number of nanoseconds represented by the duration.
     * Conversions from finer to coarser granularities truncate, so lose
     * precision. For example converting <tt>999</tt> milliseconds to seconds
     * results in <tt>0</tt>.
     * 
     * @return the number of nanoseconds, or 0 if the converted value is less
     *         than 1 of the new unit
     */
    long getNanos();

    /**
     * Retrieve the number of microseconds represented by the duration.
     * Conversions from finer to coarser granularities truncate, so lose
     * precision. For example converting <tt>999</tt> milliseconds to seconds
     * results in <tt>0</tt>.
     * 
     * @return the number of microseconds, or 0 if the converted value is less
     *         than 1 of the new unit
     */
    long getMicros();

    /**
     * Retrieve the number of milliseconds represented by the duration.
     * Conversions from finer to coarser granularities truncate, so lose
     * precision. For example converting <tt>999</tt> milliseconds to seconds
     * results in <tt>0</tt>.
     * 
     * @return the number of milliseconds, or 0 if the converted value is less
     *         than 1 of the new unit
     */
    long getMillis();

    /**
     * Retrieve the number of seconds represented by the duration. Conversions
     * from finer to coarser granularities truncate, so lose precision. For
     * example converting <tt>999</tt> milliseconds to seconds results in
     * <tt>0</tt>.
     * 
     * @return the number of seconds, or 0 if the converted value is less than 1
     *         of the new unit
     */
    long getSeconds();

    /**
     * Retrieve the number of minutes represented by the duration. Conversions
     * from finer to coarser granularities truncate, so lose precision. For
     * example converting <tt>999</tt> milliseconds to seconds results in
     * <tt>0</tt>.
     * 
     * @return the number of minutes, or 0 if the converted value is less than 1
     *         of the new unit
     */
    long getMinutes();

    /**
     * Retrieve the number of hours represented by the duration. Conversions
     * from finer to coarser granularities truncate, so lose precision. For
     * example converting <tt>999</tt> milliseconds to seconds results in
     * <tt>0</tt>.
     * 
     * @return the number of hours, or 0 if the converted value is less than 1
     *         of the new unit
     */
    long getHours();

    /**
     * Retrieve the number of hours represented by the duration. Conversions
     * from finer to coarser granularities truncate, so lose precision. For
     * example converting <tt>999</tt> milliseconds to seconds results in
     * <tt>0</tt>.
     * 
     * @return the number of days, or 0 if the converted value is less than 1 of
     *         the new unit
     */
    long getDays();

    /**
     * Add another {@link Duration} to this one.
     * 
     * @param anotherDuration
     * @return the duration of the sum
     */
    IDuration plus(IDuration anotherDuration);

    /**
     * Subtract another {@link Duration} from this one.
     * 
     * @param anotherDuration
     * @return the duration of the difference
     */
    IDuration minus(IDuration anotherDuration);
}