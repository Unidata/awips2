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

import java.util.Date;

import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.time.domain.ITimePointTypeAdapter;

/**
 * Represents an instance in time.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2013 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlJavaTypeAdapter(value = ITimePointTypeAdapter.class)
public interface ITimePoint {

    /**
     * Return the {@link ITimePoint} as a {@link Date}.
     * 
     * @return the {@link ITimePoint} as a {@link Date}
     */
    Date asDate();

    /**
     * Return the {@link ITimePoint} as the milliseconds which would be returned
     * by {@link Date}.
     * 
     * @return the milliseconds
     */
    long asMilliseconds();

    /**
     * Check whether this {@link ITimePoint} is before another one.
     * 
     * @param anotherPoint
     *            the other {@link ITimePoint}
     * @return true if this point in time is before the other one
     */
    boolean isBefore(ITimePoint anotherPoint);

    /**
     * Check whether this {@link ITimePoint} is after another one.
     * 
     * @param anotherPoint
     *            the other {@link ITimePoint}
     * @return true if this point in time is after the other one
     */
    boolean isAfter(ITimePoint anotherPoint);

    /**
     * Check whether this {@link ITimePoint} is the same as another one.
     * 
     * @param anotherPoint
     *            the other {@link ITimePoint}
     * @return true if this point in time is the same as the other one
     */
    boolean isSame(ITimePoint anotherPoint);

    /**
     * Check whether this {@link ITimePoint} falls within the specified
     * {@link ITimeInterval}.
     * 
     * @param interval
     *            the {@link ITimeInterval}
     * @return true if this point in time is within the time interval
     */
    boolean isWithin(ITimeInterval interval);
}
