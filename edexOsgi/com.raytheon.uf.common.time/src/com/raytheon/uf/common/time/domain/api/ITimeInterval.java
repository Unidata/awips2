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

import com.raytheon.uf.common.time.domain.ITimeIntervalTypeAdapter;

/**
 * Represents a specific interval in time.
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
@XmlJavaTypeAdapter(value = ITimeIntervalTypeAdapter.class)
public interface ITimeInterval {

    /**
     * Return the start {@link ITimePoint} of the {@link ITimeInterval}.
     * 
     * @return the time point the interval started
     */
    ITimePoint getStart();

    /**
     * Return the end {@link ITimePoint} of the {@link ITimeInterval}.
     * 
     * @return the time point the interval ended
     */
    ITimePoint getEnd();

    /**
     * Check whether an {@link ITimePoint} falls within the
     * {@link ITimeInterval}.
     * 
     * @param timePoint
     *            the time point
     * @return true if the interval contains the point
     */
    boolean containsTimePoint(ITimePoint timePoint);

    /**
     * Retrieve the duration of the {@link ITimeInterval}.
     * 
     * @return the duration
     */
    IDuration getDuration();
}
