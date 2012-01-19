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

package com.raytheon.uf.viz.core.style;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Abstract class of criteria to match against rules.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class MatchCriteria implements ISerializableObject {

    /**
     * Checks if the match criteria parameter is a match for this match
     * criteria, and returns an integer value rating the match, where the higher
     * the number, the better the match.
     * 
     * Note that x.matches(y) is not equal to y.matches(x).
     * 
     * @param aCriteria
     *            the criteria to compare against
     * @return the rating of the match, where a higher value is a stronger match
     *         than a lower value
     */
    public abstract int matches(MatchCriteria aCriteria)
            throws VizStyleException;
}
