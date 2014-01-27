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

package com.raytheon.uf.common.style;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElementRef;

/**
 * A rule for visualization style.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         chammack    Initial creation
 * Nov 14, 2013 2361       njensen     Remove ISerializableObject
 * 
 * </pre>
 * 
 * 
 */

@XmlAccessorType(XmlAccessType.NONE)
public class StyleRule {

    @XmlElementRef
    private MatchCriteria matchCriteria;

    @XmlElementRef
    private AbstractStylePreferences preferences;

    /**
     * @return the matchCriteria
     */
    public MatchCriteria getMatchCriteria() {
        return matchCriteria;
    }

    /**
     * @param matchCriteria
     *            the matchCriteria to set
     */
    public void setMatchCriteria(MatchCriteria matchCriteria) {
        this.matchCriteria = matchCriteria;
    }

    /**
     * @return the preferences
     */
    public AbstractStylePreferences getPreferences() {
        return preferences;
    }

    /**
     * @param preferences
     *            the preferences to set
     */
    public void setPreferences(AbstractStylePreferences preferences) {
        this.preferences = preferences;
    }

}
