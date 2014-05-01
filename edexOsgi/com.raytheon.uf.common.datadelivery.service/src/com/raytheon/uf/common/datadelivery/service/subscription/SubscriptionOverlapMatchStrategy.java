package com.raytheon.uf.common.datadelivery.service.subscription;

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
import javax.xml.bind.annotation.XmlEnum;

/**
 * Strategy for how to apply the rules in the subscription overlap
 * configuration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2013   2000     djohnson     Initial creation
 * Sep 24, 2013   2386     dhladky      Added impl for other types besides grid
 * Oct 21, 2013   2292     mpduff       Moved overlap calculation logic out
 * 
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlEnum
public enum SubscriptionOverlapMatchStrategy {
    AT_LEAST_HALF("At Least Half"), MATCH_ALL("Match All"), MATCH_ANY(
            "Match Any");

    private final String displayString;

    private SubscriptionOverlapMatchStrategy(String displayString) {
        this.displayString = displayString;
    }

    public String getDisplayString() {
        return displayString;
    }
};
