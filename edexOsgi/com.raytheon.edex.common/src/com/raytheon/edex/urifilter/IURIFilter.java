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
package com.raytheon.edex.urifilter;

import com.raytheon.edex.msg.DataURINotificationMessage;

/**
 * Interface IURIFilter
 * 
 * Interface for searching for groups of of URI's that currently are plugged
 * into EDEX. It listens to the JMS message topic that notifies DataURI's to
 * client CAVE instances.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06Feb2009    1981       dhladky     Creation.
 * author dhladky
 * @version 1.0
 */

public interface IURIFilter {

    /**
     * Check a URI message based on a rules you apply for construction in
     * implemented method.
     * 
     * @param matchURIs
     * @return
     */

    public void matchURIs(DataURINotificationMessage message);

}
