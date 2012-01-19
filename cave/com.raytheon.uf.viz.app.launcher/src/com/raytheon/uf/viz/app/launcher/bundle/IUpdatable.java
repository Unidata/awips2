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
package com.raytheon.uf.viz.app.launcher.bundle;

/**
 * Interface providing a data update capability for Application Launcher
 * bundle classes.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2009 2081       mfegan     Initial creation
 *
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */
public interface IUpdatable {
    /**
     * Requests that the IUpdatable modify its data using the provided regex
     * and substitute. Classes implementing this interface must determine the
     * appropriate mechanism for performing the substitution. The regex must
     * be a valid regular expression -- special characters must be escaped.
     * 
     * @param regex the token to replace
     * @param replacement the value to substitute
     */
    public void updateData(String regex, String replacement);
}
