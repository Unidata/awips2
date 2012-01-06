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
package com.raytheon.uf.viz.thinclient;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;

/**
 * Class for managing JMS connectivity
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class JMSConnectivityManager implements IPropertyChangeListener {

    private boolean disabled = false;

    public JMSConnectivityManager() {
        apply(Activator.getDefault().getPreferenceStore()
                .getBoolean(ThinClientPreferenceConstants.P_DISABLE_JMS));
    }

    /**
     * 
     */
    private void apply(boolean disable) {
        if (disabled != disable) {
            if (disable) {
                // TODO: Disable JMS entirely
            } else {
                // TODO: Enable JMS
            }
            disabled = !disabled;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse
     * .jface.util.PropertyChangeEvent)
     */
    @Override
    public void propertyChange(PropertyChangeEvent event) {
        if (ThinClientPreferenceConstants.P_DISABLE_JMS.equals(event
                .getProperty())) {
            Boolean disable = Boolean.parseBoolean(String.valueOf(event
                    .getNewValue()));
            apply(disable);
        }
    }

}
