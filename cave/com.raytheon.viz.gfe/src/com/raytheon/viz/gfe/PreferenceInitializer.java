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
package com.raytheon.viz.gfe;


/**
 * Used to initialize static variables from preference settings.
 * The <code>init</code> method is invoked automatically 
 * when the GFE configuration is changed.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/23/2011    9897      ryu         Initial Creation.
 * 
 * </pre>
 * 
 * @author ryu
 * @version 1.0
 */

public abstract class PreferenceInitializer implements
        IConfigurationChangeListener {
    
    public abstract void init();
        
    public void run() {
        Activator.getDefault()
                .getPreferenceStore()
                .addConfigurationChangeListener(this);
        init();
    }

    @Override
    public void configurationChanged(String config) {
        init();
    }
}
