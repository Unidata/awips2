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
package com.raytheon.viz.mpe.ui.actions;

import java.util.ArrayList;

import org.eclipse.core.runtime.Platform;

import com.raytheon.viz.mpe.util.AutoDailyQC;
import com.raytheon.viz.ui.personalities.awips.AbstractCAVEComponent;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 1, 2011            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class StartAutoDQC extends AbstractCAVEComponent {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.personalities.awips.AbstractCAVEComponent#startInternal
     * (java.lang.String)
     */
    @Override
    protected void startInternal(String componentName) throws Exception {
        String[] args = Platform.getApplicationArgs();
        String key = "autodqc";
        boolean keyfound = false;
        int index = 0;

        ArrayList<String> argval = new ArrayList<String>();
        for (int i = 0; i < args.length; i++) {
            if (args[i].equals(key)) {
                keyfound = true;
                index = i + 1;
            }
            if (keyfound == true && i >= index) {
                argval.add(args[i]);

            }
        }
        try {
            AutoDailyQC adqc = new AutoDailyQC();
            adqc.runAutoDailyQC(argval.toArray(new String[argval.size()]));
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.personalities.awips.AbstractCAVEComponent#getRuntimeModes
     * ()
     */
    @Override
    protected int getRuntimeModes() {
        // TODO Auto-generated method stub
        return 0;
    }

}
