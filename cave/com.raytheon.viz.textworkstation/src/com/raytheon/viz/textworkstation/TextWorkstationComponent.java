package com.raytheon.viz.textworkstation;

import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.viz.ui.personalities.awips.AbstractCAVEDialogComponent;

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

/**
 * This class used to made a stand alone Text workstation.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2011            mschenke    Initial creation
 * Oct 02, 2012 1229       rferrel     Make a blocking dialog.
 * Oct 17, 2012 1229       rferrel     Changes for non-blocking TextWorkstationDlg.
 * Sep 09, 2014 3580       mapeters    Removed {@link SerializationUtil} usage.
 * Oct 28, 2015 5054       randerso    Make TextWorkstationDlg appear on current monitor.
 * Jan 26, 2016 5054       randerso    Changed to use display as parent
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TextWorkstationComponent extends AbstractCAVEDialogComponent {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.personalities.awips.AbstractCAVEComponent#startInternal
     * (java.lang.String)
     */
    @Override
    protected void startInternal(String componentName) throws Exception {
        TextWorkstationDlg textWorkstationDlg = new TextWorkstationDlg(
                Display.getCurrent());
        textWorkstationDlg.open();
        blockUntilClosed(textWorkstationDlg);
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
        return ALERT_VIZ;
    }

}
