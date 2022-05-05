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
package com.raytheon.viz.xdat;

import org.eclipse.swt.widgets.Display;

import com.raytheon.viz.ui.personalities.awips.AbstractCAVEDialogComponent;

/**
 * This class starts XdatDialog as a stand alone component with runtime mode of
 * ALERT_VIZ.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------
 * Apr 28, 2011           mschenke  Initial creation
 * Oct 08, 2012  1229     rferrel   Make a blocking dialog.
 * Oct 17, 2012  1229     rferrel   Changes for non-blocking XdatDlg.
 * Jan 26, 2016  5054     randerso  Made XdatDlg parented to display
 * May 01, 2018  7027     mduff     Change to how XDat is launched.
 * Jul 10, 2019  7027     randerso  Actually open the dialog.
 * 
 * </pre>
 * 
 * @author mschenke
 */

public class XdatComponent extends AbstractCAVEDialogComponent {

    @Override
    protected void startInternal(String componentName) throws Exception {

        XdatDlg xdatDialg = XdatDlg.getInstance(Display.getCurrent());
        xdatDialg.open();
        blockUntilClosed(xdatDialg);
    }

    @Override
    protected int getRuntimeModes() {
        return (ALERT_VIZ);
    }

}