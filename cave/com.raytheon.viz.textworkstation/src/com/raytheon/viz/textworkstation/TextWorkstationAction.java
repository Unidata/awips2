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

package com.raytheon.viz.textworkstation;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Display;

import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Action for unimplemented features. To be used temporarily until final
 * behavior is implemented.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 6/27/06                  randerso    Initial creation.
 * 10/11/2007   482         grichard    Reformatted file.
 * 08/03/2011   9572        rferrel     Allow single instance of the dialog.
 * 26Sep2012    1196        lvenable    Update for the dialog refactor.
 * Jan 26, 2016 5054        randerso    Changed to use display as parent
 * 
 * </pre>
 * 
 * @author randerso
 * 
 */
public class TextWorkstationAction extends AbstractHandler {

    private static TextWorkstationDlg textWorkstationDlg;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        if ((textWorkstationDlg == null) || textWorkstationDlg.isDisposed()) {
            textWorkstationDlg = new TextWorkstationDlg(Display.getCurrent());
            textWorkstationDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    textWorkstationDlg = null;
                }
            });
            textWorkstationDlg.open();
        } else {
            textWorkstationDlg.bringToTop();
        }

        return null;
    }
}
