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

import java.util.Date;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.mpe.ui.Activator;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.rsc.MPEFieldResource;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Handler class for saving the current best estimate
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 02, 2013           mschenke  Initial creation
 * Feb 26, 2014  2842     mpduff    Use PlatformUI rather than HandlerUtil.
 * Jun 20, 2019  7137     bhurley   Changed data type to allow for accumulation
 *                                  values greater than 13 inches.
 * 
 * </pre>
 * 
 * @author mschenke
 */

public class SaveBestEstimateHandler extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IEditorPart part = EditorUtil.getActiveEditor();
        if (part instanceof IDisplayPaneContainer) {
            IDisplayPaneContainer container = (IDisplayPaneContainer) part;
            IDisplayPane toSave = getPaneToSave(container);
            if (toSave != null) {
                MPEDisplayManager.stopLooping(container);

                IWorkbenchWindow activeWindow = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow();
                Shell shell = activeWindow.getShell();
                try {
                    shell.setCursor(shell.getDisplay()
                            .getSystemCursor(SWT.CURSOR_WAIT));
                    MPEDisplayManager displayMgr = MPEDisplayManager
                            .getInstance(toSave);
                    MPEFieldResource resource = displayMgr
                            .getDisplayedFieldResource();
                    if (resource == null) {
                        // Ensure there is not a displayed resource on the pane
                        MessageBox box = new MessageBox(shell, SWT.ERROR);
                        box.setText("Cannot Save");
                        box.setMessage("No Data Available, cannot save");
                        box.open();
                    } else {
                        Date editDate = displayMgr.getCurrentEditDate();
                        // TODO: Ensure not base field
                        DisplayFieldData displayedField = displayMgr
                                .getDisplayFieldType();
                        int[] editedIntData = resource
                                .getData(new DataTime(editDate));

                        short[] editedData = new short[editedIntData.length];
                        for (int i = 0; i < editedData.length; i++) {
                            int value = editedIntData[i];
                            if (value > Short.MAX_VALUE) {
                                value = Short.MAX_VALUE;
                            } else if (value < Short.MIN_VALUE) {
                                value = Short.MIN_VALUE;
                            }
                            editedData[i] = (short) value;
                        }

                        SaveBestEstimate.saveBestEstimate(editDate,
                                displayedField, editedData,
                                toSave.getTarget().screenshot());
                    }
                } catch (VizException e) {
                    Activator.statusHandler.handle(Priority.PROBLEM,
                            "Error getting data from display for storing", e);
                } finally {
                    // Reset cursor
                    shell.setCursor(null);
                }
            }
        }
        return null;
    }

    protected IDisplayPane getPaneToSave(IDisplayPaneContainer container) {
        IDisplayPane[] panes = container.getDisplayPanes();
        if (panes.length > 0) {
            return panes[0];
        }
        return null;
    }

}
