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
package com.raytheon.viz.ui.actions;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.ProcedureXmlManager;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.UiUtil;

/**
 * Save a bundle to disk
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 29, 2007           chammack    Initial Creation.
 * Oct 22, 2013  2491     bsteffen    Switch serialization to
 *                                    ProcedureXmlManager
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class SaveBundle extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        String fileName = null;
        FileDialog fd = new FileDialog(shell, SWT.SAVE);
        fd.setOverwrite(true);
        fd.setFileName(fileName);
        fd.setFilterExtensions(new String[] { "*.xml" });
        fd.setFilterPath(System.getProperty("user.home"));
        while (fileName == null) {
            String retVal = fd.open();
            if (retVal == null) {
                return null;
            }

            String name = fd.getFileName();
            fileName = fd.getFilterPath() + File.separator + name;
            if (name.endsWith(".xml") == false) {
                name += ".xml";
                fd.setFileName(name);
                fileName = fd.getFilterPath() + File.separator + name;
                if (new File(fileName).exists()) {
                    boolean result = MessageDialog
                            .openQuestion(
                                    shell,
                                    "Confirm Overwrite",
                                    "A file named \""
                                            + name
                                            + "\" already exists.  Do you want to replace it?");
                    if (result == false) {
                        fileName = null;
                    }
                }
            }
        }

        try {
            Bundle bundle = extractCurrentBundle();
            ProcedureXmlManager.getInstance().marshalToFile(bundle, fileName);
        } catch (Exception e) {
            Status status = new Status(Status.ERROR, UiPlugin.PLUGIN_ID, 0,
                    "Error occurred during bundle save.", e);
            ErrorDialog.openError(Display.getCurrent().getActiveShell(),
                    "ERROR", "Error occurred during bundle save.", status);
            throw new ExecutionException("Error occurred during bundle save", e);
        }
        return null;
    }

    public static Bundle extractCurrentBundle() {
        IRenderableDisplay[] displays = UiUtil
                .getDisplaysFromContainer(EditorUtil.getActiveVizContainer());
        List<AbstractRenderableDisplay> absdisplays = new ArrayList<AbstractRenderableDisplay>();
        for (IRenderableDisplay display : displays) {
            if ((display instanceof AbstractRenderableDisplay)) {
                absdisplays.add((AbstractRenderableDisplay) display);
            }
        }

        Bundle bundle = new Bundle();
        bundle.setDisplays(absdisplays
                .toArray(new AbstractRenderableDisplay[absdisplays.size()]));
        bundle.setLoopProperties(EditorUtil.getActiveVizContainer()
                .getLoopProperties());
        return bundle;
    }

}
