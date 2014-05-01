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
import java.io.FileWriter;
import java.io.IOException;
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
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.XMLMemento;
import org.eclipse.ui.internal.WorkbenchPage;

import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.Procedure;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.UiUtil.ContainerPart;
import com.raytheon.viz.ui.UiUtil.ContainerPart.Container;

/**
 * SaveProcedure
 * 
 * Save a procedure that currently maps the current state of the screen
 * including the editor and any side views.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Sep 11, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class SaveProcedure extends AbstractHandler {

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
            if (name != null && name.endsWith(".xml") == false) {
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

        FileWriter fw = null;
        try {
            Procedure procedure = getCurrentProcedure();
            String xml = procedure.toXML();
            fw = new FileWriter(fileName);
            fw.write(xml);
        } catch (Exception e) {
            final String errMsg = "Error occurred during procedure save.";
            Status status = new Status(Status.ERROR, UiPlugin.PLUGIN_ID, 0,
                    errMsg, e);
            ErrorDialog.openError(Display.getCurrent().getActiveShell(),
                    "ERROR", errMsg, status);
            throw new ExecutionException(errMsg, e);
        } finally {
            if (fw != null) {
                try {
                    fw.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        }

        return null;
    }

    public static Procedure getCurrentProcedure() {
        Procedure procedure = new Procedure();

        IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        String perspectiveId = page.getPerspective().getId();

        procedure.setPerspective(perspectiveId);
        IMemento layout = XMLMemento.createWriteRoot("perspectiveLayout");
        ((WorkbenchPage) page).getEditorPresentation().saveState(layout);
        procedure.setLayout(layout);

        List<Bundle> bundleList = new ArrayList<Bundle>();

        List<ContainerPart> panes = UiUtil.getActiveDisplayMap();
        for (ContainerPart part : panes) {
            for (Container c : part.containers) {
                IRenderableDisplay[] displayArr = c.displays;
                Bundle b = new Bundle();
                if (displayArr.length > 0) {
                    b.setLoopProperties(displayArr[0].getContainer()
                            .getLoopProperties());
                }
                String key = part.id;
                b.setLayoutId(c.layoutId);
                if (UiUtil.isEditor(key)) {
                    b.setEditor(key);
                } else if (UiUtil.isView(key)) {
                    b.setView(key);
                }

                List<AbstractRenderableDisplay> displays = new ArrayList<AbstractRenderableDisplay>();
                for (IRenderableDisplay disp : displayArr) {
                    if (disp instanceof AbstractRenderableDisplay) {
                        displays.add((AbstractRenderableDisplay) disp);
                    }
                }

                if (displays.size() > 0) {
                    b.setDisplays(displays
                            .toArray(new AbstractRenderableDisplay[displays
                                    .size()]));
                    bundleList.add(b);
                }
            }
        }

        if (bundleList.size() > 0) {
            procedure.setBundles(bundleList.toArray(new Bundle[bundleList
                    .size()]));
        }
        return procedure;
    }
}
