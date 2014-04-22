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
package com.raytheon.uf.viz.localization.perspective.view.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IWorkbenchPage;

import com.raytheon.uf.viz.localization.filetreeview.LocalizationFileEntryData;
import com.raytheon.uf.viz.localization.perspective.editor.LocalizationEditorInput;
import com.raytheon.uf.viz.localization.perspective.editor.LocalizationEditorUtils;

/**
 * Action to open a localization file in the default editor (based on file
 * extension)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 3, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class OpenAction extends Action {

    private IWorkbenchPage page;

    private LocalizationFileEntryData[] files;

    private IEditorDescriptor descriptor;

    public OpenAction(IWorkbenchPage page, LocalizationFileEntryData[] files) {
        super("Open", Action.AS_PUSH_BUTTON);
        this.page = page;
        this.files = files;
    }

    public OpenAction(IWorkbenchPage page, LocalizationFileEntryData[] files,
            IEditorDescriptor descriptor) {
        super(descriptor.getLabel(), Action.AS_RADIO_BUTTON);
        this.page = page;
        this.files = files;
        this.descriptor = descriptor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        if (descriptor == null) {
            for (LocalizationFileEntryData file : files) {
                LocalizationEditorUtils.openInEditor(
                        page,
                        new LocalizationEditorInput(file.getFile(), file
                                .getResource()));
            }
        } else {
            for (LocalizationFileEntryData file : files) {
                LocalizationEditorUtils.openInEditor(
                        page,
                        new LocalizationEditorInput(file.getFile(), file
                                .getResource()), descriptor.getId());
                LocalizationEditorUtils.getEditorRegistry().setDefaultEditor(
                        file.getName(), descriptor.getId());
            }
        }
    }
}
