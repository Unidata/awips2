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

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorMatchingStrategy;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbenchPage;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.localization.adapter.LocalizationPerspectiveAdapter;
import com.raytheon.uf.viz.localization.filetreeview.LocalizationFileEntryData;
import com.raytheon.uf.viz.localization.perspective.editor.LocalizationEditorInput;
import com.raytheon.uf.viz.localization.perspective.editor.LocalizationEditorUtils;

/**
 * Action to open a localization file in a specified editor, lists available
 * editors in sub menu
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

public class OpenWithAction extends Action implements IMenuCreator {

    private Menu menu;

    private IWorkbenchPage page;

    private LocalizationFileEntryData file;

    private LocalizationPerspectiveAdapter adapter;

    public OpenWithAction(IWorkbenchPage page, LocalizationFileEntryData file,
            IFile rsc, LocalizationPerspectiveAdapter adapter) {
        super("Open With", Action.AS_DROP_DOWN_MENU);
        this.page = page;
        this.file = file;
        this.adapter = adapter;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.IMenuCreator#dispose()
     */
    @Override
    public void dispose() {
        if (menu != null) {
            menu.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
     * .Menu)
     */
    @Override
    public Menu getMenu(Menu parent) {
        if (menu != null) {
            menu.dispose();
        }

        menu = new Menu(parent);

        fillMenu(menu);
        return menu;
    }

    @Override
    public Menu getMenu(Control parent) {
        if (menu != null) {
            menu.dispose();
        }

        menu = new Menu(parent);

        fillMenu(menu);

        return menu;
    }

    /**
     * 
     */
    private void fillMenu(Menu menu) {
        LocalizationFile file = this.file.getFile();
        LocalizationEditorInput input = new LocalizationEditorInput(file,
                this.file.getResource());
        IEditorDescriptor[] adapterDescriptors = adapter.getLoadableEditors(
                LocalizationEditorUtils.getEditorRegistry(), file);
        IEditorDescriptor[] descriptors = LocalizationEditorUtils
                .getEditorsForInput(input);

        IEditorDescriptor defaultEditor = LocalizationEditorUtils
                .getDefaultEditorForInput(input);
        IEditorDescriptor systemEditor = new SystemEditorDescriptor(
                "System Editor");
        if (defaultEditor == null) {
            defaultEditor = new SystemEditorDescriptor("Default Editor");
        }
        if (descriptors.length == 0) {
            descriptors = new IEditorDescriptor[] { LocalizationEditorUtils
                    .getTextEditorDescriptor() };
        }

        boolean selected = false;
        for (IEditorDescriptor descriptor : descriptors) {
            OpenAction action = new OpenAction(page,
                    new LocalizationFileEntryData[] { this.file }, descriptor);
            if (!selected) {
                action.setChecked(true);
                selected = true;
            }
            ActionContributionItem aci = new ActionContributionItem(action);
            aci.fill(menu, -1);
        }

        for (IEditorDescriptor descriptor : adapterDescriptors) {
            OpenAction action = new OpenAction(page,
                    new LocalizationFileEntryData[] { this.file }, descriptor);
            ActionContributionItem aci = new ActionContributionItem(action);
            aci.fill(menu, -1);
        }

        new Separator().fill(menu, -1);

        new ActionContributionItem(new OpenAction(page,
                new LocalizationFileEntryData[] { this.file }, systemEditor))
                .fill(menu, -1);

        OpenAction action = new OpenAction(page,
                new LocalizationFileEntryData[] { this.file }, defaultEditor);
        if (!selected) {
            action.setChecked(true);
        }
        ActionContributionItem aci = new ActionContributionItem(action);
        aci.fill(menu, -1);
    }

    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }

    private static class SystemEditorDescriptor implements IEditorDescriptor {

        String label;

        private SystemEditorDescriptor(String label) {
            this.label = label;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.ui.IEditorDescriptor#getId()
         */
        @Override
        public String getId() {
            return IEditorRegistry.SYSTEM_EXTERNAL_EDITOR_ID;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.ui.IEditorDescriptor#getImageDescriptor()
         */
        @Override
        public ImageDescriptor getImageDescriptor() {
            return null;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.ui.IEditorDescriptor#getLabel()
         */
        @Override
        public String getLabel() {
            return label;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.ui.IEditorDescriptor#isInternal()
         */
        @Override
        public boolean isInternal() {
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.ui.IEditorDescriptor#isOpenInPlace()
         */
        @Override
        public boolean isOpenInPlace() {
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.ui.IEditorDescriptor#isOpenExternal()
         */
        @Override
        public boolean isOpenExternal() {
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.ui.IEditorDescriptor#getEditorMatchingStrategy()
         */
        @Override
        public IEditorMatchingStrategy getEditorMatchingStrategy() {
            return null;
        }

    }
}
