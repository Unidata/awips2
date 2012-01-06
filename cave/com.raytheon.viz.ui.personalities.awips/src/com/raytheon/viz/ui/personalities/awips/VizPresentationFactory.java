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
package com.raytheon.viz.ui.personalities.awips;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbenchPreferenceConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.presentations.defaultpresentation.DefaultMultiTabListener;
import org.eclipse.ui.internal.presentations.defaultpresentation.DefaultSimpleTabListener;
import org.eclipse.ui.internal.presentations.defaultpresentation.DefaultThemeListener;
import org.eclipse.ui.internal.presentations.util.PresentablePartFolder;
import org.eclipse.ui.internal.presentations.util.StandardViewSystemMenu;
import org.eclipse.ui.internal.presentations.util.TabbedStackPresentation;
import org.eclipse.ui.presentations.IStackPresentationSite;
import org.eclipse.ui.presentations.StackPresentation;
import org.eclipse.ui.presentations.WorkbenchPresentationFactory;

/**
 * Presentation Factory for the system
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 15, 2008             chammack    Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class VizPresentationFactory extends WorkbenchPresentationFactory {

    private static int editorTabPosition = PlatformUI.getPreferenceStore()
            .getInt(IWorkbenchPreferenceConstants.EDITOR_TAB_POSITION);

    private static int viewTabPosition = PlatformUI.getPreferenceStore()
            .getInt(IWorkbenchPreferenceConstants.VIEW_TAB_POSITION);

    public VizPresentationFactory() {
        super();
    }

    @Override
    public String getId() {
        // TODO Auto-generated method stub
        return super.getId();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.presentations.AbstractPresentationFactory
     */
    @SuppressWarnings("restriction")
    @Override
    public StackPresentation createEditorPresentation(Composite parent,
            IStackPresentationSite site) {
        VizTabFolder folder = new VizTabFolder(parent, editorTabPosition
                | SWT.BORDER,
                site.supportsState(IStackPresentationSite.STATE_MINIMIZED),
                site.supportsState(IStackPresentationSite.STATE_MAXIMIZED));

        folder.setSimpleTabs(false);

        /*
         * Set the minimum characters to display, if the preference is something
         * other than the default. This is mainly intended for RCP applications
         * or for expert users (i.e., via the plug-in customization file).
         * 
         * Bug 32789.
         */
        final IPreferenceStore store = PlatformUI.getPreferenceStore();
        if (store
                .contains(IWorkbenchPreferenceConstants.EDITOR_MINIMUM_CHARACTERS)) {
            final int minimumCharacters = store
                    .getInt(IWorkbenchPreferenceConstants.EDITOR_MINIMUM_CHARACTERS);
            if (minimumCharacters >= 0) {
                folder.setMinimumCharacters(minimumCharacters);
            }
        }

        PresentablePartFolder partFolder = new PresentablePartFolder(folder);

        TabbedStackPresentation result = new TabbedStackPresentation(site,
                partFolder, new VizEditorSystemMenu(site));

        DefaultThemeListener themeListener = new DefaultThemeListener(folder,
                result.getTheme());
        result.getTheme().addListener(themeListener);

        new DefaultMultiTabListener(result.getApiPreferences(),
                IWorkbenchPreferenceConstants.SHOW_MULTIPLE_EDITOR_TABS, folder);

        new DefaultSimpleTabListener(result.getApiPreferences(),
                IWorkbenchPreferenceConstants.SHOW_TRADITIONAL_STYLE_TABS,
                folder);

        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.ui.presentations.AbstractPresentationFactory#
     * createViewPresentation(org.eclipse.swt.widgets.Composite,
     * org.eclipse.ui.presentations.IStackPresentationSite)
     */
    @SuppressWarnings("restriction")
    @Override
    public StackPresentation createViewPresentation(Composite parent,
            IStackPresentationSite site) {
        VizTabFolder folder = new VizTabFolder(parent, viewTabPosition
                | SWT.BORDER,
                site.supportsState(IStackPresentationSite.STATE_MINIMIZED),
                site.supportsState(IStackPresentationSite.STATE_MAXIMIZED));

        final IPreferenceStore store = PlatformUI.getPreferenceStore();
        final int minimumCharacters = store
                .getInt(IWorkbenchPreferenceConstants.VIEW_MINIMUM_CHARACTERS);
        if (minimumCharacters >= 0) {
            folder.setMinimumCharacters(minimumCharacters);
        }

        PresentablePartFolder partFolder = new PresentablePartFolder(folder);

        folder.setUnselectedCloseVisible(false);
        folder.setUnselectedImageVisible(true);

        TabbedStackPresentation result = new TabbedStackPresentation(site,
                partFolder, new StandardViewSystemMenu(site));

        DefaultThemeListener themeListener = new DefaultThemeListener(folder,
                result.getTheme());
        result.getTheme().addListener(themeListener);

        new DefaultSimpleTabListener(result.getApiPreferences(),
                IWorkbenchPreferenceConstants.SHOW_TRADITIONAL_STYLE_TABS,
                folder);

        return result;
    }

}
