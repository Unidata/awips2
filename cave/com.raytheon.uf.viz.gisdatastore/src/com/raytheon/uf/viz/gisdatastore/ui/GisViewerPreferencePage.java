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
package com.raytheon.uf.viz.gisdatastore.ui;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.raytheon.uf.viz.gisdatastore.Activator;
import com.raytheon.uf.viz.gisdatastore.rsc.DataStoreResource;

/**
 * Preference page for GIS Viewer
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 26, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GisViewerPreferencePage extends FieldEditorPreferencePage
        implements IWorkbenchPreferencePage {

    public GisViewerPreferencePage() {
        super(FieldEditorPreferencePage.GRID);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
    public void init(IWorkbench workbench) {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        setPreferenceStore(store);
        setDescription("GIS Viewer Preferences");
    }

    @Override
    protected void createFieldEditors() {
        AbstractFieldEditor colorEditor = new ColorFieldEditor(
                DataStoreResource.HIGHLIGHT_COLOR_KEY, "Highlight Color",
                getFieldEditorParent());

        AbstractFieldEditor styleEditor = new LineStyleFieldEditor(
                DataStoreResource.HIGHLIGHT_STYLE_KEY, "Highlight Style",
                getFieldEditorParent());

        AbstractFieldEditor widthEditor = new LineWidthFieldEditor(
                DataStoreResource.HIGHLIGHT_WIDTH_KEY, "Highlight Width",
                getFieldEditorParent());

        AbstractFieldEditor opacityEditor = new OpacityFieldEditor(
                DataStoreResource.PRODUCT_OPACITY_KEY, "Product Opacity",
                getFieldEditorParent());

        addField(colorEditor);
        addField(styleEditor);
        addField(widthEditor);
        addField(opacityEditor);
    }
}
