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
package com.raytheon.uf.viz.productbrowser;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.ListEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference.PreferenceType;

/**
 * Builds each data type's preference specifically for product browser
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 16, 2011            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class DataTypePreferencePage extends FieldEditorPreferencePage implements
        IWorkbenchPreferencePage {

    private static IExtension[] extensions;

    private List<AbstractProductBrowserDataDefinition<?>> prods = null;

    private List<FieldEditor> editors = null;

    private static boolean saved = false;

    public DataTypePreferencePage() {
        super(GRID);
        prods = new ArrayList<AbstractProductBrowserDataDefinition<?>>();
        editors = new ArrayList<FieldEditor>();
        setDescription("Specify Product Browser Settings...");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
    public void init(IWorkbench workbench) {
        saved = false;
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
        setDescription("Data Type Preferences");
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry
                .getExtensionPoint(ProductBrowserUtils.DATA_DEFINITION_ID);
        if (point != null) {
            extensions = point.getExtensions();
        } else {
            extensions = new IExtension[0];
        }

        for (IExtension ext : extensions) {
            IConfigurationElement[] config = ext.getConfigurationElements();
            for (IConfigurationElement element : config) {
                try {
                    AbstractProductBrowserDataDefinition<?> prod = (AbstractProductBrowserDataDefinition<?>) element
                            .createExecutableExtension("class");
                    prods.add(prod);
                } catch (CoreException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditorPreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        boolean tf = super.performOk();
        for (IViewReference reference : PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage().getViewReferences()) {
            if (!saved) {
                if (ProductBrowserView.ID.equals(reference.getId())) {
                    ((ProductBrowserView) reference.getPart(true))
                            .populateInitialProductTree();
                    saved = true;
                }
            }
        }
        return tf;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performApply()
     */
    @Override
    protected void performApply() {
        for (FieldEditor editor : editors) {
            editor.store();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.FieldEditorPreferencePage#createFieldEditors
     * ()
     */
    @Override
    protected void createFieldEditors() {
        String[][] fields = new String[prods.size()][2];
        for (int i = 0; i < fields.length; i++) {
            for (int j = 0; j < fields[i].length; j++) {
                fields[i][j] = prods.get(i).displayName;
            }
        }

        getPreferences(getProduct(getTitle()));
        for (FieldEditor editor : editors) {
            this.addField(editor);
        }
    }

    private AbstractProductBrowserDataDefinition<?> getProduct(String name) {
        for (AbstractProductBrowserDataDefinition<?> prod : prods) {
            if (name.equals(prod.displayName)) {
                return prod;
            }
        }
        return null;
    }

    /**
     * Per each type of product, goes in and decides what preferences it needs
     * to display
     * 
     * @param prod
     */
    private void getPreferences(
            final AbstractProductBrowserDataDefinition<?> prod) {
        List<ProductBrowserPreference> prefs = prod.getPreferences();
        for (ProductBrowserPreference pref : prefs) {
            if (pref.getPreferenceType() == PreferenceType.EDITABLE_STRING) {
                StringFieldEditor editor = new StringFieldEditor(
                        pref.getLabel() + prod.displayName, pref.getLabel(),
                        getFieldEditorParent());
                editors.add(editor);
            } else if (pref.getPreferenceType() == PreferenceType.BOOLEAN) {
                BooleanFieldEditor editor = new BooleanFieldEditor(
                        pref.getLabel() + prod.displayName, pref.getLabel(),
                        getFieldEditorParent());
                editors.add(editor);
            } else if (pref.getPreferenceType() == PreferenceType.STRING_ARRAY) {
                final String[] values = ((String[]) pref.getValue());
                ListEditor editor = new ListEditor(pref.getLabel()
                        + prod.displayName, pref.getLabel(),
                        getFieldEditorParent()) {

                    @Override
                    protected String[] parseString(String stringList) {
                        return new String[0];
                    }

                    @Override
                    protected String getNewInputObject() {
                        return null;
                    }

                    @Override
                    protected String createList(String[] items) {
                        String temp = "";
                        for (int i = 0; i < items.length; i++) {
                            if (temp.isEmpty()) {
                                temp += items[i];
                            } else {
                                temp += "," + items[i];
                            }
                        }
                        return temp;
                    }

                    @Override
                    protected void doFillIntoGrid(Composite parent,
                            int numColumns) {
                        super.doFillIntoGrid(parent, numColumns);
                        for (int i = 0; i < values.length; i++) {
                            getList().add(values[i]);
                        }
                    }
                };
                editor.getButtonBoxControl(getFieldEditorParent())
                        .getChildren()[0].setVisible(false);
                editor.getButtonBoxControl(getFieldEditorParent())
                        .getChildren()[1].setVisible(false);
                editor.getButtonBoxControl(getFieldEditorParent()).layout();
                editors.add(editor);
            }
        }
    }
}