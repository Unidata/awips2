package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common;

import gov.noaa.gsd.viz.ensemble.Activator;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public class EnsembleToolPreferencesPage extends FieldEditorPreferencePage
        implements IWorkbenchPreferencePage {

    @Override
    public void init(IWorkbench workbench) {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        setPreferenceStore(store);
        setDescription("Ensemble Tool Preferences");
    }

    @Override
    protected void createFieldEditors() {

    }

}
