package gov.noaa.nws.ncep.viz.common;

/**
 * NcepPreferences
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * 5/25/2011    444        Q. Zhou     Add Ncep layer between Preference and Pgen
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.raytheon.uf.viz.core.Activator;

public class NcepPreferences extends FieldEditorPreferencePage implements
IWorkbenchPreferencePage {
	public NcepPreferences() {
        super(GRID);
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
        setDescription("Preference location for National Centers Perspective");
    }

    @Override
    protected void createFieldEditors() {

    }

    @Override
    public void init(IWorkbench workbench) {
    }
}
