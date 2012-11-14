package gov.noaa.nws.ncep.viz.common.preferences;

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
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

//import com.raytheon.uf.viz.core.Activator;

public class NcepPreferences extends FieldEditorPreferencePage implements
IWorkbenchPreferencePage {
	public NcepPreferences() {
        super(GRID);
        setPreferenceStore( NmapCommon.getNcepPreferenceStore() );
        setDescription("Preferences for the National Centers Perspective");
    }

    @Override
    protected void createFieldEditors() {

    }

    @Override
    public void init(IWorkbench workbench) {
    }
}
