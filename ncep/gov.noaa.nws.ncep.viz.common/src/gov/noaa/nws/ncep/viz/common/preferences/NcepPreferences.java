package gov.noaa.nws.ncep.viz.common.preferences;

/**
 * NcepPreferences
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * 5/25/2011    444        Q. Zhou     Add Ncep layer between Preference and Pgen
 * 11/13/2013   1051       G. Hull     Add deskName
 * 
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.viz.core.localization.LocalizationConstants;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.localization.ReadOnlyComboFieldEditor;

//import com.raytheon.uf.viz.core.Activator;

public class NcepPreferences extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
	
	public final static String DeskNamePref = "deskName";
    private boolean restartRequired = false;

	public NcepPreferences() {
        super(GRID);
        setPreferenceStore( NmapCommon.getNcepPreferenceStore() );
        String currDesk = NcPathManager.getInstance().getDeskContext().getContextName();

        setDescription("NCP Preferences.\nCurrent Desk is "+currDesk);
    }

    @Override
    protected void createFieldEditors() {
    	
//        String currDesk = NcPathManager.getInstance().getDeskContext().getContextName();
//
//        String[] deskList = NcPathManager.getInstance().
//        				getContextList( NcPathManager.getInstance().getDeskLevel() );
//        Set<String> desksSet = new HashSet<String>();           
//        desksSet.addAll(Arrays.asList( deskList ));
//        
//        if( !desksSet.contains( currDesk ) ) {
//        	desksSet.add( currDesk );
//        }
//        
//        deskList = desksSet.toArray(new String[desksSet.size()]);
//        Arrays.sort(deskList);
//                 
//        String[][] entryNamesAndValues = new String[deskList.length][2];
//
//        for (int i = 0; i < deskList.length; i++) {
//            entryNamesAndValues[i][0] = deskList[i];
//            entryNamesAndValues[i][1] = deskList[i];
//        }
//
//// This is a read only combo but since we currently don't have a complete list of 
//// 'available' desks we need to have an editable combo.        
////        FieldEditor fldEd = new ComboFieldEditor( DeskNamePref, "&Desk: ",
////                    entryNamesAndValues, getFieldEditorParent());        
////        fldEd.setLabelText( "Desk:" );        
//        
//        // just use a normal StringFieldEditor to let them type in the desk name
//        FieldEditor fldEd = new StringFieldEditor( DeskNamePref, "Desk: ", 20,
//        		getFieldEditorParent() );
//
//      this.addField( fldEd );
      
      restartRequired = false;
    }

    @Override
    public void init(IWorkbench workbench) {
//    	System.out.println("init ncep prefs called");
    }
    
    @Override
    public void propertyChange(PropertyChangeEvent event) {
    	// if not a property that requires a restart
//    	if( !event.getProperty().equals( DeskNamePref ) ) {
//    		return;
//    	}
//        if( !event.getNewValue().equals(event.getOldValue()) ) {
//            restartRequired = true;
//        }
//        super.propertyChange(event);
    }

    @Override
    public boolean performOk() {
//        if( restartRequired ) {
//            MessageBox warning = new MessageBox(getShell(), SWT.ICON_WARNING
//                    | SWT.OK | SWT.CANCEL);
//            warning.setText("Desk Changed");
//            warning.setMessage("The Desk Name has been changed. "
//                    + "Click OK to Save your changes. Cave MUST be restart for "
//            		+ "this change to take effect." );
//
//            int retVal = warning.open();
//            if (retVal == SWT.CANCEL) {
//                return false;
//            }
//        }
//
        return super.performOk();
    }
}
