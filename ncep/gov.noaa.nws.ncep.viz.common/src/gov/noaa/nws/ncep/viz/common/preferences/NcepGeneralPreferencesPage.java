package gov.noaa.nws.ncep.viz.common.preferences;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * Specifies Data Area preferences page
 * 
 * <pre>
 * 
 *     SOFTWARE HISTORY
 *    
 *     Date       	Ticket#		Engineer	Description
 *     ------------	----------	-----------	--------------------------
 *     July 31, 2012  #631        ghull    	Initial Creation.
 *     Sept 11, 2012  #860        ghull     added ShowLatestResourceTimes
 * 
 * </pre>
 * 
 * @author mli
 * 
 */
public class NcepGeneralPreferencesPage extends FieldEditorPreferencePage implements
		IWorkbenchPreferencePage {

	public final static String PromptOnDisplayClose = "PromptOnDisplayClose";
	public final static String ShowLatestResourceTimes = "ShowLatestResourceTimes";
	
	private BooleanFieldEditor promptOnDisplayCloseFieldEditor;
	
	private BooleanFieldEditor showLatestResourceTimesFieldEditor;
	
   	public NcepGeneralPreferencesPage() {
		super(GRID);
		setPreferenceStore( NmapCommon.getNcepPreferenceStore() );
	}

   	
    @Override
    public void createFieldEditors() {
    	
    	Composite composite = getFieldEditorParent();
    	
    	promptOnDisplayCloseFieldEditor = new BooleanFieldEditor( PromptOnDisplayClose, 
    			"Prompt Before Closing Displays", BooleanFieldEditor.SEPARATE_LABEL, // put the check box on the right 
    			composite );
        this.addField(promptOnDisplayCloseFieldEditor);
        
    	showLatestResourceTimesFieldEditor = new BooleanFieldEditor( ShowLatestResourceTimes, 
    			"Show Latest Resource Time", BooleanFieldEditor.SEPARATE_LABEL, // put the check box on the right 
    			composite );
        this.addField(showLatestResourceTimesFieldEditor);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    public void init(IWorkbench workbench) {
    	
    }
    
//    @Override
//    public void propertyChange(PropertyChangeEvent event) {
//    }	
}
