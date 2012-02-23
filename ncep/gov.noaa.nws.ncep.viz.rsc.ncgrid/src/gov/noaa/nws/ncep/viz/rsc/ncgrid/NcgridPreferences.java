package gov.noaa.nws.ncep.viz.rsc.ncgrid;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * Specifies Clipping area preferences page
 * 
 * <pre>
 * 
 *     SOFTWARE HISTORY
 *    
 *     Date       	Ticket#		Engineer	Description
 *     ------------	----------	-----------	--------------------------
 *     Aug. 2011             	M. Li    	Initial Creation.
 * 
 * </pre>
 * 
 * @author mli
 * 
 */
public class NcgridPreferences extends FieldEditorPreferencePage implements
		IWorkbenchPreferencePage {

	public final static String LLLAT = "LLLAT";
	public final static String LLLON = "LLLON";
	public final static String URLAT = "URLAT";
	public final static String URLON = "URLON";

	private StringFieldEditor llLatText;
	private StringFieldEditor llLonText;
	private StringFieldEditor urLatText;
	private StringFieldEditor urLonText;
	
   	public NcgridPreferences() {
		super(GRID);
		setPreferenceStore(Activator.getDefault().getPreferenceStore());
//		setDescription("Select Clipping Area preference");
	}

   	
    @Override
    public void createFieldEditors() {
    	
    	Composite composite = getFieldEditorParent();
    	
        llLatText = new StringFieldEditor(LLLAT, "&Lower Left Latitude: ",
        		composite);
        this.addField(llLatText);
        llLonText = new StringFieldEditor(LLLON, "&Lower Left Longitude: ",
        		composite);
        this.addField(llLonText);
        urLatText = new StringFieldEditor(URLAT, "&Upper Right Latitude: ",
        		composite);
        this.addField(urLatText);
        urLonText = new StringFieldEditor(URLON, "&Upper Right Longitude: ",
        		composite);
        this.addField(urLonText);
        
        
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    public void init(IWorkbench workbench) {
    	
    }
    
    /*
    @Override
    public void propertyChange(PropertyChangeEvent event) {

    	garea.setEnabled(customArea.getBooleanValue(), getFieldEditorParent());
    	proj.setEnabled(customArea.getBooleanValue(), getFieldEditorParent());
//    	customArea.setEnabled(false, getFieldEditorParent());
//    	customArea.setEnabled(true, getFieldEditorParent());
    	
    }	
*/
    
 
}
