package gov.noaa.nws.ncep.viz.rsc.solarimage.util;

/**
 * Represents the image data of a SolarImageRecord object.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/22/2012   958        qzhou       Initial creation.
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */

import gov.noaa.nws.ncep.viz.common.Activator;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public class SolarImagePreferences extends FieldEditorPreferencePage implements
	IWorkbenchPreferencePage{

	public final static String NUM_FRAMES = "NUM_FRAMES";
	
	public final static int DEFAULT_NUM_FRAMES = 27;

	public SolarImagePreferences() {
		super(GRID);
		setPreferenceStore(Activator.getDefault().getPreferenceStore());
		setDescription("Define the maximum number of frames to select");
	}
	
	@Override
	public void init(IWorkbench workbench) {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected void createFieldEditors() {

		FieldEditor editor = new IntegerFieldEditor( NUM_FRAMES,
				"&Maximum Number of Frames:", getFieldEditorParent(), 3) ;
				
		this.addField(editor );    
		
		if (getPreferenceStore().getInt(NUM_FRAMES) == 0)
			getPreferenceStore().setDefault(NUM_FRAMES, DEFAULT_NUM_FRAMES);
	}
	
}
