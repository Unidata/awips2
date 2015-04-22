package gov.noaa.nws.ncep.viz.rsc.satellite.rsc;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsInteractiveDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarFromColorMapAttrsEditorComposite;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarEditor;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;
import gov.noaa.nws.ncep.viz.common.ColorMapUtil;

/**
 *  An interface to edit Mcidas resource attributes. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/24/10      #259        Greg Hull    Initial Creation.
 * 04/27/2010    #245        Greg Hull    Added Apply Button * 
 * 03/29/2012    #651        S. Gurung    Extend AbstractEditResourceAttrsInteractiveDialog
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class EditSatelliteAttrsDialog extends AbstractEditResourceAttrsInteractiveDialog { 
   
    public EditSatelliteAttrsDialog(Shell parentShell, INatlCntrsResourceData r, Boolean apply) {
    	
		super(parentShell, r, apply);
		resourceData = r;
		// TODO Auto-generated constructor stub
	}

    private INatlCntrsResourceData resourceData;
    private ColorBarFromColorMapAttrsEditorComposite cBarComposite= null;
    // 
	@Override
	public Composite createDialog( Composite topComp ) {


        FormLayout layout0 = new FormLayout();
        topComp.setLayout(layout0);

          cBarComposite= new ColorBarFromColorMapAttrsEditorComposite(topComp, SWT.NONE,resourceData);
          
        return topComp;
    }

	
	@Override
	public void initWidgets() {
		// done in createDialog		
	}    
	
	@Override 
	protected void dispose() {
		super.dispose();
//		colorBarEditor.dispose();
		if ( cBarComposite != null)
		cBarComposite.dispose();
	}
}