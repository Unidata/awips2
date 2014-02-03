package gov.noaa.nws.ncep.viz.rsc.solarimage.rsc;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsInteractiveDialog;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarFromColorMapAttrsEditorComposite;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

/**
 * An interface to edit Solar resource attributes.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/06/10      #           Greg Hull    Initial Creation.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class EditSolarImageAttrsDialog extends
        AbstractEditResourceAttrsInteractiveDialog {

    public EditSolarImageAttrsDialog(Shell parentShell,
            INatlCntrsResourceData r, Boolean apply) {

        super(parentShell, r, apply);
        resourceData = r;
        // TODO Auto-generated constructor stub
    }

    private INatlCntrsResourceData resourceData;

    private ColorBarFromColorMapAttrsEditorComposite cBarComposite = null;

    //
    @Override
    public Composite createDialog(Composite topComp) {

        FormLayout layout0 = new FormLayout();
        topComp.setLayout(layout0);

        cBarComposite = new ColorBarFromColorMapAttrsEditorComposite(topComp,
                SWT.NONE, resourceData);

        return topComp;
    }

    @Override
    public void initWidgets() {
        // done in createDialog
    }

    @Override
    protected void dispose() {
        super.dispose();
        if (cBarComposite != null)
            cBarComposite.dispose();
    }
}