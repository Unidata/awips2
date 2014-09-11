package gov.noaa.nws.ncep.viz.rsc.timeseries.rsc;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsInteractiveDialog;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

/**
 * An interface to edit TimeSeries resource attributes.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 06/18/2014   #1136       qzhou        Initial Creation.
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */

public class EditTimeSeriesAttrsDialog extends
        AbstractEditResourceAttrsInteractiveDialog {

    public EditTimeSeriesAttrsDialog(Shell parentShell,
            INatlCntrsResourceData r, Boolean apply) {

        super(parentShell, r, apply);
        resourceData = r;

    }

    private INatlCntrsResourceData resourceData;

    @Override
    public Composite createDialog(Composite topComp) {

        FormLayout layout0 = new FormLayout();
        topComp.setLayout(layout0);

        Composite top = topComp;

        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        top.setLayout(mainLayout);

        Group colorsGroup = new Group(top, SWT.SHADOW_NONE);
        colorsGroup.setText("Colors");
        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        colorsGroup.setLayoutData(gd);

        initializeComponents(colorsGroup);

        return topComp;
    }

    public void initializeComponents(final Group colorsGroup) {

        colorsGroup.setLayout(new GridLayout(2, true));

    }

    @Override
    public void initWidgets() {
        // done in createDialog
    }

    @Override
    protected void dispose() {
        super.dispose();

    }
}