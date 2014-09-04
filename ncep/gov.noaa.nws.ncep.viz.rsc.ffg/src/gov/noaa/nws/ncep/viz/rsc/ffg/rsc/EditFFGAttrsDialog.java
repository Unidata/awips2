package gov.noaa.nws.ncep.viz.rsc.ffg.rsc;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarEditor;
import gov.noaa.nws.ncep.viz.ui.display.ColorBar;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * An interface to edit FFG resource attributes.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 29 May 2009   #115        Greg Hull     Initial Creation.
 * 30 Nov 2009               Greg Hull     migrate to to11d6
 * 21 Mar 2010   #259        Greg Hull     add colorbar editor
 * 27 Apr 2010   #245        Greg Hull     Added Apply Button
 * 01 Jul 2014 TTR 1018      Steve Russell Updated call to ColorBarEditor
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class EditFFGAttrsDialog extends AbstractEditResourceAttrsDialog {

    public EditFFGAttrsDialog(Shell parentShell, INatlCntrsResourceData r, Boolean apply) {
        super(parentShell, r, apply);
        // TODO Auto-generated constructor stub
    }

    private RscAttrValue dispValsAttr = null;

    private RscAttrValue colorBarAttr = null;

    private Label symbolLbl = null;

    private Combo symbolCombo = null;

    private ColorBarEditor colorBarEditor = null;

    // 
    @Override
    public Composite createDialog(Composite topComp) {

        dispValsAttr = editedRscAttrSet.getRscAttr("displayValues");
        colorBarAttr = editedRscAttrSet.getRscAttr("colorBar");

        if (dispValsAttr == null || dispValsAttr.getAttrClass() != Boolean.class) {
            System.out.println("displayValues is null or not of expected class Boolean?");
            return null;
        }
        if (colorBarAttr == null || colorBarAttr.getAttrClass() != ColorBar.class) {
            System.out.println("colorBar is null or not of expected class ColorBar?");
            return null;
        }

        FormLayout layout0 = new FormLayout();
        topComp.setLayout(layout0);

        final Combo showAsCombo = new Combo(topComp, SWT.READ_ONLY | SWT.DROP_DOWN);
        FormData fd = new FormData();
        fd.left = new FormAttachment(20, 0);
        fd.top = new FormAttachment(0, 20);
        showAsCombo.setLayoutData(fd);

        showAsCombo.setItems(new String[] { "Values", "Symbols" });

        showAsCombo.select(((Boolean) dispValsAttr.getAttrValue()).booleanValue() ? 0 : 1);

        showAsCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                boolean dispVals = (showAsCombo.getSelectionIndex() == 0 ? true : false);
                dispValsAttr.setAttrValue(new Boolean(dispVals));

                dispVals = false; // symobl selection not implemented
                symbolCombo.setEnabled(dispVals);
                symbolLbl.setEnabled(dispVals);
            }
        });

        Label showAsLbl = new Label(topComp, SWT.None);
        fd = new FormData();
        fd.right = new FormAttachment(showAsCombo, -5, SWT.LEFT);
        fd.top = new FormAttachment(showAsCombo, 2, SWT.TOP);
        showAsLbl.setLayoutData(fd);
        showAsLbl.setText("Show As");

        symbolCombo = new Combo(topComp, SWT.READ_ONLY | SWT.DROP_DOWN);
        fd = new FormData();
        fd.left = new FormAttachment(50, 0);
        fd.top = new FormAttachment(showAsCombo, 0, SWT.TOP);
        symbolCombo.setLayoutData(fd);

        // Symbol Selection is not Implemented
        symbolCombo.add("N/A");
        symbolCombo.select(0);
        symbolCombo.setEnabled(false);

        symbolLbl = new Label(topComp, SWT.None);
        fd = new FormData();
        fd.right = new FormAttachment(symbolCombo, -5, SWT.LEFT);
        fd.top = new FormAttachment(symbolCombo, 2, SWT.TOP);
        symbolLbl.setLayoutData(fd);
        symbolLbl.setText("Symbol");

        symbolLbl.setEnabled(false);

        Group colorBarGrp = new Group(topComp, SWT.NONE);
        colorBarGrp.setText("Edit Color Bar");
        fd = new FormData();//400,300);        
        fd.left = new FormAttachment(0, 15);
        fd.right = new FormAttachment(100, -15);
        fd.top = new FormAttachment(showAsCombo, 15, SWT.BOTTOM);
        fd.bottom = new FormAttachment(100, -20);
        colorBarGrp.setLayoutData(fd);

        colorBarGrp.setLayout(new FormLayout());

        ColorBar editedColorBar = null;

        editedColorBar = (ColorBar) colorBarAttr.getAttrValue();

        colorBarEditor = new ColorBarEditor(colorBarGrp, editedColorBar, true);

        return topComp;
    }

    @Override
    public void initWidgets() {
        // done in createDialog		
    }

    // allow to override	
    @Override
    protected void dispose() {
        super.dispose();
        colorBarEditor.dispose();
    }
}
