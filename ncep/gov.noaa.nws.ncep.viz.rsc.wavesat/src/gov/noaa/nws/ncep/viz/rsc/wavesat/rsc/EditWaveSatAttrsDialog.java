package gov.noaa.nws.ncep.viz.rsc.wavesat.rsc;

import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarEditor;
import gov.noaa.nws.ncep.viz.ui.display.ColorBar;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * A Dialog to edit SGWH resource attributes.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 09/25/11      #248        Greg Hull      Initial Creation.
 * 07/01/14     TTR 1018     Steve Russell  Updated call to ColorBarEditor
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class EditWaveSatAttrsDialog extends AbstractEditResourceAttrsDialog {

    public EditWaveSatAttrsDialog(Shell parentShell, INatlCntrsResourceData r, Boolean apply) {
        super(parentShell, r, apply);
    }

    private RscAttrValue colorBarAttr = null;

    private RscAttrValue fontNameAttr = null;

    private RscAttrValue fontSizeAttr = null;

    private RscAttrValue timeDisplayIntervalAttr = null;

    private RscAttrValue timeDisplayColorAttr = null;

    private String[] availFonts = { "Times", "Serif", "Sans", "Utopia", "Roman", "Courier" };

    private String[] availFontsSizes = { "6", "10", "12", "14", "16", "18", "20", "24", "28" };

    private String[] availTimeIntStrs = { "10 Mins", "20 Mins", "30 Mins", "45 Mins", "1 Hour", "1 1/2 Hours", "2 Hours", "3 Hours", "6 Hours", "12 Hours", "24 Hours" };

    private Integer[] availTimeIntMins = { 10, 20, 30, 45, 60, 90, 120, 180, 360, 720, 1440 };

    private ColorBarEditor colorBarEditor = null;

    // 
    @Override
    public Composite createDialog(Composite topComp) {

        colorBarAttr = editedRscAttrSet.getRscAttr("colorBar");
        fontNameAttr = editedRscAttrSet.getRscAttr("fontName");
        fontSizeAttr = editedRscAttrSet.getRscAttr("fontSize");
        timeDisplayIntervalAttr = editedRscAttrSet.getRscAttr("timeDisplayInterval");
        timeDisplayColorAttr = editedRscAttrSet.getRscAttr("timeDisplayColor");

        if (colorBarAttr == null || colorBarAttr.getAttrClass() != ColorBar.class) {
            System.out.println("colorBar is null or not of expected class ColorBar?");
            return null;
        }
        if (fontNameAttr == null || fontNameAttr.getAttrClass() != String.class) {
            System.out.println("fontName is null or not of expected class String?");
            return null;
        }
        if (fontSizeAttr == null || fontSizeAttr.getAttrClass() != Integer.class) {
            System.out.println("fontSize is null or not of expected class Integer?");
            return null;
        }
        if (timeDisplayIntervalAttr == null || timeDisplayIntervalAttr.getAttrClass() != Integer.class) {
            System.out.println("timeDisplayInterval is null or not of expected class Integer?");
            return null;
        }
        if (timeDisplayColorAttr == null || timeDisplayColorAttr.getAttrClass() != RGB.class) {
            System.out.println("timeDisplayColor is null or not of expected class RGB?");
            return null;
        }

        FormLayout layout0 = new FormLayout();
        topComp.setLayout(layout0);

        Group selFontGrp = new Group(topComp, SWT.BORDER);
        FormData fd = new FormData();
        selFontGrp.setText("Select Font");

        selFontGrp.setLayout(new FormLayout());
        fd.width = 300;
        fd.left = new FormAttachment(0, 10);
        //        fd.right = new FormAttachment( 35, 0 );
        fd.bottom = new FormAttachment(0, 100);
        fd.top = new FormAttachment(0, 10);
        selFontGrp.setLayoutData(fd);

        //        FontData[] fontdata = shell.getDisplay().getFontList(null, true);
        //        for( int x = 0; x < fontdata.length; x++ ) {
        //                System.out.println(fontdata[x].getName());
        //        }
        //        // and the non-scalable ones
        //        fontdata = shell.getDisplay().getFontList(null, false);
        //        for( int x = 0; x < fontdata.length; x++ ) {
        //                System.out.println(fontdata[x].getName());
        //        }

        final Combo fontNameCombo = new Combo(selFontGrp, SWT.READ_ONLY | SWT.DROP_DOWN);
        fd = new FormData();
        fd.left = new FormAttachment(10, 0);
        fd.top = new FormAttachment(0, 30);
        fontNameCombo.setLayoutData(fd);

        fontNameCombo.setItems(availFonts);

        fontNameCombo.setText(((String) fontNameAttr.getAttrValue()));

        fontNameCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                int selIndx = fontNameCombo.getSelectionIndex();

                fontNameAttr.setAttrValue(availFonts[selIndx]);
            }
        });

        Label nameLbl = new Label(selFontGrp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(fontNameCombo, 0, SWT.LEFT);
        fd.bottom = new FormAttachment(fontNameCombo, -3, SWT.TOP);
        nameLbl.setLayoutData(fd);
        nameLbl.setText("Name");
        //        nameLbl.setVisible( false );

        final Combo fontSizeCombo = new Combo(selFontGrp, SWT.READ_ONLY | SWT.DROP_DOWN);
        fd = new FormData();
        fd.left = new FormAttachment(fontNameCombo, 20, SWT.RIGHT);
        fd.top = new FormAttachment(fontNameCombo, 0, SWT.TOP);
        fontSizeCombo.setLayoutData(fd);

        fontSizeCombo.setItems(availFontsSizes);
        fontSizeCombo.select(0);

        for (int i = 0; i < availFontsSizes.length; i++) {
            if (Integer.parseInt(availFontsSizes[i]) == (Integer) fontSizeAttr.getAttrValue()) {
                fontSizeCombo.select(i);
                break;
            }
        }

        Label fontSizeLbl = new Label(selFontGrp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(fontSizeCombo, 0, SWT.LEFT);
        fd.bottom = new FormAttachment(fontSizeCombo, -3, SWT.TOP);
        fontSizeLbl.setLayoutData(fd);
        fontSizeLbl.setText("Size");
        //        fontSizeLbl.setVisible( false );

        fontSizeCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                int selIndx = fontSizeCombo.getSelectionIndex();

                fontSizeAttr.setAttrValue(Integer.parseInt(availFontsSizes[selIndx]));
            }
        });

        Group selTimeStampGrp = new Group(topComp, SWT.BORDER);
        fd = new FormData();
        selTimeStampGrp.setText("Time Stamp");

        selTimeStampGrp.setLayout(new FormLayout());

        fd.top = new FormAttachment(selFontGrp, 0, SWT.TOP);
        fd.left = new FormAttachment(50, 10);
        fd.right = new FormAttachment(100, -10);
        fd.bottom = new FormAttachment(selFontGrp, 0, SWT.BOTTOM);
        selTimeStampGrp.setLayoutData(fd);

        final Combo timeStampIntCombo = new Combo(selTimeStampGrp, SWT.READ_ONLY | SWT.DROP_DOWN);
        fd = new FormData();
        fd.left = new FormAttachment(10, 0);
        fd.top = new FormAttachment(0, 30);
        timeStampIntCombo.setLayoutData(fd);

        // set the list of time Intervals and initialize to the currently selected one.
        //
        timeStampIntCombo.setItems(availTimeIntStrs);

        for (int t = 0; t < availTimeIntMins.length; t++) {
            if (availTimeIntMins[t].equals((Integer) timeDisplayIntervalAttr.getAttrValue())) {
                timeStampIntCombo.select(t);
                break;
            }
        }

        timeStampIntCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                int selIndx = timeStampIntCombo.getSelectionIndex();

                timeDisplayIntervalAttr.setAttrValue(availTimeIntMins[selIndx]);
            }
        });

        Label timeStampIntLbl = new Label(selTimeStampGrp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(timeStampIntCombo, 0, SWT.LEFT);
        fd.bottom = new FormAttachment(timeStampIntCombo, -3, SWT.TOP);
        timeStampIntLbl.setLayoutData(fd);
        timeStampIntLbl.setText("Interval");
        //        timeStampIntLbl.setVisible(  false  );

        Composite selColComp = new Composite(selTimeStampGrp, SWT.NONE);
        fd = new FormData();

        fd.bottom = new FormAttachment(timeStampIntCombo, 0, SWT.BOTTOM);
        fd.left = new FormAttachment(timeStampIntCombo, 30, SWT.RIGHT);
        selColComp.setLayoutData(fd);
        selColComp.setLayout(new GridLayout());

        final ColorButtonSelector colBtnSel = new ColorButtonSelector(selColComp, 60, 30);
        colBtnSel.setColorValue((RGB) timeDisplayColorAttr.getAttrValue());
        colBtnSel.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                timeDisplayColorAttr.setAttrValue(event.getNewValue());
            }
        });

        Group colorBarGrp = new Group(topComp, SWT.NONE);
        colorBarGrp.setText("Edit Color Bar");
        fd = new FormData();//400,300);        
        fd.left = new FormAttachment(0, 15);
        fd.right = new FormAttachment(100, -15);
        fd.top = new FormAttachment(selFontGrp, 15, SWT.BOTTOM);
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

    @Override
    protected void dispose() {
        super.dispose();
        colorBarEditor.dispose();
    }
}
