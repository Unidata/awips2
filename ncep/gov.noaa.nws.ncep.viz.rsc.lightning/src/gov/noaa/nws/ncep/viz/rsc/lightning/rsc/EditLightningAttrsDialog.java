package gov.noaa.nws.ncep.viz.rsc.lightning.rsc;

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
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.widgets.Text;

/**
 * An interface to edit Lightning resource attributes.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 04/10/2010    #257        Greg Hull      Initial Creation.
 * 04/11/2010    #259        Greg Hull      Added ColorBar
 * 04/27/2010    #245        Greg Hull      Added Apply Button
 * 07/01/2014 TTR 1018       Steve Russell  Updated call to ColorBarEditor
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class EditLightningAttrsDialog extends AbstractEditResourceAttrsDialog {

    public EditLightningAttrsDialog(Shell parentShell, INatlCntrsResourceData r, Boolean apply) {
        super(parentShell, r, apply);
        // TODO Auto-generated constructor stub
    }

    private Button enablePosStrikesBtn = null;

    private Button enableNegStrikesBtn = null;

    private RscAttrValue colorByIntensityAttr = null;

    private RscAttrValue enablePosStrikesAttr = null;

    private RscAttrValue enableNegStrikesAttr = null;

    private RscAttrValue posSymbolSizeAttr = null;

    private RscAttrValue negSymbolSizeAttr = null;

    private RscAttrValue colorBarAttr = null;

    private ColorBar editedColorBar = null;

    private Group posStrikesGrp = null;

    private Group negStrikesGrp = null;

    private Composite lineWidComp1 = null;

    private Composite lineWidComp2 = null;

    private Label lwLbl1 = null;

    private Label lwLbl2 = null;

    private ColorBarEditor colorBarEditor = null;

    @Override
    public Composite createDialog(Composite topComp) {

        colorByIntensityAttr = editedRscAttrSet.getRscAttr("colorByIntensity");
        enablePosStrikesAttr = editedRscAttrSet.getRscAttr("enablePositiveStrikes");
        enableNegStrikesAttr = editedRscAttrSet.getRscAttr("enableNegativeStrikes");
        posSymbolSizeAttr = editedRscAttrSet.getRscAttr("positiveSymbolSize");
        negSymbolSizeAttr = editedRscAttrSet.getRscAttr("negativeSymbolSize");
        colorBarAttr = editedRscAttrSet.getRscAttr("colorBar");

        if (colorByIntensityAttr == null || colorByIntensityAttr.getAttrClass() != Boolean.class) {
            System.out.println("colorByIntensity is null or not of expected class Boolean?");
        }
        if (enablePosStrikesAttr == null || enablePosStrikesAttr.getAttrClass() != Boolean.class) {
            System.out.println("enablePositiveStrikes is null or not of expected class Boolean?");
        }
        if (enableNegStrikesAttr == null || enableNegStrikesAttr.getAttrClass() != Boolean.class) {
            System.out.println("enableNegativeStrikes is null or not of expected class Boolean?");
        }
        if (posSymbolSizeAttr == null || posSymbolSizeAttr.getAttrClass() != Integer.class) {
            System.out.println("posSymbolSize is null or not of expected class Integer?");
        }
        if (negSymbolSizeAttr == null || negSymbolSizeAttr.getAttrClass() != Integer.class) {
            System.out.println("negSymbolSize is null or not of expected class Integer?");
        }
        if (colorBarAttr == null || colorBarAttr.getAttrClass() != ColorBar.class) {
            System.out.println("colorBar is null or not of expected class colorBar?");
        }

        editedColorBar = (ColorBar) colorBarAttr.getAttrValue();

        FormLayout layout0 = new FormLayout();
        topComp.setLayout(layout0);

        posStrikesGrp = new Group(topComp, SWT.SHADOW_NONE);
        posStrikesGrp.setText("Positive Strikes");
        FormData fd = new FormData();
        fd.left = new FormAttachment(0, 15);
        fd.top = new FormAttachment(0, 20);
        fd.right = new FormAttachment(50, -7);
        posStrikesGrp.setLayoutData(fd);
        posStrikesGrp.setLayout(new GridLayout(2, false));

        enablePosStrikesBtn = new Button(posStrikesGrp, SWT.CHECK);
        enablePosStrikesBtn.setText("Enable     ");

        //	    fd = new FormData();        
        //        fd.left = new FormAttachment( 0, 20 );
        //        fd.top = new FormAttachment( colbarLbl, 30, SWT.BOTTOM );
        //        enablePosStrikesBtn.setLayoutData( fd );        
        enablePosStrikesBtn.setSelection(((Boolean) enablePosStrikesAttr.getAttrValue()).booleanValue());

        enablePosStrikesBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                enablePosStrikesAttr.setAttrValue(new Boolean(enablePosStrikesBtn.getSelection()));
                lineWidComp1.setEnabled(enablePosStrikesBtn.getSelection());
                lwLbl1.setEnabled(enablePosStrikesBtn.getSelection());
            }
        });

        negStrikesGrp = new Group(topComp, SWT.SHADOW_NONE);
        negStrikesGrp.setText("Negative Strikes");
        fd = new FormData();
        fd.left = new FormAttachment(posStrikesGrp, 20, SWT.RIGHT);
        fd.top = new FormAttachment(posStrikesGrp, 0, SWT.TOP);
        fd.right = new FormAttachment(100, -15);
        //fd.right = new FormAttachment( 100, -20 );
        negStrikesGrp.setLayoutData(fd);
        negStrikesGrp.setLayout(new GridLayout(2, false));

        enableNegStrikesBtn = new Button(negStrikesGrp, SWT.CHECK);
        enableNegStrikesBtn.setText("Enable     ");
        //        fd = new FormData();        
        //        fd.left = new FormAttachment( enablePosStrikesBtn, 0, SWT.LEFT );
        //        fd.top = new FormAttachment( enablePosStrikesBtn, 10, SWT.BOTTOM );
        //        enableNegStrikesBtn.setLayoutData( fd );
        enableNegStrikesBtn.setAlignment(SWT.RIGHT);
        enableNegStrikesBtn.setSelection(((Boolean) enableNegStrikesAttr.getAttrValue()).booleanValue());

        enableNegStrikesBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                enableNegStrikesAttr.setAttrValue(new Boolean(enableNegStrikesBtn.getSelection()));
                lineWidComp2.setEnabled(enableNegStrikesBtn.getSelection());
                lwLbl2.setEnabled(enableNegStrikesBtn.getSelection());
            }
        });

        lineWidComp1 = new Composite(posStrikesGrp, SWT.NONE);
        lineWidComp1.setLayout(new FormLayout());

        //        fd = new FormData( );
        //        fd.top = new FormAttachment( enableNegStrikesBtn, 20, SWT.BOTTOM );
        //        fd.left = new FormAttachment( enableNegStrikesBtn, 0, SWT.LEFT );
        //        lineWidComp1.setLayoutData( fd );

        lwLbl1 = new Label(lineWidComp1, SWT.NONE);
        lwLbl1.setText("Symbol Size");
        fd = new FormData();
        fd.top = new FormAttachment(0, 10);
        fd.left = new FormAttachment(0, 0);
        lwLbl1.setLayoutData(fd);

        final Slider widthSlider1 = new Slider(lineWidComp1, SWT.HORIZONTAL);
        widthSlider1.setValues(((Integer) posSymbolSizeAttr.getAttrValue()).intValue(), 1, 11, 1, 1, 2);
        fd = new FormData();//180,20);
        fd.left = new FormAttachment(lwLbl1, 0, SWT.LEFT);
        fd.top = new FormAttachment(lwLbl1, 5, SWT.BOTTOM);
        widthSlider1.setLayoutData(fd);

        final Text widthText1 = new Text(lineWidComp1, SWT.BORDER);
        widthText1.setText(((Integer) posSymbolSizeAttr.getAttrValue()).toString());
        fd = new FormData(20, 20);
        fd.left = new FormAttachment(widthSlider1, 5, SWT.RIGHT);
        fd.bottom = new FormAttachment(widthSlider1, 0, SWT.BOTTOM);
        fd.right = new FormAttachment(100, -10);
        widthText1.setLayoutData(fd);

        widthSlider1.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                posSymbolSizeAttr.setAttrValue(new Integer(widthSlider1.getSelection()));
                widthText1.setText(Integer.toString(widthSlider1.getSelection()));
            }
        });

        widthText1.addSelectionListener(new SelectionAdapter() {
            public void widgetDefaultSelected(SelectionEvent e) {
                int ival;
                try {
                    ival = Integer.parseInt(widthText1.getText());
                } catch (NumberFormatException exc) {
                    ival = ((Integer) posSymbolSizeAttr.getAttrValue()).intValue();
                    widthText1.setText(Integer.toString(ival));
                }

                if (ival < widthSlider1.getMinimum()) {
                    ival = widthSlider1.getMinimum();
                    widthText1.setText(Integer.toString(ival));
                } else if (ival > widthSlider1.getMaximum()) {
                    ival = widthSlider1.getMaximum();
                    widthText1.setText(Integer.toString(ival));
                }

                widthSlider1.setSelection(ival);

                posSymbolSizeAttr.setAttrValue(new Integer(ival));
            }

            // not called
            public void widgetSelected(SelectionEvent e) {
                System.out.println("widgetSelected called");
            }
        });

        lineWidComp2 = new Composite(negStrikesGrp, SWT.NONE);
        lineWidComp2.setLayout(new FormLayout());

        //        fd = new FormData( );
        //        fd.top = new FormAttachment( lineWidComp1, 10, SWT.BOTTOM );
        //        fd.left = new FormAttachment( lineWidComp1, 0, SWT.LEFT );
        //        lineWidComp2.setLayoutData( fd );

        lwLbl2 = new Label(lineWidComp2, SWT.NONE);
        lwLbl2.setText("Symbol Size");
        fd = new FormData();
        fd.top = new FormAttachment(0, 10);
        fd.left = new FormAttachment(0, 0);
        lwLbl2.setLayoutData(fd);

        final Slider widthSlider2 = new Slider(lineWidComp2, SWT.HORIZONTAL);
        widthSlider2.setValues(((Integer) negSymbolSizeAttr.getAttrValue()).intValue(), 1, 11, 1, 1, 2);
        fd = new FormData();//180,20);
        fd.left = new FormAttachment(lwLbl2, 0, SWT.LEFT);
        fd.top = new FormAttachment(lwLbl2, 5, SWT.BOTTOM);
        widthSlider2.setLayoutData(fd);

        final Text widthText2 = new Text(lineWidComp2, SWT.BORDER);
        widthText2.setText(((Integer) negSymbolSizeAttr.getAttrValue()).toString());
        fd = new FormData(20, 20);
        fd.left = new FormAttachment(widthSlider2, 5, SWT.RIGHT);
        fd.bottom = new FormAttachment(widthSlider2, 0, SWT.BOTTOM);
        fd.right = new FormAttachment(100, -10);
        widthText2.setLayoutData(fd);

        widthSlider2.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                negSymbolSizeAttr.setAttrValue(new Integer(widthSlider2.getSelection()));
                widthText2.setText(Integer.toString(widthSlider2.getSelection()));
            }
        });

        widthText2.addSelectionListener(new SelectionAdapter() {
            public void widgetDefaultSelected(SelectionEvent e) {
                int ival;
                try {
                    ival = Integer.parseInt(widthText2.getText());
                } catch (NumberFormatException exc) {
                    ival = ((Integer) negSymbolSizeAttr.getAttrValue()).intValue();
                    widthText2.setText(Integer.toString(ival));
                }

                if (ival < widthSlider2.getMinimum()) {
                    ival = widthSlider2.getMinimum();
                    widthText2.setText(Integer.toString(ival));
                } else if (ival > widthSlider2.getMaximum()) {
                    ival = widthSlider2.getMaximum();
                    widthText2.setText(Integer.toString(ival));
                }

                widthSlider2.setSelection(ival);

                negSymbolSizeAttr.setAttrValue(new Integer(ival));
            }

            // not called
            public void widgetSelected(SelectionEvent e) {
                System.out.println("widgetSelected called");
            }
        });

        lineWidComp1.setEnabled(enablePosStrikesBtn.getSelection());
        lineWidComp2.setEnabled(enableNegStrikesBtn.getSelection());

        Group colorBarGrp = new Group(topComp, SWT.NONE);
        colorBarGrp.setText("Edit Color Bar");
        fd = new FormData();//400,300);        
        fd.left = new FormAttachment(0, 15);
        fd.right = new FormAttachment(100, -15);
        fd.top = new FormAttachment(posStrikesGrp, 15, SWT.BOTTOM);
        fd.bottom = new FormAttachment(100, -20);
        colorBarGrp.setLayoutData(fd);

        colorBarGrp.setLayout(new FormLayout());

        //		final Combo colorByCombo = new Combo( colorBarGrp, SWT.DROP_DOWN | SWT.READ_ONLY );
        //        fd = new FormData();        
        //        fd.left = new FormAttachment( 0, 15 );
        //        fd.top = new FormAttachment( 0, 35 );
        //        colorByCombo.setLayoutData( fd );
        //		
        //        colorByCombo.add("Time");
        //        colorByCombo.add("Intensity");
        //        
        //        colorByCombo.select( ((Boolean)colorByIntensityAttr.getAttrValue()).booleanValue() ? 1 : 0 );
        //        
        //        colorByCombo.addSelectionListener( new SelectionAdapter() {
        //        	public void widgetSelected(SelectionEvent e) {        		
        //        		colorByIntensityAttr.setAttrValue( 
        //        				new Boolean(  (colorByCombo.getSelectionIndex() == 0 ? false : true ) ) ); 
        //        	}
        //        });
        //
        //        Label colbarLbl = new Label( colorBarGrp, SWT.NONE );
        //        colbarLbl.setText( "Apply To:" );
        //        fd = new FormData( );
        //        fd.left = new FormAttachment( colorByCombo, 0, SWT.LEFT );
        //        fd.bottom = new FormAttachment( colorByCombo, -3, SWT.TOP );
        //        colbarLbl.setLayoutData( fd );

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
