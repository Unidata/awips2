package gov.noaa.nws.ncep.viz.overlays.dialogs;

import java.util.EnumMap;
import java.util.Map;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;

import gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorMatrixSelector;
import gov.noaa.nws.ncep.viz.overlays.IPointOverlayResourceData.MarkerTextSize;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleIntervalMode;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleLatMode;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScalePosition;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleTextFont;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleTextSize;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleTextStyle;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleUnit;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;

/*
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07 Oct 2010  311         B. Hebbard	Initial Creation (derived from ChangeLatLonAttributesDialog)
 * 
 * @author bhebbard
 * @version 1
 */

public class ChangeScaleAttributesDialog extends AbstractEditResourceAttrsDialog {

    private Label modelLabel;
    private Combo modelCombo = null;
    
    private Label positionLabel;
    private Combo positionCombo = null;
    
    private Label unitLabel;
    private Combo unitCombo = null;
    
    private Label valueLabel;
    private Combo valueCombo = null;
    private Text  valueText = null;
    
    private Label latitudeLabel;
    private Combo latitudeCombo = null;
    private Text  latitudeText = null;
    
    private Label sizeLabel;
    private Combo sizeCombo = null;
    
    private Label fontLabel;
    private Combo fontCombo = null;
    
    private Label styleLabel;
    private Combo styleCombo = null;

	private final static org.apache.log4j.Logger log = 
				org.apache.log4j.Logger.getLogger(ChangeScaleAttributesDialog.class);
	
	//  Current attribute values.
    private RscAttrValue scaleModel = null;
    private RscAttrValue scalePosition = null;    
    private RscAttrValue scaleUnit = null;
    private RscAttrValue scaleIntervalMode = null;
    private RscAttrValue scaleIntervalValue = null;
    private RscAttrValue scaleLatMode = null;
    private RscAttrValue scaleEffectiveLatitudeValue = null;
    private RscAttrValue scaleTextFont = null;
    private RscAttrValue scaleTextSize = null;
    private RscAttrValue scaleTextStyle = null;
    private RscAttrValue color = null;

    private ColorMatrixSelector colorMatrixSelector; 
    
    /**
     * Constructor
     * 
     * @param parentShell
     * @param dialogTitle
     */
    public ChangeScaleAttributesDialog(Shell parentShell, INatlCntrsResourceData rd, Boolean apply ) {
        super(parentShell, rd, apply);
    }

    @Override
    public Composite createDialog(Composite composite) {
    	final Display display = composite.getDisplay();
    	
    	FormLayout layout0 = new FormLayout();
    	composite.setLayout(layout0);

    	scaleModel                  = editedRscAttrSet.getRscAttr("scaleModel");
    	scalePosition               = editedRscAttrSet.getRscAttr("scalePosition");
    	scaleUnit                   = editedRscAttrSet.getRscAttr("scaleUnit");
    	scaleIntervalMode           = editedRscAttrSet.getRscAttr("scaleIntervalMode");
    	scaleIntervalValue          = editedRscAttrSet.getRscAttr("scaleIntervalValue");
    	scaleLatMode                = editedRscAttrSet.getRscAttr("scaleLatMode");
    	scaleEffectiveLatitudeValue = editedRscAttrSet.getRscAttr("scaleEffectiveLatitudeValue");
    	scaleTextFont               = editedRscAttrSet.getRscAttr("scaleTextFont");
    	scaleTextSize               = editedRscAttrSet.getRscAttr("scaleTextSize");
    	scaleTextStyle              = editedRscAttrSet.getRscAttr("scaleTextStyle");
    	color                       = editedRscAttrSet.getRscAttr("color");

    	// confirm the classes of the attributes..
    	//if( scaleModel.getAttrClass() != ScaleModel.class ) {
    	//	System.out.println( "lineStyle is not of expected class? "+ scaleModel.getAttrClass().toString() );
    	//}  else
    	if( scalePosition.getAttrClass() != Integer.class ) {
    		System.out.println( "scalePosition is not of expected class? "+ scalePosition.getAttrClass().toString() );
    	}
    	else if( scaleUnit.getAttrClass() != Integer.class ) {
    		System.out.println( "scaleUnit is not of expected class? "+ scaleUnit.getAttrClass().toString() );
    	}
    	else if( scaleIntervalMode.getAttrClass() != Integer.class ) {
    		System.out.println( "scaleIntervalMode is not of expected class? "+ scaleIntervalMode.getAttrClass().toString() );
    	}
    	else if( scaleIntervalValue.getAttrClass() != Integer.class ) {
    		System.out.println( "scaleIntervalValue is not of expected class? "+ scaleIntervalValue.getAttrClass().toString() );
    	}
    	else if( scaleLatMode.getAttrClass() != Integer.class ) {
    		System.out.println( "scaleLatMode is not of expected class? "+ scaleLatMode.getAttrClass().toString() );
    	}
    	else if( scaleEffectiveLatitudeValue.getAttrClass() != Integer.class ) {
    		System.out.println( "scaleEffectiveLatitudeValue is not of expected class? "+ scaleEffectiveLatitudeValue.getAttrClass().toString() );
    	}
    	else if( scaleTextFont.getAttrClass() != Integer.class ) {
    		System.out.println( "scaleTextFont is not of expected class? "+ scaleTextFont.getAttrClass().toString() );
    	}
    	else if( scaleTextSize.getAttrClass() != Integer.class ) {
    		System.out.println( "scaleTextSize is not of expected class? "+ scaleTextSize.getAttrClass().toString() );
    	}
    	else if( scaleTextStyle.getAttrClass() != Integer.class ) {
    		System.out.println( "scaleTextStyle is not of expected class? "+ scaleTextStyle.getAttrClass().toString() );
    	}
    	else if( color.getAttrClass() != RGB.class ) {
    		System.out.println( "color is not of expected class? "+ scaleTextStyle.getAttrClass().toString() );
    	}

        //  Lay out the various groups within the dialog
        
        Group selectTextAttributesGroup = new Group ( composite, SWT.SHADOW_NONE );
        selectTextAttributesGroup.setText("Text Attributes");
        GridLayout textAttributesGridLayout = new GridLayout();
        textAttributesGridLayout.numColumns = 4;
        textAttributesGridLayout.marginHeight = 18;
        textAttributesGridLayout.marginWidth = 18;
        textAttributesGridLayout.horizontalSpacing = 8;
        textAttributesGridLayout.verticalSpacing = 8;
        selectTextAttributesGroup.setLayout(textAttributesGridLayout);
        
        FormData formData1 = new FormData();
        formData1.top = new FormAttachment(5, 100);
        formData1.left = new FormAttachment(2, 0);
        selectTextAttributesGroup.setLayoutData(formData1);
        
        Group selectColorGroup = new Group ( composite, SWT.SHADOW_NONE );
        selectColorGroup.setText("Color");
        
        FormData formData2 = new FormData();
        formData2.top   = new FormAttachment(selectTextAttributesGroup, 6);
        formData2.left  = new FormAttachment(2, 0);
        formData2.right = new FormAttachment(98, 0);
        formData2.height = 170;
        selectColorGroup.setLayoutData(formData2);

        //  Lay out individual components within the dialog and groups
        
		positionLabel = new Label(composite, SWT.LEFT);
		positionLabel.setText("Position:");
        
        FormData formData3 = new FormData();
        formData3.top = new FormAttachment(2, 8);
        formData3.left = new FormAttachment(4, 0);
        positionLabel.setLayoutData(formData3);

		positionCombo = new Combo( composite, SWT.DROP_DOWN | SWT.READ_ONLY );
        
        FormData formData4 = new FormData();
        formData4.top = new FormAttachment(positionLabel, 0, SWT.CENTER);
        formData4.left = new FormAttachment(positionLabel, 72, SWT.LEFT);
        positionCombo.setLayoutData(formData4);

		for ( ScalePosition sp : ScalePosition.values() ) {
			positionCombo.add( sp.getDisplayName() );
		}
		positionCombo.select( (Integer) scalePosition.getAttrValue() );
		positionCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	scalePosition.setAttrValue( positionCombo.getSelectionIndex() );
            }
        });

		unitLabel = new Label(composite, SWT.LEFT);
		unitLabel.setText("Unit:");
        
        FormData formData5 = new FormData();
        formData5.top = new FormAttachment(2, 8);
        formData5.left = new FormAttachment(positionCombo, 18, SWT.RIGHT);
        unitLabel.setLayoutData(formData5);

		unitCombo = new Combo( composite, SWT.DROP_DOWN | SWT.READ_ONLY );
        
        FormData formData6 = new FormData();
        formData6.top = new FormAttachment(unitLabel, 0, SWT.CENTER);
        formData6.left = new FormAttachment(unitLabel, 6, SWT.RIGHT);
        formData6.right = new FormAttachment(100, -6);
        unitCombo.setLayoutData(formData6);

		for ( ScaleUnit su : ScaleUnit.values() ) {
			unitCombo.add( su.toString() );
		}
		unitCombo.select( (Integer) scaleUnit.getAttrValue() );
		unitCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	scaleUnit.setAttrValue( unitCombo.getSelectionIndex() );
            }
        });

		valueLabel = new Label(composite, SWT.LEFT);
		valueLabel.setText("Interval:");
        
        FormData formData7 = new FormData();
        formData7.top = new FormAttachment(positionLabel, 20);
        formData7.left = new FormAttachment(4, 0);
        valueLabel.setLayoutData(formData7);

		valueCombo = new Combo( composite, SWT.DROP_DOWN | SWT.READ_ONLY );
        
        FormData formData8 = new FormData();
        formData8.top = new FormAttachment(valueLabel, 0, SWT.CENTER);
        formData8.left = new FormAttachment(valueLabel, 72, SWT.LEFT);
        valueCombo.setLayoutData(formData8);

		for ( ScaleIntervalMode sim : ScaleIntervalMode.values() ) {
			valueCombo.add( sim.getDisplayName() );
		}
		valueCombo.select( (Integer) scaleIntervalMode.getAttrValue() );
		valueCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	scaleIntervalMode.setAttrValue( valueCombo.getSelectionIndex() );
            	valueText.setEnabled(valueCombo.getSelectionIndex() == 1);
            }
        });
		
        valueText = new Text(composite, SWT.SINGLE);                        
        valueText.setText( String.valueOf( (Integer)scaleIntervalValue.getAttrValue() )); 
        valueText.setEditable(true);   
        valueText.addModifyListener(new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
				getValidInteger( valueText, scaleIntervalValue );
        	}
        });
		valueText.setEnabled(valueCombo.getSelectionIndex() == 1);
        
        FormData formData9 = new FormData();
        formData9.top = new FormAttachment(valueLabel, 0, SWT.CENTER);
        formData9.left = new FormAttachment(valueLabel, 220, SWT.LEFT);
        formData9.width = 60;
        valueText.setLayoutData(formData9);

		latitudeLabel = new Label(composite, SWT.LEFT);
		latitudeLabel.setText("Latitude:");
        
        FormData formData10 = new FormData();
        formData10.top = new FormAttachment(valueLabel, 20);
        formData10.left = new FormAttachment(4, 0);
        latitudeLabel.setLayoutData(formData10);

		latitudeCombo = new Combo( composite, SWT.DROP_DOWN | SWT.READ_ONLY );
        
        FormData formData11 = new FormData();
        formData11.top = new FormAttachment(latitudeLabel, 0, SWT.CENTER);
        formData11.left = new FormAttachment(latitudeLabel, 72, SWT.LEFT);
        latitudeCombo.setLayoutData(formData11);

		for ( ScaleLatMode slm : ScaleLatMode.values() ) {
			latitudeCombo.add( slm.getDisplayName() );
		}
		latitudeCombo.select( (Integer) scaleLatMode.getAttrValue() );
        latitudeCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	scaleLatMode.setAttrValue( latitudeCombo.getSelectionIndex() );
            	latitudeText.setEnabled(latitudeCombo.getSelectionIndex() == 2);
            }
        });
		
        latitudeText = new Text(composite, SWT.SINGLE);                        
        latitudeText.setText( String.valueOf( (Integer)scaleEffectiveLatitudeValue.getAttrValue() )); 
        latitudeText.setEditable(true);   
        latitudeText.addModifyListener(new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
				getValidInteger( latitudeText, scaleEffectiveLatitudeValue );
        	}
        });
        latitudeText.setEnabled(latitudeCombo.getSelectionIndex() == 2);

        FormData formData12 = new FormData();
        formData12.top = new FormAttachment(latitudeLabel, 0, SWT.CENTER);
        formData12.left = new FormAttachment(latitudeLabel, 220, SWT.LEFT);
        formData12.width = 60;
        latitudeText.setLayoutData(formData12);

		fontLabel = new Label(selectTextAttributesGroup, SWT.LEFT);
		fontLabel.setText("Font:");

		fontCombo = new Combo( selectTextAttributesGroup, SWT.DROP_DOWN | SWT.READ_ONLY );

		for ( ScaleTextFont stf : ScaleTextFont.values() ) {
			fontCombo.add( stf.getDisplayName() );
		}
		fontCombo.select( (Integer) scaleTextFont.getAttrValue() );
		fontCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	scaleTextFont.setAttrValue( fontCombo.getSelectionIndex() );
            }
        });

		sizeLabel = new Label(selectTextAttributesGroup, SWT.LEFT);
		sizeLabel.setText("Size:");

		sizeCombo = new Combo( selectTextAttributesGroup, SWT.DROP_DOWN | SWT.READ_ONLY );

		for ( ScaleTextSize sts : ScaleTextSize.values() ) {
			sizeCombo.add( sts.getDisplayName() );
		}
		sizeCombo.select( (Integer) scaleTextSize.getAttrValue() );
		sizeCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	scaleTextSize.setAttrValue( sizeCombo.getSelectionIndex() );
            }
        });

		styleLabel = new Label(selectTextAttributesGroup, SWT.LEFT);
		styleLabel.setText("Style:");

		styleCombo = new Combo( selectTextAttributesGroup, SWT.DROP_DOWN | SWT.READ_ONLY );

		for ( ScaleTextStyle sts : ScaleTextStyle.values() ) {
			styleCombo.add( sts.getDisplayName() );
		}
		styleCombo.select( (Integer) scaleTextStyle.getAttrValue() );
		styleCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	scaleTextStyle.setAttrValue( styleCombo.getSelectionIndex() );
            }
        });
		 
        //  Scale Color

		colorMatrixSelector = new ColorMatrixSelector(selectColorGroup, false, true,
				22, 88, 18, 22, 28, 86, 0, 4, 5);
		colorMatrixSelector.setColorValue( (RGB)color.getAttrValue());
		colorMatrixSelector.addListener(new IPropertyChangeListener() {
	    	public void propertyChange(PropertyChangeEvent event) {
	    		color.setAttrValue( colorMatrixSelector.getColorValue() );
	        }
	    });
       
        return composite;
    }
    
    // if an int can be parsed from the widget then set the rscAttr 
    // and if not then reset the widget with the current value of the RscAttrValue
    //
    public void getValidInteger( Text txtWid, RscAttrValue intAttr ) {
    	String intStr = txtWid.getText();
    	if( intStr.isEmpty() ) { 
    		return;
    	}
		try {
		    int ival = Integer.parseInt( txtWid.getText() );
		    intAttr.setAttrValue( Integer.valueOf(ival) );
		}
		catch( NumberFormatException exc ) {
			int ival = ((Integer)intAttr.getAttrValue()).intValue();
			txtWid.setText( Integer.toString( ival ) );
		}
    }
    
	@Override
	public void initWidgets() {
		// TODO Auto-generated method stub
	}

}

