package gov.noaa.nws.ncep.viz.resources.misc;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

import com.raytheon.uf.viz.core.map.IMapDescriptor;

//import gov.noaa.nws.ncep.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorMatrixSelector;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.EditElement;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.MiscResourceAttr;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.MiscRscAttrs;

/**
 *  an interface to edit resource attributes
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 *  05/06/09     #115        Greg Hull    Initial Creation.
 *  06/12/09     #115        Greg Hull    Modified to work with AbstractEditResourceAttrsDialog
 * 	04/25/10     #245        Greg Hull    Changed SLIDER_TEXT to SPINNER
 * 	04/27/10     #245        Greg Hull    Added Apply Button
 *  12/14/12     #861        Greg Hull    Added COLOR_PALLETTE for Pgen Rsc
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class EditMiscResourceAttrsDialog extends AbstractEditResourceAttrsDialog { 
    private MiscRscAttrs miscRscAttrs;
    
    private boolean ok=false;
    
    // can't define rsc ast IMiscResource since this constructor is invoked thru reflection and is looking for INatlCntrsResource 
//    public EditMiscResourceAttrsDialog( Shell parentShell, INatlCntrsResource r ) {
    public EditMiscResourceAttrsDialog( Shell parentShell, INatlCntrsResourceData rd, Boolean apply ) {
        super(parentShell, rd, apply );
        if( !(rd instanceof IMiscResourceData) ) {
        	System.out.print( "EditMiscResourceAttrsDialog is expecting an IMiscResource object");
        	return;
        }
        miscRscAttrs = ((IMiscResourceData)rd).getMiscResourceAttrs();
    }

    // 
    public Composite createDialog( Composite topComp ) {
    	int dispCol = 0;
    	
        Composite gridComp = new Composite( topComp, SWT.NONE );
        GridLayout gridLayout = new GridLayout( miscRscAttrs.getNumDisplayColumns(), false );
        //gridLayout.marginLeft = 10;
        gridLayout.marginHeight = 5;
        gridLayout.marginWidth = 20;
        gridLayout.verticalSpacing = 0;
        gridLayout.horizontalSpacing = 0;
        gridLayout.makeColumnsEqualWidth = false;
        
        gridComp.setLayout( gridLayout );

        FormData fd = new FormData();
        
        // loop thru the resource attributes
        for( MiscResourceAttr miscRscAttr : miscRscAttrs.getMiscResourceAttrs() ) {
        	dispCol = (dispCol % miscRscAttrs.getNumDisplayColumns()) + 1;
        	
        	if( miscRscAttr.dispColumn > miscRscAttrs.getNumDisplayColumns() ) {
        		System.out.println("Error in miscRscAttrList for attr "+ miscRscAttr.getAttrName()+
        				           " : display column > number of columns");
        		continue;
        	}
        	
        	while( miscRscAttr.dispColumn != dispCol ) {
            	dispCol = (dispCol % miscRscAttrs.getNumDisplayColumns()) + 1;
            	Composite blankComp = new Composite( gridComp, SWT.NONE );
            	blankComp.setSize(10, 10);

        		GridData gd = new GridData();
        		gd.horizontalIndent = 0;
        		gd.verticalIndent = 5;
        		//gd.grabExcessVerticalSpace = true;
        		//gd.exclude = true;  ignores the cell altogether
        		//gd.horizontalSpan  = 1;
        		gd.heightHint = 20;
        		blankComp.setLayoutData( gd );
            	//blankComp.setBackground( new Color( parent.getDisplay(), 0,0,0) );
        	}
        	
        	// 
        	if( miscRscAttr.guiElem == EditElement.CHECK_BOX ) {
        		final RscAttrValue rscAttr = editedRscAttrSet.getRscAttr( miscRscAttr.getAttrName() );
        		
        		if( rscAttr == null ) {
        			System.out.println("Resource Attr "+miscRscAttr.getAttrName()+" not found in rscAttrSet");
        			return gridComp;
        		}
        		else if( !(rscAttr.getAttrValue() instanceof Boolean) ) {
        			System.out.println("Resource Attr "+miscRscAttr.getAttrName()+" is not of expected class Boolean");
        			return gridComp;
        		}
        		Composite chkComp = new Composite( gridComp, SWT.NONE );
        		//chkComp.setBackground( new Color( parent.getDisplay(), 55,155,55));
        		chkComp.setLayout( new FormLayout() );
        		
        		GridData gd = new GridData();
        		gd.horizontalIndent = 0;
        		gd.verticalIndent = 5;
        		chkComp.setLayoutData( gd );
        		
        		final Button checkBox = new Button( chkComp, SWT.CHECK );
               	checkBox.setText( miscRscAttr.attrLabel );
            	fd = new FormData( );
            	fd.left = new FormAttachment( 0, 0 );
            	fd.top = new FormAttachment( 0, 0 );
            	checkBox.setLayoutData( fd );
               	checkBox.setAlignment( SWT.RIGHT );
               	checkBox.setSelection( ((Boolean)rscAttr.getAttrValue()).booleanValue() );
        		checkBox.addSelectionListener( new SelectionAdapter() {
					public void widgetSelected(SelectionEvent e) {
						rscAttr.setAttrValue( new Boolean( checkBox.getSelection()) );
					}
        		});
        	}
        	else if( miscRscAttr.guiElem == EditElement.COLOR_SELECTOR ) {
        		final RscAttrValue rscAttr = editedRscAttrSet.getRscAttr( miscRscAttr.getAttrName() );
        		
        		if( rscAttr == null ) {
        			System.out.println("Resource Attr "+miscRscAttr.getAttrName()+" not found in rscAttrSet");
        			return gridComp;
        		}
        		else if( !(rscAttr.getAttrValue() instanceof RGB ) ) {
        			System.out.println("Resource Attr "+miscRscAttr.getAttrName()+" is not of expected class RGB");
        			return gridComp;
        		}
        		Composite colComp = new Composite( gridComp, SWT.NONE );

        		GridData gd = new GridData();
        		gd.horizontalIndent = 0;
        		gd.verticalIndent = 5;
        		colComp.setLayoutData( gd );

        		gridLayout = new GridLayout(1,true);
        		gridLayout.verticalSpacing = 0;
        		colComp.setLayout( gridLayout );

        		if( !miscRscAttr.attrLabel.equals( "" ) ) {
                	Label colLbl = new Label( colComp, SWT.NONE );
                	colLbl.setText( miscRscAttr.attrLabel );
        		}

        		// TODO replace this with new version for nmap
        		final ColorButtonSelector colorSel = new ColorButtonSelector( colComp, 50, 20 );
        		colorSel.setColorValue( (RGB)rscAttr.getAttrValue() );

        		colorSel.addListener(new IPropertyChangeListener() {
    				// forward the property change of the color selector
    				public void propertyChange( PropertyChangeEvent event ) {
    					rscAttr.setAttrValue( event.getNewValue() );
    				}
    			});
        	}
        	else if( miscRscAttr.guiElem == EditElement.COLOR_PALLETE ) {
        		final RscAttrValue rscAttr = editedRscAttrSet.getRscAttr( miscRscAttr.getAttrName() );
        		
        		if( rscAttr == null ) {
        			System.out.println("Resource Attr "+miscRscAttr.getAttrName()+" not found in rscAttrSet");
        			return gridComp;
        		}
        		else if( !(rscAttr.getAttrValue() instanceof RGB ) ) {
        			System.out.println("Resource Attr "+miscRscAttr.getAttrName()+" is not of expected class RGB");
        			return gridComp;
        		}
        		Composite colComp = new Composite( gridComp, SWT.NONE );

        		GridData gd = new GridData();
        		gd.horizontalIndent = 0;
        		gd.verticalIndent = 5;
        		colComp.setLayoutData( gd );

        		gridLayout = new GridLayout(1,true);
        		gridLayout.verticalSpacing = 0;
        		colComp.setLayout( gridLayout );

        		if( !miscRscAttr.attrLabel.equals( "" ) ) {
                	Label colLbl = new Label( colComp, SWT.NONE );
                	colLbl.setText( miscRscAttr.attrLabel );
        		}

        		// We could add parameters for this (similar to the spinner) but since it is now
        		// only used by the PGEN resource, just set the values for it.
        		final ColorMatrixSelector cms = new ColorMatrixSelector(colComp, false, true,
        				22, 60, 20, 25, 28, 90, 4, 8, 5);
        	    cms.setColorValue( (RGB)rscAttr.getAttrValue());

        		cms.addListener(new IPropertyChangeListener() {
    				public void propertyChange( PropertyChangeEvent event ) {
    					rscAttr.setAttrValue( event.getNewValue() );
    				}
    			});
        	}

        	else if( miscRscAttr.guiElem == EditElement.SPINNER ) {
        		final RscAttrValue rscAttr = editedRscAttrSet.getRscAttr( miscRscAttr.getAttrName() );
        		
        		if( rscAttr == null ) {
        			System.out.println("Resource Attr "+miscRscAttr.getAttrName()+" not found in rscAttrSet");
        			return gridComp;
        		}
        		
        		else if( !(rscAttr.getAttrValue() instanceof Integer ) &&
        				 !(rscAttr.getAttrValue() instanceof Float ) ) {
        			System.out.println("Resource Attr "+miscRscAttr.getAttrName()+
             					        " is not of expected class Integer or Float");
        			return gridComp;
        		}
        		
        		Composite spnrComp = new Composite( gridComp, SWT.NONE );
        		//chkComp.setBackground( new Color( parent.getDisplay(), 55,155,55));
        		spnrComp.setLayout( new GridLayout(2,false) );
        		
        		GridData gd = new GridData();
        		gd.horizontalIndent = 0;
        		gd.verticalIndent = 5;
        		spnrComp.setLayoutData( gd );
        		
        		Label lblWid = new Label( spnrComp, SWT.None );
        		lblWid.setText( miscRscAttr.attrLabel );

        		final Spinner spnrWid = new Spinner( spnrComp, SWT.BORDER );
        		spnrWid.setDigits( miscRscAttr.spinnerNumDigits );
        		spnrWid.setMinimum( miscRscAttr.spinnerMin );
        		spnrWid.setMaximum( miscRscAttr.spinnerMax );
        		spnrWid.setIncrement( miscRscAttr.spinnerIncr );
        		spnrWid.setPageIncrement( miscRscAttr.spinnerPageIncr );
        		
            	spnrWid.setLayoutData( new GridData() );     
            	
            	final int numDigits = miscRscAttr.spinnerNumDigits;
            	
            	if( rscAttr.getAttrValue() instanceof Integer ) {
                	spnrWid.setSelection( ((Integer)rscAttr.getAttrValue()).intValue() );            		
            	}
            	else if( rscAttr.getAttrValue() instanceof Float ) {
            		float scale = (float)Math.pow(10, numDigits );
            		spnrWid.setSelection( 
            				Math.round( scale*((Float)rscAttr.getAttrValue()).floatValue() ) );
            	}

            	spnrWid.addSelectionListener( new SelectionAdapter() {
					public void widgetSelected(SelectionEvent e) {
		            	if( rscAttr.getAttrValue() instanceof Integer ) {
		            		rscAttr.setAttrValue( new Integer( spnrWid.getSelection()) );
		            	}
		            	else if( rscAttr.getAttrValue() instanceof Float ) {
		            		float scale = (float)Math.pow(10, numDigits );
		            		rscAttr.setAttrValue( new Float( ((float)spnrWid.getSelection())/scale) );
		            	}		            	
					}
        		});

        	}
        	else if( miscRscAttr.guiElem == EditElement.LABEL ) {
        		Composite lblComp = new Composite( gridComp, SWT.NONE );
        		lblComp.setLayout( new FillLayout() );

        		GridData gd = new GridData(  );
        		gd.horizontalIndent = 0;
        		gd.verticalIndent = 5;
        		gd.heightHint = 15;
        		lblComp.setLayoutData( gd );
        		
        		Label lbl = new Label( lblComp, SWT.NONE );        		
            	lbl.setText( miscRscAttr.attrLabel );
        	}
        	else if( miscRscAttr.guiElem == EditElement.SEPARATOR ) {        		
        		dispCol = 0;
        		
        		Composite sepComp = new Composite( gridComp, SWT.NONE );
        		sepComp.setLayout( new FillLayout() );

        		GridData gd = new GridData( GridData.FILL_HORIZONTAL );
        		gd.horizontalIndent = 0;
        		gd.verticalIndent = 5;
        		gd.heightHint = 15;
        		// this should be in the first column and take up the whole row
        		gd.horizontalSpan = miscRscAttrs.getNumDisplayColumns();
        		sepComp.setLayoutData( gd );
        		
        		new Label( sepComp, SWT.SEPARATOR | SWT.HORIZONTAL );
	    	}
        	
        	else if( miscRscAttr.guiElem == EditElement.VERTICAL_SEPARATOR ) {        		
        		
        		Composite sepComp = new Composite( gridComp, SWT.NONE );
        		GridData gd = new GridData( GridData.FILL_VERTICAL );
        		gd.horizontalIndent = 0;
        		gd.verticalIndent = 5;
        		gd.heightHint = 15;
        		sepComp.setLayoutData( gd );
         		gridLayout = new GridLayout(1,true);
        		sepComp.setLayout( gridLayout );

        		new Label( sepComp, SWT.SEPARATOR | SWT.VERTICAL );
	    	}        	
        }
        return gridComp;
    }

    @Override
	public void initWidgets() {
		// this is done in CreateDialog
	}    
}

