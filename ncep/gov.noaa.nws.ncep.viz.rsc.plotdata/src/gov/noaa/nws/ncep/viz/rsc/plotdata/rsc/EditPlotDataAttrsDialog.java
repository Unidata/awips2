package gov.noaa.nws.ncep.viz.rsc.plotdata.rsc;

import java.io.File;
import java.util.HashMap;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.ConditionalFilter;
import gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.ConditionalFilterMngr;
import gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.EditConditionalFilterAttrDialog;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.widgets.Text;

/**
 *  UI for editing Point data resource attributes. 
 *   
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/15/2009    172       	M. Li  		Initial creation.
 * 12/05/2009    xxx        Greg Hull   
 * 04/27/2010    #245       Greg Hull    Added Apply Button
 * 07/26/2010    T285       qzhou       Modified createDialog for uair
 * 10/13/2011               qzhou       Fixed default level  
 * 11/01/2011    #482       Greg Hull   edit the plotDensity
 * 12/07/2011               B. Hebbard  Added "Plot All" option
 * 02/16/2012    #639       Q. Zhou     Changed density text listener. Changed density duration to 30. Adjusted field size.
 * 04/02/2012    #615       S. Gurung   Added code related to Conditional Filtering
 * 02/05/2012    #606       Greg Hull   Don't get level attr for non-sounding resources
 * 12/25/2012    #947       Greg Hull   Allow pre-load conditional filter editing.
 * 12/29/2012    #947       Greg Hull   Add unimplemented level type
 *
 * </pre>
 * 
 * @author mli
 * @version 1.0
 */
public class EditPlotDataAttrsDialog extends AbstractEditResourceAttrsDialog {
	private RscAttrValue plotDensityAttr = null;
	private RscAttrValue plotLevelAttr = null;
	private RscAttrValue plotModelAttr = null;
	private RscAttrValue condFilterAttr = null;
	private PlotModel editedPlotModel = null;
	private ConditionalFilter editedCondFilter = null;
	
	private Combo condFilterCombo = null;
	private Button editBtn = null;
	
	private String pluginName = null;
	
	private boolean isSounding = false;
	private Combo levelCombo = null;
	private final String[] stdPressLevels = {"1000", "925", "850", "700", "500", "400", "300", "250", "200", "150", "100"}; //, "70", "50", "30", "20", "10", "7", "5", "3", "2", "1"};
//	private final String[] stdHeightLevels = 

	public EditPlotDataAttrsDialog(Shell parentShell, INatlCntrsResourceData r, Boolean apply) {
		super(parentShell, r, apply);
		if( !(r instanceof PlotResourceData ) ) {
			System.out.println("EditPlotDataAttrsDialog: Resource is not a PlotResource");
			return;
		}
		isSounding = !((PlotResourceData)r).isSurfaceOnly();
		pluginName = ((PlotResourceData)r).getPluginName();
	}

	@Override
	public Composite createDialog(Composite topComp) {
		
		// create a top-level composite to define a FormLayout
        topComp.setLayout( new FormLayout() );

		if (isSounding) {
			plotLevelAttr = editedRscAttrSet.getRscAttr( "levelKey" );
			if( plotLevelAttr == null || plotLevelAttr.getAttrClass() != String.class ) {
				System.out.println("plotModelAttr is null or not of expected class plotModel?");
			return topComp;
			}
		}
		plotDensityAttr = editedRscAttrSet.getRscAttr("plotDensity");
		plotModelAttr = editedRscAttrSet.getRscAttr( "plotModel" );
		condFilterAttr = editedRscAttrSet.getRscAttr("conditionalFilter");
		
		if( plotDensityAttr == null || plotDensityAttr.getAttrClass() != Integer.class ) {
			System.out.println("plotDensityAttr is null or not of expected class Integer?");
			return topComp;
		}
		if( plotModelAttr == null || plotModelAttr.getAttrClass() != PlotModel.class ) {
			System.out.println("plotModelAttr is null or not of expected class plotModel?");
			return topComp;
		}
		if( condFilterAttr == null || condFilterAttr.getAttrClass() != ConditionalFilter.class ) {
			System.out.println("condFilterAttr is null or not of expected class ConditionalFilter?");
			return topComp;
		}
		
		editedPlotModel = (PlotModel)plotModelAttr.getAttrValue();
		editedCondFilter = (ConditionalFilter)condFilterAttr.getAttrValue();
		
		Group densityGrp = new Group ( topComp, SWT.SHADOW_NONE );
        densityGrp.setText("Plot Density");
		FormData fd = new FormData();
        fd.left = new FormAttachment( 0, 10 );
        fd.top = new FormAttachment( 0, 10 );
        fd.right = new FormAttachment( 100, 
        								(isSounding ? -140: -10) );
        densityGrp.setLayoutData( fd );

        densityGrp.setLayout( new FormLayout() );
        
        final Slider densitySldr = new Slider( densityGrp, SWT.HORIZONTAL );
		fd = new FormData();
        fd.left = new FormAttachment( 0, 10 );
        fd.top = new FormAttachment( 0, 25 );
        fd.right = new FormAttachment( 100, -135 );
        fd.bottom = new FormAttachment( 100, -10 );
        //fd.width = 120;
        densitySldr.setLayoutData( fd );

        int initSldrVal = Math.min( ((Integer)plotDensityAttr.getAttrValue()).intValue(), 30 );
        densitySldr.setValues( initSldrVal, 1, 31, 1, 1, 5 );
        densitySldr.setToolTipText("Density of plot stations");
        
        final Label sparseLbl = new Label( densityGrp, SWT.NONE );
        sparseLbl.setText("Sparse");
        fd = new FormData( );
        fd.bottom = new FormAttachment( densitySldr, -3, SWT.TOP );
        fd.left = new FormAttachment( densitySldr, 0, SWT.LEFT );
        sparseLbl.setLayoutData( fd );

        final Label denseLbl = new Label( densityGrp, SWT.NONE );
        denseLbl.setText("Dense");
        fd = new FormData( );
        fd.bottom = new FormAttachment( densitySldr, -3, SWT.TOP );
        fd.right = new FormAttachment( densitySldr, 0, SWT.RIGHT );
        denseLbl.setLayoutData( fd );

        final Text densityTxt = new Text( densityGrp, SWT.BORDER );
        densityTxt.setText( Integer.toString( densitySldr.getSelection() ) );
        fd = new FormData( );
        fd.left = new FormAttachment( densitySldr, 10, SWT.RIGHT );
        fd.bottom = new FormAttachment( densitySldr, 0, SWT.BOTTOM );
        fd.top = new FormAttachment( densitySldr, 0, SWT.TOP );
        fd.right = new FormAttachment( densityTxt, 30, SWT.LEFT );
        densityTxt.setToolTipText("Plot density");
        densityTxt.setLayoutData( fd );
        
        densityTxt.addKeyListener(new KeyAdapter() {
            public void keyReleased(KeyEvent e) {            	
            		int ival;
            		try {
            			ival = Integer.parseInt( densityTxt.getText() );
            		}
            		catch( NumberFormatException exc ) { 
            			ival = 10; //((Integer)plotDensityAttr.getAttrValue()).intValue();
            		}
            		if( ival < densitySldr.getMinimum() ) {
            			ival = densitySldr.getMinimum();
            			densityTxt.setText( Integer.toString( ival ) );
            		}
            		else if( ival >= densitySldr.getMaximum() ) { // was > see below
            			//ival = densitySldr.getMaximum();
            			ival = densitySldr.getMaximum() - 1;  //TODO that is, 20, due to slider quirk
            			densityTxt.setText( Integer.toString( ival ) );
            		}

            		densitySldr.setSelection( ival );
            		plotDensityAttr.setAttrValue( new Integer( ival ) );
            	}            	
        });
       
        final Button plotAllBtn = new Button ( densityGrp, SWT.CHECK );
        plotAllBtn.setText("Plot All");
        plotAllBtn.setToolTipText("Plot every station unconditionally");
        fd = new FormData( );
        fd.left = new FormAttachment( densityTxt, 15, SWT.RIGHT );
        fd.bottom = new FormAttachment( densityTxt, 0, SWT.BOTTOM );
        fd.right = new FormAttachment( 100, -10 );
        plotAllBtn.setLayoutData(fd);
        
        densitySldr.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent e) {
        		plotDensityAttr.setAttrValue( new Integer( densitySldr.getSelection() ) );
        		densityTxt.setText( Integer.toString( densitySldr.getSelection() ) );
        	}
        });

        plotAllBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent e) {
        		
        		if (plotAllBtn.getSelection()) {
        			// "Plot All" checked:  Gray out slider, and make density "99"
        			densitySldr.setEnabled(false);
        			densityTxt.setEnabled(false);
        			sparseLbl.setEnabled(false);
        			denseLbl.setEnabled(false);
        			plotDensityAttr.setAttrValue( new Integer( 99 ) );
        		}
        		else {
        			// "Plot All" unchecked:  Enable slider, and use its setting for density
        			densitySldr.setEnabled(true);
        			densityTxt.setEnabled(true);
        			sparseLbl.setEnabled(true);
        			denseLbl.setEnabled(true);
            		plotDensityAttr.setAttrValue( new Integer( densitySldr.getSelection() ) );
            		densityTxt.setText( Integer.toString( densitySldr.getSelection() ) );
        		}
        	}
        });
        
        // Big initial (incoming) density value means "plot all"
        if ( ((Integer)plotDensityAttr.getAttrValue()).intValue() > 30 ) {
        	plotAllBtn.setSelection(true);
			densitySldr.setEnabled(false);
			densityTxt.setEnabled(false);
			sparseLbl.setEnabled(false);
			denseLbl.setEnabled(false);
			plotDensityAttr.setAttrValue( new Integer( 99 ) );
        }

        Group condFilterGrp = new Group ( topComp, SWT.SHADOW_NONE );
        condFilterGrp.setText("Conditional Filter");
		fd = new FormData();
        fd.left = new FormAttachment( 0, 10 );
        fd.top = new FormAttachment( densityGrp, 5, SWT.BOTTOM );
        fd.right = new FormAttachment( 100, 
        							   (isSounding ? -140: -10 ) );
//        fd.right = new FormAttachment( densityGrp, 0, SWT.RIGHT );

        condFilterGrp.setLayoutData( fd );

        condFilterGrp.setLayout( new FormLayout() );
                
        condFilterCombo = new Combo(condFilterGrp, SWT.DROP_DOWN );
    	fd = new FormData();
        fd.top = new FormAttachment( 0, 10 );
        fd.left = new FormAttachment( 0, 10 );
        fd.right = new FormAttachment( 100, -100 );
        fd.bottom = new FormAttachment( 100, -10 );

        condFilterCombo.setLayoutData(fd);
        
        editBtn = new Button ( condFilterGrp, SWT.PUSH);
        editBtn.setText(" Edit... ");
        editBtn.setToolTipText("Edit Conditional Filter");
        
        fd = new FormData( );
        fd.left = new FormAttachment( condFilterCombo, 10, SWT.RIGHT );
        fd.bottom = new FormAttachment( condFilterCombo, 0, SWT.BOTTOM );
//        fd.right = new FormAttachment( 100, -10 );
        fd.width = 80;
        editBtn.setLayoutData(fd);
        
        editBtn.addSelectionListener(new SelectionAdapter() {

			public void widgetSelected(SelectionEvent e) {
				
				editConditionalFilter();
			}

		});
        
        condFilterCombo.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				
				ConditionalFilter cf = ConditionalFilterMngr.getInstance().
				                     getConditionalFilter(pluginName, condFilterCombo.getText().trim());
				if (cf == null) {
					cf = ConditionalFilterMngr.getInstance().getDefaultConditionalFilter( pluginName );
				} 
				else {
					cf = cf.clone();
				}

				condFilterAttr.setAttrValue( cf );
				editedCondFilter = cf;
				editedRscAttrSet.setAttrValue("conditionalFilter", cf);
			}
		});
        
// Spaces are ok. Could restrict other characters in the name?.
//        condFilterCombo.addVerifyListener( new VerifyListener() {			
//			@Override
//			public void verifyText(VerifyEvent e) {
//				String newText = e.text;
//				
//				if( e.text.contains( File.separator ) ||
//					e.text.contains( " " ) ) {
//					e.doit = false;
//				}
//			}
//		});
        
        condFilterCombo.addModifyListener( new ModifyListener( ) {
			
			@Override
			public void modifyText(ModifyEvent e) {
				editedCondFilter.setName( condFilterCombo.getText().trim() );
//				editBtn.setText( " New... ");

				for(int i = 0; i < condFilterCombo.getItems().length; i++) {
					if( condFilterCombo.getItems()[i].equals(editedCondFilter.getName())) { 
						editBtn.setText( " Edit... ");
						break;
					}
				}

			}
		});

        if (isSounding) {
        	Group selLevelGrp = new Group ( topComp, SWT.SHADOW_NONE );
        	selLevelGrp.setText("Vertical Level");

        	selLevelGrp.setLayout(new FormLayout() );

        	fd = new FormData();
        	//fd.width = 70;
        	fd.left = new FormAttachment( condFilterGrp, 10, SWT.RIGHT );
        	fd.right = new FormAttachment( 100, -10 );
        	fd.top = new FormAttachment( densityGrp, 0, SWT.TOP );
        	fd.bottom = new FormAttachment( condFilterGrp, 0, SWT.BOTTOM );
        	selLevelGrp.setLayoutData( fd );

//        	Composiet vcoordComp = 
        	Button presVcoordBtn = new Button( /*vcoordComp*/selLevelGrp, SWT.RADIO );
        	presVcoordBtn.setText( "Pressure" );
        	fd = new FormData();
        	fd.left = new FormAttachment( 0, 10);
        	fd.top = new FormAttachment( 0, 20 );
        	presVcoordBtn.setLayoutData( fd );
        	presVcoordBtn.setSelection( true );

        	Button heightVcoordBtn = new Button( /*vcoordComp*/selLevelGrp, SWT.RADIO );
        	heightVcoordBtn.setText( "Height" );
        	fd = new FormData();
        	fd.left = new FormAttachment( 0, 10);
        	fd.top = new FormAttachment( presVcoordBtn, 8 );
        	heightVcoordBtn.setLayoutData( fd );

        	presVcoordBtn.setEnabled( false );
        	heightVcoordBtn.setEnabled( false );

        	levelCombo = new Combo(selLevelGrp, SWT.DROP_DOWN );
        	// levelCombo.setSize( 40, 30 );

        	fd = new FormData(75, 25);
        	fd.left = new FormAttachment( 0, 10 );
        	fd.right = new FormAttachment( 100, -25 );
        	fd.top = new FormAttachment( heightVcoordBtn, 20, SWT.BOTTOM );
        	fd.bottom = new FormAttachment( 100, -10 );
        	levelCombo.setLayoutData( fd );

        	levelCombo.setItems( stdPressLevels );
        	RscAttrValue ra = editedRscAttrSet.getRscAttr("levelKey");
        	String s = ra.getAttrValue().toString();

        	levelCombo.setText(s);

        	Label mblbl = new Label(selLevelGrp, SWT.NONE);
        	mblbl.setText("mb");
        	fd = new FormData();
        	fd.left = new FormAttachment( levelCombo, 3, SWT.RIGHT );
        	fd.bottom = new FormAttachment( levelCombo, -2, SWT.BOTTOM );
//        	fd.right = new FormAttachment( 100, -10 );
        	mblbl.setLayoutData( fd );

        	levelCombo.addSelectionListener(new SelectionAdapter() {
        		public void widgetSelected(SelectionEvent event) {
        			// TODO Auto-generated method stub
        			String s = levelCombo.getText();
        			editedRscAttrSet.setAttrValue("levelKey", s);
        		}
        	});

        	levelCombo.addModifyListener( new ModifyListener() {
        		@Override
        		public void modifyText(ModifyEvent e) {
        			// TODO Auto-generated method stub
        			String ss = levelCombo.getText();
        			editedRscAttrSet.setAttrValue("levelKey", ss);
        		}
        	});

        	selLevelGrp.setVisible( isSounding );
        }

        Group pltMdlComp = new Group ( topComp, SWT.SHADOW_NONE );
        pltMdlComp.setText("Edit Plot Model");

        pltMdlComp.setLayout(new GridLayout() );

        fd = new FormData();
        fd.left = new FormAttachment( 0, 5 ); 
        fd.right = new FormAttachment( 100, -5 );
        fd.top = new FormAttachment( condFilterGrp, 5, SWT.BOTTOM );
        fd.bottom = new FormAttachment( 100, -3 );
        pltMdlComp.setLayoutData( fd );
        
		Composite editPlotModelComp = new EditPlotModelComposite(
				pltMdlComp, SWT.NONE, editedPlotModel, this.rscData );
		GridData gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;
		editPlotModelComp.setLayoutData( gd );
		
		return topComp;
	}		
				
	@Override
	public void initWidgets() {
		String[] condFiltersArray = ConditionalFilterMngr.getInstance().
							  getAllConditionalFiltersByPlugin( pluginName );
		condFilterCombo.setItems(condFiltersArray);  
        condFilterCombo.setText( editedCondFilter.getName() );
		editBtn.setText( " Edit... " );
        
        for(int i = 0; i < condFiltersArray.length; i++) {
			if( condFiltersArray[i].equals(editedCondFilter.getName())) { 
				condFilterCombo.select(i);
				editBtn.setText( " Edit... ");
				break;
			}
		}
	}
	
	private void editConditionalFilter() {
		String condFilterName = condFilterCombo.getText().trim();

		if( editedCondFilter == null ) {
			editedCondFilter = ConditionalFilterMngr.getInstance().getDefaultConditionalFilter( pluginName );
		}

		editedCondFilter.setName( condFilterName );
		
		// since we are editing the filter there are cases where the previous name might be mis-leading
		// in these cases we will create a name based on the new filter.
		Boolean renameFilter = ( condFilterName.isEmpty() ||
								 condFilterName.equals( ConditionalFilterMngr.NullFilterName ) ||
								 condFilterName.equals( editedCondFilter.getFilterAsString() ) );

		EditConditionalFilterAttrDialog editConditionalFilterDlg = 
			 	new EditConditionalFilterAttrDialog(shell, editedCondFilter);
		
		ConditionalFilter newConditionalFilter = (ConditionalFilter)editConditionalFilterDlg.open( 
						                           shell.getLocation().x+50, shell.getLocation().y );
		
		if( newConditionalFilter != null ) {			
			if( newConditionalFilter.getSize() == 0 ) {
				condFilterName = ConditionalFilterMngr.NullFilterName;
			}
			else if( renameFilter ) {									
				condFilterName = newConditionalFilter.getFilterAsString();					
			}
			// if the NoFilter or a saved filter was edited, then add an '*' to the name to indicate that
			// it has been modified.
			else if( ConditionalFilterMngr.getInstance().getConditionalFilter( pluginName, condFilterName) != null ) {
				
				condFilterName = condFilterName+"(E)";									
			}

			newConditionalFilter.setName( condFilterName );
			condFilterCombo.setText( condFilterName );
			condFilterCombo.setSelection( new Point(0,condFilterName.length()) );
			condFilterCombo.setFocus();
			
			editedCondFilter = newConditionalFilter;
			editedRscAttrSet.setAttrValue("conditionalFilter", editedCondFilter );
			rscData.setRscAttrSet( editedRscAttrSet );
            rscData.setIsEdited( true );

// The user will need to click the apply button.
//            if( hasApplyBtn ) {
//            	NmapUiUtils.getActiveNatlCntrsEditor().refresh();
//            }
		}
	}
}
