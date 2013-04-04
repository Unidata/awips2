package gov.noaa.nws.ncep.viz.ui.locator.resource;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.ui.locator.resource.LocatorDataSource.SourceType;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;


/**
 * This class displays the Locator Edit dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/2009  	64        	M. Li    	Initial creation 
 * 03/2009		65			M. Li		Add multiple locators
 * 11/2009      138         Greg Hull   location Options
 * 12/10/2011   #561        Greg Hull   rework to actually edit the resource attributes
 * 12/14/2012   #903        G. Hull     add the font size
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 * 
 */
public class LocatorEditDialog extends AbstractEditResourceAttrsDialog { 

	private Combo fontSizeCombo;
	
	private Combo positionCombo;
	private Combo dataSourceCombo;	
    private Combo roundingCombo;
    private Combo unitsCombo;
	private Combo directionCombo;

	private int   seldPosition = 0;
	private LocatorDataSource seldDataSource = null;
	private HashMap<String,LocatorDataSource> availDataSources = null;
	
	private static final Integer fontSizes[] = { 8, 10, 12, 14, 16, 18, 20, 22, 24, 28, 32 };
    
    public static final String ROUNDING_OPTIONS[] = {"1", "5", "10"};
    
    public static final String DISTANCEUNIT_OPTIONS[] = {"omit", "NM", "SM", "KM"};
    public static final String LATLONUNIT_OPTIONS[] = {"degrees","decimal/minutes"};
    
    public static final String DIRECTIONUNIT_OPTIONS[] = {"omit", "16 point", "degrees"};
    
//    public static final String STATIONDISPLAY_OPTIONS[] = {"name", "ID"};
    
    public static final String NO_SOURCE = "None";
    
    private RscAttrValue   fontSizeAttr    = null;
    
    private RscAttrValue[] sourceNameAttrs = new RscAttrValue[LocatorResourceData.MAX_NUM_SOURCES];
    private RscAttrValue[] roundToAttrs     = new RscAttrValue[LocatorResourceData.MAX_NUM_SOURCES];
    private RscAttrValue[] displayUnitAttrs = new RscAttrValue[LocatorResourceData.MAX_NUM_SOURCES];
    private RscAttrValue[] directionUnitAttrs = new RscAttrValue[LocatorResourceData.MAX_NUM_SOURCES];

    
	//----------------------------created for pop-up this dialog: 2011-04-18    
	public LocatorEditDialog( Shell parentShell, INatlCntrsResourceData r, Boolean apply) {
		super(parentShell,r,apply); 
		
	}

	@Override
	public Composite createDialog( Composite topComp ) {
		
		fontSizeAttr    = editedRscAttrSet.getRscAttr("fontSize");

		sourceNameAttrs[0] = editedRscAttrSet.getRscAttr( "pos1LocatorSource" );
		sourceNameAttrs[1] = editedRscAttrSet.getRscAttr( "pos2LocatorSource" );
		sourceNameAttrs[2] = editedRscAttrSet.getRscAttr( "pos3LocatorSource" );
		sourceNameAttrs[3] = editedRscAttrSet.getRscAttr( "pos4LocatorSource" );
		sourceNameAttrs[4] = editedRscAttrSet.getRscAttr( "pos5LocatorSource" );
		
		roundToAttrs[0] = editedRscAttrSet.getRscAttr( "pos1RoundToNearest" );
		roundToAttrs[1] = editedRscAttrSet.getRscAttr( "pos2RoundToNearest" );
		roundToAttrs[2] = editedRscAttrSet.getRscAttr( "pos3RoundToNearest" );
		roundToAttrs[3] = editedRscAttrSet.getRscAttr( "pos4RoundToNearest" );
		roundToAttrs[4] = editedRscAttrSet.getRscAttr( "pos5RoundToNearest" );

		displayUnitAttrs[0] = editedRscAttrSet.getRscAttr( "pos1DisplayUnit" );
		displayUnitAttrs[1] = editedRscAttrSet.getRscAttr( "pos2DisplayUnit" );
		displayUnitAttrs[2] = editedRscAttrSet.getRscAttr( "pos3DisplayUnit" );
		displayUnitAttrs[3] = editedRscAttrSet.getRscAttr( "pos4DisplayUnit" );
		displayUnitAttrs[4] = editedRscAttrSet.getRscAttr( "pos5DisplayUnit" );

		directionUnitAttrs[0] = editedRscAttrSet.getRscAttr( "pos1DirectionUnit" );
		directionUnitAttrs[1] = editedRscAttrSet.getRscAttr( "pos2DirectionUnit" );
		directionUnitAttrs[2] = editedRscAttrSet.getRscAttr( "pos3DirectionUnit" );
		directionUnitAttrs[3] = editedRscAttrSet.getRscAttr( "pos4DirectionUnit" );
		directionUnitAttrs[4] = editedRscAttrSet.getRscAttr( "pos5DirectionUnit" );

		if( fontSizeAttr == null || 
				fontSizeAttr.getAttrClass() != Integer.class ) {
        	System.out.println("fontSizeAttr  is null or not of expected class Integer?");				
		}
		
		for( int p=0 ; p< LocatorResourceData.MAX_NUM_SOURCES ; p++ ) {
			if( sourceNameAttrs[p] == null || 
					sourceNameAttrs[p].getAttrClass() != String.class ) {
	        	System.out.println("sourceNameAttrs["+p+"] is null or not of expected class String?");				
			}
			if( roundToAttrs[p] == null || 
				roundToAttrs[p].getAttrClass() != Integer.class ) {
	        	System.out.println("roundToAttrs["+p+"] is null or not of expected class Integer?");				
			}
			if( displayUnitAttrs[p] == null || 
				displayUnitAttrs[p].getAttrClass() != String.class ) {
	        	System.out.println("displayUnitAttrs["+p+"] is null or not of expected class String?");				
			}
			if( directionUnitAttrs[p] == null || 
				directionUnitAttrs[p].getAttrClass() != String.class ) {
	        	System.out.println("directionUnitAttrs["+p+"] is null or not of expected class String?");				
			}
		}
		
        FormLayout layout0 = new FormLayout();
        topComp.setLayout( layout0 );

        Label fontSizeLbl = new Label( topComp, SWT.NONE  );
        fontSizeLbl.setText("Font Size");
        FormData fd = new FormData();
        fd.left = new FormAttachment( 10, 0 );
        fd.top = new FormAttachment( 0, 20 );
        fontSizeLbl.setLayoutData(fd);

        fontSizeCombo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        fd = new FormData();
        fd.left = new FormAttachment( 40, 0 );
        fd.top = new FormAttachment( fontSizeLbl, -2, SWT.TOP );
        fontSizeCombo.setLayoutData(fd);       

        Integer curFontSize = (Integer)fontSizeAttr.getAttrValue();
        for( int f=0 ; f<fontSizes.length ; f++ ) {
        	fontSizeCombo.add( fontSizes[f].toString() );

        	if( curFontSize == fontSizes[f] ) {
        		fontSizeCombo.select( f );
        	}
        }
        
        Label posLbl = new Label( topComp, SWT.NONE  );
        posLbl.setText("Position");
        fd = new FormData();
        fd.left = new FormAttachment( fontSizeLbl, 0, SWT.LEFT );
        fd.top = new FormAttachment( fontSizeLbl, 35, SWT.BOTTOM );        
        posLbl.setLayoutData(fd);

        positionCombo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);

        fd = new FormData();
        fd.left = new FormAttachment( 40, 0 );
        fd.top = new FormAttachment( posLbl, -2, SWT.TOP );
        positionCombo.setLayoutData(fd);       

        for( int p=1 ; p <= LocatorResourceData.MAX_NUM_SOURCES ; p++ ) { 
        	positionCombo.add( Integer.toString(p) );
        }
        
        positionCombo.select(0);
                        
        Label sourceNameLbl = new Label(topComp, SWT.NONE);
        sourceNameLbl.setText("Locator");
        fd = new FormData();
        fd.left = new FormAttachment( posLbl, 0, SWT.LEFT );
        fd.top = new FormAttachment( posLbl, 25, SWT.BOTTOM );        
        sourceNameLbl.setLayoutData(fd);

        dataSourceCombo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        fd = new FormData();
        fd.left = new FormAttachment( positionCombo, 0, SWT.LEFT );
        fd.right = new FormAttachment( 90, 0 );
        fd.top = new FormAttachment( sourceNameLbl, -3, SWT.TOP );        
        dataSourceCombo.setLayoutData(fd);
  
        Label roundingLbl = new Label(topComp, SWT.NONE);
        roundingLbl.setText("Round To ");
        fd = new FormData();
        fd.left = new FormAttachment(  posLbl, 0, SWT.LEFT );
        fd.top = new FormAttachment( sourceNameLbl, 25, SWT.BOTTOM );        
        roundingLbl.setLayoutData(fd);

        roundingCombo = new Combo( topComp, SWT.DROP_DOWN | SWT.READ_ONLY );
        fd = new FormData();
        fd.left = new FormAttachment( positionCombo, 0, SWT.LEFT );
        fd.top = new FormAttachment( roundingLbl, -3, SWT.TOP );
        roundingCombo.setLayoutData( fd );

        roundingCombo.setItems( ROUNDING_OPTIONS);
        
        Label distUnitsLbl = new Label(topComp, SWT.NONE);
        distUnitsLbl.setText("Units");
        fd = new FormData();
        fd.left = new FormAttachment(  posLbl, 0, SWT.LEFT );
        fd.top = new FormAttachment( roundingLbl, 25, SWT.BOTTOM );        
        distUnitsLbl.setLayoutData(fd);

        unitsCombo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        fd = new FormData();
        fd.width = 140;
        fd.left = new FormAttachment( positionCombo, 0, SWT.LEFT );
        fd.top = new FormAttachment( distUnitsLbl, 0, SWT.TOP );
        unitsCombo.setLayoutData(fd);
       
        Label dirUnitsLbl = new Label(topComp, SWT.NONE);
        dirUnitsLbl.setText("Direction\nUnits");
        fd = new FormData();
        fd.left = new FormAttachment( posLbl, 0, SWT.LEFT );
        fd.top = new FormAttachment( distUnitsLbl, 20, SWT.BOTTOM );        
        dirUnitsLbl.setLayoutData(fd);

        directionCombo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        fd = new FormData();
        fd.left = new FormAttachment( positionCombo, 0, SWT.LEFT );
        fd.top = new FormAttachment( dirUnitsLbl, 0, SWT.TOP );
        directionCombo.setLayoutData(fd);
        directionCombo.setItems( DIRECTIONUNIT_OPTIONS );
       
        
        fontSizeCombo.addSelectionListener(new SelectionAdapter() {			
			@Override	public void widgetSelected(SelectionEvent e) {							
				
            	fontSizeAttr.setAttrValue(  
            		fontSizes[ fontSizeCombo.getSelectionIndex() ] );
			}        	
        });
        
        positionCombo.addSelectionListener(new SelectionAdapter() {			
			@Override	public void widgetSelected(SelectionEvent e) {							
				seldPosition = positionCombo.getSelectionIndex();
            	String seldDataSrcName = (String)sourceNameAttrs[seldPosition].getAttrValue();

            	if( seldDataSrcName.isEmpty() ) {
            		seldDataSource = null;
            		setCombo( dataSourceCombo, NO_SOURCE );
            	}
            	else {
            		seldDataSource = availDataSources.get( seldDataSrcName );
            		setCombo( dataSourceCombo, seldDataSrcName );
            	}

				updateForSelectedDataSource();
			}        	
        });
        
        dataSourceCombo.addSelectionListener(new SelectionAdapter() {
        	@Override	
        	public void widgetSelected(SelectionEvent e) {
            	String seldDataSrcName = dataSourceCombo.getText();

            	if( seldDataSrcName.equals( NO_SOURCE ) ) {
            		seldDataSource = null;
            		sourceNameAttrs[seldPosition].setAttrValue( "" );
            	}
            	else {
            		seldDataSource = availDataSources.get( seldDataSrcName );
            		sourceNameAttrs[seldPosition].setAttrValue( seldDataSrcName );
            	}

        		updateForSelectedDataSource();
        	}        	
        });        
    	    	
        roundingCombo.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
        		roundToAttrs[seldPosition].setAttrValue(
        				(Integer)Integer.parseInt( roundingCombo.getText() ) );
			}        	
        });
        

        unitsCombo.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				// 
				displayUnitAttrs[seldPosition].setAttrValue( unitsCombo.getText() );
			}        	
        });
        
        directionCombo.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				directionUnitAttrs[seldPosition].setAttrValue( directionCombo.getText() );
			}        	
        });

        availDataSources = LocatorDataSourceMngr.getInstance().getAvailLocatorDataSources();
        
    	String[] sortList = availDataSources.keySet().toArray( new String[0] );
    	
    	Arrays.sort( sortList,  new Comparator<String>() {
    		public int compare(String l1, String l2) {
    			if( l1.equals(l2) ) {
    				return 0;
    			}
    			LocatorDataSource lds1 = availDataSources.get( l1 );
    			LocatorDataSource lds2 = availDataSources.get( l2 );
    			
    			if( lds1.getSourceType() == SourceType.LATLON ) {
    				return -1;
    			}
    			else if( lds2.getSourceType() == SourceType.LATLON ) {
    				return 1;
    			}
    			else return lds1.getSourceName().compareToIgnoreCase(
    					        lds2.getSourceName() );
    		}
		}); 
    	
    	dataSourceCombo.setItems( sortList );
    	dataSourceCombo.add( NO_SOURCE, 0 ); // None
        
    	String seldDataSrcName = (String)sourceNameAttrs[seldPosition].getAttrValue();
        
        for( int indx=1 ; indx<dataSourceCombo.getItemCount()-1 ; indx++ ) {
        	if( dataSourceCombo.getItem(indx).equals( seldDataSrcName ) ) {
        		dataSourceCombo.select( indx );
        	}
        }
        
        if( dataSourceCombo.getSelectionIndex() == -1 ) {
        	seldDataSource = null;
        }
        
    	if( seldDataSrcName.equals( NO_SOURCE ) ) {
    		seldDataSource = null;
    	}
    	else {
    		seldDataSource = availDataSources.get( seldDataSrcName );
    	}

		updateForSelectedDataSource();
        
    	return topComp;
    }
    
    
    private void updateForSelectedDataSource() {
    	
    	if( seldDataSource == null ) {
    		
        	roundingCombo.setEnabled( false );
        	unitsCombo.setEnabled( false );
        	directionCombo.setEnabled( false );        	
    	}
    	else {
    		if( seldDataSource.getSourceType() == SourceType.LATLON ) {    			
            	roundingCombo.setEnabled( false );
            	directionCombo.setEnabled( false );        	
            	
            	unitsCombo.setEnabled( true );
            	unitsCombo.setItems( LATLONUNIT_OPTIONS);
        	}
    		else if( seldDataSource.getSourceType() == SourceType.POINT ) {
    			roundingCombo.setEnabled( true );
    			unitsCombo.setEnabled( true );
            	directionCombo.setEnabled( true );
            	
            	unitsCombo.setItems( DISTANCEUNIT_OPTIONS );
    		}
    		else if( seldDataSource.getSourceType() == SourceType.BOUNDED_AREA ) {
    			roundingCombo.setEnabled( false );
    			unitsCombo.setEnabled( false );
            	directionCombo.setEnabled( false );
            	
            	unitsCombo.setItems( DISTANCEUNIT_OPTIONS );
    		}
    		
    		// if the data source has changed to or from a LATLON then reset the units
    		// to a valid value
    		//
    		String seldUnits = (String)displayUnitAttrs[seldPosition].getAttrValue();

    		if( !Arrays.asList( unitsCombo.getItems() ).contains(  seldUnits ) ) {
    			unitsCombo.select(0);
    			displayUnitAttrs[seldPosition].setAttrValue( unitsCombo.getText() );
    		}

    		setCombo( roundingCombo, ((Integer)roundToAttrs[seldPosition].getAttrValue()).toString() );
    		setCombo( unitsCombo, (String)displayUnitAttrs[seldPosition].getAttrValue() );
    		setCombo( directionCombo, (String)directionUnitAttrs[seldPosition].getAttrValue() );
    	}    	
    }
    
    private void setCombo( Combo combo, String sel ) {
    	
    	for(  int i=0 ; i<combo.getItemCount() ; i++ ) {
    		String s = combo.getItem(i);
    		if( sel.equals( s ) ) {
    			combo.select( i );
    			return;
    		}
    	}
    	combo.select(0);
    }
        
	@Override
	public void initWidgets() {
	}
    
	// allow to override	
	@Override
	protected void dispose() {
		super.dispose();
	}

}
