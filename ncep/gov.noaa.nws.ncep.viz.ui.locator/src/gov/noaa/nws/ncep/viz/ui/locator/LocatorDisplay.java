package gov.noaa.nws.ncep.viz.ui.locator;

//import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;
import gov.noaa.nws.ncep.viz.ui.locator.resource.LoadEnvelopes;
import gov.noaa.nws.ncep.viz.ui.locator.resource.Locator;
import gov.noaa.nws.ncep.viz.ui.locator.resource.LocatorBoundsResource;
import gov.noaa.nws.ncep.viz.ui.locator.resource.LocatorTableReader;
import gov.noaa.nws.ncep.viz.ui.locator.resource.LocatorTool;

import java.io.IOException;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.StatusLineLayoutData;
//import org.eclipse.jface.action.StatusLineLayoutData;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
//import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Contribution item added to the status bar which displays the locator
 * information. The locator display can be set by locator_tbl.xml, which
 * is located at "$VIZ_HOME/nmapTable/locator/".
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/2008  	22    		M. Li      Initial Creation
 * 01/2009		22			M. Li	   get locator table from NmapCommon 
 * 02/2009		64			M. Li	   Add locator editing	
 * 03/2009	    65			M. Li	   Add multiple locators
 * 03/2009		86			M. Li	   Put the locator editor on top of CAVE
 * 11/06/09     183         Q. Zhou      reduced the locator space to put the fading scale    
 * 11/24/09     138         Greg Hull    LocatorDefaults options 
 * 03/02/10      222        J. Zeng    work with DB
 * 11/08/10      229        Chin Chen   Add SFSTATION locator for NSHARP 
 * </pre>
 * 
 * @author M. Li
 * @version 1
 */
public class LocatorDisplay extends ContributionItem {
    /** Singleton instance */
    private static LocatorDisplay instance;
    
    /** The latitude longitude displayed */
    private Coordinate theLatLon;
    
    /** The display change tolerance */
    private static float TOL = 0.01f;
    
    //private static LoadEnvelopes loadEnv;
            
    private Composite comp;

    private Combo combo = null;
    
    private boolean latlonOn = true;

	private String[] locatorNames = null;
		
	private int idx = 0;
	
	private int lastIdx = 0;
	
	private int itemNum;
	
	private final String EDIT = "Edit...";
	
	private final String MULTIPLE = "Multiple Locators...";
	
    /** Label used to display the current workstation time */
    private Label theTextArea;
    
    private Font font = new Font(Display.getCurrent(), "Monospace", 10,
            SWT.NORMAL);

    private LocatorBoundsResource bndRsc;
    
    private LocatorTableReader locTblReader = null;
    
    private List<Locator> locatorList = null;
    
    private LocatorEditDialog locatorEditDialog = null;
    
    private MultipleLocatorsDialog multipleLocators = null;
    private int sfstnIndex;
    private String SFSTATION_NAME = "SFSTATION";
    private String displayedStr= "";

	public String getDisplayedStr() {
		return displayedStr;
	}

	/**
     * Constructor
     */
    public LocatorDisplay(String ID) {
        super(ID);
        instance = this;
        theLatLon = new Coordinate(-999.0f, -999.0f);    
        new LoadEnvelopes();
    }
    
    /**
     * Get active instance
     * 
     * @return
     */
    public static LocatorDisplay getInstance() {
            return instance;
    }

     /**
     * Populates the locator on the bottom bar
     */
    @Override
    public void fill(Composite parent) {
    	Layout lo = parent.getLayout();
    	
        comp = new Composite(parent, SWT.NONE);
        comp.setSize(200, 50);
        StatusLineLayoutData slLayoutData = new StatusLineLayoutData();
//        slLayoutData.heightHint = 30;
//        slLayoutData.widthHint = 300;
        comp.setLayoutData( slLayoutData );
//        comp.setBackground( Display.getDefault().getSystemColor(
//                SWT.COLOR_RED));
        GridLayout gl = new GridLayout( 2 , false );        
        comp.setLayout( gl );

        /*
         * Create Locator source list
         */
        combo = new Combo(comp, SWT.POP_UP|SWT.READ_ONLY);            
             
      	locTblReader = new LocatorTableReader(
      			LocalizationManager.getInstance().getFilename("locatorTable"));
        
        itemNum = 0;
         
		try {
			locatorList = locTblReader.getLocatorTable();
			
			locatorNames = new String[locatorList.size()+2];
			
			for(Locator itm : locatorList){
				combo.add(itm.getLocatorName());
				locatorNames[itemNum] = itm.getLocatorName();
				if(itm.getLocatorName().equals(SFSTATION_NAME)){
					sfstnIndex = itemNum;
				}
				itemNum++;
	        }    
		} catch (JAXBException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		combo.add(EDIT);	
		locatorNames[itemNum] = EDIT;
		itemNum++;
		combo.add(MULTIPLE);	
		locatorNames[itemNum] = MULTIPLE;
		itemNum++;
		
        combo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
            	idx = combo.getSelectionIndex();        	
            	if (idx < itemNum - 2) {
            		lastIdx = idx; 
            	} 
            	
            	Shell theShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
            	
            	if ( idx == itemNum - 2 ) { //Edit...       
            		combo.setText(locatorNames[lastIdx]);
            		if (locatorEditDialog== null) { 
            		    locatorEditDialog = new LocatorEditDialog( theShell, locatorList, lastIdx);
            		    locatorEditDialog.open();
            		} else {
            			locatorEditDialog.setLocatorSelection(lastIdx);
            			if (!locatorEditDialog.isOpen()) { 
            				locatorEditDialog.open();
            				
            			}	
            		}
            	}
            	else if (idx == itemNum - 1 ) { // Multiple Locators...
            		combo.setText(locatorNames[lastIdx]);
            		if (multipleLocators == null) { 
            			if (locatorEditDialog != null) {
            				multipleLocators = new MultipleLocatorsDialog( theShell, 
            					locatorEditDialog.getUpdateLocList());
            			} else {
            				multipleLocators = new MultipleLocatorsDialog( theShell, 
            						locatorList);
            			}
            		}
           			if (multipleLocators != null) {
           				multipleLocators.open();
           			}
            		
            		
            	} else {
            		
            		if ( combo.getText().compareTo("LATLON") == 0 ){
                		latlonOn = true;
                	} else {
                		latlonOn = false;
                		bndRsc = new LocatorBoundsResource();
                		
                	}	
            		
            		if (locatorEditDialog != null) {
                    	locatorEditDialog.setLocatorSelection(idx);
                    }
            	}	
            }

        });
               
        // Set default text in combo's text field
        if ( idx != combo.getSelectionIndex()) {
        	combo.setText(locatorNames[lastIdx]);
        }

        /*
         * Create the locator display text widget.
         */
        theTextArea = new Label(comp, SWT.BORDER);
        theTextArea.setFont(font);                   
        theTextArea.setLayoutData(new GridData(160, 20));
        
        
        theTextArea.setForeground(Display.getDefault().getSystemColor(
                SWT.COLOR_BLACK));
        theTextArea.setBackground(Display.getDefault().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));            
        
        update();
        
        parent.layout();
        parent.pack();     
    }

   
    /**
     * Set the current latitude longitude value
     * 
     * @param ll
     *            the latitude longitude to display
     */
    public static void setPosition(Coordinate ll) {
            //if (ll == null || (Math.abs(ll.y - theLatLon.y) < TOL && Math.abs(ll.x - theLatLon.x) < TOL))
                    //return;
            //theLatLon = ll; 
    	gov.noaa.nws.ncep.viz.ui.locator.resource.LocatorResource lr=gov.noaa.nws.ncep.viz.ui.locator.resource.LocatorResource.getLocatorResource();
    	if(lr!=null)	lr.setCoordinate(ll);            
            //update();
    }

  
    /**
     * Get the current latitude longitude value
     * 
     * @param ll
     *            the latitude longitude to display
     */
    public Coordinate getPosition() {
    	return theLatLon;                            
    }
    
    /**
     * Update the display
     */
    @Override
    public void update() {
        Display.getDefault().syncExec(new Runnable() {
       // VizApp.runAsync(new Runnable() {

            @Override
            public void run() {    
            	String latlonUnit = null;
            	if (locatorEditDialog != null) {
            		latlonUnit = locatorEditDialog.getUpdateLocList().
            				     get(lastIdx).getDisplayOptions().getLatLonUnit();
            	}
            	
                if (latlonOn) { // LatLon
                	theTextArea.setText(LocatorTool.formatCoordinate(theLatLon, latlonUnit));
                }
                else { // Edit...
                		if (locatorEditDialog != null) 
                			bndRsc.setCurrentLocator(locatorEditDialog.getUpdateLocList().get(lastIdx));
                		else
                			bndRsc.setCurrentLocator(locatorList.get(lastIdx));
                		
                		String str = null;
						try {
							str = bndRsc.getBoundsValue(theLatLon);
							
							if (str != null){
								theTextArea. setText(str);
								displayedStr = str;
							}
							else  {
								theTextArea.setText("--");		
								displayedStr = "--";
							}
						} catch (VizException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
							str = new String("--");
						} catch (RuntimeException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						} catch (IOException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						} 
						
						
                }
                
                theTextArea.pack();
                
                if (multipleLocators != null && multipleLocators.isOpen()) {
                	try {
						multipleLocators.displayOutput();
					} catch (RuntimeException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
       			}
            }

        });
    }
    
    public MultipleLocatorsDialog getMultipleLocators() { 
    	return multipleLocators;
    }
    
    public void setLocatorDfltsList( List<Locator> list ) {
        locatorList = list;

        if( locatorEditDialog != null ) {
        	locatorEditDialog.setLocatorDfltsList( locatorList );
        }
        if( multipleLocators != null ) {
        	multipleLocators.setLocatorDfltsList( locatorList );
        }

        locTblReader.writeLocatorTable( locatorList );
    }

    public List<Locator> getLocatorList( ) {
        return locatorList;
    }
    public void setSfStationLocatorToCurrent() {
    	combo.setText(locatorNames[sfstnIndex]);
    	lastIdx = sfstnIndex;
    	latlonOn = false;
		bndRsc = new LocatorBoundsResource();
    }
    public void setLatLonLocatorToCurrent() {
    	combo.setText(locatorNames[0]);
    	lastIdx = 0;
    	latlonOn = true;
    }
}

