package gov.noaa.nws.ncep.viz.ui.locator;

import gov.noaa.nws.ncep.viz.ui.locator.resource.Locator;
import gov.noaa.nws.ncep.viz.ui.locator.resource.LocatorBoundsResource;
import gov.noaa.nws.ncep.viz.ui.locator.resource.LocatorTool;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class displays multiple locators information in a detached dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * March, 2009  	65        	M. Li    	Initial creation 
 * Nov,  2009      138        G. Hull    added setting of default options
 * Mar.  2010      222        J. Zeng    work with DB
 *  
 * </pre>
 * 
 * @author mli
 * @version 1.0
 * 
 */

public class MultipleLocatorsDialog extends Dialog {

	/**
     * Dialog shell;
     */
    private Shell shell;
    
    /**
     * SWT display component.
     */
    private Display display;

    /**
     * Label font.
     */
    private Font labelFont;
    
    private final int HEIGHT_PER_LINE = 17;
    
    private final int NAME_COLUMN_WIDTH = 120;
    
    private final int VALUE_COLUMN_WIDTH = 180;
    
    private final int LINE_NUMBER = 8;
    
    private Text outputNames = null;
    
    private Text outputValues = null;
    
    private List<Locator> dfltLocatorList = null;
    
    private List<Locator> updateLocList = null;
    
    private Composite outputComp;
    
    private Menu menu;
    
    private Menu locMenu = null;
    
    private LocatorBoundsResource[] bndRsc;
    
	protected MultipleLocatorsDialog(Shell parentShell, List<Locator> list) {
		super(parentShell);
		// TODO Auto-generated constructor stub
		
		if (dfltLocatorList == null) dfltLocatorList = list;
		
		if (updateLocList == null) {
		 	updateLocList = new ArrayList<Locator>();
			for (Locator loc : list) {
				updateLocList.add(loc.copy());
			}
		}
	}

    /**
     * Open method used to display the multiple locators dialog. *
     * 
     * @return Return object (can be null).
     */
    public Object open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        shell.setText("Multiple Locators Window");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        shell.setLayout(mainLayout);
        Rectangle rect = parent.getBounds();
        Point pt = new Point(rect.x, rect.y + (int)(0.5 * rect.height));
        shell.setLocation(pt.x, pt.y);
        
        labelFont = new Font(shell.getDisplay(), "Arial", 10, SWT.BOLD);

        // Initialize all of the menus, controls, and layouts
        initializeComponents();
        
        
        // Pack the controls and open the display.
        shell.pack();
        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        labelFont.dispose();

        return null;
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
    	
    	createLocatorSelectionMenu();
    	createLocatorOutput();
    	addSeparator();    	
    	createCloseButton();
    }
    
    
    /**
     * Create the locator Selection controls.
     */
    
    private void createLocatorSelectionMenu() {
    	// Create menu bar
    	menu = new Menu(shell, SWT.BAR);
    	
    	// Create items in the bar menu
    	MenuItem locatorSelectionItem = new MenuItem(menu, SWT.CASCADE);
    	locatorSelectionItem.setText("Locator Selections");

    	// Create the locatorSelectionItem's dropdown menu
    	locMenu = new Menu(menu);
    	locatorSelectionItem.setMenu(locMenu);

    	// Create all the items in the locatorSelection dropdown menu
    	bndRsc = new LocatorBoundsResource[updateLocList.size()];
    	
    	for (int i = 0; i < updateLocList.size(); i++) {
    		MenuItem item = new MenuItem(locMenu, SWT.CHECK);
    		item.setText(updateLocList.get(i).getLocatorName());
    		item.setSelection(updateLocList.get(i).getDisplayOptions().isIsMultipleDefault());
    		
    		if (updateLocList.get(i).getDisplayOptions().isIsMultipleDefault()) { 
    			if (bndRsc[i] == null)
    				bndRsc[i] = new LocatorBoundsResource(updateLocList.get(i));
    		}
    		else {
    			bndRsc[i] = null;
    		}
    		
    		item.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {
                	int idx = 0;
                	for(int i = 0; i < updateLocList.size(); i++) {
                		if (event.getSource().toString().indexOf(
                				updateLocList.get(i).getLocatorName()) >= 0) {
                			idx = i;
                			break;
                		}
                	}
                	boolean check = locMenu.getItem(idx).getSelection();
                	updateLocList.get(idx).getDisplayOptions().setIsMultipleDefault(check);
                	
                	if (check && bndRsc[idx] == null) {
                		bndRsc[idx] = new LocatorBoundsResource(updateLocList.get(idx));
                	}
                	
                }
            });
   		
    	}
    	
    	shell.setMenuBar(menu);
    }


    /**
     * Create the locators results display area.
     */
    private void createLocatorOutput() {
        outputComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        outputComp.setLayout(gl);

        new Label(outputComp, SWT.NONE).setText("        Locators");
        new Label(outputComp, SWT.NONE).setText("             Values");
        
        outputNames = new Text(outputComp, SWT.MULTI | SWT.READ_ONLY | SWT.BORDER | SWT.V_SCROLL);
        outputNames.setLayoutData(new GridData(NAME_COLUMN_WIDTH, LINE_NUMBER * HEIGHT_PER_LINE));
        outputValues = new Text(outputComp, SWT.MULTI | SWT.READ_ONLY | SWT.BORDER | SWT.V_SCROLL);
        outputValues.setLayoutData(new GridData(VALUE_COLUMN_WIDTH, LINE_NUMBER * HEIGHT_PER_LINE));
        
    }

 
    /**
     * Add a horizontal separator to the display.
     */
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }
    
    /**
     * Create the default and close button.
     */
    private void createCloseButton() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        centeredComp.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        
        Button defaultBtn = new Button(centeredComp, SWT.NONE);
        defaultBtn.setText("Default");
        defaultBtn.setLayoutData(gd);
        defaultBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	
            	updateLocList = new ArrayList<Locator>();
            	for(int i = 0; i <dfltLocatorList.size(); i++){
            		updateLocList.add(dfltLocatorList.get(i).copy());
            		
            		if (dfltLocatorList.get(i).getDisplayOptions().isIsMultipleDefault()) {
            			locMenu.getItem(i).setSelection(true);
            			bndRsc[i] = new LocatorBoundsResource(dfltLocatorList.get(i));
            		}
            		else {
            			locMenu.getItem(i).setSelection(false);
            			bndRsc[i] = null;
            		}
            	}
            }
        });
        
        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }
    
    public void setUpdateLocList(List<Locator> locList) {
    	updateLocList = null;
    	updateLocList = new ArrayList<Locator>();
    		
    	for(int i = 0; i < locList.size(); i++){
    		// Update the locators from the edit...
    		updateLocList.add(locList.get(i).copy());
    		
    		// Update shapefile resource
    		if (bndRsc[i] != null) {
    			bndRsc[i] = new LocatorBoundsResource(updateLocList.get(i));
    		}
    		
    		//Update locator selection
    		if (isOpen()) updateLocList.get(i).getDisplayOptions().setIsMultipleDefault(
    				locMenu.getItem(i).getSelection());
    		
    	}
    }
    
    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }
    
    public void displayOutput() throws RuntimeException, IOException {
    	//utputList.refresh();
    	Coordinate coordinate = LocatorDisplay.getInstance().getPosition();
        String locNames = null;
    	String locValues = null;
    	for(int i = 0; i < updateLocList.size(); i++) {
    		
    		if (updateLocList.get(i).getDisplayOptions().isIsMultipleDefault()) {
    			String s = null;
    			if (updateLocList.get(i).getLocatorName().equals("LATLON")) {
    				s = LocatorTool.formatCoordinate(coordinate, 
    						updateLocList.get(i).getDisplayOptions().getLatLonUnit());
    			}
    			else {
					try {
						s = bndRsc[i].getBoundsValue(coordinate);
					} catch (VizException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
						s = new String("Unavailable");
					}
    			}
    			
    			if (locNames == null) {
    				locNames = updateLocList.get(i).getLocatorName()+"\n";
    				locValues = s + "\n";
    			}
    			else {
    				locNames = locNames.concat(updateLocList.get(i).getLocatorName())+"\n";
    				locValues = locValues.concat(s)+"\n";
    			}
    		}
    	}
    	
    	if (locValues == null) {
    		locValues = "";
    		locNames  = "";
    	}
    	outputNames.setText(locNames);
    	outputValues.setText(locValues);
    }
    
    public void setLocatorDfltsList( List<Locator> dfltsList ) {
        dfltLocatorList = dfltsList;
    }

}
