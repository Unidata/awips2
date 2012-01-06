package gov.noaa.nws.ncep.viz.tools.cursor;

import gov.noaa.nws.ncep.viz.tools.cursor.CursorTypes.CursorType;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * This class provides cursor selection and editing in 
 * National Centers perspective.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 2009  	109        M. Li    	Initial creation. 
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 * 
 */

public class CursorSelectDialog extends Dialog {
	/**
     * Dialog shell.
     */
    private Shell shell;
	
    Display display; 
    /**
     * Return object.
     */
    private final Boolean returnValue = false;
    
    private NCCursors ncCursor = null;
    
    private List<CursorReference> curRefList = null;
    
    private List<CursorType> cursorList = null;
    
    private String[] CurColors = null;
    
    private Combo curRefComb = null;
    
    private Combo curTypeComb = null;
    
    private Combo curColorComb = null;
    
    private static int currentCurRef = 0;
    
    private int[] curTypeSet = null;
    
    private int[] curColorSet = null;
    
    
    
	protected CursorSelectDialog(Shell parentShell) {
		super(parentShell);
		ncCursor = new NCCursors();
	}
	
	public Object open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
        shell.setText("Cursor Select and Edit");
        
		
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        getCursorAttributes();
        initializeComponents();

        shell.pack();
        shell.open();
        
        //parent.setCursor(arrowCursor);
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        return returnValue;
    }
    
    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
    	createCursorRefControls();
    	addSeparator();
    	createCursorTypeControls();
    	addSeparator();
    	createCloseButton();
    	//initialize();
    	update();
    }
    
    private void createCursorRefControls() {
    	Composite comp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        comp.setLayout(gl);
        new Label(comp, SWT.NONE).setText("CURSOR REF.       ");
        curRefComb = new Combo(comp, SWT.DROP_DOWN | SWT.READ_ONLY);
        if (curRefList != null) {
        	for (int i = 0; i < curRefList.size(); i++) {
        		curRefComb.add(curRefList.get(i).getReferenceName(), 
        				curRefList.get(i).getReferenceIndex());
        	}
        }
        
        curRefComb.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
				
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				currentCurRef = curRefComb.getSelectionIndex();
				update();
			}
        	
        });
    }
    
    private void createCursorTypeControls() {
    	Composite comp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        comp.setLayout(gl);
    	
        new Label(comp, SWT.NONE).setText("CURSOR TYPE   ");
        curTypeComb = new Combo(comp, SWT.DROP_DOWN | SWT.READ_ONLY);
        if (cursorList != null) {
        	for (int i = 0; i < cursorList.size(); i++) {
        		curTypeComb.add(cursorList.get(i).getCursorName());
        	}
        }
        
        curTypeComb.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
				
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				curTypeSet[currentCurRef] = curTypeComb.getSelectionIndex();
			}
        	
        });
        
        Label label = new Label(comp, SWT.NONE);
        label.setText("CURSOR COLOR  ");
        
        curColorComb = new Combo(comp, SWT.DROP_DOWN | SWT.READ_ONLY);
        curColorComb.setItems(CurColors);
        
        curColorComb.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				curColorSet[currentCurRef] = curColorComb.getSelectionIndex();
			}
        	
        });
    }
    
    private void createCloseButton() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, true);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        centeredComp.setLayoutData(gd);

        Button ok_btn = new Button(centeredComp, SWT.NONE);
        ok_btn.setText("OK");
        ok_btn.setLayoutData(gd);
        ok_btn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	ncCursor.setCursorTypeColorIdx(curTypeSet, curColorSet);
            	
            	Cursor cursor = NCCursors.getCursor(display, NCCursors.CursorRef.DEFAULT);
            	getParent().setCursor(cursor);
            	
            	shell.dispose();
            }
        });
        
        
        Button default_btn = new Button(centeredComp, SWT.NONE);
        default_btn.setText("Defaults");
        default_btn.setLayoutData(gd);
        default_btn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	setDefault();
            	update();
            }
        });
        
        
        
        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	//if (!IsToPlotLogos()) removeLogosResource();
                shell.dispose();
            }
        });
    }
    
    
    private void update() {
    	curRefComb.select(currentCurRef);
    	curTypeComb.select(curTypeSet[currentCurRef]);
    	curColorComb.select(curColorSet[currentCurRef]);
    }
    
    private void setDefault() {
    	currentCurRef = 0;
    	curTypeSet = NCCursors.getCursorTypeIdx(true);
    	curColorSet = NCCursors.getCursorColorIdx(true);
    }
    
    
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }
    
    private void getCursorAttributes() {
    	
    	if (curRefList == null) {
    		curRefList = ncCursor.getCursorRefList();
    	}
    	
    	if ( cursorList == null) {
    		cursorList = ncCursor.getCursorTypeList();
    	}
    	
    	if (CurColors == null) {
    		CurColors = ncCursor.getCursorColorOptions();
    	}
    	
    	if (curTypeSet == null ) {
    		curTypeSet = new int[curRefList.size()];
    		curTypeSet = NCCursors.getCursorTypeIdx(false);
    	}
    	
    	if (curColorSet == null) {
    		curColorSet = new int[curRefList.size()];
    		curColorSet = NCCursors.getCursorColorIdx(false);
    	}
    }

    
    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }

    /*
    public Cursor getCursor(Display d, NCCursors.CursorRef curRef) {
    	// Get cursor image file name
    	int refIndex = curRef.ordinal();
    	String imageFile = NmapCommon.getCursorImageDir() + File.separator
    		+ cursorList.get(curTypeSet[refIndex]).getImageFile();
    	
    	// Get hotSpot X, & Y
    	int x = 0;
    	int y = 0;
    	switch (curTypeSet[refIndex]) {
    	case 2:
    		x = 8;
    		y = 9;
    		break;
    	case 3:
    		x = 15;
    		y = 16;
    		break;
    	case 4:
    		x = 9;
    		y = 9;
    		break;
    	case 5:
    		x = 17;
    		y = 17;
    		break;
    	}

    	// Change cursor color
    	ImageData image = new ImageData(imageFile);
    	if (curColorSet[refIndex] > 0) {
    		for(int i = 0; i < image.width; i++) {
    			for(int j = 0; j < image.height; j++) {
    				if (image.getPixel(i,j) != 0)
    					image.setPixel(i, j, colors[curColorSet[refIndex]]);
    			}
    		}
    	}
    	
    	Cursor cursor = new Cursor(d, image, x, y);
    	
    	return cursor;
    }
    
    */
}
