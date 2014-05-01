package gov.noaa.nws.ncep.viz.tools.aodt.ui;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

import gov.noaa.nws.ncep.viz.tools.aodt.natives.AODTv64Native;
import gov.noaa.nws.ncep.viz.tools.aodt.natives.odtdata;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

/**
 * AODT Dialog.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/25/09		150			M. Li		initial creation
 * 10/05/10		317			X. Guo		Create loop management to
 *                                      handle all three loop options
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class AODTHistFileMngDialog extends Dialog { 
	AODTv64Native odt;
	
    private Shell shell;
    private Font font;
    
    private List resultList = null;
    private Button printAll_button = null;
    private Button printSelected_button = null;
    private Button saveAll_button = null;
    private Button saveSelected_button = null;
    private Button deleteSelected_button = null;
    private Text saveSelected_txt = null;
    
    private Text comment_txt = null;
    private Button apply_button = null;
    
    private String AODT_DIR= LocalizationManager.getUserDir();
    private String hist_file = null;

    public AODTHistFileMngDialog(Shell parent) {
        super(parent);
    }
  
    public Object open() {
    	Shell parent = getParent();
    	Display display = parent.getDisplay();
    	shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
        shell.setText("AODT History File Management");
    	
     // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        shell.setLayout(mainLayout);
        shell.setLocation(0, 0);

        font = new Font(shell.getDisplay(), "Monospace", 8, SWT.BOLD);

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.pack();
        shell.open();
        
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        font.dispose();
    	
    	return null;
    }
    
    /**
     * closes the AODT Dialog
     */
    public void close() {
    	if ( shell != null ) shell.dispose();
    }
    
    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
    	odt = AODTv64Native.getInstance();
    	
    	createResultListControls();
    	createActionControls();
    	addSeparator();
    	createCloseButton();
    	
    	setHist(true);
    }
    
    private void createResultListControls() {
    	Composite comp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = 10;
        comp.setLayout(gl);
        
        Label title1 = new Label(comp, SWT.LEFT);
        title1.setText("                  --------Intensity-------  ---Tno Values--  -Tno/CI Rules-  -Temperature-                 ");
        Label title2 = new Label(comp, SWT.LEFT);
        title2.setText("           Time        Final/MSLPLat/Vmax   6hr 3hr Adj Ini   Cnstrnt  Wkng   Eye    Mean   Scene  EstRMW   Storm Location  Fix");
        Label title3 = new Label(comp, SWT.LEFT);
        title3.setText("   Date    (UTC)   CI  MSLP /BiasAdj/(kts)  Ave Ave Raw Raw    Limit   Flag  Region  Cloud  Type    (km)     Lat     Lon    Mthd");
        
        resultList = new List(comp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
        resultList.setLayoutData(new GridData(1000, 150));
        resultList.setFont(font);
        resultList.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				if (resultList.getSelectionCount() > 0) {
					String com = resultList.getSelection()[0];
					if (com.length() >= 128) {
						String comment = com.substring(128).trim();
						comment_txt.setText(comment);
					}
				}
			}
        });
        
    }
    
    /**
     * sets the history list on AODT History File Management window
     * @param set_file
     */
    public void setHist(boolean set_file) {
    	
    	
    	int ier = -1;
    	resultList.removeAll();
    	
    	/* Check the set file flag to see if we need to set history file. */
    	if(set_file){
    		odt.aodt.aodtv64_initialize();
    		hist_file = null;
    		hist_file = odt.getHistoryFile();
    		//System.out.println("HIST.. MANAGEMENT ..setHist..hist_file="+hist_file);
    		if(hist_file != null && hist_file.length() > 0) {
    			if (!hist_file.contains(AODT_DIR))hist_file = AODT_DIR + hist_file;
    			//System.out.println("HIST1111.. MANAGEMENT ..setHist..hist_file="+hist_file);
    			ier = odt.aodt.aodtv64_sethistoryfile(hist_file);
    			//System.out.println("HIST2222.. MANAGEMENT ..setHist..hist_file="+hist_file);
    			if ( ier < 0) {
    				System.out.println("ERROR:setHist--aodtv64_sethistoryfile "+ ier);
    				return;
    			}
    		} else {
    			return;
    		}
    	}
    	
    	//System.out.println("1111111111111111111111111 hist_file="+hist_file);
    	/* Get the history file name from the AODT library. */
    	hist_file = null;
    	hist_file = odt.aodt.aodtv64_gethistoryfile2();
    	if ( hist_file == null || hist_file.length() <= 0 ) {
    		System.out.println("Invalid history file!");
    		return;
    	}
    	
    	//System.out.println("setHist ...after aodtv64_gethistoryfile..hist_file= "+hist_file);
    	
    	/* Set the date/time for history record display purpose. */
    	odt.aodt.aodtv64_setdatetime(0, 0, "", "", 0);
    	
    	/* Set history records on History File Management Window. */
    	PointerByReference historyRec = new PointerByReference();
    	odt.aodt.aodtv64_historygetnextrec(0, historyRec);
    	
    	if (historyRec.getPointer().getPointer(0) == null) return;
    	odtdata hrec = new odtdata(historyRec.getPointer().getPointer(0));
    	hrec.read();
    	
    	String line = "";
    	odtdata.ByReference odtByRef = hrec.newByReference();
    	odtByRef.IR = hrec.IR;
    	odtByRef.nextrec = hrec.nextrec;
    	line = odt.aodt.aodtv64_historylistfmt2(odtByRef, -1, null).trim();
		if(line != null && line.length() > 0) {
			resultList.add(line);
		}
		
    	while(hrec.nextrec != null) {
    		line = odt.aodt.aodtv64_historylistfmt2(hrec.nextrec, -1, null).trim();
    		if(line != null && line.length() > 0) {
    			resultList.add(line);
    		}
    		
    		odt.aodt.aodtv64_historygetnextrec(1, historyRec);
    		hrec = new odtdata(historyRec.getPointer().getPointer(0));
    		hrec.read();
    	}
    	
    }
    
    /**
     *  Delete the selected records on History Management window.
     */
    private void delHist() {
    	if (resultList.getSelectionCount() <= 0) return;
    	
    	int ier = -1;
    	for (String s : resultList.getSelection()) {
    		if (s.length() < 16) continue;
    		
    		String[] items = s.split("\\s+");
    		String cdate = items[0];
        	int time = Integer.valueOf(items[1]);
        	
        	ier = odt.aodt.aodtv64_datetime(time, time, cdate, cdate, 1);
        	if (ier != 0) continue;
        	
        	IntByReference hdels = new IntByReference();
        	IntByReference hmods = new IntByReference();
        	ier = odt.aodt.aodtv64_historydeleterec(hdels, hmods);
    	}
    	
    	/* write updated history records to file. */
    	IntByReference hrecs = new IntByReference();
    	ier = odt.aodt.aodtv64_historywritefile(hrecs);
    	
    	/*
         * Set the updated history records on GUI.
         */
    	setHist( false );

    }
    
    
    private void createActionControls() {
    	Composite comp = new Composite(shell, SWT.NONE);
    	GridLayout gl = new GridLayout(2, true);
        comp.setLayout(gl);
        
        // Action Group
        Group action_comp = new Group(comp, SWT.NONE);
        action_comp.setLayout(new GridLayout(3, false));
        action_comp.setText("Action");
        
        printAll_button = new Button(action_comp, SWT.PUSH);
        printAll_button.setText("Print All ");
        printAll_button.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				printHistoryFile(false);
			}
        });
        
        printSelected_button = new Button(action_comp, SWT.PUSH);
        printSelected_button.setText("Print  Selected");
        printSelected_button.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				printHistoryFile(true);
			}
        });
        
        
        deleteSelected_button = new Button(action_comp, SWT.PUSH);
        deleteSelected_button.setText("Delete Selected and Correct Remaining Times");
        deleteSelected_button.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			delHist();
       	}});
        
        saveAll_button = new Button(action_comp, SWT.PUSH);
        saveAll_button.setText("Save All");
        saveAll_button.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			saveToFile(false);
       	}});
        
        
        saveSelected_button = new Button(action_comp, SWT.PUSH);
        saveSelected_button.setText("Save Selected");
        saveSelected_button.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			saveToFile(true);
       	}});
        
        saveSelected_txt = new Text( action_comp, SWT.SINGLE | SWT.BORDER);
        saveSelected_txt.setLayoutData(new GridData(295, SWT.DEFAULT));
        
        // Comment group
        Group group = new Group(comp, SWT.NONE);
        group.setLayout(new GridLayout(1, true));
        group.setText("Comment");
        
        // Comment Text widget
        comment_txt = new Text( group, SWT.SINGLE | SWT.BORDER);
        comment_txt.setLayoutData(new GridData(380, SWT.DEFAULT));
        
        // Separator
        GridData gd1 = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(group, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd1);
        
        // Apply button
        apply_button = new Button(group, SWT.PUSH);
        apply_button.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true, false));
        apply_button.setText("     Apply    ");
        apply_button.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			addComment();
       	}});
    	
    }
    
    
    private void createCloseButton() {
    	Composite comp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = 400;
        comp.setLayout(gl);
        
        Button close_button = new Button(comp, SWT.PUSH);
        //close_button.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true, false));
        close_button.setText("                     Close                     ");
        close_button.addSelectionListener(new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
        
    }
    
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }
    

    public boolean isOpen() {
    	return shell != null && !shell.isDisposed();
    }

    public void clearFields() {
      
    }
    
    private void messageBox(String msg) {
    	MessageBox messageBox = new MessageBox(new Shell(),
                SWT.ICON_WARNING | SWT.OK);
        messageBox.setText("Message");
        messageBox.setMessage(msg);
        messageBox.open();
    }
    
    private void saveToFile(boolean saveSelected) {
    	String file = saveSelected_txt.getText().trim();
			if (file != null && file.length() > 0) {
				file = AODT_DIR + file;
				try {
					BufferedWriter output = new BufferedWriter(new FileWriter(file));
					String[] result = null;
					if (saveSelected) {
						result = resultList.getSelection();
					} else {
						result = resultList.getItems();
					}
					
					if (result == null || result.length <= 0) {
						messageBox("No items or no selected items to save!");
						return;
					}
					
					for (String s : result) {
					    output.write(s + "\n");
					}
					output.write("\n");
					output.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			else {
				messageBox("Please specify a file name!");
			}
    }
    
    private void printHistoryFile(boolean printSelected) {
    	String[] result = null;
		if (printSelected) {
			result = resultList.getSelection();
		} else {
			result = resultList.getItems();
		}
		
		if (result == null || result.length <= 0) {
			messageBox("No items or no selected items to print!");
			return;
		}
		
    	PrintDialog printDialog = new PrintDialog(shell, SWT.NONE);
		printDialog.setText("Print");
		PrinterData printerData = printDialog.open();
		if (!(printerData == null)) {
			Printer p = new Printer(printerData);
			p.startJob("PrintJob");
			p.startPage();
			Rectangle trim = p.computeTrim(0, 0, 0, 0);
			Point dpi = p.getDPI();
			int leftMargin = dpi.x + trim.x;
			int topMargin = dpi.y / 2 + trim.y;
			GC gc = new GC(p);
			Font font = gc.getFont();
			
			int heightOffset = 0;
			for (String printText : result) {
				printText = printText + "\n";
				heightOffset += 2 * font.getFontData()[0].getHeight();
				//Point extent = gc.stringExtent(printText);
				gc.drawString(printText, leftMargin, topMargin
						+ heightOffset);
			}
			
			p.endPage();
			gc.dispose();
			p.endJob();
			p.dispose();
		}
    }
    
    /**
     * The function gets the comment text entered by the user, and append   
     * it to the history file record and to the file box line. 
     */
    private void addComment() {
    	if (resultList.getSelectionCount() <= 0) return;

    	String comment = comment_txt.getText().trim();
    	if (comment == null || comment.length() <= 0) {
    		comment = "";
    	}
    	
    	int ier = -1;
    	for (String s : resultList.getSelection()) {
    		if (s.length() < 16) continue;
    		
    		String[] items = s.split("\\s+");
    		String cdate = items[0];
    		int time = Integer.valueOf(items[1]);

    		ier = odt.aodt.aodtv64_datetime(time, time, cdate, cdate, 1);
    		if (ier != 0) continue;

    		IntByReference modrec = new IntByReference();
    		ier = odt.aodt.aodtv64_historyaddcomment(comment, modrec);
    	}

    	/* write updated history records to file. */
    	IntByReference hrecs = new IntByReference();
    	ier = odt.aodt.aodtv64_historywritefile(hrecs);

    	/*
    	 * Set the updated history records on GUI.
    	 */
    	setHist( false );

    }

} 