package ohd.hseb.mpe.window;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
//import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
//import java.awt.event.ComponentAdapter;
//import java.awt.event.ComponentEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;

import javax.swing.AbstractListModel;
import javax.swing.ComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
//import javax.swing.ListSelectionModel;
import javax.swing.event.ListDataListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import ohd.hseb.db.DbTable;
import ohd.hseb.mpe.util.MPEProductDescriptor;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptorManager;
import ohd.hseb.util.gui.jtable.JTableManager;
import ohd.hseb.util.gui.jtable.JTableModel;

public class GageTableFrame extends JFrame implements Runnable
{
    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final String gageTableSettingFileHeader="#GageTableSettings File\n";

	private static boolean _isRunning = false;
    private static Thread _thread = null;

    private GageTableDataManager _dataManager;
    private JTableManager _tableManager;
    private FileLogger _fileLogger;  
    
    private final int _valueColumnWidth = 55;
 //   private final int _maxWindowWidth = 1250;
    
    private String _settingsFile = null;
    JComboBox _selectGridList = null;
    String [] _gridDataName = null;
    int _selected_index = DbTable.getNullInt();

    public GageTableFrame(FileLogger fileLogger,
            GageTableDataManager dataManager)
    {
        setDataManager(dataManager);
        setFileLogger(fileLogger);
    }

    static private void gageTableClosingAction(GageTableFrame gageFrame,
            String inputFilePath, String editedFilePath, FileLogger logger)
    {
        File inputFile = new File(inputFilePath);
        File editedFile = new File(editedFilePath);

        // Delete the input file.
        inputFile.delete();

        // If it doesn't exist, touch the edited gage output file.
        // This file contains the edited gage data. It also acts
        // as a sentinel to indicate that the Gage Table has been
        // closed.
        try
        {
            editedFile.createNewFile();
        }
        catch (IOException e)
        {
            logger.log(e.getMessage());
        }

        if (_isRunning == true)
        {
            _isRunning = false;            
            _thread.stop();
            
        }

        if (gageFrame != null)
        {
            gageFrame.dispose();
        }
    }

    public void showGageTable() throws IOException, ParseException
    {
        String header = "GageTableFrame.showGageTable(): ";
        
        JButton saveButton = null;
        JButton cancelButton = null;
        JLabel searchLabel = null;
        JTextField searchTextField = null;
        JPanel gageTablePanel = null;
        JPanel searchPanel = null;
       // JPanel buttonPanel = null;
        JScrollPane tableScrollPane = null;
        JLabel selectGridLabel = null;
       
       
        System.out.println(header + " 1 ");
                       
        Container container = getContentPane();

        GridBagConstraints constraints = new GridBagConstraints();

        saveButton = new JButton("Save");
        cancelButton = new JButton("Cancel");
        searchLabel = new JLabel("Search for LID:");
        searchTextField = new JTextField(5);
        gageTablePanel = new JPanel();
        searchPanel = new JPanel();
       // buttonPanel = new JPanel();
        selectGridLabel = new JLabel("      Select Grid Field Compared With Gage:");
        
               
        SimpleDateFormat utcSimpleDateFormat = new SimpleDateFormat(
                "HH'Z' MMMMM dd, yyyy");
        String productDate;

        utcSimpleDateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));

        System.out.println(header + " 2 ");
                
        container.setLayout(new GridBagLayout());
        gageTablePanel.setLayout(new GridLayout(1, 1));
        searchPanel.setLayout(new FlowLayout(FlowLayout.LEFT) );
     //   buttonPanel.setLayout(new BorderLayout());
        
        
        System.out.println(header + " 2.1 ");
        
        // load rowDataList
        List<GageTableRowData> rowDataList = getDataManager().readData();
                       
        System.out.println(header + " 2.2 ");
                
        // define the directory and name of the office-wide settings file including the 
        // the table column header names and sorting, and table column length etc info
        
        //String settingsFileDir = getDataManager().getEditedGageFilePath(); 
        
        AppsDefaults appsDefaults = new AppsDefaults();       
        String settingsFileDir = appsDefaults.getToken("rfcwide_input_dir",
                        "/awips/hydroapps/precip_proc/local/data/app/mpe");
                               
    	File fileHandlerDir = new File(settingsFileDir);
    	String settingsFile = fileHandlerDir.getPath() +"/MPEGageTableDisplaySettings.txt";
    
    	set_settingsFile(settingsFile);
    	    	    	
        System.out.println(header + " 2.3 ");
        
        // retrieve the columns
        
        List<JTableColumnDescriptor> columnDescriptorList = createColumnDescriptorList();
        
        System.out.println(header + " 2.4 ");
        
        productDate = utcSimpleDateFormat.format(getDataManager()
                .getProductDate());
        setTitle("MPE Gage Table    " + productDate);
        
        System.out.println(header + " 3 ");
        
        
        tableScrollPane = createJTable(columnDescriptorList, rowDataList);

        System.out.println(header + " 4 ");
                
        createMenus();
        
        System.out.println(header + " 5 ");
        
        /* create a list for "select grid field" to allow user customize the grid field which
        they want to compare with gage value field, then the difference of gagevallue-gridvalue will
        be displayed in the "Diff (Gage - Grid)" colunm. The default field "Best Estimate QPE" will
        be used as the grid field if 1) token mpe_selected_grid_gagediff is NOT set AND 2)the "Best
        Estimate QPE" field IS included in mpe_generate_list token. Otherwise, "No field selected" 
        will be highlighted in the combobox list */
                             
        createSelectGridList();
        
        String [] gridDataName = get_gridDataName();
        int selected_index = get_selected_index();             
        
        _selectGridList = new JComboBox(gridDataName);        
        _selectGridList.setSelectedIndex(selected_index);
        _selectGridList.addActionListener(new selectGridActionListener());
         
                    
        searchTextField.addKeyListener(new keyActionListener());
       
        constraints.anchor = GridBagConstraints.NORTHWEST;        
        constraints.fill = GridBagConstraints.NONE;
        constraints.insets = new Insets(5, 5, 5, 5);

        addComponent(container, searchPanel, constraints, 0, 0, 1, 1, 0, 0);
        addComponent(searchPanel, searchLabel, constraints, 0, 0, 1, 1, 1, 0);
        addComponent(searchPanel, searchTextField, constraints, 1, 0, 1, 1, 1, 0);
       
        addComponent(searchPanel, selectGridLabel, constraints, 2, 0, 2, 1, 5, 1.0);
        addComponent(searchPanel, _selectGridList, constraints, 3, 0, 2, 1, 1, 0);

       /* searchPanel.add(searchLabel);
        searchPanel.add(searchTextField);
        searchPanel.add(selectGridLabel);
        searchPanel.add(_selectGridList);
        */
        
        constraints.anchor = GridBagConstraints.LINE_START;
        constraints.fill = GridBagConstraints.BOTH;
        constraints.insets = new Insets(10, 16, 2, 20);   
        
        gageTablePanel.add(tableScrollPane);

        addComponent(container, gageTablePanel, constraints, 0, 1, 3, 2, 1.0,
                1.0);

        constraints.anchor = GridBagConstraints.LAST_LINE_START;
        constraints.fill = GridBagConstraints.NONE;
        constraints.insets = new Insets(15, 16, 15, 20);

        addComponent(container, saveButton, constraints, 0, 3, 1, 1, 0.0, 0.0);

        constraints.anchor = GridBagConstraints.EAST;
        constraints.fill = GridBagConstraints.NONE;
        constraints.insets = new Insets(15, 20, 15, 20);

        addComponent(container, cancelButton, constraints, 2, 3, 1, 1, 0.0, 0.0);
  
     /*           buttonPanel.add(saveButton, BorderLayout.WEST);
                buttonPanel.add(cancelButton, BorderLayout.EAST);        
       */
        
        System.out.println(header + " 6 ");
        
        /*container.setLayout(new BorderLayout());
        container.add(searchPanel,BorderLayout.NORTH);
        container.add(gageTablePanel, BorderLayout.CENTER);      
        container.add(buttonPanel, BorderLayout.SOUTH);
        */
        
        pack();
        
        System.out.println(header + " 7 ");
        
        //set the initial width and height of the window
       
        /*int columnWidthTotal = _valueColumnWidth * columnDescriptorList.size();
        
        int windowWidth = 100 + columnWidthTotal;
        if (windowWidth > _maxWindowWidth)
        {
            windowWidth = _maxWindowWidth;
        }
        int windowHeight = 700;
       
        
        setSize(new Dimension(windowWidth, windowHeight));
        */
        System.out.println(header + " 8 ");

        saveButton.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                try
                {
                    saveGageTableEdits();
                }
                catch (IOException ex)
                {
                    getFileLogger().log(ex.getMessage());
                }

                gageTableClosingAction(GageTableFrame.this, getDataManager()
                        .getGageFilePath(), getDataManager()
                        .getEditedGageFilePath(), getFileLogger());
            }
        });

        cancelButton.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                gageTableClosingAction(GageTableFrame.this, getDataManager()
                        .getGageFilePath(), getDataManager()
                        .getEditedGageFilePath(), getFileLogger());
            }
        });
        addWindowListener(new WindowAdapter()
        {
            public void windowClosing(WindowEvent e)
            {
                gageTableClosingAction(GageTableFrame.this, getDataManager()
                        .getGageFilePath(), getDataManager()
                        .getEditedGageFilePath(), getFileLogger());
            }
        });
        
        
        System.out.println(header + " 9 ");
        setVisible(true);
        
        // Resize the window to trick the column headings into expanding to be large 
        // enough to show full labels.
        /*Rectangle bounds = this.getBounds();
        Dimension originalFrameDimension = new Dimension (bounds.width, bounds.height);
        Dimension newFrameDimension = new Dimension (bounds.width+100, bounds.height);
        this.setSize(newFrameDimension);
        this.repaint();
        this.setSize(originalFrameDimension);*/
        
        System.out.println(header + " 10 ");
        _tableManager.refreshDisplay();
        pack();
        
        System.out.println(header + " 11 ");
    }

    public JScrollPane createJTable(List<JTableColumnDescriptor> columnDescriptorList, List<GageTableRowData> rowDataList)
    {

        ComplexJTableManager jTableManager = new ComplexJTableManager(
                columnDescriptorList, rowDataList);
        
        jTableManager.setTableCellEditor(Object.class, new GageTableCellEditor ());

        setTableManager(jTableManager);
        
        // check if settings file exists, if does, create the table with the columns
        // defined in settings file. If not, then create the table with default setting.
        
        String settingString = loadSettingStringFromSettingsFile();
      
        if (settingString.length() > 0)
        {
        	getTableManager().getTableSettings().setSettingsFromString(settingString);
        	getTableManager().refreshDisplay();
         	
        }
        
        // Add listener for Table Cell Update events.
        jTableManager.addTableListener(new GageTableModelListener());

       // getTableManager().setPreferredSizeForJScrollPane(
       //                    new Dimension(680, 340));
        getTableManager().setPreferredSizeForJScrollPane(
                           new Dimension(1190, 545));
       
        JScrollPane tableScrollPane= getTableManager().getJScrollPane();
        
        return tableScrollPane;
    }

    public void createMenus()
    {
        JMenuBar menuBar = new JMenuBar();
        setJMenuBar(menuBar);

        JMenu fileMenu = new JMenu("File");
        fileMenu.setMnemonic('F');
        JMenuItem columnSelectionMenuItem = new JMenuItem("Column Selection");
        columnSelectionMenuItem.setMnemonic('C');
        fileMenu.add(columnSelectionMenuItem);

        JMenuItem refreshMenuItem = new JMenuItem("Refresh");
        refreshMenuItem.setMnemonic('R');
        fileMenu.add(refreshMenuItem);
        
        JMenuItem saveSettingMenuItem = new JMenuItem("Save Settings");
        saveSettingMenuItem.setMnemonic('S');
        fileMenu.add(saveSettingMenuItem);
        
        
        ChangeColumnsDisplayedMenuListener changeMenuListener = new ChangeColumnsDisplayedMenuListener();
        columnSelectionMenuItem.addActionListener(changeMenuListener);

        RefreshMenuListener RefreshMenuListener = new RefreshMenuListener();
        refreshMenuItem.addActionListener(RefreshMenuListener);

        SaveSettingMenuListener saveSettingMenuListener = new SaveSettingMenuListener();
        saveSettingMenuItem.addActionListener(saveSettingMenuListener);
        
        menuBar.add(fileMenu);
    }

    public class RefreshMenuListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            try
            {
                List<GageTableRowData> rowDataList = getDataManager().readData();
                getTableManager().setChangedAllRowDataList(rowDataList);                                   
                getTableManager().refreshDisplay();
            }
            catch (Exception ex)
            {
                getFileLogger().log(ex.getMessage());
                gageTableClosingAction(GageTableFrame.this, getDataManager()
                        .getGageFilePath(), getDataManager()
                        .getEditedGageFilePath(), getFileLogger());
            }
        }
    }
    
    public class SaveSettingMenuListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
        	saveSettingsToFile();
        }
        
    }    

    private class GageTableModelListener implements TableModelListener
    {
        public void tableChanged(TableModelEvent e)
        {

            if ((e != null) && (e.getType() == TableModelEvent.UPDATE))
            {
                int cellRow = e.getFirstRow();
                int cellColumn = e.getColumn();
                JTableModel tableModel = (JTableModel) e.getSource();

                if ((cellRow != TableModelEvent.HEADER_ROW)
                        && (cellColumn != TableModelEvent.ALL_COLUMNS)
                        && (cellRow == e.getLastRow()))
                {
                    String newValue = (String) tableModel.getValueAt(cellRow,
                            cellColumn);

                    // Get the data list structure from the Table Manager.
                    List<GageTableRowData> rowDataList = getTableManager()
                            .getAllRowDataList();
                    GageTableRowData rowData = rowDataList.get(cellRow);

                    // Update the edited row.
                    // The custom cell editor makes sure that the value is
                    // either a real number
                    // or 'm' or 'M' or "".
                    if (newValue.compareTo("") == 0)
                    {
                    	rowData.setValue(rowData.getOrginalValue());
                    }
                    else
                    {	
                       if (newValue.compareToIgnoreCase("m") == 0)
                       {                                                              	   
                	      rowData.setValue(DbTable.getNullFloat());
                       }
                              
                       else
                       {                       
                    	   rowData.setValue(new Float(newValue).floatValue());
                       }                      
                    } 
                    
                    rowData.addAllCellsToMap();
                }
            }
        }
    }

    public void saveGageTableEdits() throws IOException
    {
        List<GageTableRowData> gageTableRowList = getTableManager()
                .getAllRowDataList();
        getDataManager().saveUpdatedGageData(gageTableRowList);
    }

    private class ChangeColumnsDisplayedMenuListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {        	        	
            String header="GageTableFrame.ChangeColumnsDisplayedMenuListener(): ";
        	getTableManager().invokeColumnSelector(GageTableFrame.this);
            
            // update the combobox for grid data to compare with gage data
          
            createSelectGridList();
            
            String [] updatedGridList = get_gridDataName();
            int updated_selected_index = get_selected_index();
            
            List <String> strList = new ArrayList <String> ();
            for(String str : updatedGridList)
            {
            	strList.add(str);
            }
            
            for (int l = 0; l < strList.size(); l++)
       		 System.out.println(header + "strList is " + strList.get(l));
       	 
       	 System.out.println(header +"the updated selected_index is " + updated_selected_index);
            
            ChangeSelectGridListModel changeSelectGridListModel = new ChangeSelectGridListModel(strList);                       
            
            get_selectGridList().setModel(changeSelectGridListModel);
            
            get_selectGridList().setSelectedIndex(updated_selected_index);
            get_selectGridList().addActionListener(new selectGridActionListener());                                 
           
        }
    }
    
    private class  ChangeSelectGridListModel extends AbstractListModel implements ComboBoxModel
    {
    	private Object selectedItem;

    	  private List<String> strList;

    	  public ChangeSelectGridListModel(List arrayList) {
    	    strList = arrayList;
    	  }

    	  public Object getSelectedItem() {
    	    return selectedItem;
    	  }

    	  public void setSelectedItem(Object newValue) {
    	    selectedItem = newValue;
    	  }

    	  public int getSize() {
    	    return strList.size();
    	  }

    	  public Object getElementAt(int i) {
    	    return strList.get(i);
    	  }
		    
     
    	
    }
    
  public void createSelectGridList()
    {    	
    	String header="GageTableFrame.create_selectGridList(): ";
        boolean qpe_found = false;        
        String[] currentlyDisplayedName = null;
        int selected_index = DbTable.getNullInt();
        int gridDataName_num = 0;
        String [] gridDataName;
                       
        // get current Displayed column names
        currentlyDisplayedName = getTableManager().getColumnNamesThatAreCurrentlyDisplayed();
    	
    	// get rid of "LID", "Radar ID", "Gage Value", "Edit Gage Value" and "Diff (Gage-Grid)"
        // column names        
        gridDataName = new String[currentlyDisplayedName.length - 4 ];
        
        for (int i = 0; i < currentlyDisplayedName.length; i++)
        {
        	
        	if ((currentlyDisplayedName[i].equals("LID") == false) &&
        		(currentlyDisplayedName[i].equals("Radar ID") == false) &&
        		(currentlyDisplayedName[i].equals("Gage Value") == false) &&
        		(currentlyDisplayedName[i].equals("Edit Gage Value") == false) &&
        		(currentlyDisplayedName[i].equals("Diff (Gage-Grid)") == false))        	
        	{        		
        		gridDataName[gridDataName_num] = currentlyDisplayedName[i];        	
        		gridDataName_num++;
        	}	
        	
         }
                     
    	 for (int k =0; k<gridDataName.length; k++ )
         {       		
             if (gridDataName[k] != null)
             {
         
                 if (gridDataName[k].equals(getDataManager().get_mpe_selected_grid_gagediff()))
                 {            	         
         	        selected_index = k;
         	        System.out.println(header + "The selected grid field in combobox is " + 
         	    	   	    gridDataName[selected_index]);
                 }
                 if (gridDataName[k].equals("Best Estimate QPE"))
                 {
            	     qpe_found = true;
            	     if (selected_index == DbTable.getNullInt())
            	    	 selected_index = k;
                 }
             }	
         }
    	 
    	 if (qpe_found == false)
    	 {
    		 gridDataName[gridDataName_num-1]= "Not Valid Field";
    		 selected_index = gridDataName_num - 1;
    	 }
    	 
    	 for (int ii = 0; ii < gridDataName.length; ii++)
        	 System.out.println(header + " the gridDataName is " + gridDataName[ii]);
         
         System.out.println(header + " get_mpe_selected_grid_gagediff is " +
        		 getDataManager().get_mpe_selected_grid_gagediff());
         
         
    	 set_gridDataName(gridDataName);
    	 set_selected_index(selected_index);
    	
    }
    
    private void saveSettingsToFile()
    {
    	String header = "GageTableFrame.saveSettingsToFile(): ";
    	String settingString = null;
    	File gageTableSettingFile = null;
    	BufferedWriter out = null;
    	
    	// get settings from current table display window
    	settingString = getTableManager().getTableSettings().getSettingsString();
    	    	
    	// create the file which stores the settings
    	
    	gageTableSettingFile = createSettingsFile();
    	
    	// write table setting string (settingString) to the created file
    	try
    	{
    		out = new BufferedWriter(new FileWriter(gageTableSettingFile));
    	    
    		out.write(gageTableSettingFileHeader);
    		out.write(settingString, 0, settingString.length());
    		out.close();
    	}
    	catch(IOException e)
    	{
    		_fileLogger.log(header+"ERROR = " + e.getMessage());
    		e.printStackTrace(_fileLogger.getPrintWriter());
    		e.printStackTrace();
    	}    	    	        	    	
    	
    }
    
    private File createSettingsFile()
    {
    	String header = "GageTableFrame.createSettingsFile(): ";
    	boolean fileExist = false;
    	
    	File fileHandler = new File(get_settingsFile());
    	    	    	    	
    	fileExist = fileHandler.exists();
    	
    	if (!fileExist)
    	{
    		try
    		{
    			fileHandler.createNewFile();
    		}
    		catch(IOException e)
    		{
    			_fileLogger.log(header+"Unable to create file:" +
    					        fileHandler.getAbsolutePath()+
    					        " for saving table settings");
    		}    		
    	}
    	else
    	{
    		boolean fileDelete = false;
    		fileDelete = fileHandler.delete();
    		
    		if(!fileDelete)
    		{
    			_fileLogger.log(header+"FileName [" + fileHandler.getAbsolutePath() +
    					        "] exists alerady and unable to delete in order to " +
    					        "create a new file");
    		}
    		else
    	    {
    			try
    			{
    				fileHandler.createNewFile();
    			}
    			catch(IOException e)
    			{
    				_fileLogger.log(header+"Unable to create file:" + fileHandler.getAbsolutePath() +
    						        "for saving table setting");
    				_fileLogger.log(header+"ERROR = " + e.getMessage());
    				e.printStackTrace(_fileLogger.getPrintWriter());  
    				e.printStackTrace();
    			
    			}
    		 
    	    }
    	}
    	
    	System.out.println(header+ "The created setting file is " + get_settingsFile());
    	
    	return fileHandler;
    	
    }

    private void addComponent(Container container, Component component,
            GridBagConstraints gbc, int column, int row, int columnCells,
            int rowCells, double weightX, double weightY)
    {

        // how much it can grow in the X and Y directions
        gbc.weightx = weightX;
        gbc.weighty = weightY;

        // what row and column it starts in
        gbc.gridx = column;
        gbc.gridy = row;

        // the number of columns and rows it takes up
        gbc.gridwidth = columnCells;
        gbc.gridheight = rowCells;

        container.add(component, gbc);

        return;
    }

    public List<JTableColumnDescriptor> createColumnDescriptorList()
    {
        String header = "GageTableFrame.createColumnDescriptorList(): ";
        
        // create the default columnDescriptorList
        System.out.println(header + "1");
        List<JTableColumnDescriptor> columnDescriptorList = new ArrayList<JTableColumnDescriptor>();
        List<MPEProductDescriptor> availableMPEProductList;
        
        System.out.println(header + "2");
        
        // The LID and Value and Radar ID columns always exist.
        columnDescriptorList.add(new JTableColumnDescriptor("LID", true, _valueColumnWidth,
                "center", "Location ID"));
        
        System.out.println(header + "3");
        
        // for GAGE Value column.
        JTableColumnDescriptor originalValueColumnDescriptor = new JTableColumnDescriptor("Gage Value", true,
        		_valueColumnWidth, "center", "Gage Value");
        originalValueColumnDescriptor.setSortAsc(true);
        
        columnDescriptorList.add(originalValueColumnDescriptor);
        
        System.out.println(header + "4");
        
        // Edit GAGE Value column
        JTableColumnDescriptor valueColumnDescriptor = new JTableColumnDescriptor(
                "Edit Gage Value", true, _valueColumnWidth, "center", "Edit Gage Value");
        
        System.out.println(header + "4.1");
                     
        GageTableCellEditor cellEditor = new GageTableCellEditor();
        
        System.out.println(header + "4.2");
        
        valueColumnDescriptor.turnOnCellEditing(cellEditor);
        
        System.out.println(header + "4.3");
        
        valueColumnDescriptor.setSortAsc(true);
        
        
        System.out.println(header + "4.4");
        
        columnDescriptorList.add(valueColumnDescriptor);               
        
        System.out.println(header + "5");
        
        JTableColumnDescriptor diffColumnDescriptor = new JTableColumnDescriptor("Diff (Gage-Grid)", true,
                74, "center", "Difference between Gage Value and Grid Data");
        
        diffColumnDescriptor.setSortAsc(true);
        columnDescriptorList.add(diffColumnDescriptor);
        
        System.out.println(header + "6");
        
        
        columnDescriptorList.add(new JTableColumnDescriptor("Radar ID", true,
        		_valueColumnWidth, "center", "Radar ID"));

        System.out.println(header + "7");
        
        // Get a list of the fields being generated by the MPE Field Generator.
        availableMPEProductList = getDataManager().getAvailableMPEProductList();

        System.out.println(header + "8");
        
        int counter = 0;
        
        for (MPEProductDescriptor mpeProduct : availableMPEProductList)
        {
            counter++;
            
            System.out.println(header + "9, iteration # " + counter);
            
            JTableColumnDescriptor descriptor = new JTableColumnDescriptor(mpeProduct
                    .getProductName(), true, _valueColumnWidth, "center", mpeProduct
                    .getProductDescription());
            descriptor.setSortAsc(true);
            
            System.out.println(header + "10, iteration # " + counter);
            
            
            if (mpeProduct.getProductFilenamePrefix().equals(MPEProductDescriptor.MPE_BESTQPE))
            {
                
                System.out.println(header + "11, iteration # " + counter);
                columnDescriptorList.add(5, descriptor);
                System.out.println(header + "12, iteration # " + counter);
            }
            else //all other columns
            {
                columnDescriptorList.add(descriptor);
            }
        }
                            
        return columnDescriptorList;
    }
    
    private String loadSettingStringFromSettingsFile()
    {
    	String header = "GageTableFrame.loadSettingStringFromSettingsFile(): ";
    	StringBuffer settingString = new StringBuffer();
    	BufferedReader in = null;
    	boolean fileExist = false;
    	String line;
    	
    	File fileHandler = new File(get_settingsFile());
    	fileExist = fileHandler.exists();
    	
    	if (!fileExist)
    	{
    		_fileLogger.log(header + "Setting File can not be found, use default.");    	
    		System.out.println(header + "Setting File not be found, use default");
    	}
    	else
    	{	
    		
    	   try
    	   {
    		   in = new BufferedReader(new FileReader(fileHandler));
    		   if ( in.readLine() != null)
    		   {    			       			   
    			   while ((line = in.readLine()) != null )
    			   {    				       			       			     
    			       settingString.append(line).append("\n");
    			       	           			    
    			   }    			
    			       			   
    		   }
    		   
    		   _fileLogger.log(header + "Setting File " + get_settingsFile() + " is loaded");    	
       		   System.out.println(header + "Setting File " + get_settingsFile() + " is loaded");
       		   		
    		   in.close();
    		
    	   } catch (IOException e)
    	   {
    		   _fileLogger.log(header+ "Unable to read from file " + get_settingsFile());
    		   e.printStackTrace(_fileLogger.getPrintWriter());
    		   e.printStackTrace();
    	   }
    	}
    	
    	return settingString.toString(); 
    }
  
    public static void show(String logFilePath, String inputGageFilePath,
            String editedGageFilePath, String gageLogPath)
    {
        File editedFile = new File(editedGageFilePath);
        File inputFile = new File(inputGageFilePath);
       
        if (!_isRunning) // make sure that we don't invoke 2 of these gage
        // tables
        // at a time
        {
            FileLogger logger = new FileLogger(logFilePath);
            GageTableFrame gageTable = null;

            try
            {
                editedFile.delete();
                GageTableDataManager gageTableDataManager = new GageTableDataManager(
                        logger, inputGageFilePath, editedGageFilePath);
                gageTable = new GageTableFrame(logger, gageTableDataManager);
                gageTable.showGageTable();
            }
            catch (Exception e)
            {
                System.out.println(e.getMessage());
                logger.log(e.getMessage());
                gageTableClosingAction(gageTable, inputGageFilePath,
                        editedGageFilePath, logger);
            }

            _thread = new Thread(gageTable);
            _thread.start();
            
            _isRunning = true;
        }
    }

    // --------------------------------------------------------------------------------------------------
    public static void main(String[] args)
    {
        AppsDefaults appsDefaults = new AppsDefaults();
        File editedFile = null;
        File inputFile = null;
        String logDir = appsDefaults
                .getToken("mpe_editor_logs_dir",
                        "/awips/hydroapps/precip_proc/local/data/log/mpe_editor");
        FileLogger fileLogger = new FileLogger(logDir + "/GageTable.log");
        String gageFileDir = appsDefaults.getToken("mpe_scratch_dir",
                        "/awips/hydroapps/precip_proc/local/data/mpe/dailyQC/scratch");
        String inputGageFilePath = gageFileDir + "/gages.dat";
        String editedGageFilePath = gageFileDir + "/edited_gages.dat";
        GageTableFrame gageTable = null;
        editedFile = new File(editedGageFilePath);
        inputFile = new File(inputGageFilePath);
               

        try
        {
            editedFile.delete();
            GageTableDataManager gageTableDataManager = new GageTableDataManager(
                    fileLogger, inputGageFilePath, editedGageFilePath);
            gageTable = new GageTableFrame(fileLogger, gageTableDataManager);
            gageTable.showGageTable();
        }
        catch (Exception e)
        {
            e.printStackTrace();
            fileLogger.log(e.getMessage());
            inputFile.delete();

            gageTableClosingAction(gageTable, inputGageFilePath,
                    editedGageFilePath, fileLogger);
        }
    }

    // ---------------------------------------------------------------------------------------------------
    public void setFileLogger(FileLogger _fileLogger)
    {
        this._fileLogger = _fileLogger;
    }

    public FileLogger getFileLogger()
    {
        return _fileLogger;
    }

    public void setDataManager(GageTableDataManager _dataManager)
    {
        this._dataManager = _dataManager;
    }

    public GageTableDataManager getDataManager()
    {
        return _dataManager;
    }

    public void run()
    {
        // This is here to allow the caller to continue functioning while this
        // window is open
    	
          this.setVisible(true);
    	 
    }

    public void setTableManager(JTableManager tableManager)
    {
        _tableManager = tableManager;
    }

    public JTableManager getTableManager()
    {
        return _tableManager;
    }

    private class keyActionListener extends KeyAdapter
    {
        public void keyReleased(KeyEvent e)
        {
            ArrayList<Integer> rowsToHighlight = null;
            Component eventComponent = e.getComponent();
            boolean foundFirstMatch = false;
            int firstRowToHighlight;
            int lastRowToHighlight;
            String lidInTable = null;
            String lidInTableSubstring = null;
            String searchText = ((JTextField) eventComponent).getText().toUpperCase();
                        

            List<GageTableRowData> gageTableRowList = getTableManager()
                    .getAllRowDataList();

            getTableManager().deselectRows(0, gageTableRowList.size() - 1);

            if (searchText.length() > 0)
            {
                rowsToHighlight = new ArrayList<Integer>();

                for (int i = 0; i < gageTableRowList.size(); ++i)
                {
                    lidInTable = gageTableRowList.get(i).getGageId();

                    if (searchText.length() <= lidInTable.length())
                    {
                        lidInTableSubstring = lidInTable.substring(0,
                                searchText.length());

                        if (searchText.equals(lidInTableSubstring))
                        {
                            foundFirstMatch = true;
                            rowsToHighlight.add(new Integer(i));

                        }
                       // else if (searchText.compareTo(lidInTableSubstring) < 0)
                      //  {
                      //      break;
                      //  }
                    }
                }

                if (foundFirstMatch)
                {
                    firstRowToHighlight = rowsToHighlight.get(0).intValue();
                    lastRowToHighlight = rowsToHighlight.get(
                            rowsToHighlight.size() - 1).intValue();
                    getTableManager().selectRows(firstRowToHighlight,
                            lastRowToHighlight);
                }
            }
            else
            {
                getTableManager().selectRows(0, 0);
            }

        }

    }
    
   private class selectGridActionListener implements ActionListener 
    {
      
		public void actionPerformed(ActionEvent e)
		{
			/* get the selected Grid Field from the combobox, then recalculate the "Diff (Gage-Grid)" 
			field, and update the gage table window */ 
			
			JComboBox cb = (JComboBox)e.getSource();
			String gridName = (String)cb.getSelectedItem();
			
			getDataManager().set_mpe_selected_grid_gagediff(gridName);
			System.out.println("updated _mpe_selected_grid_gagediff is :" + getDataManager().get_mpe_selected_grid_gagediff());
	        			
		    RefreshMenuListener refresh = new RefreshMenuListener();
				
			refresh.actionPerformed(e);							 		
			
		}
    	
    }

public String get_settingsFile() {
	
	return _settingsFile;
}

public void set_settingsFile(String file) {
	_settingsFile = file;
}



public JComboBox get_selectGridList() {
	return _selectGridList;
}

public void set_selectGridList(JComboBox gridList) {
	_selectGridList = gridList;
}

public String [] get_gridDataName() {
	return _gridDataName;
}

public void set_gridDataName(String [] dataName) {
	_gridDataName = dataName;
}

public int get_selected_index() {
	return _selected_index;
}

public void set_selected_index(int _selected_index) {
	this._selected_index = _selected_index;
}



}
