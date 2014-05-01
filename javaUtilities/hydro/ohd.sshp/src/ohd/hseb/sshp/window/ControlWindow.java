/*
 * Created on Dec 19, 2003
 *
 * 
 */
package ohd.hseb.sshp.window;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableColumn;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import ohd.hseb.model.LocationDescriptor;
import ohd.hseb.sshp.*;
import ohd.hseb.sshp.gui.GuiHelper;
import ohd.hseb.sshp.precip.MAPPreprocessor;
import ohd.hseb.util.EnvHelper;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.Logger;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.TableColumnComparator;
import ohd.hseb.util.gui.WindowResizingManager;

/**
 * @author GobsC
 * This class is the startup window for the SSHP application when no argument for 
 * the location id is entered.  It allows for the selection of a location.
 */
public class ControlWindow extends JFrame
{
     private Dimension _maxDimension = new Dimension(800, 700);
     private Dimension _initialDimension = new Dimension(800, 400);
     private Dimension _minDimension = new Dimension(600, 200);
    
     private AppController _controller = null;   
     private List _locationIdList = null; 
     private DataMgr _dataMgr = null;
     
     
     // ---GUI items------------------------
     
     private JMenuBar _menuBar = null;
     private JPanel _mainPanel = null;
     
     private JPanel _selectionPanel = null; 
     private JLabel _locationLabel = null; 
     
     private ControlWindowTableAdapter _tableAdapter = null;
     
     private JPanel  _buttonPanel = null;
     private JButton _exitButton = null;
     private JButton _launchButton = null;
   //  private JButton _launchButton2 = null;
      
 	 public static final String LID = 	"Location Id";
	 public static final String NAME = 	"Name";
	 public static final String BASIN_ID = "Basin Id";
	 public static final String STREAM_NAME= "Stream Name";
     public static final String HSA =  "HSA";
     public static final String MODEL_PREFERENCE =  "RR Model Pref"; //rainfall-runoff model preference
	
	 private List _descriptorList  = null;
     private LocationDescriptor _selectedLocationDescriptor = null;
      
     // ---------------------------------------------------------------------
    
     public ControlWindow(DataMgr dataMgr, AppController controller)
     {
          _dataMgr = dataMgr;
          _controller = controller;

          initGui();
     }
     
     // ---------------------------------------------------------------------
     public void enableLaunchButton(boolean value)
     {
         _launchButton.setEnabled(value);
     }
   
     // ---------------------------------------------------------------------
         
     private void initGui()
     {
    //    Dimension scrolledPaneDimension = new Dimension(500, 300);
   
         
         
        initMenuBar();
        setJMenuBar(_menuBar);
         
       // String header = "LaunchWindow.initGui(): "; 
        setTitle("Site Specific Hydrologic Predictor Control Window on " + _dataMgr.getDatabaseName());
       
        JPanel spaceFiller = new JPanel();
//      Note: setMinimumSize() does not work for this purpose
        spaceFiller.setPreferredSize(new Dimension(300, 100)); 
    
        //selection Panel
        
        createSelectionPanel();

        // button Panel   
        _buttonPanel = new JPanel();
        
        _launchButton = GuiHelper.getJButton("Analysis Window...");
        _exitButton = GuiHelper.getJButton("Exit Application");
     
    
        _buttonPanel.add(_launchButton);
        _buttonPanel.add(_exitButton);
        
        
        
        // setup the main panel
        
        _mainPanel = new JPanel();
    //    _mainPanel.setPreferredSize(new Dimension(DEFAULT_WIDTH, DEFAULT_HEIGHT));
        
        GridBagLayout layoutMgr = new GridBagLayout();       
        GridBagConstraints mainGbc = new GridBagConstraints();
        
        mainGbc.fill = GridBagConstraints.BOTH;
        mainGbc.anchor = GridBagConstraints.NORTHWEST;
        mainGbc.weightx = 1;
        mainGbc.weighty = 1;
       
        _mainPanel.setLayout(layoutMgr);
                                                     //       col, row   numCols numRows  Wcol wrow   
        addComponent(_mainPanel, spaceFiller,       mainGbc,  0,     0,  1,      1,       0,   0);
        addComponent(_mainPanel, _selectionPanel,   mainGbc,  0,     1,  1,      1,       1,   1);
        addComponent(_mainPanel, _buttonPanel,      mainGbc,  0,     2,  2,      2,       0,   0);
    
        
        
        //set up initial bounds limitations
        //see WindowResize listener for the minimum setting

        Rectangle maxBoundsRectangle = new Rectangle(_maxDimension);
        this.setMaximizedBounds(maxBoundsRectangle);

         //hook up the mainPanel to the contentPane
      
        this.getContentPane().add(_mainPanel);
        this.pack();

        Rectangle initialBoundsRectangle = new Rectangle(_initialDimension);
        this.setBounds(initialBoundsRectangle);

        
        addListeners();
        
        return;    
     }
     // ---------------------------------------------------------------------
     private void createSelectionPanel()
     {
         Dimension selectionPanelDimension = new  Dimension(800,700);
         
         _locationLabel = new JLabel("Available Locations:");
         _locationIdList = _dataMgr.loadLocationIdsForForecasts();
         
         int listSize = _locationIdList.size();
         _selectionPanel = new JPanel();
         _selectionPanel.setPreferredSize(selectionPanelDimension);
         
         
         
         // create the JTable from which to choose the Location    
         _descriptorList = createLocationDescriptorList(_locationIdList);       
         _tableAdapter = createDescriptorTableAdapter(_descriptorList);
  
         
         GridBagLayout layoutMgr = new GridBagLayout();       
         GridBagConstraints mainGbc = new GridBagConstraints();
         
         mainGbc.fill = GridBagConstraints.BOTH;
         mainGbc.anchor = GridBagConstraints.NORTHWEST;
         mainGbc.weightx = 1;
         mainGbc.weighty = 1;
        
         _selectionPanel.setLayout(layoutMgr);
                                
         JScrollPane scrollPane = _tableAdapter.getScrollPane();
         
         //                                                            col, row   numCols numRows  Wcol wrow   
         addComponent(_selectionPanel, _locationLabel,       mainGbc,  0,   0,     1,      1,       0,   0);
         addComponent(_selectionPanel, scrollPane,           mainGbc,  0,   1,     1,      1,       1,   1);
                 
     }
     // ---------------------------------------------------------------------
     private ControlWindowTableAdapter createDescriptorTableAdapter(List descriptorList)
 	 {
         
        Dimension scrolledPaneDimension = new Dimension(500, 150);
        Dimension scrolledPaneViewportDimension = new Dimension(300, 100);
         
 	    JTable table = null;
 		String[] columnNameArray = 	{ LID, NAME, BASIN_ID, STREAM_NAME, HSA, MODEL_PREFERENCE };
 		ControlWindowTableAdapter tableAdapter = null;
 		
 		TableColumnComparator comparator = new ControlWindowTableColumnComparator();
 		tableAdapter = new ControlWindowTableAdapter(comparator, 
 		        									 columnNameArray,
 		        									 descriptorList );
 	      
 		
 		JScrollPane tableScrollPane = tableAdapter.getScrollPane();
     	tableScrollPane.setPreferredSize(scrolledPaneDimension);
     	tableScrollPane.setMinimumSize(scrolledPaneDimension);
     	
     		
     	//select the first descriptor in the list	
     	if (tableAdapter.getDescriptorCount() > -1)
     	{
     	    tableAdapter.setSelectedIndex(0);
     	    LocationDescriptor descriptor = tableAdapter.getDescriptorByIndex(0);
     	}
     	
    	tableAdapter.getTable().setPreferredScrollableViewportSize( scrolledPaneViewportDimension );
    	
     	
     	TableColumn col = tableAdapter.getTable().getColumnModel().getColumn(0);
        int width = 20;
        col.setPreferredWidth(width);

   
   	    return tableAdapter;
 	
 	 }
  
     // ---------------------------------------------------------------------
     
     private List createLocationDescriptorList(List locationIdList)
     {
         List locationDescriptorList = new ArrayList();
         final int maxNameStringLength = 20;
         
       //  StringBuffer buffer = new StringBuffer();
         
         for (int i = 0; i < locationIdList.size(); i++)
         {
             //clear out any old string
           //  buffer.setLength(0);
             
             String locationId = (String) locationIdList.get(i);
 
             LocationDescriptor descriptor = _dataMgr.getLocationDescriptor(locationId);
             locationDescriptorList.add(descriptor);
             
         }
         
         return locationDescriptorList;
         
     }
     // ---------------------------------------------------------------------
    
     // ---------------------------------------------------------------------
    
     private void initMenuBar()
     {
          _menuBar = new JMenuBar();
           
         JMenu menu = null;
         JMenuItem menuItem = null;
   
         //File Menu
          
         menu = new JMenu("File");
         menu.setMnemonic(KeyEvent.VK_F);
         menu.getAccessibleContext().setAccessibleDescription(
                          "Access File Menus");
         _menuBar.add(menu);
         
         
         menuItem = new JMenuItem("Open Analysis Window");
         menuItem.setAccelerator(KeyStroke.getKeyStroke(
                 KeyEvent.VK_O, ActionEvent.ALT_MASK));
         menuItem.getAccessibleContext().setAccessibleDescription(
                 "Open a new Analysis Window");
         menu.add(menuItem); 
         menuItem.addActionListener(new AnalysisWindowLaunchListener());

         
         menuItem = new JMenuItem("Exit Application");
         menuItem.setAccelerator(KeyStroke.getKeyStroke(
                 KeyEvent.VK_E, ActionEvent.ALT_MASK));
         menuItem.getAccessibleContext().setAccessibleDescription(
                 "Exit the Application");
         menu.add(menuItem); 
         menuItem.addActionListener(new ExitListener());

        
 
         // Configuration Menu
         menu = new JMenu("Configuration");
         menu.setMnemonic(KeyEvent.VK_C);
         menu.getAccessibleContext().setAccessibleDescription(
                          "Configure the forecast points");
         _menuBar.add(menu);
         
         menuItem = new JMenuItem("Configure Fcst Points for SSHP...");
     //    menuItem.setAccelerator(KeyStroke.getKeyStroke(
     //            KeyEvent.VK_S, ActionEvent.ALT_MASK));
         menuItem.getAccessibleContext().setAccessibleDescription(
                 "Configure a point for SSHP");
         menu.add(menuItem);
         
         menuItem.addActionListener(new ConfigureLaunchListener());

 
         // monthly values menuItem
         menuItem = new JMenuItem("MAPE Monthly Values Editor...");
         menuItem.setAccelerator(KeyStroke.getKeyStroke(
                 KeyEvent.VK_M, ActionEvent.ALT_MASK));
         menuItem.getAccessibleContext().setAccessibleDescription(
                 "View and edit MAPE Monthly Values");
         menu.add(menuItem);
         
         menuItem.addActionListener(new  MonthlyMapeEditorLaunchActionListener());
         
          
         //SAC-SMA parameters editor menu item
         menuItem = new JMenuItem("SAC-SMA Parameters Editor...");
         menuItem.setAccelerator(KeyStroke.getKeyStroke(
                 KeyEvent.VK_P, ActionEvent.ALT_MASK));
         menuItem.getAccessibleContext().setAccessibleDescription(
                 "View and edit SAC-SMA Parameters");
         menu.add(menuItem);
         
         menuItem.addActionListener(new  SacParamsLaunchActionListener());
        
         // SAC-SMA states editor menu Item
         menuItem = new JMenuItem("SAC-SMA States Editor...");
         menuItem.setAccelerator(KeyStroke.getKeyStroke(
                 KeyEvent.VK_S, ActionEvent.ALT_MASK));
         menuItem.getAccessibleContext().setAccessibleDescription(
                 "View and edit SAC-SMA States");
         menu.add(menuItem);
         
         menuItem.addActionListener(new  SacStateLaunchActionListener());
        
         
         // Rating Curve editor menu Item
         menuItem = new JMenuItem("Rating Curve Editor...");
         menuItem.setAccelerator(KeyStroke.getKeyStroke(
                 KeyEvent.VK_R, ActionEvent.ALT_MASK));
         menuItem.getAccessibleContext().setAccessibleDescription(
                 "View and edit Rating Curves");
         menu.add(menuItem);
         
         menuItem.addActionListener(new  RatingCurveLaunchActionListener());
         
         
         // UnitHydrograph editor menu Item
         menuItem = new JMenuItem("Unit Hydrograph Editor...");
         menuItem.setAccelerator(KeyStroke.getKeyStroke(
                 KeyEvent.VK_U, ActionEvent.ALT_MASK));
         menuItem.getAccessibleContext().setAccessibleDescription(
                 "View and edit Unit Hyddrographs");
         menu.add(menuItem);
         
         menuItem.addActionListener(new  UnitHydrographLaunchActionListener());
         
         
         // extra
         menu = new JMenu("Extra");
         menu.setMnemonic(KeyEvent.VK_X);
         menu.getAccessibleContext().setAccessibleDescription(
                          "Extra functionality");
         _menuBar.add(menu);
         
         // Run preprocessor
         menuItem = new JMenuItem("Run MAP Preprocessor...");
      //   menuItem.setAccelerator(KeyStroke.getKeyStroke(
      //           KeyEvent.VK_P, ActionEvent.ALT_MASK));
         menuItem.getAccessibleContext().setAccessibleDescription(
                 "Run MAP Preprocessor");
         menu.add(menuItem);
         
         menuItem.addActionListener(new  MapPreprocessorRunListener());
         
         // separator
         
         //JSeparator menuSeparator = new JSeparator();  
         //_menuBar.add(menuSeparator);
         
              
         //acts as a separator to move the Help menu all the way to the right
         menuItem = new JMenuItem("");
         menuItem.setEnabled(false);
         _menuBar.add(menuItem);
        
        
         // Help Menu
          
         menu = new JMenu("Help");
         menu.setMnemonic(KeyEvent.VK_HELP);
         menu.getAccessibleContext().setAccessibleDescription(
                 "Access help menu items.");        
         _menuBar.add(menu);
         
         menuItem = new JMenuItem("About...");
         menuItem.setAccelerator(KeyStroke.getKeyStroke(
                 KeyEvent.VK_A, ActionEvent.ALT_MASK));
         menuItem.getAccessibleContext().setAccessibleDescription(
                 "Display the About Dialog.");
         menu.add(menuItem);
         
         menuItem.addActionListener(new AboutLaunchListener());

        
     }
     
     //  ---------------------------------------------------------------------  
     private void addComponent(Container container,
                                  Component component,
                                  GridBagConstraints gbc,
                                  int column, int row,
                                  int columnCells, int rowCells,
                                  int weightX, int weightY)
     {

        // how much it can grow in the X and Y directions   
        gbc.weightx = weightX;
        gbc.weighty  = weightY;

        // what row and column it starts in
        gbc.gridx = column;
        gbc.gridy = row;

        //the number of columns and rows it takes up
        gbc.gridwidth = columnCells;
        gbc.gridheight = rowCells;
    
        container.add(component, gbc);
    
        return;
     } 
    
    // ---------------------------------------------------------------------
     private void runMAPPreprocessor()
     {
         final long millisPerHour = 1000 * 60 * 60;
         MAPPreprocessor preprocessor = null;

         long endTime = System.currentTimeMillis();
         endTime = (endTime/millisPerHour) * millisPerHour;

         long startTime = endTime - (6 * 24 * millisPerHour) ;

         String fileDir = _controller.getLogDirName();
         String fileName = "/MAPPreprocessor.log";
         String filePath = null;
         
         if (fileDir != null)
         {
            filePath = fileDir + fileName;    
         }
         else
         {
            filePath = fileName;    
         }
           
          
         Logger logger = new FileLogger(filePath, true, true);

        // preprocessor = new MAPPreprocessor(_dataMgr, startTime, endTime, logger);
         preprocessor = new MAPPreprocessor(_dataMgr, logger);

         System.out.println("Beginning MAP preprocessing");
         preprocessor.preprocessAll();  
         System.out.println("Completed MAP preprocessing");
            
     
     } //runMAPPreprocessor (in same VM)
     // ---------------------------------------------------------------------
         
     private void runMAPPreprocessorAsSeparateProcess()
     {
         String header = "ControlWindow.runMAPPreprocessorAsSeparateProcess(): ";
         System.out.println("Beginning MAP preprocessing");
         EnvHelper envHelper = new EnvHelper();
         
         String dirString = envHelper.getProperty("WHFS_BIN_DIR");
           
         String commandString = dirString + "/run_SSHP_MAP_preprocess";
         
         if (_controller.isRunningOnWindows())
         {
             commandString += ".bat";
         }
         
         System.out.println(header + "making system call:" + commandString + ":");
         System.out.flush();
         try
         {
             Process process = Runtime.getRuntime().exec(commandString);
             process.waitFor();
         }
         catch(IOException e)
         {
             e.printStackTrace();
         }
         catch( InterruptedException e )
         {
         	e.printStackTrace();
         }

         System.out.println("Completed MAP preprocessing");
     }
   
     // ---------------------------------------------------------------------
     

     private void addListeners()
     {
         //allow the frame to close when the user presses the close-box
          addWindowListener(new FrameCloseWindowListener());
   
         //hook up the configure launch button
 //        _configButton.addActionListener(new ConfigureLaunchListener()); 
   
         //hook up the run MAP preprocessor button
  //       _mapPreprocessorButton.addActionListener(new MapPreprocessorRunListener());
          
         //hook up the application launch button and the double click launching from the table
       //  _launchButton.addActionListener(new OldAnalysisWindowLaunchListener());
         _launchButton.addActionListener(new AnalysisWindowLaunchListener());
         _tableAdapter.getTable().addMouseListener(new DoubleClickAnalysisWindowLaunchListener());
         
         // set up the action to perform when the close button is pressed
         _exitButton.addActionListener( new ExitListener());

		 // sets up the minimum and maximum sizes for the window    
		 WindowResizingManager mgr = new WindowResizingManager(this, _minDimension, _maxDimension);
     
		 _tableAdapter.getListSelectionModel().addListSelectionListener( new DescriptorRowSelectionListener() );       
		
     }
     
// ---------------------------------------------------------------------------------     
    public String getSelectedLocationId()
    {
        String locationId = null;
        int listSize = _locationIdList.size();

        int selectedIndex = _tableAdapter.getFirstSelectedIndex();
        if ( (selectedIndex >= 0) && (listSize > 0) )
        {
           // locationId = (String) _locationIdList.get(selectedIndex);
            
            LocationDescriptor desc = (LocationDescriptor) _tableAdapter.getDataList().get(selectedIndex);
            locationId = desc.getId();
        }

        return locationId;
    }
     
    // ---------------------------------------------------------------------
    private synchronized void launchAnalysisWindow()
    {
        DialogHelper.setWaitCursor(ControlWindow.this);
        _launchButton.setEnabled(false);
        
        String locationId = getSelectedLocationId();
        
        if (locationId != null)
        {
            _controller.displayAnalysisWindow(locationId);
            
        } 
        else
        {
            DialogHelper.displayErrorDialog(ControlWindow.this, 
                    "You need to select a station in order to display the Analysis Window.",
            "No Station Selected");
        }
        
        _launchButton.setEnabled(true);
        
        DialogHelper.setDefaultCursor(ControlWindow.this);
        
    }
    // ---------------------------------------------------------------------  
    private class FrameCloseWindowListener extends WindowAdapter
    {
        public void windowClosing(WindowEvent evt)
        {
            dispose();
            _dataMgr.disconnect();
            System.exit(0);
        }

    }
     
    // ---------------------------------------------------------------------
    
    private class MapPreprocessorRunListener implements ActionListener
    { 
        public void actionPerformed(ActionEvent evt)
        {
            DialogHelper.setWaitCursor(_menuBar);
            runMAPPreprocessorAsSeparateProcess();
            DialogHelper.setDefaultCursor(_menuBar);
        }
    }
    // ---------------------------------------------------------------------
   
    private class ExitListener implements ActionListener
    { 
          public void actionPerformed(ActionEvent evt)
          {
              dispose();
              System.exit(0);   
          }
    }

    // ---------------------------------------------------------------------
    private class AboutLaunchListener implements ActionListener
    { 
         public void actionPerformed(ActionEvent evt)
         {
             //JOptionPane.showMessageDialog(LaunchWindow.this, _aboutText, "About SSHP", JOptionPane.INFORMATION_MESSAGE );  
             String aboutText = AboutInfo.getText();
             DialogHelper.displayMessageDialog(ControlWindow.this, aboutText, "About SSHP");
         }
    }

    // ---------------------------------------------------------------------
    private class AnalysisWindowLaunchListener implements ActionListener
    { 
         public void actionPerformed(ActionEvent evt)
         {    
             launchAnalysisWindow();   
             return;
         }
    }
    
    // ---------------------------------------------------------------------
    
    private class DoubleClickAnalysisWindowLaunchListener extends MouseAdapter
    { 
        public void mouseClicked(MouseEvent event)
        {
            int button = event.getButton();
            if (button == MouseEvent.BUTTON1)
            {
                if (event.getClickCount() > 1)
                {
                    launchAnalysisWindow();
                }
            }
             return;
        }
    }
    // ---------------------------------------------------------------------
    private class ConfigureLaunchListener implements ActionListener
    { 
        public void actionPerformed(ActionEvent evt)
        {
            SSHPConfigDialog dialog = new SSHPConfigDialog(ControlWindow.this, _dataMgr);
                 
            dialog.setVisible(true);
               
        }
    } //ConfigureLaunchListener

    // ---------------------------------------------------------------------
    
    private class   MonthlyMapeEditorLaunchActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            MonthlyValueEditor editor = null;
            
            JFrame ownerFrame = ControlWindow.this;
            DataMgr dataMgr =  _dataMgr;
            String basinId = null;
            boolean isModal = false;
            String pe = "EA";
              
            editor = new MonthlyValueEditor(ownerFrame, dataMgr, basinId, pe, isModal);
            editor.setVisible(true);

        }
    }  
    //--------------------------------------------------------------------
    
    private class SacParamsLaunchActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {

            String basinId = null;
            SacParamsEditor editor = null;
            
            editor = new SacParamsEditor(ControlWindow.this, _dataMgr, basinId);
            editor.setVisible(true);
        }
    } 
    
    //--------------------------------------------------------------------
      
    private class SacStateLaunchActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            String basinId = null;

            SacStateEditor editor = null;
            editor = new SacStateEditor(ControlWindow.this, _dataMgr, basinId);

            editor.setVisible(true);
        }
    }

    //--------------------------------------------------------------------
    private class RatingCurveLaunchActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {

            String lid = getSelectedLocationId();

            if (lid != null)
            {
                RatingCurveEditor editor = null;
                //  editor = new RatingCurveEditor(ControlWindow.this, _dataMgr,
                // basinId);
                boolean isModal = false;
                editor = new RatingCurveEditor(ControlWindow.this, _dataMgr,
                        lid, isModal);


                editor.setVisible(true);
            }
        }
    }

    //--------------------------------------------------------------------
    
    private class UnitHydrographLaunchActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {

            String lid = getSelectedLocationId();

            if (lid != null)
            {
                UnitHydrographEditor editor = null;
                boolean isModal = false;
                editor = new UnitHydrographEditor(ControlWindow.this, _dataMgr, lid, isModal );
        
                editor.setVisible(true);
            }
        }
    } 
       
    //-------------------------------------------------------------------- 
    private class DescriptorRowSelectionListener implements
            ListSelectionListener
    {

        public void valueChanged(ListSelectionEvent event)
        {
            String header = "DescriptorRowSelectionListener.valueChanged(): ";
            LocationDescriptor descriptor = null;
         
            int index = _tableAdapter.getFirstSelectedIndex();
            
            if (index > -1)
            {
                _selectedLocationDescriptor = (LocationDescriptor)_descriptorList.get(index);
               // System.out.println(header + "selected lid = " +
               //         	_selectedLocationDescriptor.getId());
            }
            else
            {
                _selectedLocationDescriptor = null;
            }
     
        }
    }
    //-------------------------------------------------------------------- 
    
} //end LaunchWindow class 
