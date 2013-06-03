package ohd.hseb.dimensions_file_uploader;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.border.BevelBorder;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;

import ohd.hseb.rfc.util.listpanels.JListPanel;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledComboBox;

public class NewDimensions_File_Uploader extends JFrame
{
    private static final String OB8_1_WORKSET = "AWIPS:OHD-OB8.1";
    private static final String OB8_2_WORKSET = "AWIPS:OHD-OB8.2";
    private static final String OB8_3_WORKSET = "AWIPS:OHD-OB8.3";
    private static final String OB9_0_WORKSET = "AWIPS:OHD-OB9";
    private static final String OB9_1_WORKSET = "AWIPS:OHD-OB9.1";
    private static final String OB9_2_WORKSET = "AWIPS:OHD-OB9.2";
    private static final String OB8_1_1_WORKSET = "AWIPS:OHD-OB8.1.1";
    private static final String OB8_2_1_WORKSET = "AWIPS:OHD-OB8.2.1";
    private static final String OB8_3_1_WORKSET = "AWIPS:OHD-OB8.3.1";
    private static final String OB9_0_1_WORKSET = "AWIPS:OHD-OB9.0.1";
    private static final String OB9_0_2_WORKSET = "AWIPS:OHD-OB9.0.2";
    

    private String _dimensionsFileQueueBaseDir = "/awips/hydroapps/PVCSFileQueue/";
    private String _queuedFileListDir = _dimensionsFileQueueBaseDir + "filelist/";
    private String _failedFileListDir = _dimensionsFileQueueBaseDir + "filelist.failed/";
    private String _successFileListDir = _dimensionsFileQueueBaseDir + "filelist.success/";
    private String _dimensionsMirrorBaseDir = "/awips/hydroapps/DimensionsMirror/ob83/";
	private String _attributesDir = "";
    private String _fileToBeUploadedString = "";
    private String _drNumberString = "";
    private String _buildString = "";
    private String _username = null;
    private String _password = null;
    private String _workset = null;
    private boolean _drUploadSuccess = true;
    private boolean _validUsernamePassword = false;
    
    private JPanel _queuedDRDCSPanel = new JPanel( new GridBagLayout() );
    private LabeledComboBox _DRDCSSelectionCB = new LabeledComboBox( "Select DR/DCS:   " );
    private Container _frameContentPane = getContentPane();
    protected JListPanel _fileListPanel = null;
    private JTextArea _drInfoTextArea = new JTextArea( 20, 30 );
    
    private JPanel _failedDRPanel = new JPanel( new GridBagLayout() );
    private LabeledComboBox _failedDRDCSSelectionCB = new LabeledComboBox( "Select Failed DR/DCS:  " );
    private JButton _resetButton = new JButton( "Reset DR/DCS" );
    private JListPanel _failedFileListPanel = null;
    
    private JPanel _successDRPanel = new JPanel( new GridBagLayout() );
    private LabeledComboBox _successDRDCSSelectionCB = new LabeledComboBox( "Select Successful DR/DCS:  " );
    private JListPanel _successFileListPanel = null;
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _uploadButton = new JButton( "Upload Selected DR/DCS" );
    private JButton _resetGUIButton = new JButton( "Reset GUI" );
    private JButton _closeButton = new JButton( "Close" );
    
    private ActionListener _queuedSelectionComboBoxListener = new QueuedSelectionComboBoxListener();
    private ActionListener _failedDRDCSSelectionComboBoxListener = new FailedDRDCSSelectionComboBoxListener();
    private ActionListener _successDRDCSSelectionComboBoxListener = new SuccessDRDCSSelectionComboBoxListener();
    private JMenuBar _menuBar = null;
    
    private JPanel _drInfoPanel = new JPanel( new GridBagLayout() );

    private JSplitPane _splitPane = null;
    private JSplitPane _leftSplitPane = null;
    private JSplitPane _rightSplitPane = null;
    
    private JPopupMenu _popupMenu = new JPopupMenu();

    private String _dateString = "July 14, 2008";
    private String _verString = "1.22";
    private String _versionString = "Application:     Dimensions File Uploader" + 
                                    "\nVersion:           " + _verString + 
                                    "\nDate:               " + _dateString + 
                                    "\nDeveloped By: Your friendly neighborhood DSA"; 

	public NewDimensions_File_Uploader()
	{
        setTitle( "Dimensions_File_Uploader - " + _dateString );
        setPreferredSize( new Dimension( 1200, 900 ) );
	}
    
    private void initGUI()
    {
        initDRList();
        updateQueuedFileJList();
        updateFailedSuccededFileJList( _failedDRDCSSelectionCB, true );
        updateFailedSuccededFileJList( _successDRDCSSelectionCB, false );
        initButtonPanel();
        initQueuedPanel();
        initFailedPanel();
        initSuccessPanel();
        initSplitPane();
        initFrameComponents();
        initMenuBar();
        refreshPopupMenu();
        addListeners();
        setJMenuBar( _menuBar );
        pack();
        setVisible( true );
        _splitPane.setDividerLocation( .50 );  // 50%
        _leftSplitPane.setDividerLocation( .50 );  // 50%
        _rightSplitPane.setDividerLocation( .50 );  // 50%
    }
    
    private void initMenuBar()
    {
        _menuBar = new JMenuBar();
        JMenu menu = null;
        JMenuItem menuItem = null;
  
        //File Menu
         
        menu = new JMenu( "File" );
        menu.setMnemonic( KeyEvent.VK_F );
        menu.getAccessibleContext().setAccessibleDescription( "Access File Menus" );
        _menuBar.add( menu );
        
        menuItem = new JMenuItem( "Log Viewer" );
        menuItem.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_L, ActionEvent.ALT_MASK ) );
        menuItem.getAccessibleContext().setAccessibleDescription( "Open Log Viewer" );
        menu.add( menuItem );
        menuItem.addActionListener( new LogViewerListener() );
        
        menuItem = new JMenuItem("Exit Application");
        menuItem.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_E, ActionEvent.ALT_MASK ) );
        menuItem.getAccessibleContext().setAccessibleDescription( "Exit the Application" );
        menu.add(menuItem); 
        menuItem.addActionListener( new WindowCloserListener() );
        
        //acts as a separator to move the Help menu all the way to the right
        menuItem = new JMenuItem("");
        menuItem.setEnabled(false);
        _menuBar.add(menuItem);
       
       
        // Help Menu
         
        menu = new JMenu("Help");
        menu.setMnemonic(KeyEvent.VK_H);
        menu.getAccessibleContext().setAccessibleDescription( "Access help menu items." );        
        _menuBar.add(menu);
        
        menuItem = new JMenuItem( "Common Dimensions Error Messages" );
        menuItem.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_E, ActionEvent.ALT_MASK ) );
        menuItem.getAccessibleContext().setAccessibleDescription( "Display common Dimensions Error Messages" );
        menu.add( menuItem );
        menuItem.addActionListener( new DisplayDimensionsErrorMessages() );
        
        menuItem = new JMenuItem("About...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke( KeyEvent.VK_B, ActionEvent.ALT_MASK ) );
        menuItem.getAccessibleContext().setAccessibleDescription( "Display the About Dialog." );
        menu.add(menuItem);
        menuItem.addActionListener( new DisplayAboutDialog() );
    }
    
    private class DisplayAboutDialog implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            DialogHelper.displayMessageDialog( NewDimensions_File_Uploader.this, _versionString, "About" );
        }
    }

    private class DisplayDimensionsErrorMessages implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            DialogHelper.displayMessageDialog( NewDimensions_File_Uploader.this, getDimensionsErrors(), "Common Dimensions Error Messages" );
        }
    }
        
    private String getDimensionsErrors()
    {
        String errorString = null;
        
        errorString = "1128-You do not have the change document AWIPS_DR_##### in your pending list\n" + 
                      "1128-You do not have the change document AWIPS_DCS_##### in your pending list\n" + 
                      "The DR/DCS has not been delegated to the user.  The GUI will show who the DR/DCS is assigned to.  See who is designated as the “DEV MGR”.\n" + 
                      "The user will have to contact this individual to have the DR/DCS delegated to them.\n\n" + 
                      "2261-Error: You do not have a role to create this Work Set Directory\n" + 
                      "2385-Error: The specified item is not in the workset AWIPS:OHD-OB9\n" + 
                      "The attributes file for the specific failed file has the file designated as an OLD file.\n" + 
                      "The attributes file will need to be modified so the file is designated as a NEW file.\n\n" +
                      "i.e.\n" +
                      "/fs/hseb/ob83/ohd/pproc_lib/inc/polygon_RFCW.h\n" +
                      "AWIPS_DR_20077\n" + 
                      "OLD\n" + 
                      "ob83\n" + 
                      "becomes:\n" + 
                      "/fs/hseb/ob83/ohd/pproc_lib/inc/polygon_RFCW.h\n" +
                      "AWIPS_DR_20077\n" + 
                      "NEW\n" +
                      "ob83\n\n" +
                      "2210-Error: The Work Set filename is not unique in the Work Set Directory\n" + 
                      "2229-Error: Filename #########  is already used for another Item in the same library, please specify a different filename\n" +
                      "The attributes file for the specific failed file has the file designated as a NEW file.\n" + 
                      "The attributes file will need to be modified so the file is designated as an OLD file.\n\n" +
                      "Error: User authentication failed\n" +
                      "The username/password entered is incorrect.\n";
        
        return errorString;
    }
    
    
    private void refreshPopupMenu()
    {
        _popupMenu.removeAll();

        _popupMenu.setBorder( BorderFactory.createTitledBorder( BorderFactory.createBevelBorder( BevelBorder.RAISED, Color.GRAY, Color.BLACK ), "File Options" ) );
        JMenuItem menuItem = null;
        
        menuItem = new JMenuItem( "Compare to Dimensions version" );
        _popupMenu.add( menuItem );
        menuItem.addActionListener( new LaunchXDiffListener() );
        menuItem = new JMenuItem( "View File" );
        _popupMenu.add( menuItem );
        menuItem.addActionListener( new ViewFileListener() );
        menuItem = new JMenuItem( "Edit Attributes File" );
        _popupMenu.add( menuItem );
        menuItem.addActionListener( new EditAttributesFileListener() );
    }
    
    private void initQueuedPanel()
    {
        _queuedDRDCSPanel.setBorder( BorderFactory.createTitledBorder( "Queued DR/DCSs" ) );
//                                                                                X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _queuedDRDCSPanel, _DRDCSSelectionCB,  0,   0,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( _queuedDRDCSPanel, _uploadButton,      1,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _queuedDRDCSPanel, _fileListPanel,     0,   1,    2,     1, 1, 5, GridBagConstraints.BOTH );
    }

    private void initFailedPanel()
    {
        _failedDRPanel.setBackground( Color.RED );
        _failedDRDCSSelectionCB.setBackground( Color.RED );
        
        _failedDRPanel.setBorder( BorderFactory.createTitledBorder( "Failed DR/DCSs" ) );
//                                                                                   X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _failedDRPanel, _failedDRDCSSelectionCB,  0,   0,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( _failedDRPanel, _resetButton,             1,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _failedDRPanel, _failedFileListPanel,     0,   1,    2,     1, 1, 10, GridBagConstraints.BOTH );
    }

    private void initSuccessPanel()
    {
        _successDRPanel.setBackground( Color.GREEN );
        _successDRDCSSelectionCB.setBackground( Color.GREEN );

        _successDRPanel.setBorder( BorderFactory.createTitledBorder( "Succeeded DR/DCSs" ) );
//                                                                                     X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _successDRPanel, _successDRDCSSelectionCB,  0,   0,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( _successDRPanel, _successFileListPanel,     0,   1,    1,     1, 1, 10, GridBagConstraints.BOTH );
    }
    
    protected void initButtonPanel()
    {
//                                                                       X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _buttonPanel, _resetGUIButton,0,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _buttonPanel, _closeButton,   1,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
    }

    private void initSplitPane()
    {
        _drInfoTextArea.setEditable( false );
        _drInfoTextArea.setPreferredSize( new Dimension( 20, 50 ) );

//        _drInfoPanel.setBorder( BorderFactory.createTitledBorder( "DR Information (from Dimensions)" ) );

        ComponentHelper.addPanelComponent( _drInfoPanel, _drInfoTextArea,  0, 0, 1, 1, 1, 1, GridBagConstraints.BOTH );

        _leftSplitPane = new JSplitPane( JSplitPane.VERTICAL_SPLIT, _queuedDRDCSPanel, _failedDRPanel );
        _rightSplitPane = new JSplitPane( JSplitPane.VERTICAL_SPLIT, _drInfoPanel, _successDRPanel );
        _splitPane = new JSplitPane( JSplitPane.HORIZONTAL_SPLIT, _leftSplitPane, _rightSplitPane );
    }
    
    private void initFrameComponents()
    {
        setLayout( new GridBagLayout() );
        
//                                                                                   X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _frameContentPane, _splitPane,            0,   0,   10,     1, 1, 10, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _frameContentPane, _buttonPanel,          4,   1,    2,     1, 1, 1, GridBagConstraints.BOTH );
    }
    
    private void launchLogViewer()
    {
        LogViewer logViewer = new LogViewer( this );
        logViewer.displayGUI();
    }
    
    private class LogViewerListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            launchLogViewer();
        }
    }
    
    private void initDRList()
    {
        String[] noEntries = { "No Entries Found" };
        File filelistDir = new File( _queuedFileListDir );
        File[] filelistDirFileArray = filelistDir.listFiles();
        List drList = new ArrayList();

        for ( int i = 0; i < filelistDirFileArray.length; i++ )
        {
            File drDir = filelistDirFileArray[ i ];
            drList.add( drDir.getName() );
        }
        Collections.sort( drList );
        if ( ! drList.isEmpty() )
        {
            _DRDCSSelectionCB.setComboBox( drList );
        }
        else
        {
            _DRDCSSelectionCB.setComboBox( noEntries );
        }

        
        filelistDir = new File( _failedFileListDir );
        filelistDirFileArray = filelistDir.listFiles();
        drList = new ArrayList();
        
        for ( int i = 0; i < filelistDirFileArray.length; i++ )
        {
            File drDir = filelistDirFileArray[ i ];
            drList.add( drDir.getName() );
        }
        Collections.sort( drList );
        if ( ! drList.isEmpty() )
        {
            _failedDRDCSSelectionCB.setComboBox( drList );
        }
        else
        {
            _failedDRDCSSelectionCB.setComboBox( noEntries );
        }
        
        filelistDir = new File( _successFileListDir );
        filelistDirFileArray = filelistDir.listFiles();
        drList = new ArrayList();
        
        for ( int i = 0; i < filelistDirFileArray.length; i++ )
        {
            File drDir = filelistDirFileArray[ i ] ;
            drList.add( drDir.getName() );
        }
        
        Collections.sort( drList );
        if ( ! drList.isEmpty() )
        {
            _successDRDCSSelectionCB.setComboBox( drList );
        }
        else
        {
            _successDRDCSSelectionCB.setComboBox( noEntries );
        }
    }
    
    private void updateDRList()
    {
        _DRDCSSelectionCB.removeComboBoxActionListener( _queuedSelectionComboBoxListener );
        _failedDRDCSSelectionCB.removeComboBoxActionListener( _failedDRDCSSelectionComboBoxListener );
        _successDRDCSSelectionCB.removeComboBoxActionListener( _successDRDCSSelectionComboBoxListener );
        
        initDRList();
        
        _DRDCSSelectionCB.addComboBoxActionListener( _queuedSelectionComboBoxListener );
        _failedDRDCSSelectionCB.addComboBoxActionListener( _failedDRDCSSelectionComboBoxListener );
        _successDRDCSSelectionCB.addComboBoxActionListener( _successDRDCSSelectionComboBoxListener );

        _DRDCSSelectionCB.setSelectedIndex( 0 );
        _failedDRDCSSelectionCB.setSelectedIndex( 0 );
        _successDRDCSSelectionCB.setSelectedIndex( 0 );
    }
    

    private void uploadDR()
    {
        if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to upload " + _DRDCSSelectionCB.getSelectedCBItem(), "Upload DR/DCS" ) )
        {
            processAttributesFileDir();
        }
    }
    
    private void resetDRDCS()
    {
        if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to reset " + _failedDRDCSSelectionCB.getSelectedCBItem(), "Reset DR/DCS" ) )
        {
            String queuedFilePath = _queuedFileListDir + _failedDRDCSSelectionCB.getSelectedCBItem();
            String failedFilePath = _failedFileListDir + _failedDRDCSSelectionCB.getSelectedCBItem();
            File queuedDRFile = new File( queuedFilePath );
            File failedDRFile = new File( failedFilePath );
            
            if ( ! queuedDRFile.exists() )
            {
                boolean success = failedDRFile.renameTo( queuedDRFile );
                updateDRList();
            }
        }
    }
    
    private String getAttributeFile( String selectedFile )
    {
        String returnString = _queuedFileListDir + _drNumberString;
        
        String attributeFileString = selectedFile.replace( "/", ":" );
        attributeFileString = attributeFileString.substring( 1 );
        returnString = returnString + "/" + attributeFileString + ".attributes";

        return returnString;
    }
    
    private void editAttributesFile()
    {
        String selectedFile = (String) _fileListPanel.getTheJList().getSelectedValue();

        String attributesFileString = getAttributeFile( selectedFile );
        
        String commandString = "nedit " + attributesFileString;
        
        if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to edit the Attributes file?", "Edit Attributes File" ) )
        {
            if ( attributesFileString != null )
            {
                try
                {
                    Process process = Runtime.getRuntime().exec( commandString );

                    process.waitFor();
                }
                catch ( InterruptedException e )
                {
                    e.printStackTrace();
                }
                catch ( IOException e )
                {
                    e.printStackTrace();
                }
            }
            else
            {
                DialogHelper.displayErrorDialog( this, "You must select a file first", "View File Error" );
            }
        }
    }
    
    private void launchNEdit()
    {
        String selectedFile = (String) _fileListPanel.getTheJList().getSelectedValue();
        String queuedFileString = _dimensionsFileQueueBaseDir + selectedFile;
        
        String commandString = "nedit -read " + queuedFileString;
        
        if ( selectedFile != null )
        {
            try
            {
                Process process = Runtime.getRuntime().exec(commandString);

                process.waitFor();
            }
            catch ( InterruptedException e )
            {
                e.printStackTrace();
            }
            catch ( IOException e )
            {
                e.printStackTrace();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a file first", "View File Error" );
        }
    }

    
    private void launchXDiff()
    {
        String selectedFile = (String) _fileListPanel.getTheJList().getSelectedValue();
        String queuedFileString = _dimensionsFileQueueBaseDir + selectedFile;
        String dimensionsMirrorFileString = _dimensionsMirrorBaseDir + selectedFile;
        String commandString = "xdiff " + queuedFileString + " " + dimensionsMirrorFileString;
        
        File queuedFile = new File( queuedFileString );
        File dimensionsMirrorFile = new File( dimensionsMirrorFileString );

        if ( selectedFile != null )
        {
            if ( ! dimensionsMirrorFile.exists() )
            {
                DialogHelper.displayErrorDialog( this, selectedFile + " does not exist in the Dimensions Mirror", "Compare File Error" );
            }
            else
            {
                try
                {
                    Process process = Runtime.getRuntime().exec(commandString);

                    process.waitFor();
                }
                catch ( InterruptedException e )
                {
                    e.printStackTrace();
                }
                catch ( IOException e )
                {
                    e.printStackTrace();
                }
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a file first", "Compare File Error" );
        }
    }
    
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();
        _DRDCSSelectionCB.addComboBoxActionListener( _queuedSelectionComboBoxListener );
        _failedDRDCSSelectionCB.addComboBoxActionListener( _failedDRDCSSelectionComboBoxListener );
        _successDRDCSSelectionCB.addComboBoxActionListener( _successDRDCSSelectionComboBoxListener );
        _uploadButton.addActionListener( new UploadButtonListener() );
        _resetGUIButton.addActionListener( new ResetGUIButtonListener() );
        _closeButton.addActionListener( windowCloser );
        _resetButton.addActionListener( new ResetButtonListener() );
        _fileListPanel.getTheJList().addMouseListener( new FileListPanelListener() );
    }
    
    private class LaunchXDiffListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            launchXDiff();
        }
    }
    
    private class ViewFileListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            launchNEdit();
        }
    }
    
    private class EditAttributesFileListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            editAttributesFile();
        }
    }
    
    private class FileListPanelListener extends MouseAdapter implements MouseListener
    {
        public void mousePressed(MouseEvent e) 
        {
            maybeShowPopup(e);
        }

        public void mouseReleased(MouseEvent e) {}

        private void maybeShowPopup(MouseEvent e) 
        {
            if ( ( e.getClickCount() == 2 ) && ( e.getButton() == MouseEvent.BUTTON1 ) ) 
            {
                launchNEdit();
            }
            if ( e.isPopupTrigger() ) 
            {
                refreshPopupMenu();
                _popupMenu.show( e.getComponent(), e.getX(), e.getY() );
            }
        }
    }
    
    private class WindowCloserListener extends WindowAdapter implements ActionListener
    {
        public void windowClosing ( WindowEvent e )
        {
            closeWindow();
        }
        
        public void actionPerformed( ActionEvent e )
        {
            closeWindow();
        }
    }

    private void closeWindow()
    /********************
        Purpose: Exits the program gracefully 
     *********************/
    {
        this.dispose();
    }

    private class ResetButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            resetDRDCS();
        }
    }
    
    private class ResetGUIButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            _splitPane.setDividerLocation( .50 );  // 50%
            _leftSplitPane.setDividerLocation( .50 );  // 50%
            _rightSplitPane.setDividerLocation( .50 );  // 50%
        }
    }
    
    private class UploadButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            uploadDR();
            
            if ( _validUsernamePassword )
            {
                if ( _drUploadSuccess )
                {
                    DialogHelper.displayMessageDialog( null, "DR/DCS uploaded successfully.", "Upload DR/DCS" );
                }
                else
                {
                    DialogHelper.displayErrorDialog( null, "Error occurred while uploading DR/DCS.  Check the log for more details.", "Upload DR/DCS" );
                }
            }
            _validUsernamePassword = false;
            _drUploadSuccess = true;
            updateDRList();
        }
    }
    
    private void updateFailedSuccededFileJList( LabeledComboBox lcb, boolean failed )
    {
        try
        {
            FileReader fileReader = null;
            BufferedReader bufferedReader = null;
            List fileList = new ArrayList();
            File drFile = null;
            String headerString = null;
            
            String selectedItem = lcb.getSelectedCBItem();
            if ( failed )
            {
                drFile = new File( _failedFileListDir + "/" + selectedItem );
                if ( selectedItem != null )
                {
                    headerString = "Files that failed for " + selectedItem;
                }
                else
                {
                    headerString = "Files that failed";
                }
            }
            else
            {
                drFile = new File( _successFileListDir + "/" + selectedItem );
                headerString = "Files that succeeded for " + selectedItem;
            }
            
            File[] attributesFileList = drFile.listFiles();

            if ( attributesFileList != null )
            {
                for ( int i = 0; i < attributesFileList.length; i++ )
                {
                    File attributeFile = attributesFileList[ i ];
                    fileReader = new FileReader( attributeFile );
                    bufferedReader = new BufferedReader( fileReader );
                    String line = bufferedReader.readLine();
                    fileList.add( line );
                }
            }
            
            Color panelColor = null;
            
            if ( failed )
            {
                panelColor = Color.RED;
            }
            else
            {
                panelColor = Color.GREEN;
            }
            
            if ( failed )
            {
                if ( _failedFileListPanel == null )
                {
                    _failedFileListPanel = new JListPanel( "", fileList, false, panelColor,
                            false, false, null );
                    _failedFileListPanel.setBorder( new TitledBorder( new LineBorder( Color.LIGHT_GRAY), headerString ) );

                }
                else
                {
                    _failedFileListPanel.setBorder( new TitledBorder( new LineBorder( Color.LIGHT_GRAY), headerString ) );
                    _failedFileListPanel.updateListData( fileList );
                    _failedFileListPanel.refreshJList();
                }
            }
            else
            {
                if ( _successFileListPanel == null )
                {
                    _successFileListPanel = new JListPanel( "", fileList, false, panelColor,
                            false, false, null );
                    _successFileListPanel.setBorder( new TitledBorder( new LineBorder( Color.LIGHT_GRAY), headerString ) );

                }
                else
                {
                    _successFileListPanel.setBorder( new TitledBorder( new LineBorder( Color.LIGHT_GRAY), headerString ) );
                    _successFileListPanel.updateListData( fileList );
                    _successFileListPanel.refreshJList();
                }
            }
            
            refreshDrInfoPanel( selectedItem );
        }

//            _attributesDir = _queuedFileListDir + "/" + selectedItem;
//            _drNumberString = selectedItem;

        catch ( FileNotFoundException f )
        {
        }
        catch ( IOException g )
        {

        }
    }

    private String getStandardError( Process p )
    {
        StreamDrainer drainer = new StreamDrainer(p.getErrorStream());
        return drainer.drain();

    }
    // -----------------------------------------------------------------------

    private String getStandardOutput( Process p )
    {
        StreamDrainer drainer = new StreamDrainer( p.getInputStream() );
        return drainer.drain();
    }

    public String getPaddedString( String str )
    {
        String blankString = "";
        int strLength = 0;
        
        strLength = str.length();
        
        for ( int i = 0; i < ( 9 - strLength); i++ )
        {
            blankString += " ";
        }
        
        return ( str + blankString );
    }


    private void refreshDrInfoPanel( String drNumberString )
    {
        InputStream inputStream = null;
        
        String commandString = " /awips/hydroapps/PVCSFileQueue/scripts/dc_drinfo " + drNumberString;
        String line = null;
        
        try 
        {
            Process process = Runtime.getRuntime().exec(commandString);
//            BufferedReader input = new BufferedReader( new InputStreamReader( process.getInputStream() ) );
//            inputStream = new BufferedInputStream( process.getInputStream() );
//            String stdErrText = getStandardError( process );
            String stdOutText = getStandardOutput( process );
            
            process.waitFor();
//            int ic = inputStream.read();
//            StringBuffer stringBuffer = new StringBuffer();
//            while (ic  != -1 )
//            {
//                char c = (char ) ic;
//                stringBuffer.append( c );
//                System.out.print( c );
//                ic = inputStream.read();
//            }
            _drInfoPanel.setBorder( BorderFactory.createTitledBorder( drNumberString + " DR Information (from Dimensions)" ) );

            _drInfoTextArea.setText( stdOutText );
            _drInfoTextArea.setFont( new Font( "monospaced", Font.PLAIN, 12 ) );
        }
        catch ( InterruptedException e ){}
        catch ( IOException e ){}
        catch ( IllegalMonitorStateException e )
        {
            e.printStackTrace();
        }
    }
    
    
    private void updateQueuedFileJList()
    {
        try
        {
            FileReader fileReader = null;
            BufferedReader bufferedReader = null;
            List fileList = new ArrayList();
            String headerString = "Files that are currently queued up for " + _DRDCSSelectionCB.getSelectedCBItem();

            String selectedItem = _DRDCSSelectionCB.getSelectedCBItem();
            File drFile = new File( _queuedFileListDir + "/" + selectedItem );
            File[] attributesFileList = drFile.listFiles();
            if ( attributesFileList != null )
            {
                for ( int i = 0; i < attributesFileList.length; i++ )
                {
                    File attributeFile = attributesFileList[ i ];
                    fileReader = new FileReader( attributeFile );
                    bufferedReader = new BufferedReader( fileReader );
                    String line = bufferedReader.readLine();
                    fileList.add( line );
                }
            }
            if ( _fileListPanel == null )
            {
                _fileListPanel = new JListPanel( "", fileList, false, Color.LIGHT_GRAY,
                        false, false, null );
                _fileListPanel.setBorder( new TitledBorder( new LineBorder( Color.LIGHT_GRAY), headerString ) );

            }
            else
            {
                _fileListPanel.setBorder( new TitledBorder( new LineBorder( Color.LIGHT_GRAY), headerString ) );
                _fileListPanel.updateListData( fileList );
                _fileListPanel.refreshJList();
            }
            
            _attributesDir = _queuedFileListDir + "/" + selectedItem;
            _drNumberString = selectedItem;
            
            refreshDrInfoPanel( selectedItem );
        }
        catch ( FileNotFoundException f )
        {
        }
        catch ( IOException g )
        {
            
        }
    }
    
    private class FailedDRDCSSelectionComboBoxListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            updateFailedSuccededFileJList( _failedDRDCSSelectionCB, true );
        }
    }
    
    private class SuccessDRDCSSelectionComboBoxListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            updateFailedSuccededFileJList( _successDRDCSSelectionCB, false );
        }
    }

    private class QueuedSelectionComboBoxListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            updateQueuedFileJList();
        }
    }
    
	private void processAttributesFileDir()
	{
		File attributesDirFile = new File( _attributesDir );
		File[] attributesFileArray = attributesDirFile.listFiles();

        getUserNamePassword();
        
        if ( _validUsernamePassword )
        {
            for ( int i = 0; i < attributesFileArray.length; i++ )
            {
                File attributeFile = attributesFileArray[ i ];

                processAttributeFile( attributeFile );
            }
            attributesDirFile.delete();
            updateDRList();
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "Invalid username/password", "Invalid Username/Password" );
        }
	}

	private void processAttributeFile( File attributeFile )
	{
        FileReader fileReader = null;
        BufferedReader bufferedReader = null;
        String line = null;
        String relativeFilePath = null;
        String DRDCSNumberString = null;
        String fileStatusString = null;
        String commentsString = null;
        String PCMScommandString = null;
        
		if ( ! attributeFile.isDirectory() )       // Checks if the local file is a directory
		{
			try
			{
				fileReader = new FileReader( attributeFile );
				bufferedReader = new BufferedReader( fileReader );
				
				line = bufferedReader.readLine(); //read the relateive filepath of the source file
				relativeFilePath = line.substring( 1 );
				_fileToBeUploadedString = relativeFilePath;
				
				line = bufferedReader.readLine(); //read the DR/DCS #
				DRDCSNumberString = line; 
				
				line = bufferedReader.readLine(); //read the status of the file.  
				fileStatusString = line;
				
                line = bufferedReader.readLine(); //read the build level
                _buildString = line;
                setWorkset();
                
				line = bufferedReader.readLine(); //read the comments for the file
				commentsString = line;
			}
			
			catch (FileNotFoundException e)
			{
				
			}
			catch (IOException e)
			{
				
			}

			if ( fileStatusString.equalsIgnoreCase( "OLD" ) )
			{
				PCMScommandString = GetUpdateItemString( relativeFilePath, DRDCSNumberString, commentsString );
			}
			else
			{
				PCMScommandString = GetCreateItemString( relativeFilePath, DRDCSNumberString, commentsString );
			}
			
			RunPCMSCommandLineTool( PCMScommandString, attributeFile, DRDCSNumberString );
		}
	}
	
    private void setWorkset()
    {
        if ( _buildString.equalsIgnoreCase( "ob81" ) )
        {
            _workset = OB8_1_WORKSET;
        }
        else if ( _buildString.equalsIgnoreCase( "ob811" ) )
        {
            _workset = OB8_1_1_WORKSET;
        }
        else if ( _buildString.equalsIgnoreCase( "ob82" ) )
        {
            _workset = OB8_2_WORKSET;
        }
        else if ( _buildString.equalsIgnoreCase( "ob821" ) )
        {
            _workset = OB8_2_1_WORKSET;
        }
        else if ( _buildString.equalsIgnoreCase( "ob83" ) )
        {
            _workset = OB8_3_WORKSET;
        }
        else if ( _buildString.equalsIgnoreCase( "ob831" ) )
        {
            _workset = OB8_3_1_WORKSET;
        }
        else if ( _buildString.equalsIgnoreCase( "ob90" ) )
        {
            _workset = OB9_0_WORKSET;
        }
        else if ( _buildString.equalsIgnoreCase( "ob901" ) )
        {
            _workset = OB9_0_1_WORKSET;
        }
        else if ( _buildString.equalsIgnoreCase( "ob902" ) )
        {
            _workset = OB9_0_2_WORKSET;
        }
        else if ( _buildString.equalsIgnoreCase( "ob91" ) )
        {
            _workset = OB9_1_WORKSET;
        }
        else if ( _buildString.equalsIgnoreCase( "ob92" ) )
        {
            _workset = OB9_2_WORKSET;
        }
        else
        {
            _workset = "INVALID WORKSET";
        }
    }
    
	private void RunPCMSCommandLineTool( String PCMSCommandString, File attributeFile, String DRDCSNumberString )
	{
        String drDCSString = DRDCSNumberString.replaceAll( "AWIPS_", "" );
        String commandString = "/awips/hydroapps/PVCSFileQueue/upload_file_to_dimensions " + _fileToBeUploadedString + 
                               " " + _username + " " + _password + " " + drDCSString + " " + PCMSCommandString;
		String successfulFilePath = "/awips/hydroapps/PVCSFileQueue/filelist.success/" + _drNumberString;
		String failureFilePath = "/awips/hydroapps/PVCSFileQueue/filelist.failed/" + _drNumberString;
		File successfulFilePathFile = new File( successfulFilePath );
		File failureFilePathFile = new File( failureFilePath );
		
        System.out.println( "commandString: " + commandString );
        System.out.println( "PCMSCommandString: " + PCMSCommandString );
		InputStream inputStream = null;
		
		try 
		{
			Process process = Runtime.getRuntime().exec(commandString);
			inputStream = new BufferedInputStream( process.getInputStream() );
			process.waitFor();
			int ic = inputStream.read();
			while (ic  != -1 )
			{
				char c = (char ) ic;
				System.out.print( c );
				ic = inputStream.read();
			}


			if ( process.exitValue() == 0 )
			{
				if ( ! successfulFilePathFile.exists() )
				{
					successfulFilePathFile.mkdir();
				}
				attributeFile.renameTo( new File( successfulFilePath + "/" + attributeFile.getName() ) );
			}
			else 
			{
				if ( ! failureFilePathFile.exists() )
				{
					failureFilePathFile.mkdir();
				}
				attributeFile.renameTo( new File( failureFilePath + "/" + attributeFile.getName() ) );
                _drUploadSuccess = false;
			}
				
		}
		
		catch (InterruptedException e) 
		{
			e.printStackTrace();
		} 
		catch (IOException e) {
			e.printStackTrace();
		}
	}
	
    private void getUserNamePassword()
    {
        Process process = null;
        String commandString = null;
        
        PasswordDialog passwordDialog = new PasswordDialog();
        passwordDialog.requestPasswordByDialog( "Please enter your Dimensions username/password" );
        
        _username = passwordDialog.getUserName();
        _password = passwordDialog.getPassword();
        
        commandString = "/awips/hydroapps/PVCSFileQueue/scripts/check_login_info " + _username + " " + _password;
        try
        {
            process = Runtime.getRuntime().exec(commandString);
            int exitValue = process.waitFor();
            
            System.out.println( "ExitValue = " + exitValue );
            if ( exitValue == 0 )
            {
                _validUsernamePassword = true;
            }
        }
        catch( IOException e )
        {
            e.printStackTrace();
        }
        catch( InterruptedException e )
        {
            e.printStackTrace();
        }
    }

    private String GetCreateItemString( String relativeString, String DRDCSNumberString, String commentsString )
	{
		String CIString = null;
		
//		CI "AWIPS:.AAAA-DEVSRC;1" /PART="AWIPS:SOFTWARE.AAAA;1" /FILENAME="fs/hseb/ob81/ohd/whfs/TEST_EMPTY_FILE.txt" /KEEP 
//		/FORMAT="DEVSRC" /WORKSET="AWIPS:OHD-OB8.1" /CHANGE_DOC_IDS="AWIPS_DR_18222"
		
		CIString = "CI \"AWIPS:.AAAA-DEVSRC;1\" /PART=\"AWIPS:SOFTWARE.AAAA;1\" /FILENAME=\"" + relativeString +
		           "\" /STATUS=\"REVIEW\" /USER_FILENAME=\"" + relativeString + "\" /KEEP /COMMENT=\"" + commentsString + "\" /FORMAT=\"DEVSRC\" /WORKSET=\"" + _workset + "\" " + 
		           " /CHANGE_DOC_IDS=\"" + DRDCSNumberString + "\"";
		
		return CIString;
	}
	
	private String GetUpdateItemString( String relativeString, String DRDCSNumberString, String commentsString )
	{
		String UIString = null;
		
//		UI AWIPS:; /FILENAME="fs/hseb/bld/devl_sys/cmd/update_ticket_no" /KEEP /CHANGE_DOC_IDS="AWIPS_DR_18222" 
//		/COMMENT="TESTING checkin script" /WORKSET="AWIPS:OHD-OB8.1"
		
		UIString = "UI AWIPS:; /FILENAME=\"" + relativeString + "\" /USER_FILENAME=\"" + relativeString + "\" /CHANGE_DOC_IDS=\"" + DRDCSNumberString +
		           "\" /STATUS=\"REVIEW\" /KEEP /COMMENT=\"" + commentsString + "\" /WORKSET=\"" + _workset + "\"";
		
		return UIString;
	}
	
	public static void main(String[] args) 
	{
        NewDimensions_File_Uploader dimensions_file_uploader = new NewDimensions_File_Uploader();
        dimensions_file_uploader.initGUI();
	}
}
