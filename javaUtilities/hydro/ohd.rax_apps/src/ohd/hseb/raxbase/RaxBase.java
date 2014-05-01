package ohd.hseb.raxbase;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
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
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.border.BevelBorder;

import ohd.hseb.raxbase.gui.AdjustFactorEditor;
import ohd.hseb.raxbase.gui.AgencyEditor;
import ohd.hseb.raxbase.gui.AverageEditor;
import ohd.hseb.raxbase.gui.CountiesEditor;
import ohd.hseb.raxbase.gui.CountryEditor;
import ohd.hseb.raxbase.gui.CrestEditor;
import ohd.hseb.raxbase.gui.DataLimitsEditor;
import ohd.hseb.raxbase.gui.Huc2Editor;
import ohd.hseb.raxbase.gui.Huc4Editor;
import ohd.hseb.raxbase.gui.Huc6Editor;
import ohd.hseb.raxbase.gui.Huc8Editor;
import ohd.hseb.raxbase.gui.IngestFilterEditor;
import ohd.hseb.raxbase.gui.LocationEditor;
import ohd.hseb.raxbase.gui.ModCtrlEditor;
import ohd.hseb.raxbase.gui.ProcessAdjustFactorDifferencesDialog;
import ohd.hseb.raxbase.gui.ProcessCrestDifferencesDialog;
import ohd.hseb.raxbase.gui.ProcessDataLimitsDifferencesDialog;
import ohd.hseb.raxbase.gui.ProcessIngestFilterDifferencesDialog;
import ohd.hseb.raxbase.gui.ProcessLocDataLimitsDifferencesDialog;
import ohd.hseb.raxbase.gui.ProcessLocationDifferencesDialog;
import ohd.hseb.raxbase.gui.ProcessRatingDifferencesDialog;
import ohd.hseb.raxbase.gui.ProcessReservoirDifferencesDialog;
import ohd.hseb.raxbase.gui.ProcessRiverCritDifferencesDialog;
import ohd.hseb.raxbase.gui.ProdEditor;
import ohd.hseb.raxbase.gui.RatingCurveEditor;
import ohd.hseb.raxbase.gui.ReservoirEditor;
import ohd.hseb.raxbase.gui.RfcEditor;
import ohd.hseb.raxbase.gui.RiverCritEditor;
import ohd.hseb.raxbase.gui.SensOKEditor;
import ohd.hseb.raxbase.gui.ShefDurationEditor;
import ohd.hseb.raxbase.gui.ShefExtremumEditor;
import ohd.hseb.raxbase.gui.ShefPE1Editor;
import ohd.hseb.raxbase.gui.ShefPEEditor;
import ohd.hseb.raxbase.gui.ShefPETransEditor;
import ohd.hseb.raxbase.gui.ShefProbEditor;
import ohd.hseb.raxbase.gui.ShefQCEditor;
import ohd.hseb.raxbase.gui.ShefTSEditor;
import ohd.hseb.raxbase.gui.SlopeProfileEditor;
import ohd.hseb.raxbase.gui.StateEditor;
import ohd.hseb.raxbase.gui.StatusBar;
import ohd.hseb.raxbase.gui.WfoHsaEditor;
import ohd.hseb.raxbase.model.RaxLocation;
import ohd.hseb.raxbase.table.ArcBaseLocationJTableRowData;
import ohd.hseb.raxbase.util.CursorController;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class RaxBase extends JFrame
{
    private Container _frameContentPane = getContentPane();

    private JMenuBar _menuBar = null;
    private JPopupMenu _popupMenu = new JPopupMenu();
    private JPanel _filterPanel = new JPanel( new FlowLayout() );
    private JLabel _filterLabel = new JLabel( "LID: " );
    private JTextField _filterTextField = new JTextField();
    private JButton _filterButton = new JButton( "Filter" );
    private StatusBar _statusBar = new StatusBar();

    private String _dateString = "May 16, 2008";
    private String _buildString = "OB8.3";
    private String _versionString = "Application:     RaxBase" + 
    "\nVersion:           " + _buildString + 
    "\nDate:               " + _dateString + 
    "\nDeveloped By: National Weather Service" + 
    "\n                       Office of Hydrologic Development" + 
    "\n                       Hydrology Laboratory";
    private RaxBaseDataMgr _dataMgr = null;

    private String _titleString = null;

    private boolean _filtered = false;

//  JTable variables
    private JTableManager _arcBaseTableManager = null;
    private List _arcBaseColumnDescriptorList = null;
    private List _allRowDataList = null;
    private JScrollPane _tableScrollPane = null;

    public RaxBase( RaxBaseDataMgr dataMgr )
    {
        _dataMgr = dataMgr;
        _dataMgr.initRaxLocationRowDataList();
        initGui();
        setJMenuBar( _menuBar );
        setTitle( "RaxBase" );
    }

    /**
     * Initializes the GUI for the main window
     *
     */    
    private void initGui()
    {
        setSize( 1024, 768 );

        setLayout( new GridBagLayout() );
        initMenuBar();
        initFilterPanel();
        initJTable();
        initMainWindow();
        refreshPopupMenu();
        addListeners();
    }

    /**
     * Initialize the ArcBase JTable
     *
     * @return None
     *
     */    
    private void initJTable() 
    {
        GridBagConstraints mainGbc = new GridBagConstraints();

        mainGbc.fill = GridBagConstraints.BOTH;
        mainGbc.weightx = 1;
        mainGbc.weighty = 1;

        _allRowDataList = new ArrayList();
        setArcBaseColumnDescriptorList();
        _arcBaseTableManager = new ComplexJTableManager( _arcBaseColumnDescriptorList, _allRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _arcBaseTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _arcBaseTableManager.setDisplayableColumns( columnsSelected, true, true );
        _arcBaseTableManager.setPreferredSizeForJScrollPane( new Dimension( 1020, 600 ) );
        _tableScrollPane = _arcBaseTableManager.getJScrollPane();

        JPanel tableScrollPanePanel = new JPanel(); 
        tableScrollPanePanel.add( _tableScrollPane );
        tableScrollPanePanel.setMinimumSize( new Dimension( 1030,710 ) );
        JPanel tablePanel = new JPanel();
        tablePanel.add( tableScrollPanePanel );

        //                                                                     X,  Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _frameContentPane, tablePanel,      0,  3,    3,     30, 1, 1, GridBagConstraints.BOTH );
    }

    /**
     * Sets up the Columns for the arcbase jtable
     *
     */    
    private void setArcBaseColumnDescriptorList()
    {
        _arcBaseColumnDescriptorList = new ArrayList();

        _arcBaseColumnDescriptorList.add(new JTableColumnDescriptor( "Station", true, 100, "center" ) );
        _arcBaseColumnDescriptorList.add(new JTableColumnDescriptor( "Begin Date", true, 100, "center" ) );
        _arcBaseColumnDescriptorList.add(new JTableColumnDescriptor( "End Date", true, 100, "center" ) );
        _arcBaseColumnDescriptorList.add(new JTableColumnDescriptor( "Latitude", true, 100, "center" ) );
        _arcBaseColumnDescriptorList.add(new JTableColumnDescriptor( "Longitude", true, 100, "center" ) );
        _arcBaseColumnDescriptorList.add(new JTableColumnDescriptor( "Name", true, 225, "center" ) );
        _arcBaseColumnDescriptorList.add(new JTableColumnDescriptor( "Detailed Info", true, 225, "center" ) );
        _arcBaseColumnDescriptorList.add(new JTableColumnDescriptor( "HSA", true, 50, "center" ) );

        _arcBaseColumnDescriptorList.add(new JTableColumnDescriptor( "Elevation", false, 50, "center" ) );
        _arcBaseColumnDescriptorList.add(new JTableColumnDescriptor( "State", false, 50, "center" ) );
        _arcBaseColumnDescriptorList.add(new JTableColumnDescriptor( "HUC", false, 50, "center" ) );
        _arcBaseColumnDescriptorList.add(new JTableColumnDescriptor( "WFO", false, 50, "center" ) );
        _arcBaseColumnDescriptorList.add(new JTableColumnDescriptor( "RFC", false, 50, "center" ) );

    }

    /**
     * Initializes the Main window
     *
     */
    private void initMainWindow()
    {
        JPanel horizontalPanel = new JPanel();
        horizontalPanel.setPreferredSize(new Dimension( 50, 50 ) );

        //                                                                         X,  Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _frameContentPane, horizontalPanel,     0,  1,    3,     1, 1, 0, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _frameContentPane, _statusBar,          0,  33,    3,     1, 1, 0, GridBagConstraints.BOTH);
    }

    private void refreshPopupMenu()
    {
        _popupMenu.removeAll();

        String lidString = "";
        RaxLocation raxLocation = null;
        String dashString = "";

        int rows[] = _arcBaseTableManager.getSelectedRowIndices();
        if ( rows.length != 0 )
        {
            int rowIndex = rows[ 0 ];
            ArcBaseLocationJTableRowData arcBaseLocationJTableRowData = (ArcBaseLocationJTableRowData) _arcBaseTableManager.getSelectedRowData( rowIndex );
            raxLocation = _dataMgr.getRaxLocationFromJTableRowData( arcBaseLocationJTableRowData );
            lidString = raxLocation.getLid() + " options";
        }

//      JMenuItem menuItem = new JMenuItem( lidString );
//      _popupMenu.add( menuItem );
//      _popupMenu.setBorder( new LineBorder( Color.BLACK, 2 ) );
//      _popupMenu.setBorder( BorderFactory.createBevelBorder( BevelBorder.RAISED, Color.GRAY, Color.BLACK ) );

//      _popupMenu.setBorder( BorderFactory.createTitledBorder( new LineBorder( Color.BLACK, 2 ), lidString ) );
        _popupMenu.setBorder( BorderFactory.createTitledBorder( BorderFactory.createBevelBorder( BevelBorder.RAISED, Color.GRAY, Color.BLACK ), lidString ) );

//      for ( int i = 0; i < lidString.length(); i++ )
//      {
//      dashString += "-";
//      }

//      menuItem = new JMenuItem( dashString );
//      _popupMenu.add( menuItem );

        JMenuItem menuItem = null;

        menuItem = new JMenuItem( "Modify Location" );
        _popupMenu.add( menuItem );
        menuItem.addActionListener( new LaunchModifyLocationEditorGUI() );
        menuItem = new JMenuItem( "River Gage" );
        _popupMenu.add( menuItem );
        menuItem.addActionListener( new LaunchRiverCritEditorGUI() );
        menuItem = new JMenuItem( "Rating Information" );
        _popupMenu.add( menuItem );
        menuItem.addActionListener( new LaunchRatingCurveEditorGUI() );
        menuItem = new JMenuItem( "Crest History" );
        _popupMenu.add( menuItem );
        menuItem.addActionListener( new LaunchCrestHistoryEditorGUI() );
        menuItem = new JMenuItem( "Slope Profile" );
        _popupMenu.add( menuItem );
        menuItem.addActionListener( new LaunchSlopeProfileEditorGUI() );
        menuItem = new JMenuItem( "Average" );
        _popupMenu.add( menuItem );
        menuItem.addActionListener( new LaunchAverageEditorGUI() );
        menuItem = new JMenuItem( "Reservoir" );
        _popupMenu.add( menuItem );
        menuItem.addActionListener( new LaunchReservoirEditorGUI() );
        menuItem = new JMenuItem( "SensOK" );
        _popupMenu.add( menuItem );
        menuItem.addActionListener( new LaunchSensOKEditor() );
    }

    /**
     * Initializes the Filter Panel
     *
     */
    private void initFilterPanel()
    {

        _filterTextField.setColumns( 10 );
        _filterPanel.add( _filterLabel);
        _filterPanel.add( _filterTextField );
        _filterPanel.add( _filterButton );
//      X,  Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _frameContentPane, _filterPanel,        0,  0,    3,     1, GridBagConstraints.BOTH );
    }

    /**
     * Adds listeneres to various components in the GUI
     * 
     * @return NONE
     *
     */
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        addWindowListener( windowCloser );

        _filterButton.addActionListener( new FilterButtonListener() );
        MouseListener doubleClickListener = new DoubleClickListener();
        MouseListener popupListener = new PopupListener();
        _arcBaseTableManager.addTableListener( doubleClickListener );
        _arcBaseTableManager.addTableListener( popupListener );
    }

    private class PopupListener extends MouseAdapter implements MouseListener
    {
        public void mousePressed(MouseEvent e) 
        {
            maybeShowPopup(e);
        }

        public void mouseReleased(MouseEvent e) 
        {
            maybeShowPopup(e);
        }

        private void maybeShowPopup(MouseEvent e) 
        {

            if (e.isPopupTrigger()) 
            {
                refreshPopupMenu();
                _popupMenu.show(e.getComponent(),
                        e.getX(), e.getY());
            }
        }
    }

    /**
     * Listener for the JTable to allow the user to double click on a location entry
     * @author gsood
     *
     */
    private class DoubleClickListener implements MouseListener
    {
        public void mouseClicked( MouseEvent e )
        {
            if ( ( e.getClickCount() == 2 ) && ( e.getButton() == MouseEvent.BUTTON1 ) ) 
            {
                boolean newLocation = false;
                LaunchLocationEditor( newLocation );
            }
        }

        public void mouseEntered( MouseEvent e ){}
        public void mouseExited( MouseEvent e ){}
        public void mousePressed( MouseEvent e ){}
        public void mouseReleased( MouseEvent e ){}
    }

    /**
     * Listener for closing the main application window
     * @author gsood
     *
     */
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

    /**
     * Location Editor (Add) Launcher Listener
     * @author gsood
     *
     */
    private class LaunchAddLocationEditorGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            boolean newLocation = true;
            LaunchLocationEditor( newLocation );
        }
    }

    /**
     * Location Editor (Modify) Launcher Listener
     * @author gsood
     *
     */
    private class LaunchModifyLocationEditorGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( _arcBaseTableManager.getSelectedRowIndices().length == 0 )
            {
                DialogHelper.displayErrorDialog( RaxBase.this, "You must select a Location first", "Modify Location" );
            }
            else
            {
                boolean newLocation = false;
                LaunchLocationEditor( newLocation );
            }
        }
    }

    /**
     * River Crit Editor Launcher Listener
     * @author gsood
     *
     */
    private class LaunchRiverCritEditorGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( _arcBaseTableManager.getSelectedRowIndices().length == 0 )
            {
                DialogHelper.displayErrorDialog( RaxBase.this, "You must select a Location first", "RiverCrit Editor" );
            }
            else
            {
                LaunchRiverCritEditor();
            }
        }
    }

    /**
     * Rating Curve Editor Launcher Listener
     * @author gsood
     *
     */
    private class LaunchRatingCurveEditorGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( _arcBaseTableManager.getSelectedRowIndices().length == 0 )
            {
                DialogHelper.displayErrorDialog( RaxBase.this, "You must select a Location first", "Rating Curve Editor" );
            }
            else
            {
                LaunchRatingCurveEditor();
            }
        }
    }

    /**
     * Crest History Editor Launcher Listener
     * @author gsood
     *
     */
    private class LaunchCrestHistoryEditorGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( _arcBaseTableManager.getSelectedRowIndices().length == 0 )
            {
                DialogHelper.displayErrorDialog( RaxBase.this, "You must select a Location first", "Crest History Editor" );
            }
            else
            {
                LaunchCrestEditor();
            }
        }
    }

    /**
     * Slope Profile Editor Launcher Listener
     * @author gsood
     *
     */
    private class LaunchSlopeProfileEditorGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( _arcBaseTableManager.getSelectedRowIndices().length == 0 )
            {
                DialogHelper.displayErrorDialog( RaxBase.this, "You must select a Location first", "Slope Profile Editor" );
            }
            else
            {
                LaunchSlopeProfileEditor();
            }
        }
    }

    /**
     * Average Editor Launcher Listener
     * @author gsood
     *
     */
    private class LaunchAverageEditorGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( _arcBaseTableManager.getSelectedRowIndices().length == 0 )
            {
                DialogHelper.displayErrorDialog( RaxBase.this, "You must select a Location first", "Average Editor" );
            }
            else
            {
                LaunchAverageEditor();
            }
        }
    }

    /**
     * Reservoir Editor Launcher Editor
     * @author gsood
     *
     */
    private class LaunchReservoirEditorGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( _arcBaseTableManager.getSelectedRowIndices().length == 0 )
            {
                DialogHelper.displayErrorDialog( RaxBase.this, "You must select a Location first", "Reservoir Editor" );
            }
            else
            {
                LaunchReservoirEditor();
            }
        }
    }

    /**
     * IngestFilter Editor Launcher Listener
     * @author gsood
     *
     */
    private class LaunchIngestFilterEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchIngestFilterEditor();
        }
    }

    /**
     * AdjustFactor Editor Launcher Listener
     * @author gsood
     *
     */
    private class LaunchAdjustFactorEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchAdjustFactorEditor();
        }
    }

    /**
     * ProcessAdjustFactor Launcher
     * @author gsood
     *
     */
    private class LaunchProcessAdjustFactorGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchProcessAdjustFactorGUI();
        }
    }

    /**
     * ProcessRiverCrit Launcher
     * @author gsood
     *
     */
    private class LaunchProcessRiverCritGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchProcessRiverCritGUI();
        }
    }

    /**
     * ProcessLocation Launcher
     * @author gsood
     *
     */
    private class LaunchProcessLocationGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchProcessLocationGUI();
        }
    }

    private class LaunchModCtrlEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchModCtrlEditor();
        }
    }

    private class LaunchCountryEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchCountryEditor();
        }
    }

    private class LaunchHuc2Editor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchHuc2Editor();
        }
    }

    private class LaunchHuc4Editor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchHuc4Editor();
        }
    }

    private class LaunchHuc6Editor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchHuc6Editor();
        }
    }

    private class LaunchWfoHsaEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchWfoHsaEditor();
        }
    }

    private class LaunchShefPETransEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchShefPETransEditor();
        }
    }

    private class LaunchProdEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchProdEditor();
        }
    }

    private class LaunchAgencyEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchAgencyEditor();
        }
    }

    private class LaunchShefTSEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchShefTSEditor();
        }
    }

    private class LaunchShefQCEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchShefQCEditor();
        }
    }

    private class LaunchShefProbEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchShefProbEditor();
        }
    }

    private class LaunchShefPE1Editor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchShefPE1Editor();
        }
    }

    private class LaunchShefPEEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchShefPEEditor();
        }
    }

    private class LaunchShefExEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchShefExEditor();
        }
    }

    private class LaunchShefDurEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchShefDurEditor();
        }
    }

    private class LaunchRfcEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchRfcEditor();
        }
    }

    private class LaunchHuc8Editor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchHuc8Editor();
        }
    }

    private class LaunchCountiesEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchCountiesEditor();
        }
    }

    private class LaunchStateEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchStateEditor();
        }
    }
    /**
     * ProcessIngestFilter Launcher
     * @author gsood
     *
     */
    private class LaunchProcessIngestFilterGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchProcessIngestFilterGUI();
        }
    }

    /**
     * ProcessCrest Launcher
     * @author gsood
     *
     */
    private class LaunchProcessCrestGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchProcessCrestGUI();
        }
    }

    private class LaunchProcessRatingGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchProcessRatingGUI();
        }
    }

    /**
     * ProcessDataLimits Launcher
     * @author gsood
     *
     */
    private class LaunchProcessDataLimitsGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchProcessDataLimitsGUI();
        }
    }

    /**
     * ProcessLocDataLimits Launcher
     * @author gsood
     *
     */
    private class LaunchProcessLocDataLimitsGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchProcessLocDataLimitsGUI();
        }
    }

    /**
     * ProcessReservoir Launcher
     * @author gsood
     *
     */
    private class LaunchProcessReservoirGUI implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchProcessReservoirGUI();
        }
    }


    private class LaunchDataLimitsEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            LaunchDataLimitsEditor();
        }
    }

    private class LaunchSensOKEditor implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( _arcBaseTableManager.getSelectedRowIndices().length == 0 )
            {
                DialogHelper.displayErrorDialog( RaxBase.this, "You must select a Location first", "Sensok Editor" );
            }
            else
            {
                LaunchSensOKEditor();
            }
        }

    }

    private class DisplayAboutDialog implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            DialogHelper.displayMessageDialog( RaxBase.this, _versionString );
        }
    }

    private class DisplayPreferencesListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            LaunchDisplayPreferences();
        }
    }


    private void LaunchDisplayPreferences()
    {
        _arcBaseTableManager.invokeColumnSelector( this );
    }

    private void LaunchLocationEditor( boolean newLocation )
    {
        RaxLocation raxLocation = null;

        if ( ! newLocation ) // modify a location
        {
            int rowIndex = ( _arcBaseTableManager.getSelectedRowIndices() )[ 0 ];
            ArcBaseLocationJTableRowData arcBaseLocationJTableRowData = (ArcBaseLocationJTableRowData) _arcBaseTableManager.getSelectedRowData( rowIndex );
            raxLocation = _dataMgr.getRaxLocationFromJTableRowData( arcBaseLocationJTableRowData );
        }
        LocationEditor locEditorGUI = new LocationEditor( this, _dataMgr, raxLocation );
        locEditorGUI.displayGUI();
        _dataMgr.updateRaxLocationRowDataList();
        if ( _filtered )
        {
            filterLids();
        }
    }

    private void updateTitleString( RaxLocation raxLocation )
    {
        String lid = raxLocation.getLid();

        _titleString = lid;

        if ( raxLocation.getName() != null )
        {
            _titleString += " - " + raxLocation.getName();
        }
    }

    private void LaunchRiverCritEditor()
    {
        RaxLocation raxLocation = null;
        String lid = null;

        int rowIndex = ( _arcBaseTableManager.getSelectedRowIndices() )[ 0 ];
        ArcBaseLocationJTableRowData arcBaseLocationJTableRowData = (ArcBaseLocationJTableRowData) _arcBaseTableManager.getSelectedRowData( rowIndex );
        raxLocation = _dataMgr.getRaxLocationFromJTableRowData( arcBaseLocationJTableRowData );

        updateTitleString( raxLocation );

        lid = raxLocation.getLid();

        RiverCritEditor riverCritEditorGUI = new RiverCritEditor( this, _dataMgr, lid, _titleString );
        riverCritEditorGUI.displayGUI();
    }

    private void LaunchRatingCurveEditor()
    {
        RaxLocation raxLocation = null;
        String lid = null;

        int rowIndex = ( _arcBaseTableManager.getSelectedRowIndices() )[ 0 ];
        ArcBaseLocationJTableRowData arcBaseLocationJTableRowData = (ArcBaseLocationJTableRowData) _arcBaseTableManager.getSelectedRowData( rowIndex );
        raxLocation = _dataMgr.getRaxLocationFromJTableRowData( arcBaseLocationJTableRowData );
        lid = raxLocation.getLid();
        updateTitleString( raxLocation );

        RatingCurveEditor ratingCurveEditor = new RatingCurveEditor( this, _dataMgr, lid, _titleString );
        ratingCurveEditor.displayGUI();
    }

    private void LaunchCrestEditor()
    {
        RaxLocation raxLocation = null;
        String lid = null;

        int rowIndex = ( _arcBaseTableManager.getSelectedRowIndices() )[ 0 ];
        ArcBaseLocationJTableRowData arcBaseLocationJTableRowData = (ArcBaseLocationJTableRowData) _arcBaseTableManager.getSelectedRowData( rowIndex );
        raxLocation = _dataMgr.getRaxLocationFromJTableRowData( arcBaseLocationJTableRowData );
        lid = raxLocation.getLid();
        updateTitleString( raxLocation );

        CrestEditor crestEditorGUI = new CrestEditor( this, _dataMgr, lid, _titleString );
        crestEditorGUI.displayGUI();
    }

    private void LaunchSlopeProfileEditor()
    {
        RaxLocation raxLocation = null;
        String lid = null;

        int rowIndex = ( _arcBaseTableManager.getSelectedRowIndices() )[ 0 ];
        ArcBaseLocationJTableRowData arcBaseLocationJTableRowData = (ArcBaseLocationJTableRowData) _arcBaseTableManager.getSelectedRowData( rowIndex );
        raxLocation = _dataMgr.getRaxLocationFromJTableRowData( arcBaseLocationJTableRowData );
        lid = raxLocation.getLid();
        updateTitleString( raxLocation );

        SlopeProfileEditor slopeProfileEditor = new SlopeProfileEditor( this, _dataMgr, lid, _titleString );
        slopeProfileEditor.displayGUI();
    }

    private void LaunchAverageEditor()
    {
        RaxLocation raxLocation = null;
        String lid = null;

        int rowIndex = ( _arcBaseTableManager.getSelectedRowIndices() )[ 0 ];
        ArcBaseLocationJTableRowData arcBaseLocationJTableRowData = (ArcBaseLocationJTableRowData) _arcBaseTableManager.getSelectedRowData( rowIndex );
        raxLocation = _dataMgr.getRaxLocationFromJTableRowData( arcBaseLocationJTableRowData );
        lid = raxLocation.getLid();
        updateTitleString( raxLocation );

        AverageEditor averageEditor = new AverageEditor( this, _dataMgr, lid, _titleString );
        averageEditor.displayGUI();
    }

    private void LaunchReservoirEditor()
    {
        RaxLocation raxLocation = null;
        String lid = null;

        int rowIndex = ( _arcBaseTableManager.getSelectedRowIndices() )[ 0 ];
        ArcBaseLocationJTableRowData arcBaseLocationJTableRowData = (ArcBaseLocationJTableRowData) _arcBaseTableManager.getSelectedRowData( rowIndex );
        raxLocation = _dataMgr.getRaxLocationFromJTableRowData( arcBaseLocationJTableRowData );
        lid = raxLocation.getLid();
        updateTitleString( raxLocation );

        ReservoirEditor reservoirEditorGUI = new ReservoirEditor( this, _dataMgr, lid, _titleString );
        reservoirEditorGUI.displayGUI();
    }

    private void LaunchSensOKEditor()
    {
        RaxLocation raxLocation = null;
        String lid = null;

        int rowIndex = ( _arcBaseTableManager.getSelectedRowIndices() )[ 0 ];
        ArcBaseLocationJTableRowData arcBaseLocationJTableRowData = (ArcBaseLocationJTableRowData) _arcBaseTableManager.getSelectedRowData( rowIndex );
        raxLocation = _dataMgr.getRaxLocationFromJTableRowData( arcBaseLocationJTableRowData );
        lid = raxLocation.getLid();
        updateTitleString( raxLocation );

        SensOKEditor sensOKEditorGUI = new SensOKEditor( this, _dataMgr, lid, _titleString );
        sensOKEditorGUI.displayGUI();

    }

    private void LaunchIngestFilterEditor()
    {
        IngestFilterEditor ingestFilterEditorGUI = new IngestFilterEditor( this, _dataMgr );
        ingestFilterEditorGUI.displayGUI();
    }

    private void LaunchAdjustFactorEditor()
    {
        AdjustFactorEditor adjustFactorEditorGUI = new AdjustFactorEditor( this, _dataMgr );
        adjustFactorEditorGUI.displayGUI();
    }

    private void LaunchProcessAdjustFactorGUI()
    {
        if ( _dataMgr.initProcessDataMgr() )
        {
            ProcessAdjustFactorDifferencesDialog procAdjFactorDiffGUI = new ProcessAdjustFactorDifferencesDialog( this, _dataMgr.getRaxSyncDataMgr() );
            procAdjFactorDiffGUI.displayGUI();
        }
    }

    private void LaunchProcessRiverCritGUI()
    {
        if ( _dataMgr.initProcessDataMgr() )
        {
            ProcessRiverCritDifferencesDialog procRiverCritDiffGUI = new ProcessRiverCritDifferencesDialog( this, _dataMgr.getRaxSyncDataMgr() );
            procRiverCritDiffGUI.displayGUI();
        }
    }

    private void LaunchProcessLocationGUI()
    {
        if ( _dataMgr.initProcessDataMgr() )
        {
            ProcessLocationDifferencesDialog procLocationDiffGUI = new ProcessLocationDifferencesDialog( this, _dataMgr.getRaxSyncDataMgr() );
            procLocationDiffGUI.displayGUI();
        }
    }

    private void LaunchModCtrlEditor()
    {
        ModCtrlEditor modCtrlEditor = new ModCtrlEditor( this, _dataMgr );
        modCtrlEditor.displayGUI();
    }

    private void LaunchCountryEditor()
    {
        CountryEditor countryEditor = new CountryEditor( this, _dataMgr );
        countryEditor.displayGUI();
    }

    private void LaunchCountiesEditor()
    {
        CountiesEditor countiesEditor = new CountiesEditor( this, _dataMgr );
        countiesEditor.displayGUI();
    }

    private void LaunchHuc2Editor()
    {
        Huc2Editor huc2Editor = new Huc2Editor( this, _dataMgr );
        huc2Editor.displayGUI();
    }

    private void LaunchHuc4Editor()
    {
        Huc4Editor huc4Editor = new Huc4Editor( this, _dataMgr );
        huc4Editor.displayGUI();
    }

    private void LaunchShefPETransEditor()
    {
        ShefPETransEditor shefPeTransEditor = new ShefPETransEditor( this, _dataMgr );
        shefPeTransEditor.displayGUI();
    }

    private void LaunchProdEditor()
    {
        ProdEditor prodEditor = new ProdEditor( this, _dataMgr );
        prodEditor.displayGUI();
    }

    private void LaunchAgencyEditor()
    {
        AgencyEditor agencyEditor = new AgencyEditor( this, _dataMgr );
        agencyEditor.displayGUI();
    }

    private void LaunchShefTSEditor()
    {
        ShefTSEditor shefTSEditor = new ShefTSEditor( this, _dataMgr );
        shefTSEditor.displayGUI();
    }

    private void LaunchShefQCEditor()
    {
        ShefQCEditor shefQCEditor = new ShefQCEditor( this, _dataMgr );
        shefQCEditor.displayGUI();
    }

    private void LaunchShefProbEditor()
    {
        ShefProbEditor shefProbEditor = new ShefProbEditor( this, _dataMgr );
        shefProbEditor.displayGUI();
    }

    private void LaunchShefPE1Editor()
    {
        ShefPE1Editor shefPe1Editor = new ShefPE1Editor( this, _dataMgr );
        shefPe1Editor.displayGUI();
    }

    private void LaunchShefPEEditor()
    {
        ShefPEEditor shefPeEditor = new ShefPEEditor( this, _dataMgr );
        shefPeEditor.displayGUI();
    }

    private void LaunchShefExEditor()
    {
        ShefExtremumEditor shefExEditor = new ShefExtremumEditor( this, _dataMgr );
        shefExEditor.displayGUI();
    }

    private void LaunchShefDurEditor()
    {
        ShefDurationEditor shefDurEditor = new ShefDurationEditor( this, _dataMgr );
        shefDurEditor.displayGUI();
    }

    private void LaunchRfcEditor()
    {
        RfcEditor rfcEditor = new RfcEditor( this, _dataMgr );
        rfcEditor.displayGUI();
    }

    private void LaunchWfoHsaEditor()
    {
        WfoHsaEditor wfoHsaEditor = new WfoHsaEditor( this, _dataMgr );
        wfoHsaEditor.displayGUI();
    }

    private void LaunchHuc8Editor()
    {
        Huc8Editor huc8Editor = new Huc8Editor( this, _dataMgr );
        huc8Editor.displayGUI();
    }

    private void LaunchHuc6Editor()
    {
        Huc6Editor huc6Editor = new Huc6Editor( this, _dataMgr );
        huc6Editor.displayGUI();
    }

    private void LaunchStateEditor()
    {
        StateEditor stateEditor = new StateEditor( this, _dataMgr );
        stateEditor.displayGUI();
    }

    private void LaunchProcessIngestFilterGUI()
    {
        if ( _dataMgr.initProcessDataMgr() )
        {
            ProcessIngestFilterDifferencesDialog procIngestFilterDiffGUI = new ProcessIngestFilterDifferencesDialog( this, _dataMgr.getRaxSyncDataMgr() );
            procIngestFilterDiffGUI.displayGUI();
        }
    }

    private void LaunchProcessRatingGUI()
    {
        if ( _dataMgr.initProcessDataMgr() )
        {
            ProcessRatingDifferencesDialog procRatingDiffGUI = new ProcessRatingDifferencesDialog( this, _dataMgr.getRaxSyncDataMgr() );
            procRatingDiffGUI.displayGUI();
        }
    }

    private void LaunchProcessCrestGUI()
    {
        if ( _dataMgr.initProcessDataMgr() )
        {
            ProcessCrestDifferencesDialog procCrestDiffGUI = new ProcessCrestDifferencesDialog( this, _dataMgr.getRaxSyncDataMgr() );
            procCrestDiffGUI.displayGUI();
        }
    }

    private void LaunchProcessReservoirGUI()
    {
        if ( _dataMgr.initProcessDataMgr() )
        {
            ProcessReservoirDifferencesDialog procResvDiffGUI = new ProcessReservoirDifferencesDialog( this, _dataMgr.getRaxSyncDataMgr() );
            procResvDiffGUI.displayGUI();
        }
    }

    private void LaunchProcessDataLimitsGUI()
    {
        if ( _dataMgr.initProcessDataMgr() )
        {
            ProcessDataLimitsDifferencesDialog procDataLimitsDiffGUI = new ProcessDataLimitsDifferencesDialog( this, _dataMgr.getRaxSyncDataMgr() );
            procDataLimitsDiffGUI.displayGUI();
        }
    }

    private void LaunchProcessLocDataLimitsGUI()
    {
        if ( _dataMgr.initProcessDataMgr() )
        {
            ProcessLocDataLimitsDifferencesDialog procLocDataLimitsDiffGUI = new ProcessLocDataLimitsDifferencesDialog( this, _dataMgr.getRaxSyncDataMgr() );
            procLocDataLimitsDiffGUI.displayGUI();
        }
    }

    private void LaunchDataLimitsEditor()
    {
        DataLimitsEditor dataLimitsEditorGUI = new DataLimitsEditor( this, _dataMgr );
        dataLimitsEditorGUI.displayGUI();
    }

    private void filterLids()
    {
        _filtered = true;
        String filterString = _filterTextField.getText().toUpperCase().trim();
        _allRowDataList = _dataMgr.getFilteredRaxLocationRowDataList( filterString );
        _arcBaseTableManager.setChangedAllRowDataList( _allRowDataList );
        _arcBaseTableManager.refreshDisplay();
        _statusBar.setMessage( "Record count: " + _allRowDataList.size() );
    }

    private void closeWindow()
    /********************
        Purpose: Exits the program gracefully 
     *********************/
    {
        this.dispose();
        System.exit( 1 );
    }


    /**
     * Filter Button listener.  This will take the text out of the _filterTextField and use it to filter the list
     * of locations coming from the Location table in the Rax Database.
     * 
     *
     */
    private class FilterButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e ) 
        {
            filterLids();
        }
    }

    /**
     * Initializes the Menu Bar for the main GUI
     * 
     * @return NONE
     *
     */
    private void initMenuBar()
    {
        _menuBar = new JMenuBar();

        ActionListener cursorListener = null;

        JMenu menu = null;
        JMenuItem menuItem = null;

        //File Menu

        menu = new JMenu( "File" );
        menu.setMnemonic( KeyEvent.VK_F );
        menu.getAccessibleContext().setAccessibleDescription( "Access File Menus" );
        _menuBar.add( menu );

        menuItem = new JMenuItem( "Preferences" );
        menuItem.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_P, ActionEvent.ALT_MASK ) );
        menuItem.getAccessibleContext().setAccessibleDescription( "Open preferences window" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new DisplayPreferencesListener() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem("Exit Application");
        menuItem.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_Q, ActionEvent.ALT_MASK ) );
        menuItem.getAccessibleContext().setAccessibleDescription( "Exit the Application" );
        menu.add(menuItem); 
        menuItem.addActionListener( new WindowCloserListener() );

        //======================

        menu = new JMenu( "Location" );
        menu.setMnemonic( KeyEvent.VK_L );
        menu.getAccessibleContext().setAccessibleDescription( "Access Location Menus" );
        _menuBar.add( menu );

        menuItem = new JMenuItem( "Add Location" );
        menuItem.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_A, ActionEvent.ALT_MASK ) );
        menuItem.getAccessibleContext().setAccessibleDescription( "Add a Location" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchAddLocationEditorGUI() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Modify Location" );
        menuItem.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_M, ActionEvent.ALT_MASK ) );
        menuItem.getAccessibleContext().setAccessibleDescription( "Modify a Location" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchModifyLocationEditorGUI() );
        menuItem.addActionListener( cursorListener );

        //======================

        menu = new JMenu( "GageInfo" );
        menu.setMnemonic( KeyEvent.VK_G );
        menu.getAccessibleContext().setAccessibleDescription( "Access RiverGage Menu" );
        _menuBar.add( menu );

        menuItem = new JMenuItem( "River Gage" );
        menuItem.getAccessibleContext().setAccessibleDescription( "River Crit" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchRiverCritEditorGUI() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Rating Information" );
        menuItem.getAccessibleContext().setAccessibleDescription( "Rating Information" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchRatingCurveEditorGUI() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Crest History" );
        menuItem.getAccessibleContext().setAccessibleDescription( "Crest History" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchCrestHistoryEditorGUI() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Slope Profile" );
        menuItem.getAccessibleContext().setAccessibleDescription( "Slope Profile" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchSlopeProfileEditorGUI() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Average" );
        menuItem.getAccessibleContext().setAccessibleDescription( "Average" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchAverageEditorGUI() );
        menuItem.addActionListener( cursorListener );

        //======================

        menu = new JMenu( "Reservoir" );
        menu.setMnemonic( KeyEvent.VK_R );
        menu.getAccessibleContext().setAccessibleDescription( "Reservoir Menu" );
        _menuBar.add( menu );

        menuItem = new JMenuItem( "Reservoir" );
        menuItem.getAccessibleContext().setAccessibleDescription( "Reservoir" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchReservoirEditorGUI() );
        menuItem.addActionListener( cursorListener );
        //======================

        menu = new JMenu( "Data Ingest" );
        menu.setMnemonic( KeyEvent.VK_D );
        menu.getAccessibleContext().setAccessibleDescription( "Data Ingest Menu" );
        _menuBar.add( menu );

        menuItem = new JMenuItem( "Ingest Filter" );
        menuItem.getAccessibleContext().setAccessibleDescription( "Ingest Filter" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchIngestFilterEditor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Adjust Factor" );
        menuItem.getAccessibleContext().setAccessibleDescription( "Adjust Factor" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchAdjustFactorEditor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "QC Data Limits" );
        menuItem.getAccessibleContext().setAccessibleDescription( "QC Data Limits" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchDataLimitsEditor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Sensok" );
        menuItem.getAccessibleContext().setAccessibleDescription( "Sensok" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchSensOKEditor() );
        menuItem.addActionListener( cursorListener );

        //======================

        menu = new JMenu( "Reference" );
        menu.setMnemonic( KeyEvent.VK_E );
        menu.getAccessibleContext().setAccessibleDescription( "Reference Menu" );
        _menuBar.add( menu );

        menuItem = new JMenuItem( "Country" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchCountryEditor() );
        menuItem.addActionListener( cursorListener );


        menuItem = new JMenuItem( "State" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchStateEditor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Counties" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchCountiesEditor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "HUC2" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchHuc2Editor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "HUC4" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchHuc4Editor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "HUC6" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchHuc6Editor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "HUC8" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchHuc8Editor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "WFO_HSA" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchWfoHsaEditor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "RFC" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchRfcEditor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "ShefDur" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchShefDurEditor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Shefex" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchShefExEditor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Shefpetrans" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchShefPETransEditor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "ShefPE" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchShefPEEditor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "ShefPE1" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchShefPE1Editor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Shefprob" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchShefProbEditor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "ShefQC" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchShefQCEditor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "ShefTS" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchShefTSEditor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Agency" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchAgencyEditor() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Prod" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchProdEditor() );
        menuItem.addActionListener( cursorListener );

        //======================

        menu = new JMenu( "NWSRFS" );
        menu.setMnemonic( KeyEvent.VK_N );
        menu.getAccessibleContext().setAccessibleDescription( "NWSRFS Menu" );
        _menuBar.add( menu );

        menuItem = new JMenuItem( "Modctrl" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchModCtrlEditor() );
        menuItem.addActionListener( cursorListener );

        //======================

        menu = new JMenu( "SyncDBs" );
        menu.setMnemonic( KeyEvent.VK_S );
        menu.getAccessibleContext().setAccessibleDescription( "Process IHFS Menu" );
        menu.setToolTipText( "" );
        _menuBar.add( menu );

        menuItem = new JMenuItem( "Location" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchProcessLocationGUI() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Ingest Filter" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchProcessIngestFilterGUI() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Adjust Factor" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchProcessAdjustFactorGUI() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "River Crit" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchProcessRiverCritGUI() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Crest" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchProcessCrestGUI() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Rating/Shift" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchProcessRatingGUI() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Data Limits" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchProcessDataLimitsGUI() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "LocData Limits" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchProcessLocDataLimitsGUI() );
        menuItem.addActionListener( cursorListener );

        menuItem = new JMenuItem( "Reservoir" );
        menu.add( menuItem );
        cursorListener = CursorController.createListener( this, new LaunchProcessReservoirGUI() );
        menuItem.addActionListener( cursorListener );

        //acts as a separator to move the Help menu all the way to the right
        menuItem = new JMenuItem("");
        menuItem.setEnabled(false);
        _menuBar.add(menuItem);


        // Help Menu

        menu = new JMenu("Help");
        menu.setMnemonic(KeyEvent.VK_H);
        menu.getAccessibleContext().setAccessibleDescription( "Access help menu items." );        
        _menuBar.add(menu);

        menuItem = new JMenuItem("About...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke( KeyEvent.VK_B, ActionEvent.ALT_MASK ) );
        menuItem.getAccessibleContext().setAccessibleDescription( "Display the About Dialog." );
        menu.add(menuItem);
        menuItem.addActionListener( new DisplayAboutDialog() );
    }

    /**
     * Displays the RaxBase GUI
     * 
     * @param jdbcConnectionString - JDBC Connection String
     * @param logFilePath - Full filepath for the logfile
     * 
     * @return NONE
     * 
     */
    public static void show(String jdbcConnectionString, String ihfsConnectionString, String raxDBNameString, String logFilePath )
    {
        RaxBaseDataMgr dataMgr = new RaxBaseDataMgr( jdbcConnectionString, ihfsConnectionString, logFilePath );
        RaxBase raxBase = new RaxBase( dataMgr );
        raxBase.setTitle( "RaxBase (" + raxDBNameString + ") - OB8.3" );
        raxBase.setVisible( true );
    }

    public static boolean validateInputParameters( String[] args )
    {
        boolean valid = false;

        if ( args.length > 3 )
        {
            valid = true;
        }

        return valid;
    }

    public static void main( String[] args ) 
    {
        boolean validInputParameters = false;

        validInputParameters = RaxBase.validateInputParameters( args );

        if ( validInputParameters == true )
        {
            String raxJDBCConnectionString = args[0];
            String ihfsJDBCConnectionString = args[ 1 ];
            String raxDBNameString = args[ 2 ];
            String logFilePath = args[ 3 ];
            show(raxJDBCConnectionString, ihfsJDBCConnectionString, raxDBNameString, logFilePath );
        }
        else
        {
            System.err.println( "Usage: RaxBase <raxJDBCConnectionURL> <ihfsJDBCConnectionURL> <raxDBName> <logFilePath>" );
        }
    }
}