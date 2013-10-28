package ohd.hseb.raxbase.gui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;

import ohd.hseb.raxbase.util.StringPadder;
import ohd.hseb.raxdb_sync.DiffSet;
import ohd.hseb.raxdb_sync.RaxSyncDataMgr;
import ohd.hseb.raxdb_sync.RecordDifference;
import ohd.hseb.raxdb_sync.RecordDifferenceOriginType;
import ohd.hseb.rfc.util.listpanels.JListPanel;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledComboBox;

public abstract class ProcessDifferencesDialog extends JDialog
{
    private String _tableName = null;
    
    protected RaxSyncDataMgr _raxSyncDataMgr = null;

    private Container _dialogContentPane = getContentPane();
   
    protected LabeledComboBox _selectionLCB = new LabeledComboBox( "Select Crest Record:" );
    
    private Map _selectionStringToDifferenceMap = new HashMap();
    
    private DiffSet _diffset = null;
    protected RecordDifference _selectedRecordDiff = null;
    
    protected JListPanel _recordPanel = null;
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _processSingleRecordButton = new JButton( "Synchronize Selected Record" );
    private JButton _processAllRecordsButton = new JButton( "Synchronize All Records" );
    private JButton _closeButton = new JButton( "Close" );
    
    protected String _keyString = "";
    
    protected StringPadder _padder = null;

    protected Dimension _preferredSize = null;
    
    private StatusBar _statusBar = new StatusBar();

    public ProcessDifferencesDialog( JFrame frame, String dialogTitle, String tableName, RaxSyncDataMgr syncDataMgr, StringPadder stringPadder, Dimension prefSizeDim )
    {
        super( frame, dialogTitle, true );
        _raxSyncDataMgr = syncDataMgr;
        _tableName = tableName;
        _padder = stringPadder;
        _preferredSize = prefSizeDim;
    }
    
    public void displayGUI()
    {
        setPreferredSize( _preferredSize );
        initGUI();
    }
    
    protected String getPaddedString( String ihfsString, String colString, String raxString )
    {
        return ( _padder.getFrontPaddedString( ihfsString ) + colString + _padder.getEndPaddedString( colString, raxString ) );
    }

    protected String getPaddedString( double ihfsValue, String colString, double raxValue )
    {
        return ( _padder.getFrontPaddedString( ihfsValue ) + colString + _padder.getEndPaddedString( colString, raxValue ) );
    }
    
    protected String getPaddedString( String ihfsString, String colString, double raxValue )
    {
        return ( _padder.getFrontPaddedString( ihfsString ) + colString + _padder.getEndPaddedString( colString, raxValue ) );
    }

    protected String getPaddedString( long ihfsValue, String colString, long raxValue )
    {
        return ( _padder.getFrontPaddedString( ihfsValue ) + colString + _padder.getEndPaddedString( colString, raxValue ) );
    }
    
    protected String getPaddedDateString( long ihfsDate, String colString, long raxDate )
    {
        return ( _padder.getFrontPaddedDateString( ihfsDate ) + colString + _padder.getEndPaddedDateString( colString, raxDate ) ); 
    }
    
    protected String getPaddedDateString( String ihfsDate, String colString, long raxDate )
    {
        return ( _padder.getFrontPaddedString( ihfsDate ) + colString + _padder.getEndPaddedDateString( colString, raxDate ) ); 
    }

    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        processDifferences();
        if ( _diffset.getList().isEmpty() )
        {
            DialogHelper.displayErrorDialog( this, "No differences found", "Synchronize " + _tableName + " Records" );
            closeWindow();
        }
        else
        {
            updateSelectionComboBox();
            refreshRecordPanel();
            initButtonPanel();
            initFrameComponents();

            addListeners();
            pack();
            setVisible( true );
        }
    }
    
    protected abstract void refreshRecordPanel();
    
    protected abstract String getSelectionComboBoxString( RecordDifference recordDiff );
    
    public void processDifferences()
    {
        _diffset = _raxSyncDataMgr.findAllDifferences( _tableName );
        _statusBar.setText( "Number of differences found: " + _diffset.getList().size() );
        _raxSyncDataMgr.reportAllDifferences( _diffset );
    }

    private void updateSelectionComboBox()
    {
        List selectionComboBoxStringList = new ArrayList();
   
        _selectionStringToDifferenceMap.clear();
        _selectionLCB.getComboBox().removeAllItems();
        List diffsetList = _diffset.getList();
        RecordDifference recordDiff = null;
        
        for ( int i = 0; i < diffsetList.size(); i++ )
        {
            recordDiff = (RecordDifference) diffsetList.get( i );
            selectionComboBoxStringList.add( getSelectionComboBoxString( recordDiff ) );
            _selectionStringToDifferenceMap.put( getSelectionComboBoxString( recordDiff ), recordDiff );
        }
   
        if ( selectionComboBoxStringList.isEmpty() )
        {
            selectionComboBoxStringList.add( "No differences found" );
        }
        _selectionLCB.setComboBoxFromStringArray( selectionComboBoxStringList.toArray() );
        _selectionLCB.setLabel( "<HTML>Select " + _tableName + " Entry:<BR>" + _keyString + "</HTML>");
        _selectionLCB.setLabelPreferredSize( new Dimension( 250, 30 ) );
        _selectionLCB.setComboBoxPreferredSize( new Dimension( 375, 15 ) );
   
        _selectionLCB.setComboBoxFont( new Font( "monospaced", Font.PLAIN, 12 ) );

        String selectedItem = _selectionLCB.getSelectedCBItem();
        
        _selectedRecordDiff = (RecordDifference) _selectionStringToDifferenceMap.get( selectedItem );
    }
    
    protected String getHeaderString()
    {
        String headerString = null;
        
        if ( _selectedRecordDiff.getDiffType() == RecordDifferenceOriginType.MOD )
        {
            headerString = "IHFS Record                                                              Existing Rax Record";
        }
        else
        {
            headerString = "IHFS Record                                                                    New Rax Record";
        }

        return headerString;
    }
    
    protected void initButtonPanel()
    {
        _buttonPanel.setBorder( BorderFactory.createTitledBorder( "Database Controls" ) );
        
//                                                                                   X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _buttonPanel, _processSingleRecordButton, 0,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _buttonPanel, _processAllRecordsButton,   1,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _buttonPanel, _closeButton,               2,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
    }
    
    protected void initFrameComponents()
    {
        JPanel horizontalPanel = new JPanel();
        horizontalPanel.setPreferredSize(new Dimension( 50, 50 ) );

//                                                                                 X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _selectionLCB,      0,   1,    3,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _dialogContentPane, _recordPanel,       0,   2,    3,     1, 1, 3, GridBagConstraints.VERTICAL );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,       0,   3,    3,     1, 1, 1, GridBagConstraints.NONE );
    
        ComponentHelper.addFrameComponent( _dialogContentPane, horizontalPanel,    0,   4,    3,     1, 1, 0, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _dialogContentPane, _statusBar,         0,   5,    3,     1, 1, 0, GridBagConstraints.BOTH);

    
    }

    public void setTableName( String tableName )
    {
        _tableName = tableName;
    }

    public String getTableName()
    {
        return _tableName;
    }

    private void processAllRecords()
    {
        int numberOfErrors = _raxSyncDataMgr.proccessAllDifferences( _diffset, false );

        if ( numberOfErrors == 0 )
        {
            DialogHelper.displayMessageDialog( this, "Successfully synchronized " + _diffset.getList().size() + " record(s)", "Sync " + _tableName + " table" );
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "Failed to synchronize " + numberOfErrors + " record(s)\nPlease check the log " + _raxSyncDataMgr.getInsertUpdateLogFilename() + " for more details", "Sync " + _tableName + " table" );
        }

        processDifferences();
        updateSelectionComboBox();
        refreshRecordPanel();
    }
    
    private void processSingleRecord()
    {
        DiffSet diffSet = new DiffSet();
        String selectedItem = _selectionLCB.getSelectedCBItem();
        int numberOfErrors = 0;
        
        RecordDifference recordDiff = (RecordDifference) _selectionStringToDifferenceMap.get( selectedItem );

        diffSet.addDifference( recordDiff );
        numberOfErrors = _raxSyncDataMgr.proccessAllDifferences( diffSet, false );
        if ( numberOfErrors == 0 )
        {
            DialogHelper.displayMessageDialog( this, "Successfully synchronized 1 record", "Sync " + _tableName + " table" );
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "Failed to synchronize 1 record\nPlease check the log " + _raxSyncDataMgr.getInsertUpdateLogFilename() + " for more details", "Sync " + _tableName + " table" );
        }
        processDifferences();
        updateSelectionComboBox();
        refreshRecordPanel();
    }

    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();
        MouseWheelListener itemSelectionMouseWheelListener = new ItemSelectionMouseWheelListener();
        
        _closeButton.addActionListener( windowCloser );

        _selectionLCB.addComboBoxActionListener( new ItemSelectionListener() );
        _selectionLCB.addComboBoxWheelListener( itemSelectionMouseWheelListener );
        _recordPanel.addMouseWheelListener( itemSelectionMouseWheelListener );
        _processSingleRecordButton.addActionListener( new ProcessSingleRecordButtonListener() );
        _processAllRecordsButton.addActionListener( new ProcessAllRecordButtonListener() );
        this.addMouseWheelListener( itemSelectionMouseWheelListener );
    }
    
    private class ProcessSingleRecordButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            processSingleRecord();
        }
    }
    
    private class ProcessAllRecordButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            processAllRecords();
        }
    }
    
    private class ItemSelectionMouseWheelListener implements MouseWheelListener
    {
        public void mouseWheelMoved( MouseWheelEvent e )
        {
            int index = _selectionLCB.getSelectedIndex();
            int newIndex = 0;
            
            if ( e.getWheelRotation() > 0 )
            {
                if ( ( _selectionLCB.getItemCount() - 1 ) == index )
                {
                    newIndex = 0;
                }
                else
                {
                    newIndex = index + 1;
                }
            }
            else
            {
                if ( index == 0 )
                {
                    newIndex = _selectionLCB.getItemCount() - 1;
                }
                else
                {
                    newIndex = index - 1;
                }
            }
            _selectionLCB.setSelectedIndex( newIndex );
        }
    }

    private class ItemSelectionListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e)
        {
            String selectedItem = _selectionLCB.getSelectedCBItem();
            
            _selectedRecordDiff = (RecordDifference) _selectionStringToDifferenceMap.get( selectedItem );

            if ( _selectedRecordDiff != null )
            {
                refreshRecordPanel();
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

}
