package ohd.hseb.color_chooser;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import ohd.hseb.util.FileLogger;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;

public class ColorChooserDialog extends JDialog implements Runnable
{
    
    private static boolean _isRunning = false;
    private static  Thread _thread = null;
    private static long _lastUpdateTime = 0;
    
    private Container _frameContentPane =                   getContentPane();

    private JTabbedPane _tabbedPane =                       new JTabbedPane();
    private JPanel _tabbedPanePanel =                       new JPanel( new GridBagLayout() );
	private JComponent _viewPanelTab =                      new JPanel( new GridBagLayout() );
    private JComponent _editPanelTab =                      new JPanel( new GridBagLayout() );
    
	private JPanel _viewTabComboBoxPanel =                  new JPanel( new GridBagLayout() );
    private JPanel _editTabComboBoxPanel =                  new JPanel( new GridBagLayout() );
    private JPanel _viewTabColorsDisplayPanel =             new JPanel( new GridBagLayout() );
    
    private String[] _setComboBoxString =                   { "default", "user", "office" };
    private JPanel _viewTabSetComboBoxPanel =               new JPanel( new GridBagLayout() );
    private JLabel _viewTabSetComboBoxLabel =               new JLabel( "Source" );
	private JLabel _viewTabUsedColorsSetLabel =             new JLabel( "" );
    private JLabel _editTabUsedColorsSetLabel =             new JLabel( "" );
    private JPanel _viewTabUserIDComboBoxPanel =            new JPanel( new GridBagLayout() );
    private JComboBox _viewTabUserIDComboBox =              new JComboBox();
	private JLabel _viewTabUserIDLabel =                    new JLabel( "User ID" );
    private JPanel _viewTabColorUseComboBoxPanel =          new JPanel( new GridBagLayout() );
    private JComboBox _viewTabColorUseComboBox =            null;
	private JLabel _viewTabColorUseLabel =                  new JLabel( "Data Type" );
    private String[] _viewTabDurationComboBoxInteger =     { "0" };
	private JComboBox _viewTabDurationComboBox =            new JComboBox ( _viewTabDurationComboBoxInteger );
    private JPanel _viewTabDurationPanel =                  new JPanel( new GridBagLayout() );
    private JLabel _viewTabDurationLabel =                  new JLabel( "Duration" );
    private JLabel _viewTabDurationUnitsLabel =             new JLabel( "Hrs" );
 
    private JPanel _editTabSetComboBoxPanel =               new JPanel( new GridBagLayout() );
    private JLabel _editTabSetComboBoxLabel =               new JLabel( "Source" );
    private JComboBox _editTabSetComboBox =                 new JComboBox( _setComboBoxString );
    private JPanel _editTabUserIDComboBoxPanel =            new JPanel( new GridBagLayout() );
    private JComboBox _editTabUserIDComboBox =              new JComboBox();
    private JLabel _editTabUserIDLabel =                    new JLabel( "User ID" );
    private JPanel _editTabColorUseComboBoxPanel =          new JPanel( new GridBagLayout() );
    private JComboBox _editTabColorUseComboBox =            null;
    private JLabel _editTabColorUseLabel =                  new JLabel( "Data Type" );
    private String[] _editTabDurationComboBoxDouble =       { "0" };
    private JComboBox _editTabDurationComboBox =            new JComboBox ( _editTabDurationComboBoxDouble );
    private JPanel _editTabDurationPanel =                  new JPanel( new GridBagLayout() );
    private JLabel _editTabDurationLabel =                  new JLabel( "Duration" );
    private JLabel _editTabDurationUnitsLabel =             new JLabel( "Hrs" );

    private JComboBox _editTabSaveAsColorUseComboBox =      null;
    private JLabel _editTabSaveAsLabel =                    new JLabel( "Save for datatype:" );
    
    private JPanel _editTabModifyColorScaleValueSetPanel =  new JPanel( new GridBagLayout() );
    private JPanel _editTabColor =                          new JPanel();
    private JTextField _editTabScaleValue =                 new JTextField();
    private JButton _editTabAddColorSet =                   new JButton( "<HTML><CENTER>Add/Update<BR>Color-Value<BR>Pair</CENTER></HTML>" );
    private JButton _editTabDeleteColorSet =                new JButton( "<HTML><CENTER>Delete Color-Value<BR>Pair</CENTER></HTML>" );
    
	private JPanel _editTabButtonPanel =                    new JPanel( new GridBagLayout() );
	private JButton _saveAsUserButton =                     null;
	private JButton _saveAsOfficeButton =                   new JButton( "<HTML><CENTER>Save<BR>as office</CENTER></HTML>" );
	private JButton _deleteUserButton =                     null;
	private JButton _deleteOfficeButton =                   new JButton( "<HTML><CENTER>Delete<BR>as office</CENTER></HTML> ");
    
    private JPanel _editTabColorsDisplayPanel =             new JPanel( new GridBagLayout() );
    private JPanel _editTabUsedColorsDisplayPanel =         new JPanel( new GridBagLayout() );
   
    private JPanel _viewTabCloseButtonPanel =               new JPanel( new GridBagLayout() );
    private JButton _closeButton =                          new JButton( "Close" );
    
    private JPanel _viewTabVCRButtonColorPanel =            new JPanel( new GridBagLayout() );
    private JButton _viewTabVCRLeftColorButton =            new JButton( "<html><FONT FACE=\"Times New Roman\">\u25C4</FONT></html>");
    private JButton _viewTabVCRRightColorButton =           new JButton( "<html><FONT FACE=\"Times New Roman\">\u25BA</FONT></html>");

    private JButton _editTabResetColorButton =              new JButton( "Undo Unsaved Changes");
    
    private JPanel _editTabVCRButtonScaleValuePanel =       new JPanel( new GridBagLayout() );
    private JButton _editTabVCRLeftScaleValueButton =       new JButton( "<html><FONT FACE=\"Times New Roman\">\u25C4</FONT></html>");
    private JButton _editTabVCRRightScaleValueButton =      new JButton( "<html><FONT FACE=\"Times New Roman\">\u25BA</FONT></html>");

	private ColorChooserDialogDataMgr _dataMgr =            null;

    private List _usedColorScaleValueSetList =              new ArrayList();
    private String _userID = null;
    
    private static final int VIEW_PANEL_TAB_ID = 1;
    private static final int EDIT_PANEL_TAB_ID = 2;
    private int _activeTabID = EDIT_PANEL_TAB_ID;
	
    private static final int DEFAULT_WIDTH = 600;
	private static final int DEFAULT_HEIGHT = 550;
    
    private static final float MISSING = -9999;
    private static final float MINIMUM_VALUE = -8888;
    
    private ColorScaleValueSet _displayedColorScaleValueSet = null;
    private ColorScaleValueSet _usedColorScaleValueSet = null;
    private TreeSet _allColorUsesSet = new TreeSet();
    private Set _currentDisplayedDurationSet =              null;
    private NamedColorMatcher _namedColorMatcher =          null;
	
    private boolean _updatingComboBoxes = false;
    
	public ColorChooserDialog()
	{
	}
	
	public ColorChooserDialog( JFrame owner, ColorChooserDialogDataMgr dataMgr, String userID )
	/********************
	Purpose: Constructor 
	*********************/
	{
		super( owner, "Color Scale Manager - User:" + userID, true );
		_dataMgr = dataMgr;
        _userID = userID;
        _dataMgr.setUserID( userID );
		initGui();
        addListeners();
	}
    // -------------------------------------------------------------------------------------

	private void initGui()
	/********************
	Purpose: Initializes the GUI by calling the appropriate methods 
	*********************/
	{
        setSize( DEFAULT_WIDTH, DEFAULT_HEIGHT );

        setLayoutManager();
        addFrameComponenets();

        updateUsedColorScaleValueSetList();
        updateAllColorUsesSet();
        
        addViewPanelTabComponents();
        addEditPanelTabComponents();
        addComboBoxComponents();
        addCloseButtonPanelComponents();
        
        initComboBoxes();
        
        addToolTips();
        
        displayColorScaleValues();
        return;
	}
    
	private void addToolTips()
	{
	    //    private JButton _deleteUserButton =                     null;
	    //    private JButton _deleteOfficeButton =                   new JButton( "<HTML><CENTER>Delete<BR>as office</CENTER></HTML> ");

           
        _editTabResetColorButton.setToolTipText("<HTML> Undoes the changes in working  <BR> memory for the currently selected <BR> color set, returning it to  <BR> what is stored in the database.</HTML>  ");

	    _editTabVCRLeftScaleValueButton.setToolTipText("<HTML> Selects the previous DataType/Duration  <BR> combination for the currently selected source. </HTML> ");
        _editTabVCRRightScaleValueButton.setToolTipText("<HTML> Selects the next DataType/Duration  <BR> combination for the currently selected source. </HTML> ");

	    _editTabAddColorSet.setToolTipText("<HTML> Puts the edited color-value pair  <BR> into the set in working memory.</HTML> ");
	    _editTabDeleteColorSet.setToolTipText("<HTML> Removes the edited color-value  <BR> from the set in working memory.</HTML> ");
	
        _saveAsUserButton.setToolTipText("<HTML> For the current user, <BR> saves the currently selected color set to the database.</HTML> ");
        _saveAsOfficeButton.setToolTipText("<HTML> For the office default settings, <BR> saves the currently selected color set to the database.</HTML> ");
        
        _deleteUserButton.setToolTipText("<HTML> Remove the color set with the currently selected <BR> DataType/Duration combination  <BR> from the database for this user. </HTML>");
        _deleteOfficeButton.setToolTipText("<HTML> Remove the color set with the currently selected <BR> DataType/Duration combination <BR>  from the database for the office.</HTML>");
        
    
    }   

    private void addCloseButtonPanelComponents()
    {
        //                                                                                        X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _viewTabCloseButtonPanel, _closeButton,                1,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
    }
    
    private void addEditPanelTabComponents()
    {
        JPanel editTabHorizontalSeparatorPanel = new JPanel();
        JPanel editTabVirticalSeparatorPanel = new JPanel();
        JPanel editTabVirticalSeparatorPanel2 = new JPanel();

        _editTabButtonPanel.setBorder( BorderFactory.createTitledBorder( "Color-Value Set Database Controls" ) );
        editTabHorizontalSeparatorPanel.setPreferredSize( new Dimension( 50, 250 ) );
        editTabVirticalSeparatorPanel.setPreferredSize( new Dimension( 350, 60 ) );
        editTabVirticalSeparatorPanel2.setPreferredSize( new Dimension( 350, 60 ) );
        _editTabColorUseComboBox = new JComboBox( getColorUseComboBoxString() );
        _editTabUsedColorsDisplayPanel.setBorder( BorderFactory.createTitledBorder( "Used Color Set" ) );

        //                                                                                             X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _editPanelTab, _editTabComboBoxPanel,                       0,   0,   25,    1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _editPanelTab, _editTabColorsDisplayPanel,                  0,   1,   25,    5, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _editPanelTab, editTabVirticalSeparatorPanel,               0,   6,    7,    5, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _editPanelTab, _editTabModifyColorScaleValueSetPanel,       8,   6,   10,    5, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _editPanelTab, editTabVirticalSeparatorPanel2,             18,   6,    7,    5, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _editPanelTab, editTabHorizontalSeparatorPanel,             0,  11,   25,   10, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _editPanelTab, _editTabUsedColorsDisplayPanel,              0,  22,   25,    5, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _editPanelTab, _editTabButtonPanel,                         0,  27,   25,    5, 1, 1, GridBagConstraints.BOTH );

        _editTabColor.setBackground( Color.BLACK );
        _editTabSaveAsColorUseComboBox = new JComboBox( _allColorUsesSet.toArray() );

        JPanel saveAsColorUseLabelComboBoxPanel = new JPanel();
        JPanel saveDeleteDbButtonPanel = new JPanel();
        JPanel modifyColorSetsPanel = new JPanel( new GridBagLayout() );
        JPanel colorSetPanel = new JPanel( new GridBagLayout() );
        
        saveAsColorUseLabelComboBoxPanel.add( _editTabSaveAsLabel );
        saveAsColorUseLabelComboBoxPanel.add( _editTabSaveAsColorUseComboBox );
        saveDeleteDbButtonPanel.add( _saveAsUserButton );
        saveDeleteDbButtonPanel.add( _saveAsOfficeButton );
        saveDeleteDbButtonPanel.add( _deleteUserButton );
        saveDeleteDbButtonPanel.add( _deleteOfficeButton );
 
        GridBagConstraints buttonGbc = new GridBagConstraints();
        buttonGbc.insets = new Insets(2,2,2,2);
        buttonGbc.weightx = 1;
        buttonGbc.weighty = 1;
        //JPanel panel, Component component, GridBagConstraints gbc, int column, int row, int width, int height, int fill )
        ComponentHelper.addPanelComponent( modifyColorSetsPanel, _editTabAddColorSet, buttonGbc,    1,   1,    1,    1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( modifyColorSetsPanel, _editTabDeleteColorSet, buttonGbc, 1,   2,    1,    1, GridBagConstraints.NONE );

        ComponentHelper.addPanelComponent( colorSetPanel, _editTabColor, buttonGbc,    1,   1,    1,    1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( colorSetPanel, _editTabScaleValue, buttonGbc, 1,   2,    1,    1, GridBagConstraints.BOTH );

        //                                                                                                X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _editTabModifyColorScaleValueSetPanel, colorSetPanel,          1,   1,    1,    2, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( _editTabModifyColorScaleValueSetPanel, modifyColorSetsPanel,   2,   1,    1,    2, 1, 1, GridBagConstraints.BOTH );

        ComponentHelper.addPanelComponent( _editTabButtonPanel, saveAsColorUseLabelComboBoxPanel,         1,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _editTabButtonPanel, saveDeleteDbButtonPanel,                  1,   2,    1,    1, 1, 1, GridBagConstraints.NONE );
    }

    private void addViewPanelTabComponents()
    {
        JPanel horizontalSeparatorPanel = new JPanel();

        horizontalSeparatorPanel.setPreferredSize( new Dimension( 50, 350 ) );
        
        _viewTabColorUseComboBox = new JComboBox( getColorUseComboBoxString() );
        
        //                                                                                                  X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _viewPanelTab, _viewTabComboBoxPanel,                            0,   0,   25,    1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _viewPanelTab, _viewTabColorsDisplayPanel,                       0,   2,   25,    5, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _viewPanelTab, horizontalSeparatorPanel,                         0,   7,   25,   10, 1, 1, GridBagConstraints.BOTH );
    }
    
    private void addComboBoxComponents()
    {

        //                                                                                                  X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _viewTabSetComboBoxPanel, _viewTabSetComboBoxLabel,              1,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _viewTabSetComboBoxPanel, _viewTabUsedColorsSetLabel,            1,   2,    1,    1, 1, 1, GridBagConstraints.NONE );

        
        ComponentHelper.addPanelComponent( _viewTabUserIDComboBoxPanel, _viewTabUserIDLabel,                1,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _viewTabUserIDComboBoxPanel, _viewTabUserIDComboBox,             1,   2,    1,    1, 1, 1, GridBagConstraints.NONE );

        ComponentHelper.addPanelComponent( _viewTabColorUseComboBoxPanel, _viewTabColorUseLabel,            1,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _viewTabColorUseComboBoxPanel, _viewTabColorUseComboBox,         1,   2,    1,    1, 1, 1, GridBagConstraints.NONE );

        ComponentHelper.addPanelComponent( _viewTabDurationPanel, _viewTabDurationLabel,                    1,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _viewTabDurationPanel, _viewTabDurationComboBox,                 1,   2,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _viewTabDurationPanel, _viewTabDurationUnitsLabel,               2,   2,    1,    1, 1, 1, GridBagConstraints.NONE );
        
        _viewTabUserIDComboBoxPanel.setVisible( false );
        
        //                                                                                                  X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _viewTabComboBoxPanel, _viewTabSetComboBoxPanel,                 1,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _viewTabComboBoxPanel, _viewTabUserIDComboBoxPanel,              2,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _viewTabComboBoxPanel, _viewTabColorUseComboBoxPanel,            3,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _viewTabComboBoxPanel, _viewTabDurationPanel,                    4,   1,    1,    1, 1, 1, GridBagConstraints.NONE );

        
        
        
        //                                                                                                  X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _editTabSetComboBoxPanel, _editTabSetComboBoxLabel,              1,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _editTabSetComboBoxPanel, _editTabSetComboBox,                   1,   2,    1,    1, 1, 1, GridBagConstraints.NONE );

        ComponentHelper.addPanelComponent( _editTabUserIDComboBoxPanel, _editTabUserIDLabel,                1,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _editTabUserIDComboBoxPanel, _editTabUserIDComboBox,             1,   2,    1,    1, 1, 1, GridBagConstraints.NONE );

        ComponentHelper.addPanelComponent( _editTabColorUseComboBoxPanel, _editTabColorUseLabel,            1,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _editTabColorUseComboBoxPanel, _editTabColorUseComboBox,         1,   2,    1,    1, 1, 1, GridBagConstraints.NONE );

        ComponentHelper.addPanelComponent( _editTabDurationPanel, _editTabDurationLabel,                    1,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _editTabDurationPanel, _editTabDurationComboBox,                 1,   2,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _editTabDurationPanel, _editTabDurationUnitsLabel,               2,   2,    1,    1, 1, 1, GridBagConstraints.NONE );
        
        _editTabUserIDComboBoxPanel.setVisible( false );
        _editTabDurationComboBox.setEditable( true );
        
        //                                                                                                  X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _editTabComboBoxPanel, _editTabSetComboBoxPanel,                 1,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _editTabComboBoxPanel, _editTabUserIDComboBoxPanel,              2,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _editTabComboBoxPanel, _editTabColorUseComboBoxPanel,            3,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _editTabComboBoxPanel, _editTabDurationPanel,                    4,   1,    1,    1, 1, 1, GridBagConstraints.NONE );
        
        _editTabDurationComboBox.setName( "EditTabDuration" );
        _viewTabDurationComboBox.setName( "ViewTabDuration" );
    }
    
    private void setLayoutManager()
    {
        _frameContentPane.setLayout( new GridBagLayout() );
    }

    private void addFrameComponenets()
    {
        _tabbedPane.setPreferredSize( new Dimension( 200, 600 ) );
        
        ComponentHelper.addPanelComponent( _tabbedPanePanel, _tabbedPane,                         1,   1,   1,    1, 1, 1, GridBagConstraints.BOTH );
        
        //                                                                                        X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _frameContentPane, _viewTabComboBoxPanel,              0,   0,   25,    1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _frameContentPane, _tabbedPanePanel,                   0,   2,   25,    30, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _frameContentPane, _viewTabCloseButtonPanel,           0,   32,  25,    1, 1, 1, GridBagConstraints.BOTH );
        _viewPanelTab.setName( "ViewPanelTab" );
        _editPanelTab.setName( "EditPanelTab" );
        _tabbedPane.addTab( "Edit Color Sets", _editPanelTab );
        _tabbedPane.addTab( "Browse Color Sets", _viewPanelTab );
        _saveAsUserButton = new JButton( "<HTML><CENTER>Save as<BR><B>" + _userID + "</B></CENTER></HTML>" );
        _deleteUserButton = new JButton( "<HTML><CENTER>Delete as<BR><B>" + _userID + "</B></CENTER></HTML> ");
        _deleteOfficeButton.setEnabled( false );
        _deleteUserButton.setEnabled( false );
    }
    
    private void displayColorScaleValues()
    {
        _viewTabColorsDisplayPanel.removeAll();
        _editTabColorsDisplayPanel.removeAll();
        _editTabUsedColorsDisplayPanel.removeAll();
        ColorSet colorSet = null;
        ScaleValueSet scaleValueSet = null;
        String colorUse = null;
        String userIDString = null;
        
        if ( _activeTabID == VIEW_PANEL_TAB_ID )
        {
            colorUse = (String) _viewTabColorUseComboBox.getSelectedItem();
        }
        else
        {
            colorUse = (String) _editTabColorUseComboBox.getSelectedItem();
        }
        
        ColorScaleValueSet colorScaleValueSet = getColorScaleValueSetByColorUse( colorUse );

        if ( colorScaleValueSet != null )
        {
            userIDString = colorScaleValueSet.getUserID();
            
            if ( userIDString.equalsIgnoreCase( "HARDCODED" ) )
            {
                _viewTabUsedColorsSetLabel.setText( "default" );
            }
            else if ( userIDString.equalsIgnoreCase( "default" ) )
            {
                _viewTabUsedColorsSetLabel.setText( "office" );
            }
            else
            {
                _viewTabUsedColorsSetLabel.setText( "user (" + userIDString + ")" );
            }
        }

        if ( colorScaleValueSet != null )
        {
            colorSet = colorScaleValueSet.getColorSet();
            scaleValueSet = colorScaleValueSet.getScaleValueSet();
        }
        
        if ( ( colorSet != null ) && ( scaleValueSet != null ) )
        {
            List namedColorList = colorSet.getNamedColorList();
            int colorSetListSize = namedColorList.size();

            //                                                                                                       X,   Y,  #Col,  #Row
            ComponentHelper.addPanelComponent( _viewTabVCRButtonColorPanel, _viewTabVCRLeftColorButton,              1,   1,    1,    1, 1, 1, GridBagConstraints.BOTH );
            ComponentHelper.addPanelComponent( _viewTabVCRButtonColorPanel, _viewTabVCRRightColorButton,             2,   1,    1,    1, 1, 1, GridBagConstraints.BOTH );
            ComponentHelper.addPanelComponent( _viewTabColorsDisplayPanel, _viewTabVCRButtonColorPanel,              1,   1,    colorSetListSize,    1, 1, 1, GridBagConstraints.NONE );

            ComponentHelper.addPanelComponent( _editTabVCRButtonScaleValuePanel, _editTabVCRLeftScaleValueButton,    1,   1,    1,    1, 1, 1, GridBagConstraints.BOTH );
            ComponentHelper.addPanelComponent( _editTabVCRButtonScaleValuePanel, _editTabVCRRightScaleValueButton,   2,   1,    1,    1, 1, 1, GridBagConstraints.BOTH );
            ComponentHelper.addPanelComponent( _editTabColorsDisplayPanel, _editTabVCRButtonScaleValuePanel,         1,   1,    colorSetListSize,    1, 1, 1, GridBagConstraints.NONE );
            ComponentHelper.addPanelComponent( _editTabColorsDisplayPanel, _editTabResetColorButton,                 1,   8,    colorSetListSize,    1, 1, 1, GridBagConstraints.NONE );

            for ( int i = 0; i < namedColorList.size(); i++ )
            {
                Double scaleValueDouble =  (Double) scaleValueSet.getScaleValueSet().get( i );
                NamedColor namedColor = getClosestNamedColor( (Color) namedColorList.get( i ) );

                addColorValueDisplayPanelToTabContainerPanel( scaleValueDouble,
                                                              namedColor,
                                                              _viewTabColorsDisplayPanel, 
                                                              i,  
                                                              false);
                
                addColorValueDisplayPanelToTabContainerPanel( scaleValueDouble,
                                                              namedColor,
                                                              _editTabColorsDisplayPanel, 
                                                              i,  
                                                              true);
            } //end for
        }
        _displayedColorScaleValueSet = colorScaleValueSet;

        if ( _activeTabID == EDIT_PANEL_TAB_ID ) 
        {
            _usedColorScaleValueSet = getUsedColorScaleValueSetByColorUse( colorUse );
            
            if ( _usedColorScaleValueSet != null )
            {
                List usedColorSetList = _usedColorScaleValueSet.getColorSet().getNamedColorList();
                List usedScaleValueSetList = _usedColorScaleValueSet.getScaleValueSet().getScaleValueSet();

                for ( int i = 0; i < usedColorSetList.size(); i++ )
                {
                    Double scaleValueDouble = (Double) usedScaleValueSetList.get( i );
                    NamedColor namedColor = getClosestNamedColor( (Color) usedColorSetList.get( i ) );
                    addColorValueDisplayPanelToTabContainerPanel( scaleValueDouble,
                            namedColor,
                            _editTabUsedColorsDisplayPanel, 
                            i,  
                            false);
                }

                userIDString = _usedColorScaleValueSet.getUserID();

                if ( userIDString.equalsIgnoreCase( "HARDCODED" ) )
                {
                    _editTabUsedColorsSetLabel.setText( "default" );
                }
                else if ( userIDString.equalsIgnoreCase( "default" ) )
                {
                    _editTabUsedColorsSetLabel.setText( "office" );
                }
                else
                {
                    _editTabUsedColorsSetLabel.setText( "user (" + userIDString + ")" );
                }
            }
        }
        
        if ( _usedColorScaleValueSet != null )
        {
            _editTabUsedColorsDisplayPanel.setBorder( BorderFactory.createTitledBorder( "Used Color Set - Source: " + _editTabUsedColorsSetLabel.getText() + 
                                                                                        "           Data-Type: " + _usedColorScaleValueSet.getColorUseString() +
                                                                                        "           Duration: " + _usedColorScaleValueSet.getDuration() ) );
        }

        _viewTabColorsDisplayPanel.setVisible( false );
        _viewTabColorsDisplayPanel.setVisible( true );
        _editTabColorsDisplayPanel.setVisible( false );
        _editTabColorsDisplayPanel.setVisible( true );
        _editTabUsedColorsDisplayPanel.setVisible( false );
        _editTabUsedColorsDisplayPanel.setVisible( true );
    }
    
    // ----------------------------------------------------------------------------------------------------
    private void addColorValueDisplayPanelToTabContainerPanel(Double scaleValueDouble,
                                                              NamedColor namedColor,
                                                              JPanel colorsDisplayPanel, 
                                                              int indexOfSlotPanel,  
                                                              boolean addClickColorSelectionListener)
    {
        int i = indexOfSlotPanel;
        JPanel colorPanel = new JPanel();
        JTextField scaleValueTextField = new JTextField();
        JPanel colorAndScalePairPanel = new JPanel();
        
        colorPanel.setBackground( namedColor );
        colorPanel.setBorder( BorderFactory.createRaisedBevelBorder() );
   
        colorAndScalePairPanel.setLayout( new GridBagLayout() );
        colorAndScalePairPanel.setPreferredSize( new Dimension( 10, 50 ) );
  
        scaleValueTextField.setText( getFormattedDoubleString(scaleValueDouble));
      
        scaleValueTextField.setEditable( false );
        
        //                                                                                           X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( colorAndScalePairPanel, colorPanel,                       0,   0,    1,    5, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( colorAndScalePairPanel, scaleValueTextField,              0,   5,    1,    5, 1, 1, GridBagConstraints.BOTH );

        ComponentHelper.addPanelComponent( colorsDisplayPanel, colorAndScalePairPanel,               i,   2,    1,    5, 1, 1, GridBagConstraints.BOTH );

        if (addClickColorSelectionListener)
        {
            colorPanel.addMouseListener( new ClickColorSelectionListener( scaleValueDouble ) );
            scaleValueTextField.addMouseListener( new EditScaleValueTextFieldListener( colorPanel, scaleValueDouble ) );
        }
    }
  
    private void initComboBoxes()
    {
        String selectedColorUseString = (String) _editTabColorUseComboBox.getSelectedItem();
        _editTabSaveAsColorUseComboBox.setSelectedItem( selectedColorUseString );
    }

    
    private List getFilteredColorScaleValueSetList( String userID, String colorUse )
    {
        List filteredColorScaleValueSetList = new ArrayList();
        List databaseColorScaleValueSetList = _usedColorScaleValueSetList;

        ColorScaleValueSet extractedColorScaleValueSet = null;
        
        for ( int i = 0; i < databaseColorScaleValueSetList.size(); i++ )
        {
            extractedColorScaleValueSet = (ColorScaleValueSet) databaseColorScaleValueSetList.get( i );
            
            if ( ( extractedColorScaleValueSet.getUserID().equalsIgnoreCase( userID ) ) &&
                 ( extractedColorScaleValueSet.getColorUseString().equalsIgnoreCase( colorUse ) ) )
            {
                filteredColorScaleValueSetList.add( extractedColorScaleValueSet );
            }
        }

        return filteredColorScaleValueSetList;
    }

    private ColorScaleValueSet getUsedColorScaleValueSetByColorUse( String colorUse )
    {
        ColorScaleValueSet foundColorScaleValueSet = null;
        ColorScaleValueSet returnColorScaleValueSet = null;
        
        List userColorScaleValueSetList = new ArrayList();
        List officeColorScaleValueSetList = new ArrayList();
//        List flatFileColorScaleValueSetList = _dataMgr.getDefaultColorScaleValueSetList();
        List flatFileColorScaleValueSetList = _usedColorScaleValueSetList;
        
        userColorScaleValueSetList = getFilteredColorScaleValueSetList( _userID, colorUse );
        officeColorScaleValueSetList = getFilteredColorScaleValueSetList( "default", colorUse );
        
        foundColorScaleValueSet = findClosestColorScaleValueSet( userColorScaleValueSetList, colorUse );
        
        if ( foundColorScaleValueSet == null )
        {
            foundColorScaleValueSet = findClosestColorScaleValueSet( officeColorScaleValueSetList, colorUse );
        }
        
        if ( foundColorScaleValueSet == null )
        {
            foundColorScaleValueSet = findClosestColorScaleValueSet( flatFileColorScaleValueSetList, colorUse );
        }
        
        returnColorScaleValueSet = new ColorScaleValueSet( foundColorScaleValueSet );

        return returnColorScaleValueSet;
    }
    
    private ColorScaleValueSet findClosestColorScaleValueSet( List userColorScaleValueSetList, String colorUse  )
    {
        ColorScaleValueSet foundColorScaleValueSet = null;
        double duration = getDurationComboBoxDoubleValue( _editTabDurationComboBox );
        
        ColorScaleValueSet extractedColorScaleValueSet = null;
        List foundColorUseColorScaleValueSetList = new ArrayList();

        for ( int i = 0; i < userColorScaleValueSetList.size(); i++ )
        {
            extractedColorScaleValueSet = (ColorScaleValueSet) userColorScaleValueSetList.get( i );
            
            if ( ( extractedColorScaleValueSet.getColorUseString().equalsIgnoreCase( colorUse ) ) )
            {
                foundColorUseColorScaleValueSetList.add( extractedColorScaleValueSet );
            }
        }
        
        if ( foundColorUseColorScaleValueSetList.size() > 0 )
        {
            foundColorScaleValueSet = getClosestMatchingDurationColorScaleValueSet( foundColorUseColorScaleValueSetList,
                                                                                    duration );
        }
        
        return foundColorScaleValueSet;
    }
    
    private ColorScaleValueSet getClosestMatchingDurationColorScaleValueSet( List foundColorUseColorScaleValueSetList,
                                                                             double duration )
    {
        ColorScaleValueSet closestMatchingColorScaleValueSet = null;
        ColorScaleValueSet extractedColorScaleValueSet = null;
        double durationDiff = 1000;
        
        for ( int i = 0; i < foundColorUseColorScaleValueSetList.size(); i++ )
        {
            extractedColorScaleValueSet = (ColorScaleValueSet) foundColorUseColorScaleValueSetList.get( i );
            
            if ( ( Math.abs( extractedColorScaleValueSet.getDuration() - duration ) ) < durationDiff )
            {
                durationDiff = Math.abs( extractedColorScaleValueSet.getDuration() - duration );
                closestMatchingColorScaleValueSet = extractedColorScaleValueSet;
            }
        }
        return closestMatchingColorScaleValueSet;
    }
    
    private void updateAllColorUsesSet()
    {
        List defaultColorList = _dataMgr.getDefaultColorScaleValueSetList();
        List databaseColorList = _dataMgr.getColorValueTableColorScaleValueSetList();
        ColorScaleValueSet extractedColorScaleValueSet = null;
        
        for ( int i = 0; i < defaultColorList.size(); i++ )
        {
            extractedColorScaleValueSet = (ColorScaleValueSet) defaultColorList.get( i );
            _allColorUsesSet.add( extractedColorScaleValueSet.getColorUseString() );
        }
        for ( int i = 0; i < databaseColorList.size(); i++ )
        {
            extractedColorScaleValueSet = (ColorScaleValueSet) databaseColorList.get( i );
            _allColorUsesSet.add( extractedColorScaleValueSet.getColorUseString() );
        }
    }
    
    private String getFormattedDoubleString(double value)
    {   
        return getFormattedDoubleString(value, "%7.2f");
    }

    private String getFormattedDoubleString(double value, String format)
    {
        String text = null;
        
        if ( value == MINIMUM_VALUE )
        {
            text = "< Min";
        }
        else if ( value == MISSING )
        {
            text =  "MSG";
        }
        else
        {
            text = String.format(format, value);
        }
        
        return text;
    }
    
    private NamedColor getClosestNamedColor( Color color )
    {
        NamedColor namedColor = null;
        
        if ( _namedColorMatcher == null )
        {
            _namedColorMatcher = new NamedColorMatcher( _dataMgr.getRGBColorMap() );
        }
        
        namedColor = _namedColorMatcher.getClosestNamedColor( color );
        
        return namedColor;
    }
    
    private class EditScaleValueTextFieldListener extends MouseAdapter
    {
        private JPanel _associatedColorPanel = null;
        private double _scaleValueDouble = 0;
        
        public EditScaleValueTextFieldListener( JPanel associatedColorPanel, double scaleValueDouble )
        {
            _associatedColorPanel = associatedColorPanel;
            _scaleValueDouble = scaleValueDouble;
        }

        public void mousePressed( MouseEvent e )
        {
            _editTabColor.setBackground( _associatedColorPanel.getBackground() );
            
            _editTabScaleValue.setText( getFormattedDoubleString( _scaleValueDouble ) );
        }
    }
    
    private class ClickColorSelectionListener extends MouseAdapter
    {
        private double  _scaleValueDouble = 0;
        
        public ClickColorSelectionListener( Double scaleValueDouble )
        {
            _scaleValueDouble = scaleValueDouble;
        }
        
        public void mousePressed( MouseEvent e )
        {
            JPanel panel = (JPanel) e.getSource();

            _editTabColor.setBackground( panel.getBackground() );
            
            _editTabScaleValue.setText( getFormattedDoubleString(_scaleValueDouble ) );

            displayColorPickerDialog();
            
            addColorSet();
        }
    }
    
    
    
    private String[] getColorUseComboBoxString()
    {
        String[] colorUseComboBoxString = null;
        List hvDefaultColorList = _dataMgr.getDefaultColorScaleValueSetList();
        List colorScaleValueSetList = _dataMgr.getColorValueTableColorScaleValueSetList();
        Set colorUseSet = new HashSet();
        String selectedItem = null;
        String colorUseString = null;
        
        List colorUseNamesList = new ArrayList();

        if ( _activeTabID == VIEW_PANEL_TAB_ID )
        {
            selectedItem = _viewTabUsedColorsSetLabel.getText();
        }
        else
        {
            selectedItem = (String) _editTabSetComboBox.getSelectedItem();
        }
        
        if ( selectedItem.equalsIgnoreCase( "default" ) )
        {
            if ( _activeTabID == VIEW_PANEL_TAB_ID )
            {
                _viewTabUserIDComboBoxPanel.setVisible( false );
            }
            else
            {
                _editTabUserIDComboBoxPanel.setVisible( false );
            }
            for ( int i = 0; i < hvDefaultColorList.size(); i++ )
            {
                ColorScaleValueSet colorScaleValueSet = (ColorScaleValueSet) hvDefaultColorList.get( i );
                colorUseString = colorScaleValueSet.getColorUseString();
                if ( ! colorUseSet.contains( colorUseString ) )
                {
                    colorUseSet.add( colorUseString );
                    colorUseNamesList.add( colorUseString );
                }
            }

            colorUseComboBoxString = (String[]) colorUseNamesList.toArray( new String[ colorUseNamesList.size() ] );
        }
        
        if ( selectedItem.equalsIgnoreCase( "user" ) )
        {
            if ( _activeTabID == VIEW_PANEL_TAB_ID )
            {
                _viewTabUserIDComboBoxPanel.setVisible( true );
            }
            else
            {
                _editTabUserIDComboBoxPanel.setVisible( true );
            }
            
            for ( int i = 0; i < colorScaleValueSetList.size(); i++ )
            {
                ColorScaleValueSet colorScaleValueSet = (ColorScaleValueSet) colorScaleValueSetList.get( i );
                if ( ! colorScaleValueSet.getUserID().equalsIgnoreCase( "default" ) )
                {
                    colorUseNamesList.add( colorScaleValueSet.getColorUseString() );
                }
            }

            colorUseSet.addAll( colorUseNamesList );
            
            colorUseComboBoxString = (String[]) colorUseSet.toArray( new String[ colorUseSet.size() ] );
        }
        
        if ( selectedItem.equalsIgnoreCase( "office" ) )
        {
            if ( _activeTabID == VIEW_PANEL_TAB_ID )
            {
                _viewTabUserIDComboBoxPanel.setVisible( false );
            }
            else
            {
                _editTabUserIDComboBoxPanel.setVisible( false );
            }
            
            for ( int i = 0; i < colorScaleValueSetList.size(); i++ )
            {
                ColorScaleValueSet colorScaleValueSet = (ColorScaleValueSet) colorScaleValueSetList.get( i );
                if ( colorScaleValueSet.getUserID().equalsIgnoreCase( "default" ) )
                {
                    colorUseNamesList.add( colorScaleValueSet.getColorUseString() );
                }
            }
            colorUseSet.addAll( colorUseNamesList );
            
            colorUseComboBoxString = (String[]) colorUseSet.toArray( new String[ colorUseSet.size() ] );
        }
        
        if ( selectedItem.equalsIgnoreCase( "" ) )
        {
            for ( int i = 0; i < _usedColorScaleValueSetList.size(); i++ )
            {
                ColorScaleValueSet colorScaleValueSet = (ColorScaleValueSet) _usedColorScaleValueSetList.get( i );
                colorUseNamesList.add( colorScaleValueSet.getColorUseString() );
            }
            colorUseSet.addAll( colorUseNamesList );
        
            colorUseComboBoxString = (String[]) colorUseSet.toArray( new String[ colorUseSet.size() ] );
                
        }
        return colorUseComboBoxString;
    }

    private void updateUserIDComboBox()
    {
        List colorScaleValueSetList = _dataMgr.getColorValueTableColorScaleValueSetList();
        List userIDsList = new ArrayList();
        Set userIDsSet = new HashSet();

        for ( int i = 0; i < colorScaleValueSetList.size(); i++ )
        {
            ColorScaleValueSet colorScaleValueSet = (ColorScaleValueSet) colorScaleValueSetList.get( i );
            if ( ! colorScaleValueSet.getUserID().equalsIgnoreCase( "default" ) )
            {
                userIDsList.add( colorScaleValueSet.getUserID() );
            }
        }
        
        userIDsSet.addAll( userIDsList );

        Iterator iterator = userIDsSet.iterator();
        
        if ( _activeTabID == VIEW_PANEL_TAB_ID )
        {
            _viewTabUserIDComboBox.removeAllItems();
            while ( iterator.hasNext() )
            {
                String userID = (String) iterator.next();

                _viewTabUserIDComboBox.addItem( userID );
            }
        }
        else
        {
//            _updatingComboBoxes = true;
            _editTabUserIDComboBox.removeAllItems();
            while ( iterator.hasNext() )
            {
                String userID = (String) iterator.next();

                _editTabUserIDComboBox.addItem( userID );
            }
//            _updatingComboBoxes = false;
        }
        
        if ( userIDsSet.isEmpty() )
        {
            _editTabUserIDComboBox.addItem( "-----" );
        }
    }


    private List getDurationSet()
    {
        String selectedItem = null;
        ColorScaleValueSet extractedColorScaleValueSet = null;
        
        if ( _activeTabID == VIEW_PANEL_TAB_ID )
        {
            selectedItem = _viewTabUsedColorsSetLabel.getText();
        }
        else
        {
            selectedItem = (String) _editTabSetComboBox.getSelectedItem();
        }

        List colorScaleValueSetList = _dataMgr.getColorValueTableColorScaleValueSetList();
        SortedSet <Double> durationSet = new TreeSet <Double> ();
        
        if ( _activeTabID == EDIT_PANEL_TAB_ID )
        {
            if ( selectedItem.equalsIgnoreCase( "user" ) )
            {
                String selectedUser = null;
                String selectedColorUse = null;

                if ( _activeTabID == VIEW_PANEL_TAB_ID )
                {
                    selectedUser = (String) _viewTabUserIDComboBox.getSelectedItem();
                    selectedColorUse = (String) _viewTabColorUseComboBox.getSelectedItem();

                }
                else
                {
                    selectedUser = (String) _editTabUserIDComboBox.getSelectedItem();
                    selectedColorUse = (String) _editTabColorUseComboBox.getSelectedItem();

                }
                for ( int i = 0; i < colorScaleValueSetList.size(); i++ )
                {
                    extractedColorScaleValueSet = (ColorScaleValueSet) colorScaleValueSetList.get( i );

                    if ( ( extractedColorScaleValueSet.getUserID().equalsIgnoreCase( selectedUser ) ) &&
                            ( extractedColorScaleValueSet.getColorUseString().equalsIgnoreCase( selectedColorUse ) ) )
                    {
                        durationSet.add( extractedColorScaleValueSet.getDuration() );
                    }
                }
            }
            else if ( selectedItem.equalsIgnoreCase( "office" ) )
            {
                String selectedColorUse = null;

                if ( _activeTabID == VIEW_PANEL_TAB_ID )
                {
                    selectedColorUse = (String) _viewTabColorUseComboBox.getSelectedItem();
                }
                else
                {
                    selectedColorUse = (String) _editTabColorUseComboBox.getSelectedItem();
                }
                for ( int i = 0; i < colorScaleValueSetList.size(); i++ )
                {
                    extractedColorScaleValueSet = (ColorScaleValueSet) colorScaleValueSetList.get( i );

                    if ( ( extractedColorScaleValueSet.getUserID().equalsIgnoreCase( "default" ) ) &&
                            ( extractedColorScaleValueSet.getColorUseString().equalsIgnoreCase( selectedColorUse ) ) )
                    {
                        durationSet.add( extractedColorScaleValueSet.getDuration() );
                    }
                }
            }
        }

        if ( ( _activeTabID == VIEW_PANEL_TAB_ID ) || ( selectedItem.equalsIgnoreCase( "default" ) ) )
        {
            String selectedColorUse = null;

            if ( _activeTabID == VIEW_PANEL_TAB_ID )
            {
                selectedColorUse = (String) _viewTabColorUseComboBox.getSelectedItem();
            }
            else
            {
                selectedColorUse = (String) _editTabColorUseComboBox.getSelectedItem();
            }

            if ( _activeTabID == VIEW_PANEL_TAB_ID )
            {
                if ( selectedColorUse != null )
                {
                    for ( int i = 0; i < _usedColorScaleValueSetList.size(); i++ )
                    {
                        extractedColorScaleValueSet = (ColorScaleValueSet) _usedColorScaleValueSetList.get( i );
                        if ( extractedColorScaleValueSet.getColorUseString().equalsIgnoreCase( selectedColorUse ) )
                        {
                            durationSet.add( extractedColorScaleValueSet.getDuration() );
                        }
                    }
                }
            }
            else
            {
                durationSet.add( 0.0 );
            }
        }
        
        _currentDisplayedDurationSet = durationSet;
        
        List sortedDurationStringList = getSortedDurationStringList( durationSet );

        return sortedDurationStringList;
    }
    
    private List <String> getSortedDurationStringList( SortedSet durationSet )
    {
        List <Double> sortedDurationDoubleList = new ArrayList <Double> ( durationSet );
        List <String> sortedDurationStringList = new ArrayList <String> ();
        
        
        for( Double duration : sortedDurationDoubleList )
        {
            sortedDurationStringList.add( getComboBoxDurationString( duration ) );
        }
        
        return sortedDurationStringList;
    }
    
    
    public String getComboBoxDurationString( double doubleValue )
    {
        String returnString = "";
        
        if ( Math.floor( doubleValue ) == doubleValue )
        {
            returnString = "" + ( (int) doubleValue );
        }
        else
        {
            returnString = "" + doubleValue;
        }
        
        return returnString;
    }
    
    private ColorScaleValueSet getColorScaleValueSetByColorUse( String colorUse )
    {
        List colorScaleValueSetList = null;
        ColorScaleValueSet colorScaleValueSet = null;
        String selectedUser = null;
        Double selectedDurationDouble = -1.0;
        double selectedDuration = -1;
        ColorScaleValueSet extractedColorScaleValueSet = null;
        
        String setComboBoxString = null;
        
        if ( _activeTabID == VIEW_PANEL_TAB_ID )
        {
            selectedDurationDouble = getDurationComboBoxDoubleValue( _viewTabDurationComboBox );
        }
        else
        {
            setComboBoxString = (String) _editTabSetComboBox.getSelectedItem();
            selectedDurationDouble = getDurationComboBoxDoubleValue( _editTabDurationComboBox );
        }
        int i = 0;
        
        if ( _activeTabID == EDIT_PANEL_TAB_ID ) // in Edit Tab
        {
            if ( ( setComboBoxString.equalsIgnoreCase( "default" ) ) && ( selectedDurationDouble != null ) )
            {
                colorScaleValueSetList = _dataMgr.getDefaultColorScaleValueSetList();
                while ( ( i < colorScaleValueSetList.size() ) && ( colorScaleValueSet == null ) )
                {
                    extractedColorScaleValueSet = (ColorScaleValueSet) colorScaleValueSetList.get( i );
                    if ( ( extractedColorScaleValueSet.getColorUseString().equalsIgnoreCase( colorUse ) ) &&
                            ( extractedColorScaleValueSet.getDuration() == selectedDurationDouble ) )
                    {
                        colorScaleValueSet = (ColorScaleValueSet) colorScaleValueSetList.get( i );
                    }
                    i++;
                }

            }
            else if ( ( setComboBoxString.equalsIgnoreCase( "user" ) ) && ( selectedDurationDouble != null ) )
            {
                colorScaleValueSetList = _dataMgr.getColorValueTableColorScaleValueSetList();
                if ( _activeTabID == VIEW_PANEL_TAB_ID )
                {
                    selectedUser = (String) _viewTabUserIDComboBox.getSelectedItem();
                    selectedDurationDouble = (Double) _viewTabDurationComboBox.getSelectedItem();
                }
                else
                {
                    selectedUser = (String) _editTabUserIDComboBox.getSelectedItem();
                    selectedDurationDouble = getDurationComboBoxDoubleValue( _editTabDurationComboBox );
                }
                if ( selectedDurationDouble != null )
                {
                    selectedDuration = selectedDurationDouble;

                    extractedColorScaleValueSet =  null;

                    while ( ( i < colorScaleValueSetList.size() ) && ( colorScaleValueSet == null )  )
                    {
                        extractedColorScaleValueSet = (ColorScaleValueSet) colorScaleValueSetList.get( i );
                        if ( ( extractedColorScaleValueSet.getColorUseString().equalsIgnoreCase( colorUse ) ) &&
                                ( extractedColorScaleValueSet.getUserID().equalsIgnoreCase( selectedUser ) ) &&
                                ( extractedColorScaleValueSet.getDuration() == selectedDuration ) )
                        {
                            colorScaleValueSet = extractedColorScaleValueSet;
                        }
                        i++;
                    }
                }

            }
            else if ( ( setComboBoxString.equalsIgnoreCase( "office" ) ) && ( selectedDurationDouble != null ) )
            {
                colorScaleValueSetList = _dataMgr.getColorValueTableColorScaleValueSetList();
                if ( _activeTabID == VIEW_PANEL_TAB_ID )
                {
                    selectedDurationDouble = (Double) _viewTabDurationComboBox.getSelectedItem();
                }
                else
                {
                    selectedDurationDouble = getDurationComboBoxDoubleValue( _editTabDurationComboBox );
                }

                if ( selectedDurationDouble != null )
                {
                    selectedDuration = selectedDurationDouble;

                    while ( ( i < colorScaleValueSetList.size() ) && ( colorScaleValueSet == null )  )
                    {
                        extractedColorScaleValueSet = (ColorScaleValueSet) colorScaleValueSetList.get( i );
                        if ( ( extractedColorScaleValueSet.getUserID().equalsIgnoreCase( "default" ) ) &&
                                ( extractedColorScaleValueSet.getColorUseString().equalsIgnoreCase( colorUse ) ) &&
                                ( extractedColorScaleValueSet.getDuration() == selectedDuration ) )
                        {
                            colorScaleValueSet = extractedColorScaleValueSet;
                        }
                        i++;
                    }
                }

            }
        }
        else
        {
            double duration = -1.0;
            if ( _viewTabDurationComboBox.getSelectedItem() != null )
            {
                Double durationDouble = getDurationComboBoxDoubleValue( _viewTabDurationComboBox );
                duration = durationDouble;
            }
            
            extractedColorScaleValueSet = null;
            while ( ( i < _usedColorScaleValueSetList.size() ) && ( colorScaleValueSet == null ) )
            {
                extractedColorScaleValueSet = (ColorScaleValueSet) _usedColorScaleValueSetList.get( i );

                if ( ( ( extractedColorScaleValueSet.getColorUseString().equalsIgnoreCase( colorUse ) ) ||
                        ( extractedColorScaleValueSet.getColorUseString().equalsIgnoreCase( colorUse ) ) ) &&
                        ( ( extractedColorScaleValueSet.getDuration() ) == duration ) )
                {
                    colorScaleValueSet = extractedColorScaleValueSet;
                }
                i++;
            }
        }
        return colorScaleValueSet;
    }
    
    private Double getDurationComboBoxDoubleValue( JComboBox comboBox )
    {
        Double returnValue = null;
        
        Object selectedItem = comboBox.getSelectedItem();
        
        if ( selectedItem instanceof String )
        {
            String selectedItemString = (String) selectedItem;
            if ( selectedItemString != null )
            {
                returnValue = Double.parseDouble( selectedItemString );
            }
        }
        else if ( selectedItem instanceof Integer )
        {
            Integer selectedItemInteger = (Integer) selectedItem;
            if ( selectedItemInteger != null )
            {
                returnValue = selectedItemInteger.doubleValue();
            }
        }
        return returnValue;
    }
    
    private void updateColorUseComboBox( String userID )
    {
        List colorUseComboBoxList = new ArrayList();
        List colorScaleValueSetList = _dataMgr.getColorValueTableColorScaleValueSetList();
        Set colorUseSet = new HashSet();
        ColorScaleValueSet colorScaleValueSet = null;
        
        List colorUseNamesList = new ArrayList();

        for ( int i = 0; i < colorScaleValueSetList.size(); i++ )
        {
            colorScaleValueSet = (ColorScaleValueSet) colorScaleValueSetList.get( i );
            if ( colorScaleValueSet.getUserID().equalsIgnoreCase( userID ) )
            {
                colorUseNamesList.add( colorScaleValueSet.getColorUseString() );
            }
        }

        colorUseSet.addAll( colorUseNamesList );

        colorUseComboBoxList.addAll( colorUseSet );

        if ( _activeTabID == VIEW_PANEL_TAB_ID )
        {
            _viewTabColorUseComboBox.removeAllItems();

            for ( int i = 0; i < colorUseComboBoxList.size(); i++ )
            {
                _viewTabColorUseComboBox.addItem( (String) colorUseComboBoxList.get( i ) );
            }
        }
        else
        {
            _editTabColorUseComboBox.removeAllItems();

            for ( int i = 0; i < colorUseComboBoxList.size(); i++ )
            {
                _editTabColorUseComboBox.addItem( (String) colorUseComboBoxList.get( i ) );
            }
        }
    }
    
    private void clickLeftVCRButton( JComboBox setComboBox, JComboBox colorUseComboBox, JComboBox durationComboBox )
    {
        int index = 0;

        if ( _activeTabID == EDIT_PANEL_TAB_ID )
        {
            String selectedSetString = (String) setComboBox.getSelectedItem();
            String selectedColorUseString = (String) colorUseComboBox.getSelectedItem();

            if ( ( selectedSetString.equalsIgnoreCase( "user" ) ) && ( selectedColorUseString.equalsIgnoreCase( "precipitation" ) ) )
            {
                index = durationComboBox.getSelectedIndex();
                int durationItemCount = durationComboBox.getItemCount();

                if ( index == 0 )
                {
                    index = durationItemCount - 1;
                }
                else
                {
                    index--;
                }
                durationComboBox.setSelectedIndex( index );

            }
            else
            {
                index = colorUseComboBox.getSelectedIndex();
                int colorUseItemCount = colorUseComboBox.getItemCount();

                if ( index == 0 )
                {
                    index = colorUseItemCount - 1;
                }
                else
                {
                    index--;
                }
                colorUseComboBox.setSelectedIndex( index );
            }
        }
        else
        {
            index = colorUseComboBox.getSelectedIndex();
            int colorUseItemCount = colorUseComboBox.getItemCount();

            if ( index == 0 )
            {
                index = colorUseItemCount - 1;
            }
            else
            {
                index--;
            }
            colorUseComboBox.setSelectedIndex( index );
        }

        displayColorScaleValues();
    }

    private void clickRightVCRButton( JComboBox setComboBox, JComboBox colorUseComboBox, JComboBox durationComboBox )
    {
        int index = 0;
     
        if ( _activeTabID == EDIT_PANEL_TAB_ID )
        {
            String selectedSetString = (String) setComboBox.getSelectedItem();
            String selectedColorUseString = (String) colorUseComboBox.getSelectedItem();
            if ( ( selectedSetString.equalsIgnoreCase( "user" ) ) && ( selectedColorUseString.equalsIgnoreCase( "precipitation" ) ) )
            {
                index = durationComboBox.getSelectedIndex();
                int durationItemCount = durationComboBox.getItemCount();

                if ( index == ( durationItemCount - 1 ) )
                {
                    index = 0;
                }
                else
                {
                    index++;
                }
                durationComboBox.setSelectedIndex( index );
            }
            else
            {
                index = colorUseComboBox.getSelectedIndex();
                int colorUseItemCount = colorUseComboBox.getItemCount();

                if ( index == ( colorUseItemCount - 1 ) )
                {
                    index = 0;
                }
                else
                {
                    index++;
                }
                colorUseComboBox.setSelectedIndex( index );
            }
        }
        else
        {
            index = colorUseComboBox.getSelectedIndex();
            int colorUseItemCount = colorUseComboBox.getItemCount();

            if ( index == ( colorUseItemCount - 1 ) )
            {
                index = 0;
            }
            else
            {
                index++;
            }
            colorUseComboBox.setSelectedIndex( index );
        }
        
        displayColorScaleValues();
    }
    
    private ColorScaleValueSet getNewColorScaleValueSet()
    {
        ColorScaleValueSet colorScaleValueSet = new ColorScaleValueSet();
        ScaleValueSet scaleValueSet = new ScaleValueSet();
        ColorSet colorSet = new ColorSet();
        
        scaleValueSet.addScaleValue( -9999 );
        scaleValueSet.addScaleValue( -8888 );
        
        colorSet.addNamedColor( new Color( 0, 0, 0 ) );
        colorSet.addNamedColor( new Color( 102, 102, 102 ) );
        
        colorScaleValueSet.setScaleValueSet( scaleValueSet );
        colorScaleValueSet.setColorSet( colorSet );
        return colorScaleValueSet;
    }
    
	private void addListeners()
	{
        WindowCloserListener windowCloser = new WindowCloserListener();
        
        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _viewTabColorUseComboBox.addActionListener( new ColorUseComboBoxSelectionListener() );
        _editTabColorUseComboBox.addActionListener( new ColorUseComboBoxSelectionListener() );
        _editTabSetComboBox.addActionListener( new SetComboBoxSelectionListener() );
        _viewTabDurationComboBox.addActionListener( new DurationComboBoxListener() );
        _editTabDurationComboBox.addActionListener( new DurationComboBoxListener() );
        _tabbedPane.addChangeListener( new TabChangeListener() );
        _viewTabVCRLeftColorButton.addActionListener( new ViewTabLeftVCRButtonColorListener() );
        _viewTabVCRRightColorButton.addActionListener( new ViewTabRightVCRButtonColorListener() );
        _editTabVCRLeftScaleValueButton.addActionListener( new EditTabLeftVCRButtonScaleValueListener() );
        _editTabVCRRightScaleValueButton.addActionListener( new EditTabRightVCRButtonScaleValueListener() );
        _frameContentPane.addMouseWheelListener( new ColorUseComboBoxMouseWheelListener() );
        _viewTabUserIDComboBox.addActionListener( new UserIDComboBoxListener() );
        _editTabUserIDComboBox.addActionListener( new UserIDComboBoxListener() );
        _editTabAddColorSet.addActionListener( new AddColorSet() );
        _editTabDeleteColorSet.addActionListener( new DeleteColorSet() );
        _editTabResetColorButton.addActionListener( new ResetColorSetListener() );
        _editTabColor.addMouseListener( new ClickEditTabColor() );
        _saveAsUserButton.addActionListener( new SaveAsUserButtonListener() );
        _saveAsOfficeButton.addActionListener( new SaveAsOfficeButtonListener() );
        _deleteUserButton.addActionListener( new DeleteAsUserButtonListener() );
        _deleteOfficeButton.addActionListener( new DeleteAsOfficeButtonListener() );
	}

    
    private class DeleteAsUserButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( DialogHelper.displayConfirmDialog( ColorChooserDialog.this, "Are you sure you want to delete the user set " + 
                                                  ( (String) _editTabColorUseComboBox.getSelectedItem() ), "Delete Confirmation" ) )
            {
                ColorScaleValueSet colorScaleValueSetToDelete = getCurrentlyDisplayedColorScaleValueSet();
                _dataMgr.deleteAsUser(  colorScaleValueSetToDelete );
                _editTabSetComboBox.setSelectedItem( "user" );
                _editTabUserIDComboBox.setSelectedItem( _userID );

                String colorUseString = colorScaleValueSetToDelete.getColorUseString();
                
                if ( userHasColorUse( colorUseString, _userID ) )
                {
                    _editTabColorUseComboBox.setSelectedItem( colorUseString );
                }
                updateUsedColorScaleValueSetList();
                displayColorScaleValues();
                setLastUpdateTimeToCurrentTime();
            }
        }
    }
    
    private boolean userHasColorUse( String colorUseString, String userID )
    {
        boolean foundColorUse = false;
        List userColorScaleValueSetList = getFilteredColorScaleValueSetList( userID, colorUseString );
        if ( userColorScaleValueSetList.size() > 0 )
        {
            foundColorUse = true;
        }
        
        return foundColorUse;
    }
    
    
    private class DeleteAsOfficeButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( DialogHelper.displayConfirmDialog( ColorChooserDialog.this, "Are you sure you want to delete the office set " + 
                    ( (String) _editTabColorUseComboBox.getSelectedItem() ), "Delete Confirmation" ) )
            {
                ColorScaleValueSet colorScaleValueSetToDelete = getCurrentlyDisplayedColorScaleValueSet();
                _dataMgr.deleteAsOffice( colorScaleValueSetToDelete );
                _editTabSetComboBox.setSelectedItem( "office" );
                String colorUseString = colorScaleValueSetToDelete.getColorUseString();
                
                if ( userHasColorUse( colorUseString, "default" ) )
                {
                    _editTabColorUseComboBox.setSelectedItem( colorUseString );
                }

                updateUsedColorScaleValueSetList();
                displayColorScaleValues();
                setLastUpdateTimeToCurrentTime();
            }
        }
    }
    
    private class SaveAsOfficeButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( DialogHelper.displayConfirmDialog( ColorChooserDialog.this, "Are you sure you want to save the displayed color set as the office set?", "Save Confirmation" ) )
            {
                Double durationDouble = getDurationComboBoxDoubleValue( _editTabDurationComboBox );
                ColorScaleValueSet colorScaleValueSetToSave = getCurrentlyDisplayedColorScaleValueSet();
                if ( ( colorScaleValueSetToSave == null ) || ( durationDouble == null ) ) 
                {
                    DialogHelper.displayErrorDialog( ColorChooserDialog.this, "No color set is displayed", "Unable to save" );
                }
                else
                {
                    double duration = durationDouble;
                    String colorUseToSaveString = (String) _editTabSaveAsColorUseComboBox.getSelectedItem();
                    
                    colorScaleValueSetToSave.setDuration( duration );
                    colorScaleValueSetToSave.setColorUseString( colorUseToSaveString );
                    
                    _dataMgr.saveAsOffice(  colorScaleValueSetToSave );
                    setLastUpdateTimeToCurrentTime();
                }
                String setString = (String) _editTabSetComboBox.getSelectedItem();

                if ( setString.equalsIgnoreCase( "default" ) )
                {
                    _editTabDurationComboBox.setSelectedItem( 0 );
                }
                updateUsedColorScaleValueSetList();
                displayColorScaleValues();
            }
        }
    }
    
    private class SaveAsUserButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e ) 
        {
            if ( DialogHelper.displayConfirmDialog( ColorChooserDialog.this, "Are you sure you want to save the displayed color set as your user set?", "Save Confirmation" ) )
            {
                Double durationDouble = getDurationComboBoxDoubleValue( _editTabDurationComboBox );
                ColorScaleValueSet colorScaleValueSetToSave = getCurrentlyDisplayedColorScaleValueSet();
                if ( ( colorScaleValueSetToSave == null ) || ( durationDouble == null ) ) 
                {
                    DialogHelper.displayErrorDialog( ColorChooserDialog.this, "No color set is displayed", "Unable to save" );
                }
                else
                {
                    double duration = getDurationComboBoxDoubleValue( _editTabDurationComboBox );
                    String colorUseToSaveString = (String) _editTabSaveAsColorUseComboBox.getSelectedItem();
                    colorScaleValueSetToSave.setDuration( duration );
                    colorScaleValueSetToSave.setColorUseString( colorUseToSaveString );
                    
                    _dataMgr.saveAsUser( colorScaleValueSetToSave, duration );
                    setLastUpdateTimeToCurrentTime();
                }
                String setString = (String) _editTabSetComboBox.getSelectedItem();
                
                if ( setString.equalsIgnoreCase( "default" ) )
                {
                    _editTabDurationComboBox.setSelectedIndex( 0 );
                }

                updateUsedColorScaleValueSetList();
                displayColorScaleValues();
            }
        }
    }

    private ColorScaleValueSet getCurrentlyDisplayedColorScaleValueSet()
    {
        return _displayedColorScaleValueSet;
    }
    
    private void updateUsedColorScaleValueSetList()
    {
        _dataMgr.resetColorValueTableColorScaleValueSetList();
        List hvDefaultColorList = _dataMgr.getDefaultColorScaleValueSetList();
        List colorValueTableList = _dataMgr.getColorValueTableColorScaleValueSetList();
        String colorUseString = null;
        String userIDString = null;
        double duration = 0.0;
        Set colorUseNamesSet = new HashSet();
        Set usedColorScaleValueSetsSet = new HashSet();
        ColorScaleValueSet matchingColorScaleValueSet = null;
        
        ColorScaleValueSet colorScaleValueSet = null;

        _usedColorScaleValueSetList = new ArrayList();
        
        // Find used ColorScaleValueSet (user/office/hardcoded) for each default ColorUse
        for ( int i = 0; i < hvDefaultColorList.size(); i++ )
        {
            colorScaleValueSet = (ColorScaleValueSet) hvDefaultColorList.get( i );
            matchingColorScaleValueSet = findMatchingUsedSet( colorScaleValueSet );
            usedColorScaleValueSetsSet.add( matchingColorScaleValueSet );
            colorUseNamesSet.add( matchingColorScaleValueSet.getColorUseString() );
        }
        
        // To include "non standard durations" for precip
        for ( int i = 0; i < colorValueTableList.size(); i++ )
        {
            colorScaleValueSet = (ColorScaleValueSet) colorValueTableList.get( i );

            userIDString = colorScaleValueSet.getUserID();
            
            if ( ( userIDString.equalsIgnoreCase( _userID ) ) || ( userIDString.equalsIgnoreCase( "default" ) ) )
            {
                colorUseString = colorScaleValueSet.getColorUseString();

                matchingColorScaleValueSet = findMatchingUsedSet( colorScaleValueSet );
                usedColorScaleValueSetsSet.add( matchingColorScaleValueSet );
            }
        }
        _usedColorScaleValueSetList.addAll( usedColorScaleValueSetsSet );
    }
    
    private ColorScaleValueSet findMatchingUsedSet( ColorScaleValueSet defaultColorScaleValueSet )
    {
        ColorScaleValueSet usedColorScaleValueSet = ColorScaleValueSet.getCopyOfColorScaleValueSet( defaultColorScaleValueSet );
        
        List colorScaleValueSetFromDatabaseList = _dataMgr.getColorValueTableColorScaleValueSetList();
        ColorScaleValueSet extractedColorScaleValueSet = null;
        String colorUseName = usedColorScaleValueSet.getColorUseString();
        double duration = usedColorScaleValueSet.getDuration();
        String extractedColorUseString = null;
        boolean foundUserSet = false;
        int i = 0;
        
        while ( ( i < colorScaleValueSetFromDatabaseList.size() ) && ( ! foundUserSet ) )
        {
            extractedColorScaleValueSet = (ColorScaleValueSet) colorScaleValueSetFromDatabaseList.get( i );
            extractedColorUseString = extractedColorScaleValueSet.getColorUseString();
            
            if ( ( colorUseName.equalsIgnoreCase( extractedColorUseString ) ) &&
                    ( duration == ( extractedColorScaleValueSet.getDuration() ) ) )
            {
                String extractedUserIDString = extractedColorScaleValueSet.getUserID();
                
                if ( extractedUserIDString.equalsIgnoreCase( "default" ) )
                {
                    usedColorScaleValueSet = new ColorScaleValueSet( extractedColorScaleValueSet );
                }
                else if ( extractedUserIDString.equalsIgnoreCase( _userID ) )
                {
                    usedColorScaleValueSet = new ColorScaleValueSet( extractedColorScaleValueSet );

                    foundUserSet = true;
                }
            }
            i++;
        }
        
        return usedColorScaleValueSet;
    }
    
    private class ResetColorSetListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e ) 
        {
            _dataMgr.resetColorValueTableColorScaleValueSetList();
            _dataMgr.resetHvDefaultColorList();
            
            displayColorScaleValues();
        }
    }
    
    private void displayColorPickerDialog()
    {
        ColorHolder colorHolder = new ColorHolder( _editTabColor.getBackground() );

        ColorPickerDialog colorPicker = new ColorPickerDialog( ColorChooserDialog.this, colorHolder );
        colorPicker.setPreferredSize( new Dimension( 300, 300 ) );
        colorPicker.setVisible( true );

        NamedColor namedColor = getClosestNamedColor( colorHolder.getColor() );

        _editTabColor.setBackground( namedColor );
    }
    
    private class ClickEditTabColor extends MouseAdapter
    {
        public void mousePressed( MouseEvent e )
        {
            displayColorPickerDialog();
        }
    }
    
    private class DeleteColorSet implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            try
            {
                double scaleValueToDelete = Double.valueOf( _editTabScaleValue.getText() );
                int i = 0;
                int index = -1;
                String colorUseComboBoxString = (String) _editTabColorUseComboBox.getSelectedItem();
                ColorScaleValueSet colorScaleValueSet = getColorScaleValueSetByColorUse( colorUseComboBoxString );
                ScaleValueSet scaleValueSet = colorScaleValueSet.getScaleValueSet();
                ColorSet colorSet = colorScaleValueSet.getColorSet();
                
                while ( ( i < scaleValueSet.getScaleValueSet().size() ) && ( index == -1 ) )
                {
                    double scaleValue = (Double) scaleValueSet.getScaleValueSet().get( i );
                    
                    if ( closeEnoughToBeEqual( scaleValueToDelete, scaleValue ) )
                    {
                        index = i;
                    }
                    i++;
                }
                
                if ( index != -1 )
                {
                    scaleValueSet.deleteScaleValueAtIndex( index );
                    colorSet.deleteNamedColorAtIndex( index );
                }
                
                displayColorScaleValues();

            }
            catch ( NumberFormatException error )
            {
                DialogHelper.displayErrorDialog( ColorChooserDialog.this, "Can not delete Missing/Below Threshold value", "Unable to Delete" );
            }
            
        }
    }
    
    private void addColorSet()
    {
        String editTabScaleValueString = _editTabScaleValue.getText().trim();
        double newScaleValue = -1;
        boolean validScaleValue = true;
        
        if ( ! editTabScaleValueString.equalsIgnoreCase( "" ) )
        {
            if ( editTabScaleValueString.equalsIgnoreCase( "MSG" ) )
            {
                newScaleValue = -9999;
            }
            else if ( editTabScaleValueString.equalsIgnoreCase( "< Min" ) )
            {
                newScaleValue = -8888;
            }
            else
            {
                try 
                {
                    newScaleValue = Double.valueOf( editTabScaleValueString );
                }
                catch (NumberFormatException ex )
                {
                    validScaleValue = false;
                }
            }

            if ( validScaleValue )
            {
                int index = -1;
                int i = 0;
                boolean replaceValue = false;

                String colorUseComboBoxString = (String) _editTabColorUseComboBox.getSelectedItem();
                ColorScaleValueSet colorScaleValueSet = getColorScaleValueSetByColorUse( colorUseComboBoxString );
                if ( colorScaleValueSet == null )
                {
                    colorScaleValueSet = _displayedColorScaleValueSet;
                    Double durationDouble = getDurationComboBoxDoubleValue( _editTabDurationComboBox );
                    colorScaleValueSet.setColorUseString( colorUseComboBoxString );
                    colorScaleValueSet.setDuration( durationDouble );
                    _dataMgr.addColorScaleValueSetToColorValueTableList( colorScaleValueSet );
                }
                
                ScaleValueSet scaleValueSet = colorScaleValueSet.getScaleValueSet();
                ColorSet colorSet = colorScaleValueSet.getColorSet();
                if ( ( scaleValueSet != null ) && ( colorSet != null ) )
                {
                    while ( ( i < scaleValueSet.getScaleValueSet().size() ) && ( index == -1 ) )
                    {
                        double scaleValue = (Double) scaleValueSet.getScaleValueSet().get( i );

                        if ( closeEnoughToBeEqual(newScaleValue, scaleValue) )
                        {
                            index = i;
                            replaceValue = true;
                        }

                        if ( newScaleValue < scaleValue )
                        {
                            index = i;
                        }
                        i++;
                    }
                }

                if ( index == -1 )
                {
                    index = i;
                }

                if ( replaceValue )
                {
                    scaleValueSet.replaceScaleValueAtIndex( newScaleValue, index );
                    colorSet.replaceNamedColorAtIndex( _editTabColor.getBackground(), index );
                }
                else
                {
                    scaleValueSet.addScaleValueAtIndex( newScaleValue, index );
                    colorSet.addNamedColorAtIndex( _editTabColor.getBackground(), index );
                }
                
                _displayedColorScaleValueSet = colorScaleValueSet;
                displayColorScaleValues();
            }
            else 
            {
                DialogHelper.displayErrorDialog( ColorChooserDialog.this, "Invalid ScaleValue entered", "Invalid ScaleValue" );
            }
        }
    }
    
    private class AddColorSet implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            addColorSet();
        }
    }
    
    private boolean closeEnoughToBeEqual(double value1, double value2)
    {
        boolean result = false;
        
        if (Math.abs((value1 - value2)) < 0.0001)
        {
            result = true;   
        }
        
        return result;
        
    }
    
    private class ViewTabLeftVCRButtonColorListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            clickLeftVCRButton( null, _viewTabColorUseComboBox, _viewTabDurationComboBox );
        }
    }

    private class ViewTabRightVCRButtonColorListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            clickRightVCRButton( null, _viewTabColorUseComboBox, _viewTabDurationComboBox );
        }
    }

    private class EditTabLeftVCRButtonScaleValueListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            clickLeftVCRButton( _editTabSetComboBox, _editTabColorUseComboBox, _editTabDurationComboBox );
        }
    }

    private class EditTabRightVCRButtonScaleValueListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            clickRightVCRButton( _editTabSetComboBox, _editTabColorUseComboBox, _editTabDurationComboBox );
        }
    }

    private class ColorUseComboBoxMouseWheelListener implements MouseWheelListener
    {

        public void mouseWheelMoved( MouseWheelEvent e ) 
        {
            
            if ( _activeTabID == VIEW_PANEL_TAB_ID )
            {
                if ( e.getWheelRotation() > 0 ) 
                {
                    clickLeftVCRButton( null, _viewTabColorUseComboBox, _viewTabDurationComboBox );
                }
                else
                {
                    clickRightVCRButton( null, _viewTabColorUseComboBox, _viewTabDurationComboBox );
                }
            }
            else
            {
                if ( e.getWheelRotation() > 0 ) 
                {
                    clickLeftVCRButton( _editTabSetComboBox, _editTabColorUseComboBox, _editTabDurationComboBox );
                }
                else
                {
                    clickRightVCRButton( _editTabSetComboBox, _editTabColorUseComboBox, _editTabDurationComboBox  );
                }
            }
        }
    }
    
    
    private class TabChangeListener implements ChangeListener
    {

        public void stateChanged( ChangeEvent e ) 
        {
            JTabbedPane jTabbedPane = (JTabbedPane) e.getSource();
            String activeTabString = jTabbedPane.getSelectedComponent().getName();

            jTabbedPane.setBackground( Color.LIGHT_GRAY ) ;

            if ( activeTabString.equalsIgnoreCase( "ViewPanelTab" ) )
            {
                _activeTabID = VIEW_PANEL_TAB_ID;
                _dataMgr.resetColorValueTableColorScaleValueSetList();
                _dataMgr.resetHvDefaultColorList();
//                displayUsedColorSets();
            }
            else if ( activeTabString.equalsIgnoreCase( "EditPanelTab" ) )
            {
                _activeTabID = EDIT_PANEL_TAB_ID;
                _editTabSetComboBox.setSelectedItem( "default" );
            }
            displayColorScaleValues();
        }
    }
    
    private class ColorUseComboBoxSelectionListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( ! _updatingComboBoxes )
            {
                if ( _activeTabID == VIEW_PANEL_TAB_ID )
                {
                    String colorUseString = (String) _viewTabColorUseComboBox.getSelectedItem();
                    
                    if ( ( colorUseString != null ) && ( ! colorUseString.equalsIgnoreCase( "-----" ) ) )
                    {
                        UpdateDurationComboBoxByColorUseComboBox( _viewTabColorUseComboBox, _viewTabDurationComboBox );
                    }
                }
                else
                {
                    String colorUseString = (String) _editTabColorUseComboBox.getSelectedItem();

                    if ( ( colorUseString != null ) && ( ! colorUseString.equalsIgnoreCase( "-----" ) ) )
                    {
                        UpdateDurationComboBoxByColorUseComboBox( _editTabColorUseComboBox, _editTabDurationComboBox );
                    }
                }
            }
        }
    }
    
    private void UpdateDurationComboBoxByColorUseComboBox( JComboBox colorUseComboBox, JComboBox durationComboBox )
    {
        List durationSet = getDurationSet();
        String selectedColorUseString = (String) colorUseComboBox.getSelectedItem();
        String setString = null;

        if ( _activeTabID == VIEW_PANEL_TAB_ID )
        {
            setString = _viewTabUsedColorsSetLabel.getText();
        }
        else
        {
            setString = (String) _editTabSetComboBox.getSelectedItem();
        }
        if ( selectedColorUseString != null )
        {
            List<String> durationList = new ArrayList( durationSet );

            durationComboBox.removeAllItems();

            for( String duration : durationList )
            {
                durationComboBox.addItem( duration );
            }
            if ( _activeTabID == EDIT_PANEL_TAB_ID )
            {
                _editTabSaveAsColorUseComboBox.setSelectedItem( selectedColorUseString );
            }
        }
        
        if ( colorUseComboBox.getItemCount() > 0 )
        {
            displayColorScaleValues();
        }
        String colorUseString = (String) _editTabColorUseComboBox.getSelectedItem();
    }
    
    private class UserIDComboBoxListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( ! _updatingComboBoxes )
            {

                String selectedUserID = null;

                if ( _activeTabID == VIEW_PANEL_TAB_ID )
                {
                    selectedUserID = (String) _viewTabUserIDComboBox.getSelectedItem();
                }
                else
                {
                    selectedUserID = (String) _editTabUserIDComboBox.getSelectedItem();
                    if ( isUsersSet() )
                    {
                        _deleteUserButton.setEnabled( true );
                    }
                    else
                    {
                        _deleteUserButton.setEnabled( false );
                    }
                }

                updateColorUseComboBox( selectedUserID );
            }
        }
    }
    
    private class DurationComboBoxListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( ! _updatingComboBoxes )
            {

                if ( _activeTabID == VIEW_PANEL_TAB_ID )
                {
                    UpdateColorScaleValuesByDurationUpdate( _viewTabUsedColorsSetLabel.getText(), _viewTabUserIDComboBox );
                }
                else  // In the edit tab
                {
                    Double durationDouble = getDurationComboBoxDoubleValue( _editTabDurationComboBox );
                    if ( durationDouble != null )
                    {
                        if ( ( _currentDisplayedDurationSet != null ) && ( _currentDisplayedDurationSet.contains( durationDouble ) ) )
                        {
                            String editTabSetComboBoxString = (String) _editTabSetComboBox.getSelectedItem();
                            UpdateColorScaleValuesByDurationUpdate( editTabSetComboBoxString, _editTabUserIDComboBox );
                        }
                    }
                }
            }
        }
    }
    
    private void UpdateColorScaleValuesByDurationUpdate( String setComboBoxString, JComboBox userIDComboBox )
    {
        String selectedUserID = null;
        
        if ( setComboBoxString.indexOf( "user" ) != -1  )
        {
            if ( _activeTabID == EDIT_PANEL_TAB_ID )
            {
                selectedUserID = (String) userIDComboBox.getSelectedItem();
            }
            else
            {
                selectedUserID = _userID;
            }
        }
        
        displayColorScaleValues();        
    }
    
    
    private class SetComboBoxSelectionListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( _activeTabID == VIEW_PANEL_TAB_ID )
            {
                populateSetComboBox( _viewTabUsedColorsSetLabel.getText(), _viewTabColorUseComboBox );
            }
            else
            {
                String editTabSetComboBoxString = (String) _editTabSetComboBox.getSelectedItem();
                
                populateSetComboBox( editTabSetComboBoxString, _editTabColorUseComboBox );
                if ( isUsersSet() )
                {
                    _deleteUserButton.setEnabled( true );
                }
                else
                {
                    _deleteUserButton.setEnabled( false );
                }
                if ( isOfficeSet() )
                {
                    _deleteOfficeButton.setEnabled( true );
                }
                else
                {
                    _deleteOfficeButton.setEnabled( false );
                }

            }
        }
    }

    private boolean isUsersSet()
    {
        boolean isUserSet = false;
        
        String setString = (String) _editTabSetComboBox.getSelectedItem();
        String userID = (String) _editTabUserIDComboBox.getSelectedItem();
        
        if ( 
                ( setString != null ) &&
                ( userID != null ) &&
                ( setString.equalsIgnoreCase( "user" ) ) &&
                ( userID.equalsIgnoreCase( _userID ) ) )
        {
            isUserSet = true;
        }
        
        return isUserSet;
    }
    
    private boolean isOfficeSet()
    {
        boolean isOfficeSet = false;
        
        String setString = (String) _editTabSetComboBox.getSelectedItem();
        
        if ( 
                ( setString != null ) &&
                ( setString.equalsIgnoreCase( "office" ) ) )
        {
            isOfficeSet = true;
        }
        
        return isOfficeSet;
    }
    
    private void populateSetComboBox( String setComboBoxString, JComboBox colorUseComboBox )
    {
        String[] colorUseStringArray = getColorUseComboBoxString();

        if ( setComboBoxString.equalsIgnoreCase( "default" ) )
        {
            colorUseComboBox.removeAllItems();

            for ( int i = 0; i < colorUseStringArray.length; i++ )
            {
                colorUseComboBox.addItem( colorUseStringArray[ i ] );
            }
        }
        else if ( setComboBoxString.equalsIgnoreCase( "user" ) )
        {
            updateUserIDComboBox();
        }
        else if ( colorUseStringArray.length == 0 ) // empty database and office is selected
        {
            colorUseComboBox.removeAllItems();
            colorUseComboBox.addItem( "-----" );
        }
        else if ( setComboBoxString.equalsIgnoreCase( "office" ) )
        {
            colorUseComboBox.removeAllItems();

            for ( int i = 0; i < colorUseStringArray.length; i++ )
            {
                colorUseComboBox.addItem( colorUseStringArray[ i ] );
            }
            displayColorScaleValues();
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
    
   
    
	public static boolean validateInputParameters( String[] args )
	{
		boolean valid = false;
		
		if ( args.length < 6 )
		{
            System.out.println( "ColorChooserDialog <DBConnection String> <log file path> <application name> <userID> <default color file path> <rgb.txt file path>" );
		}
        else
        {
        	valid = true;
        }
		
		return valid;
	}
    
	private void closeWindow()
	/********************
        Purpose: Exits the program gracefully 
	 *********************/
	{
        //shutdown the thread in the ColorChooseDialog and set the _isRunning variable to false;
	    _thread.stop();
        _lastUpdateTime = -1;  //indicate that we are closed
	    ColorChooserDialog._isRunning = false;
  
        this.dispose();
	}


    public static long getLastUpdateTime()
    {
        return _lastUpdateTime;
    }
    
    private static void setLastUpdateTimeToCurrentTime()
    {
        //return the time in seconds, so divide by 1000
        _lastUpdateTime = System.currentTimeMillis()/1000;
    }
 
    public static void show(String jdbcConnectionString, String logFilePath,
                            String applicationName,  String userID, String defaultColorFilePath,
                            String rgbColorFilePath )
    {
        if (! _isRunning)  //make sure that we don't invoke 2 of these dialogs at a time
        {
            _lastUpdateTime = 0; //when called a second time, this variable needs to be reset
            
            ColorChooserDialogDataMgr dataMgr = null;
            ColorChooserDialog dialog = null;
            boolean validInputParameters = false;
            FileLogger logger = null;

            logger = new FileLogger( logFilePath );
              
            dataMgr = new ColorChooserDialogDataMgr( jdbcConnectionString, logger, applicationName, defaultColorFilePath, rgbColorFilePath);

            dialog = new ColorChooserDialog( new JFrame(), dataMgr, userID );

            _thread = new Thread(dialog);
            _thread.start();
            _isRunning = true;
        }
    }
    
    public void run()
    {
        //This is here to allow the caller to continue functioning while this window is open
        this.setVisible(true);
    }
      
	
    public static void main(String[] args) 
    {
        boolean validInputParameters = false;

        validInputParameters = ColorChooserDialog.validateInputParameters( args );

        if ( validInputParameters == true )
        {

            String jdbcConnectionString = args[0];
            String logFilePath = args[ 1 ];
            String applicationName = args[ 2 ];
            String userID = args[3];
            String defaultColorFilePath = args[ 4 ];
            String rgbColorFilePath = args[ 5 ];

            show(jdbcConnectionString, logFilePath, applicationName, userID, defaultColorFilePath, rgbColorFilePath);  
        }
    }

   
}
