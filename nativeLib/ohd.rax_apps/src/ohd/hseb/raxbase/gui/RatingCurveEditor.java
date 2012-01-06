package ohd.hseb.raxbase.gui;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import ChartDirector.Chart;

import ohd.hseb.color_chooser.ColorHolder;
import ohd.hseb.color_chooser.ColorPickerDialog;
import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxbase.RaxBaseDataMgr;
import ohd.hseb.raxbase.model.RaxRating;
import ohd.hseb.raxbase.model.RaxRatingOffset;
import ohd.hseb.raxbase.model.RaxRatingPoint;
import ohd.hseb.raxbase.model.RaxRatingShift;
import ohd.hseb.raxbase.model.ShefPE;
import ohd.hseb.raxbase.table.ArcBaseRatingJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseRatingOffsetsJTableRowData;
import ohd.hseb.raxbase.util.DateManager;
import ohd.hseb.raxbase.util.HDateSuperChooser;
import ohd.hseb.raxbase.util.RaxRatingShiftCalculator;
import ohd.hseb.rfc.chart.ChartCreationException;
import ohd.hseb.rfc.chart.ChartDataSourceException;
import ohd.hseb.rfc.chart.ChartDrawer;
import ohd.hseb.rfc.chart.DefaultChartDrawer;
import ohd.hseb.rfc.chart.DefaultChartViewer;
import ohd.hseb.rfc.chart.DefaultDataSource;
import ohd.hseb.rfc.chart.props.SeriesProperty;
import ohd.hseb.rfc.util.graphics.HDateChooserOwner;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledComboBox;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class RatingCurveEditor extends JDialog implements HDateChooserOwner
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
    private String _lid = null;
    
    private List _ratingCurveList = null;
    private List _ratingShiftList = null;
    
    private HDateSuperChooser _hDateChooser = new HDateSuperChooser( true );

    private RaxRating _selectedRaxRating = new RaxRating();
    private List _shiftedRaxRatingPointList = new ArrayList();
    
    private StringDataConverter _converter = new StringDataConverter();

    private RaxRatingShiftCalculator ratingShiftCalculator = new RaxRatingShiftCalculator();
    
    private RaxRatingShift _selectedRaxRatingShift = new RaxRatingShift();
    
    private Map _selectedRatingStringToRaxRating = new HashMap();
    
    private JButton _ratingColorButton = new JButton( "Unshifted Curve Color" );
    private JButton _shiftedRatingColorButton = new JButton( "Shifted Curve Color" );
    private Color _ratingColor = null;
    private Color _shiftedRatingColor = null;
    
    private LabeledComboBox _selectRatingLCB = new LabeledComboBox( "Select Rating Curve:" );
    
    private DefaultChartViewer _viewer = null;

    private List _peCBStringList = new ArrayList();
    private Map _peCBStringToPeMap = new HashMap();

    private enum axis { DISCHARGE, STAGE };
    private enum lineColor { UNSHIFTED, SHIFTED };

//  JTable variables
    private JTableManager _RCERatingTableManager = null;
    private List _RCERatingColumnDescriptorList = null;
    private List _ratingCurveRowDataList = null;
    private JScrollPane _ratingTableScrollPane = null;
    private JPanel _ratingJPanel = new JPanel( new GridBagLayout() );
    
//  JTable variables
    private JTableManager _RCEOffsetsTableManager = null;
    private List _RCEOffsetsColumnDescriptorList = null;
    private List _offsetsRowDataList = null;
    private JScrollPane _offsetsTableScrollPane = null;
    private JPanel _offsetsJPanel = new JPanel( new GridBagLayout() );
    
    private JPanel _ratingCurvePanel = new JPanel( new GridBagLayout() );
    private JButton _updateInsertRatingPointButton = new JButton( "Update/Insert" );
    private JButton _removeRatingPointButton = new JButton( "Remove Point" );
    private LabeledTextField _stageCurveLTF = new LabeledTextField( "       Stage:", "", 8 );
    private LabeledTextField _dischargeLTF = new LabeledTextField( "Discharge:", "", 8 );
    
    private JPanel _offsetsPanel = new JPanel( new GridBagLayout() );
    private JButton _updateInsertOffsetButton = new JButton( "Update/Insert" );
    private JButton _removeOffsetButton = new JButton( "Remove Offset" );
    private LabeledTextField _stageOffsetLTF = new LabeledTextField( " Stage:", "", 8 );
    private LabeledTextField _offsetLTF = new LabeledTextField( "Offset:", "", 8 );
    
    private JPanel _dataInputPanel = new JPanel( new GridBagLayout() );
    private LabeledComboBox _peLCB = null;
    private LabeledTextField _tblLTF = new LabeledTextField( "               Table:", "", "Number of rating table", 6 );
    private LabeledTextField _validDateLTF = new LabeledTextField( "Valid Date:", "", "Valid date and time of rating curve", 15 );
    private LabeledTextField _srcLTF = new LabeledTextField( "     Source:", "", "<HTML>Source of rating table, for example: <BR>USGS, <BR>ALERT, <BR>CODWR, <BR>USBR, <BR>USACE, <BR>OFS, etc.", 3 );
    private LabeledTextField _otherAgencyIDLTF = new LabeledTextField( "        Other Agency ID:", "", "Other Agency ID", 16 );
    private JCheckBox _rfsInput = new JCheckBox( "RFS Input" );
    private LabeledTextField _unitsLTF = new LabeledTextField( "          Units:", "", "Units", 4 );
    private LabeledTextField _allowStgLTF = new LabeledTextField( "                Allow Stage:", "", "Minimum allowable stage to which a rating curve may be extended", 10 );
    private LabeledComboBox _interpolationLCB = new LabeledComboBox( "Interpolation method:", "Method of interpolation/extrapolation used, LIN or LOG" );
    
    private JPanel _shiftPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _shiftStageALTF = new LabeledTextField( "Stage A:", "", "Stage value for first shift", 10 );
    private LabeledTextField _shiftStageBLTF = new LabeledTextField( "Stage B:", "", "Stage value for second shift", 10 );
    private LabeledTextField _shiftStageCLTF = new LabeledTextField( "Stage C:", "", "Stage value for third shift", 10 );
    private LabeledTextField _shiftStageDLTF = new LabeledTextField( "Stage D:", "", "Stage value for fourth shift", 10 );
    private LabeledTextField _shiftALTF = new LabeledTextField( "Shift A:", "", "First Shift", 10 );
    private LabeledTextField _shiftBLTF = new LabeledTextField( "Shift B:", "", "Second Shift", 10 );
    private LabeledTextField _shiftCLTF = new LabeledTextField( "Shift C:", "", "Third Shift", 10 );
    private LabeledTextField _shiftDLTF = new LabeledTextField( "Shift D:", "", "Fourth Shift", 10 );
    private LabeledTextField _datumAdjLTF = new LabeledTextField( "Datum Adjustment:", "", "Datum adjustment", 10 );
    
    private JPanel _ratingButtonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveRatingButton = new JButton( "Save Rating" );
    private JButton _clearRatingButton = new JButton( "Clear Rating" );
    
    private JButton _saveShiftsButton = new JButton( "Save Rating Shifts" );
    private JButton _clearShiftsButton = new JButton( "Clear Rating Shifts" );
    
    private JPanel _mainButtonPanel = new JPanel( new GridBagLayout() );
    private JButton _closeButton = new JButton( "Close" );
    
    private ActionListener _selectionLCBListener = new SelectRatingListener();
    
    public static final short MISSING = -9999;

    public RatingCurveEditor( JFrame frame, RaxBaseDataMgr dataMgr, String lid, String title )
    {
        super( frame, "Rating Curve Editor - " + title, true );
        _dataMgr = dataMgr;
        _lid = lid;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 1200, 875 ) );
        initGUI();
        populateDataPanel();
    }
    
    private void initGUI()
    {
        setTitle( "Rating Curve Editor - " + _lid );
        setLayout( new GridBagLayout() );
        updateRatingCurveShiftList();
        populateSelectionComboBoxes();
        setSelectedRaxRating();
        setSelectedRaxRatingShift();
        initRatingJTable();
        initOffsetsJTable();
        initRatingCurvePanel();
        initOffsetsPanel();
        initDataInputPanel();
        initShiftPanel();
        initRatingCurveGraph();
        initFrameComponents();
        initRatingButtonPanel();
        initMainButtonPanel();
        setColorLabels();
        addListeners();
        populateDataPanel();
        pack();
        setVisible( true );
    }
    
    private void updateRatingCurveShiftList()
    {
        _ratingCurveList = _dataMgr.getRatingCurveList( _lid );
        _ratingShiftList = _dataMgr.getRatingShiftList( _lid );
    }
    
    private void initOffsetsPanel()
    {
        _offsetsPanel.setBorder( BorderFactory.createTitledBorder( "Offsets" ) );

        JPanel hPanel1 = new JPanel();
        JPanel hPanel2 = new JPanel();
        JPanel hPanel3 = new JPanel();

//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _offsetsPanel, _offsetsJPanel,             0,   0,    2,    20, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( _offsetsPanel, hPanel1,                    0,  21,    2,     1, 1, 0, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _offsetsPanel, _stageOffsetLTF,            0,  22,    2,     1, 0, 0, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( _offsetsPanel, _offsetLTF,                 0,  23,    2,     1, 0, 0, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( _offsetsPanel, hPanel2,                    0,  24,    2,     1, 1, 0, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _offsetsPanel, _updateInsertOffsetButton,  0,  25,    1,     1, 1, 0, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _offsetsPanel, _removeOffsetButton,        1,  25,    1,     1, 1, 0, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _offsetsPanel, hPanel3,                    0,  26,    2,     1, 1, 0, GridBagConstraints.NONE );
    }
    
    private void initRatingCurvePanel()
    {
        _ratingCurvePanel.setBorder( BorderFactory.createTitledBorder( "Rating Points" ) );

        JPanel hPanel1 = new JPanel();
        JPanel hPanel2 = new JPanel();
        JPanel hPanel3 = new JPanel();
        _ratingJPanel.setBackground( Color.YELLOW );
        

//                                                                                             X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _ratingCurvePanel, _ratingJPanel,                   0,   0,    2,    20, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( _ratingCurvePanel, hPanel1,                         0,  21,    2,     1, 1, 0, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _ratingCurvePanel, _stageCurveLTF,                  0,  22,    2,     1, 1, 0, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( _ratingCurvePanel, _dischargeLTF,                   0,  23,    2,     1, 1, 0, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( _ratingCurvePanel, hPanel2,                         0,  24,    2,     1, 1, 0, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _ratingCurvePanel, _updateInsertRatingPointButton,  0,  25,    1,     1, 1, 0, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _ratingCurvePanel, _removeRatingPointButton,        1,  25,    1,     1, 1, 0, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _ratingCurvePanel, hPanel3,                         0,  26,    2,     1, 1, 0, GridBagConstraints.NONE );
    }
    
    private void initDataInputPanel()
    {
        _dataInputPanel.setBorder( BorderFactory.createTitledBorder( "Misc Data" ) );

        String[] interpolationStringArray = { "LIN", "LOG" };
        
        _interpolationLCB.setComboBox( interpolationStringArray );
        setupPEComboBox();


//                                                                               X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _dataInputPanel, _peLCB,              0,   0,    2,     1, 1, 1, GridBagConstraints.BOTH );

        ComponentHelper.addPanelComponent( _dataInputPanel, _tblLTF,             0,   1,    2,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( _dataInputPanel, _validDateLTF,       0,   2,    2,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( _dataInputPanel, _srcLTF,             0,   3,    2,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( _dataInputPanel, _unitsLTF,           0,   4,    2,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( _dataInputPanel, _allowStgLTF,        0,   5,    2,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( _dataInputPanel, _otherAgencyIDLTF,   0,   6,    2,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( _dataInputPanel, _rfsInput,           0,   7,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _interpolationLCB,   0,   8,    2,     1, 1, 1, GridBagConstraints.BOTH );
        _hDateChooser.addOwner( this );
        _validDateLTF.setEditTextField( false );
    }

    private void setupPEComboBox()
    {
        List shefPeStringList = new ArrayList();
        List shefPeList = _dataMgr.getShefPeList();

        for ( int i = 0; i < shefPeList.size(); i++ )
        {
            ShefPE shefPE = (ShefPE) shefPeList.get( i );
            shefPeStringList.add( getShefPEComboBoxString( shefPE ) );
            _peCBStringList.add( getShefPEComboBoxString( shefPE ) );
            _peCBStringToPeMap.put( getShefPEComboBoxString( shefPE ), shefPE.getPe() );
        }
        
        _peLCB = new LabeledComboBox( "PE:", shefPeStringList );
    }
    
    private String getShefPEComboBoxString( ShefPE shefPE )
    {
        String shefPEString = shefPE.getPe() + " " + shefPE.getName();
        
        return shefPEString;
    }

    private void initShiftPanel()
    {
        _shiftPanel.setBorder( BorderFactory.createTitledBorder( BorderFactory.createLineBorder( Color.DARK_GRAY, 2 ), "Rating Shift" ) );

        JPanel bPanel = new JPanel();
        
        bPanel.add( _saveShiftsButton );
        bPanel.add( _clearShiftsButton );

//                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _shiftPanel, _shiftStageALTF,   0,   0,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _shiftPanel, _shiftStageBLTF,   0,   1,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _shiftPanel, _shiftStageCLTF,   0,   2,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _shiftPanel, _shiftStageDLTF,   0,   3,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _shiftPanel, _shiftALTF,        2,   0,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _shiftPanel, _shiftBLTF,        2,   1,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _shiftPanel, _shiftCLTF,        2,   2,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _shiftPanel, _shiftDLTF,        2,   3,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _shiftPanel, _datumAdjLTF,      0,   4,    4,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _shiftPanel, bPanel,            0,   5,    4,     1, 1, 1, GridBagConstraints.NONE );

    }
    
    private void initRatingButtonPanel()
    {
        _ratingButtonPanel.setBorder( BorderFactory.createTitledBorder( "Rating Database Controls" ) );
        
//                                                                                  X,  Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _ratingButtonPanel, _saveRatingButton,   0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _ratingButtonPanel, _clearRatingButton,  1,  0,    1,     1, GridBagConstraints.NONE );
    }
    
    private void initMainButtonPanel()
    {
        JPanel hPanel = new JPanel();
//                                                                          X,  Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _mainButtonPanel, _closeButton,  0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _mainButtonPanel, hPanel,        0,  1,    1,     1, GridBagConstraints.NONE );
    }
    
    private void initFrameComponents()
    {
        JPanel hPanel = new JPanel();
        JPanel hPanel2 = new JPanel();
        JPanel hPanel3 = new JPanel();
        
//                                                                                      X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _selectRatingLCB,        0,   0,    6,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _dialogContentPane, hPanel,                  0,   1,    6,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _dialogContentPane, _viewer,                 0,   2,    4,    20, 1, 5, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _dialogContentPane, _ratingCurvePanel,       4,   2,    2,    10, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _offsetsPanel,           4,  12,    2,    10, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _shiftPanel,             0,  23,    4,     5, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _dialogContentPane, _dataInputPanel,         4,  23,    2,     5, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _dialogContentPane, hPanel3,                 4,  28,    2,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _dialogContentPane, _ratingButtonPanel,      4,  29,    2,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _dialogContentPane, hPanel2,                 0,  30,    6,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _dialogContentPane, _mainButtonPanel,        0,  31,    6,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        
        _viewer.setBackground( Color.YELLOW );
        _viewer.setPreferredSize( _viewer.getChartDrawer().getChartProperties().getChartDimensions().getDimensions() );
    }

    private void setColorLabels()
    {
        _ratingColor = _viewer.getChartDrawer().getChartProperties().getLegendProps().getEntry( 0 ).getColor();
        _shiftedRatingColor =_viewer.getChartDrawer().getChartProperties().getLegendProps().getEntry( 1 ).getColor(); 
    }
    
    private void setRatingColor( Color color )
    {
        _viewer.getChartDrawer().getChartProperties().getLegendProps().getEntry( 0 ).setColor( color );
        setColorLabels();
        _viewer.refresh();
    }
    
    private void setShiftedRatingColor( Color color )
    {
        _viewer.getChartDrawer().getChartProperties().getLegendProps().getEntry( 1 ).setColor( color );
        setColorLabels();
        _viewer.refresh();
    }

    private double[] getStageArray( List raxRatingPointList, axis xyaxis )
    {
        double[] doubleArray = new double[ raxRatingPointList.size() ];
        
        if ( xyaxis == axis.STAGE )
        {
            for ( int i = 0; i < raxRatingPointList.size(); i++ )
            {
                RaxRatingPoint raxRatingPoint = (RaxRatingPoint) raxRatingPointList.get( i );

                doubleArray[ i ] = raxRatingPoint.getStage();
            }
        }
        else
        {
            for ( int i = 0; i < raxRatingPointList.size(); i++ )
            {
                RaxRatingPoint raxRatingPoint = (RaxRatingPoint) raxRatingPointList.get( i );

                doubleArray[ i ] = raxRatingPoint.getDischarge();
            }
        }
        return doubleArray;
    }
    
    private void calculateRatingShift()
    {
        if ( _selectedRaxRating != null )
        {
            List raxRatingPointList = _selectedRaxRating.getRaxRatingPointList();

            ratingShiftCalculator.setUnShiftedRaxRatingPointList( raxRatingPointList );
            ratingShiftCalculator.setRaxRatingShift( _selectedRaxRatingShift );
            ratingShiftCalculator.initRatingShift();
            _shiftedRaxRatingPointList = ratingShiftCalculator.getShiftedRaxRatingList();
        }
    }
    
    private void updateGraph()
    {
        List raxRatingPointList = _selectedRaxRating.getRaxRatingPointList();
        
        calculateRatingShift();

        double[] unShiftedStageArray = getStageArray( raxRatingPointList, axis.STAGE );
        double[] unShiftedDischargeArray = getStageArray( raxRatingPointList, axis.DISCHARGE );
        double[] shiftedStageArray = getStageArray( _shiftedRaxRatingPointList, axis.STAGE );
        double[] shiftedDischargeArray = getStageArray( _shiftedRaxRatingPointList, axis.DISCHARGE );

        ChartDrawer drawer = _viewer.getChartDrawer(); 

        ( (DefaultDataSource) drawer.getPrimaryPlotData() ).setXAxisSeries( 0, unShiftedDischargeArray );
        ( (DefaultDataSource) drawer.getPrimaryPlotData() ).setYAxisSeries( 0, unShiftedStageArray );
        ( (DefaultDataSource) drawer.getPrimaryPlotData() ).setXAxisSeries( 1, shiftedDischargeArray );
        ( (DefaultDataSource) drawer.getPrimaryPlotData() ).setYAxisSeries( 1, shiftedStageArray );
        try
        {
            drawer.dataChanged( false );
        }
        catch ( ChartCreationException e )
        {
            e.printStackTrace();
        }

        _viewer.refresh();
    }
    
    private void initRatingCurveGraph()
    {
        DefaultDataSource primaryDataSource = new DefaultDataSource();
        Chart.setLicenseCode("DIST-0000-05a9-b536-41d2"); 
        List raxRatingPointList = new ArrayList();
        
        if ( _selectedRaxRating != null )
        {
            raxRatingPointList = _selectedRaxRating.getRaxRatingPointList();
        }
        
        calculateRatingShift();
        
        double[] unShiftedStageArray = getStageArray( raxRatingPointList, axis.STAGE );
        double[] unShiftedDischargeArray = getStageArray( raxRatingPointList, axis.DISCHARGE );
        double[] shiftedStageArray = getStageArray( _shiftedRaxRatingPointList, axis.STAGE );
        double[] shiftedDischargeArray = getStageArray( _shiftedRaxRatingPointList, axis.DISCHARGE );
      
        try
        {
            primaryDataSource.addNumericalSeries(
                    "Series 1",
                    unShiftedDischargeArray,
                    unShiftedStageArray,
                    SeriesProperty.LINE_PLOT);
            
            primaryDataSource.addNumericalSeries(
                    "Series 2",
                    shiftedDischargeArray,
                    shiftedStageArray,
                    SeriesProperty.LINE_PLOT);

            
            DefaultChartDrawer drawer = new DefaultChartDrawer(primaryDataSource, null);
            drawer.getChartProperties().getPlotTitle().setTextAndChangeDefault("Rating Curve");
            drawer.getChartProperties().getXAxisProps().getAxisLabelProps().setText("CFS");
            drawer.getChartProperties().getPrimaryYAxisProps().getAxisLabelProps().setText("Feet");
            drawer.getChartProperties().getLegendProps().getLegendTitleProps().setText("<html>Rating Curves<br>(Click entry to change color)</html>");
            drawer.getChartProperties().getLegendProps().getEntry(0).setText("UnShifted Curve");
            drawer.getChartProperties().getLegendProps().getEntry(1).setText("Shifted Curve");
            drawer.getChartProperties().getLegendProps().getEntry(0).setShapeStr("CIRCLE");
            drawer.getGeneralColorProperties().setPlotExteriorBGColor( Color.LIGHT_GRAY );
            drawer.setBackgroundImageFileName( "/fs/hseb/pda/users/gsood/java/trunk/ohd-common/externaljar/chart_bg.png" );
            _viewer = new DefaultChartViewer( drawer, false );
            _viewer.setPreferredSize( null );
        }
        
        catch ( ChartCreationException e )
        {
            e.printStackTrace();
        }
        catch ( ChartDataSourceException e )
        {
            e.printStackTrace();
        }
    }
    
  
    private void populateDataPanel()
    {
        if ( _selectedRaxRating != null )
        {
            long validDate = 0;
            String validDateString = null;
            
            ShefPE shefPE = (ShefPE) _dataMgr.getShefPEMap().get( _selectedRaxRating.getPe() );
            if ( shefPE != null )
            {
                _peLCB.setSelectedItem( getShefPEComboBoxString( shefPE ) );
            }
            else
            {
                _peLCB.setSelectedItem( "" );
            }
            
            validDate = _selectedRaxRating.getValidDate();
            
            if ( validDate == MISSING )
            {
                validDate = System.currentTimeMillis();
            }
            
            validDateString = DbTimeHelper.getDateTimeStringFromLongTime( validDate );

            setNumberFields( _tblLTF, _selectedRaxRating.getTable() );
            _validDateLTF.setTextField( validDateString );
            _srcLTF.setTextField( _selectedRaxRating.getSrc() );
            _unitsLTF.setTextField( _selectedRaxRating.getUnits() );
            setNumberFields( _allowStgLTF, _selectedRaxRating.getAllowStage() );
            _otherAgencyIDLTF.setTextField( _selectedRaxRating.getOthagid() );
            _rfsInput.setSelected( _selectedRaxRating.isRfsInput() );

            String interpolateString = _selectedRaxRating.getInterpolate();

            if ( interpolateString != null )
            {
                if ( interpolateString.equalsIgnoreCase( "LIN" ) )
                {
                    _interpolationLCB.setSelectedIndex( 0 );
                }
                else
                {
                    _interpolationLCB.setSelectedIndex( 1 );
                }
            }
        }
        setNumberFields( _shiftALTF, _selectedRaxRatingShift.getShiftA() );
        setNumberFields( _shiftBLTF, _selectedRaxRatingShift.getShiftB() );
        setNumberFields( _shiftCLTF, _selectedRaxRatingShift.getShiftC() );
        setNumberFields( _shiftDLTF, _selectedRaxRatingShift.getShiftD() );
        setNumberFields( _shiftStageALTF, _selectedRaxRatingShift.getValA() );
        setNumberFields( _shiftStageBLTF, _selectedRaxRatingShift.getValB() );
        setNumberFields( _shiftStageCLTF, _selectedRaxRatingShift.getValC() );
        setNumberFields( _shiftStageDLTF, _selectedRaxRatingShift.getValD() );
        setNumberFields( _datumAdjLTF, _selectedRaxRatingShift.getDatumAdjustment() );
    }
    
    private void setNumberFields( LabeledTextField labeledTextField, double doubleValue )
    {
        if ( ( DbTable.isNull( doubleValue ) ) || ( doubleValue == MISSING ) )
        {
            labeledTextField.setTextField( "" );
        }
        else
        {
            labeledTextField.setTextField( Double.toString( doubleValue ) );
        }
    }

    private void setSelectedRaxRating()
    {
        String selectionComboBoxString = _selectRatingLCB.getSelectedCBItem();
  
        _selectedRaxRating = (RaxRating) _selectedRatingStringToRaxRating.get( selectionComboBoxString );
        if ( _selectedRaxRating == null )
        {
            _selectedRaxRating = new RaxRating();
            _selectedRaxRating.setLid( _lid );
            _selectedRaxRating.setRaxRatingPointList( new ArrayList() );
            _selectedRaxRating.setOffsets( new ArrayList() );
        }
    }

    private void setSelectedRaxRatingShift()
    {
        RaxRatingShift raxRatingShift = new RaxRatingShift();
        
        for ( int i = 0; i < _ratingShiftList.size(); i++ )
        {
            raxRatingShift = (RaxRatingShift) _ratingShiftList.get( i );
            
            if ( ( _selectedRaxRating.getLid().equalsIgnoreCase( raxRatingShift.getLid() ) ) &&
                 ( _selectedRaxRating.getPe().equalsIgnoreCase( raxRatingShift.getPe() ) ) &&
                 ( _selectedRaxRating.getTable() == raxRatingShift.getRatingTableNumber() ) &&
                 ( _selectedRaxRating.getValidDate() == raxRatingShift.getBeginDate() ) &&
                 ( _selectedRaxRating.getSrc().equalsIgnoreCase( raxRatingShift.getSource() ) ) )
            {
                _selectedRaxRatingShift = raxRatingShift;
                break;
            }
        }
    }
    
    private void populateSelectionComboBoxes()
    {
        RaxRating raxRating = null;
        String selectedRatingString = null;
        List selectedRatingStringList = new ArrayList();
        
        _selectedRatingStringToRaxRating.clear();

        _selectRatingLCB.getComboBox().removeAllItems();

        for ( int i = 0; i < _ratingCurveList.size(); i++ )
        {
            raxRating = (RaxRating) _ratingCurveList.get( i );
            selectedRatingString = getSelectedRatingString( raxRating );
            selectedRatingStringList.add( selectedRatingString );
            _selectedRatingStringToRaxRating.put( selectedRatingString, raxRating );
        }
        _selectRatingLCB.setComboBox( selectedRatingStringList );
    }
    
    private void updateSelectionComboBoxes()
    {
        _selectRatingLCB.getComboBox().removeActionListener( _selectionLCBListener );
        populateSelectionComboBoxes();
        _selectRatingLCB.addComboBoxActionListener( _selectionLCBListener );
    }
    
    private String getSelectedRatingString( RaxRating raxRating )
    {
        return "PE = " + raxRating.getPe() + " | Tbl = " + raxRating.getTable() + " | ValidDate = " + 
               DbTimeHelper.getDateTimeStringFromLongTime( raxRating.getValidDate() ) + " | Src = " + 
               raxRating.getSrc();
    }
    

    private void updateRatingJTable()
    {
        _ratingCurveRowDataList = _dataMgr.getRatingCurveRowDataList( _selectedRaxRating );
        calculateRatingShift();
        updateRatingCurveRowDataList();
        _RCERatingTableManager.setChangedAllRowDataList( _ratingCurveRowDataList );
        _RCERatingTableManager.refreshDisplay();
    }
    
    private void updateOffsetsJTable()
    {
        _offsetsRowDataList = _dataMgr.getOffsetsRowDataList( _selectedRaxRating );
        _RCEOffsetsTableManager.setChangedAllRowDataList( _offsetsRowDataList );
        _RCEOffsetsTableManager.refreshDisplay();
    }
    
    private void updateRatingCurveRowDataList()
    {
        for ( int i = 0; i < _ratingCurveRowDataList.size(); i++ )
        {
            ArcBaseRatingJTableRowData rowData = (ArcBaseRatingJTableRowData) _ratingCurveRowDataList.get( i );
            RaxRatingPoint shiftedRaxRatingPoint = (RaxRatingPoint) _shiftedRaxRatingPointList.get( i );
            rowData.setShiftedStage( shiftedRaxRatingPoint.getStage() );
            rowData.addAllCellsToMap();
        }
    }
    
    
    private void initRatingJTable() 
    {
        _ratingCurveRowDataList = _dataMgr.getRatingCurveRowDataList( _selectedRaxRating );
        calculateRatingShift();
        updateRatingCurveRowDataList();
        setRatingCurveColumnDescriptorList();
        _RCERatingTableManager = new ComplexJTableManager( _RCERatingColumnDescriptorList, _ratingCurveRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _RCERatingTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _RCERatingTableManager.setDisplayableColumns( columnsSelected, true, true );
        _ratingTableScrollPane = _RCERatingTableManager.getJScrollPane();
        
//                                                                                     X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _ratingJPanel, _ratingTableScrollPane,      0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
    }

    private void initOffsetsJTable() 
    {
        _offsetsRowDataList = _dataMgr.getOffsetsRowDataList( _selectedRaxRating );
        setOffsetsColumnDescriptorList();
        _RCEOffsetsTableManager = new ComplexJTableManager( _RCEOffsetsColumnDescriptorList, _offsetsRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String selectedColumns[] = _RCEOffsetsTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _RCEOffsetsTableManager.setDisplayableColumns( selectedColumns, true, true );
        _offsetsTableScrollPane = _RCEOffsetsTableManager.getJScrollPane();

//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _offsetsJPanel, _offsetsTableScrollPane,   0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );

    }
    
    private void setRatingCurveColumnDescriptorList()
    {
        _RCERatingColumnDescriptorList = new ArrayList();

        _RCERatingColumnDescriptorList.add(new JTableColumnDescriptor( "Stage", true, 100, "center" ) );
        _RCERatingColumnDescriptorList.add(new JTableColumnDescriptor( "Shifted Stage", true, 100, "center" ) );
        _RCERatingColumnDescriptorList.add(new JTableColumnDescriptor( "Discharge", true, 100, "center" ) );
    }

    private void setOffsetsColumnDescriptorList()
    {
        _RCEOffsetsColumnDescriptorList = new ArrayList();

        _RCEOffsetsColumnDescriptorList.add(new JTableColumnDescriptor( "Stage", true, 100, "center" ) );
        _RCEOffsetsColumnDescriptorList.add(new JTableColumnDescriptor( "Offset", true, 100, "center" ) );
    }
    
    private void populateRatingPanel( ArcBaseRatingJTableRowData rowData )
    {
        setNumberFields( _stageCurveLTF, rowData.getStage() );
        setNumberFields( _dischargeLTF, rowData.getDischarge() );
    }
    
    private void populateOffsetsPanel( ArcBaseRatingOffsetsJTableRowData rowData )
    {
        setNumberFields( _stageOffsetLTF, rowData.getStage() );
        setNumberFields( _offsetLTF, rowData.getOffset() );
    }
    
    private void launchDateWindow()
    {
        GregorianCalendar calendar = new GregorianCalendar();
        long dayInMillis = 60*60*24*1000;
        long dateLong = 0;

        if ( _validDateLTF.getTextFieldText().equalsIgnoreCase( "" ) )
        {
            dateLong = System.currentTimeMillis();
        }
        else
        {
            dateLong = DbTimeHelper.getLongTimeFromDateString( _validDateLTF.getTextFieldText() ) + dayInMillis;
        }
        
        calendar.setTimeInMillis( dateLong );

        _hDateChooser.setDate( calendar );
        _hDateChooser.setModal( true );
        _hDateChooser.setVisible( true );
    }
    
    public void dateChosen( JDialog jDialog )
    {
            Calendar calendar = _hDateChooser.getDate();
            String dateString = null;
            
            if ( _hDateChooser.isClearDate() )
            {
                dateString = "";
            }
            else
            {
                dateString = DateManager.getDateTimeStringFromCalendar( calendar );
            }
            
            _validDateLTF.setTextField( dateString );
    }
    
    private void removeRatingPoint()
    {
        ArcBaseRatingJTableRowData rowData = null;
        try
        {
            rowData = (ArcBaseRatingJTableRowData) _RCERatingTableManager.getSelectedRowsData().get( 0 );
            
            RaxRatingPoint raxRatingPoint = new RaxRatingPoint( rowData.getStage(), rowData.getDischarge() );

            _selectedRaxRating.removeRatingPoint( raxRatingPoint );
            
            updateRatingJTable();
            updateGraph();
        }
        catch ( IndexOutOfBoundsException e )
        {
            DialogHelper.displayErrorDialog( this, "A Rating Point must be selected", "Delete Rating Point" );
        }
    }

    private void removeOffset()
    {
        ArcBaseRatingOffsetsJTableRowData rowData = null;
        
        try
        {
            rowData = (ArcBaseRatingOffsetsJTableRowData) _RCEOffsetsTableManager.getSelectedRowsData().get( 0 );

            RaxRatingOffset offset = new RaxRatingOffset( rowData.getStage(), rowData.getOffset() );
            
            _selectedRaxRating.removeOffset( offset );
            
            updateOffsetsJTable();
        }
        catch ( IndexOutOfBoundsException e )
        {
            DialogHelper.displayErrorDialog( this, "An offset must be selected", "Delete Offset" );
        }
    }

    private void updateInsertOffset()
    {
        try
        {
            double stage = Double.parseDouble( _stageOffsetLTF.getTextFieldText() );
            double offset = Double.parseDouble( _offsetLTF.getTextFieldText() );

            RaxRatingOffset raxRatingOffset = new RaxRatingOffset( stage, offset );

            _selectedRaxRating.addOffset( raxRatingOffset );

            updateOffsetsJTable();
        }
        catch ( NumberFormatException error )
        {
            DialogHelper.displayErrorDialog( this, "A valid stage/offset must be entered", "Update/Insert Offset error" );
        }
        catch ( NullPointerException error ){}
    }
    
    private void updateInsertRatingPoint()
    {
        try
        {
            double stage = Double.parseDouble( _stageCurveLTF.getTextFieldText() );
            double discharge = Double.parseDouble( _dischargeLTF.getTextFieldText() );

            RaxRatingPoint raxRatingPoint = new RaxRatingPoint( stage, discharge );

            _selectedRaxRating.addRatingPoint( raxRatingPoint );

            calculateRatingShift();
            updateRatingJTable();
            updateGraph();
        }
        catch ( NumberFormatException error )
        {
            DialogHelper.displayErrorDialog( this, "A valid stage/discharge must be entered", "Update/Insert Rating Point error" );
        }
        catch ( NullPointerException error )
        {
        }
    }
    private void saveShift()
    {
        RaxRatingShift raxRatingShift = new RaxRatingShift();
        
        raxRatingShift.setLid( _selectedRaxRating.getLid() );
        raxRatingShift.setPe( _selectedRaxRating.getPe() );
        raxRatingShift.setRatingTableNumber( _selectedRaxRating.getTable() );
        raxRatingShift.setBeginDate( _selectedRaxRating.getValidDate() );
        raxRatingShift.setSource( _selectedRaxRating.getSrc() );
        
        raxRatingShift.setValA( _converter.getDoubleValue( _shiftStageALTF.getTextFieldText() ) );
        raxRatingShift.setValB( _converter.getDoubleValue( _shiftStageBLTF.getTextFieldText() ) );
        raxRatingShift.setValC( _converter.getDoubleValue( _shiftStageCLTF.getTextFieldText() ) );
        raxRatingShift.setValD( _converter.getDoubleValue( _shiftStageDLTF.getTextFieldText() ) );
        raxRatingShift.setShiftA( _converter.getDoubleValue( _shiftALTF.getTextFieldText() ) );
        raxRatingShift.setShiftB( _converter.getDoubleValue( _shiftBLTF.getTextFieldText() ) );
        raxRatingShift.setShiftC( _converter.getDoubleValue( _shiftCLTF.getTextFieldText() ) );
        raxRatingShift.setShiftD( _converter.getDoubleValue( _shiftDLTF.getTextFieldText() ) );
        raxRatingShift.setDatumAdjustment( _converter.getDoubleValue( _datumAdjLTF.getTextFieldText() ) );
        
        _dataMgr.saveRaxRatingShift( raxRatingShift );
        _selectedRaxRatingShift = raxRatingShift;

        populateDataPanel();
        calculateRatingShift();
        updateRatingJTable();
        updateGraph();
    }
    
    private void clearRating()
    {
        _selectedRaxRating = new RaxRating();
        _selectedRaxRating.setTable( DbTable.getNullDouble() );
        _selectedRaxRating.setAllowStage( DbTable.getNullDouble() );
        _selectedRaxRating.setValidDate( 0 );
        populateDataPanel();
        updateRatingJTable();
        updateOffsetsJTable();
        updateGraph();
    }
    
    private void saveRating()
    {
        RaxRating raxRating = new RaxRating();
        boolean primaryKeyEntered = false;

        if ( verifyPrimaryKey() )
        {
            raxRating.setLid( _selectedRaxRating.getLid() );
            String pe = (String) _peLCB.getSelectedItem();

            raxRating.setPe( (String) _peCBStringToPeMap.get( pe ) );
            raxRating.setTable( _converter.getDoubleValue( _tblLTF.getTextFieldText() ) );
            raxRating.setValidDate( _converter.getLongDateTimeValue( _validDateLTF.getTextFieldText() ) );
            raxRating.setSrc( _converter.getString( _srcLTF.getTextFieldText() ) ); 
            raxRating.setOthagid( _converter.getString( _otherAgencyIDLTF.getTextFieldText() ) );
            raxRating.setRfsInput( _rfsInput.isSelected() );
            raxRating.setRaxRatingPointList( _selectedRaxRating.getRaxRatingPointList() );
            raxRating.setUnits( _converter.getString( _unitsLTF.getTextFieldText() ) );
            raxRating.setInterpolate( _interpolationLCB.getSelectedCBItem() );
            raxRating.setOffsets( _selectedRaxRating.getOffsets() );
            raxRating.setAllowStage( _converter.getDoubleValue( _allowStgLTF.getTextFieldText() ) );
            
            _dataMgr.saveRaxRating( raxRating );
            
            updateRatingCurveShiftList();
            updateSelectionComboBoxes();
//            updateRatingJTable();
//            updateOffsetsJTable();
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "PE, Table, ValidDate, and Src must be entered", "Error Saving Rating" );
        }
    }
    
    private boolean verifyPrimaryKey()
    {
        boolean valid = true;
        String pe = (String) _peLCB.getSelectedItem();

        if ( ( pe == null ) || ( pe.equalsIgnoreCase( "" ) ) )
        {
            valid = false;
        }
        else if ( ( _tblLTF.getTextFieldText().equalsIgnoreCase( "" ) ) || ( _tblLTF.getTextFieldText() == null ) ) 
        {
            valid = false;
        }
        else if ( ( _validDateLTF.getTextFieldText().equalsIgnoreCase( "" ) ) || ( _validDateLTF.getTextFieldText() == null ) )
        {
            valid = false;
        }
        else if ( ( _srcLTF.getTextFieldText().equalsIgnoreCase( "" ) ) || ( _srcLTF.getTextFieldText() == null ) )
        {
            valid = false;
        }
        return valid;
    }

    private void clearShift()
    {
        _shiftStageALTF.setTextField( "0" );
        _shiftStageBLTF.setTextField( "0" );
        _shiftStageCLTF.setTextField( "0" );
        _shiftStageDLTF.setTextField( "0" );
        _shiftALTF.setTextField( "0" );
        _shiftBLTF.setTextField( "0" );
        _shiftCLTF.setTextField( "0" );
        _shiftDLTF.setTextField( "0" );
        _datumAdjLTF.setTextField( "0" );
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();
        addWindowListener( windowCloser );
        
        _RCERatingTableManager.addTableListener( new RCERatingTableListener() );
        _RCEOffsetsTableManager.addTableListener(  new RCEOffsetsListener() );
        
        _validDateLTF.addTextFieldMouseListener( new DateFieldMouseListener() );

        _selectRatingLCB.addComboBoxActionListener( _selectionLCBListener );

        _ratingColorButton.addActionListener( new RatingColorButtonListener() );
        _shiftedRatingColorButton.addActionListener( new ShiftedRatingColorButtonListener() );
        
        _updateInsertRatingPointButton.addActionListener( new UpdateInsertRatingPointButtonListener() );
        _removeRatingPointButton.addActionListener( new RemoveRatingPointButtionListener() );
        
        _saveShiftsButton.addActionListener( new SaveShiftsButtonListener() );
        _clearShiftsButton.addActionListener( new ClearShiftsButtonListener() );
        
        _updateInsertOffsetButton.addActionListener( new UpdateInsertOffsetButtonListener() );
        _removeOffsetButton.addActionListener( new RemoveOffsetButtonListener() );
        
        _saveRatingButton.addActionListener( new SaveRatingButton() );
        _clearRatingButton.addActionListener( new ClearRatingButton() );
        _viewer.addMouseListener( new ViewMouserListener() );
        
        _closeButton.addActionListener( windowCloser );
    }
    
    private void displayColorPickerDialog( lineColor line )
    {
        if ( line == lineColor.UNSHIFTED )
        {
            ColorHolder colorHolder = new ColorHolder( _ratingColor );

            ColorPickerDialog colorPicker = new ColorPickerDialog( this, colorHolder );
            colorPicker.setPreferredSize( new Dimension( 300, 300 ) );
            colorPicker.setVisible( true );
            setRatingColor( colorHolder.getColor() );
        }
        else
        {
            ColorHolder colorHolder = new ColorHolder( _shiftedRatingColor );

            ColorPickerDialog colorPicker = new ColorPickerDialog( this, colorHolder );
            colorPicker.setPreferredSize( new Dimension( 300, 300 ) );
            colorPicker.setVisible( true );
            setShiftedRatingColor( colorHolder.getColor() );
        }
    }
    
    private class ViewMouserListener implements MouseListener
    {

        public void mouseClicked( MouseEvent arg0 )
        {
            int index = _viewer.getChartDrawer().retrieveSeriesLabelIndex( arg0.getPoint() );
            
            if ( index == 0 )
            {
                displayColorPickerDialog( lineColor.UNSHIFTED );
            }
            else if ( index == 1 )
            {
                displayColorPickerDialog( lineColor.SHIFTED );
            }
        }

        public void mouseEntered( MouseEvent arg0 ){}
        public void mouseExited( MouseEvent arg0 ){}
        public void mousePressed( MouseEvent e ){}
        public void mouseReleased( MouseEvent e ){}
    }

    private class SaveRatingButton implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveRating();
        }
    }
    
    private class ClearRatingButton implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            clearRating();
        }
    }
    
    private class RatingColorButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            displayColorPickerDialog( lineColor.UNSHIFTED );
        }
    }
    
    private class ShiftedRatingColorButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            displayColorPickerDialog( lineColor.SHIFTED );
        }
    }
    
    private class UpdateInsertRatingPointButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            updateInsertRatingPoint();
        }
    }
    
    private class UpdateInsertOffsetButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            updateInsertOffset();
        }
    }
    
    private class SaveShiftsButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveShift();
        }
    }
    
    private class ClearShiftsButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            clearShift();
        }
    }
    
    private class RemoveRatingPointButtionListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            removeRatingPoint();
        }
    }
    
    private class RemoveOffsetButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            removeOffset();
        }
    }
    
    private class SelectRatingListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            setSelectedRaxRating();
            setSelectedRaxRatingShift();
            populateDataPanel();
            updateRatingJTable();
            updateOffsetsJTable();
            updateGraph();
        }
    }
    
    private class RCERatingTableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                ArcBaseRatingJTableRowData rowData = (ArcBaseRatingJTableRowData) _RCERatingTableManager.getSelectedRowsData().get( 0 );
                populateRatingPanel( rowData );
            }
        }
    }
    
    private class RCEOffsetsListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            ArcBaseRatingOffsetsJTableRowData rowData = (ArcBaseRatingOffsetsJTableRowData) _RCEOffsetsTableManager.getSelectedRowsData().get( 0 );
            populateOffsetsPanel( rowData );
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

    private class DateFieldMouseListener implements MouseListener
    {
        public void mouseClicked( MouseEvent e ){}
        public void mouseEntered( MouseEvent e ){}
        public void mouseExited( MouseEvent e ){}
        public void mouseReleased( MouseEvent e ){}
        public void mousePressed( MouseEvent e )
        {
            launchDateWindow();
        }
    }

    private void closeWindow()
    /********************
        Purpose: Exits the program gracefully 
     *********************/
    {
        this.dispose();
    }

    public static void main( String args[] )
    {
        JFrame frame = new JFrame();
        frame.setSize( new Dimension( 1024, 768 ) );
        RaxBaseDataMgr dataMgr = new RaxBaseDataMgr( "jdbc:postgresql://lx5:5432/adb_ob83raxtest?user=pguser" );
        String title = "ABOVE_FS";
        String name = dataMgr.getRaxLocation( "ABOVE_FS" ).getName();
        
        if ( name != null )
        {
            title += " - " + name;
        }
        

        RatingCurveEditor ratingCurveEditor = new RatingCurveEditor( frame, dataMgr, "ABOVE_FS", title );
//        RatingCurveEditor ratingCurveEditor = new RatingCurveEditor( frame, dataMgr, "WFRC2" );
        ratingCurveEditor.displayGUI();
    }
}
