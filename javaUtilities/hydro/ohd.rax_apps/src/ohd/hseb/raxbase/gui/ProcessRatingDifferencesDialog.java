package ohd.hseb.raxbase.gui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.JFrame;
import javax.swing.ListCellRenderer;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxbase.model.RaxRating;
import ohd.hseb.raxbase.model.RaxRatingOffset;
import ohd.hseb.raxbase.model.RaxRatingPoint;
import ohd.hseb.raxbase.model.RaxRatingShift;
import ohd.hseb.raxbase.util.JListPanelSuperCellRenderer;
import ohd.hseb.raxbase.util.StringPadder;
import ohd.hseb.raxdb_sync.FieldDifference;
import ohd.hseb.raxdb_sync.RatingCurveHolder;
import ohd.hseb.raxdb_sync.RaxSyncDataMgr;
import ohd.hseb.raxdb_sync.RecordDifference;
import ohd.hseb.rfc.util.listpanels.JListPanel;
import ohd.hseb.util.StringDataConverter;

public class ProcessRatingDifferencesDialog extends ProcessDifferencesDialog
{
    public ProcessRatingDifferencesDialog( JFrame frame, RaxSyncDataMgr syncDataMgr )
    {
        super( frame, "Process Rating", "Rating", syncDataMgr, new StringPadder( 25, 35 ), new Dimension( 650, 550 ) );
        _keyString = "(LID | PE | tbl | ValidDate | Src)";
    }

    protected String getSelectionComboBoxString( RecordDifference recordDiff )
    {
        String cbString = null;
        
        RatingCurveHolder ratingCurveHolder = (RatingCurveHolder) recordDiff.getRaxRecord();
        RaxRating raxRating = ratingCurveHolder.getRatingCurve();
        
        StringPadder lidPadder = new StringPadder( 8, 8 );

        cbString = lidPadder.getFrontPaddedString( raxRating.getLid() ) + " | " +
                   raxRating.getPe() + " | " +
                   raxRating.getTable() + " | " +
                   DbTimeHelper.getDateStringFromLongTime( raxRating.getValidDate() ) + " | " +
                   raxRating.getSrc();
        
        return cbString;
    }

    protected void refreshRecordPanel()
    {
        List recordStringList = new ArrayList();
        String headerString = null;
        List highlightStringList = new ArrayList();
        StringDataConverter converter = new StringDataConverter();

        if ( _selectedRecordDiff != null )
        {
            headerString = getHeaderString();

            RatingCurveHolder ihfsRatingCurveHolder = (RatingCurveHolder) _selectedRecordDiff.getIhfsRecord();
            RatingCurveHolder raxRatingCurveHolder = (RatingCurveHolder) _selectedRecordDiff.getRaxRecord();
            
            RaxRating ihfsRating = (RaxRating) ihfsRatingCurveHolder.getRatingCurve();
            RaxRating raxRating = (RaxRating) raxRatingCurveHolder.getRatingCurve();

            List ihfsRatingPointList = ihfsRating.getRaxRatingPointList();
            List raxRatingPointList = raxRating.getRaxRatingPointList();
            
            String lid = _padder.getFrontPaddedString( ihfsRating.getLid() ) + "LID" + _padder.getEndPaddedString( "LID", raxRating.getLid() );
            String pe = _padder.getFrontPaddedString( "" ) + "PE" + _padder.getEndPaddedString( "PE", raxRating.getPe() );
            String tbl = _padder.getFrontPaddedString( "" ) + "Table" + _padder.getEndPaddedString( "Table", raxRating.getTable() );
            String validDate = _padder.getFrontPaddedString( "" ) + "ValidDate" + _padder.getEndPaddedDateString( "ValidDate", raxRating.getValidDate() );
            String src = _padder.getFrontPaddedString( "" ) + "Src" + _padder.getEndPaddedString( "Src", raxRating.getSrc() );
            String othagid = _padder.getFrontPaddedString( "" ) + "OthAgId" + _padder.getEndPaddedString( "OthAgId", raxRating.getOthagid() );
            String rfsInput = _padder.getFrontPaddedString( "" ) + "RFSInput" + _padder.getEndPaddedString( "RFSInput", raxRating.isRfsInput() );
            String units = _padder.getFrontPaddedString( "" ) + "Units" + _padder.getEndPaddedString( "Units", raxRating.getUnits() );
            String interpolate = _padder.getFrontPaddedString( "" ) + "Interpolate" + _padder.getEndPaddedString( "Interpolate", raxRating.getInterpolate() );
            String allowStg = _padder.getFrontPaddedString( "" ) + "AllowStage" + _padder.getEndPaddedString( "AllowStage", raxRating.getAllowStage() );
            
            recordStringList.add( lid );
            recordStringList.add( pe );
            recordStringList.add( tbl ); 
            recordStringList.add( validDate ); 
            recordStringList.add( src ); 
            recordStringList.add( othagid ); 
            recordStringList.add( rfsInput ); 
            recordStringList.add( units ); 
            recordStringList.add( interpolate ); 
            recordStringList.add( allowStg );

            recordStringList.add( " " );
            recordStringList.add( _padder.getFrontPaddedString( "Discharge" ) + "Stage" + _padder.getEndPaddedString( "Stage", "Discharge" ) );
            recordStringList.add( _padder.getFrontPaddedString( "---------" ) + "-----" + _padder.getEndPaddedString( "-----", "---------" ) );

            List stageList = getCombinedStageList( ihfsRatingPointList, raxRatingPointList );
            List fieldDifferenceList = _selectedRecordDiff.getFieldDifferenceList();

            for ( int i = 0; i < stageList.size(); i++ )
            {
                double stageValue = (Double) stageList.get( i );
                
                String ihfsStageDischargeString = getStageDischargeString( stageValue, ihfsRatingPointList );
                String raxStageDischargeString = getStageDischargeString( stageValue, raxRatingPointList );
                String stgFlow = _padder.getFrontPaddedString( ihfsStageDischargeString ) + stageValue + _padder.getEndPaddedString( "" + stageValue, raxStageDischargeString );
                recordStringList.add( stgFlow );
                
                if ( ( fieldDifferenceList != null ) && ( isStageValueDifferent( stageValue, fieldDifferenceList ) ) )
                {
                    highlightStringList.add( stgFlow );
                }
            }
            
            recordStringList.add( " " );
            List offSetsList = raxRating.getOffsets();

            if ( ( offSetsList != null ) && ( ! offSetsList.isEmpty() ) )
            {
                recordStringList.add( _padder.getFrontPaddedString( "Offset" ) + "Stage" + _padder.getEndPaddedString( "Stage", "Discharge" ) );
                recordStringList.add( _padder.getFrontPaddedString( "---------" ) + "-----" + _padder.getEndPaddedString( "-----", "---------" ) );


                for ( int i = 0; i < offSetsList.size(); i++ )
                {
                    RaxRatingOffset offset = (RaxRatingOffset) offSetsList.get( i );

                    String offsetString = _padder.getFrontPaddedString( "" ) + "Offset" + _padder.getEndPaddedString( "Offset", getOffsetString( offset ) );
                }
            }
            
            RaxRatingShift raxRatingShift = raxRatingCurveHolder.getRatingShift();
            RaxRatingShift ihfsRatingShift = ihfsRatingCurveHolder.getRatingShift();
            
            if ( raxRatingShift.getLid().equalsIgnoreCase( "LWAW4" ) )
            {
                System.out.println( "MEOW" );
            }
            if ( raxRatingShift != null )
            {
                String lidShiftString = "";
                String shiftAString = "";
                String beginDateTimeString = ""; 
                
                if ( ihfsRatingShift != null )
                {
                    lidShiftString = ihfsRatingShift.getLid();
                    shiftAString = "" + ihfsRatingShift.getShiftA();
                    beginDateTimeString = converter.getDateTimeStringFromDateTimeLong( ihfsRatingShift.getBeginDate() );
                }
                
                recordStringList.add( " " );
                recordStringList.add( _padder.getFrontPaddedString( "" ) + "-- RatingShift --" + _padder.getEndPaddedString( "-- RatingShift --", "" ) );
                recordStringList.add( _padder.getFrontPaddedString( lidShiftString ) + "LID" + _padder.getEndPaddedString( "LID", raxRatingShift.getLid() ) );
                recordStringList.add( _padder.getFrontPaddedString( beginDateTimeString ) + "BeginDate" + _padder.getEndPaddedDateTimeString( "BeginDate", raxRatingShift.getBeginDate() ) );
                recordStringList.add( _padder.getFrontPaddedString( "" ) + "PE" + _padder.getEndPaddedString( "PE", raxRatingShift.getPe() ) );
                recordStringList.add( _padder.getFrontPaddedString( "" ) + "Table" + _padder.getEndPaddedString( "Table", raxRatingShift.getRatingTableNumber() ) );
                recordStringList.add( _padder.getFrontPaddedString( "" ) + "Src" + _padder.getEndPaddedString( "Src", raxRatingShift.getSource() ) );
                recordStringList.add( _padder.getFrontPaddedString( "" ) + "ValA" + _padder.getEndPaddedString( "ValA", raxRatingShift.getValA() ) );
                recordStringList.add( _padder.getFrontPaddedString( shiftAString ) + "ShiftA" + _padder.getEndPaddedString( "ShiftA", raxRatingShift.getShiftA() ) );
                recordStringList.add( _padder.getFrontPaddedString( "" ) + "ValB" + _padder.getEndPaddedString( "ValB", raxRatingShift.getValB() ) );
                recordStringList.add( _padder.getFrontPaddedString( "" ) + "ShiftB" + _padder.getEndPaddedString( "ShiftB", raxRatingShift.getShiftB() ) );
                recordStringList.add( _padder.getFrontPaddedString( "" ) + "ValC" + _padder.getEndPaddedString( "ValC", raxRatingShift.getValC() ) );
                recordStringList.add( _padder.getFrontPaddedString( "" ) + "ShiftC" + _padder.getEndPaddedString( "ShiftC", raxRatingShift.getShiftC() ) );
                recordStringList.add( _padder.getFrontPaddedString( "" ) + "ValD" + _padder.getEndPaddedString( "ValD", raxRatingShift.getValD() ) );
                recordStringList.add( _padder.getFrontPaddedString( "" ) + "ShiftD" + _padder.getEndPaddedString( "ShiftD", raxRatingShift.getShiftD() ) );
                recordStringList.add( _padder.getFrontPaddedString( "" ) + "DatumAdj" + _padder.getEndPaddedString( "DatumAdj", raxRatingShift.getDatumAdjustment() ) );
            }
        }
        if ( _recordPanel == null )
        {
            _recordPanel = new JListPanel( "", recordStringList, false, Color.LIGHT_GRAY,
                    false, false, null );
            _recordPanel.setBorder( new TitledBorder( new LineBorder( Color.LIGHT_GRAY), headerString ) );

        }
        else
        {
            _recordPanel.setBorder( new TitledBorder( new LineBorder( Color.LIGHT_GRAY), headerString ) );
            _recordPanel.updateListData( recordStringList );
            _recordPanel.refreshJList();
        }

        ListCellRenderer renderer = new JListPanelSuperCellRenderer( _recordPanel, highlightStringList, "", "" );
        _recordPanel.getTheJList().setCellRenderer(renderer);

        _recordPanel.getTheJList().setFont( new Font( "monospaced", Font.PLAIN, 12 ) );
        _recordPanel.refreshJList();

        _recordPanel.setVisible( false );
        if ( ! recordStringList.isEmpty() )
        {
            _recordPanel.setVisible( true );
        }
    }
    
    private String getOffsetString( RaxRatingOffset offset )
    {
        return offset.getStage() + "," + offset.getOffset();
    }
    
    private boolean isStageValueDifferent( double stageValue, List fieldDifferencesList )
    {
        boolean different = false;
        
        for ( int i = 0; i < fieldDifferencesList.size(); i++ )
        {
            FieldDifference fieldDifference = (FieldDifference) fieldDifferencesList.get( i );
            if ( Double.parseDouble( fieldDifference.getName() ) == stageValue )
            {
                different = true;
                break;
            }
        }
        
        return different;
    }

    private String getStageDischargeString( double stageValue, List stageDischargeList )
    {
        String stageDischargeString = "";
        double discharge = -9999;
        
        for ( int i = 0; i < stageDischargeList.size(); i++ )
        {
            RaxRatingPoint raxRatingPoint = (RaxRatingPoint) stageDischargeList.get( i );
            if ( raxRatingPoint.getStage() == stageValue )
            {
                discharge = raxRatingPoint.getDischarge();
                break;
            }
        }
        
        if ( discharge != -9999 )
        {
            stageDischargeString = "" + discharge;
        }
        else
        {
        }
        
        return stageDischargeString;
    }
    
    private List getCombinedStageList( List ihfsRatingPointList, List raxRatingPointList )
    {
        List combinedStageList = new ArrayList();
        Set combinedStageTreeSet = new TreeSet();
        
        for ( int i = 0; i < ihfsRatingPointList.size(); i++ )
        {
            RaxRatingPoint raxRatingPoint = (RaxRatingPoint) ihfsRatingPointList.get( i );
            combinedStageTreeSet.add( raxRatingPoint.getStage() );
        }
        for ( int i = 0; i < raxRatingPointList.size(); i++ )
        {
            RaxRatingPoint raxRatingPoint = (RaxRatingPoint) raxRatingPointList.get( i );
            combinedStageTreeSet.add( raxRatingPoint.getStage() );
        }
        
        for ( Iterator iterator = combinedStageTreeSet.iterator(); iterator.hasNext(); ) 
        {
            combinedStageList.add( (Double) iterator.next() );
        }
        
        return combinedStageList;
    }
    
    public static void main( String[] args )
    {
        JFrame frame = new JFrame();

        RaxSyncDataMgr syncDataMgr = new RaxSyncDataMgr( "jdbc:postgresql://lx5:5432/hd_ob82krfx?user=pguser", "jdbc:postgresql://ax2:5432/adb_ob82krf?user=pguser" );

        ProcessRatingDifferencesDialog test = new ProcessRatingDifferencesDialog( frame, syncDataMgr );
        test.displayGUI();
    }

}
