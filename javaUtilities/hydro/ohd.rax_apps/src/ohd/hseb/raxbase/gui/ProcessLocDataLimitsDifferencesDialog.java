package ohd.hseb.raxbase.gui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.ListCellRenderer;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;

import ohd.hseb.ihfsdb.generated.LocDataLimitsRecord;
import ohd.hseb.raxbase.util.JListPanelSuperCellRenderer;
import ohd.hseb.raxbase.util.PEManager;
import ohd.hseb.raxbase.util.StringPadder;
import ohd.hseb.raxdb.generated.RaxLocDataLimitsRecord;
import ohd.hseb.raxdb_sync.FieldDifference;
import ohd.hseb.raxdb_sync.RaxSyncDataMgr;
import ohd.hseb.raxdb_sync.RecordDifference;
import ohd.hseb.rfc.util.listpanels.JListPanel;

public class ProcessLocDataLimitsDifferencesDialog extends ProcessDifferencesDialog
{
    public ProcessLocDataLimitsDifferencesDialog( JFrame frame, RaxSyncDataMgr syncDataMgr )
    {
        super( frame, "Process LocDataLimits", "LocDataLimits", syncDataMgr, new StringPadder( 25, 35 ), new Dimension( 650, 550 ) );
        _keyString = "(LID | PE | Dur | IDur | MonthDayStart)";
    }

    protected String getSelectionComboBoxString( RecordDifference recordDiff )
    {
        String cbString = null;
        
        RaxLocDataLimitsRecord record = (RaxLocDataLimitsRecord) recordDiff.getRaxRecord();
        StringPadder lidPadder = new StringPadder( 8, 8 );
        StringPadder idurPadder = new StringPadder( 4, 8 );

        cbString = lidPadder.getFrontPaddedString( record.getLid() ) + " | " + 
                   PEManager.getPEFromPe1Pe2( record.getPe1(), record.getPe2() ) + " | " + 
                   record.getDur() + " | " + 
                   idurPadder.getFrontPaddedString( record.getIdur() ) + " | " + 
                   record.getMonthdaystart();
        
        return cbString;
    }

    protected void refreshRecordPanel()
    {
        List recordStringList = new ArrayList();
        String headerString = null;
        List highlightStringList = new ArrayList();

        if ( _selectedRecordDiff != null )
        {
            headerString = getHeaderString();

            LocDataLimitsRecord ihfsRecord = (LocDataLimitsRecord) _selectedRecordDiff.getIhfsRecord();
            RaxLocDataLimitsRecord raxRecord = (RaxLocDataLimitsRecord) _selectedRecordDiff.getRaxRecord();

            String lid = _padder.getFrontPaddedString( ihfsRecord.getLid() ) + "LID" + _padder.getEndPaddedString( "LID", raxRecord.getLid() );
            String pe = _padder.getFrontPaddedString( ihfsRecord.getPe() ) + "PE" + _padder.getEndPaddedString( "PE", raxRecord.getPe1() + raxRecord.getPe2() );
            String dur = _padder.getFrontPaddedString( ihfsRecord.getDur() ) + "Dur" + _padder.getEndPaddedString( "Dur", raxRecord.getIdur() + "/" + raxRecord.getDur() );
            String iDur = _padder.getFrontPaddedString( "" ) + "IDur" + _padder.getEndPaddedString( "IDur", raxRecord.getIdur() );
            String monthDayStart = _padder.getFrontPaddedString( ihfsRecord.getMonthdaystart() ) + "MonthDayStart" + _padder.getEndPaddedString( "MonthDayStart", raxRecord.getMonthdaystart() );
            String monthDayEnd = _padder.getFrontPaddedString( ihfsRecord.getMonthdayend() ) + "MonthDayEnd" + _padder.getEndPaddedString( "MonthDayEnd", raxRecord.getMonthdayend() );
            String grossRangeMin = _padder.getFrontPaddedString( ihfsRecord.getGross_range_min() ) + "GrossRangeMin" + _padder.getEndPaddedString( "GrossRangeMin", raxRecord.getGross_range_min() );
            String grossRangeMax = _padder.getFrontPaddedString( ihfsRecord.getGross_range_max() ) + "GrossRangeMax" + _padder.getEndPaddedString( "GrossRangeMax", raxRecord.getGross_range_max() );
            String reasonRangeMin = _padder.getFrontPaddedString( ihfsRecord.getReason_range_min() ) + "ReasonRangeMin" + _padder.getEndPaddedString( "ReasonRangeMin", raxRecord.getReason_range_min() );
            String reasonRangeMax = _padder.getFrontPaddedString( ihfsRecord.getReason_range_max() ) + "ReasonRangeMax" + _padder.getEndPaddedString( "ReasonRangeMax", raxRecord.getReason_range_max() );
            String rocMax = _padder.getFrontPaddedString( ihfsRecord.getRoc_max() ) + "RocMax" + _padder.getEndPaddedString( "RocMax", raxRecord.getRoc_max() );
            String alertLimit = _padder.getFrontPaddedString( ihfsRecord.getAlert_upper_limit() ) + "AlertLimit" + _padder.getEndPaddedString( "AlertLimit", raxRecord.getAlert_limit() );
            String alertRocLimit = _padder.getFrontPaddedString( ihfsRecord.getAlert_roc_limit() ) + "AlertRocLimit" + _padder.getEndPaddedString( "AlertRocLimit", raxRecord.getAlert_roc_limit() );
            String alarmLimit = _padder.getFrontPaddedString( ihfsRecord.getAlarm_upper_limit() ) + "AlarmLimit" + _padder.getEndPaddedString( "AlarmLimit", raxRecord.getAlarm_limit() );
            String alarmRocLimit = _padder.getFrontPaddedString( ihfsRecord.getAlarm_roc_limit() ) + "AlarmRocLimit" + _padder.getEndPaddedString( "AlarmRocLimit", raxRecord.getAlarm_roc_limit() );

            recordStringList.add( lid );
            recordStringList.add( pe );
            recordStringList.add( dur );
            recordStringList.add( iDur );
            recordStringList.add( monthDayStart );
            recordStringList.add( monthDayEnd );
            recordStringList.add( grossRangeMin );
            recordStringList.add( grossRangeMax );
            recordStringList.add( reasonRangeMin );
            recordStringList.add( reasonRangeMax );
            recordStringList.add( rocMax );
            recordStringList.add( alertLimit );
            recordStringList.add( alertRocLimit );
            recordStringList.add( alarmLimit );
            recordStringList.add( alarmRocLimit );
            
            List fieldDifferenceList = _selectedRecordDiff.getFieldDifferenceList();

            if ( fieldDifferenceList != null )
            {
                for ( int i = 0; i < fieldDifferenceList.size(); i++ )
                {
                    FieldDifference fieldDifference = (FieldDifference) fieldDifferenceList.get( i );
                    String columnName = fieldDifference.getName();

                    if ( columnName.equalsIgnoreCase( "monthdayend" ) )
                    {
                        highlightStringList.add( monthDayEnd );
                    }
                    else if( columnName.equalsIgnoreCase( "gross_range_min" ) )
                    {
                        highlightStringList.add( grossRangeMin );
                    }
                    else if( columnName.equalsIgnoreCase( "gross_range_max" ) )
                    {
                        highlightStringList.add( grossRangeMax );
                    }
                    else if( columnName.equalsIgnoreCase( "reason_range_min" ) )
                    {
                        highlightStringList.add( reasonRangeMin );
                    }
                    else if( columnName.equalsIgnoreCase( "reason_range_max" ) )
                    {
                        highlightStringList.add( reasonRangeMax );
                    }
                    else if( columnName.equalsIgnoreCase( "roc_max" ) )
                    {
                        highlightStringList.add( rocMax );
                    }
                    else if( columnName.equalsIgnoreCase( "alert_limit" ) )
                    {
                        highlightStringList.add( alertLimit );
                    }
                    else if( columnName.equalsIgnoreCase( "alert_roc_limit" ) )
                    {
                        highlightStringList.add( alertRocLimit );
                    }
                    else if( columnName.equalsIgnoreCase( "alarm_limit" ) )
                    {
                        highlightStringList.add( alarmLimit );
                    }
                    else if( columnName.equalsIgnoreCase( "alarm_roc_limit" ) )
                    {
                        highlightStringList.add( alarmRocLimit );
                    }
                }
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
    
    public static void main( String[] args )
    {
        JFrame frame = new JFrame();

        RaxSyncDataMgr syncDataMgr = new RaxSyncDataMgr( "jdbc:postgresql://lx5:5432/hd_ob83raxtest?user=pguser", "jdbc:postgresql://lx5:5432/adb_ob83raxtest?user=pguser" );

        ProcessLocDataLimitsDifferencesDialog test = new ProcessLocDataLimitsDifferencesDialog( frame, syncDataMgr );
        test.displayGUI();
    }

}
