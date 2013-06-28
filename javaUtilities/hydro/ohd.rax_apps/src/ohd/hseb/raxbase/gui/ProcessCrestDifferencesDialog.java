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

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.ihfsdb.generated.CrestRecord;
import ohd.hseb.raxbase.util.JListPanelSuperCellRenderer;
import ohd.hseb.raxbase.util.StringPadder;
import ohd.hseb.raxdb.generated.RaxCrestRecord;
import ohd.hseb.raxdb_sync.FieldDifference;
import ohd.hseb.raxdb_sync.RaxSyncDataMgr;
import ohd.hseb.raxdb_sync.RecordDifference;
import ohd.hseb.rfc.util.listpanels.JListPanel;

public class ProcessCrestDifferencesDialog extends ProcessDifferencesDialog
{

    public ProcessCrestDifferencesDialog( JFrame frame, RaxSyncDataMgr syncDataMgr )
    {
        super( frame, "Process Crest", "Crest", syncDataMgr, new StringPadder( 25, 35 ), new Dimension( 650, 550 ) );
        _keyString = "(Lid | DateCrest)";
    }
    
    protected void refreshRecordPanel()
    {
        List recordStringList = new ArrayList();
        String headerString = null;
        List highlightStringList = new ArrayList();

        if ( _selectedRecordDiff != null )
        {
            headerString = getHeaderString();

            CrestRecord ihfsRecord = (CrestRecord) _selectedRecordDiff.getIhfsRecord();
            RaxCrestRecord raxRecord = (RaxCrestRecord) _selectedRecordDiff.getRaxRecord();

            String lid = _padder.getFrontPaddedString( ihfsRecord.getLid() ) + "LID" + _padder.getEndPaddedString( "LID", raxRecord.getLid() );
            String datecrst = _padder.getFrontPaddedString( DbTimeHelper.getDateStringFromLongTime( ihfsRecord.getDatcrst() ) ) + "DateCrest" + _padder.getEndPaddedString( "DateCrest", DbTimeHelper.getDateStringFromLongTime( raxRecord.getDatecrst() ) );
            String timecrst = _padder.getFrontPaddedString( ihfsRecord.getTimcrst() ) + "TimeCrest" + _padder.getEndPaddedString( "TimeCrest", raxRecord.getCrstdatetime() );
            String stage = _padder.getFrontPaddedString( ihfsRecord.getStage() ) + "Stage" + _padder.getEndPaddedString( "Stage",raxRecord.getStage() );
            String stg_qual = _padder.getFrontPaddedString( "" ) + "StageQual" + _padder.getEndPaddedString( "StageQual", raxRecord.getStg_qual() );
            String flow = _padder.getFrontPaddedString( ihfsRecord.getQ() ) + "Flow" + _padder.getEndPaddedString( "Flow", raxRecord.getFlow() );
            String flow_qual = _padder.getFrontPaddedString( "" ) + "FlowQual" + _padder.getEndPaddedString( "FlowQual", raxRecord.getFlow_qual() );
            String hw = _padder.getFrontPaddedString( ihfsRecord.getHw() ) + "Hw" + _padder.getEndPaddedString( "Hw", raxRecord.getHw() );
            String jam = _padder.getFrontPaddedString( ihfsRecord.getJam() ) + "Jam" + _padder.getEndPaddedString( "Jam", raxRecord.getJam() );
            String oldDatum = _padder.getFrontPaddedString( ihfsRecord.getOlddatum() ) + "Old Datum" + _padder.getEndPaddedString( "Old Datum", raxRecord.getOlddatum() );
            String prelim = _padder.getFrontPaddedString( ihfsRecord.getPrelim() ) + "Prelim" + _padder.getEndPaddedString( "Prelim", raxRecord.getPrelim() );
            
            recordStringList.add( lid );
            recordStringList.add( datecrst );
            recordStringList.add( timecrst );
            recordStringList.add( stage );
            recordStringList.add( stg_qual );
            recordStringList.add( flow );
            recordStringList.add( flow_qual );
            recordStringList.add( hw );
            recordStringList.add( jam );
            recordStringList.add( oldDatum );
            recordStringList.add( prelim );
            
            List fieldDifferenceList = _selectedRecordDiff.getFieldDifferenceList();

            if ( fieldDifferenceList != null )
            {
                for ( int i = 0; i < fieldDifferenceList.size(); i++ )
                {
                    FieldDifference fieldDifference = (FieldDifference) fieldDifferenceList.get( i );
                    String columnName = fieldDifference.getName();
                    
                    if ( columnName.equalsIgnoreCase( "crstdatetime" ) )
                    {
                        highlightStringList.add( timecrst );
                    }
                    else if ( columnName.equalsIgnoreCase( "stage" ) )
                    {
                        highlightStringList.add( stage );
                    }
                    else if ( columnName.equalsIgnoreCase( "flow" ) )
                    {
                        highlightStringList.add( flow );
                    }
                    else if ( columnName.equalsIgnoreCase( "hw" ) )
                    {
                        highlightStringList.add( hw );
                    }
                    else if ( columnName.equalsIgnoreCase( "jam" ) )
                    {
                        highlightStringList.add( jam );
                    }
                    else if ( columnName.equalsIgnoreCase( "olddatum" ) )
                    {
                        highlightStringList.add( oldDatum );
                    }
                    else if ( columnName.equalsIgnoreCase( "prelim" ) )
                    {
                        highlightStringList.add( prelim );
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

    protected String getSelectionComboBoxString( RecordDifference recordDiff )
    {
        String cbString = null;
        
        RaxCrestRecord record = (RaxCrestRecord) recordDiff.getRaxRecord();
        StringPadder padder = new StringPadder( 8, 8 );

        cbString = padder.getFrontPaddedString( record.getLid() ) + " | " + DbTimeHelper.getDateStringFromLongTime( record.getDatecrst() );
        
        return cbString;
    }
    
    public static void main( String[] args )
    {
        JFrame frame = new JFrame();

        RaxSyncDataMgr syncDataMgr = new RaxSyncDataMgr( "jdbc:postgresql://lx5:5432/hd_ob83raxtest?user=pguser", "jdbc:postgresql://lx5:5432/adb_ob83raxtest?user=pguser" );

        ProcessCrestDifferencesDialog test = new ProcessCrestDifferencesDialog( frame, syncDataMgr );
        test.displayGUI();
    }
}
