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

import ohd.hseb.ihfsdb.generated.IngestFilterRecord;
import ohd.hseb.raxbase.util.JListPanelSuperCellRenderer;
import ohd.hseb.raxbase.util.PEManager;
import ohd.hseb.raxbase.util.StringPadder;
import ohd.hseb.raxbase.util.TSManager;
import ohd.hseb.raxdb.generated.RaxIngestFilterRecord;
import ohd.hseb.raxdb_sync.FieldDifference;
import ohd.hseb.raxdb_sync.RaxSyncDataMgr;
import ohd.hseb.raxdb_sync.RecordDifference;
import ohd.hseb.rfc.util.listpanels.JListPanel;

public class ProcessIngestFilterDifferencesDialog extends ProcessDifferencesDialog
{
    public ProcessIngestFilterDifferencesDialog( JFrame frame, RaxSyncDataMgr syncDataMgr )
    {
        super( frame, "Process IngestFilter", "IngestFilter", syncDataMgr, new StringPadder( 25, 35 ), new Dimension( 650, 550 ) );
        _keyString = "(Lid | PE | Dur | IDur | TS | Extremum)";
    }

    protected String getSelectionComboBoxString( RecordDifference recordDiff )
    {
        String cbString = null;
        
        RaxIngestFilterRecord record = (RaxIngestFilterRecord) recordDiff.getRaxRecord();
        StringPadder lidPadder = new StringPadder( 8, 8 );
        StringPadder idurPadder = new StringPadder( 4, 8 );

        cbString = lidPadder.getFrontPaddedString( record.getLid() ) + " | " + 
                   PEManager.getPEFromPe1Pe2( record.getPe1(), record.getPe2() ) + " | " + 
                   record.getDur() + " | " + 
                   idurPadder.getFrontPaddedString( record.getIdur() ) + " | " + 
                   TSManager.getTSFromTandS( record.getT(), record.getS() ) + " | " + 
                   record.getE();
        
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

            IngestFilterRecord ihfsRecord = (IngestFilterRecord) _selectedRecordDiff.getIhfsRecord();
            RaxIngestFilterRecord raxRecord = (RaxIngestFilterRecord) _selectedRecordDiff.getRaxRecord();

            String lid = _padder.getFrontPaddedString( ihfsRecord.getLid() ) + "LID" + _padder.getEndPaddedString( "LID", raxRecord.getLid() );
            String pe = _padder.getFrontPaddedString( ihfsRecord.getPe() ) + "PE" + _padder.getEndPaddedString( "PE", raxRecord.getPe1() + raxRecord.getPe2() );
            String dur = _padder.getFrontPaddedString( ihfsRecord.getDur() ) + "Dur" + _padder.getEndPaddedString( "Dur", raxRecord.getIdur() + "/" + raxRecord.getDur() );
            String iDur = _padder.getFrontPaddedString( "" ) + "IDur" + _padder.getEndPaddedString( "IDur", raxRecord.getIdur() );
            String ts = _padder.getFrontPaddedString( ihfsRecord.getTs() ) + "TS" + _padder.getEndPaddedString( "TS", raxRecord.getT() + raxRecord.getS() );
            String extremum = _padder.getFrontPaddedString( ihfsRecord.getExtremum() ) + "Extremum" + _padder.getEndPaddedString( "Extremum", raxRecord.getE() );
            String tsRank = _padder.getFrontPaddedString( ihfsRecord.getTs_rank() ) + "TS Rank" + _padder.getEndPaddedString( "TS Rank", raxRecord.getTs_rank() );
            String det = _padder.getFrontPaddedString( "" ) + "Det" + _padder.getEndPaddedString( "Det", raxRecord.getDet() );
            String ingest = _padder.getFrontPaddedString( ihfsRecord.getIngest() ) + "Ingest" + _padder.getEndPaddedString( "Ingest", raxRecord.getIngest() );
            String newReport = _padder.getFrontPaddedString( "" ) + "New Report" + _padder.getEndPaddedString( "New Report", raxRecord.getNew_report() );
            String active = _padder.getFrontPaddedString( "" ) + "Active" + _padder.getEndPaddedString( "Active", raxRecord.getActive() );
            String ofsInput = _padder.getFrontPaddedString( ihfsRecord.getOfs_input() ) + "OFS Input" + _padder.getEndPaddedString( "OFS Input", raxRecord.getOfs_input() );
            String obsTime = _padder.getFrontPaddedString( "" ) + "ObsTime" + _padder.getEndPaddedString( "ObsTime", raxRecord.getObstime() );
            String ownag = _padder.getFrontPaddedString( "" ) + "Ownag" + _padder.getEndPaddedString( "Ownag", raxRecord.getOwnag() );
            String ownloc = _padder.getFrontPaddedString( "" ) + "Ownloc" + _padder.getEndPaddedString( "Ownloc", raxRecord.getOwnloc() );
            String mpeInput = _padder.getFrontPaddedString( ihfsRecord.getStg2_input() ) + "MPE Input" + _padder.getEndPaddedString( "MPE Input", raxRecord.getMpe_input() );
            
            recordStringList.add( lid );
            recordStringList.add( pe );
            recordStringList.add( dur );
            recordStringList.add( iDur );
            recordStringList.add( ts );
            recordStringList.add( extremum );
            recordStringList.add( tsRank );
            recordStringList.add( det );
            recordStringList.add( ingest );
            recordStringList.add( newReport );
            recordStringList.add( active );
            recordStringList.add( ofsInput );
            recordStringList.add( obsTime );
            recordStringList.add( ownag );
            recordStringList.add( ownloc );
            recordStringList.add( mpeInput );
            
            List fieldDifferenceList = _selectedRecordDiff.getFieldDifferenceList();

            if ( fieldDifferenceList != null )
            {
                for ( int i = 0; i < fieldDifferenceList.size(); i++ )
                {
                    FieldDifference fieldDifference = (FieldDifference) fieldDifferenceList.get( i );
                    String columnName = fieldDifference.getName();

                    if ( columnName.equalsIgnoreCase( "ts_rank" ) )
                    {
                        highlightStringList.add( tsRank );
                    }
                    else if ( columnName.equalsIgnoreCase( "ingest" ) )
                    {
                        highlightStringList.add( ingest );
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

        ProcessIngestFilterDifferencesDialog test = new ProcessIngestFilterDifferencesDialog( frame, syncDataMgr );
        test.displayGUI();
    }

}
