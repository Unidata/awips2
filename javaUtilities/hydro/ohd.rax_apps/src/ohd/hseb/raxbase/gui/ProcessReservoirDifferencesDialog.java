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
import ohd.hseb.ihfsdb.generated.ReservoirRecord;
import ohd.hseb.raxbase.util.JListPanelSuperCellRenderer;
import ohd.hseb.raxbase.util.StringPadder;
import ohd.hseb.raxdb.generated.RaxReservoirRecord;
import ohd.hseb.raxdb_sync.FieldDifference;
import ohd.hseb.raxdb_sync.RaxSyncDataMgr;
import ohd.hseb.raxdb_sync.RecordDifference;
import ohd.hseb.rfc.util.listpanels.JListPanel;

public class ProcessReservoirDifferencesDialog extends ProcessDifferencesDialog
{
    public ProcessReservoirDifferencesDialog( JFrame frame, RaxSyncDataMgr syncDataMgr )
    {
        super( frame, "Process Reservoir", "Reservoir", syncDataMgr, new StringPadder( 25, 35 ), new Dimension( 650, 550 ) );
        _keyString = "(LID | BeginDate)";
    }

    protected String getSelectionComboBoxString( RecordDifference recordDiff )
    {
        String cbString = null;
        
        RaxReservoirRecord record = (RaxReservoirRecord) recordDiff.getRaxRecord();
        StringPadder lidPadder = new StringPadder( 8, 8 );

        cbString = lidPadder.getFrontPaddedString( record.getLid() ) + " | " + 
                   DbTimeHelper.getDateStringFromLongTime( record.getSbd() );
        
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

            ReservoirRecord ihfsRecord = (ReservoirRecord) _selectedRecordDiff.getIhfsRecord();
            RaxReservoirRecord raxRecord = (RaxReservoirRecord) _selectedRecordDiff.getRaxRecord();

            String lid = _padder.getFrontPaddedString( ihfsRecord.getLid() ) + "LID" + _padder.getEndPaddedString( "LID", raxRecord.getLid() );
            String sbd = _padder.getFrontPaddedString( "" ) + "Sbd" + _padder.getEndPaddedDateString( "Sbd", raxRecord.getSbd() );
            String sed = _padder.getFrontPaddedString( "" ) + "Sed" + _padder.getEndPaddedDateString( "Sed", raxRecord.getSed() );
            String name = _padder.getFrontPaddedString( ihfsRecord.getName()  ) + "Name" + _padder.getEndPaddedString( "Name", raxRecord.getName() );
            String type = _padder.getFrontPaddedString( ihfsRecord.getType() ) + "Type" + _padder.getEndPaddedString( "Type", raxRecord.getType() );
            String owner = _padder.getFrontPaddedString( ihfsRecord.getOwner() ) + "Owner" + _padder.getEndPaddedString( "Owner", raxRecord.getOwner() );
            String deadPool = _padder.getFrontPaddedString( ihfsRecord.getDeadpool() ) + "Dead Pool" + _padder.getEndPaddedString( "Dead Pool", raxRecord.getDeadpool() );
            String conserPool = _padder.getFrontPaddedString( ihfsRecord.getConserpool() ) + "Conser Pool" + _padder.getEndPaddedString( "Conser Pool", raxRecord.getConserpool() );
            String floodPool = _padder.getFrontPaddedString( ihfsRecord.getFloodpool() ) + "Flood Pool" + _padder.getEndPaddedString( "Flood Pool", raxRecord.getFloodpool() );
            String spillWay = _padder.getFrontPaddedString( ihfsRecord.getSpillway() ) + "Spillway" + _padder.getEndPaddedString( "Spillway", raxRecord.getSpillway() );
            String sill = _padder.getFrontPaddedString( ihfsRecord.getSill() ) + "Sill" + _padder.getEndPaddedString( "Sill", raxRecord.getSill() );
            String top = _padder.getFrontPaddedString( ihfsRecord.getTop() ) + "Top" + _padder.getEndPaddedString( "Top", raxRecord.getTop() );
            String surchg = _padder.getFrontPaddedString( ihfsRecord.getSurchg() ) + "Surchg" + _padder.getEndPaddedString( "Surchg", raxRecord.getSurchg() );
            String elev = _padder.getFrontPaddedString( ihfsRecord.getElev() ) + "Elev" + _padder.getEndPaddedString( "Elev", raxRecord.getElev() );
            String gates = _padder.getFrontPaddedString( ihfsRecord.getGates() ) + "Gates" + _padder.getEndPaddedString( "Gates", raxRecord.getGates() );
            String impounded = _padder.getFrontPaddedDateString( ihfsRecord.getImpounded() ) + "Impounded" + _padder.getEndPaddedDateString( "Impounded", raxRecord.getImpounded() );
            String uses = _padder.getFrontPaddedString( ihfsRecord.getUses() ) + "Uses" + _padder.getEndPaddedString( "Uses", raxRecord.getUses() );
            
            recordStringList.add( lid );
            recordStringList.add( sbd );
            recordStringList.add( sed );
            recordStringList.add( name );
            recordStringList.add( type );
            recordStringList.add( owner );
            recordStringList.add( deadPool );
            recordStringList.add( conserPool );
            recordStringList.add( floodPool );
            recordStringList.add( spillWay );
            recordStringList.add( sill );
            recordStringList.add( top );
            recordStringList.add( surchg );
            recordStringList.add( elev );
            recordStringList.add( gates );
            recordStringList.add( impounded );
            recordStringList.add( uses );
            
            List fieldDifferenceList = _selectedRecordDiff.getFieldDifferenceList();

            if ( fieldDifferenceList != null )
            {
                for ( int i = 0; i < fieldDifferenceList.size(); i++ )
                {
                    FieldDifference fieldDifference = (FieldDifference) fieldDifferenceList.get( i );
                    String columnName = fieldDifference.getName();
                    
                    if ( columnName.equalsIgnoreCase( "name" ) )
                    {
                        highlightStringList.add( name );
                    }
                    else if ( columnName.equalsIgnoreCase( "type" ) )
                    {
                        highlightStringList.add( type );
                    }
                    else if ( columnName.equalsIgnoreCase( "owner" ) )
                    {
                        highlightStringList.add( owner );
                    }
                    else if ( columnName.equalsIgnoreCase( "deadpool" ) )
                    {
                        highlightStringList.add( deadPool );
                    }
                    else if ( columnName.equalsIgnoreCase( "conserpool" ) )
                    {
                        highlightStringList.add( conserPool );
                    }
                    else if ( columnName.equalsIgnoreCase( "floodpool" ) )
                    {
                        highlightStringList.add( floodPool );
                    }
                    else if ( columnName.equalsIgnoreCase( "spillway" ) )
                    {
                        highlightStringList.add( spillWay );
                    }
                    else if ( columnName.equalsIgnoreCase( "sill" ) )
                    {
                        highlightStringList.add( sill );
                    }
                    else if ( columnName.equalsIgnoreCase( "top" ) )
                    {
                        highlightStringList.add( top );
                    }
                    else if ( columnName.equalsIgnoreCase( "surchg" ) )
                    {
                        highlightStringList.add( surchg );
                    }
                    else if ( columnName.equalsIgnoreCase( "elev" ) )
                    {
                        highlightStringList.add( elev );
                    }
                    else if ( columnName.equalsIgnoreCase( "gates" ) )
                    {
                        highlightStringList.add( gates );
                    }
                    else if ( columnName.equalsIgnoreCase( "impounded" ) )
                    {
                        highlightStringList.add( impounded );
                    }
                    else if ( columnName.equalsIgnoreCase( "uses" ) )
                    {
                        highlightStringList.add( uses );
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

        ProcessReservoirDifferencesDialog test = new ProcessReservoirDifferencesDialog( frame, syncDataMgr );
        test.displayGUI();
    }

}
