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

import ohd.hseb.ihfsdb.generated.LocationRecord;
import ohd.hseb.raxbase.util.JListPanelSuperCellRenderer;
import ohd.hseb.raxbase.util.StringPadder;
import ohd.hseb.raxdb.generated.RaxLocationRecord;
import ohd.hseb.raxdb_sync.FieldDifference;
import ohd.hseb.raxdb_sync.RaxSyncDataMgr;
import ohd.hseb.raxdb_sync.RecordDifference;
import ohd.hseb.raxdb_sync.RecordDifferenceOriginType;
import ohd.hseb.rfc.util.listpanels.JListPanel;

public class ProcessLocationDifferencesDialog extends ProcessDifferencesDialog
{
    public ProcessLocationDifferencesDialog( JFrame frame, RaxSyncDataMgr syncDataMgr )
    {
        super( frame, "Process Location", "Location", syncDataMgr, new StringPadder( 45, 45 ), new Dimension( 850, 550 ) );
        _keyString = "(LID | BeginDate)";
    }

    protected void refreshRecordPanel()
    {
        List recordStringList = new ArrayList();
        String headerString = null;
        List highlightStringList = new ArrayList();

        if ( _selectedRecordDiff != null )
        {
            headerString = getLidHeaderString();

            LocationRecord ihfsRecord = (LocationRecord) _selectedRecordDiff.getIhfsRecord();
            RaxLocationRecord raxRecord = (RaxLocationRecord) _selectedRecordDiff.getRaxRecord();
            
            String lid = _padder.getFrontPaddedString( ihfsRecord.getLid() ) + "LID" + _padder.getEndPaddedString( "LID", raxRecord.getLid() );
            String sbd = _padder.getFrontPaddedString( "" ) + "Sbd" + _padder.getEndPaddedDateString( "Sbd", raxRecord.getSbd() );
            String sed = _padder.getFrontPaddedString( "" ) + "Sed" + _padder.getEndPaddedDateString( "Sed", raxRecord.getSed() );
            String goes = _padder.getFrontPaddedString( "" ) + "Goes" + _padder.getEndPaddedString( "Goes", raxRecord.getGoes() );
            String name = _padder.getFrontPaddedString( ihfsRecord.getName() ) + "Name" + _padder.getEndPaddedString( "Name", raxRecord.getName() );
            String det = _padder.getFrontPaddedString( ihfsRecord.getDet() ) + "Det" + _padder.getEndPaddedString( "Det", raxRecord.getDet() );
            String lat = _padder.getFrontPaddedString( ihfsRecord.getLat() ) + "Lat" + _padder.getEndPaddedString( "Lat", raxRecord.getLat() );
            String lon = _padder.getFrontPaddedString( ihfsRecord.getLon() ) + "Lon" + _padder.getEndPaddedString( "Lon", raxRecord.getLon() );
            String elev = _padder.getFrontPaddedString( ihfsRecord.getElev() ) + "Elev" + _padder.getEndPaddedString( "Elev", raxRecord.getElev() );
            String state = _padder.getFrontPaddedString( ihfsRecord.getState() ) + "State" + _padder.getEndPaddedString( "State", raxRecord.getState() );
            String huc = _padder.getFrontPaddedString( "" ) + "HUC" + _padder.getEndPaddedString( "HUC", raxRecord.getHuc() );
            String countyFips = _padder.getFrontPaddedString( ihfsRecord.getCounty() ) + "CountyFips" + _padder.getEndPaddedString( "CountyFips", raxRecord.getCountyfips() );
            String zon = _padder.getFrontPaddedString( "" ) + "Zon" + _padder.getEndPaddedString( "Zon", raxRecord.getZon() );
            String hsa = _padder.getFrontPaddedString( ihfsRecord.getHsa() ) + "HSA" + _padder.getEndPaddedString( "HSA", raxRecord.getHsa() );
            String wfo = _padder.getFrontPaddedString( ihfsRecord.getWfo() ) + "WFO" + _padder.getEndPaddedString( "WFO", raxRecord.getWfo() );
            String post = _padder.getFrontPaddedString( ihfsRecord.getPost() ) + "Post" + _padder.getEndPaddedString( "Post", raxRecord.getPost() );
            String dbSource = _padder.getFrontPaddedString( "" ) + "DbSource" + _padder.getEndPaddedString( "DbSource", raxRecord.getDbsource() );
            String rfc = _padder.getFrontPaddedString( ihfsRecord.getRfc() ) + "RFC" + _padder.getEndPaddedString( "RFC", raxRecord.getRfc() );
            String countryFips = _padder.getFrontPaddedString( "" ) + "CountryFips" + _padder.getEndPaddedString( "CountryFips", raxRecord.getCountryfips() );
            
            recordStringList.add( lid );
            recordStringList.add( sbd );
            recordStringList.add( sed );
            recordStringList.add( goes );
            recordStringList.add( name );
            recordStringList.add( det );
            recordStringList.add( lat );
            recordStringList.add( lon );
            recordStringList.add( elev );
            recordStringList.add( state );
            recordStringList.add( huc );
            recordStringList.add( countyFips );
            recordStringList.add( zon );
            recordStringList.add( hsa );
            recordStringList.add( wfo );
            recordStringList.add( post );
            recordStringList.add( dbSource );
            recordStringList.add( rfc );
            recordStringList.add( countryFips );
            
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
                    else if ( columnName.equalsIgnoreCase( "det" ) )
                    {
                        highlightStringList.add( det );
                    }
                    else if ( columnName.equalsIgnoreCase( "lat" ) )
                    {
                        highlightStringList.add( lat );
                    }
                    else if ( columnName.equalsIgnoreCase( "lon" ) )
                    {
                        highlightStringList.add( lon );
                    }
                    else if ( columnName.equalsIgnoreCase( "elev" ) )
                    {
                        highlightStringList.add( elev );
                    }
                    else if ( columnName.equalsIgnoreCase( "state" ) )
                    {
                        highlightStringList.add( state );
                    }
                    else if ( columnName.equalsIgnoreCase( "hsa" ) )
                    {
                        highlightStringList.add( hsa );
                    }
                    else if ( columnName.equalsIgnoreCase( "wfo" ) )
                    {
                        highlightStringList.add( wfo );
                    }
                    else if ( columnName.equalsIgnoreCase( "rfc" ) )
                    {
                        highlightStringList.add( rfc );
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
        StringPadder padder = new StringPadder( 8, 8 );
        
        RaxLocationRecord record = (RaxLocationRecord) recordDiff.getRaxRecord();
        
        cbString = padder.getFrontPaddedString( record.getLid() ) + " | " + padder.getFrontPaddedDateString( record.getSbd() );
        
        return cbString;
    }
    
    protected String getLidHeaderString()
    {
        String headerString = null;
        
        if ( _selectedRecordDiff.getDiffType() == RecordDifferenceOriginType.MOD )
        { 
            headerString = "IHFS Record                                                                                                                     Existing Rax Record";
        }
        else
        {
            headerString = "IHFS Record                                                                                                                         New Rax Record";
        }

        return headerString;
    }

    public static void main( String[] args )
    {
        JFrame frame = new JFrame();

        RaxSyncDataMgr syncDataMgr = new RaxSyncDataMgr( "jdbc:postgresql://lx5:5432/hd_ob83raxtest?user=pguser", "jdbc:postgresql://lx5:5432/adb_ob83raxtest?user=pguser" );

        ProcessLocationDifferencesDialog test = new ProcessLocationDifferencesDialog( frame, syncDataMgr );
        test.displayGUI();
    }
}
