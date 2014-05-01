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

import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxbase.model.RaxRiverCrit;
import ohd.hseb.raxbase.util.JListPanelSuperCellRenderer;
import ohd.hseb.raxbase.util.PEManager;
import ohd.hseb.raxbase.util.StringPadder;
import ohd.hseb.raxdb.generated.RaxRiverCritRecord;
import ohd.hseb.raxdb_sync.FieldDifference;
import ohd.hseb.raxdb_sync.RaxSyncDataMgr;
import ohd.hseb.raxdb_sync.RecordDifference;
import ohd.hseb.raxdb_sync.RiverCritHolder;
import ohd.hseb.rfc.util.listpanels.JListPanel;

public class ProcessRiverCritDifferencesDialog extends ProcessDifferencesDialog
{
    public ProcessRiverCritDifferencesDialog( JFrame frame, RaxSyncDataMgr syncDataMgr )
    {
        super( frame, "Process RiverCrit", "RiverCrit", syncDataMgr, new StringPadder( 25, 35 ), new Dimension( 1024, 800 ) );
        _keyString = "(LID | PE | VDTime)";
    }

    protected String getSelectionComboBoxString( RecordDifference recordDiff )
    {
        String cbString = null;
        
        RaxRiverCritRecord record = (RaxRiverCritRecord) recordDiff.getRaxRecord();
        StringPadder lidPadder = new StringPadder( 8, 4 );
        
        cbString = lidPadder.getFrontPaddedString( record.getLid() ) + " | " + 
                   PEManager.getPEFromPe1Pe2( record.getPe1(), record.getPe2() ) + " | " + 
                   DbTimeHelper.getDateStringFromLongTime( record.getVdtime() );
        
        return cbString;
    }
    

    protected void refreshRecordPanel()
    {
        List recordStringList = new ArrayList();
        String headerString = null;
        List highlightStringList = new ArrayList();
        double nullDouble = DbTable.getNullDouble();
        long nullLong = DbTable.getNullLong();
        
        if ( _selectedRecordDiff != null )
        {
            headerString = getHeaderString();

            RiverCritHolder riverCritHolder = (RiverCritHolder) _selectedRecordDiff.getIhfsRecord();
            RaxRiverCrit raxRiverCrit = riverCritHolder.getRaxRiverCrit();
            RaxRiverCritRecord raxRecord = (RaxRiverCritRecord) _selectedRecordDiff.getRaxRecord();
            
            String lid = getPaddedString( raxRiverCrit.getLid(), "LID", raxRecord.getLid() );
            String pe = getPaddedString( raxRiverCrit.getPe(), "PE", raxRecord.getPe1() + raxRecord.getPe2() );
            String vdTime = getPaddedDateString( "", "ValidTime", raxRecord.getVdtime() );
            String lowScreen = getPaddedString( "", "LowScreen", raxRecord.getLowscreen() );
            String sigrate = getPaddedString( "", "SigRate", raxRecord.getSigrate() );
            String screenrate = getPaddedString( "", "ScreenRate", raxRecord.getScreenrate() );
            String fis = getPaddedString( raxRiverCrit.getFis(), "Fis", raxRecord.getFis() );
            String action = getPaddedString( raxRiverCrit.getAction(), "Action", raxRecord.getAction() );
            String alert = getPaddedString( "", "Alert", raxRecord.getAlert() );
            String bank = getPaddedString( raxRiverCrit.getBank(), "Bank", raxRecord.getBank() );
            String flood = getPaddedString( raxRiverCrit.getMinorFloodStage(), "Flood", raxRecord.getFlood() );
            String modflood = getPaddedString( raxRiverCrit.getModerateFloodStage(), "ModFlood", raxRecord.getModflood() );
            String majflood = getPaddedString( raxRiverCrit.getMajorFloodStage(), "MajFlood", raxRecord.getMajflood() );
            String record = getPaddedString( "", "Record", raxRecord.getRecord() );
            String highscreen = getPaddedString( "", "HighScreen", raxRecord.getHighscreen() );
            String damscreen = getPaddedString( "", "DamScreen", raxRecord.getDamscreen() );
            String lowscreenf = getPaddedString( "", "LowScreenF", raxRecord.getLowscreenf() );
            String sigratef = getPaddedString( "", "SigRateF", raxRecord.getSigratef() );
            String screenratef = getPaddedString( "", "ScreenRateF", raxRecord.getScreenratef() );
            String fisf = getPaddedString( raxRiverCrit.getFisF(), "FisF", raxRecord.getFisf() );
            String actionf = getPaddedString( raxRiverCrit.getActionF(), "ActionF", raxRecord.getActionf() );
            String alertf = getPaddedString( "", "AlertF", raxRecord.getAlertf() );
            String bankf = getPaddedString( "", "BankF", raxRecord.getBankf() );
            String floodf = getPaddedString( raxRiverCrit.getMinorFloodFlow(), "FloodF", raxRecord.getFloodf() );
            String modfloodf = getPaddedString( raxRiverCrit.getModerateFloodFlow(), "ModFloodF", raxRecord.getModfloodf() );
            String majfloodf = getPaddedString( raxRiverCrit.getMajorFloodFlow(), "MajFloodF", raxRecord.getMajfloodf() );
            String recordf = getPaddedString( "", "RecordF", raxRecord.getRecordf() );
            String highscreenf = getPaddedString( "", "HighScreenF", raxRecord.getHighscreenf() );
            String damscreenf = getPaddedString( "", "DamScreenF", raxRecord.getDamscreenf() );
            String sigratet = getPaddedString( "", "SigRateT", raxRecord.getSigratet() );
            String screenratet = getPaddedString( "", "ScreenRateT", raxRecord.getScreenratet() );
            String lowscreenq = getPaddedString( "", "LowScreenQ", raxRecord.getLowscreenq() );
            String sigrateq = getPaddedString( "", "SigRateQ", raxRecord.getSigrateq() );
            String screenrateq = getPaddedString( "", "ScreenRateQ", raxRecord.getScreenrateq() );
            String fisq = getPaddedString( "", "FisQ", raxRecord.getFisq() );
            String actionq = getPaddedString( "", "ActionQ", raxRecord.getActionq() );
            String alertq = getPaddedString( "", "AlertQ", raxRecord.getAlertq() );
            String bankq = getPaddedString( "", "BankQ", raxRecord.getBankq() );
            String floodq = getPaddedString( "", "FloodQ", raxRecord.getFloodq() );
            String modfloodq = getPaddedString( "", "ModFloodQ", raxRecord.getModfloodq() );
            String majfloodq = getPaddedString( "", "MajFloodQ", raxRecord.getMajfloodq() );
            String recordq = getPaddedString( "", "RecordQ", raxRecord.getRecordq() );
            String highscreenq = getPaddedString( "", "HighScreenQ", raxRecord.getHighscreenq() );
            String damscreenq = getPaddedString( "", "DamScreenQ", raxRecord.getDamscreenq() );
            String stream = getPaddedString( raxRiverCrit.getStream(), "Stream", raxRecord.getStream() );
            String lat = getPaddedString( _raxSyncDataMgr.formatLatLon( raxRiverCrit.getLatitude() ), "Lat", _raxSyncDataMgr.formatLatLon( raxRecord.getLat() ) );
            String lon = getPaddedString( _raxSyncDataMgr.formatLatLon( raxRiverCrit.getLongitude() ), "Lon", _raxSyncDataMgr.formatLatLon( raxRecord.getLon() ) );
            String da = getPaddedString( raxRiverCrit.getDa(), "Da", raxRecord.getDa() );
            String mile = getPaddedString( raxRiverCrit.getMile(), "Mile", raxRecord.getMile() );
            String zd = getPaddedString( raxRiverCrit.getZd(), "Zd", raxRecord.getZd() );
            String vdatum = getPaddedString( raxRiverCrit.getVdatum(), "VDatum", raxRecord.getVdatum() );
            String cb = getPaddedString( raxRiverCrit.getCb(), "Cb", raxRecord.getCb() );
            String level = getPaddedString( raxRiverCrit.getLevel(), "Level", raxRecord.getLevel() );
            String pool = getPaddedString( raxRiverCrit.getPool(), "Pool", raxRecord.getPool() );
            String por = getPaddedString( raxRiverCrit.getPor(), "Por", raxRecord.getPor() );
            String tide = getPaddedString( raxRiverCrit.getTide(), "Tide", raxRecord.getTide() );
            String backwater = getPaddedString( raxRiverCrit.getBackwater(), "Backwater", raxRecord.getBackwater() );
            String rrevise = getPaddedDateString( raxRiverCrit.getRrevise(), "RRevise", raxRecord.getRrevise() );
            String rsource = getPaddedString( raxRiverCrit.getRsource(), "RSource", raxRecord.getRsource() );
            String responseTime = getPaddedString( raxRiverCrit.getResponseTime(), "ResponseTime", raxRecord.getResponse_time() );
            String thresholdRunoff = getPaddedString( raxRiverCrit.getThresholdRunoff(), "ThresholdRunoff", raxRecord.getThreshold_runoff() );
            String uhgdur = getPaddedString( raxRiverCrit.getUhgdur(), "UhgDur", raxRecord.getUhgdur() );
            String remark = getPaddedString( raxRiverCrit.getRemark(), "Remark", raxRecord.getRemark() );
            

            recordStringList.add( lid );
            recordStringList.add( pe );
            recordStringList.add( vdTime );
            recordStringList.add( lowScreen );
            recordStringList.add( sigrate );
            recordStringList.add( screenrate );
            recordStringList.add( fis );
            recordStringList.add( action );
            recordStringList.add( alert );
            recordStringList.add( bank );
            recordStringList.add( flood );
            recordStringList.add( modflood );
            recordStringList.add( majflood );
            recordStringList.add( record );
            recordStringList.add( highscreen );
            recordStringList.add( damscreen );
            recordStringList.add( lowscreenf );
            recordStringList.add( sigratef );
            recordStringList.add( screenratef );
            recordStringList.add( fisf );
            recordStringList.add( actionf );
            recordStringList.add( alertf );
            recordStringList.add( bankf );
            recordStringList.add( floodf );
            recordStringList.add( modfloodf );
            recordStringList.add( majfloodf );
            recordStringList.add( recordf );
            recordStringList.add( highscreenf );
            recordStringList.add( damscreenf );
            recordStringList.add( sigratet );
            recordStringList.add( screenratet );
            recordStringList.add( lowscreenq );
            recordStringList.add( sigrateq );
            recordStringList.add( screenrateq );
            recordStringList.add( fisq );
            recordStringList.add( actionq );
            recordStringList.add( alertq );
            recordStringList.add( bankq );
            recordStringList.add( floodq );
            recordStringList.add( modfloodq );
            recordStringList.add( majfloodq );
            recordStringList.add( recordq );
            recordStringList.add( highscreenq );
            recordStringList.add( damscreenq );
            recordStringList.add( stream );
            recordStringList.add( lat );
            recordStringList.add( lon );
            recordStringList.add( da );
            recordStringList.add( mile );
            recordStringList.add( zd );
            recordStringList.add( vdatum );
            recordStringList.add( cb );
            recordStringList.add( level );
            recordStringList.add( pool );
            recordStringList.add( por );
            recordStringList.add( tide );
            recordStringList.add( backwater );
            recordStringList.add( rrevise );
            recordStringList.add( rsource );
            recordStringList.add( responseTime );
            recordStringList.add( thresholdRunoff );
            recordStringList.add( uhgdur );
            recordStringList.add( remark );

            List fieldDifferenceList = _selectedRecordDiff.getFieldDifferenceList();

            if ( fieldDifferenceList != null )
            {
                for ( int i = 0; i < fieldDifferenceList.size(); i++ )
                {
                    FieldDifference fieldDifference = (FieldDifference) fieldDifferenceList.get( i );
                    String columnName = fieldDifference.getName();

                    if ( columnName.equalsIgnoreCase( "fis" ) )
                    {
                        highlightStringList.add( fis );
                    }
                    else if ( columnName.equalsIgnoreCase( "action" ) )
                    {
                        highlightStringList.add( action );
                    }
                    else if ( columnName.equalsIgnoreCase( "bank" ) )
                    {
                        highlightStringList.add( bank );
                    }
                    else if ( columnName.equalsIgnoreCase( "flood" ) )
                    {
                        highlightStringList.add( flood );
                    }
                    else if ( columnName.equalsIgnoreCase( "modflood" ) )
                    {
                        highlightStringList.add( modflood );
                    }
                    else if ( columnName.equalsIgnoreCase( "majflood" ) )
                    {
                        highlightStringList.add( majflood );
                    }
                    else if ( columnName.equalsIgnoreCase( "fisf" ) )
                    {
                        highlightStringList.add( fisf );
                    }
                    else if ( columnName.equalsIgnoreCase( "actionf" ) )
                    {
                        highlightStringList.add( actionf );
                    }
                    else if ( columnName.equalsIgnoreCase( "floodf" ) )
                    {
                        highlightStringList.add( floodf );
                    }
                    else if ( columnName.equalsIgnoreCase( "modfloodf" ) )
                    {
                        highlightStringList.add( modfloodf );
                    }
                    else if ( columnName.equalsIgnoreCase( "majfloodf" ) )
                    {
                        highlightStringList.add( majfloodf );
                    }
                    else if ( columnName.equalsIgnoreCase( "stream" ) )
                    {
                        highlightStringList.add( stream );
                    }
                    else if ( columnName.equalsIgnoreCase( "lat" ) )
                    {
                        highlightStringList.add( lat );
                    }
                    else if ( columnName.equalsIgnoreCase( "lon" ) )
                    {
                        highlightStringList.add( lon );
                    }
                    else if ( columnName.equalsIgnoreCase( "da" ) )
                    {
                        highlightStringList.add( da );
                    }
                    else if ( columnName.equalsIgnoreCase( "mile" ) )
                    {
                        highlightStringList.add( mile );
                    }
                    else if ( columnName.equalsIgnoreCase( "zd" ) )
                    {
                        highlightStringList.add( zd );
                    }
                    else if ( columnName.equalsIgnoreCase( "vdatum" ) )
                    {
                        highlightStringList.add( vdatum );
                    }
                    else if ( columnName.equalsIgnoreCase( "cb" ) )
                    {
                        highlightStringList.add( cb );
                    }
                    else if ( columnName.equalsIgnoreCase( "level" ) )
                    {
                        highlightStringList.add( level );
                    }
                    else if ( columnName.equalsIgnoreCase( "pool" ) )
                    {
                        highlightStringList.add( pool );
                    }
                    else if ( columnName.equalsIgnoreCase( "por" ) )
                    {
                        highlightStringList.add( por );
                    }
                    else if ( columnName.equalsIgnoreCase( "tide" ) )
                    {
                        highlightStringList.add( tide );
                    }
                    else if ( columnName.equalsIgnoreCase( "backwater" ) )
                    {
                        highlightStringList.add( backwater );
                    }
                    else if ( columnName.equalsIgnoreCase( "rrevise" ) )
                    {
                        highlightStringList.add( rrevise );
                    }
                    else if ( columnName.equalsIgnoreCase( "rsource" ) )
                    {
                        highlightStringList.add( rsource );
                    }
                    else if ( columnName.equalsIgnoreCase( "reponse_time" ) )
                    {
                        highlightStringList.add( responseTime );
                    }
                    else if ( columnName.equalsIgnoreCase( "threshold_runoff" ) )
                    {
                        highlightStringList.add( thresholdRunoff );
                    }
                    else if ( columnName.equalsIgnoreCase( "uhgdur" ) )
                    {
                        highlightStringList.add( uhgdur );
                    }
                    else if ( columnName.equalsIgnoreCase( "remark" ) )
                    {
                        highlightStringList.add( remark );
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

        _recordPanel.setPreferredSize( new Dimension( 900, 400 ) );
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

        RaxSyncDataMgr syncDataMgr = new RaxSyncDataMgr( "jdbc:postgresql://lx5:5432/hd_ob83fwr?user=pguser", "jdbc:postgresql://lx5:5432/adb_ob83raxtest?user=pguser" );

        ProcessRiverCritDifferencesDialog test = new ProcessRiverCritDifferencesDialog( frame, syncDataMgr );
        test.displayGUI();
    }
}
