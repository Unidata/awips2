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
import ohd.hseb.ihfsdb.generated.AdjustFactorRecord;
import ohd.hseb.raxbase.util.JListPanelSuperCellRenderer;
import ohd.hseb.raxbase.util.PEManager;
import ohd.hseb.raxbase.util.StringPadder;
import ohd.hseb.raxbase.util.TSManager;
import ohd.hseb.raxdb.generated.RaxAdjustFactorRecord;
import ohd.hseb.raxdb_sync.FieldDifference;
import ohd.hseb.raxdb_sync.RaxSyncDataMgr;
import ohd.hseb.raxdb_sync.RecordDifference;
import ohd.hseb.rfc.util.listpanels.JListPanel;

public class ProcessAdjustFactorDifferencesDialog extends ProcessDifferencesDialog
{
     public ProcessAdjustFactorDifferencesDialog(  JFrame frame, RaxSyncDataMgr syncDataMgr )
     {
         super( frame, "Process AdjustFactor", "AdjustFactor", syncDataMgr, new StringPadder( 25, 35 ), new Dimension( 650, 550 ) );
         _keyString = "(Lid | PE | Dur | TS | Extremum)";
     }
    
     protected String getSelectionComboBoxString( RecordDifference recordDiff )
     {
         String cbString = null;
         
         RaxAdjustFactorRecord record = (RaxAdjustFactorRecord) recordDiff.getRaxRecord();
         StringPadder padder = new StringPadder( 8, 8 );

         cbString = padder.getFrontPaddedString( record.getLid() ) + " | " + 
                    PEManager.getPEFromPe1Pe2( record.getPe1(), record.getPe2() ) + " | " + 
                    record.getDur() + " | " + 
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

             AdjustFactorRecord ihfsRecord = (AdjustFactorRecord) _selectedRecordDiff.getIhfsRecord();
             RaxAdjustFactorRecord raxRecord = (RaxAdjustFactorRecord) _selectedRecordDiff.getRaxRecord();

             String lid = _padder.getFrontPaddedString( ihfsRecord.getLid() ) + "LID" + _padder.getEndPaddedString( "LID", raxRecord.getLid() );
             String pe = _padder.getFrontPaddedString( ihfsRecord.getPe() ) + "PE" + _padder.getEndPaddedString( "PE", raxRecord.getPe1() + raxRecord.getPe2() );
             String dur = _padder.getFrontPaddedString( ihfsRecord.getDur() ) + "Dur" + _padder.getEndPaddedString( "Dur", raxRecord.getIdur() + "/" + raxRecord.getDur() );
             String iDur = _padder.getFrontPaddedString( "" ) + "IDur" + _padder.getEndPaddedString( "IDur", raxRecord.getIdur() );
             String ts = _padder.getFrontPaddedString( ihfsRecord.getTs() ) + "TS" + _padder.getEndPaddedString( "TS", raxRecord.getT() + raxRecord.getS() );
             String extremum = _padder.getFrontPaddedString( ihfsRecord.getExtremum() ) + "Extremum" + _padder.getEndPaddedString( "Extremum", raxRecord.getE() );
             String beginDate = _padder.getFrontPaddedString( "" ) + "BeginDate" + _padder.getEndPaddedString( "BeginDate", DbTimeHelper.getDateStringFromLongTime( raxRecord.getBegin_date() ) ); 
             String divisor = _padder.getFrontPaddedString( ihfsRecord.getDivisor() ) + "Divisor" + _padder.getEndPaddedString( "Divisor", raxRecord.getDivisor() );
             String base = _padder.getFrontPaddedString( ihfsRecord.getBase() ) + "Base" + _padder.getEndPaddedString( "Base", raxRecord.getBase() );
             String multiplier = _padder.getFrontPaddedString( ihfsRecord.getMultiplier() ) + "Multiplier" + _padder.getEndPaddedString( "Multiplier", raxRecord.getMultiplier() );
             String adder = _padder.getFrontPaddedString( ihfsRecord.getAdder() ) + "Adder" + _padder.getEndPaddedString( "Adder", raxRecord.getAdder() );

             recordStringList.add( lid );
             recordStringList.add( pe );
             recordStringList.add( dur );
             recordStringList.add( iDur );
             recordStringList.add( ts );
             recordStringList.add( extremum );
             recordStringList.add( beginDate );
             recordStringList.add( divisor );
             recordStringList.add( base );
             recordStringList.add( multiplier );
             recordStringList.add( adder );

             List fieldDifferenceList = _selectedRecordDiff.getFieldDifferenceList();

             if ( fieldDifferenceList != null )
             {
                 for ( int i = 0; i < fieldDifferenceList.size(); i++ )
                 {
                     FieldDifference fieldDifference = (FieldDifference) fieldDifferenceList.get( i );
                     String columnName = fieldDifference.getName();

                     if ( columnName.equalsIgnoreCase( "divisor" ) )
                     {
                         highlightStringList.add( divisor );
                     }
                     else if ( columnName.equalsIgnoreCase( "base" ) )
                     {
                         highlightStringList.add( base );
                     }
                     else if ( columnName.equalsIgnoreCase( "multiplier" ) )
                     {
                         highlightStringList.add( multiplier );
                     }
                     else if ( columnName.equalsIgnoreCase( "adder" ) )
                     {
                         highlightStringList.add( adder );
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

         RaxSyncDataMgr syncDataMgr = new RaxSyncDataMgr( "jdbc:postgresql://lx5:5432/hd_ob82krfx?user=pguser", "jdbc:postgresql://ax2:5432/adb_ob82krf?user=pguser" );

         ProcessAdjustFactorDifferencesDialog test = new ProcessAdjustFactorDifferencesDialog( frame, syncDataMgr );
         test.displayGUI();
     }
}
