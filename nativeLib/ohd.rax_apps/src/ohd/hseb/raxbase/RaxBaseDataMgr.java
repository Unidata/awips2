package ohd.hseb.raxbase;

import java.beans.PropertyChangeListener;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JFrame;

import ohd.hseb.db.Database;
import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.ihfsdb.generated.ShefProbRecord;
import ohd.hseb.ihfsdb.generated.ShefProbTable;
import ohd.hseb.raxbase.db.CustomRaxRatingTable;
import ohd.hseb.raxbase.model.Agency;
import ohd.hseb.raxbase.model.Average;
import ohd.hseb.raxbase.model.Counties;
import ohd.hseb.raxbase.model.Country;
import ohd.hseb.raxbase.model.Huc2;
import ohd.hseb.raxbase.model.Huc4;
import ohd.hseb.raxbase.model.Huc6;
import ohd.hseb.raxbase.model.Huc8;
import ohd.hseb.raxbase.model.ModCtrl;
import ohd.hseb.raxbase.model.Prod;
import ohd.hseb.raxbase.model.RaxAdjustFactor;
import ohd.hseb.raxbase.model.RaxCrest;
import ohd.hseb.raxbase.model.RaxDataLimits;
import ohd.hseb.raxbase.model.RaxIngestFilter;
import ohd.hseb.raxbase.model.RaxLocDataLimits;
import ohd.hseb.raxbase.model.RaxLocation;
import ohd.hseb.raxbase.model.RaxRating;
import ohd.hseb.raxbase.model.RaxRatingOffset;
import ohd.hseb.raxbase.model.RaxRatingPoint;
import ohd.hseb.raxbase.model.RaxRatingShift;
import ohd.hseb.raxbase.model.RaxReservoir;
import ohd.hseb.raxbase.model.RaxRiverCrit;
import ohd.hseb.raxbase.model.Rfc;
import ohd.hseb.raxbase.model.Sensok;
import ohd.hseb.raxbase.model.ShefDuration;
import ohd.hseb.raxbase.model.ShefExtremum;
import ohd.hseb.raxbase.model.ShefPE;
import ohd.hseb.raxbase.model.ShefPE1;
import ohd.hseb.raxbase.model.ShefPETrans;
import ohd.hseb.raxbase.model.ShefProb;
import ohd.hseb.raxbase.model.ShefQC;
import ohd.hseb.raxbase.model.ShefTS;
import ohd.hseb.raxbase.model.SlopeProfile;
import ohd.hseb.raxbase.model.State;
import ohd.hseb.raxbase.model.WfoHsa;
import ohd.hseb.raxbase.table.ArcBaseAdjustFactorJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseAgencyJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseCountiesJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseCountryJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseCrestJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseDataLimitsJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseHuc2JTableRowData;
import ohd.hseb.raxbase.table.ArcBaseHuc4JTableRowData;
import ohd.hseb.raxbase.table.ArcBaseHuc6JTableRowData;
import ohd.hseb.raxbase.table.ArcBaseHuc8JTableRowData;
import ohd.hseb.raxbase.table.ArcBaseIngestFilterJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseLocationJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseModCtrlJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseProdJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseRatingJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseRatingOffsetsJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseRfcJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseShefDurationJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseShefExtremumJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseShefPE1JTableRowData;
import ohd.hseb.raxbase.table.ArcBaseShefPEJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseShefPETransJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseShefProbJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseShefQCJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseShefTSJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseStateJTableRowData;
import ohd.hseb.raxbase.table.ArcBaseWfoHsaJTableRowData;
import ohd.hseb.raxbase.util.ModalDialogRunner;
import ohd.hseb.raxbase.util.StatusDialog;
import ohd.hseb.raxdb.generated.AgencyRecord;
import ohd.hseb.raxdb.generated.AgencyTable;
import ohd.hseb.raxdb.generated.AvgRecord;
import ohd.hseb.raxdb.generated.AvgTable;
import ohd.hseb.raxdb.generated.CountiesRecord;
import ohd.hseb.raxdb.generated.CountiesTable;
import ohd.hseb.raxdb.generated.CountryRecord;
import ohd.hseb.raxdb.generated.CountryTable;
import ohd.hseb.raxdb.generated.Huc2Record;
import ohd.hseb.raxdb.generated.Huc2Table;
import ohd.hseb.raxdb.generated.Huc4Record;
import ohd.hseb.raxdb.generated.Huc4Table;
import ohd.hseb.raxdb.generated.Huc6Record;
import ohd.hseb.raxdb.generated.Huc6Table;
import ohd.hseb.raxdb.generated.Huc8Record;
import ohd.hseb.raxdb.generated.Huc8Table;
import ohd.hseb.raxdb.generated.ModctrlRecord;
import ohd.hseb.raxdb.generated.ModctrlTable;
import ohd.hseb.raxdb.generated.ProdRecord;
import ohd.hseb.raxdb.generated.ProdTable;
import ohd.hseb.raxdb.generated.RaxAdjustFactorRecord;
import ohd.hseb.raxdb.generated.RaxAdjustFactorTable;
import ohd.hseb.raxdb.generated.RaxCrestRecord;
import ohd.hseb.raxdb.generated.RaxCrestTable;
import ohd.hseb.raxdb.generated.RaxDataLimitsRecord;
import ohd.hseb.raxdb.generated.RaxDataLimitsTable;
import ohd.hseb.raxdb.generated.RaxIngestFilterRecord;
import ohd.hseb.raxdb.generated.RaxIngestFilterTable;
import ohd.hseb.raxdb.generated.RaxLocDataLimitsRecord;
import ohd.hseb.raxdb.generated.RaxLocDataLimitsTable;
import ohd.hseb.raxdb.generated.RaxLocationRecord;
import ohd.hseb.raxdb.generated.RaxLocationTable;
import ohd.hseb.raxdb.generated.RaxRatingRecord;
import ohd.hseb.raxdb.generated.RaxRatingShiftRecord;
import ohd.hseb.raxdb.generated.RaxRatingShiftTable;
import ohd.hseb.raxdb.generated.RaxRatingTable;
import ohd.hseb.raxdb.generated.RaxReservoirRecord;
import ohd.hseb.raxdb.generated.RaxReservoirTable;
import ohd.hseb.raxdb.generated.RaxRiverCritRecord;
import ohd.hseb.raxdb.generated.RaxRiverCritTable;
import ohd.hseb.raxdb.generated.RfcRecord;
import ohd.hseb.raxdb.generated.RfcTable;
import ohd.hseb.raxdb.generated.SensokRecord;
import ohd.hseb.raxdb.generated.SensokTable;
import ohd.hseb.raxdb.generated.ShefdurRecord;
import ohd.hseb.raxdb.generated.ShefdurTable;
import ohd.hseb.raxdb.generated.ShefexRecord;
import ohd.hseb.raxdb.generated.ShefexTable;
import ohd.hseb.raxdb.generated.Shefpe1Record;
import ohd.hseb.raxdb.generated.Shefpe1Table;
import ohd.hseb.raxdb.generated.ShefpeRecord;
import ohd.hseb.raxdb.generated.ShefpeTable;
import ohd.hseb.raxdb.generated.ShefpetransRecord;
import ohd.hseb.raxdb.generated.ShefpetransTable;
import ohd.hseb.raxdb.generated.ShefprobRecord;
import ohd.hseb.raxdb.generated.ShefprobTable;
import ohd.hseb.raxdb.generated.ShefqcRecord;
import ohd.hseb.raxdb.generated.ShefqcTable;
import ohd.hseb.raxdb.generated.SheftsRecord;
import ohd.hseb.raxdb.generated.SheftsTable;
import ohd.hseb.raxdb.generated.SlopeprofileRecord;
import ohd.hseb.raxdb.generated.SlopeprofileTable;
import ohd.hseb.raxdb.generated.StateRecord;
import ohd.hseb.raxdb.generated.StateTable;
import ohd.hseb.raxdb.generated.Wfo_hsaRecord;
import ohd.hseb.raxdb.generated.Wfo_hsaTable;
import ohd.hseb.raxdb_sync.RaxSyncDataMgr;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.Logger;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.DialogHelper;

public class RaxBaseDataMgr 
{
    private Database _raxDb = null;
    private List _raxLocationRowDataList = null;
    private List _raxLocationList = null; 
    private List _raxDataLimitsList = null;
    private List _raxAdjustFactorList = null;
    private List _raxCrestList = null;
    private List _modCtrlList = null;
    private List _countryList = null;
    private List _huc2List = null;
    private List _huc4List = null;
    private List _huc6List = null;
    private List _huc8List = null;
    private List _shefPE1List = null;
    private List _wfoHsaList = null;
    private List _rfcList = null;
    private List _stateList = null;
    private List _countiesList = null;
    private List _shefPETransList = null;
    private List _shefQCList = null;
    private List _agencyList = null;
    private List _prodList = null;
    private List _raxRatingList = null;
    private List _raxRatingShiftList = null;
    private List _raxOffsetsList = null;
    private List _raxLocDataLimitsList = null;
    private List _raxIngestFilterRowDataList = null;
    private List _raxIngestFilterList = null;  // might not keep
    private List _raxRiverCritList = null;
    private Map _raxIngestFilterRowDataToRaxIngestFilterMap = new HashMap();
    private Map _raxDataLimitsRowDataToRaxDataLimitsMap = new HashMap();
    private Map _raxAdjustFactorRowDataToRaxAdjustFactorMap = new HashMap();
    private Map _raxCrestRowDataToRaxCrestMap = new HashMap();
    private Map _modCtrlRowDataToModCtrlMap = new HashMap();
    private Map _raxLocDataLimitsRowDataToRaxLocDataLimitsMap = new HashMap();
    private Map _countryRowDataToCountryMap = new HashMap();
    private Map _stateRowDataToStateMap = new HashMap();
    private Map _countiesRowDataToCountiesMap = new HashMap();
    private Map _huc2RowDataToHuc2Map = new HashMap();
    private Map _huc4RowDataToHuc4Map = new HashMap();
    private Map _huc6RowDataToHuc6Map = new HashMap();
    private Map _huc8RowDataToHuc8Map = new HashMap();
    private Map _wfoHsaRowDataToWfoHsaMap = new HashMap();
    private Map _rfcRowDataToRfcMap = new HashMap();
    private Map _shefDurRowDataToShefDurMap = new HashMap();
    private Map _shefExRowDataToShefExMap = new HashMap();
    private Map _shefPeTransRowDataToShefPeTransMap = new HashMap();
    private Map _shefPeRowDataToShefPeMap = new HashMap();
    private Map _shefPe1RowDataToShefPe1Map = new HashMap();
    private Map _shefProbRowDataToShefProbMap = new HashMap();
    private Map _shefQCRowDataToShefQCMap = new HashMap();
    private Map _shefTsRowDataToShefTsMap = new HashMap();
    private Map _agencyRowDataToAgencyMap = new HashMap();
    private Map _prodRowDataToProdMap = new HashMap();
    private Map _stateCountryFipsStringToStateRecordMap = new HashMap();

    private RaxSyncDataMgr _raxSyncDataMgr = null;
    
    private String _ihfsConnectionString = null;
    
    private List _shefPeList = null;
    private List _shefDurationList = null;
    private List _shefTSList = null;
    private List _shefExtremumList = null;
    private List _shefProbList = null;
    private Map _shefPEMap = new HashMap();
    private Map _shefDurationMap = new HashMap();
    private Map _shefTSMap = new HashMap();
    private Map _shefExtremumMap = new HashMap();
    private Map _shefProbMap = new HashMap();
    
    private Logger _logger = null;

    private static final String _numberFormatString = "0.0000";
    private static final DecimalFormat _numberFormatter = new DecimalFormat(_numberFormatString);

    private StatusDialog _statDialog = null;
    
    //    private static final short MISSING = -9999;
    
    public RaxBaseDataMgr( String jdbcConnectionString )
    {
        _raxDb = new Database();
        setLogger( "/awips/hydroapps/ob83_fwr/whfs/local/log" );
        _raxDb.connectWithDriverSearch( jdbcConnectionString );
        initShefPeList();
        initShefDurationList();
        initShefProbList();
        initShefTSList();
        initShefExtremumList();
    }
    
    public void initRaxLocationRowDataList()
    {
        _raxLocationRowDataList = getLocationRowDataList();
    }
    
    public RaxBaseDataMgr( String jdbcConnectionString, String ihfsConnectionString, String logFilePath )
    {
        this( jdbcConnectionString );
        setLogger( logFilePath );
        _ihfsConnectionString = ihfsConnectionString;
    }
    
    private void setLogger( String logFilePath )
    {
        String logFileName = logFilePath + "/RaxBase.log";
        
        _logger = new FileLogger( logFileName, true, true );
    }
    

    
    public void updateRaxLocationRowDataList()
    {
        _raxLocationRowDataList = getLocationRowDataList();
    }
// ------------------------------------ PUBLIC METHODS --------------------------------

    public boolean initProcessDataMgr()
    {
        boolean created = false;
        
        if ( getRaxSyncDataMgr() == null )
        {
            try
            {
                Connection connection = DriverManager.getConnection( _ihfsConnectionString );
                connection.close();
                _raxSyncDataMgr = new RaxSyncDataMgr( _ihfsConnectionString, _raxDb );
                created = true;
            }
            catch( SQLException e )
            {
                DialogHelper.displayErrorDialog( new JFrame(), "Error in IHFS JDBC URL: " + _ihfsConnectionString, "Bad IHFS JDBC URL" );
            }
        }
        else  // not null
        {
            created = true;
        }
        
        return created;
    }
    
    /**
     * This method reads from the Location table in the Rax Database, creates a list of RaxLocation objects,
     * creates a list of ArcBaseLocationJTableRowData objects (used for the jtable), and returns the 
     * rowdata List
     * 
     * @return Returns a list of Location Row Data objects
     */
    public List getLocationRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        List raxLocationRecordsList = null;
        List raxLocationRowDataList = null;
        
        raxLocationRecordsList = getRaxLocationRecordsList();
        _raxLocationList = getRaxLocationListFromRaxLocationRecordsList( raxLocationRecordsList );
       
        raxLocationRowDataList = getRowDataListFromRaxLocationList();
        
        _logger.log( "Finished reading in RaxLocation data in " + timer1.stop() );
        return raxLocationRowDataList;
    }
    
    public String getFormattedDouble( double value )
    {
        return _numberFormatter.format(value); 
    } 
    
    public List getFilteredRaxLocationRowDataList( String filterString )
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        List filteredRaxLocationRowDataList = new ArrayList();
        boolean foundMatch = false;
        ArcBaseLocationJTableRowData rowData = null;
        int startIndex = -9999;

        if ( ! filterString.equalsIgnoreCase( "" ) )  // not a blank string
        {
            for ( int i = 0; i < _raxLocationRowDataList.size(); i++ )
            {
                rowData = (ArcBaseLocationJTableRowData) _raxLocationRowDataList.get( i );

                if ( rowData.getRaxLocation().getLid().startsWith( filterString ) )
                {
                    startIndex = i;
                    foundMatch = true;
                    break;
                }
            }

            while ( ( foundMatch == true ) && ( startIndex < _raxLocationRowDataList.size() ) )
            {
                rowData = (ArcBaseLocationJTableRowData) _raxLocationRowDataList.get( startIndex );
                if ( rowData.getRaxLocation().getLid().startsWith( filterString ) )
                {
                    rowData.addAllCellsToMap();
                    filteredRaxLocationRowDataList.add( rowData );
                }
                else
                {
                    foundMatch = false;
                }
                startIndex++;
            }
        }
        else
        {
            filteredRaxLocationRowDataList = _raxLocationRowDataList;
        }
        
        _logger.log( "Finished filtering RaxLocationRowData in " + timer1.stop() );
        return filteredRaxLocationRowDataList;
    }

    /**
     * This method does the following:
     * 1) Reads from the IngestFilter, ShefPE, ShefDur, ShefTS, and ShefEX table in the Rax Database
     * 2) Creates a list of RaxIngestFilter objects
     * 3) Creates a list of ArcBaseIngestFilterJTableRowData objects (used for the jtable)
     * 4) returns the rowdata List
     *  
     * @return Returns a list of IngestFilter Row Data objects
     */
    public void initIngestFilterRowData( PropertyChangeListener listener )
    {
        _statDialog = new StatusDialog( "Loading IngestFilter data..." );
        _statDialog.addPropertyChangeListener( listener );

        Thread t = new Thread( new Runnable()
        {
            public void run()
            {
                _statDialog.setVisible( true );
            }
        }
        );
        
        t.start();

        ModalDialogRunner dialogRunner = new ModalDialogRunner( this );
        dialogRunner.start();

//       _raxIngestFilterRowDataList = createIngestFilterRowDataList();
    }
    
    public void finishProcess()
    {
        _statDialog.setVisible( false );
        _statDialog.firePropListener();
    }
    
    public void updateIngestFilterRowData()
    {
        _raxIngestFilterRowDataList = createIngestFilterRowDataList();
    }

    public List getDataLimitsRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        List raxDataLimitsRecordsList = null;
        List raxDataLimitsRowDataList = null;
        
        raxDataLimitsRecordsList = getRaxDataLimitsRecordsList();
        _raxDataLimitsList = getRaxDataLimitsListFromRaxDataLimitsRecordsList( raxDataLimitsRecordsList );
        
        raxDataLimitsRowDataList = getRowDataListFromRaxDataLimitsList();
        
        _logger.log( "Finished reading in RaxDataLimits data in " + timer1.stop() );
        
        return raxDataLimitsRowDataList;
    }

    public List getAdjustFactorRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List raxAdjustFactorRecordsList = null;
        List raxAdjustFactorRowDataList = null;
        
        raxAdjustFactorRecordsList = getRaxAdjustFactorRecordsList();
        _raxAdjustFactorList = getRaxAdjustFactorListFromRaxAdjustFactorRecordsList( raxAdjustFactorRecordsList );
        
        raxAdjustFactorRowDataList = getRowDataListFromRaxAdjustFactorList();
        
        _logger.log( "Finished reading in RaxAdjustFactor data in " + timer1.stop() );
        
        return raxAdjustFactorRowDataList;
    }
    
    public List getCrestRowDataList( String lid )
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List raxCrestRecordsList = null;
        List raxCrestRowDataList = null;
        
        raxCrestRecordsList = getRaxCrestRecordsList( lid );
        _raxCrestList = getRaxCrestListFromRaxCrestRecordsList( raxCrestRecordsList );
        
        raxCrestRowDataList = getRowDataListFromRaxCrestList();
        
        _logger.log( "Finished reading in Crest data in " + timer1.stop() );
        
        return raxCrestRowDataList;
    }
    
    public List getCountiesRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List countiesRecordsList = null;
        List countiesRowDataList = null;
        
        countiesRecordsList = getCountiesRecordsList();
        _countiesList = getCountiesListFromCountiesRecordsList( countiesRecordsList );
        
        countiesRowDataList = getRowDataFromCountiesList();
        
        _logger.log( "Finished reading in Counties data in " + timer1.stop() );
        
        return countiesRowDataList;
    }
    
    public List getStateRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List stateRecordsList = null;
        List stateRowDataList = null;
        
        stateRecordsList = getStateRecordsList();
        _stateList = getStateListFromStateRecordsList( stateRecordsList );
        
        stateRowDataList = getRowDataFromStateList();
        
        _logger.log( "Finished reading in State data in " + timer1.stop() );
        
        return stateRowDataList;
    }
    
    public List getProdRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List prodRecordsList = null;
        List prodRowDataList = null;
        
        prodRecordsList = getProdRecordsList();
        _prodList = getProdListFromProdRecordsList( prodRecordsList );
        
        prodRowDataList = getRowDataFromProdList();
        
        _logger.log( "Finished reading in Prod data in " + timer1.stop() );
        
        return prodRowDataList;
    }
    
    public List getHuc8RowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List huc8RecordsList = null;
        List huc8RowDataList = null;
        
        huc8RecordsList = getHuc8RecordsList();
        _huc8List = getHuc8ListFromHuc8RecordsList( huc8RecordsList );
        
        huc8RowDataList = getRowDataFromHuc8List();
        
        _logger.log( "Finished reading in Huc8 data in " + timer1.stop() );
        
        return huc8RowDataList;
    }

    public List getHuc6RowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List huc6RecordsList = null;
        List huc6RowDataList = null;
        
        huc6RecordsList = getHuc6RecordsList();
        _huc6List = getHuc6ListFromHuc6RecordsList( huc6RecordsList );
        
        huc6RowDataList = getRowDataFromHuc6List();
        
        _logger.log( "Finished reading in Huc6 data in " + timer1.stop() );
        
        return huc6RowDataList;
    }

    public List getShefQCRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();

        List shefQCRecordsList = null;
        List shefQCRowDataList = null;
        
        shefQCRecordsList = getShefQCRecordsList();
        _shefQCList = getShefQCListFromShefQCRecordsList( shefQCRecordsList );
        
        shefQCRowDataList = getRowDataFromShefQCList();
        
        _logger.log( "Finished reading in ShefQC data in " + timer1.stop() );
        
        return shefQCRowDataList;
    }
    
    public List getAgencyRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();

        List agencyRecordsList = null;
        List agencyRowDataList = null;
        
        agencyRecordsList = getAgencyRecordsList();
        _agencyList = getAgencyListFromAgencyRecordsList( agencyRecordsList );
        
        agencyRowDataList = getRowDataFromAgencyList();
        
        _logger.log( "Finished reading in Agency data in " + timer1.stop() );
        
        return agencyRowDataList;
    }
    
    public List getShefPe1RowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();

        List shefPe1RecordsList = null;
        List shefPe1RowDataList = null;
        
        shefPe1RecordsList = getPE1RecordsList();
        _shefPE1List = getShefPE1ListFromShefPE1RecordsList( shefPe1RecordsList );
        
        shefPe1RowDataList = getRowDataFromShefPE1List();
        
        _logger.log( "Finished reading in ShefPE1 data in " + timer1.stop() );
        
        return shefPe1RowDataList;
    }
    
    public List getHuc4RowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List huc4RecordsList = null;
        List huc4RowDataList = null;
        
        huc4RecordsList = getHuc4RecordsList();
        _huc4List = getHuc4ListFromHuc4RecordsList( huc4RecordsList );
        
        huc4RowDataList = getRowDataFromHuc4List();
        
        _logger.log( "Finished reading in Huc4 data in " + timer1.stop() );
        
        return huc4RowDataList;
    }

    public List getRfcRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List rfcRecordsList = null;
        List rfcRowDataList = null;
        
        rfcRecordsList = getRfcRecordsList();
        _rfcList = getRfcListFromRfcRecordsList( rfcRecordsList );
        
        rfcRowDataList = getRowDataFromRfcList();
        
        _logger.log( "Finished reading in Rfc data in " + timer1.stop() );
        
        return rfcRowDataList;
    }

    public List getWfoHsaRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List wfoHsaRecordsList = null;
        List wfoHsaRowDataList = null;
        
        wfoHsaRecordsList = getWfoHsaRecordsList();
        _wfoHsaList = getWfoHsaListFromWfoHsaRecordsList( wfoHsaRecordsList );
        
        wfoHsaRowDataList = getRowDataFromWfoHsaList();
        
        _logger.log( "Finished reading in WfoHsa data in " + timer1.stop() );
        
        return wfoHsaRowDataList;
    }

    public List getShefTsRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List shefTsRowDataList = null;
        
        shefTsRowDataList = getRowDataFromShefTsList();
        
        _logger.log( "Finished reading in ShefTS data in " + timer1.stop() );

        return shefTsRowDataList;
    }

    public List getShefExRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List shefExRowDataList = null;
        
        shefExRowDataList = getRowDataFromShefExList();
        
        _logger.log( "Finished reading in ShefExtremum data in " + timer1.stop() );

        return shefExRowDataList;
    }
    
    public List getShefProbRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List shefProbRowDataList = null;
        
        shefProbRowDataList = getRowDataFromShefProbList();
        
        _logger.log( "Finished reading in ShefProb data in " + timer1.stop() );
        
        return shefProbRowDataList;
    }

    public List getShefPeRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List shefPeRowDataList = null;
        
        shefPeRowDataList = getRowDataFromShefPEList();
        
        _logger.log( "Finished reading in ShefPE data in " + timer1.stop() );
        
        return shefPeRowDataList;
    }
    
    public List getShefDurRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List shefDurRowDataList = null;
        initShefDurationList();
        shefDurRowDataList = getRowDataFromShefDurationList();
        
        _logger.log( "Finished reading in ShefDur data in " + timer1.stop() );

        return shefDurRowDataList;
    }
    
    public List getShefPeTransRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List shefPETransRecordsList = null;
        List shefPETransRowDataList = null;
        
        shefPETransRecordsList = getShefPETransRecordsList();
        _shefPETransList = getShefPETransListFromShefPETransRecordsList( shefPETransRecordsList );
        
        shefPETransRowDataList = getRowDataFromShefPETransList();
        
        _logger.log( "Finished reading in ShefPETrans data in " + timer1.stop() );
        
        return shefPETransRowDataList;
    }
    
    public List getHuc2RowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List huc2RecordsList = null;
        List huc2RowDataList = null;
        
        huc2RecordsList = getHuc2RecordsList();
        _huc2List = getHuc2ListFromHuc2RecordsList( huc2RecordsList );
        
        huc2RowDataList = getRowDataFromHuc2List();
        
        _logger.log( "Finished reading in Huc2 data in " + timer1.stop() );
        
        return huc2RowDataList;
    }
    
    public List getCountryRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List countryRecordsList = null;
        List countryRowDataList = null;
        
        countryRecordsList = getCountryRecordsList();
        _countryList = getCountryListFromCountryRecordsList( countryRecordsList );
        
        countryRowDataList = getRowDataFromCountryList();
        
        _logger.log( "Finished reading in Country data in " + timer1.stop()  );
        
        return countryRowDataList;
    }
    
    public List getModCtrlRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List modCtrlRecordsList = null;
        List modCtrlRowDataList = null;
        
        modCtrlRecordsList = getModCtrlRecordsList();
        _modCtrlList = getModCtrlListFromModCtrlRecordsList( modCtrlRecordsList );
        
        modCtrlRowDataList = getRowDataListFromModCtrlList();
        
        _logger.log( "Finished reading in ModCtrl data in " + timer1.stop()  );
        
        return modCtrlRowDataList;
    }

    public List getRatingCurveList( String lid )
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List raxRatingRecordsList = null;
        
        raxRatingRecordsList = getRaxRatingRecordsList( lid );
        _raxRatingList = getRaxRatingListFromRaxRatingRecordsList( raxRatingRecordsList );
        
        _logger.log( "Finished reading in Rating Data in " + timer1.stop()  );
        
        return _raxRatingList;
    }
    
    public List getRatingShiftList( String lid )
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List raxRatingShiftRecordsList = null;
        
        raxRatingShiftRecordsList = getRaxRatingShiftRecordsList( lid );
        _raxRatingShiftList = getRaxRatingShiftListFromRaxRatingShiftRecordsList( raxRatingShiftRecordsList );
        
        _logger.log( "Finished reading in Rating Shift data in " + timer1.stop()  );
        
        return _raxRatingShiftList;
    }
   
    
    public List getRatingCurveRowDataList( RaxRating raxRating )
    {
        List ratingCurveRowDataList = new ArrayList();
        
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        if ( raxRating != null )
        {
            ratingCurveRowDataList = getRatingCurveRowDataListFromRaxRating( raxRating.getRaxRatingPointList() );
        }
            
        _logger.log( "Finished reading in Rating Point data in " + timer1.stop()  );
        
        return ratingCurveRowDataList;
    }
    
    public List getOffsetsRowDataList( RaxRating raxRating )
    {
        List ratingOffsetsRowDataList = new ArrayList();
        
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        if ( raxRating != null )
        {
            ratingOffsetsRowDataList = getRatingOffsetsRowDataListFromRaxRating( raxRating.getOffsets() );
        }
        _logger.log( "Finished reading in Offsets data in " + timer1.stop()  );
        
        return ratingOffsetsRowDataList;
    }
    
    public List getLocDataLimitsRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        List raxLocDataLimitsRecordsList = null;
        List raxLocDataLimitsRowDataList = null;
        
        raxLocDataLimitsRecordsList = getRaxLocDataLimitsRecordsList();
        _raxLocDataLimitsList = getRaxLocDataLimitsListFromRaxLocDataLimitsRecordsList( raxLocDataLimitsRecordsList );
        
        raxLocDataLimitsRowDataList = getRowDataListFromRaxLocDataLimitsList();
        
        _logger.log( "Finished reading in RaxLocDataLimits data in " + timer1.stop()  );
        
        return raxLocDataLimitsRowDataList;
    }
    
    public List setupAndGetRaxRiverCritList( String lid )
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        
        List raxRiverCritRecordsList = null;
        
        raxRiverCritRecordsList = getRaxRiverCritRecordsList( lid );
        _raxRiverCritList = getRaxRiverCritListFromRaxRiverCritRecordsList( raxRiverCritRecordsList );
        
        _logger.log( "Finished reading in RaxRiverCrit data in " + timer1.stop()  );
        
        return _raxRiverCritList;
    }
    
    public List getRaxReservoirListFromRaxLocation( String lid )
    {
        List raxReservoirList = new ArrayList();
        List raxReservoirRecordsList = null;
        RaxReservoirRecord record = null;
        RaxReservoirTable table = new RaxReservoirTable( _raxDb );
        String where = " WHERE lid='" + lid + "' ";
        
        try
        {
            raxReservoirRecordsList = table.select( where );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        for ( int i = 0; i < raxReservoirRecordsList.size(); i++ )
        {
            record = (RaxReservoirRecord) raxReservoirRecordsList.get( i );
            raxReservoirList.add( new RaxReservoir( record ) );
        }
        
        return raxReservoirList;
    }
    
    public List getAverageList( String lid )
    {
        List averageList = new ArrayList();
        List averageRecordsList = null;
        AvgRecord record = null;
        AvgTable table = new AvgTable( _raxDb );
        String where = " WHERE lid ='" + lid + "' ";
        
        try
        {
            averageRecordsList = table.select( where );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        for ( int i = 0; i < averageRecordsList.size(); i++ )
        {
            record = (AvgRecord) averageRecordsList.get( i );
            averageList.add( new Average( record ) );
        }
        
        return averageList;
    }
    
    public List getSensokList( String lid )
    {
        List sensokList = new ArrayList();
        List sensOkRecordList = null;
        SensokRecord record = null;
        SensokTable table = new SensokTable( _raxDb );
        String where = " WHERE lid = '" + lid + "' ";
        
        try
        {
            sensOkRecordList = table.select( where );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        for ( int i = 0; i < sensOkRecordList.size(); i++ )
        {
            record = (SensokRecord) sensOkRecordList.get( i );
            sensokList.add( new Sensok( record ) );
        }
        
        return sensokList;
    }
    
    public List getSlopeProfileList( String lid )
    {
        List slopeProfileList = new ArrayList();
        List slopeProfileRecordList = null;
        SlopeprofileRecord record = null;
        SlopeprofileTable table = new SlopeprofileTable( _raxDb );
        String where = " WHERE lid = '" + lid + "' ";
        try
        {
            slopeProfileRecordList = table.select( where );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        for ( int i = 0; i < slopeProfileRecordList.size(); i++ )
        {
            record = (SlopeprofileRecord) slopeProfileRecordList.get( i );
            slopeProfileList.add( new SlopeProfile( record ) );
        }
        
        return slopeProfileList;
    }
    
// ------------------------------------ INGEST FILTER ---------------------------------

    private List getRowDataListFromRaxIngestFilterList()
    {
        List raxIngestFilterRowDataList = new ArrayList();
        ArcBaseIngestFilterJTableRowData rowData = new ArcBaseIngestFilterJTableRowData();
        RaxIngestFilter raxIngestFilter = null;
        
        for ( int i = 0; i < _raxIngestFilterList.size(); i++ )
        {
            raxIngestFilter = (RaxIngestFilter) _raxIngestFilterList.get( i );
            rowData = new ArcBaseIngestFilterJTableRowData();
  
            rowData.setRaxIngestFilter( raxIngestFilter );

            rowData.addAllCellsToMap();

            raxIngestFilterRowDataList.add( rowData );
            _raxIngestFilterRowDataToRaxIngestFilterMap.put( rowData, raxIngestFilter );
        }

        return raxIngestFilterRowDataList;
    }
    
    private List getRaxIngestFilterRecordsList()
    {
        List raxIngestFilterRecordsList = null;
        RaxIngestFilterTable table = new RaxIngestFilterTable( _raxDb );
        
        _logger.log( "Starting to read IngestFilterRecords" );
        try
        {
//            raxIngestFilterRecordsList = table.select( " WHERE lid > 'A' AND lid <= 'C' ORDER BY lid " );
            raxIngestFilterRecordsList = table.select( " ORDER BY lid " );
        }
        catch ( SQLException e )
        {
        }
        
        System.gc();

        _logger.log( "Finished reading " + raxIngestFilterRecordsList.size() + " records" );
        
        return raxIngestFilterRecordsList;
    }
    
    private List getRaxIngestFilterListFromRaxIngestFilterRecordsList( List raxIngestFilterRecordsList )
    {
        List raxIngestFilterList = new ArrayList();
        RaxIngestFilterRecord raxIngestFilterRecord = null;
        
        for ( int i = 0; i < raxIngestFilterRecordsList.size(); i++ )
        {
            raxIngestFilterRecord = (RaxIngestFilterRecord) raxIngestFilterRecordsList.get( i );
            
            raxIngestFilterList.add( new RaxIngestFilter( raxIngestFilterRecord ) );
        }
        
        return raxIngestFilterList;
    }
    
    
// -------------------------------- LOCATION -----------------------------    
    
    
    /**
     * Takes a List of RaxLocation objects and creates a list of ArcBaseLocationJTableRowData objects used by the
     * JTable in the main GUI
     * 
     * @param locationList - List of RaxLocation objects
     * @return - List of ArcBaseLocationJTableRowData objects
     */
    private List getRowDataListFromRaxLocationList()
    {
        List raxLocationRowDataList = new ArrayList();
        ArcBaseLocationJTableRowData rowData = new ArcBaseLocationJTableRowData();
        RaxLocation raxLocation = null;
        
        for ( int i = 0; i < _raxLocationList.size(); i++ )
        {
            raxLocation = (RaxLocation) _raxLocationList.get( i );
            rowData = new ArcBaseLocationJTableRowData();
            
            rowData.setRaxLocation( raxLocation );

            rowData.addAllCellsToMap();
            raxLocationRowDataList.add( rowData );
        }
        return raxLocationRowDataList;
    }
    
    /**
     * Takes a List of RaxLocationRecord objects retrieved from the archive database and converts it to a List
     * of RaxLocation objects.
     *  
     * @param raxLocationRecordsList - List of RaxLocationRecords
     * @return - List of RaxLocation objects
     */
    private List getRaxLocationListFromRaxLocationRecordsList( List raxLocationRecordsList )
    {
        List raxLocationList = new ArrayList();
        RaxLocationRecord raxLocationRecord = null;
        
        for ( int i = 0; i < raxLocationRecordsList.size(); i++ )
        {
            raxLocationRecord = (RaxLocationRecord) raxLocationRecordsList.get( i );
            
            raxLocationList.add( new RaxLocation( raxLocationRecord ) );
        }
        return raxLocationList;
    }


    public RaxLocation getRaxLocationFromJTableRowData( ArcBaseLocationJTableRowData rowData )
    {
        RaxLocation raxLocation = new RaxLocation();
        RaxLocation extractedRaxLocation = null;
        
        for ( int i = 0; i < _raxLocationList.size(); i++ )
        {
            extractedRaxLocation = (RaxLocation) _raxLocationList.get( i );
            
            if ( ( extractedRaxLocation.getLid().equalsIgnoreCase( rowData.getRaxLocation().getLid() ) ) &&
                 ( extractedRaxLocation.getBeginDate() == rowData.getRaxLocation().getBeginDate() ) )
            {
                raxLocation = extractedRaxLocation;
                break;
            }
        }
        return raxLocation;
    }

// ------------------------------------ DATA LIMITS --------------------------------    
    
    private List getRowDataListFromRaxDataLimitsList()
    {
        List raxDataLimitsRowDataList = new ArrayList();
        ArcBaseDataLimitsJTableRowData rowData = new ArcBaseDataLimitsJTableRowData();
        RaxDataLimits dataLimits = null;
        
        for ( int i = 0; i < _raxDataLimitsList.size(); i++ )
        {
            dataLimits = (RaxDataLimits) _raxDataLimitsList.get( i );
            rowData = new ArcBaseDataLimitsJTableRowData();
            
            rowData.setLid( "" );
            rowData.setPe( dataLimits.getPe() );
            rowData.setDuration( dataLimits.getDur() + "/" + dataLimits.getIdur() );
            rowData.setMonthDayStart( dataLimits.getMonthDayStart() );
            rowData.setMonthDayEnd( dataLimits.getMonthDayEnd() );
            rowData.setGrossRangeMin( dataLimits.getGrossRangeMin() );
            rowData.setGrossRangeMax( dataLimits.getGrossRangeMax() );
            rowData.setReasonRangeMin( dataLimits.getReasonRangeMin() );
            rowData.setReasonRangeMax( dataLimits.getReasonRangeMax() );
            rowData.setRocMax( dataLimits.getRoc() );
            rowData.setAlertLimit( dataLimits.getAlertLimit() );
            rowData.setAlertRocLimit( dataLimits.getAlertRocLimit() );
            rowData.setAlarmLimit( dataLimits.getAlarmLimit() );
            rowData.setAlarmRocLimit( dataLimits.getAlarmRocLimit() );
            
            rowData.addAllCellsToMap();
            raxDataLimitsRowDataList.add( rowData );
            _raxDataLimitsRowDataToRaxDataLimitsMap.put( rowData, dataLimits );
        }
        
        return raxDataLimitsRowDataList;
    }
    
    private List getRowDataListFromRaxAdjustFactorList()
    {
        List raxAdjustFactorRowDataList = new ArrayList();
        ArcBaseAdjustFactorJTableRowData rowData = new ArcBaseAdjustFactorJTableRowData();
        RaxAdjustFactor raxAdjustFactor = null;
        
        for ( int i = 0; i < _raxAdjustFactorList.size(); i++ )
        {
            raxAdjustFactor = (RaxAdjustFactor) _raxAdjustFactorList.get( i );
            rowData = new ArcBaseAdjustFactorJTableRowData();

            rowData.setRaxAdjustFactor( raxAdjustFactor );

            rowData.addAllCellsToMap();
            raxAdjustFactorRowDataList.add( rowData );
            _raxAdjustFactorRowDataToRaxAdjustFactorMap.put( rowData, raxAdjustFactor );
        }
        
        return raxAdjustFactorRowDataList;
    }
    
    private List getRowDataFromCountiesList()
    {
        List countiesRowDataList = new ArrayList();
        ArcBaseCountiesJTableRowData rowData = new ArcBaseCountiesJTableRowData();
        Counties counties = null;
        
        for ( int i = 0; i < _countiesList.size(); i++ )
        {
            counties = (Counties) _countiesList.get( i );
            rowData = new ArcBaseCountiesJTableRowData();
            
            rowData.setCounty( counties.getCounty() );
            rowData.setCountryFips( counties.getCountryFips() );
            rowData.setCountyFips( counties.getCountyFips() );
            rowData.setState( counties.getState() );
            rowData.setWfo( counties.getWfo() );
            rowData.setZon( counties.getZon() );
            
            rowData.addAllCellsToMap();
            countiesRowDataList.add( rowData );
            _countiesRowDataToCountiesMap.put( rowData, counties );
        }
        
        return countiesRowDataList;
    }
    
    private List getRowDataFromStateList()
    {
        List stateRowDataList = new ArrayList();
        ArcBaseStateJTableRowData rowData = new ArcBaseStateJTableRowData();
        State state = null;
        
        for ( int i = 0; i < _stateList.size(); i++ )
        {
            state = (State) _stateList.get( i );
            rowData = new ArcBaseStateJTableRowData();
            
            rowData.setState( state.getState() );
            rowData.setCountryFips( state.getCountryFips() );
            rowData.setName( state.getName() );
            rowData.setNcdc( state.getNcdc() );
            rowData.setStateFips( state.getStateFips() );
            
            rowData.addAllCellsToMap();
            stateRowDataList.add( rowData );
            _stateRowDataToStateMap.put( rowData, state );
        }
        
        return stateRowDataList;
    }
    
    private List getRowDataFromProdList()
    {
        List prodRowDataList = new ArrayList();
        ArcBaseProdJTableRowData rowData = new ArcBaseProdJTableRowData();
        Prod prod = null;
        
        for ( int i = 0; i < _prodList.size(); i++ )
        {
            prod = (Prod) _prodList.get( i );
            rowData = new ArcBaseProdJTableRowData();
            
            rowData.setId( prod.getId() );
            rowData.setPmax( prod.getPmax() );
            rowData.setPap( prod.getPap() );
            rowData.setErr( prod.getErr() );
            rowData.setGra( prod.getGra() );
            rowData.setPr1( prod.getPr1() );
            rowData.setNet( prod.getNet() );
            
            rowData.addAllCellsToMap();
            prodRowDataList.add( rowData );
            _prodRowDataToProdMap.put( rowData, prod );
        }
        return prodRowDataList;
    }
    
    private List getRowDataFromHuc8List()
    {
        List huc8RowDataList = new ArrayList();
        ArcBaseHuc8JTableRowData rowData = new ArcBaseHuc8JTableRowData();
        Huc8 huc8 = null;
        
        for ( int i = 0; i < _huc8List.size(); i++ )
        {
            huc8 = (Huc8) _huc8List.get( i );
            rowData = new ArcBaseHuc8JTableRowData();
            
            rowData.setCode12( huc8.getCode12() );
            rowData.setCode34( huc8.getCode34() );
            rowData.setCode56( huc8.getCode56() );
            rowData.setCode78( huc8.getCode78() );
            rowData.setCode8( huc8.getCode8() );
            rowData.setDescat( huc8.getDescat() );
            
            rowData.addAllCellsToMap();
            huc8RowDataList.add( rowData );
            _huc8RowDataToHuc8Map.put( rowData, huc8 );
        }
        
        return huc8RowDataList;
    }
    
    private List getRowDataFromHuc6List()
    {
        List huc6RowDataList = new ArrayList();
        ArcBaseHuc6JTableRowData rowData = new ArcBaseHuc6JTableRowData();
        Huc6 huc6 = null;
        
        for ( int i = 0; i < _huc6List.size(); i++ )
        {
            huc6 = (Huc6) _huc6List.get( i );
            rowData = new ArcBaseHuc6JTableRowData();
            
            rowData.setCode12( huc6.getCode12() );
            rowData.setCode34( huc6.getCode34() );
            rowData.setCode56( huc6.getCode56() );
            rowData.setCode6( huc6.getCode6() );
            rowData.setDesacct( huc6.getDesacct() );
            
            rowData.addAllCellsToMap();
            huc6RowDataList.add( rowData );
            _huc6RowDataToHuc6Map.put( rowData, huc6 );
        }
        
        return huc6RowDataList;
    }

    private List getRowDataFromShefQCList()
    {
        List shefQCRowDataList = new ArrayList();
        ArcBaseShefQCJTableRowData rowData = new ArcBaseShefQCJTableRowData();
        ShefQC shefQC = null;
        
        for ( int i = 0; i < _shefQCList.size(); i++ )
        {
            shefQC = (ShefQC) _shefQCList.get( i );
            rowData = new ArcBaseShefQCJTableRowData();
            
            rowData.setShefQualifierCode( shefQC.getShefQualifierCode() );
            rowData.setPower( shefQC.getPower() );
            rowData.setName( shefQC.getName() );
            
            rowData.addAllCellsToMap();
            shefQCRowDataList.add( rowData );
            _shefQCRowDataToShefQCMap.put( rowData, shefQC );
        }
        
        return shefQCRowDataList;
    }
   
    private List getRowDataFromAgencyList()
    {
        List agencyRowDataList = new ArrayList();
        ArcBaseAgencyJTableRowData rowData = new ArcBaseAgencyJTableRowData();
        Agency agency = null;
        
        for ( int i = 0; i < _agencyList.size(); i++ )
        {
            agency = (Agency) _agencyList.get( i );
            rowData = new ArcBaseAgencyJTableRowData();
            
            rowData.setAgCode( agency.getAgCode() );
            rowData.setAgLoc( agency.getAgLoc() );
            rowData.setDes( agency.getDes() );
            
            rowData.addAllCellsToMap();
            agencyRowDataList.add( rowData );
            _agencyRowDataToAgencyMap.put( rowData, agency );
        }
        
        return agencyRowDataList;
    }
    
    private List getRowDataFromShefPE1List()
    {
        List shefPe1RowDataList = new ArrayList();
        ArcBaseShefPE1JTableRowData rowData = new ArcBaseShefPE1JTableRowData();
        ShefPE1 shefPe1 = null;
        
        for ( int i = 0; i < _shefPE1List.size(); i++ )
        {
            shefPe1 = (ShefPE1) _shefPE1List.get( i );
            rowData = new ArcBaseShefPE1JTableRowData();
            
            rowData.setPe1( shefPe1.getPe1() );
            rowData.setName( shefPe1.getName() );
            
            rowData.addAllCellsToMap();
            shefPe1RowDataList.add( rowData );
            _shefPe1RowDataToShefPe1Map.put( rowData, shefPe1 );
        }
        
        return shefPe1RowDataList;
    }
    
    private List getRowDataFromHuc4List()
    {
        List huc4RowDataList = new ArrayList();
        ArcBaseHuc4JTableRowData rowData = new ArcBaseHuc4JTableRowData();
        Huc4 huc4 = null;
        
        for ( int i = 0; i < _huc4List.size(); i++ )
        {
            huc4 = (Huc4) _huc4List.get( i );
            rowData = new ArcBaseHuc4JTableRowData();
            
            rowData.setCode12( huc4.getCode12() );
            rowData.setCode34( huc4.getCode34() );
            rowData.setCode4( huc4.getCode4() );
            rowData.setDesreg( huc4.getDessubreg() );
            
            rowData.addAllCellsToMap();
            huc4RowDataList.add( rowData );
            _huc4RowDataToHuc4Map.put( rowData, huc4 );
        }
        
        return huc4RowDataList;
    }

    private List getRowDataFromRfcList()
    {
        List rfcRowDataList = new ArrayList();
        ArcBaseRfcJTableRowData rowData = new ArcBaseRfcJTableRowData();
        Rfc rfc = null;
        
        for ( int i = 0; i < _rfcList.size(); i++ )
        {
            rfc = (Rfc) _rfcList.get( i );
            rowData = new ArcBaseRfcJTableRowData();
            
            rowData.setRfc( rfc.getRfc() );
            
            rowData.addAllCellsToMap();
            rfcRowDataList.add( rowData );
            _rfcRowDataToRfcMap.put( rowData, rfc );
        }
        
        return rfcRowDataList;
    }
    
    private List getRowDataFromWfoHsaList()
    {
        List wfoHsaRowDataList = new ArrayList();
        ArcBaseWfoHsaJTableRowData rowData = new ArcBaseWfoHsaJTableRowData();
        WfoHsa wfoHsa = null;
        
        for ( int i = 0; i < _wfoHsaList.size(); i++ )
        {
            wfoHsa = (WfoHsa) _wfoHsaList.get( i );
            rowData = new ArcBaseWfoHsaJTableRowData();
            
            rowData.setWfoHsa( wfoHsa.getWfoHsa() );
            
            rowData.addAllCellsToMap();
            wfoHsaRowDataList.add( rowData );
            _wfoHsaRowDataToWfoHsaMap.put( rowData, wfoHsa );
        }

        return wfoHsaRowDataList;
    }
    
    private List getRowDataFromShefTsList()
    {
        List shefTsRowDataList = new ArrayList();
        ArcBaseShefTSJTableRowData rowData = new ArcBaseShefTSJTableRowData();
        ShefTS shefTS = null;
        
        for ( int i = 0; i < _shefTSList.size(); i++ )
        {
            shefTS = (ShefTS) _shefTSList.get( i );
            rowData = new ArcBaseShefTSJTableRowData();
            
            rowData.setTS( shefTS.getTs() );
            rowData.setName( shefTS.getName() );
            
            rowData.addAllCellsToMap();
            shefTsRowDataList.add( rowData );
            _shefTsRowDataToShefTsMap.put( rowData, shefTS );
        }
        
        return shefTsRowDataList;
    }
    
    private List getRowDataFromShefExList()
    {
        List shefExRowDataList = new ArrayList();
        ArcBaseShefExtremumJTableRowData rowData = new ArcBaseShefExtremumJTableRowData();
        ShefExtremum shefEx = null;
        
        for ( int i = 0; i < _shefExtremumList.size(); i++ )
        {
            shefEx = (ShefExtremum) _shefExtremumList.get( i );
            rowData = new ArcBaseShefExtremumJTableRowData();
            
            rowData.setExtremum( shefEx.getExtremum() );
            rowData.setName( shefEx.getName() );
            
            rowData.addAllCellsToMap();
            shefExRowDataList.add( rowData );
            _shefExRowDataToShefExMap.put( rowData, shefEx );
        }
        
        return shefExRowDataList;
    }

    private List getRowDataFromShefProbList()
    {
        List shefProbRowDataList = new ArrayList();
        ArcBaseShefProbJTableRowData rowData = new ArcBaseShefProbJTableRowData();
        ShefProb shefProb = null;
        
        for ( int i = 0; i < _shefProbList.size(); i++ )
        {
            shefProb = (ShefProb) _shefProbList.get( i );
            rowData = new ArcBaseShefProbJTableRowData();
            
            rowData.setP( shefProb.getP() );
            rowData.setProbability( shefProb.getProbability() );
            rowData.setName( shefProb.getName() );
            
            rowData.addAllCellsToMap();
            shefProbRowDataList.add( rowData );
            _shefProbRowDataToShefProbMap.put( rowData, shefProb );
        }
        
        return shefProbRowDataList;
    }
    
    private List getRowDataFromShefPEList()
    {
        List shefPeRowDataList = new ArrayList();
        ArcBaseShefPEJTableRowData rowData = new ArcBaseShefPEJTableRowData();
        ShefPE shefPe = null;
        
        for( int i = 0; i < _shefPeList.size(); i++ )
        {
            shefPe = (ShefPE) _shefPeList.get( i );
            rowData = new ArcBaseShefPEJTableRowData();
            
            rowData.setPe( shefPe.getPe() );
            rowData.setName( shefPe.getName() );
            rowData.setEngUnit( shefPe.getEngUnit() );
            rowData.setMetUnit( shefPe.getMetUnit() );
            
            rowData.addAllCellsToMap();
            shefPeRowDataList.add( rowData );
            _shefPeRowDataToShefPeMap.put( rowData, shefPe );
        }
        
        return shefPeRowDataList;
    }
    
    private List getRowDataFromShefDurationList()
    {
        List shefDurRowDataList = new ArrayList();
        ArcBaseShefDurationJTableRowData rowData = new ArcBaseShefDurationJTableRowData();
        ShefDuration shefDur = null;
        
        for ( int i = 0; i < _shefDurationList.size(); i++ )
        {
            shefDur = (ShefDuration) _shefDurationList.get( i );
            rowData = new ArcBaseShefDurationJTableRowData();
            
            rowData.setDuration( shefDur.getDuration() );
            rowData.setIDuration( shefDur.getIduration() );
            rowData.setName( shefDur.getName() );
            
            rowData.addAllCellsToMap();
            shefDurRowDataList.add( rowData );
            _shefDurRowDataToShefDurMap.put( rowData, shefDur );
        }
        
        return shefDurRowDataList;
    }
    
    private List getRowDataFromShefPETransList()
    {
        List shefPETransRowDataList = new ArrayList();
        ArcBaseShefPETransJTableRowData rowData = new ArcBaseShefPETransJTableRowData();
        ShefPETrans shefPETrans = null;
        
        for ( int i = 0; i < _shefPETransList.size(); i++ )
        {
            shefPETrans = (ShefPETrans) _shefPETransList.get( i );
            rowData = new ArcBaseShefPETransJTableRowData();
            
            rowData.setPe( shefPETrans.getPe() );
            rowData.setCodePosition( shefPETrans.getCodePosition() );
            rowData.setCodedValue( shefPETrans.getCodedValue() );
            
            rowData.addAllCellsToMap();
            shefPETransRowDataList.add( rowData );
            _shefPeTransRowDataToShefPeTransMap.put( rowData, shefPETrans );
        }
        
        return shefPETransRowDataList;
    }
    
    private List getRowDataFromHuc2List()
    {
        List huc2RowDataList = new ArrayList();
        ArcBaseHuc2JTableRowData rowData = new ArcBaseHuc2JTableRowData();
        Huc2 huc2 = null;
        
        for ( int i = 0; i < _huc2List.size(); i++ )
        {
            huc2 = (Huc2) _huc2List.get( i );
            rowData = new ArcBaseHuc2JTableRowData();
            
            rowData.setCode12( huc2.getCode12() );
            rowData.setCode2( huc2.getCode2() );
            rowData.setDesreg( huc2.getDesreg() );
            
            rowData.addAllCellsToMap();
            huc2RowDataList.add( rowData );
            _huc2RowDataToHuc2Map.put( rowData, huc2 );
        }
        
        return huc2RowDataList;
    }
    
    private List getRowDataFromCountryList()
    {
        List countryRowDataList = new ArrayList();
        ArcBaseCountryJTableRowData rowData = new ArcBaseCountryJTableRowData();
        Country country = null;
        
        for ( int i = 0; i < _countryList.size(); i++ )
        {
            country = (Country) _countryList.get( i );
            rowData = new ArcBaseCountryJTableRowData();
            
            rowData.setCountry( country.getCountry() );
            rowData.setCountryFips( country.getCountryFips() );
            
            rowData.addAllCellsToMap();
            countryRowDataList.add( rowData );
            _countryRowDataToCountryMap.put( rowData, country );
        }
        
        return countryRowDataList;
    }
    
    private List getRowDataListFromModCtrlList()
    {
        List modCtrlRowDataList = new ArrayList();
        ArcBaseModCtrlJTableRowData rowData = new ArcBaseModCtrlJTableRowData();
        ModCtrl modCtrl = null;
        
        for ( int i = 0; i < _modCtrlList.size(); i++ )
        {
            modCtrl = (ModCtrl) _modCtrlList.get( i );
            rowData = new ArcBaseModCtrlJTableRowData();
            
            rowData.setModName( modCtrl.getModName() );
            rowData.setLoad( modCtrl.isLoad() );
            rowData.setFetchOper( modCtrl.isFetchOper() );
            rowData.setFetchSpin( modCtrl.isFetchSpin() );
            
            rowData.addAllCellsToMap();
            modCtrlRowDataList.add( rowData );
            getModCtrlRowDataToModCtrlMap().put( rowData, modCtrl );
        }
        
        return modCtrlRowDataList;
    }
    
    private List getRowDataListFromRaxCrestList()
    {
        List raxCrestRowDataList = new ArrayList();
        ArcBaseCrestJTableRowData rowData = new ArcBaseCrestJTableRowData();
        RaxCrest raxCrest = null;
        
        for ( int i = 0; i < _raxCrestList.size(); i++ )
        {
            raxCrest = (RaxCrest) _raxCrestList.get( i );
            rowData = new ArcBaseCrestJTableRowData();
            
            rowData.setDate( raxCrest.getDateCrest() );
            rowData.setFlow( raxCrest.getFlow() );
            rowData.setStage( raxCrest.getStage() );
            rowData.setTime( raxCrest.getCrestDateTime() );
            
            rowData.addAllCellsToMap();
            raxCrestRowDataList.add( rowData );
            getRaxCrestRowDataToRaxCrestMap().put( rowData, raxCrest );
        }
        
        return raxCrestRowDataList;
    }
    
    private List getRatingCurveRowDataListFromRaxRating( List raxRatingPointList )
    {
        List raxRatingRowDataList = new ArrayList();
        ArcBaseRatingJTableRowData rowData = new ArcBaseRatingJTableRowData();
        RaxRatingPoint raxRatingPoint = null;
        
        for ( int i = 0; i < raxRatingPointList.size(); i++ )
        {
            raxRatingPoint = (RaxRatingPoint) raxRatingPointList.get( i );
            rowData = new ArcBaseRatingJTableRowData();
            
            rowData.setStage( raxRatingPoint.getStage() );
            rowData.setDischarge( raxRatingPoint.getDischarge() );
            rowData.setShiftedStage( 0 );
            rowData.addAllCellsToMap();
            raxRatingRowDataList.add( rowData );
        }
        
        return raxRatingRowDataList;
    }
    
    private List getRatingOffsetsRowDataListFromRaxRating( List raxRatingOffsetsList )
    {
        List ratingOffsetsRowDataList = new ArrayList();
        ArcBaseRatingOffsetsJTableRowData rowData = new ArcBaseRatingOffsetsJTableRowData();
        RaxRatingOffset raxRatingOffset = null;
        
        for ( int i = 0; i < raxRatingOffsetsList.size(); i++ )
        {
            raxRatingOffset = (RaxRatingOffset) raxRatingOffsetsList.get( i );
            rowData = new ArcBaseRatingOffsetsJTableRowData();
            
            rowData.setStage( raxRatingOffset.getStage() );
            rowData.setOffset( raxRatingOffset.getOffset() );
            rowData.addAllCellsToMap();
            ratingOffsetsRowDataList.add( rowData );
        }
        
        return ratingOffsetsRowDataList;
    }

    private List getRowDataListFromRaxLocDataLimitsList()
    {
        List raxLocDataLimitsRowDataList = new ArrayList();
        ArcBaseDataLimitsJTableRowData rowData = new ArcBaseDataLimitsJTableRowData();
        RaxLocDataLimits locDataLimits = null;

        for ( int i = 0; i < _raxLocDataLimitsList.size(); i++ )
        {
            locDataLimits = (RaxLocDataLimits) _raxLocDataLimitsList.get( i );
            rowData = new ArcBaseDataLimitsJTableRowData();
            
            rowData.setLid( locDataLimits.getLid() );
            rowData.setPe( locDataLimits.getPe() );
            rowData.setDuration( locDataLimits.getDur() + "/" + locDataLimits.getIdur() );
            rowData.setMonthDayStart( locDataLimits.getMonthDayStart() );
            rowData.setMonthDayEnd( locDataLimits.getMonthDayEnd() );
            rowData.setGrossRangeMin( locDataLimits.getGrossRangeMin() );
            rowData.setGrossRangeMax( locDataLimits.getGrossRangeMax() );
            rowData.setReasonRangeMin( locDataLimits.getReasonRangeMin() );
            rowData.setReasonRangeMax( locDataLimits.getReasonRangeMax() );
            rowData.setRocMax( locDataLimits.getRoc() );
            rowData.setAlertLimit( locDataLimits.getAlertLimit() );
            rowData.setAlertRocLimit( locDataLimits.getAlertRocLimit() );
            rowData.setAlarmLimit( locDataLimits.getAlarmLimit() );
            rowData.setAlarmRocLimit( locDataLimits.getAlarmRocLimit() );
            
            rowData.addAllCellsToMap();
            raxLocDataLimitsRowDataList.add( rowData );
            _raxLocDataLimitsRowDataToRaxLocDataLimitsMap.put( rowData, locDataLimits );
        }
        
        return raxLocDataLimitsRowDataList;

    }
    

    private List getRaxDataLimitsListFromRaxDataLimitsRecordsList( List raxDataLimitsRecordsList )
    {
        List raxDataLimitsList = new ArrayList();
        RaxDataLimitsRecord record = null;
        
        for ( int i = 0; i < raxDataLimitsRecordsList.size(); i++ )
        {
            record = (RaxDataLimitsRecord) raxDataLimitsRecordsList.get( i );
            
            raxDataLimitsList.add( getRaxDataLimitsFromRaxDataLimitsRecord( record ) );
        }
        
        return raxDataLimitsList;
    }
    
    private List getCountiesListFromCountiesRecordsList( List countiesRecordsList )
    {
        List countiesList = new ArrayList();
        CountiesRecord record = null;
        
        for ( int i = 0; i < countiesRecordsList.size(); i++ )
        {
            record = (CountiesRecord) countiesRecordsList.get( i );
            
            countiesList.add( new Counties( record ) );
        }
        
        return countiesList;
    }
    
    private List getStateListFromStateRecordsList( List stateRecordsList )
    {
        List stateList = new ArrayList();
        StateRecord record = null;
        
        for ( int i = 0; i < stateRecordsList.size(); i++ )
        {
            record = (StateRecord) stateRecordsList.get( i );
            
            stateList.add( new State( record ) );
        }
        
        return stateList;
    }
    
    private List getProdListFromProdRecordsList( List prodRecordsList )
    {
        List prodList = new ArrayList();
        ProdRecord record = null;
        
        for ( int i = 0; i < prodRecordsList.size(); i++ )
        {
            record = (ProdRecord) prodRecordsList.get( i );
            
            prodList.add( new Prod( record ) );
        }
        
        return prodList;
    }
    
    private List getHuc8ListFromHuc8RecordsList( List huc8RecordsList )
    {
        List huc8List = new ArrayList();
        Huc8Record record = null;
        
        for ( int i = 0; i < huc8RecordsList.size(); i++ )
        {
            record = (Huc8Record) huc8RecordsList.get( i );
            
            huc8List.add( new Huc8( record ) );
        }
        
        return huc8List;
    }

    private List getHuc6ListFromHuc6RecordsList( List huc6RecordsList )
    {
        List huc6List = new ArrayList();
        Huc6Record record = null;
        
        for ( int i = 0; i < huc6RecordsList.size(); i++ )
        {
            record = (Huc6Record) huc6RecordsList.get( i );
            
            huc6List.add( new Huc6( record ) );
        }
        
        return huc6List;
    }
    
    private List getShefQCListFromShefQCRecordsList( List shefQCRecordsList )
    {
        List shefQCList = new ArrayList();
        ShefqcRecord record = null;
        
        for ( int i = 0; i < shefQCRecordsList.size(); i++ )
        {
            record = (ShefqcRecord) shefQCRecordsList.get( i );
            
            shefQCList.add( new ShefQC( record ) );
        }
        
        return shefQCList;
    }
    
    private List getAgencyListFromAgencyRecordsList( List agencyRecordsList )
    {
        List agencyList = new ArrayList();
        AgencyRecord record = null;
        
        for ( int i = 0; i < agencyRecordsList.size(); i++ )
        {
            record = (AgencyRecord) agencyRecordsList.get( i );
            
            agencyList.add( new Agency( record ) );
        }
        
        return agencyList;
    }
    
    private List getShefPE1ListFromShefPE1RecordsList( List shefPe1RecordsList )
    {
        List shefPe1List = new ArrayList();
        Shefpe1Record record = null;
        
        for ( int i = 0; i < shefPe1RecordsList.size(); i++ )
        {
            record = (Shefpe1Record) shefPe1RecordsList.get( i );
            
            shefPe1List.add( new ShefPE1( record ) );
        }
        
        return shefPe1List;
    }
    
    private List getHuc4ListFromHuc4RecordsList( List huc4RecordsList )
    {
        List huc4List = new ArrayList();
        Huc4Record record = null;
        
        for ( int i = 0; i < huc4RecordsList.size(); i++ )
        {
            record = (Huc4Record) huc4RecordsList.get( i );
            
            huc4List.add( new Huc4( record ) );
        }
        
        return huc4List;
    }
    
    private List getRfcListFromRfcRecordsList( List rfcRecordsList )
    {
        List rfcList = new ArrayList();
        RfcRecord record = null;
        
        for ( int i = 0; i < rfcRecordsList.size(); i++ )
        {
            record = (RfcRecord) rfcRecordsList.get( i );
            
            rfcList.add( new Rfc( record ) );
        }
        
        return rfcList;
    }
    
    private List getWfoHsaListFromWfoHsaRecordsList( List wfoHsaRecordsList )
    {
        List wfoHsaList = new ArrayList();
        Wfo_hsaRecord record = null;
        
        for ( int i = 0; i < wfoHsaRecordsList.size(); i++ )
        {
            record = (Wfo_hsaRecord) wfoHsaRecordsList.get( i );
            
            wfoHsaList.add( new WfoHsa( record ) );
        }
        
        return wfoHsaList; 
    }
    
    private List getShefPETransListFromShefPETransRecordsList( List shefPETransRecordsList )
    {
        List shefPETransList = new ArrayList();
        ShefpetransRecord record = null;
        
        for ( int i = 0; i < shefPETransRecordsList.size(); i++ )
        {
            record = (ShefpetransRecord) shefPETransRecordsList.get( i );
            
            shefPETransList.add( new ShefPETrans( record ) );
        }
        
        return shefPETransList;
    }
    
    private List getHuc2ListFromHuc2RecordsList( List huc2RecordsList )
    {
        List huc2List = new ArrayList();
        Huc2Record record = null;
        
        for ( int i = 0; i < huc2RecordsList.size(); i++ )
        {
            record = (Huc2Record) huc2RecordsList.get( i );
            
            huc2List.add( new Huc2( record ) );
        }
        
        return huc2List;
    }
    
    private List getCountryListFromCountryRecordsList( List countryRecordsList )
    {
        List countryList = new ArrayList();
        CountryRecord record = null;
        
        for ( int i = 0; i < countryRecordsList.size(); i++ )
        {
            record = (CountryRecord) countryRecordsList.get( i );
            
            countryList.add( new Country( record ) );
        }
        
        return countryList;
    }
    
    private List getModCtrlListFromModCtrlRecordsList( List modCtrlRecordsList )
    {
        List modCtrlList = new ArrayList();
        ModctrlRecord record = null;
        
        for ( int i = 0; i < modCtrlRecordsList.size(); i++ )
        {
            record = (ModctrlRecord) modCtrlRecordsList.get( i );
            
            modCtrlList.add( new ModCtrl( record ) );
        }

        return modCtrlList;
    }

    private List getRaxCrestListFromRaxCrestRecordsList( List raxCrestRecordsList )
    {
        List raxCrestList = new ArrayList();
        RaxCrestRecord record = null;
        
        for ( int i = 0; i < raxCrestRecordsList.size(); i++ )
        {
            record = (RaxCrestRecord) raxCrestRecordsList.get( i );
            
            raxCrestList.add( new RaxCrest( record ) );
        }
        return raxCrestList;
    }
    
    private List getRaxRatingListFromRaxRatingRecordsList( List raxRatingRecordsList )
    {
        List raxRatingList = new ArrayList();
        RaxRatingRecord record = null;
        
        for ( int i = 0; i < raxRatingRecordsList.size(); i++ )
        {
            record = (RaxRatingRecord) raxRatingRecordsList.get( i );
            
            raxRatingList.add( new RaxRating( record ) );
        }
        
        return raxRatingList;
    }
    
    private List getRaxRatingShiftListFromRaxRatingShiftRecordsList( List raxRatingShiftRecordsList )
    {
        List raxRatingShiftList = new ArrayList();
        RaxRatingShiftRecord record = null;
        
        for ( int i = 0; i < raxRatingShiftRecordsList.size(); i++ )
        {
            record = (RaxRatingShiftRecord) raxRatingShiftRecordsList.get( i );
            
            raxRatingShiftList.add( new RaxRatingShift( record ) );
        }
        
        return raxRatingShiftList;
    }
    
    private List getRaxAdjustFactorListFromRaxAdjustFactorRecordsList( List raxAdjustFactorRecordsList )
    {
        List raxAdjustFactorsList = new ArrayList();
        RaxAdjustFactorRecord record = null;
        
        for ( int i = 0; i < raxAdjustFactorRecordsList.size(); i++ )
        {
            record = (RaxAdjustFactorRecord) raxAdjustFactorRecordsList.get( i );
            
            raxAdjustFactorsList.add( new RaxAdjustFactor( record ) );
        }
        
        return raxAdjustFactorsList;
    }
    
    private RaxDataLimits getRaxDataLimitsFromRaxDataLimitsRecord( RaxDataLimitsRecord record )
    {
        RaxDataLimits raxDataLimits = new RaxDataLimits( record );
        return raxDataLimits;
    }
    
    private List getRaxLocDataLimitsListFromRaxLocDataLimitsRecordsList( List raxLocDataLimitsRecordsList )
    {
        List raxLocDataLimitsList = new ArrayList();
        RaxLocDataLimitsRecord record = null;
        
        for ( int i = 0; i < raxLocDataLimitsRecordsList.size(); i++ )
        {
            record = (RaxLocDataLimitsRecord) raxLocDataLimitsRecordsList.get( i );
            
            raxLocDataLimitsList.add( new RaxLocDataLimits( record ) );
        }
        
        return raxLocDataLimitsList;
    }
    
    private List getRaxRiverCritListFromRaxRiverCritRecordsList( List raxRiverCritRecordsList )
    {
        List raxRiverCritList = new ArrayList();
        RaxRiverCritRecord record = null;
        
        for ( int i = 0; i < raxRiverCritRecordsList.size(); i++ )
        {
            record = (RaxRiverCritRecord) raxRiverCritRecordsList.get( i );
            raxRiverCritList.add( RaxRiverCrit.getRaxRiverCrit( record ) );
        }
        return raxRiverCritList;
    }
    
    public List createIngestFilterRowDataList()
    {
        CodeTimer timer1 = new CodeTimer();
        timer1.start();
        List raxIngestFilterRecordsList = null;
        List raxIngestFilterRowDataList = null;
        
        raxIngestFilterRecordsList = getRaxIngestFilterRecordsList();
        _raxIngestFilterList = getRaxIngestFilterListFromRaxIngestFilterRecordsList( raxIngestFilterRecordsList );
        
        raxIngestFilterRowDataList = getRowDataListFromRaxIngestFilterList(); 

        _logger.log( "Finished reading IngestFilter data in " + timer1.stop()  );
        return raxIngestFilterRowDataList;
    }

    
    public List getIngestFilterRowDataList()
    {
        return _raxIngestFilterRowDataList;
    }
    
    public void initShefExtremumList()
    {
        _shefExtremumList = new ArrayList();
        List shefExRecordList = new ArrayList();
        ShefexTable table = new ShefexTable( _raxDb );
        ShefexRecord record = null;
        ShefExtremum shefExtremum = null;
        
        try
        {
            shefExRecordList = table.select( " ORDER BY e " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        for ( int i = 0; i < shefExRecordList.size(); i++ )
        {
            record = (ShefexRecord) shefExRecordList.get( i );
            
            shefExtremum = new ShefExtremum();
            
            shefExtremum.setExtremum( record.getE() );
            shefExtremum.setName( record.getName() );
            
            _shefExtremumList.add( shefExtremum );
            _shefExtremumMap.put( shefExtremum.getExtremum(), shefExtremum );
            _shefExtremumMap.put( shefExtremum, shefExtremum.getExtremum() );
        }
    }
    
    public void initShefTSList()
    {
        _shefTSList = new ArrayList();
        List shefTSRecordList = new ArrayList();
        SheftsTable table = new SheftsTable( _raxDb );
        SheftsRecord record = null;
        ShefTS shefTS = null;
       
        try
        {
            shefTSRecordList = table.select( " ORDER BY t,s " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        for ( int i = 0; i < shefTSRecordList.size(); i++ )
        {
            record = (SheftsRecord) shefTSRecordList.get( i );
            
            shefTS = new ShefTS();
            
            shefTS.setTs( record.getT() + record.getS() );
            shefTS.setName( record.getName() );
            
            _shefTSList.add( shefTS );
            _shefTSMap.put( shefTS.getTs(), shefTS );
            _shefTSMap.put( shefTS, shefTS.getTs() );
        }
    }

    public void initShefProbList()
    {
        _shefProbList = new ArrayList();
        List shefProbRecordList = new ArrayList();
        ShefProbTable table = new ShefProbTable( _raxDb );
        ShefProbRecord record = null;
        ShefProb shefProb = null;
        
        try
        {
            shefProbRecordList = table.select( " ORDER BY p " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        for ( int i = 0; i < shefProbRecordList.size(); i++ )
        {
            record = (ShefProbRecord) shefProbRecordList.get( i );
            shefProb = new ShefProb();
            
            shefProb.setName( record.getName() );
            shefProb.setP( record.getProbcode() );
            shefProb.setProbability( Float.parseFloat( getFormattedDouble( record.getProbability() ) ) );
            
            _shefProbList.add( shefProb );
            _shefProbMap.put( shefProb.getP(), shefProb );
            _shefProbMap.put( shefProb, shefProb.getP() );
        }
    }
    
    public void initShefDurationList()
    {
        _shefDurationList = new ArrayList();
        List shefDurationRecordList = new ArrayList();
        ShefdurTable table = new ShefdurTable( _raxDb );
        ShefdurRecord record = null;
        ShefDuration shefDur = null;
        
        try
        {
            shefDurationRecordList = table.select( " ORDER BY idur " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        for ( int i = 0; i < shefDurationRecordList.size(); i++ )
        {
            record = (ShefdurRecord) shefDurationRecordList.get( i );
            
            shefDur = new ShefDuration( record );
            
            _shefDurationList.add( shefDur );
            _shefDurationMap.put( shefDur.getDuration(), shefDur );
            _shefDurationMap.put( shefDur, shefDur.getDuration() );
        }
    }
    
    
    public void initShefPeList()
    {
        _shefPeList = new ArrayList();
        List shefPeRecordList = new ArrayList();
        ShefpeTable table = new ShefpeTable( _raxDb );
        ShefpeRecord record = null;
        ShefPE shefPE = null;
        
        try
        {
            shefPeRecordList = table.select( " ORDER BY pe1, pe2 " );
            
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        for ( int i = 0; i < shefPeRecordList.size(); i++ )
        {
            record = (ShefpeRecord) shefPeRecordList.get( i );
            
            shefPE = new ShefPE();
            
            shefPE.setPe( record.getPe1() + record.getPe2() );
            shefPE.setName( record.getName() );
            shefPE.setEngUnit( record.getEng_unit() );
            shefPE.setMetUnit( record.getMet_unit() );
            
            _shefPeList.add( shefPE );
            _shefPEMap.put( shefPE.getPe(), shefPE );
            _shefPEMap.put( shefPE, shefPE.getPe() );
        }
    }
    
    /**
     * Retrieves a list of RaxLocation objects from the Archive database
     * 
     * @return NONE
     */
    private List getRaxLocationRecordsList()
    {
        List raxLocationRecordsList = null;
        RaxLocationTable table = new RaxLocationTable( _raxDb );
        
        try
        {
            raxLocationRecordsList = table.select( " ORDER BY lid " );
        }
        catch ( SQLException e )
        {
            
        }
        
        return raxLocationRecordsList;
    }
    
    private List getRaxDataLimitsRecordsList()
    {
        List raxDataLimitsRecordsList = null;
        RaxDataLimitsTable table = new RaxDataLimitsTable( _raxDb );
        
        try
        {
            raxDataLimitsRecordsList = table.select( " ORDER BY pe1,pe2 " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return raxDataLimitsRecordsList;
    }

    private List getRaxAdjustFactorRecordsList()
    {
        List raxAdjustFactorRecordsList = null;
        RaxAdjustFactorTable table = new RaxAdjustFactorTable( _raxDb );
        
        try
        {
            raxAdjustFactorRecordsList = table.select( "" );
        }
        catch (SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return raxAdjustFactorRecordsList;
    }
    
    private List getCountiesRecordsList()
    {
        List countiesRecordsList = null;
        CountiesTable table = new CountiesTable( _raxDb );
        
        try
        {
            countiesRecordsList = table.select( " ORDER BY county" );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    
        return countiesRecordsList;
    }
    
    private List getStateRecordsListOrderedByCountryState()
    {
        List stateRecordsList = null;
        StateTable table = new StateTable( _raxDb );
        
        try
        {
            stateRecordsList = table.select( "ORDER BY countryfips, state" );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return stateRecordsList;
    }
    
    private List getWfoHsaRecordsListOrderedByWfo()
    {
        List wfoHsaRecordsList = null;
        Wfo_hsaTable table = new Wfo_hsaTable( _raxDb );
        
        try
        {
            wfoHsaRecordsList = table.select( "ORDER BY wfo_hsa" );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }

        return wfoHsaRecordsList;
    }

    private List getStateRecordsList()
    {
        List stateRecordsList = null;
        StateTable table = new StateTable( _raxDb );
        
        try
        {
            stateRecordsList = table.select( "ORDER BY state" );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return stateRecordsList;
    }
  
    private List getRfcRecordsList()
    {
        List rfcRecordsList = null;
        RfcTable table = new RfcTable( _raxDb );
        
        try
        {
            rfcRecordsList = table.select( " ORDER BY rfc " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return rfcRecordsList;
    }
    
    private List getWfoHsaRecordsList()
    {
        List wfoHsaRecordsList = null;
        Wfo_hsaTable table = new Wfo_hsaTable( _raxDb );
        
        try
        {
            wfoHsaRecordsList = table.select( " ORDER BY wfo_hsa " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return wfoHsaRecordsList;
    }
    
    private List getShefPETransRecordsList()
    {
        List shefPETransRecordsList = null;
        ShefpetransTable table = new ShefpetransTable( _raxDb );
        
        try
        {
            shefPETransRecordsList = table.select( " ORDER BY pe1, pe2 " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return shefPETransRecordsList;
    }
    
    private List getHuc2RecordsList()
    {
        List huc2RecordsList = null;
        Huc2Table table = new Huc2Table( _raxDb );
        
        try
        {
            huc2RecordsList = table.select( " ORDER BY code12" );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }

        return huc2RecordsList;
    }
    
    private List getProdRecordsList()
    {
        List prodRecordsList = null;
        ProdTable table = new ProdTable( _raxDb );
        
        try
        {
            prodRecordsList = table.select( " ORDER BY id " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return prodRecordsList;
    }
    
    private List getHuc8RecordsList()
    {
        List huc8RecordsList = null;
        Huc8Table table = new Huc8Table( _raxDb );
        
        try
        {
            huc8RecordsList = table.select( " ORDER BY code12" );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }

        return huc8RecordsList;
    }

    private List getHuc6RecordsList()
    {
        List huc6RecordsList = null;
        Huc6Table table = new Huc6Table( _raxDb );
        
        try
        {
            huc6RecordsList = table.select( " ORDER BY code12" );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }

        return huc6RecordsList;
    }
    
    private List getShefQCRecordsList()
    {
        List shefQCRecordsList = null;
        ShefqcTable table = new ShefqcTable( _raxDb );
        
        try
        {
            shefQCRecordsList = table.select( " ORDER BY power " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return shefQCRecordsList;
    }
    
    private List getAgencyRecordsList()
    {
        List agencyRecordsList = null;
        AgencyTable table = new AgencyTable( _raxDb );
        
        try
        {
            agencyRecordsList = table.select( " ORDER BY agcode " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return agencyRecordsList;
    }
    
    private List getPE1RecordsList()
    {
        List shefPe1RecordsList = null;
        Shefpe1Table table = new Shefpe1Table( _raxDb );
        
        try
        {
            shefPe1RecordsList = table.select( " ORDER BY PE1 " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return shefPe1RecordsList;
    }

    private List getHuc4RecordsList()
    {
        List huc4RecordsList = null;
        Huc4Table table = new Huc4Table( _raxDb );
        
        try
        {
            huc4RecordsList = table.select( " ORDER BY code12" );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }

        return huc4RecordsList;
    }
    
    private List getCountryRecordsList()
    {
        List countryRecordsList = null;
        CountryTable table = new CountryTable( _raxDb );
        
        try
        {
            countryRecordsList = table.select( " ORDER BY country" );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return countryRecordsList;
    }

    private List getModCtrlRecordsList()
    {
        List modCtrlRecordsList = null;
        ModctrlTable table = new ModctrlTable( _raxDb );
        
        try
        {
            modCtrlRecordsList = table.select( " ORDER BY mod_name" );
        }
        catch( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return modCtrlRecordsList;
    }
    
    private List getRaxCrestRecordsList( String lid )
    {
        List raxCrestRecordsList = null;
        RaxCrestTable table = new RaxCrestTable( _raxDb );
        
        try
        {
            raxCrestRecordsList = table.select( " WHERE LID = '" + lid + "' " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return raxCrestRecordsList;
    }
    
    private List getRaxRatingRecordsList( String lid )
    {
        List raxRatingRecordsList = null;
        RaxRatingTable table = new RaxRatingTable( _raxDb );
        
        try
        {
            raxRatingRecordsList = table.select( " WHERE LID = '" + lid + "' " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return raxRatingRecordsList;
    }
    
    private List getRaxRatingShiftRecordsList( String lid )
    {
        List raxRatingShiftRecordsList = null;
        RaxRatingShiftTable table = new RaxRatingShiftTable( _raxDb );
        
        try
        {
            raxRatingShiftRecordsList = table.select( " WHERE LID = '" + lid + "' " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return raxRatingShiftRecordsList;
    }
    
    private List getRaxLocDataLimitsRecordsList()
    {
        List raxLocDataLimitsRecordsList = null;
        RaxLocDataLimitsTable table = new RaxLocDataLimitsTable( _raxDb );
        
        try 
        {
            raxLocDataLimitsRecordsList = table.select( " ORDER BY pe1, pe2, lid " );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return raxLocDataLimitsRecordsList;
    }
    
    private List getRaxRiverCritRecordsList( String lid )
    {
        List raxRiverCritRecordsList = null;
        RaxRiverCritTable table = new RaxRiverCritTable( _raxDb );
        
        try
        {
            raxRiverCritRecordsList = table.select( " WHERE LID='" + lid + "'" );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }

        return raxRiverCritRecordsList;
    }

    public List getShefPeList()
    {
        return _shefPeList;
    }

    public List getShefProbList()
    {
        return _shefProbList;
    }
    
    public List getShefDurationList()
    {
        return _shefDurationList;
    }

    public List getShefTSList()
    {
        return _shefTSList;
    }

    public List getShefExtremumList()
    {
        return _shefExtremumList;
    }
    public Map getRaxIngestFilterRowDataToRaxIngestFilterMap()
    {
        return _raxIngestFilterRowDataToRaxIngestFilterMap;
    }

    public Map getShefPEMap()
    {
        return _shefPEMap;
    }

    public Map getShefDurationMap()
    {
        return _shefDurationMap;
    }

    public Map getShefTSMap()
    {
        return _shefTSMap;
    }

    public Map getShefExtremumMap()
    {
        return _shefExtremumMap;
    }
    
    public Map getShefProbMap()
    {
        return _shefProbMap;
    }

    public Map getRaxDataLimitsRowDataToRaxDataLimitsMap()
    {
        return _raxDataLimitsRowDataToRaxDataLimitsMap;
    }

    public Map getRaxLocDataLimitsRowDataToRaxLocDataLimitsMap()
    {
        return _raxLocDataLimitsRowDataToRaxLocDataLimitsMap;
    }

    public Map getRaxAdjustFactorRowDataToRaxAdjustFactorMap()
    {
        return _raxAdjustFactorRowDataToRaxAdjustFactorMap;
    }

    public Map getRaxCrestRowDataToRaxCrestMap()
    {
        return _raxCrestRowDataToRaxCrestMap;
    }
    
    public void setEndDateForLidInReservoir( RaxReservoir reservoir )
    {
        RaxReservoirTable table = new RaxReservoirTable( _raxDb );
        RaxReservoirRecord record = null;
        RaxReservoirRecord newRecord = null;
        List raxReservoirRecordList = new ArrayList();
        StringDataConverter converter = new StringDataConverter();
        String where = " WHERE LID = '" + reservoir.getLid() + "' ";
        String endDateString = DbTimeHelper.getDateStringFromLongTime( System.currentTimeMillis() );
        long endDate = converter.getLongDateValue( endDateString );
        
        try
        {
            raxReservoirRecordList = table.select( where );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        for ( int i = 0; i < raxReservoirRecordList.size(); i++ )
        {
            record = (RaxReservoirRecord) raxReservoirRecordList.get( i );
            
            if ( !
                 ( ( record.getLid().equalsIgnoreCase( reservoir.getLid()  ) ) &&
                 ( record.getSbd() == reservoir.getBeginDate() ) ) )
            {
                if ( record.getSed() == DbTable.getNullLong() ) // End date is null 
                {
                    newRecord = new RaxReservoirRecord( record );
                    newRecord.setSed( endDate );
                    try
                    {
                        table.update( record, newRecord );
                    }
                    catch ( SQLException e )
                    {
                        e.printStackTrace( _logger.getPrintWriter() );
                    }
                }
            }
        }
    }
    
    public void setEndDateForLidInLocation( RaxLocation raxLocation )
    {
        RaxLocationTable table = new RaxLocationTable( _raxDb );
        RaxLocationRecord record = null;
        RaxLocationRecord newRecord = null;
        List raxLocationRecordList = new ArrayList();
        StringDataConverter converter = new StringDataConverter();
        String where = " WHERE LID='" + raxLocation.getLid() + "' ";
        String endDateString = DbTimeHelper.getDateStringFromLongTime( System.currentTimeMillis() );
        long endDate = converter.getLongDateValue( endDateString );

        try
        {
            raxLocationRecordList = table.select( where );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        for ( int i = 0; i < raxLocationRecordList.size(); i++ )
        {
            record = (RaxLocationRecord) raxLocationRecordList.get( i );
            
            if ( ! 
                 ( ( record.getLid().equalsIgnoreCase( raxLocation.getLid() ) ) &&
                 ( record.getSbd() == raxLocation.getBeginDate() ) ) )
            {
                if ( record.getSed() == DbTable.getNullLong() )  // End date is null
                {
                    newRecord = new RaxLocationRecord( record );
                    newRecord.setSed( endDate );
                    try
                    {
                        table.update( record, newRecord );
                    }
                    catch ( SQLException e ){}
                }
            }
        }
    }
    
    public boolean doesRaxLocationExist( RaxLocation raxLocation )
    {
        boolean exists = false;
        
        RaxLocationTable table = new RaxLocationTable( _raxDb );
        List raxLocationList = null;
        StringDataConverter converter = new StringDataConverter();
        
        String where = " WHERE LID = '" + raxLocation.getLid() + "' AND SBD = '" + converter.getDateStringFromDateLong( raxLocation.getBeginDate() ) + "' ";
        
        try
        {
            raxLocationList = table.select( where );
            if ( ! raxLocationList.isEmpty() )
            {
                exists = true;
            }
        }
        catch ( SQLException e ){}
        
        return exists;
    }
    
    public boolean doesRaxReservoirExist( RaxReservoir raxReservoir )
    {
        boolean exists = false;
        
        RaxReservoirTable table = new RaxReservoirTable( _raxDb );
        List raxReservoirList = null;
        StringDataConverter converter = new StringDataConverter();
        
        String where = " WHERE LID = '" + raxReservoir.getLid() + "' AND SBD = '" + converter.getDateStringFromDateLong( raxReservoir.getBeginDate() ) + "' ";
        
        try
        {
            raxReservoirList = table.select( where );
            if ( ! raxReservoirList.isEmpty() )
            {
                exists = true;
            }
        }
        catch ( SQLException e ){}
        
        return exists;
    }
    
    public boolean doesNewerRaxLocationExist( RaxLocation raxLocation )
    {
        boolean exists = false;
        
        RaxLocationTable table = new RaxLocationTable( _raxDb );
        List raxLocationList = null;
        StringDataConverter converter = new StringDataConverter();
        RaxLocationRecord raxLocationRecord = null;
        
        String where = " WHERE LID = '" + raxLocation.getLid() + "' AND SED IS NULL ";
        
        try
        {
            raxLocationList = table.select( where );
            
            for ( int i = 0; i < raxLocationList.size(); i++ )
            {
                raxLocationRecord = (RaxLocationRecord) raxLocationList.get( i );
                if ( raxLocationRecord.getSbd() > raxLocation.getBeginDate() )
                {
                    exists = true;
                    break;
                }
            }
        }
        catch ( SQLException e ){}
        
        return exists;
    }
    
    public boolean doesNewerRaxReservoirExist( RaxReservoir reservoir )
    {
        boolean exists = false;
        
        RaxReservoirTable table = new RaxReservoirTable( _raxDb );
        List raxReservoirList = null;
        StringDataConverter converter = new StringDataConverter();
        RaxReservoirRecord raxReservoirRecord = null;
        
        String where = " WHERE LID = '" + reservoir.getLid() + "' AND SED IS NULL ";
        
        try
        {
            raxReservoirList = table.select( where );
            
            for ( int i = 0; i < raxReservoirList.size(); i++ )
            {
                raxReservoirRecord = (RaxReservoirRecord) raxReservoirList.get( i );
                if ( raxReservoirRecord.getSbd() > reservoir.getBeginDate() )
                {
                    exists = true;
                    break;
                }
            }
        }
        catch ( SQLException e ){}
     
        return exists;
    }
    
    public String getStateCountryFipsString( StateRecord record )
    {
        String stateCountryFips = record.getState() + " / " + record.getCountryfips();
        
        return stateCountryFips;
    }
    
    public String getStateCountryFipsString( Counties counties )
    {
        String stateCountryFips = counties.getState() + " / " + counties.getCountryFips();
        
        return stateCountryFips;
    }
    
    
    public List getStateCountryFipsListFromStateRecordsList( List stateRecordsList )
    {
        List stateCountryFipsList = new ArrayList();
        
        for ( int i = 0 ; i < stateRecordsList.size(); i++ )
        {
            StateRecord record = (StateRecord) stateRecordsList.get( i );
            stateCountryFipsList.add( getStateCountryFipsString( record ) );
            _stateCountryFipsStringToStateRecordMap.put( getStateCountryFipsString( record ), record );
        }
        
        return stateCountryFipsList;
    }
    
    public List getWfoListFromWfoHsaRecordsList( List wfoHsaRecordsList )
    {
        List wfoList = new ArrayList();
        
        for ( int i = 0; i < wfoHsaRecordsList.size(); i++ )
        {
            Wfo_hsaRecord record = (Wfo_hsaRecord) wfoHsaRecordsList.get( i );
            wfoList.add( record.getWfo_hsa() );
        }
        
        return wfoList;
    }
    
    public List getStateCountryFipsList()
    {
        List stateRecordsList = getStateRecordsListOrderedByCountryState();
        List stateCountryFipsList = getStateCountryFipsListFromStateRecordsList( stateRecordsList );
    
        return stateCountryFipsList;
    }
    
    public List getWfoList()
    {
        List wfoHsaRecordsList = getWfoHsaRecordsListOrderedByWfo();
        List wfoList = getWfoListFromWfoHsaRecordsList( wfoHsaRecordsList );
        
        return wfoList;
    }
    
//    ------------------------------ Save to Database Methods ------------------------
    
    public void saveRaxCrestToDatabase( RaxCrest raxCrest )
    {
        RaxCrestTable table = new RaxCrestTable( _raxDb );
        RaxCrestRecord record = RaxCrest.getRaxCrestRecord( raxCrest );
        
        try
        {
            table.insertOrUpdate( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void saveRaxIngestFilter( RaxIngestFilter raxIngestFilter )
    {
        RaxIngestFilterTable table = new RaxIngestFilterTable( _raxDb );
        RaxIngestFilterRecord record = RaxIngestFilter.getRaxIngestFilterRecord( raxIngestFilter );
        
        try
        {
            table.insertOrUpdate( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public boolean saveCounties( Counties counties )
    {
        CountiesTable table = new CountiesTable( _raxDb );
        CountiesRecord record = Counties.getCountiesRecord( counties );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }
    
    public boolean saveState( State state )
    {
        StateTable table = new StateTable( _raxDb );
        StateRecord record = State.getStateRecord( state );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }
    
    public boolean saveProd( Prod prod )
    {
        ProdTable table = new ProdTable( _raxDb );
        ProdRecord record = Prod.getProdRecord( prod );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }
    public boolean saveHuc8( Huc8 huc8 )
    {
        Huc8Table table = new Huc8Table( _raxDb );
        Huc8Record record = Huc8.getHuc8Record( huc8 );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }
    
    public boolean saveHuc6( Huc6 huc6 )
    {
        Huc6Table table = new Huc6Table( _raxDb );
        Huc6Record record = Huc6.getHuc6Record( huc6 );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }

    public boolean saveHuc4( Huc4 huc4 )
    {
        Huc4Table table = new Huc4Table( _raxDb );
        Huc4Record record = Huc4.getHuc4Record( huc4 );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }

    public boolean saveRfc( Rfc rfc )
    {
        RfcTable table = new RfcTable( _raxDb );
        RfcRecord record = Rfc.getRfcRecord( rfc );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }

    public boolean saveWfoHsa( WfoHsa wfoHsa )
    {
        Wfo_hsaTable table = new Wfo_hsaTable( _raxDb );
        Wfo_hsaRecord record = WfoHsa.getWfoHsaRecord( wfoHsa );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }

    public boolean saveShefProb( ShefProb shefProb )
    {
        ShefprobTable table = new ShefprobTable( _raxDb );
        ShefprobRecord record = ShefProb.getShefProbRecord( shefProb );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }

    public boolean saveShefQC( ShefQC shefQC )
    {
        ShefqcTable table = new ShefqcTable( _raxDb );
        ShefqcRecord record = ShefQC.getShefQCRecord( shefQC );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }

    public boolean saveShefPe( ShefPE shefPe )
    {
        ShefpeTable table = new ShefpeTable( _raxDb );
        ShefpeRecord record = ShefPE.getShefPeRecord( shefPe );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }

    public boolean saveAgency( Agency agency )
    {
        AgencyTable table = new AgencyTable( _raxDb );
        AgencyRecord record = Agency.getAgencyRecord( agency );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }

    public boolean saveShefPe1( ShefPE1 shefPe1 )
    {
        Shefpe1Table table = new Shefpe1Table( _raxDb );
        Shefpe1Record record = ShefPE1.getShefPe1Record( shefPe1 );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }

    public boolean saveShefPeTrans( ShefPETrans shefPETrans )
    {
        ShefpetransTable table = new ShefpetransTable( _raxDb );
        ShefpetransRecord record = ShefPETrans.getShefPeTransRecord( shefPETrans );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }
    
    public boolean saveShefTs( ShefTS shefTS )
    {
        SheftsTable table = new SheftsTable( _raxDb );
        SheftsRecord record = ShefTS.getShefTSRecord( shefTS );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }

    public boolean saveShefEx( ShefExtremum shefEx )
    {
        ShefexTable table = new ShefexTable( _raxDb );
        ShefexRecord record = ShefExtremum.getShefExRecord( shefEx );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }
    
    public boolean saveShefDur( ShefDuration shefDur )
    {
        ShefdurTable table = new ShefdurTable( _raxDb );
        ShefdurRecord record = ShefDuration.getShefDurRecord( shefDur );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }
    
    public boolean saveHuc2( Huc2 huc2 )
    {
        Huc2Table table = new Huc2Table( _raxDb );
        Huc2Record record = Huc2.getHuc2Record( huc2 );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }

    public boolean saveCountry( Country country )
    {
        CountryTable table = new CountryTable( _raxDb );
        CountryRecord record = Country.getCountryRecord( country );
        boolean saved = false;
        
        try 
        {
            table.insert( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }
    
    
    
    public boolean saveModCtrl( ModCtrl modCtrl )
    {
        ModctrlTable table = new ModctrlTable( _raxDb );
        ModctrlRecord record = ModCtrl.getModCtrlRecord( modCtrl );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }
    
    public boolean saveRaxDataLimits( RaxDataLimits raxDataLimits )
    {
        RaxDataLimitsTable table = new RaxDataLimitsTable( _raxDb );
        RaxDataLimitsRecord record = RaxDataLimits.getRaxDataLimitsRecord( raxDataLimits );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }

    
    public boolean saveRaxLocDataLimits( RaxLocDataLimits raxLocDataLimits )
    {
        RaxLocDataLimitsTable table = new RaxLocDataLimitsTable( _raxDb );
        RaxLocDataLimitsRecord record = RaxLocDataLimits.getRaxLocDataLimitsRecord( raxLocDataLimits );
        boolean saved = false;
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }

    public boolean saveRaxRiverCrit( RaxRiverCrit raxRiverCrit )
    {
        RaxRiverCritTable table = new RaxRiverCritTable( _raxDb );
        RaxRiverCritRecord record = RaxRiverCrit.getRaxRiverCritRecord( raxRiverCrit );
        boolean saved = false;
        
        try
        {
            table.insertOrUpdate( record );
            saved = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return saved;
    }
    
    public void saveRaxAdjustFactor( RaxAdjustFactor raxAdjustFactor )
    {
        RaxAdjustFactorTable table = new RaxAdjustFactorTable( _raxDb );
        RaxAdjustFactorRecord record = RaxAdjustFactor.getRaxAdjustFactorRecord( raxAdjustFactor );
        
        try
        {
            table.insertOrUpdate( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public boolean insertNewLocation( RaxLocation raxLocation )
    {
        RaxLocationTable table = new RaxLocationTable( _raxDb );
        RaxLocationRecord record = RaxLocation.getRaxLocationRecord( raxLocation );
        int numberOfRecordsInserted = -9999;
        boolean inserted = false;
        
        try
        {
            numberOfRecordsInserted = table.insert( record );
            if ( numberOfRecordsInserted == 1 )
            {
                inserted = true;
            }
        }
        catch ( SQLException e )
        {
        }
        
        return inserted;
    }
    
    public void saveLocation( RaxLocation raxLocation )
    {
        RaxLocationTable table = new RaxLocationTable( _raxDb );
        RaxLocationRecord record = RaxLocation.getRaxLocationRecord( raxLocation );
        
        try
        {
            table.insertOrUpdate( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void saveReservoir( RaxReservoir raxReservoir )
    {
        RaxReservoirTable table = new RaxReservoirTable( _raxDb );
        RaxReservoirRecord record = RaxReservoir.getRaxReservoirRecord( raxReservoir );
        
        try
        {
            table.insertOrUpdate( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void saveRaxRatingShift( RaxRatingShift raxRatingShift )
    {
        RaxRatingShiftTable table = new RaxRatingShiftTable( _raxDb );
        RaxRatingShiftRecord record = RaxRatingShift.getRaxRatingShiftRecord( raxRatingShift );
        
        try
        {
            table.insertOrUpdate( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void saveRaxRating( RaxRating raxRating )
    {
        CustomRaxRatingTable table = new CustomRaxRatingTable( _raxDb );
        RaxRatingRecord record = RaxRating.getRaxRatingRecord( raxRating );
        _logger.log( "RECORD = " + record );
        try
        {
            table.insertOrUpdate( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public boolean saveAverage( Average avg )
    {
        boolean success = false;
        
        AvgTable table = new AvgTable( _raxDb );
        AvgRecord record = Average.getAverageRecord( avg );
        
        try
        {
            table.insertOrUpdate( record );
            success = true;
        }
        catch( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return success;
    }
    
    public boolean saveSlopeProfile( SlopeProfile slopeProfile )
    {
        boolean success = false;
        
        SlopeprofileTable table = new SlopeprofileTable( _raxDb );
        SlopeprofileRecord record = SlopeProfile.getSlopeProfileRecord( slopeProfile );
        
        try 
        {
            table.insertOrUpdate( record );
            success = true;
        }
        catch( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return success;
    }
    
    public boolean saveSensok( Sensok sensok )
    {
        boolean success = false;
        
        SensokTable table = new SensokTable( _raxDb );
        SensokRecord record = Sensok.getSensokRecord( sensok );
        
        try 
        {
            table.insertOrUpdate( record );
            success = true;
        }
        catch( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return success;
    }
    
//  ------------------------------ Delete from Database Methods ------------------------

    public void deleteRaxCrestFromDatabase( RaxCrest raxCrest )
    {
        RaxCrestTable table = new RaxCrestTable( _raxDb );
        RaxCrestRecord record = RaxCrest.getRaxCrestRecord( raxCrest );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteCountiesFromDataBase( Counties counties )
    {
        CountiesTable table = new CountiesTable( _raxDb );
        CountiesRecord record = Counties.getCountiesRecord( counties );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteStateFromDataBase( State state )
    {
        StateTable table = new StateTable( _raxDb );
        StateRecord record = State.getStateRecord( state );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteProdFromDataBase( Prod prod )
    {
        ProdTable table = new ProdTable( _raxDb );
        ProdRecord record = Prod.getProdRecord( prod );
    
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }

    public void deleteHuc8FromDataBase( Huc8 huc8 )
    {
        Huc8Table table = new Huc8Table( _raxDb );
        Huc8Record record = Huc8.getHuc8Record( huc8 );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteHuc6FromDataBase( Huc6 huc6 )
    {
        Huc6Table table = new Huc6Table( _raxDb );
        Huc6Record record = Huc6.getHuc6Record( huc6 );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }

    public void deleteHuc4FromDataBase( Huc4 huc4 )
    {
        Huc4Table table = new Huc4Table( _raxDb );
        Huc4Record record = Huc4.getHuc4Record( huc4 );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteRfcFromDataBase( Rfc rfc )
    {
        RfcTable table = new RfcTable( _raxDb );
        RfcRecord record = Rfc.getRfcRecord( rfc );
        
        try
        {
            table.delete( record );
        }
        catch( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }

    public void deleteWfoHsaFromDataBase( WfoHsa wfoHsa )
    {
        Wfo_hsaTable table = new Wfo_hsaTable( _raxDb );
        Wfo_hsaRecord record = WfoHsa.getWfoHsaRecord( wfoHsa );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteAgencyFromDataBase( Agency agency )
    {
        AgencyTable table = new AgencyTable( _raxDb );
        AgencyRecord record = Agency.getAgencyRecord( agency );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }

    public void deleteShefPe1FromDataBase( ShefPE1 shefPe )
    {
        Shefpe1Table table = new Shefpe1Table( _raxDb );
        Shefpe1Record record = ShefPE1.getShefPe1Record( shefPe );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteShefProbFromDataBase( ShefProb shefProb )
    {
        ShefprobTable table = new ShefprobTable( _raxDb );
        ShefprobRecord record = ShefProb.getShefProbRecord( shefProb );
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteShefQCFromDataBase( ShefQC shefQC )
    {
        ShefqcTable table = new ShefqcTable( _raxDb );
        ShefqcRecord record = ShefQC.getShefQCRecord( shefQC );
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }

    public void deleteShefPeFromDataBase( ShefPE shefPe )
    {
        ShefpeTable table = new ShefpeTable( _raxDb );
        ShefpeRecord record = ShefPE.getShefPeRecord( shefPe );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }

    public void deleteShefPeTransFromDataBase( ShefPETrans shefPETrans )
    {
        ShefpetransTable table = new ShefpetransTable( _raxDb );
        ShefpetransRecord record = ShefPETrans.getShefPeTransRecord( shefPETrans );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteShefTsFromDataBase( ShefTS shefTS )
    {
        SheftsTable table = new SheftsTable( _raxDb );
        SheftsRecord record = ShefTS.getShefTSRecord( shefTS );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }

    public void deleteShefExtremumFromDataBase( ShefExtremum shefEx )
    {
        ShefexTable table = new ShefexTable( _raxDb );
        ShefexRecord record = ShefExtremum.getShefExRecord( shefEx );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteShefDurationFromDataBase( ShefDuration shefDur )
    {
        ShefdurTable table = new ShefdurTable( _raxDb );
        ShefdurRecord record = ShefDuration.getShefDurRecord( shefDur );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteHuc2FromDataBase( Huc2 huc2 )
    {
        Huc2Table table = new Huc2Table( _raxDb );
        Huc2Record record = Huc2.getHuc2Record( huc2 );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }

    public void deleteCountryFromDataBase( Country country )
    {
        CountryTable table = new CountryTable( _raxDb );
        CountryRecord record = Country.getCountryRecord( country );
        String where = " WHERE Country='" + record.getCountry() + "' AND CountryFips='" + record.getCountryfips() + "'";
        
        try
        {
            table.delete( where );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }

    public void deleteModCtrlFromDataBase( ModCtrl modCtrl )
    {
        ModctrlTable table = new ModctrlTable( _raxDb );
        ModctrlRecord record = ModCtrl.getModCtrlRecord( modCtrl );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteRaxReservoirFromDatabase( RaxReservoir raxReservoir )
    {
        RaxReservoirTable table = new RaxReservoirTable( _raxDb );
        RaxReservoirRecord record = RaxReservoir.getRaxReservoirRecord( raxReservoir );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteRaxIngestFilterFromDatabase( RaxIngestFilter raxIngestFilter )
    {
        RaxIngestFilterTable table = new RaxIngestFilterTable( _raxDb );
        RaxIngestFilterRecord record = RaxIngestFilter.getRaxIngestFilterRecord( raxIngestFilter );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteRaxLocDataLimitsFromDatabase( RaxLocDataLimits raxLocDataLimits )
    {
        RaxLocDataLimitsTable table = new RaxLocDataLimitsTable( _raxDb );
        RaxLocDataLimitsRecord record = RaxLocDataLimits.getRaxLocDataLimitsRecord( raxLocDataLimits );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteRaxDataLimitsFromDatabase( RaxDataLimits raxDataLimits )
    {
        RaxDataLimitsTable table = new RaxDataLimitsTable( _raxDb );
        RaxDataLimitsRecord record = RaxDataLimits.getRaxDataLimitsRecord( raxDataLimits );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteRaxRiverCrit( RaxRiverCrit raxRiverCrit )
    {
        RaxRiverCritTable table = new RaxRiverCritTable( _raxDb );
        RaxRiverCritRecord record = RaxRiverCrit.getRaxRiverCritRecord( raxRiverCrit );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public void deleteRaxAdjustFactorFromDatabase( RaxAdjustFactor raxAdjustFactor )
    {
        RaxAdjustFactorTable table = new RaxAdjustFactorTable( _raxDb );
        RaxAdjustFactorRecord record = RaxAdjustFactor.getRaxAdjustFactorRecord( raxAdjustFactor );
        
        try
        {
            table.delete( record );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
    }
    
    public boolean deleteAverageFromDatabase( Average average )
    {
        boolean deleted = false;
        
        AvgTable table = new AvgTable( _raxDb );
        AvgRecord record = Average.getAverageRecord( average );
        
        try
        {
            table.delete( record );
            deleted = true;
        }
        catch( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return deleted;
    }
    
    public boolean deleteSlopeProfileFromDatabase( SlopeProfile slopeProfile )
    {
        boolean deleted = false;
        
        SlopeprofileTable table = new SlopeprofileTable( _raxDb );
        SlopeprofileRecord record = SlopeProfile.getSlopeProfileRecord( slopeProfile );
        
        try
        {
            table.delete( record );
            deleted = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return deleted;
    }
    
    public boolean deleteSensokFromDatabase( Sensok sensok )
    {
        boolean deleted = false;
        
        SensokTable table = new SensokTable( _raxDb );
        SensokRecord record = Sensok.getSensokRecord( sensok );
        
        try
        {
            table.delete( record );
            deleted = true;
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        return deleted;
    }

    public RaxSyncDataMgr getRaxSyncDataMgr()
    {
        return _raxSyncDataMgr;
    }
    
    
    public RaxLocation getRaxLocation( String lid )
    {
        RaxLocation raxLocation = null;
        RaxLocationRecord record = null;
        RaxLocationTable table = new RaxLocationTable( _raxDb );
        List recordList = null;
        
        try
        {
            recordList = table.select( " WHERE LID ='" + lid + "'" );
        }
        catch ( SQLException e )
        {
            e.printStackTrace( _logger.getPrintWriter() );
        }
        
        if ( ( recordList != null ) && ( ! recordList.isEmpty() ) )
        {
            record = (RaxLocationRecord) recordList.get( 0 );
            raxLocation = new RaxLocation( record );
        }
        
        return raxLocation;
    }

    public void setRaxIngestFilterRowDataList( List rowDataList )
    {
        _raxIngestFilterRowDataList = rowDataList;
    }

    public Map getModCtrlRowDataToModCtrlMap()
    {
        return _modCtrlRowDataToModCtrlMap;
    }
    
    public Map getCountryRowDataToCountryMap()
    {
        return _countryRowDataToCountryMap;
    }
    
    public Map getStateRowDataToStateMap()
    {
        return _stateRowDataToStateMap;
    }

    public Map getCountiesRowDataToCountiesMap()
    {
        return _countiesRowDataToCountiesMap;
    }

    public Map getHuc2RowDataToHuc2Map()
    {
        return _huc2RowDataToHuc2Map;
    }

    public Map getHuc4RowDataToHuc4Map()
    {
        return _huc4RowDataToHuc4Map;
    }

    public Map getHuc6RowDataToHuc6Map()
    {
        return _huc6RowDataToHuc6Map;
    }

    public Map getHuc8RowDataToHuc8Map()
    {
        return _huc8RowDataToHuc8Map;
    }

    public Map getWfoHsaRowDataToWfoHsaMap()
    {
        return _wfoHsaRowDataToWfoHsaMap;
    }
    
    public Map getRfcRowDataToRfcMap()
    {
        return _rfcRowDataToRfcMap;
    }

    public Map getShefDurRowDataToShefDurMap()
    {
        return _shefDurRowDataToShefDurMap;
    }

    public Map getShefExRowDataToShefExMap()
    {
        return _shefExRowDataToShefExMap;
    }

    public Map getShefPeTransRowDataToShefPeTransMap()
    {
        return _shefPeTransRowDataToShefPeTransMap;
    }
    
    public Map getShefPeRowDataToShefPeMap()
    {
        return _shefPeRowDataToShefPeMap;
    }

    public Map getShefPe1RowDataToShefPe1Map()
    {
        return _shefPe1RowDataToShefPe1Map;
    }

    public Map getShefProbRowDataToShefProbMap()
    {
        return _shefProbRowDataToShefProbMap;
    }

    public Map getShefQCRowDataToShefQCMap()
    {
        return _shefQCRowDataToShefQCMap;
    }

    public Map getShefTsRowDataToShefTsMap()
    {
        return _shefTsRowDataToShefTsMap;
    }

    public Map getAgencyRowDataToAgencyMap()
    {
        return _agencyRowDataToAgencyMap;
    }

    public Map getProdRowDataToProdMap()
    {
        return _prodRowDataToProdMap;
    }

    public Map getStateCountryFipsStringToStateMap()
    {
        return _stateCountryFipsStringToStateRecordMap;
    }
}

