package ohd.hseb.sshp.precip;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.grid.XmrgGrid;
import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.sshp.DataMgr;
import ohd.hseb.util.Logger;

public abstract class BaseArealPrecipPreprocessor implements ArealPrecipPreprocessor
{
    protected static final double _timeSeriesMissingValue = -9999.0;

    protected DataMgr _dataMgr = null;

    protected String _mpeQpeGridDirName = null;
    protected String _rfcQpeGridDirName = null;

    protected String _bestQpeDateFormatFromToken = null; 
    protected String _sshpMapQpeToUseTokenValueString = null;

    SimpleDateFormat _localXmrgFileDateFormat = null;
    SimpleDateFormat _rfcXmrgFileDateFormat = null;
    SimpleDateFormat _hpeXmrgFileDateFormat = null;


    // QPE usage
    protected int _qpeUsage = MIXED_QPE;

    protected static final int RFC_QPE = 0;
    protected static final int MIXED_QPE = 1;
    protected static final int LOCAL_BEST_QPE = 2;

    protected static final String _rfcOnlyString = "RFC_ONLY";
    protected static final String _mixedString = "MIXED";
    protected static final String _localBestOnlyString = "LOCAL_BEST_ONLY";


    protected List _areaIdList = null;
    protected List _basinHrapHelperList  = null;
    protected Logger _logger = null;
    protected String _typeSourceCode = null;

    abstract protected void readAppsDefaults();

    // ----------------------------------------------------------------------------

    public void initialize(DataMgr dataMgr, Logger logger, String typeSourceCode)
    {
        _dataMgr = dataMgr;
        _logger = logger;
        _typeSourceCode = typeSourceCode;

        readAppsDefaults();

        // load all the areas that are suitable for use as SSHP basins
        _areaIdList = _dataMgr.loadAllSshpBasinIds();

        //let the db manager add rows to ingest filter as needed
        _dataMgr.saveMapRowsToIngestFilterAsNeeded(_areaIdList, _typeSourceCode);

        //load the basinHrapHelpers and return them in a List
        _basinHrapHelperList = getBasinHrapHelperList(_areaIdList);

    }
 
    // ----------------------------------------------------------------------------

    private List getBasinHrapHelperList(List areaIdList)
    {
        List basinHrapHelperList = new ArrayList();
        for (int i = 0; i < areaIdList.size(); i++)
        {
            String areaId = (String ) areaIdList.get(i);

            BasinHrapHelper helper = _dataMgr.loadBasinHrapHelper(areaId);

            if (helper != null)
            {
                //_logger.log(helper.toString());

                basinHrapHelperList.add(helper);
            }
        }    


        return basinHrapHelperList;

    } 

    // ----------------------------------------------------------------------------

    protected void preprocessQpeFiles(List timeList, DateFormat dateFormat, String baseDirectory)
    {
        String header = "BaseArealPrecipPreprocessor.preprocessQpeFiles(): ";
        // create an Xmrg file reader
        XmrgReader reader = new XmrgReader(_logger);

        _logger.log(header + "baseDirectory = " + baseDirectory );    

        long productTime = System.currentTimeMillis();

        // for each re-run hour of input
        for (int i = 0; i < timeList.size(); i++)
        {
            Long timeLong = (Long) timeList.get(i);
            long time = timeLong.longValue();
            
            long basisTime = time;
  

            String gridFileName = dateFormat.format(new Date(time));
            String fullGridPath = baseDirectory + "/" + gridFileName;

            long validTime = getValidTime(time);
            
            String timeString = DbTimeHelper.getDateTimeStringFromLongTime(validTime);            
             
            XmrgGrid grid = reader.loadGrid(validTime, fullGridPath);

            if (grid.isValid())
            {
                _logger.log(header + "grid for time = " + timeString + " is VALID ");    

                // for each basin, process its data
                for (int j = 0; j < _basinHrapHelperList.size(); j++)
                {
                    BasinHrapHelper basinHrapHelper = (BasinHrapHelper) _basinHrapHelperList.get(j);

                    preprocess(basinHrapHelper, grid, basisTime, productTime);
                }
            }
            else
            {
                // _logger.log(header + "grid for time = " + timeString + " is invalid");    
            }
        } // for loop

    }

    // ----------------------------------------------------------------------------

    protected long getValidTime(long origTime)
    {
        return origTime;
    }

    // ----------------------------------------------------------------------------

    public void preprocess(BasinHrapHelper basinHrapHelper, XmrgGrid grid, long basisTime, long productTime)
    {

        String areaId = basinHrapHelper.getBasinId(); 

        Measurement mapMeasurement = getMAPMeasurementFromGrid(basinHrapHelper, grid);
        
        _dataMgr.saveMapMeasurement(areaId, _typeSourceCode, mapMeasurement, grid.getTime(), basisTime, productTime, _logger);

        return;

    }

    // ----------------------------------------------------------------------------

    private Measurement getMAPMeasurementFromGrid(BasinHrapHelper basinHrapHelper, XmrgGrid grid)
    {
        String header = "BaseArealPrecipPreprocessor.getMAPMeasurementFromGrid(): ";
        MeasuringUnit precipUnit = MeasuringUnit.inches;
        double mapValue = -1.0;
        double binValue = 0;
        double totalValue = 0;
        int validValueCount = 0;

        // CHANGE 8/23/05    
        _logger.log(header + " for basinId =  " + basinHrapHelper.getBasinId());

        int rowCount = basinHrapHelper.getRowCount();

        boolean gridAccessProblem = false;

        //for each row
        for (int i = 0; (i < rowCount && !gridAccessProblem) ; i++)
        {
            int hrapRow = basinHrapHelper.getBasinHrapRow(i);

            // for each column
            for (int hrapCol = basinHrapHelper.getBasinHrapBegColumn(i); 
            ( (hrapCol <= basinHrapHelper.getBasinHrapEndColumn(i)) && 
                    (!gridAccessProblem));
            hrapCol++)
            {
                //yank out the bin value at that point
                binValue = grid.getValue(hrapRow, hrapCol);
                if (binValue >= 0.0)
                {
                    totalValue += binValue;
                    validValueCount++;       
                }
                else if (binValue == XmrgGrid.OFF_GRID_VALUE)
                {
                    _logger.log(header + "Problem with basinId = " + basinHrapHelper.getBasinId() +
                            " attempting to access out of bounds grid location row = " + 
                            hrapRow + " and column = " + hrapCol + ".");
                    gridAccessProblem = true;

                }
            }           
        }


        // if some kind of failure occured
        Measurement mapMeasurement = null;
        if  ( (validValueCount < 1) || (gridAccessProblem) )
        {
            mapValue = _timeSeriesMissingValue;  
            mapMeasurement = new Measurement(mapValue, precipUnit);
            mapMeasurement.setIsMissing(true);  
            // System.out.println(header + "for " + basinHrapHelper.getBasinId() + " totalValue = " + totalValue + " validValueCount = " + validValueCount);
        }
        else //success occured, so create a decent measurement
        {   
            // System.out.println(header + "for " + basinHrapHelper.getBasinId() + " totalValue = " + totalValue + " validValueCount = " + validValueCount);
            mapValue = totalValue / validValueCount; 
            if (mapValue < 0.005)
            {
                mapValue = 0.0;            
            }
            mapMeasurement = new Measurement(mapValue, precipUnit);   
        }


        return mapMeasurement;   

    }  

    // ----------------------------------------------------------------------------

    public void finalize()
    {
        try
        {
            _dataMgr.disconnect();
            _logger.close();    
        }
        catch (Throwable t)
        {
            System.err.println(t.getMessage());
        }

    }

    // ----------------------------------------------------------------------------

} // End of BaseArealPrecipPreprocessor abstract class