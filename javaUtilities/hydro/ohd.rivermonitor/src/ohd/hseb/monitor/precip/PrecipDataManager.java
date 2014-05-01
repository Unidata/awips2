package ohd.hseb.monitor.precip;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ohd.hseb.db.Database;
import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.geomap.model.HrapRowColToLatLonConverter;
import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.model.RowColumnPoint;
import ohd.hseb.grid.XmrgGrid;
import ohd.hseb.ihfsdb.generated.LocationRecord;
import ohd.hseb.ihfsdb.generated.LocationTable;
import ohd.hseb.model.ParamCode;
import ohd.hseb.monitor.river.RiverMonitorJTableRowData;
import ohd.hseb.monitor.util.HydroappsDefaultDirs;
import ohd.hseb.pdc_pp.PDCFileReader;
import ohd.hseb.pdc_pp.RegularObsTimeSeries;
import ohd.hseb.pdc_pp.TimeValuePair;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.MemoryLogger;
import ohd.hseb.util.SessionLogger;
import ucar.ma2.IndexIterator;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

public class PrecipDataManager
{
    private AppsDefaults _appsDefaults;

    private double _missingDoubleValue = -9999.0;

    private SessionLogger _logger;
    private Database _db;
    
    private PDCFileReader _pdcFileReaderForPrecipInstant;
    private PDCFileReader _pdcFileReaderForPrecip1Hour;
    private PDCFileReader _pdcFileReaderForPrecip3Hour;
    private PDCFileReader _pdcFileReaderForPrecip6Hour;
    private PDCFileReader _pdcFileReaderForPrecip24Hour;
    private long _pdcFileUpdateLongTime = 0; //this is the time of file creation
   
    private String _pdcPreprocessorFilesDir;
    private Map<String, List<RegularObsTimeSeries>> _lidToLatestPrecipMap = null;

    private Map<String, List<RegularObsTimeSeries>> _lidToTohPrecipMapFor1Hr = null;
    private Map<String, List<RegularObsTimeSeries>> _lidToTohPrecipMapFor3Hr = null;
    private Map<String, List<RegularObsTimeSeries>> _lidToTohPrecipMapFor6Hr = null;
    private Map<String, List<RegularObsTimeSeries>> _lidToTohPrecipMapFor24Hr = null;

    private Map _lidToLatLonPointMap = null;

    private HrapRowColToLatLonConverter _hrapRowColToLatLonConverter = null;
    private int _baseNationalHrapRowSW;
    private int _baseNationalHrapColSW;

    private XmrgGrid _xmrgGridFor1hrFFG = null;
    private XmrgGrid _xmrgGridFor3hrFFG = null;
    private XmrgGrid _xmrgGridFor6hrFFG = null;

    public PrecipDataManager(Database db, AppsDefaults appsDefaults, String missingRepresentation, SessionLogger logger)
    {
        _logger = logger;
        
        _db = db;
        _appsDefaults = appsDefaults;
        _pdcPreprocessorFilesDir = _appsDefaults.getToken("pdc_pp_dir", 
        	HydroappsDefaultDirs.PDC_PP_DIR);
    }
//  ---------------------------------------------------------------------------------------------------
    protected Map getLidToPrecipTotalsMapFromPreprocessorFile(PDCFileReader pdcFileReader, Map lidToPrecipTotalsMap)
    {
        String header = "PrecipDataManager.getLidToPrecipTotalsMapFromPreprocessorFile(): ";
        
        //this block of code attempts to help Garbage collection reduce the amount of memory required
        if(lidToPrecipTotalsMap == null)
        {
           lidToPrecipTotalsMap = new HashMap();
        }
        else //clear out the map and its contents
        {
            Set<String> lidSet = lidToPrecipTotalsMap.keySet();

            //run through each List <RegularObsTimeSeries>  associated with each lid and clear it
            for (String lid : lidSet)
            {
                List regularObsTimeSeriesForAllTSList =(List)lidToPrecipTotalsMap.get(lid);
                regularObsTimeSeriesForAllTSList.clear();
            }

            lidToPrecipTotalsMap.clear();
        }
       
        checkMemoryAndGarbageCollectLog(header +"G.065 before pdcFileReader.read(), before GC");

        //read precip data from the PDC file
        List<RegularObsTimeSeries> regularObsTimeSeriesList = pdcFileReader.read();

        checkMemoryAndGarbageCollectLog(header +"G.07 after pdcFileReader.read(), before GC");
        
        // put the list of  RegularObsTimeSeries for each lid in the lidToPrecipTotalsMap 
        if( regularObsTimeSeriesList != null )
        {
            List<RegularObsTimeSeries> regularObsTimeSeriesForAllTSList = null;

            for (RegularObsTimeSeries regularObsTimeSeries: regularObsTimeSeriesList)
            {
                String lid = regularObsTimeSeries.getDescriptor().getLid();

                regularObsTimeSeriesForAllTSList = (List) lidToPrecipTotalsMap.get(lid);

                if(regularObsTimeSeriesForAllTSList == null)
                {
                    regularObsTimeSeriesForAllTSList = new ArrayList<RegularObsTimeSeries>();

                    lidToPrecipTotalsMap.put(lid, regularObsTimeSeriesForAllTSList);
                }

                regularObsTimeSeriesForAllTSList.add(regularObsTimeSeries);
                
            } //end for each regularObsTimeSeries

            checkMemoryAndGarbageCollectLog(header +"G.08, after for loop  " );

        }
        
        checkMemoryAndGarbageCollectLog(header +"Here G.09 after if loop " );

        return lidToPrecipTotalsMap;
    }

    //---------------------------------------------------------------------------------------
/*
    private String getFFGFileName(int hour)
    {
        final String header = "PrecipDataManager.getFFGFileName(): ";
        String ffgDir;
        String ffgFileName = null;
        String st3RfcTokenValue = _appsDefaults.getToken("st3_rfc");

        //System.out.println(header + "st3_rfc token value =  " + st3RfcTokenValue);
        File filesForhrMentioned[] ;

        File dir;

        if(st3RfcTokenValue.equalsIgnoreCase("host"))
        {
            ffgDir = _appsDefaults.getToken("gaff_mosaic_dir", HydroappsDefaultDirs.GAFF_MOSAIC_DIR) + "/";
            dir = new File(ffgDir);

            FilenameFilter filter = new NetCdfFileNameFilter(1);

            //TODO  we should get rid of this switch statement
            switch(hour)
            {
                case 1:
                    filter = new NetCdfFileNameFilter(1);
                    break;
                case 3:
                    filter = new NetCdfFileNameFilter(3);
                    break;
                case 6:
                    filter = new NetCdfFileNameFilter(6);
                    break;
            }
            filesForhrMentioned = dir.listFiles(filter);

        }
        else //RFC grids
        {
            String gafInputDir = _appsDefaults.getToken("gaff_input_dir");
            ffgDir = gafInputDir + "/" + new String(st3RfcTokenValue.toUpperCase()) + "/"
            + hour+"hr"+"/";
            dir = new File(ffgDir);
            filesForhrMentioned = dir.listFiles();
        }
        
        System.out.println(header + "ffgDir = " + ffgDir);
        
        if(filesForhrMentioned != null && filesForhrMentioned.length > 0 &&
                filesForhrMentioned[0].length() > 0)
        {
            File lastModifiedFile = findTheLatestModifiedFile(filesForhrMentioned);
            //System.out.println(header + " lastmodifiedfile:" + lastModifiedFile);
            ffgFileName = lastModifiedFile.toString();
        }
        else
        {
            _logger.log(header + "No ffg files found in directory [" + dir.getAbsolutePath() +"]");
        }

        return ffgFileName;
    }
*/
    private String getFFGFileName(int hour)
    {
           String header = "PrecipDataManager.getFFGFileName(): ";
           String ffgFileName = null;
           
           File[] mosaicFileArray = getGaffMosaicFileArray(hour);
         
           if ((mosaicFileArray != null ) && (mosaicFileArray.length > 0))
           {
               ffgFileName = getNameOfLastModifiedFile(mosaicFileArray);
           }
         
           else //if I didn't find any GAFF output mosaic files, use GAFF input files
           {
           
               File[] gaffInputFileArray = getGaffInputFileArray(hour);
               if  ((gaffInputFileArray != null) && (gaffInputFileArray.length > 0))
               {
                   ffgFileName = getNameOfLastModifiedFile(gaffInputFileArray);
               }
           
           }
           
           System.out.println(header + "ffgFileName = " + ffgFileName);
           
           return ffgFileName;
           
    }
    
    private String getNameOfLastModifiedFile(File[] fileArray)
    {
        String header = "PrecipDataManager.getNameOfLastModifiedFile(): ";
        String nameOfLastModifiedFile = null;
        
        if(fileArray != null && fileArray.length > 0 &&
                fileArray[0].length() > 0)
        {
            File lastModifiedFile = findTheLatestModifiedFile(fileArray);
            //System.out.println(header + " lastmodifiedfile:" + lastModifiedFile);
            nameOfLastModifiedFile = lastModifiedFile.toString();
        }
        else
        {
            _logger.log(header + "No ffg files found.");
        }
        
        return nameOfLastModifiedFile;
    }
//  ------------------------------------------------------------------------------------------------------------
    private File[] getGaffMosaicFileArray(int hour)
    {
        File filesForHour[] ;
        String ffgDirString = _appsDefaults.getToken("gaff_mosaic_dir", HydroappsDefaultDirs.GAFF_MOSAIC_DIR) + "/";
        File dir = new File(ffgDirString);

        FilenameFilter filter = new NetCdfFileNameFilter(hour);

        filesForHour = dir.listFiles(filter);
        
        return filesForHour;
    }
    
    private File[] getGaffInputFileArray(int hour)
    {
        File filesForHour[] ;
        File dir = null;
        String st3RfcTokenValue = _appsDefaults.getToken("st3_rfc");

        String gafInputDirString = _appsDefaults.getToken("gaff_input_dir");
        String ffgDirString = gafInputDirString + "/" + new String(st3RfcTokenValue.toUpperCase()) + "/"
                              + hour+"hr"+"/";
        dir = new File(ffgDirString);
        filesForHour = dir.listFiles();
        
        return filesForHour;
    }
//  ------------------------------------------------------------------------------------------------------------

    
    private File findTheLatestModifiedFile(File filesForHrMentioned[])
    {
        File lastModifiedFile = filesForHrMentioned[0];
        for(int i=1 ; i < filesForHrMentioned.length; i++)
        {
            if(filesForHrMentioned[i].lastModified() > lastModifiedFile.lastModified())
            {
                lastModifiedFile = filesForHrMentioned[i];
            }
        }
        return lastModifiedFile;
    }


//  ------------------------------------------------------------------------------------------------------------

    private void readFFGInfoForMentionedHr(int hour)
    {
        String ffgFileName = getFFGFileName(hour);

        String header = "PrecipDataManager.readFFGInfoForMentionedHr(): ";
        if(ffgFileName != null)
        {
            NetcdfFile netCdfFile = null; 
            try 
            {  
                netCdfFile = NetcdfFile.open(ffgFileName);  

                readNetCdfFileAndCreateXmargGridForMentionedHr(netCdfFile, hour);

            }
            catch (Exception ioe) 
            {   
                _logger.log(header + "Trying to open netcdf files" + ffgFileName +" " +ioe);
            }
            finally 
            { 
                if (null != netCdfFile)
                {
                    try
                    {
                        netCdfFile.close();
                    }
                    catch (IOException ioe) 
                    {
                        System.out.println(header + "Trying to close netcdf files" + ffgFileName+" " +ioe);
                    }
                }   

            }
        }
    }
//  -----------------------------------------------------------------------------------------------


    private void createLidToLatLongMap()
    {
        if(_lidToLatLonPointMap == null)
        {
            String whereClause = "order by lid";
            LocationTable locationTable = new LocationTable(_db);
            LocationRecord locationRecord = new LocationRecord();
            List locationList = null;
            try
            {
                locationList = locationTable.select(whereClause);
            }
            catch(SQLException e)
            {
                _logger.log("Error  while reading location table for lat/long values:" + e);
            }

            if(locationList != null)
            {
                LatLonPoint latLonPoint;
                _lidToLatLonPointMap = new HashMap();
                for(int i=0; i < locationList.size(); i++)
                {
                    locationRecord = (LocationRecord) locationList.get(i);
                    String lid = locationRecord.getLid();
                    double lat = locationRecord.getLat();
                    double lon = locationRecord.getLon();
                    latLonPoint = new LatLonPoint(lat, lon);
                    _lidToLatLonPointMap.put(lid, latLonPoint);
                }
            }
        }
    }

//  -----------------------------------------------------------------------------------------------
    private void readNetCdfFileAndCreateXmargGridForMentionedHr(NetcdfFile netCdfFile, int hour)
    {
        createLidToLatLongMap();

        try
        {
            //read the netcdf file for values of x & y 
            //ie., the numOfLocalHrapCols & numOfLocalHrapRows
            int numOfLocalHrapCols = netCdfFile.findDimension("x").getLength() ;
            int numOfLocalHrapRows = netCdfFile.findDimension("y").getLength() ;
            //          System.out.println("[y, x] ie., [numOfLocalHrapRows ,numOfLocalHrapCols]:["+numOfLocalHrapRows+","+ numOfLocalHrapCols+"]");

            //read netcdf file for all the ffg values in an array 
            //ie., read the netcdf file for values of image
            ucar.ma2.Array imageArray =  (ucar.ma2.Array) netCdfFile.read("image", false);
            //       System.out.println("Array:["+imageArray+"]");

            // read netcdf file for valid time 
            Variable validTimeVariable = netCdfFile.findVariable("validTime");
            ucar.ma2.Array validTimeArray = validTimeVariable.read();

            IndexIterator indexIterator = validTimeArray.getIndexIterator();
            long validTimeLong = indexIterator.getLongNext();

            //         System.out.println("valid time:"+ validTimeArray +" long:"+ validTimeLong);

            // read netcdf file for latitude and longitude values of the grid area.
            // this is the NW point (top left corner) point in the whole grid area represented by the netcdf file 
            ucar.nc2.Attribute lat = (ucar.nc2.Attribute) netCdfFile.findGlobalAttribute("lat00");
            ucar.nc2.Attribute lon = (ucar.nc2.Attribute) netCdfFile.findGlobalAttribute("lon00");

            double latitude = lat.getNumericValue().doubleValue();
            double longitude = lon.getNumericValue().doubleValue();

            //        System.out.println("[latitude,longitude]:["+latitude+","+ longitude+"]");

            createHrapRowColToLatLonConverter(latitude, longitude,numOfLocalHrapRows );

            createXmrgGridAndSetGridValues(validTimeLong, numOfLocalHrapRows, numOfLocalHrapCols, imageArray, hour);
        }
        catch(Exception e)
        {
            System.out.println("Error while reading netcdf file:"+ e);
            e.printStackTrace();
        }
    }

//  -----------------------------------------------------------------------------------------------

    private void createXmrgGridAndSetGridValues(long validTime, int numOfLocalHrapRows, 
            int numOfLocalHrapCols, ucar.ma2.Array array, int hour )
    {

        XmrgGrid xmargGrid =  new XmrgGrid(validTime, _baseNationalHrapRowSW, _baseNationalHrapColSW, numOfLocalHrapRows, numOfLocalHrapCols);
     
        //create a xmrggrid with the given validtime
        
        switch(hour)
        {
            case 1:
                _xmrgGridFor1hrFFG = xmargGrid;              
                break;
            case 3:
                _xmrgGridFor3hrFFG = xmargGrid;
                break;
            case 6:
                _xmrgGridFor6hrFFG = xmargGrid;
                break;
            default:
                _xmrgGridFor1hrFFG = xmargGrid;
        }


        //populate the xmrggrid with values read from netcdf file
        //note: netcdf values are is read from NW and it is left to right in consecutive line ( top to bottom ) ie., usual way
        //      xmrggrid values are read from SW and it is also left to right in consecutive line  ( bottom to top) ie., not usual way
        int xmrgRowNumber = 0;
        IndexIterator indexIterator = array.getIndexIterator();

        for(int rowIndex=numOfLocalHrapRows -1; rowIndex >=0;  rowIndex--)
        {
            for(int columnIndex=0; columnIndex < numOfLocalHrapCols; columnIndex++ )
            {
                if(indexIterator.hasNext())
                {

                   // double value = indexIterator.getDoubleNext();
                    int value = indexIterator.getIntNext();
                    double convertedValue = convertFfgValue(value);
                    
                    //  System.out.println("Try to enter:["+ rowIndex+","+ columnIndex+"]" + value);
                    xmargGrid.setValue(rowIndex + _baseNationalHrapRowSW, columnIndex + _baseNationalHrapColSW, convertedValue);
                }
            }
            xmrgRowNumber++;
        }

    }
    
//  -----------------------------------------------------------------------------------------------
    /*
     * This method is replicated from ffconv method in ffconv.c
     */
    private double convertFfgValue(double originalValue)
    {
        
        String header = "PrecipDataManage.convertFfgValue(): ";
        
        double signFixedValue = originalValue;
        double returnValue = 0.0;
        final double lowerThreshold = 201;
        final double upperThreshold = 253;
        final double mFactor = 6;
        final double inchPerMM = 0.03937;
        
   
        // These are values from image file and they are byte counts.  
        //These are similar to an enhanced satellite image.  
        //So normally the values go from 0 to 255 (256 values).  
        //But, for values beyond 127, they turn negative.  
        //So value 128 is really 128 - 256 = -128 in the file
        //hence you will need to add 256 for negative values to get the actual positive value
        //This actual value is what is needed.
        if (signFixedValue < 0)
        {
            signFixedValue += 256;
        }

        //convert the coded value to actual precip amounts in inches
        if (signFixedValue == 0) 
        {
            returnValue = DbTable.getNullDouble();
        }
        else if (signFixedValue <= lowerThreshold)
        {
            returnValue = signFixedValue - 1;
            returnValue *= inchPerMM;
        }
        else if (signFixedValue <= upperThreshold)
        {
            returnValue = (lowerThreshold - 1) + ((signFixedValue - 1) - (lowerThreshold - 1))* mFactor ;
            returnValue *= inchPerMM;
        }
        else if (signFixedValue >= upperThreshold)
        {
            returnValue = DbTable.getNullDouble();
        }
        
      

        return returnValue;
   }
//  -----------------------------------------------------------------------------------------------
    private void createHrapRowColToLatLonConverter(double latitude, double longitude, int numOfLocalHrapRows)
    {
        // with the NW(top left corner) latitude/longitude info 
        // get the national hrap cordinates for the NW (top left corner)
        //TODO examine the negation of longitude - ask Bryon 

        RowColumnPoint nationalHrapCoorinatesNW = HrapRowColToLatLonConverter.getNationalRowColumnPoint(latitude,-longitude);

        // with the national hrap coordinates for the NW (top left corner) 
        // and the num of local hrap rows and num of local hrap cols info read from the netcdf file
        // determine the base national hrap row and base national hrap col 
        // ie., the base national SW point's row number and column number
        // which is the bottom left corner's info
        _baseNationalHrapRowSW = (int) (nationalHrapCoorinatesNW.getRow() - numOfLocalHrapRows) ;
        _baseNationalHrapColSW = (int) nationalHrapCoorinatesNW.getCol();

        if(_hrapRowColToLatLonConverter == null)
        {
            _hrapRowColToLatLonConverter = new HrapRowColToLatLonConverter(_baseNationalHrapRowSW, _baseNationalHrapColSW);
        }
//      LatLonPoint latLong = _hrapRowColToLatLonConverter.getLatLonPoint(numOfLocalHrapRows -1, 0);
        //      System.out.println("lat long:"+ latLong);
    }

//  -----------------------------------------------------------------------------------------------

    public void setTohPrecipDataForThisLidForMentionedHour(String lid, PrecipData precipData, int hour)
    {
        Map<String, List<RegularObsTimeSeries>> lidToTohPrecipTimeSeriesMap = null;

        switch(hour)
        {
            case 1:
                lidToTohPrecipTimeSeriesMap = _lidToTohPrecipMapFor1Hr;
                break;
            case 3:
                lidToTohPrecipTimeSeriesMap = _lidToTohPrecipMapFor3Hr;
                break;
            case 6:
                lidToTohPrecipTimeSeriesMap = _lidToTohPrecipMapFor6Hr;
                break;
            case 24:
                lidToTohPrecipTimeSeriesMap = _lidToTohPrecipMapFor24Hr;
                break;
        }
        if (lidToTohPrecipTimeSeriesMap.size() > 0)
        {
            List<RegularObsTimeSeries> regularObsTimeSeriesForAllTSList = lidToTohPrecipTimeSeriesMap.get(lid);

            int count = 0;
            if( regularObsTimeSeriesForAllTSList != null )
            {
                for(RegularObsTimeSeries regularObsTimeSeries: regularObsTimeSeriesForAllTSList)
                {
                    String paramCodeString = regularObsTimeSeries.getDescriptor().getShef_param_code();
                    ParamCode paramCode = new ParamCode(paramCodeString);

                    List timeValuePairsForParticularTSList = regularObsTimeSeries.getTimeValuePairList(true);
                    int indexOfLastTimeValuePair = timeValuePairsForParticularTSList.size() - 1;
                    
                    //The last period is a partially-completed period.
                    //The previous period the most recent complete period.
                    int indexOfOfLastCompletedPeriod = indexOfLastTimeValuePair - 1;

                    TimeValuePair timeValuePair = (TimeValuePair) timeValuePairsForParticularTSList.get(indexOfOfLastCompletedPeriod);
                    double precipTotal = timeValuePair.getValue();

                    boolean checkPrevValue = true;
                    if(count == 0)
                    {
                        checkPrevValue = false;
                    }

                    if(precipTotal != _missingDoubleValue)
                    {
                        if(checkPrevValue)
                        {
                            if(precipTotal > precipData.getTohPrecipByHour(hour) )
                            {
                                precipData.setTohPrecipByHour(hour, precipTotal);
                                precipData.setTohParamCodeByHour(hour, paramCode);
                            }
                        }
                        else
                        {
                            precipData.setTohPrecipByHour(hour, precipTotal);
                            precipData.setTohParamCodeByHour(hour, paramCode);
                        }
                    }
                    else // make sure that the param code is available for missing data
                    {
                        precipData.setTohParamCodeByHour(hour, paramCode);
                    }
                    count++;
                }//end of for loop
            }// end of if(regularobstimeseriesforallTSlist != null)
        }
    }


//  -----------------------------------------------------------------------------------------------

    public void setLatestPrecipDataForThisLid(String lid, PrecipData precipData)
    {
        if (_lidToLatestPrecipMap != null && _lidToLatestPrecipMap.size() > 0)
        {
            List<RegularObsTimeSeries> regularObsTimeSeriesForAllTSList = _lidToLatestPrecipMap.get(lid);

            if( regularObsTimeSeriesForAllTSList != null )
            {
                for(int i=0; i < regularObsTimeSeriesForAllTSList.size(); i++)
                {
                    RegularObsTimeSeries regularObsTimeSeriesForParticularTS = regularObsTimeSeriesForAllTSList.get(i);

                    String paramCodeString = regularObsTimeSeriesForParticularTS.getDescriptor().getShef_param_code();
                    ParamCode paramCode = new ParamCode(paramCodeString);

                    precipData.setLatestPrecipParamCode(paramCode);

                    List<TimeValuePair> timeValuePairsForParticularTSList = regularObsTimeSeriesForParticularTS.getTimeValuePairList(true);

                    int indexOfLastTimeValuePair = timeValuePairsForParticularTSList.size() - 1;
                    boolean checkPrevValue = true;
                    if(i == 0)
                    {
                        checkPrevValue = false;
                    }
                    
                    
                    TimeValuePair timeValuePair = (TimeValuePair) timeValuePairsForParticularTSList.get(indexOfLastTimeValuePair);
                    setLatestPrecipForHrMentioned(0.5f, timeValuePair, precipData, checkPrevValue);
                    
                    timeValuePair = (TimeValuePair) timeValuePairsForParticularTSList.get(indexOfLastTimeValuePair - 1);
                    setLatestPrecipForHrMentioned(1, timeValuePair, precipData, checkPrevValue);

                    timeValuePair = (TimeValuePair) timeValuePairsForParticularTSList.get(indexOfLastTimeValuePair - 3);
                    setLatestPrecipForHrMentioned(3, timeValuePair, precipData, checkPrevValue);

                    timeValuePair = (TimeValuePair) timeValuePairsForParticularTSList.get(indexOfLastTimeValuePair - 6);
                    setLatestPrecipForHrMentioned(6, timeValuePair, precipData, checkPrevValue);

                    timeValuePair = (TimeValuePair) timeValuePairsForParticularTSList.get(indexOfLastTimeValuePair - 12);
                    setLatestPrecipForHrMentioned(12, timeValuePair, precipData, checkPrevValue);                    

                    timeValuePair = (TimeValuePair) timeValuePairsForParticularTSList.get(indexOfLastTimeValuePair - 18);
                    setLatestPrecipForHrMentioned(18, timeValuePair, precipData, checkPrevValue);   
                    
                    timeValuePair = (TimeValuePair) timeValuePairsForParticularTSList.get(indexOfLastTimeValuePair - 24);
                    setLatestPrecipForHrMentioned(24, timeValuePair, precipData, checkPrevValue);                    
                      
                }
            }
        }
    }

//  ------------------------------------------------------------------------------------------------------------

    public void setFFGDataForThisLid(String lid, PrecipData precipData)
    {
        if(_lidToLatLonPointMap != null)
        {
            LatLonPoint latLon = (LatLonPoint) _lidToLatLonPointMap.get(lid);
            if(latLon != null)
            {
                double lat = latLon.getLat();
                double lon = latLon.getLon();
                //   System.out.println("For lid:"+ lid + " [lat,lon]:"+ lat +"," + lon);

                RowColumnPoint rowColumnPoint = HrapRowColToLatLonConverter.getNationalRowColumnPoint(lat, lon);
                int rowPoint = (int) rowColumnPoint.getRow();
                int colPoint = (int) rowColumnPoint.getCol();

                double value = _xmrgGridFor1hrFFG.getValue(rowPoint, colPoint);
                if(value != XmrgGrid.OFF_GRID_VALUE)
                    precipData.setFFG1Hr(value);

                value = _xmrgGridFor3hrFFG.getValue(rowPoint, colPoint);
                if(value != XmrgGrid.OFF_GRID_VALUE)
                    precipData.setFFG3Hr(value);

                value = _xmrgGridFor6hrFFG.getValue(rowPoint, colPoint);
                if(value != XmrgGrid.OFF_GRID_VALUE)
                    precipData.setFFG6Hr(value );

                if(value == 0.00)
                {
                    System.out.println("For lid:" + lid + " value is:"+ value);
                }
            }
        
        }
    }

//  -----------------------------------------------------------------------------------------------
    public void set1Hr3Hr6HrRatio(PrecipData precipData)
    {         
        precipData.setRatio1Hr( determinePrecipFFGRatio(precipData.getLatestPrecip1Hr(), precipData.getFFG1Hr()));
        precipData.setRatio3Hr( determinePrecipFFGRatio(precipData.getLatestPrecip3Hr(), precipData.getFFG3Hr()));
        precipData.setRatio6Hr( determinePrecipFFGRatio(precipData.getLatestPrecip6Hr(), precipData.getFFG6Hr()));
        return;
    }
//  -----------------------------------------------------------------------------------------------

    private int determinePrecipFFGRatio(double precip, double ffg)
    {
        int ratio = DbTable.getNullInt();
        
        if( (ffg != 0.00 && ffg != DbTable.getNullDouble()) 
                && precip != DbTable.getNullDouble()) //to avoid divide by 0 value 
        {
            ratio = (int)((precip / ffg) * 100);
        }
        
        return ratio;
    }
    
//  -----------------------------------------------------------------------------------------------

    public Map<String, List<RegularObsTimeSeries>> readPrecipFile(String fileName, PDCFileReader pdcFileReader, Map<String, List<RegularObsTimeSeries>> lidToLatestPrecipMap)
    {
        if(pdcFileReader == null)
        {
            pdcFileReader = new PDCFileReader(_pdcPreprocessorFilesDir + fileName);
        }
        else
        {
            pdcFileReader.resetObsTimeSeriesList();
        }
        
        lidToLatestPrecipMap = getLidToPrecipTotalsMapFromPreprocessorFile(pdcFileReader, lidToLatestPrecipMap);
        
        return lidToLatestPrecipMap;
    }
    
    public void readPDCPreprocessorPrecipFiles()
    {
        String header = "PrecipDataManager.readPDCPreprocessorPrecipFiles(): ";

        //determine the last update time for PDC Precip data
        setPdcFileUpdateLongTime(0);
        File file = new File(_pdcPreprocessorFilesDir + "/PrecipInstant.dat");
        if (file != null)
        {
            setPdcFileUpdateLongTime(file.lastModified());
            System.out.println(header + " the last modified file time = " +
                                 DbTimeHelper.getDateTimeStringFromLongTime(getPdcFileUpdateLongTime()));
            file = null;
        }
  
        //read the 
        _lidToLatestPrecipMap = readPrecipFile("/PrecipInstant.dat", _pdcFileReaderForPrecipInstant, _lidToLatestPrecipMap);
        checkMemoryAndGarbageCollectLog(header +"Here after reading precip instant");
                
        _lidToTohPrecipMapFor1Hr = readPrecipFile("/Precip1Hour.dat", _pdcFileReaderForPrecip1Hour, _lidToTohPrecipMapFor1Hr);
        checkMemoryAndGarbageCollectLog(header +"Here after reading precip 1 hour :");

        _lidToTohPrecipMapFor3Hr = readPrecipFile("/Precip3Hour.dat", _pdcFileReaderForPrecip3Hour, _lidToTohPrecipMapFor3Hr);
        checkMemoryAndGarbageCollectLog(header +"Here after reading precip 3 hour :");

        _lidToTohPrecipMapFor6Hr = readPrecipFile("/Precip6Hour.dat", _pdcFileReaderForPrecip6Hour, _lidToTohPrecipMapFor6Hr);
        checkMemoryAndGarbageCollectLog(header +"Here after reading precip 6 hour :");

        _lidToTohPrecipMapFor24Hr = readPrecipFile("/Precip24Hour.dat", _pdcFileReaderForPrecip24Hour, _lidToTohPrecipMapFor24Hr);
        checkMemoryAndGarbageCollectLog(header +"Here after reading precip 24 hour :");
    }
    
    public void checkMemoryAndGarbageCollectLog(String message)
    {
        Runtime runtime = Runtime.getRuntime();
        String text = null;
        
        long freeMemory = runtime.freeMemory();
        
        final long SMALL_AMOUNT_OF_MEMORY = 30000000;
        
        if (freeMemory < SMALL_AMOUNT_OF_MEMORY)
        {
            text =  message + "Free:" + freeMemory + " Max:"+ runtime.maxMemory()+ " total:"+ runtime.totalMemory();
            MemoryLogger.log(text);
            
            runtime.gc();
        }
    }
//  ------------------------------------------------------------------------------------------------------------

    public void readPrecipData(List rowDataList, String tag)
    {
        String header = "PrecipDataManager.readLatestPrecipData(): ";
       
        System.out.println(header + "Reading precip data");
        readPDCPreprocessorPrecipFiles();
        
        readFFGInfoForMentionedHr(1);
        readFFGInfoForMentionedHr(3);
        readFFGInfoForMentionedHr(6);

        for(int i=0; i < rowDataList.size(); i++)
        {
            String lid = null;
            PrecipData precipData = new PrecipData();
            if(tag.compareTo("PRECIP") == 0)
            {
                PrecipMonitorJTableRowData precipMonitorRowData = (PrecipMonitorJTableRowData) rowDataList.get(i);
                lid = (String) precipMonitorRowData.getLid();
                precipMonitorRowData.setPrecipData(precipData);
            }
            else
            {
                RiverMonitorJTableRowData riverMonitorRowData = (RiverMonitorJTableRowData) rowDataList.get(i);
                lid = (String) riverMonitorRowData.getLid();
                riverMonitorRowData.setPrecipData(precipData);
            }
            setLatestPrecipDataForThisLid(lid, precipData);
            
            setFFGDataForThisLid(lid, precipData);
            
            setTohPrecipDataForThisLidForMentionedHour( lid,  precipData, 1 );
            setTohPrecipDataForThisLidForMentionedHour( lid,  precipData, 3 );
            setTohPrecipDataForThisLidForMentionedHour( lid,  precipData, 6 );
            setTohPrecipDataForThisLidForMentionedHour( lid,  precipData, 24 );
            
            setFfgDifferenceValues(precipData);

            set1Hr3Hr6HrRatio(precipData);

        }//end of for loop
        checkMemoryAndGarbageCollectLog(header +"End of read precip method " );
    }
    

//  ------------------------------------------------------------------------------------------------------------
    private void setFfgDifferenceValues(PrecipData precipData)
    {
        precipData.setDiff1Hr(determineFfgDifferenceValue(precipData.getLatestPrecip1Hr(), precipData.getFFG1Hr()));
        precipData.setDiff3Hr(determineFfgDifferenceValue(precipData.getLatestPrecip3Hr(), precipData.getFFG3Hr()));
        precipData.setDiff6Hr(determineFfgDifferenceValue(precipData.getLatestPrecip6Hr(), precipData.getFFG6Hr()));
    }

//  ------------------------------------------------------------------------------------------------------------

    private double determineFfgDifferenceValue(double precipValue, double ffgValue)
    {

        double diffValue = DbTable.getNullDouble();

        if ( (DbTable.isNull(precipValue)) ||  (DbTable.isNull(ffgValue)) )
        {
            //no good
        }
        else
        {
            diffValue = precipValue - ffgValue;
        }

        return diffValue;
    }

   
    //  ------------------------------------------------------------------------------------------------------------

    private void setLatestPrecipForHrMentioned(float hour, TimeValuePair timeValuePair, PrecipData precipData, boolean checkPrevValue)
    {
        double precipTotal = timeValuePair.getValue();
        
        if(precipTotal != _missingDoubleValue) // used since the pdc preprocessor represents missing as -9999.0
        {
            if(checkPrevValue)
            {
                if(precipTotal > precipData.getLatestPrecipByHour(hour))
                    precipData.setLatestPrecipByHour(hour, precipTotal);
            } 
            else
                precipData.setLatestPrecipByHour(hour, precipTotal);
        }
    }


    public void setPdcFileUpdateLongTime(long pdcFileUpdateLongTime)
    {
        _pdcFileUpdateLongTime = pdcFileUpdateLongTime;
    }
    
    public long getPdcFileUpdateLongTime()
    {
        return _pdcFileUpdateLongTime;
    }


    //--------------------------------------------------------------------------------
    class NetCdfFileNameFilter implements FilenameFilter
    {
        String _pattern = "0"; // <---removed the leading underscore "_0"
        public NetCdfFileNameFilter(int hour)
        {
            _pattern += hour+".ffg";
        }
        public boolean accept(String name)
        {
            return name.contains(_pattern);
        }
        public boolean accept(File arg0, String name)
        {
            return name.contains(_pattern);
        }
        public String toString()
        {
            return _pattern;
        }

    }

}
