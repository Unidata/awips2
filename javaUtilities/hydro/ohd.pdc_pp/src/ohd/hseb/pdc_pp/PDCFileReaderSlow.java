package ohd.hseb.pdc_pp;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Map;
import java.util.Scanner;


public class PDCFileReaderSlow
{
    private List _obsTimeSeriesList;
    private String _fullFilePath = null;
    private Scanner _scanner = null;
    
    private int _numberOfValues = 0;
    private long _startTimeInSeconds = 0;
    private int _timeIntervalInSeconds  = 0;
    private Map _durCodetoDurMap = null;
    private boolean _debug = false;
    


    private static final int SECONDS_PER_HOUR = 3600;
 
    
    // ----------------------------------------------------------------------------------
    public PDCFileReaderSlow(String fullFilePath)
    {       
        this(fullFilePath, PDCFileReaderSlow.getDurCodeToDurMap());
    }
    
    // ----------------------------------------------------------------------------------
    public PDCFileReaderSlow(String fullFilePath, Map durCodeToDurMap)
    {
        _fullFilePath = fullFilePath;
        _durCodetoDurMap = durCodeToDurMap;
        
        _obsTimeSeriesList = new ArrayList();
    }
    // ----------------------------------------------------------------------------------
    public List read()
    {
        String header = "PDCFileReaderSlow.read() ";
      //  boolean done = false;
        
        try
        {
            FileReader fileReader = new FileReader(_fullFilePath);
            BufferedReader reader = new BufferedReader(fileReader);
            _scanner = new Scanner ( reader );
 
            readFileHeader();
              
            while ( _scanner.hasNext() )
            {
                 try
                 {    
                    RegularObsTimeSeries timeSeries = readRegularStationData();
                    if (timeSeries != null)
                    {
                       //  debug_logline(header + 
                       //         " adding timeSeries to _obsTimeSeriesList " +
                       //         timeSeries.getDescriptor() +
                       //         " value pair list size() = " +
                       //         timeSeries.getTimeValuePairList(true).size());
                        
                        _obsTimeSeriesList.add(timeSeries);
                   
                    }
                    else
                    {
                        debug_logline(header + "timeSeries is null");    
                    }
              
                 }
                 catch (Exception e)
                 {              
                     System.err.println(header + 
                                         "Exception reading file " + _fullFilePath + 
                                         "  "  + e.getMessage());
                     e.printStackTrace();
                    // done = true;
                 }
            }
            
         //   System.out.println(header + "_obsTimeSeriesList contains " +
         //                          _obsTimeSeriesList.size() + " time series. ");
            
         
        }
        catch (FileNotFoundException e)
        {
            System.err.println("Unable to open" + _fullFilePath);
            _obsTimeSeriesList = null;
        }
        
        if (_scanner != null)
        {
            _scanner.close();
        }
        
        return _obsTimeSeriesList;
    }
   
    // ----------------------------------------------------------------------------------
    private void readFileHeader()
    {
        String header = "PDCFileReaderSlow.readFileHeader(): ";
        String line = null;
 
        //ignore the first 6 lines
        line = _scanner.nextLine();
        debug_logline(header + " line 1 = " + line);
        
        line = _scanner.nextLine();
        debug_logline(header + " line 2 = " + line);
        
        line = _scanner.nextLine();
        debug_logline(header + " line 3 = " + line);
        
        line = _scanner.nextLine();
        debug_logline(header + " line 4 = " + line);
        
        line = _scanner.nextLine();
        debug_logline(header + " line 5 = " + line);
        
        line = _scanner.nextLine();
        debug_logline(header + " line 6 = " + line);
        
        String fileHeaderString = _scanner.nextLine();
        Scanner fileHeaderScanner = new Scanner(fileHeaderString);
     
        
        _numberOfValues = fileHeaderScanner.nextInt();
        
        _startTimeInSeconds = fileHeaderScanner.nextLong();
        
        _timeIntervalInSeconds = fileHeaderScanner.nextInt();
  
        
        debug_logline("_numberOfHours = " + _numberOfValues + 
                           " _startTimeInSeconds = " + _startTimeInSeconds +
                           " _timeIntervalInSeconds = " + _timeIntervalInSeconds );
        
    }
    // ----------------------------------------------------------------------------------
    private RegularObsTimeSeries readRegularStationData() throws Exception
    {
        
        String header = "PDCFileReaderSlow.readRegularStationData() ";
        RegularObsTimeSeriesDescriptor descriptor = readStationHeader();
        
        debug_log(header + "descriptor = " + descriptor);
        
        TimeSlotPolicy timeSlotPolicy = new HourlyTimeSlotPolicy();
        
        long timeIntervalInHours =  _timeIntervalInSeconds/SECONDS_PER_HOUR ;
         
        RegularObsTimeSeries timeSeries = new RegularObsTimeSeries(descriptor,
                                                                   timeIntervalInHours,
                                                                   timeSlotPolicy);
        
        try
        {
            readValues(timeSeries);
        }
        catch (Exception e)
        {
            System.err.println(header + 
                                "Error when reading values for descriptor = " +
                                descriptor);
            e.printStackTrace();
        }
        
      //  System.out.println(header + "returning timeSeries = " + timeSeries);
        return timeSeries;
    }
    
    
    // ----------------------------------------------------------------------------------
    
    private RegularObsTimeSeriesDescriptor readStationHeader()
    {
        
        String stationHeaderLine = _scanner.nextLine();     
        Scanner headerScanner = new Scanner(stationHeaderLine);
        
        String deviceLine = _scanner.nextLine();
        Scanner deviceScanner = new Scanner(deviceLine);
        
        RegularObsTimeSeriesDescriptor descriptor = new RegularObsTimeSeriesDescriptor();
        String header = "PDCFileReaderSlow.readStationHeader(): ";
        long startTimeInMillis = _startTimeInSeconds * 1000;
        long timeIntervalInMillis = _timeIntervalInSeconds * 1000;
        long endTimeInMillis = startTimeInMillis +  ((_numberOfValues-1) * timeIntervalInMillis);
        
        descriptor.setStartTime(startTimeInMillis);
        descriptor.setEndTime(endTimeInMillis) ;
        
        String lid = headerScanner.next();  
        descriptor.setLid(lid);
        
        String hsa = headerScanner.next();
        String paramCode = headerScanner.next();
        
        //Work on the ParamCode.  It is in multiple parts
        setPedtse(descriptor, paramCode);
        
        double elevation = headerScanner.nextDouble();   
        double lat = headerScanner.nextDouble();
        double lon = headerScanner.nextDouble();
        String fcst_pointString = headerScanner.next();
        String name = headerScanner.nextLine();
        name = name.trim();
        
        debug_logline(header + "name = :" + name + ":");
        
        
        // parse out the device line
        String dcpString = deviceScanner.next();
        String observerString = deviceScanner.next();  
        String telemTypes = deviceScanner.nextLine();
        
        debug_logline("");
        debug_logline("dcpString = " + dcpString +
                " observerString =  " + observerString +
                " telemTypes String = " + telemTypes);
        
        // descriptor.setHsa //  can't do it easily, but don't need it anyway
        
        //     System.out.println(header + " descriptor = " + descriptor);
        
        return descriptor;
    }
    
    //  ---------------------------------------------------------------------------------- 
    private void setPedtse(RegularObsTimeSeriesDescriptor descriptor, String paramCode)
    {
        
        int length = paramCode.length();
        
        try
        {
            descriptor.setPe(paramCode.substring(0, 2));

            String durCodeString = paramCode.substring(2, 3);
            String durString = (String) _durCodetoDurMap.get(durCodeString);
            short dur = 0;

            if (durString != null)
            {
                dur = Short.parseShort(durString);
            }

            descriptor.setDur(dur);

            String typeSource = paramCode.substring(3, 5);
            descriptor.setTs(typeSource);

            String extremum = paramCode.substring(5, 6);

            descriptor.setExtremum(extremum);
        }
        catch (Error e)
        {

        }
        
        return;
    }
    // ---------------------------------------------------------------------------------- 
    
    private void readValues(RegularObsTimeSeries timeSeries)
    {
        String header = "PDCFileReaderSlow.readValues(): ";
        long time = _startTimeInSeconds * 1000;
        long incrementTime = timeSeries.getTimeStepIntervalInMillis();
        
        RegularObsTimeSeriesDescriptor desc = timeSeries.getDescriptor();
        
        double value = 0.0;
        
        List pairList = new ArrayList();
        
        debug_logline(header + "attempting to read " + _numberOfValues + " values.");
        
        String valueLine = _scanner.nextLine(); 
        Scanner valueScanner = new Scanner(valueLine);
        boolean isMissing = false;  
        
        
        if (valueLine.indexOf("MISSING") > -1)
        {
            System.out.println(header + " time series " + desc.toString() + " is SET to MISSING!");
            desc.setMissing(true);
            isMissing = true;
        }
         
       
        for (int i = 0; i < _numberOfValues; i++)
        {
            try
            {
                if (isMissing)
                {
                    value = PDCFileWriter.MISSING;
                }
                else
                {
                    value = valueScanner.nextDouble();
                }
                
                //         debug_log("value[" + i +"] = " + value);
            }
            catch (InputMismatchException e1)
            {     
                debug_log("when i = " + i);
                debug_logline("read in a bad value from line = " + valueLine);
                
                throw (new InputMismatchException("bad read at entry # " +
                        i + " (starts at 0)" +
                        " value line = " + valueLine +
                        " TS Descriptor = " + desc ));
            }
                        
            TimeValuePair pair = new TimeValuePair(time, value);
            pairList.add(pair);
            
            time += incrementTime;
        }
        
        debug_logline("");
         
        timeSeries.addTimeValuePairList(pairList);
        
      //  int size = timeSeries.getTimeValuePairList(true).size();
        
    //    System.out.println(header + "timeSeries.getTimeValuePairList(true).size() = " + size );
        
        
        return; 
    }
    
    // ---------------------------------------------------------------------------------- 
    public static void main(String[] argArray)
    {
        String fileName = "/fs/hseb/ob72/wfo_rfc/whfs/local/data/pdc_pp/keep/cachedWind.dat";
        
        Map durCodeToDurMap = getDurCodeToDurMap();
   
        PDCFileReaderSlow reader = new PDCFileReaderSlow (fileName, durCodeToDurMap);
        reader.setDebug(true);
        
        reader.read();
       
        
    }
    // ----------------------------------------------------------------------------------
    private static Map getDurCodeToDurMap()
    {
        // used just for testing.  Real code uses database to access
        //shefdur table.
        Map durCodeToDurMap = new HashMap();
        
        addCodeToMap(durCodeToDurMap, "I", 0);
        addCodeToMap(durCodeToDurMap, "H", 1001);
        addCodeToMap(durCodeToDurMap, "B", 1002);
        addCodeToMap(durCodeToDurMap, "T", 1003);
        addCodeToMap(durCodeToDurMap, "F", 1004);
        addCodeToMap(durCodeToDurMap, "Q", 1006);
        addCodeToMap(durCodeToDurMap, "D", 2001);
        addCodeToMap(durCodeToDurMap, "Z", 5000);
        
        
        return durCodeToDurMap;
    }
    
    private static void addCodeToMap(Map durCodeToDurMap, String durCode, int duration)
    {
        durCodeToDurMap.put(durCode, duration + "");
    }
    /**
     * @param debug The debug to set.
     */
    public void setDebug(boolean debug)
    {
        this._debug = debug;
    }
    /**
     * @return Returns the debug.
     */
    public boolean getDebug()
    {
        return _debug;
    }
    
    // ---------------------------------------------------------------------------------- 
    private void debug_logline(String message)
    {
        debug_log(message + '\n');
        
        return;
    }
    // ----------------------------------------------------------------------------------
    private void debug_log(String message)
    {
        if (getDebug())
        {
            System.out.print(message);    
        }
        
        return;
    }
    // ----------------------------------------------------------------------------------
}


