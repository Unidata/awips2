package ohd.hseb.pdc_pp;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;


public class PDCFileReader
{
    private List _obsTimeSeriesList;
    private String _fullFilePath = null;
     
    private int _numberOfValues = 0;
    private long _startTimeInSeconds = 0;
    private int _timeIntervalInSeconds  = 0;
    private Map _durCodetoDurMap = null;
    private boolean _debug = false;
    
     private BufferedReader _reader = null;
    
     private static final int SECONDS_PER_HOUR = 3600;
 
    
    // ----------------------------------------------------------------------------------
    public PDCFileReader(String fullFilePath)
    {       
        this(fullFilePath, PDCFileReader.getDurCodeToDurMap());
    }
    
    // ----------------------------------------------------------------------------------
    public PDCFileReader(String fullFilePath, Map durCodeToDurMap)
    {
        _fullFilePath = fullFilePath;
        _durCodetoDurMap = durCodeToDurMap;
        _obsTimeSeriesList = new ArrayList();
    }
    
    public void resetObsTimeSeriesList()
    {
        if(_obsTimeSeriesList == null)
            _obsTimeSeriesList = new ArrayList();
        else
            _obsTimeSeriesList.clear();
        
        Runtime.getRuntime().gc();
    }
    // ----------------------------------------------------------------------------------
    public List read()
    {
        String header = "PDCFileReader.read() ";
      //  boolean done = false;
        
        
        try
        {
            FileReader fileReader = new FileReader(_fullFilePath);
            _reader = new BufferedReader(fileReader);
  
            readFileHeader();
              
   
            String line = "DUMMY";
            
            boolean done = false;
            
            while ( ! done )
            {
                 try
                 {    
                    RegularObsTimeSeries timeSeries = readRegularStationData();
                    if (timeSeries != null)
                    {
                      //  debug_logline(header + 
                      //          " adding timeSeries to _obsTimeSeriesList " +
                      //          timeSeries.getDescriptor() +
                      //          " value pair list size() = " +
                      //          timeSeries.getTimeValuePairList(true).size());
                        
                        _obsTimeSeriesList.add(timeSeries);
                   
                    }
                    else
                    {
                        debug_logline(header + "timeSeries is null");    
                    }
              
                 }
                 catch (EOFException e)
                 {
                     done = true;
                 }
                 catch (Exception e)
                 {              
                     System.err.println(header + 
                                         "Exception reading file " + _fullFilePath + 
                                         "  "  + e.getMessage());
                     e.printStackTrace();
                    
                     done = true;
                 }
            } //end while
            
         //   System.out.println(header + "_obsTimeSeriesList contains " +
         //                          _obsTimeSeriesList.size() + " time series. ");
            
             
            if (_reader != null)
            {
                _reader.close();
                _reader = null;
            }
            
        } //end try
        catch (FileNotFoundException e)
        {
            System.err.println("Unable to open" + _fullFilePath);
            _obsTimeSeriesList = null;
        }
        catch (IOException e)
        {
            System.err.println("Unable to open" + _fullFilePath);
            _obsTimeSeriesList = null;
        }
        
        return _obsTimeSeriesList;
    }
   
    // ----------------------------------------------------------------------------------
    private void readFileHeader() throws IOException
    {
        String header = "PDCFileReader.readFileHeader(): ";
        String line = null;
 
        //ignore the first 6 lines
        line = _reader.readLine();
        debug_logline(header + " line 1 = " + line);
        
        line = _reader.readLine();
        debug_logline(header + " line 2 = " + line);
        
        line = _reader.readLine();
        debug_logline(header + " line 3 = " + line);
        
        line = _reader.readLine();
        debug_logline(header + " line 4 = " + line);
        
        line = _reader.readLine();
        debug_logline(header + " line 5 = " + line);
        
        line = _reader.readLine();
        debug_logline(header + " line 6 = " + line);
        
        String fileHeaderString = _reader.readLine();
        
        StringTokenizer tokenizer = new StringTokenizer(fileHeaderString);
            
        _numberOfValues = Integer.parseInt(tokenizer.nextToken());
        
        _startTimeInSeconds = Long.parseLong(tokenizer.nextToken());
        
        _timeIntervalInSeconds = Integer.parseInt(tokenizer.nextToken());
  
        
        debug_logline("_numberOfHours = " + _numberOfValues + 
                           " _startTimeInSeconds = " + _startTimeInSeconds +
                           " _timeIntervalInSeconds = " + _timeIntervalInSeconds );
        
    }
    // ----------------------------------------------------------------------------------
    private RegularObsTimeSeries readRegularStationData() throws Exception
    {
        
        String header = "PDCFileReader.readRegularStationData() ";
        RegularObsTimeSeriesDescriptor descriptor = readStationHeader();
        
        debug_log(header + "descriptor = " + descriptor + "\n");
        
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
    
    private RegularObsTimeSeriesDescriptor readStationHeader() throws IOException
    {
        String header = "PDCFileReader.readStationHeader(): ";
        
        
        String stationHeaderLine = _reader.readLine();   
        
        
        
        StringTokenizer headerTokenizer = new StringTokenizer(stationHeaderLine);
      //  Scanner headerScanner = new Scanner(stationHeaderLine);
        
        String deviceLine = _reader.readLine();
        
        if ( (stationHeaderLine == null) || (deviceLine == null) )
        {
            throw new EOFException();
        }
        
        StringTokenizer deviceLineTokenizer = new StringTokenizer(deviceLine);
        
     //  Scanner deviceScanner = new Scanner(deviceLine);
        
        RegularObsTimeSeriesDescriptor descriptor = new RegularObsTimeSeriesDescriptor();
        
        long startTimeInMillis = _startTimeInSeconds * 1000;
        long timeIntervalInMillis = _timeIntervalInSeconds * 1000;
        long endTimeInMillis = startTimeInMillis +  ((_numberOfValues-1) * timeIntervalInMillis);
        
        descriptor.setStartTime(startTimeInMillis);
        descriptor.setEndTime(endTimeInMillis) ;
        
        String lid = headerTokenizer.nextToken();  
        descriptor.setLid(lid);
        
        String hsa = headerTokenizer.nextToken();
        String paramCode = headerTokenizer.nextToken();
        
        debug_logline("lid = " + lid +
                " hsa =  " + hsa +
                " paramCode = " + paramCode);
    
        
        //Work on the ParamCode.  It is in multiple parts
        setPedtse(descriptor, paramCode);
        
        double elevation = Double.parseDouble(headerTokenizer.nextToken());   
        double lat = Double.parseDouble(headerTokenizer.nextToken());
        double lon = Double.parseDouble(headerTokenizer.nextToken());   
        String fcst_pointString = headerTokenizer.nextToken();
        String name = restOfLine(headerTokenizer);
        name = name.trim();
        
        debug_logline(header + "name = :" + name + ":");
        
        
        // parse out the device line
        String dcpString = deviceLineTokenizer.nextToken();
        String observerString = deviceLineTokenizer.nextToken();  
        String telemTypes = restOfLine(deviceLineTokenizer);
        
        debug_logline("");
        debug_logline("dcpString = " + dcpString +
                " observerString =  " + observerString +
                " telemTypes String = " + telemTypes);
        
        // descriptor.setHsa //  can't do it easily, but don't need it anyway
        
        //     System.out.println(header + " descriptor = " + descriptor);
        
        return descriptor;
    }
 
    //  ---------------------------------------------------------------------------------- 
    String restOfLine(StringTokenizer tokenizer)
    {
        String header = "PDCFileReader.restOfLine(): ";
        
        StringBuffer buffer = new StringBuffer();
        
        int count = 0;
        
        while (tokenizer.hasMoreTokens())
        {
            buffer.append(tokenizer.nextToken() + " "); 
            count++;
        }
       
        //get rid of extra " " at the end
        if (count > 0)
        {
            int spaceIndex = buffer.lastIndexOf(" ");
            buffer.delete(spaceIndex, spaceIndex+1);
        }
        
        String restOfLineString = buffer.toString();
        
    //    debug_log(header + " restofLine = " + restOfLineString + "\n");
        
        return restOfLineString;
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
    
    private void readValues(RegularObsTimeSeries timeSeries) throws IOException
    {
        String header = "PDCFileReader.readValues(): ";
        long time = _startTimeInSeconds * 1000;
        long incrementTime = timeSeries.getTimeStepIntervalInMillis();
        
        RegularObsTimeSeriesDescriptor desc = timeSeries.getDescriptor();
        
        double value = 0.0;
        
        List pairList = new ArrayList();
        
        debug_logline(header + "attempting to read " + _numberOfValues + " values.");
        
        String valueLine = _reader.readLine(); 
        StringTokenizer tokenizer = new StringTokenizer(valueLine);
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
                     value = Double.parseDouble(tokenizer.nextToken());
                }
                
                     debug_log("value[" + i +"] = " + value);
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
   
        PDCFileReader reader = new PDCFileReader (fileName, durCodeToDurMap);
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


