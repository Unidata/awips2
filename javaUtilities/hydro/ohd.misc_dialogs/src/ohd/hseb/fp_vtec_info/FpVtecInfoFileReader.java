package ohd.hseb.fp_vtec_info;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
//import java.util.List;
import java.util.Map;
//import java.util.Scanner;
import java.util.StringTokenizer;
import ohd.hseb.fp_vtec_info.FpCurPrevVtec;

public class FpVtecInfoFileReader
{
    //private List _fpvtecinfoList;
    private String _fullFilePath = null;
    private boolean _debug = false;
    private BufferedReader _reader = null;
    private Map     _fpcurprevvtecMap = null;

    // ------------------------------------------------------------------------------
    public FpVtecInfoFileReader()
    {
    	    	
    }
    // ----------------------------------------------------------------------------------
    public FpVtecInfoFileReader(String fullFilePath)
    {
        _fullFilePath = fullFilePath;       
        _fpcurprevvtecMap = new HashMap();      
    }
    
    // ----------------------------------------------------------------------------------
    public Map read()
    {
        String header = "FpVtecInfoFileReader.read() ";		    
           
        try
        {
            FileReader fileReader = new FileReader(_fullFilePath);
            _reader = new BufferedReader(fileReader);
  
            readFileHeader();               
            
            boolean done = false;
            
            while ( ! done )
            {
                 try
                 {    
                    FpCurPrevVtec fpvtecinfo = readFpVtecData();
                    if (fpvtecinfo != null)
                    {
                       _fpcurprevvtecMap.put(fpvtecinfo.getLocId(), fpvtecinfo);
                   
                    }
                    else
                    {
                    	System.out.println("fpvtecinfo is null");
                        debug_logline(header + "fpvtecinfo is null");    
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
                               
             
            if (_reader != null)
            {
                _reader.close();
                _reader = null;
            }
            
        } //end try
        catch (FileNotFoundException e)
        {
            System.err.println("Unable to open" + _fullFilePath);
           
        }
        catch (IOException e)
        {
            System.err.println("Unable to open" + _fullFilePath);           
        }
        
        return _fpcurprevvtecMap;
    }
   
    // ----------------------------------------------------------------------------------
    private void readFileHeader() throws IOException
    {
        String header = "FpVtecInfoFileReader.readFileHeader(): ";
        String line = null;
 
        //ignore the first 4 lines in the fpvtecinfo.dat
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
         
    }
   
    // ----------------------------------------------------------------------------------
    
    private FpCurPrevVtec readFpVtecData() throws IOException
    {
        String header = "FpVtecInfoFileReader.readFpVtecData(): ";
        StringTokenizer  fpLineTokenizer = null;
        StringTokenizer  curVtecLineTokenizer = null;
        StringTokenizer  prevVtecLineTokenizer = null;
        StringTokenizer  observedTSLineTokenizer = null;
        StringTokenizer  fcstTSLineTokenizer = null;
        ArrayList obsValueTimeList = new ArrayList();
        ArrayList fcstValueTimeList = new ArrayList();
                                     
        // The first line includes fp information for specific location
        
        String stationFpDataLine = _reader.readLine();   
       
        if (stationFpDataLine == null)
        {
            throw new EOFException();
        }
        else
            fpLineTokenizer = new StringTokenizer(stationFpDataLine, ",");
       
       // The second line includes current VTEC information for specific location
        
        String stationCurVtecDataLine = _reader.readLine();
        if (stationCurVtecDataLine == null)
        {
            throw new EOFException();
        }
        else   
            curVtecLineTokenizer = new StringTokenizer(stationCurVtecDataLine, ",");
        
        
        // The third line includes previous VTEC information for specific location
        
        String stationPrevVtecDataLine = _reader.readLine();
       
        if (stationPrevVtecDataLine == null)
        {
            throw new EOFException();
        }
        else if (stationPrevVtecDataLine.equals("MSG"))
        {
        	//no previous event exist
        	System.out.println("No previous event exist.");        	
        }
        else
        {                  
            prevVtecLineTokenizer = new StringTokenizer(stationPrevVtecDataLine, ",");        
        }
        
        // The fourth line includes observed timesreis data
        String stationObservedTSLine = _reader.readLine();
        
        if (stationObservedTSLine == null)
        {
            throw new EOFException();
        }
        else if (stationObservedTSLine.equals("MSG"))
        {
        	//no observed TS exists
        	System.out.println("No Observed TimeSeries data exists.");        	
        }
        else
        {                   
            observedTSLineTokenizer = new StringTokenizer(stationObservedTSLine, ";");        
        }
        
        // The fifth line includes forecast timesreis data
        String stationForecastTSLine = _reader.readLine();
      
        if (stationForecastTSLine == null)
        {
            throw new EOFException();
        }
        else if (stationForecastTSLine.equals("MSG"))
        {
        	//no forecast TS exists
        	System.out.println("No Forecast TimeSeries data exists.");        	
        }
        else
        {                  
            fcstTSLineTokenizer = new StringTokenizer(stationForecastTSLine, ";");        
        }
                
        FpCurPrevVtec descriptor = new FpCurPrevVtec();
        
        // parse out the first line FP information
        
        String lid = fpLineTokenizer.nextToken();  
        descriptor.setLocId(lid.trim());

        
        String name = fpLineTokenizer.nextToken();
        descriptor.setLocName(name.trim());

        
        String stream = fpLineTokenizer.nextToken();
        descriptor.setLocStream(stream.trim());

        
        String pe = fpLineTokenizer.nextToken();
        descriptor.setLocPe(pe.trim());

        
        double fs = Double.parseDouble(fpLineTokenizer.nextToken());
	    descriptor.setFloodStg(fs);

	    
        double fq = Double.parseDouble(fpLineTokenizer.nextToken());
	    descriptor.setFloodFlow(fq);

	            
	    long obs_begintime = Long.parseLong(fpLineTokenizer.nextToken());
        descriptor.setObsBeginTime(obs_begintime);
	            
	    long fcst_endtime = Long.parseLong(fpLineTokenizer.nextToken());
        descriptor.setFcstEndTime(fcst_endtime);
	    
        double adjustendhrs = Double.parseDouble(fpLineTokenizer.nextToken());
        descriptor.setAdjustEndhrs(adjustendhrs);
        
	
        debug_logline(header + "lid = " + lid + " " +
                "name =  " + name + " " +
                "stream = " + stream);
        
    
        // parse out second line current VTEC information
        
        String cur_action = curVtecLineTokenizer.nextToken();
        descriptor.setcurAction(cur_action.trim());
        
        String cur_phenom = curVtecLineTokenizer.nextToken();
        descriptor.setcurPhenom(cur_phenom.trim());
        
        String cur_signif = curVtecLineTokenizer.nextToken();
        descriptor.setcurSignif(cur_signif.trim());
        
        String cur_severity = curVtecLineTokenizer.nextToken();
        descriptor.setcurSeverity(cur_severity.trim());
        
        String cur_cause = curVtecLineTokenizer.nextToken();
        descriptor.setcurCause(cur_cause.trim());
        
        String cur_record = curVtecLineTokenizer.nextToken();
        descriptor.setcurRecord(cur_record.trim());
        
        int  cur_etn = Integer.parseInt(curVtecLineTokenizer.nextToken());
	    descriptor.setcurEtn(cur_etn);
	    
	    double cur_crest_value = Double.parseDouble(curVtecLineTokenizer.nextToken());
	    descriptor.setcurCrestValue(cur_crest_value);
	
        long cur_begintime = Long.parseLong(curVtecLineTokenizer.nextToken());
        descriptor.setcurBeginTime(cur_begintime);
	
	    long cur_endtime = Long.parseLong(curVtecLineTokenizer.nextToken());
        descriptor.setcurEndTime(cur_endtime);
	
	    long cur_risetime = Long.parseLong(curVtecLineTokenizer.nextToken());
        descriptor.setcurRiseTime(cur_risetime);
	
	    long cur_cresttime = Long.parseLong(curVtecLineTokenizer.nextToken());
        descriptor.setcurCrestTime(cur_cresttime);
	
	    long cur_falltime = Long.parseLong(curVtecLineTokenizer.nextToken());
        descriptor.setcurFallTime(cur_falltime);
	               
        debug_logline(header + "cur_action = " + cur_action + " " +
        		      "cur_phenom = " + cur_phenom + " " +
        		      "cur_signif = " + cur_signif + " " +
        		      "cur_severity = " + cur_severity + " " +
        		      "cur_cause = " + cur_cause + " " +
        		      "cur_record = " + cur_record + " " +
        		      "cur_etn = " + cur_etn + " " +
        		      "cur_crest_value = " + cur_crest_value);
        
        // parse out third line previous VTEC information
        
        if (prevVtecLineTokenizer != null)
        {
           String prev_action = prevVtecLineTokenizer.nextToken();
           descriptor.setprevAction(prev_action);
        
           String prev_phenom = prevVtecLineTokenizer.nextToken();
           descriptor.setprevPhenom(prev_phenom);
        
           String prev_signif = prevVtecLineTokenizer.nextToken();
           descriptor.setprevSignif(prev_signif);
        
           String prev_severity = prevVtecLineTokenizer.nextToken();
           descriptor.setprevSeverity(prev_severity);
        
           String prev_cause = prevVtecLineTokenizer.nextToken();
           descriptor.setprevCause(prev_cause);
        
           String prev_record = prevVtecLineTokenizer.nextToken();
           descriptor.setprevRecord(prev_record);
        
        
           int  prev_etn = Integer.parseInt(prevVtecLineTokenizer.nextToken());
	       descriptor.setprevEtn(prev_etn);
	
	       double prev_crest_value = Double.parseDouble(prevVtecLineTokenizer.nextToken());
	       descriptor.setprevCrestValue(prev_crest_value);
	       
	       String prev_rise_ts = prevVtecLineTokenizer.nextToken();
	       descriptor.setprevRiseType(prev_rise_ts);
	       
	       String prev_crest_ts = prevVtecLineTokenizer.nextToken();
	       descriptor.setprevCrestType(prev_crest_ts);
	       
	       String prev_fall_ts = prevVtecLineTokenizer.nextToken();
	       descriptor.setprevFallType(prev_fall_ts);
	       
           long prev_begintime = Long.parseLong(prevVtecLineTokenizer.nextToken());
	       descriptor.setprevBeginTime(prev_begintime);
	
           long prev_endtime = Long.parseLong(prevVtecLineTokenizer.nextToken());
           descriptor.setprevEndTime(prev_endtime);
	
    	   long prev_risetime = Long.parseLong(prevVtecLineTokenizer.nextToken());
           descriptor.setprevRiseTime(prev_risetime);
	
	       long prev_cresttime = Long.parseLong(prevVtecLineTokenizer.nextToken());
           descriptor.setprevCrestTime(prev_cresttime);
	
	       long prev_falltime = Long.parseLong(prevVtecLineTokenizer.nextToken());
           descriptor.setprevFallTime(prev_falltime);
	     
           long prev_producttime = Long.parseLong(prevVtecLineTokenizer.nextToken());
           descriptor.setprevProductTime(prev_producttime);
        
           debug_logline(header + "prev_action = " + prev_action + " " + 
        		      "prev_phenom = " + prev_phenom + " " +
        		      "prev_signif = " + prev_signif + " " +
        		      "prev_severity = " + prev_severity + " " +
        		      "prev_cause = " + prev_cause + " " +
        		      "prev_record = " + prev_record + " " +
        		      "prev_etn = " + prev_etn + " " +
        		      "prev_crest_value = " + prev_crest_value + " " +
        		      "prev_rise_ts = " + prev_rise_ts + " " +
        		      "prev_crest_ts = " + prev_crest_ts + " " +
        		      "prev_fall_ts = " + prev_fall_ts);
        }
      
        // parse out the observed time series data
                      
        if (observedTSLineTokenizer != null)
        {                	   
            while (observedTSLineTokenizer.hasMoreTokens()) {        	
        	  obsValueTimeList.add(observedTSLineTokenizer.nextToken());
            }
        
            descriptor.setObsValuetTimeList(obsValueTimeList);
        }
                    
        // pase out the forecast time series data
        
        if (fcstTSLineTokenizer != null)
        {                	   
            while (fcstTSLineTokenizer.hasMoreTokens()) {        	
        	  fcstValueTimeList.add(fcstTSLineTokenizer.nextToken());
            }
        
           descriptor.setFcstValueTimeList(fcstValueTimeList);
        }                   
        
        return descriptor;
    }
 
   //---------------------------------------------------------     
    public void setDebug(boolean debug)
    {
        this._debug = debug;
    }
    
    //---------------------------------------------------------------------------------
    
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
    
    public Map getFpCurPrevVtecMap() 
	{
        return _fpcurprevvtecMap;
	}
       
    // ---------------------------------------------------------------------------------
}


