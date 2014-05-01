package ohd.hseb.util;

// create a Logger class.  Give to Gautam for java practice purposes.

public class CodeTimer
{
	
    private boolean _isRunning = false;
    private long _startTime = 0;
    private long _endTime = 0;
    private long _elapsedTime = 0;
    private Logger _logger = null;
    
    //-----------------------------------------------
    
    public CodeTimer()
    {
     
    
    }
	//-----------------------------------------------
  
    public CodeTimer(Logger logger)
    {
         _logger = logger;	
    }
    
	//-----------------------------------------------
 
    public void start()
    {
        _isRunning = true;
        _startTime = System.currentTimeMillis();
        _elapsedTime = 0;
        
        return;
    }

	//-----------------------------------------------
 
    public long stop()
    {
        return this.stop("");
    }
    
	//-----------------------------------------------
     
    public long stop(String message)
    {
        _endTime = System.currentTimeMillis();
        _elapsedTime += (_endTime - _startTime);
       
        _isRunning = false;
    
        if ( (message != null) && (message.length() > 0) )
        {
           log(message + " " + _elapsedTime + " millis.");
        }
           
        if (_logger != null)
        {
           _logger.close();   
        }
        
        return _elapsedTime;
    }

	//----------------------------------------------- 
       
    public void restart()
    {
        _isRunning = true;
        _startTime = System.currentTimeMillis();
    }

	//-----------------------------------------------
 
    // reset all the state variables    
    public void clear()
    {
        _isRunning = false;
        _startTime = 0;
        _endTime = 0;
        _elapsedTime = 0;                    
    }

	//-----------------------------------------------
	
    private void log(String message)
    {
    	if (_logger != null)
    	{
    	    _logger.log(message);	
    	}
    	else
    	{
    	    System.out.println(message);
    	}
    	return;
    }
    
	//-----------------------------------------------
     
    public long getElapsedTime()
    {
        return _elapsedTime;
    }


}  //end class CodeTimer
