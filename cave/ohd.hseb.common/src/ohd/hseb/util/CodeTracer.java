/*
 * Created on Jul 28, 2004
 *
 */
package ohd.hseb.util;

/**
 * @author GobsC
 *
 *
 */
public class CodeTracer
{
    private Logger _logger = null;

    // -------------------------------------------------------------
    // nobody can call this
    public CodeTracer()
    {
  
    }
    
    public CodeTracer(Logger logger)
    {
        _logger = logger;

    }
    // -------------------------------------------------------------
    public void trace(String message)
    {
     
        // calls trace with a prepended message
        if (_logger != null)
        {
            _logger.log(message);
            trace();
        }
        else
        {
            System.out.println(message);
            trace();
        }
    }
    // -------------------------------------------------------------
    public void trace()
    {
        try
        {
            throw new Exception("Just Tracing, not an actual error");
        }
        catch (Exception e)
        {
            if (_logger != null)
            {
                e.printStackTrace(_logger.getPrintWriter());
            }
            else
            {
                e.printStackTrace();
            }
        }
        
    } //end trace()
    // -------------------------------------------------------------
}
