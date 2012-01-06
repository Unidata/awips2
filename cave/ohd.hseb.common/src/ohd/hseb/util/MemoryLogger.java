package ohd.hseb.util;

public class MemoryLogger
{

    private static SessionLogger _logger = null;

    public static void setLogger(Logger logger)
    {
        _logger = (SessionLogger) logger;

        System.out.println("MemoryLogger.setLogger():  _logger.getSessionId() = " + _logger.getSessionId());
    }

    public static void log(String message) 
    {
        Runtime runtime = Runtime.getRuntime();
        String text = null;

        text =  message + "Free:" + runtime.freeMemory() + " Max:"+ runtime.maxMemory()+ " total:"+ runtime.totalMemory();
        _logger.log(text);

    }
}
