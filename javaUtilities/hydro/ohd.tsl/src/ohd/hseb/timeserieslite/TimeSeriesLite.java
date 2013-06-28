package ohd.hseb.timeserieslite;

import ohd.hseb.util.CodeTimer;
//import ohd.hseb.util.gui.drawing.TsPaintableCanvas;


public class TimeSeriesLite
{
//  -------------------------------------------------------------------------------------

    //accepted as command-line arguments
    //private String _locationId = null;
    private String _drawingManagerClassName = null;
     
    //internal variables
    private TSLDrawingMgr _drawingMgr = null;
   
   // -------------------------------------------------------------------------------------
 
    private void readCommandLineArgs(String[] argStringArray)
    {
        if (argStringArray.length > 1)
        {   
            //_locationId = argStringArray[0];
            _drawingManagerClassName = argStringArray[0];
        }
        else
        {
            System.out.println("usage: java " + 
                    this.getClass().getName() +
                    "dataManagerClassName <other args depend on dataManager >");
        }
            
        return; 
    } 
    
//  -------------------------------------------------------------------------------------

    private void loadDrawingManager(String[] argumentStringArray)
    { 
        String header = "TimeSeriesList.loadDataManager(): ";
        
        try
        {   
            Class drawingManagerClass = Class.forName(_drawingManagerClassName);   
            
            _drawingMgr = (TSLDrawingMgr) drawingManagerClass.newInstance();          
            //_drawingMgr.handleCommandLineArgs(argumentStringArray);
            
            System.out.println(header + "_drawingMgr's class name = " +
                    _drawingMgr.getClass().getName());
            
            
        }
        catch (ClassNotFoundException e)
        {
            e.printStackTrace();   
        }
        catch (IllegalAccessException e)
        {
            e.printStackTrace();   
        }
        catch (InstantiationException e)
        {
            e.printStackTrace();   
        }
        
        return;
    }
//  -------------------------------------------------------------------------------------
    public void display(String[] argStringArray, boolean exitOnClose)
    {
        CodeTimer timer = new CodeTimer();
        String header = "TimeSeriesLite.display(): ";
        
        readCommandLineArgs(argStringArray);
         
        timer.start();
        loadDrawingManager(argStringArray);
        timer.stop("loadDrawingManager() took ");
        
        if (_drawingMgr != null)
        {
            _drawingMgr.display(argStringArray, exitOnClose);
            
        }
        
        else
        {
            System.out.println(header + "_drawingMgr == null");
        }
        
        return;
        
    }
    
//  -------------------------------------------------------------------------------------
 /* 
    public void display_old(String[] argStringArray, boolean exitOnClose)
    {
        
        CodeTimer timer = new CodeTimer();
        String header = "TimeSeriesLite.display(): ";
        
        readCommandLineArgs(argStringArray);
         
        timer.start();
        loadDrawingManager(argStringArray);
        timer.stop("loadDrawingManager() took ");
        
        if (_drawingMgr != null)
        {
            TSLFrame frame = new TSLFrame();
            
            frame.setExitOnClose(exitOnClose);
            
            System.out.println(header + "after TSLFrame()");
                 
            TsPaintableCanvas canvas = frame.getCanvas();
               
            timer.start();
            _drawingMgr.initializeCanvas(canvas);
            timer.stop("initializeCanvas() took ");
            
          //  System.out.println(header + "after _drawingMgr.initialize(canvas);");
             
            
            timer.start();
            frame.setVisible(true);
            timer.stop("setVisible() took ");
            
           // System.out.println(header + "after frame.setVisible(true)");
            
        }
        
        else
        {
            System.out.println(header + "_drawingMgr == null");
        }
        
        return;
    }
  */  
// -------------------------------------------------------------------------------------
 
    public static void show(String nameOfDrawingMgr,
                            String dbConnString,
                            String locationId,
                            String paramCodeString1,
                            String paramCodeString2)
    {
        String header = "TimeSeriesLite.show(): ";
    
        String[] argStringArray = 
        {
                nameOfDrawingMgr,
                dbConnString,
                locationId,
                paramCodeString1,
                paramCodeString2
        };
        
        for (int i = 0; i < argStringArray.length; i++)
        {
            System.out.println("argStringArray[" +
                    i + "] = " + argStringArray[i]);
        }
              
        TimeSeriesLite app = new TimeSeriesLite();
        app.display(argStringArray, true);
          
    }
    
    public static void main(String[] argStringArray)
    {        
        TimeSeriesLite app = new TimeSeriesLite();
        app.display(argStringArray, true);
        
        //test();
           
        return;
    }
//  -------------------------------------------------------------------------------------
//  -------------------------------------------------------------------------------------
    
    public static void test()
    {        
        
        String[] argStringArray = {"ohd.hseb.timeserieslite.pdc.PDCDrawingMgr",
                                   "jdbc:postgresql://140.90.91.111:5432/hd_ob72ounx?user=pguser",
                                  "BLUO2",
                                  "HGIRGZ",
                                  "HGIFZZ"
                                  };
        
        TimeSeriesLite app = new TimeSeriesLite();
        app.display(argStringArray, true);
  
        
        /*
        argStringArray[3] = "HGIRGZ";
        app.display(argStringArray, true);
        
        
        argStringArray[3] = "TAIRGZ";
        app.display(argStringArray, true);
        
        
        argStringArray[3] = "USIRGZ";
        app.display(argStringArray, true);
        
        argStringArray[3] = "UDIRGZ";
        app.display(argStringArray, true);
        */
        
        return;
    }
//  -------------------------------------------------------------------------------------
   
}
