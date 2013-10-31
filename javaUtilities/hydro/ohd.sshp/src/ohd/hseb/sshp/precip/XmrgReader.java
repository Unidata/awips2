/*
 * Created on May 24, 2004
 *
 * 
 */
package ohd.hseb.sshp.precip;


//import java.util.*; // needed for Vector class
import java.io.*;   // needed for ByteArrayInputStream

import java.text.SimpleDateFormat;  // needed for SimpleDateFormat class

//import ohd.hseb.util.AppsDefaults;
import ohd.hseb.grid.XmrgGrid;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.EndianConverter;
import ohd.hseb.util.Logger;

//import whfs.db.gen.*;


public class XmrgReader
{

    // each value was stored in mm of precip times 100
    // Conversion factor needed to convert (mm x 100) to inches
    // need to divide by 2540.0 (25.4mm/inch x 100) to get inches
    private static final double _CONVERSION_FACTOR = 2540.0;
    private static final double _missingValue = -999.0;
  
    private String _mpeQpeGridDirName = null; 
    private SimpleDateFormat _xmrgFileDateFormat = null;
    private boolean _needEndianChange = false;  

    // I may be able to turn these into local variables
    private int _numBinsPerColXmrg=0;
    private int _numBinsPerRowXmrg=0;

    private int _swHrapRowXmrg=0;
    private int _swHrapColXmrg=0;

    private int _southernRowXmrg = 0;
    private int _northernRowXmrg = 0;
    private int _westernColXmrg = 0;
    private int _easternColXmrg = 0;

    private Logger _logger;
    //private CodeTimer _loadGridAccumulatedTimer = null;
    //private CodeTimer _readLoopAccumulatedTimer = null;
    //private CodeTimer _readSetAccumulatedTimer = null;
    
 
//  -----------------------------------------------------------------------------------

    public XmrgReader(Logger logger)
    {
        _logger = logger;         
    }
//  -----------------------------------------------------------------------------------
    
    public XmrgGrid loadGrid(long time, String gridFileName)
    {
   
        XmrgGrid grid = readMPEGridFile(time, gridFileName);
   
        return grid;
    }
//  -----------------------------------------------------------------------------------
    private DataInputStream initDataInputStream(String fullGridFilePath)
    {
        String header = "XmrgReader.initDataInputStream: ";
        
        DataInputStream dis = null;
        
        boolean normalMode = true;
        
        CodeTimer timer = new CodeTimer(_logger);
        
        
        try
        {
            
            if (normalMode)
            {
                // timer.start();
                FileInputStream fis = new FileInputStream(fullGridFilePath);
                BufferedInputStream bis = new BufferedInputStream(fis);
                dis = new DataInputStream(bis);
                //timer.stop(header + "init of 'normal' reading " );
                
            }
            
            else // "faster" mode
            {
                // timer.start();
                
                FileInputStream fis = new FileInputStream(fullGridFilePath);
                
                int byteCount = fis.available();
                byte[] byteArray = new byte[byteCount];
                
                fis.read(byteArray);
                fis.close();
                
                ByteArrayInputStream bais = new ByteArrayInputStream(byteArray);
                dis = new DataInputStream(bais);
                
                //timer.stop(header + "init of 'faster' reading " + byteCount + " bytes from the file " + fullGridFilePath + " took ");
                
            }
        }
        catch (java.io.IOException e)
        {
            e.printStackTrace(_logger.getPrintWriter());
        }
        
        return dis;        
        
    }
//  -----------------------------------------------------------------------------------
     
    private XmrgGrid readMPEGridFile(long gridTime, String gridFileName)
    {
        XmrgGrid grid = null;
        
        String header = "XmrgReaderFaster.readMPEGridFile(): ";

        String fullGridFilePath;

        if (_mpeQpeGridDirName != null)
            fullGridFilePath = _mpeQpeGridDirName + "/" + gridFileName.trim();
        else
            fullGridFilePath = gridFileName.trim();

        File xmrgFile = new File(fullGridFilePath);


        if (xmrgFile.exists())
        {
            try
            {
             
                DataInputStream dis = initDataInputStream(fullGridFilePath);
      
                int numBytesRecordBegin;
                int numBytesRecordEnd;
              
                // read in the first header of the xmrg file which contains
                // the SouthWest HRAP column, row, # of columns and # of rows
                numBytesRecordBegin = dis.readInt();
    
          //    System.out.println(header + "raw numBytesRecordBegin = " + numBytesRecordBegin);
    
    
                //determine endianess
                if (numBytesRecordBegin == 16)
                {
                    _needEndianChange = false;  
         //          System.out.println(header + "good raw numBytesRecordBegin = " + numBytesRecordBegin);
 
                }
                else
                {   
                    _needEndianChange = true;
                    numBytesRecordBegin = EndianConverter.convert(numBytesRecordBegin);
       //           System.out.println(header + "converted raw numBytesRecordBegin = " + numBytesRecordBegin);
                }
    
                _swHrapColXmrg = readInt(dis);
                _swHrapRowXmrg = readInt(dis);
    
    
  
        /*                               
              System.out.println(header + "XOR = " + swHrapColXmrg + 
                                 " YOR = " + swHrapRowXmrg);
         */     

                _numBinsPerRowXmrg = readInt(dis);
             
                if ( (_numBinsPerRowXmrg < 1) || (_numBinsPerRowXmrg > 1000) )
                {
                    _logger.log("Error: The number of HRAP Bins Per Row for " + fullGridFilePath + " is " + _numBinsPerRowXmrg);
                    _logger.log(" Note: This number should be in the range of 1 - 1000.  This MPE xmrg file is being skipped.");
                    return XmrgGrid.getInvalidGrid(gridTime);
                }    

                _numBinsPerColXmrg = readInt(dis);
                             

             
                       
                if ( (_numBinsPerColXmrg < 1) || (_numBinsPerColXmrg > 1000) )
                {
                    _logger.log("Error: The number of HRAP Bins Per Col for " + fullGridFilePath + " is " + _numBinsPerColXmrg);
                    _logger.log(" Note: This number should be in the range of 1 - 1000.  This MPE xmrg file is being skipped.");
                    return( XmrgGrid.getInvalidGrid(gridTime));
                }    

                numBytesRecordEnd = readInt(dis);
           
      //       System.out.println("XOR=" + swHrapColXmrg + " YOR=" + swHrapRowXmrg +
      //           " MAXX=" + numBinsPerRowXmrg + " MAXY=" + numBinsPerColXmrg);

                // calculate the Southern and Northern most rows of the XMRG file
                _southernRowXmrg = _swHrapRowXmrg;
                _northernRowXmrg = _southernRowXmrg + _numBinsPerColXmrg - 1;

                // calculate the Western and Eastern most columns of the XMRG file
                _westernColXmrg = _swHrapColXmrg;
                _easternColXmrg = _westernColXmrg + _numBinsPerRowXmrg - 1;
    
    
                int rowCount = _numBinsPerColXmrg;
                int colCount = _numBinsPerRowXmrg;
       
                grid = new XmrgGrid(gridTime,
                                    _southernRowXmrg, _westernColXmrg,
                                    rowCount, colCount); 
        
                // read in the second header of the xmrg file which contains
                // the userid, saved date, process flag, valid date,
                // maximum value and version number and throw them away
                numBytesRecordBegin = readInt(dis);
                numBytesRecordEnd = dis.skipBytes(numBytesRecordBegin);
                numBytesRecordEnd = readInt(dis);

                double value = _missingValue;
  
               
             //   _readLoopAccumulatedTimer.restart();

                for (int hrapRow = _southernRowXmrg; hrapRow <= _northernRowXmrg && grid.isValid(); hrapRow++)
                {
                    numBytesRecordBegin = readInt(dis);
                    // IF numBytesRecordBegin NOT EQUAL TO TWICE numBinsPerRowXmrg THEN BAIL
                    if ( numBytesRecordBegin != (_numBinsPerRowXmrg * 2) )
                    {
                         _logger.log("Error: The number of bytes per record " + numBytesRecordBegin +
                                   " should be twice the number of bins per row " + _numBinsPerRowXmrg );
                         grid.setIsValid(false);
                 
                    }    

                 //  if ( (ctrRows < firstRowToBeRead) || (ctrRows > lastRowToBeRead) )
                 //      numBytesRecordEnd = dis.skipBytes(numBytesRecordBegin);
                //   else
                
                    

                    for (int hrapCol=_westernColXmrg; hrapCol <= _easternColXmrg && grid.isValid(); hrapCol++)
                    {
                        value = (double)readShort(dis);
                        
                        
                        if (value != _missingValue)
                        {
                            value /= _CONVERSION_FACTOR;
                     //       _readSetAccumulatedTimer.restart();
                            grid.setValue(hrapRow, hrapCol, value);
                    //        _readSetAccumulatedTimer.stop();
                        }
                        else
                        {
                            grid.setValue(hrapRow, hrapCol, _missingValue);
                           // System.out.println("value at " + hrapRow + ", " + hrapCol + " is missing");
                        }
                    }

                    numBytesRecordEnd = readInt(dis);
                }
            //    _readLoopAccumulatedTimer.stop("The total time in the nested for loop took ");
            //    _logger.log("The total accumulated time of grid.setValue() is " + _readSetAccumulatedTimer.getElapsedTime());
      /*
              for (int ctr=0; ctr<gridValues.length; ctr++)
              {
                  if (gridValues[ctr] > 0.0)
                      System.out.println("gridValue[" + ctr + "]=" + gridValues[ctr]);
              }
      */

              dis.close();
             }
             catch(EOFException ioe)
             {
                 _logger.log( "Read End Of File" + ioe);
                 grid.setIsValid(false);
             }
             catch(IOException ioe)
             {
                 _logger.log( "Something went wrong trying to read " + ioe);
                 grid.setIsValid(false);
             }
          }
          else
          {
              //_logger.log("\nWarning: file " + gridFileName + " does NOT Exist!");
              grid = XmrgGrid.getInvalidGrid(gridTime);
          }
          
          
          return grid;

     } // end of readMPEGridFile method

//  -----------------------------------------------------------------------------------
   

// ----------------------------------------------------
    private int readInt(DataInputStream dis) throws IOException
    {
        int value = dis.readInt(); 
        
        if (_needEndianChange)
        {
            value = EndianConverter.convert(value);
        }   
        return value;
    }
// ----------------------------------------------------

    private short readShort(DataInputStream dis) throws IOException
    {
        short value = dis.readShort(); 
        
        if (_needEndianChange)
        {
            value = EndianConverter.convert(value);
        }   
        return value;
    }
  // ----------------------------------------------------
  
    
}
