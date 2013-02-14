/*
 * Created on Feb 25, 2004
 *
 * This is the main program for the extraction of OFS
 * data, conversion to XML, and then sending to the WFO
 */
package ohd.hseb.sshp.messaging;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.TimeZone;

import ohd.hseb.util.EnvHelper;
import ohd.hseb.util.FileLogger;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

/**
 * @author GobsC
 *
 * 
 */
public class SshpDataTransferMgr
{
   
    private static String _controlFilePath = null;
    private static String _extractedDirName = null;
    private static String _processedDirName = null;
    private static String _logDirName = null;
 
    private Document _controlDoc = null;
    private SAXBuilder _builder = new SAXBuilder();
    private XMLOutputter _xmlOutputter = null;
    
    private FileLogger _logger = null;
    private String _logFilePath = null;
    
    // --------------------------------------------------------------
    public SshpDataTransferMgr(String controlFilePath, 
                               String extractedDirName,
                               String processedDirName,
                               String logDirName)
    {
        
        _controlFilePath = controlFilePath;
        _extractedDirName =  extractedDirName;
        _processedDirName =  processedDirName;
        
        _logDirName = logDirName;
        
        _logFilePath = _logDirName + "/SshpDataTransferMgr.log"; 
        
        _logger = new FileLogger(_logFilePath);
        
    	Format format = Format.getRawFormat();
    	format.setIndent("   ");
    	this._xmlOutputter = new XMLOutputter(format);
        
        loadControlFile();
    
        return;
    }

//  --------------------------------------------------------------
    public void execute()
    {
        String header = "SshpDataTransferMgr.execute(): ";
        System.out.println("SshpDataTransferMgr logging to " + _logFilePath);
        //using the control xml document
        //for each wfo
        
            //for each forecast point
                //extract all the segment data to a file  
                //translate the extract file and add contents to the live XML doc
            //end for each forecast point
            
            //output live XML document object to a file
            //send file using MHS
        // end for each wfo     
        
        Element controlRoot =_controlDoc.getRootElement();     
        List wfoElementList = controlRoot.getChildren("wfo");
        int wfoCount = wfoElementList.size();
        
            
        long time = System.currentTimeMillis();           
        String timeString = getDateTimeStringFromLongTime(time);
          
          
        //for each wfo  
        for (int i = 0; i < wfoCount; i++)
        {
            Element wfoElement = (Element) wfoElementList.get(i);
            String wfoName = wfoElement.getChildText("name");
            String mhsId = wfoElement.getChildText("mhsId");
            String productId = wfoElement.getChildText( "productId" );
            
            _logger.log("processing wfoName = " + wfoName);
            
            List fcstPointElementList = wfoElement.getChildren("fcstPoint");
            int fcstPointCount = fcstPointElementList.size();
            
            Document outputXmlDoc = getBaseDocument();
                  
            //for each forecast point 
            for (int j = 0; j < fcstPointCount; j++)
            {
                Element fcstPointElement = (Element) fcstPointElementList.get(j);
                String segmentId = fcstPointElement.getChildText("ofsSegmentId");
                String locationId = fcstPointElement.getChildText("ihfsLocId");
                String basinId = fcstPointElement.getChildText("ihfsBasinId");
                
                _logger.log("segmentId = " + segmentId + " locationId = " + locationId);
                
                String extractFileName = _extractedDirName + "/" +
                                          segmentId + "." + timeString + ".txt";
                                         
               // String extractFileName = _extractedDirName + "/" +
               //                                        segmentId + "_" + "1";
            
                                         
                // extract all the segment data to a file  
                extractOfsData(segmentId, extractFileName);
                
                      
                //translate the extract file and add contents to the live XML doc
                translateToXml(basinId, extractFileName, outputXmlDoc);
            } 
            
            if (fcstPointCount < 1)
            {
                 _logger.log("There were no forecast points defined in the control file: " +
                              _controlFilePath);    
            }    
            
            //output the xml document to a file
            String xmlFilePath = _processedDirName + "/" + 
                                  wfoName + "." + timeString + ".xml";
                                  
            String xmlFileName = wfoName + "." + timeString + ".xml";
            
            writeXmlToFile(outputXmlDoc, xmlFilePath);
            
            //send xml file to the site (WFO, usually) with this mhsId
            sendXml(xmlFilePath, xmlFileName, mhsId, productId );
        }    
        
       if ( wfoCount < 1 )
       {
           _logger.log("There were no wfos defined in the control file: " + _controlFilePath);    
       } 
        
       _logger.log("SshpDataTransferMgr completed.");
       System.out.println("SshpDataTransferMgr completed.");
        
    }   

//  --------------------------------------------------------------

    private Document getBaseDocument()
    {
        Document doc = new Document();
    
        Element rootElement = new Element("SacModelMessage");

        doc.setRootElement(rootElement);
    
        Element sacParamsListElement = new Element("SacParamsList");
        Element sacStateListElement = new Element("SacStateList");
        Element peTimeSeriesListElement = new Element("PeTimeSeriesList");
        Element runoffTimeSeriesElement = new Element("RunoffTimeSeriesList");
        Element monthlyValuesListElement = new Element("MonthlyValuesList");
    
        rootElement.addContent(sacParamsListElement);
        rootElement.addContent(sacStateListElement);
        rootElement.addContent(peTimeSeriesListElement);
        rootElement.addContent(runoffTimeSeriesElement);
        rootElement.addContent(monthlyValuesListElement);

       return doc;
    }   
       
//  --------------------------------------------------------------

    private void loadControlFile()
    {
        try
        {
            _controlDoc = _builder.build( _controlFilePath );
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        catch (JDOMException e2)
        {
            e2.printStackTrace();
        }
    }
    
//  --------------------------------------------------------------

    private void writeXmlToFile(Document outputXmlDoc, String xmlFileName)
    {
        
        try
        {
           FileWriter writer = new FileWriter(xmlFileName);
           _xmlOutputter.output(outputXmlDoc, writer);
           writer.close();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
    }
    
//     --------------------------------------------------------------
    private static String getDateTimeStringFromLongTime(long time)
    {
         String timeString  = getStringFromLongTime(time, "yyyy_MM_dd_HH_mm_ss");
    
         return timeString;
    }

        //  -------------------------------------------------------
  
    private static String getStringFromLongTime(long time, String dateFormat)
    {
        String timeString  = null;

        //System.out.println("timeString = !" + timeString + "!");
        SimpleDateFormat utcSdf2 = new SimpleDateFormat(dateFormat);
        utcSdf2.setTimeZone(TimeZone.getTimeZone("UTC"));
        timeString = utcSdf2.format(new java.util.Date(time));

        return timeString;
    }
//  --------------------------------------------------------------
    private void extractOfsData(String segmentId, String extractFileName)
    {
        String header = "SshpDataTransferMgr.extractOfsData(): ";
     
        EnvHelper envHelper = new EnvHelper();
        
        String dirString = envHelper.getProperty("WHFS_BIN_DIR");
          
        String commandString = dirString + "/run_SSHP_ofs_extract " + segmentId + " " + extractFileName;
       // System.out.println(header + "making system call:" + commandString + ":");
        Process process = null;
        try
        {
            process = Runtime.getRuntime().exec(commandString);
            process.waitFor();
        }
        catch(IOException e)
        {
            e.printStackTrace();
        }
        catch( InterruptedException e )
        {
        	e.printStackTrace();
        }
        finally
        {
           // DR#10955
           if (process != null)
           {
               process.destroy();
           }
        }
    }
//  --------------------------------------------------------------
    private void translateToXml(String basinId, String extractFileName, Document outputXmlDoc)
    {
       File file = new File(extractFileName);
       
       int sleepLimit = 15;
       int  sleepCount = 0;
       
       boolean done = false;
       long oldLength = 0;
       long length = 0;
       
       while (!done  && (sleepCount < sleepLimit) )
       { 
           try
           {
               Thread.sleep(1000);
           }
           catch (InterruptedException e)
           {
               e.printStackTrace();
           }
           sleepCount++;
           
           if (file.exists())
           {
               length = file.length();
               if (length > 0)
               {
                   if (length == oldLength)
                   {
                       //_logger.log("length = oldlength = " + length);
                       done = true;    
                   }    
               }
           }
           
           oldLength = length;      
       }
       
       SacXMLEncoder encoder = new SacXMLEncoder(basinId, extractFileName, outputXmlDoc, _logger);
       
       try
       {
       		encoder.parse();
       }
       catch ( OfsFileParserException e )
       {
       		exitOnError();
       }
       
    }

//	--------------------------------------------------------------

	private void exitOnError()
	{
		_logger.log( "Error parsing OFS extract file" );
		_logger.log( "Please check the OFS extract files for any problems" );
		_logger.log( "Exiting..." );
//		System.exit( 0 );
	}
	
//  --------------------------------------------------------------
    private void sendXml(String xmlFilePath, String xmlFileName, String mhsId, String productId)
    {
        String header = "SshpDataTransferMgr.sendXml(): ";
        
        /*
        # Send product to specific location
        # add the one-word description and the office id
        #
        #echo "Sending file:$FILENAME  product_ID:$PRODUCT_ID to *OFFICE* via distributeProduct" >> $LOGNAME
        #SUBJECT="*DESCRIPTION* $PRODUCT_ID"
        #/awips/fxa/bin/distributeProduct -c SHEFPRODUCT -s "$SUBJECT" -a *OFFICE* $PRODUCT_ID $FILENAME
        #RETURN_STATUS=$?
        */

       // String commandString = "/awips/fxa/bin/distributeProduct -c HYDRO_MODEL_DATA -a " +
       //                         mhsId + " " + productId + " " + xmlFileName; 
       
        EnvHelper envHelper = new EnvHelper();
        
        String dirString = envHelper.getProperty("WHFS_BIN_DIR"); 
        String commandString = dirString + "/run_SSHP_data_send " + xmlFilePath + " " + xmlFileName + " " + mhsId + " " + productId;
        
//        System.out.println(header + "making system call:" + commandString + ":");
        
        Process process = null;
        try
        {
            process = Runtime.getRuntime().exec(commandString);
            process.waitFor();
        }
        catch(IOException e)
        {
            e.printStackTrace();
        }
		catch( InterruptedException e )
		{
			e.printStackTrace();
		}
        finally
        {
           // DR#10955
           if (process != null)
           {
               process.destroy();
           }
        }
        return;
    } 
//  --------------------------------------------------------------
    
    public static void main(String[] argStringArray)
    {
        if (argStringArray.length >= 4)
        {
            String controlFilePath = argStringArray[0];
            String extractedDirName = argStringArray[1];
            String processedDirName = argStringArray[2];
            String logDirName = argStringArray[3];

            SshpDataTransferMgr mgr = new SshpDataTransferMgr(controlFilePath, extractedDirName, processedDirName, logDirName);
            
            mgr.execute();
        }
        else
        {
            System.out.println("usage: ohd.hseb.sshp.message.SSHPDataTransferManager controlFilePath  extractedDirName processedDirName logDirName"); 
        }
    }

//  --------------------------------------------------------------
   
   
} //end class SshpDataTransferMgr
