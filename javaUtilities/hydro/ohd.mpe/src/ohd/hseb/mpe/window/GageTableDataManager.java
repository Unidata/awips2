package ohd.hseb.mpe.window;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.TimeZone;

import ohd.hseb.mpe.util.MPEProductDescriptor;

import ohd.hseb.sshp.precip.XmrgReader;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.FileLogger;
import ohd.hseb.db.DbTable;
import ohd.hseb.grid.XmrgGrid;


public class GageTableDataManager
{
    private Date _productDate;

    private String _inputGageFilePath;

    private String _editedGageFilePath;
    
    private String _bestEstimateDateForm;

    private List<MPEProductDescriptor> _availableMPEProductList;

    private FileLogger _fileLogger;
    
    private boolean _first = true;
    private static boolean _mpe_selected_grid_status = false;
    private String _mpe_selected_grid_gagediff = null;
    
   

    public GageTableDataManager(FileLogger fileLogger,
            String inputGageFilePath, String editedGageFilePath)
    {
        List<MPEProductDescriptor> availableMPEProductList;
        
        setFileLogger(fileLogger);
        setGageFilePath(inputGageFilePath);
        setEditedGageFilePath(editedGageFilePath);
        
        // retrieve availableMPEProductList from setting file first if it exists
       // availableMPEProductList = getAvailableMPEProductFromSettingFile();
      
        //if (availableMPEProductList.size() <= 0)
        //{        	        	         
        	//availableMPEProductList = MPEProductDescriptor
              // .getListOfAllAvailableMPEProductDescriptors();
        //}
        
         
    	availableMPEProductList = MPEProductDescriptor
                     .getListOfAllAvailableMPEProductDescriptors();
        setAvailableMPEProductList(availableMPEProductList);
       
        _productDate = null;
        
        // Set the best estimate XMRG date form.
        AppsDefaults appsDefaults = new AppsDefaults();
        _bestEstimateDateForm = appsDefaults.getToken("st3_date_form", "mdY");               
       
    };

  /*  private List <MPEProductDescriptor> getAvailableMPEProductFromSettingFile ()
    {
    	String header = "GageTableDataManager.getAvailableMPEProductFromSettingFile(): ";
    	List<MPEProductDescriptor> availableMPEProductListFromSettingFile = new  ArrayList<MPEProductDescriptor> ();
    	
    	BufferedReader in = null;
    	boolean fileExist = false;
    	String line;
    	
    	File fileHandlerDir = new File(getEditedGageFilePath());
    	String settingsFile = fileHandlerDir.getParent() +"/MPEGageTableDisplaySettings.txt";
    	
    	System.out.println(header+"The settingsFile is "+ settingsFile);
    	File fileHandler = new File(settingsFile);
    	fileExist = fileHandler.exists();
    	
    	if (!fileExist)
    	{    		
    		_fileLogger.log(header + "Setting File can not be found, use default.");    	
    		System.out.println(header + "Setting File not be found, use default");
    	}
    	else
    	{	    		
    	   try
    	   {
    		   in = new BufferedReader(new FileReader(fileHandler));
    		   if ( in.readLine() != null)
    		   {    			       			   
    			   // return the info from settings file as a whole string buffer
    			   while ((line = in.readLine()) != null )
    			   {    				       			       			         			           			       
    			       int strStart = in.toString().indexOf("COLUMN:");
    			       int strEnd = in.toString().indexOf("|");
    			       if (strStart >= 0 && strEnd > strStart)
    			       {	   
    			           String productName = in.toString().substring(strStart, strEnd);
    			       }    
    			   }  
    			       			   
    			       			   
    		   }
    		   
    		   _fileLogger.log(header + "Retrieve availableMPEProductList from setting file " + settingsFile);    	
       		   System.out.println(header + "Retrieve availableMPEProductList from setting file " + settingsFile);
       		   		
    		   in.close();    		  
    		
    	   } catch (IOException e)
    	   {    		   
    		   _fileLogger.log(header+ "Unable to retrieve availableMPEProductList from file " + settingsFile);
    		   e.printStackTrace(_fileLogger.getPrintWriter());
    		   e.printStackTrace();
    	   }
    	}
    	    	    
    	return availableMPEProductListFromSettingFile;
    }*/
    
    public List<GageTableRowData> readData() throws IOException, ParseException
    {
        String header = "GageTableDataManager.readData(): ";
                
        Float default_editValue = DbTable.getNullFloat();
        double default_diffValue;
        double gridValue = DbTable.getNullDouble();        
        List<GageFileRecord> gageRecordList = null;
        Map<String, Double> productValueMap = new HashMap<String, Double>();
        XmrgGrid xmrgGrid = null;
        
        

        System.out.println(header + "1");
        
        gageRecordList = readGageValueFile();
        
        System.out.println(header + "2");
        
        
        List<GageTableRowData> rowDataList = new ArrayList<GageTableRowData>(
                gageRecordList.size());
        
        System.out.println(header + "2.1");
        
        readProductFile();
        
        System.out.println(header + "3");
        
        System.out.println(header + "gageRecordList.size() = " + gageRecordList.size());
        System.out.println(header + "getAvailableMPEProductList().size() = " + getAvailableMPEProductList().size());
        
        int maxIterations = gageRecordList.size() * getAvailableMPEProductList().size();
        System.out.println(header + "nested loop iterations will equal = " + maxIterations);
       
        System.out.println(header + "4");
                 
        int iterationCount = 0;
        
        // Build records based on the number of gages.
        // For each gage read from the gage value list.
        for (GageFileRecord record : gageRecordList)
        {
            for (MPEProductDescriptor mpeProductDescriptor : getAvailableMPEProductList())        	
            {
                iterationCount++;
                xmrgGrid = mpeProductDescriptor.getMpeProductDataGrid();

                gridValue = DbTable.getNullDouble();

                if (xmrgGrid.isValid())
                {
                    gridValue = xmrgGrid.getValue((int) record.getHrapY(),
                            (int) record.getHrapX());

                    if (gridValue < 0)
                    {
                        gridValue = DbTable.getNullDouble();
                    }
                }

                productValueMap.put(mpeProductDescriptor.getProductName(),
                        new Double(gridValue));
                
                  
                if ((iterationCount % 2000) == 0)
                {
                    System.out.println(header + " iterationCount = " + iterationCount + " of " + maxIterations);
                }
            }

          //initialize the default_diffValue between GageValue and Best Estimate Grid Value
            default_diffValue = setDiffGageGridValue(record.getValue(), productValueMap);
            
            GageTableRowData gageRow = new GageTableRowData(record.getLid(),
                    record.getValue(), default_editValue, default_diffValue, record.getRadarID(), productValueMap);
            gageRow.addAllCellsToMap();
            rowDataList.add(gageRow);
        }

        return rowDataList;
    }
       
    
    private Double setDiffGageGridValue(float gageValue,Map<String, Double> productValueMap)
    		                           
    {
    	String header = "GageTableDataManager.setDiffGageGridValue: ";        	
    	final String MPE_SELECTED_GRID_GAGEDIFF_TOKEN = "mpe_selected_grid_gagediff";
    	final String DEFAULT_SELECTED_GRID_GAGEDIFF = "Best Estimate QPE";
    	
    	String tokenPrefix = null;
    	String tokenName = null;
    	
    	AppsDefaults appsDefaults = new AppsDefaults();
    	Double default_diffValue = DbTable.getNullDouble();
        
    	
    	if (_first == true)
    	{	
    	   tokenPrefix = appsDefaults.getToken(MPE_SELECTED_GRID_GAGEDIFF_TOKEN);
    	       	 	      	   
    	   for (Iterator<MPEProductDescriptor> i = MPEProductDescriptor.getListOfAvailableMPEProductDescriptors().iterator();
    	                                 i.hasNext();)     
    	   {	  
    		   MPEProductDescriptor  item = i.next();    	       
    	       if (tokenPrefix != null && tokenPrefix .equals(item.getProductFilenamePrefix()))
    	    		   tokenName = item.getProductName();
    	   }
    	   
    	   if (tokenName == null)
    		   _mpe_selected_grid_gagediff  = DEFAULT_SELECTED_GRID_GAGEDIFF;
    	 
    	   else if (productValueMap.containsKey(tokenName) == false)    		  
    		   _mpe_selected_grid_gagediff  = DEFAULT_SELECTED_GRID_GAGEDIFF;
    	   
    	   else
    		   _mpe_selected_grid_gagediff  = tokenName;
    	    		      	
    	   System.out.println(header + "The value of token mpe_selected_grid_gagediff is " + 
    			   _mpe_selected_grid_gagediff);
    	          	   
    	   _first = false;
    	}
    	
    	
    	Double gridData = (Double) productValueMap.get(_mpe_selected_grid_gagediff);                 
        
        if (gageValue < 0.0 || gridData == null || gridData == DbTable.getNullDouble() )        	
            default_diffValue = DbTable.getNullDouble();
        else
        	default_diffValue = gageValue - gridData;
    
    	
    	return default_diffValue;
    }
    
    
    private List<GageFileRecord> readGageValueFile() throws IOException, ParseException
    {
        Date productDate = null;
        float hrapX;
        float hrapY;
        float value;

        List<GageFileRecord> gageRecordList = new ArrayList<GageFileRecord>();
        Scanner recordScanner = null;
        SimpleDateFormat utcSimpleDateFormat = new SimpleDateFormat(
                "yyyy MM dd HH");
        String gageRecord = null;
        String lid = null;
        String radarID = null;

        utcSimpleDateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));

        BufferedReader inputGageFile = new BufferedReader(
                new InputStreamReader(new FileInputStream(getGageFilePath())));

        gageRecord = inputGageFile.readLine();

        if (gageRecord != null)
        {
            /*
             * The first record of the gage file contains the date and time of
             * the gage data int year, month, day, hour format.
             */
            productDate = new Date(utcSimpleDateFormat.parse(gageRecord)
                    .getTime());
            setProductDate(productDate);
        }
        else
        {
            throw new IOException("Missing date string in first record of gage file.");
        }

        gageRecord = inputGageFile.readLine();

        while (gageRecord != null && gageRecord.equals("") == false)
        {
            recordScanner = new Scanner(gageRecord);
            lid = recordScanner.next();
            hrapX = recordScanner.nextFloat();
            hrapY = recordScanner.nextFloat();
            radarID = recordScanner.next();
            value = recordScanner.nextFloat();

            if (value < 0)
            {
                value = DbTable.getNullFloat();
            }

            gageRecordList.add(new GageFileRecord(lid, hrapX, hrapY, radarID, value));
            gageRecord = inputGageFile.readLine();
        }

        inputGageFile.close();

        return gageRecordList;
    }

    private void readProductFile()
    {
        String header = "GageTableDataManager.readProductFile(): ";
        AppsDefaults appsDefaults = new AppsDefaults();
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMddHH");
        SimpleDateFormat alternativeXmrgDateFormat = new SimpleDateFormat("MMddyyyyHH");
        
        System.out.println(header + "1" );
        
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        alternativeXmrgDateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        System.out.println(header + "2" );
        
        
        String productDateString = dateFormat.format(getProductDate());
        
        System.out.println(header + "3" );
        String alternativeXmrgProductDateString = alternativeXmrgDateFormat.format(getProductDate());
        
        System.out.println(header + "4" );
        String productPath = null;

        XmrgGrid xmrgGrid = null;
        
        System.out.println(header + "5" );
        XmrgReader xmrgReader = new XmrgReader(getFileLogger());
        
        System.out.println(header + "6" );
        
        System.out.println(header + "getAvailableMPEProductList().size() = " + getAvailableMPEProductList().size() );
    
        System.out.println(header + "7" );
        
        for (MPEProductDescriptor product : getAvailableMPEProductList())
        {
            // Build the path to the product.  If reading the XMRG product file
            // then need to take into consideration that the date on the filename
            // may be format yyyyMMddHH or MMddYYYYHH.
            productPath = appsDefaults.getToken(product.getProductPathToken(),
                    "");
            
            if ((product.getProductFilenamePrefix().compareTo("xmrg") == 0) &&
                    (_bestEstimateDateForm.compareTo("mdY") == 0 ) )
            {
               productPath += "/" + product.getProductFilenamePrefix() + alternativeXmrgProductDateString
                           + "z";
            }
            else
            {
                productPath += "/" + product.getProductFilenamePrefix() + productDateString
                + "z";
            }

            // Retrieve the XMRG grid for the product.
            System.out.println("Retrieving data for " + productPath);

            xmrgGrid = xmrgReader.loadGrid(getProductDate().getTime(),
                    productPath);
            
            if (xmrgGrid != null)
            {
               product.setMpeProductDataGrid(xmrgGrid);
            }
        }
    }

    public void saveUpdatedGageData(List<GageTableRowData> gageRowDataList) throws IOException
    {
        File editedGageFile = new File(getEditedGageFilePath());
        File tempGageFile = null;
        float gageValue;
        List<GageFileRecord> gageFileRecordList = new ArrayList<GageFileRecord>();
        String gageId = null;

        // Write the data to a temp file and then move it to the
        // official edited gage data file. This prevents the C
        // program from reading the file while the Java program is
        // writing it.
        String outputPath = editedGageFile.getParent() + "/temp";
        tempGageFile = new File(outputPath);
        System.out.println("The temp output path is " + outputPath);

        try
        {
            PrintWriter outputGageFile = new PrintWriter(new BufferedWriter(
                    new OutputStreamWriter(new FileOutputStream(outputPath))));

            for (GageTableRowData gageTableRowData : gageRowDataList)
            {
                // Check if this gage value has been edited.
            		            	               	                   	   
            	if (gageTableRowData.getOrginalValue() != gageTableRowData
                        .getValue())
                   {
                       gageId = gageTableRowData.getGageId();                                           
                       gageValue = gageTableRowData.getValue();

                       if (gageValue == DbTable.getNullFloat())
                       {
                           gageValue = -1;
                       }
                       
                       gageFileRecordList.add(new GageFileRecord(gageId, 0.0f,
                               0.0f, "M", gageValue));
                     
                   }                    
                
            }

            for (GageFileRecord gageFileRecord : gageFileRecordList)
            {
                outputGageFile.println(gageFileRecord);
            }

            outputGageFile.close();
            tempGageFile.renameTo(editedGageFile);
        }
        catch (IOException e)
        {
            getFileLogger().log(
                    "Error processing gage file " + getGageFilePath()
                            + "/gages.txt");
            throw e;
        }
    }

    public void setAvailableMPEProductList(
            List<MPEProductDescriptor> availableMPEProductList)
    {
        _availableMPEProductList = availableMPEProductList;
    }

    public List<MPEProductDescriptor> getAvailableMPEProductList()
    {
        return _availableMPEProductList;
    
    }

    public void setFileLogger(FileLogger fileLogger)
    {
        _fileLogger = fileLogger;
    }

    public FileLogger getFileLogger()
    {
        return _fileLogger;
    }

    public void setGageFilePath(String _gageFilePath)
    {
        this._inputGageFilePath = _gageFilePath;
    }

    public String getGageFilePath()
    {
        return _inputGageFilePath;
    }

    public void setProductDate(Date productDate)
    {
        _productDate = productDate;
    }

    public Date getProductDate()
    {
        return _productDate;
    }

    public void setEditedGageFilePath(String editedGageFilePath)
    {
        this._editedGageFilePath = editedGageFilePath;
    }

    public String getEditedGageFilePath()
    {
        return _editedGageFilePath;
    }

    private class GageFileRecord
    {
        private String _lid;
        private float _hrapX;
        private float _hrapY;
        private String _radarID;
        private float _value;

        public GageFileRecord(String lid, float hrapX, float hrapY, String radarID, float value)
        {
            setLid(lid);
            setHrapX(hrapX);
            setHrapY(hrapY);
            setRadarID(radarID);
            setValue(value);
        }

        public void setLid(String lid)
        {
            _lid = lid;
        }

        public String getLid()
        {
            return _lid;
        }

        public void setHrapX(float hrapX)
        {
            _hrapX = hrapX;
        }

        public float getHrapX()
        {
            return _hrapX;
        }

        public void setHrapY(float hrapY)
        {
            _hrapY = hrapY;
        }

        public float getHrapY()
        {
            return _hrapY;
        }

        public void setValue(float value)
        {
            _value = value;
        }

        public float getValue()
        {
            return _value;
        }

        public String toString()
        {
            String outputString = getLid() + " " + getHrapX() + " "
                    + getHrapY() + " " + getValue();
            return outputString;
        }

        public void setRadarID(String radarID)
        {
            _radarID = radarID;
        }

        public String getRadarID()
        {
            return _radarID;
        }

    }

	public String get_mpe_selected_grid_gagediff() {
		return _mpe_selected_grid_gagediff;
	}

	public void set_mpe_selected_grid_gagediff(String _mpe_selected_grid_gagediff) {
		this._mpe_selected_grid_gagediff = _mpe_selected_grid_gagediff;
	}

}
