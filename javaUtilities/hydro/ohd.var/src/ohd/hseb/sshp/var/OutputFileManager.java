package ohd.hseb.sshp.var;
import static ohd.hseb.util.TimeHelper.MILLIS_PER_HOUR;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.measurement.RegularTimeSeries;
import ohd.hseb.model.sacsma.SacSmaParameters;
import ohd.hseb.model.sacsma.SacSmaState;
import ohd.hseb.util.FileLogger;


public class OutputFileManager 
{
    private long _assimilationStartTimeLong;
    private long _assimilationTargetTimeLong;
    private String _basinId;
    private int _uhgOrdinateNum;
    private List _sacSmaStateList = null;
    private RegularTimeSeries _runOffRegularTimeSeries;
    private FileLogger _varControllerLogger;

    public OutputFileManager(FileLogger logger, String basinId, long assimilationStartTimeLong, long assimilationTargetTimeLong,
            int uhgOrdinateNum) 
    {   
        _varControllerLogger = logger;
        _assimilationStartTimeLong = assimilationStartTimeLong;
        _assimilationTargetTimeLong = assimilationTargetTimeLong;

        _basinId = basinId;
        _uhgOrdinateNum = uhgOrdinateNum;
    }

//  ----------------------------------------------------------------------------------------------------------------------------------
    /***************************************************************************************************************************
     * Reads the file "basinId_states_02_var" or "basinId_adj_states_02_var"
     * for getting the SAC-SMA state variables & runoff time series .
     *@param fileName: "basinId_states_02_var" or "basinId_adj_states_02_var"
     */
    public boolean readVarOutputFileForStateAndRunoffValues(String fileName)
    {
        boolean isReadSuccessful = false;
        try
        {
            BufferedReader bufferedReader = new BufferedReader(new FileReader(fileName));

            String fortranVarTimeString = null;  //jtime, precip, precip_after_adjustmen, pe, pe_after_adjustment

            _runOffRegularTimeSeries = new RegularTimeSeries(_assimilationStartTimeLong + MILLIS_PER_HOUR,
                                                             _assimilationTargetTimeLong + MILLIS_PER_HOUR, 
                                                             1,
                                                             MeasuringUnit.mm);

            String[] lineArray = null;

//          looping through reading and discarding un-wanted data
            lineArray = readFourLines(bufferedReader);

            Pattern pattern = Pattern.compile(" +"); //dealing with multiple spaces
            String[] arrayStr = null;

            _sacSmaStateList = new ArrayList();
            boolean done = false;
            
            if (lineArray[3] == null) //check to see if the last one is already null, if so, we are done.  
            {
                done = true;
            }
            
            while (! done )
            {          
//              processing the 1st line
                arrayStr = pattern.split(lineArray[0].trim()); //trimming preceding and trailing spaces
                fortranVarTimeString = arrayStr[0];
                
                long time = VarHelper.getLongTimeFromDateTimeString(fortranVarTimeString, "yyyyMMddHH");
                //ignore the rest of the 1st line, because they are(in order): precip, precip_after_adjustmen, pe, pe_after_adjustment
                //                                                            pxadj = precip_after_adjustmen/precip
                //                                                            peadj = pe_after_adjustment/pe

//              processing the 2nd line
                arrayStr = pattern.split(lineArray[1].trim());
                double uztwc = Double.parseDouble(arrayStr[0]);
                double uzfwc = Double.parseDouble(arrayStr[1]);
                double lztwc = Double.parseDouble(arrayStr[2]);
                double lzfsc = Double.parseDouble(arrayStr[3]);

                //processing the 3rd line
                arrayStr = pattern.split(lineArray[2].trim());
                double lzfpc = Double.parseDouble(arrayStr[0]);
                double adimc = Double.parseDouble(arrayStr[1]);

//              processing the 4th line, get tci
                arrayStr = pattern.split(lineArray[3].trim());
                double totalChannelInflow  = Double.parseDouble(arrayStr[1]); //also known as tci

                //saving sacSmaState into database table SacSmaState
                SacSmaState sacSmaState = new SacSmaState();

                sacSmaState.setValidTime(time);
                sacSmaState.setUztwc(uztwc);
                sacSmaState.setUzfwc(uzfwc);
                sacSmaState.setLztwc(lztwc);
                sacSmaState.setLzfsc(lzfsc);
                sacSmaState.setLzfpc(lzfpc);
                sacSmaState.setAdimc(adimc);
                sacSmaState.setBasinId(_basinId);

                _sacSmaStateList.add(sacSmaState);

                //adding the tci value to the time series
                _runOffRegularTimeSeries.setMeasurementByTime(new Measurement(totalChannelInflow, 
                        MeasuringUnit.mm), 
                        time);

                //finished the 4 lines now, read the next 4 lines
                lineArray = readFourLines(bufferedReader);
                if( (lineArray == null) || (lineArray[0] == null) )
                {
                    done = true;
                }
            } //end of while loop
       
            _varControllerLogger.log(" number of hours read = " + _sacSmaStateList.size());
            bufferedReader.close();
            
            if (_sacSmaStateList.size() > 0)
            {
                isReadSuccessful = true;
                _varControllerLogger.log("The file [" + fileName + "] has been read and the Var SacSMA state variables and TCI have been saved in the database " +
                "by Java program.");
            }
            else
            {   
                isReadSuccessful = false;
                _varControllerLogger.log("Error: the file [" + fileName + "] was read, but Var SacSMA state variables and TCI were not saved in the database " +
                "by the Java program.");
            }
         
        }
        catch(FileNotFoundException e)
        {
            _varControllerLogger.log("File [" + fileName + "] not found or could not be opened");
            
        }
        catch(IOException e)
        {
            _varControllerLogger.log("Error reading from the file, " + fileName);
            
        }

        return isReadSuccessful;
    }

//  ----------------------------------------------------------------------------------------------------------------------------------
    public List getSacSmaStateList()
    {
        return _sacSmaStateList;
    }
    
//  ----------------------------------------------------------------------------------------------------------------------------------

    public RegularTimeSeries getRunOffRegularTimeSeries()
    {
        return _runOffRegularTimeSeries;
    }

//  ----------------------------------------------------------------------------------------------------------------------------------    
    private int getPastHourlyValueCount()
    {
        long elapsedTimeInMillis =  _assimilationTargetTimeLong - _assimilationStartTimeLong ;
        int elapsedTimeInHours = (int)(elapsedTimeInMillis / MILLIS_PER_HOUR) + 1; 
        return  elapsedTimeInHours ;
    }
//  ----------------------------------------------------------------------------------------------------------------------------------
    private String[] readFourLines(BufferedReader reader)
    {
        String lineArray[] = new String[4];
        for(int i=0; i < 4; i++)
        {
            try
            {
                lineArray[i] = reader.readLine();
               // System.out.println("line[" + i + "] = " + lineArray[i]);
            }
            catch(IOException e)
            {
                _varControllerLogger.log("OutputFilesReader.readFourLines(): " +e);
            }
        }
        return lineArray;
    }

//  ----------------------------------------------------------------------------------------------------------------------------------
    /**************************************************************************************************************************************************
     * Saving the adjusted SacSma parameters(peadj, pxadj) and its correspondant validtime into a text file, "basinId_sacsmaparams", 
     * to be read later for deck file generation when running var next time. 
     * Only the number of UHG coordinate hours worth data is saved. 
     *
     */
    //this method is not used currently
    public boolean  saveSacSmaParamsFromAdjFileToParamsFile(String outputDir, String basinId)
    {
        boolean isSaveSacSmaParamsSucccessful = false;
        try
        {
            BufferedReader bufferedReader = new BufferedReader(new FileReader(outputDir + basinId + "_adj_states_02_var"));
            PrintWriter outputStream = new PrintWriter(new FileOutputStream(outputDir + basinId + "_sacsmaparams"), true);

            String[] lineArray = null;

//          looping through reading and discarding un-wanted data
            for(int i=1; i <= (getPastHourlyValueCount() - _uhgOrdinateNum); i++)  
            {
                lineArray = readFourLines(bufferedReader);
            }

            //now the inputStream has been the right place:
            lineArray = readFourLines(bufferedReader);

            Pattern pattern = Pattern.compile(" +"); //dealing with multiple spaces
            String[] arrayStr = null;
            for(int i = 0; i < _uhgOrdinateNum; i++)
            {          
//              processing the 1st line
                arrayStr = pattern.split(lineArray[0].trim()); //trimming preceding and trailing spaces
                String jtime = arrayStr[0];
                //ignore the rest of the 1st line, because they are(in order): precip, precip_after_adjustmen, pe, pe_after_adjustment
                //                                                            pxadj = precip_after_adjustmen/precip
                //                                                            peadj = pe_after_adjustment/pe

//              skip the 2nd line

                //processing the 3rd line: get the 3rd, 4th item this line
                arrayStr = pattern.split(lineArray[2].trim());
                double pxadj = Double.parseDouble(arrayStr[2]);
                double peadj = Double.parseDouble(arrayStr[3]);

                outputStream.printf("%1$10s \t%2$9.7f \t%3$9.7f\n", jtime,pxadj, peadj);

                //skip the 4th line

                //finished the 4 lines now, read the next 4 lines
                lineArray = readFourLines(bufferedReader);
            } //close for loop, finished outputing to file

            outputStream.close();
        } //close try block
        catch(IOException e)
        {
            _varControllerLogger.log("Exception during saveSacSmaParamsToFile() in VarDataMgr.java");
            return isSaveSacSmaParamsSucccessful;
        }

        _varControllerLogger.log("SacSma parameters pxadj & peadj have been saved into text file " 
                + outputDir + basinId + "_sacsmaparams");
        isSaveSacSmaParamsSucccessful = true;
        return isSaveSacSmaParamsSucccessful;
    } 
    
//  ----------------------------------------------------------------------------------------------------------------------------------
    
    /*****************************************************************************************************************************
     * Read the pxadj and the peadj from the file, but this method is probably not needed.  -Chip
     * @return
     */
    //this method is not used currently
    private SacSmaParameters readAdjustmentFactorsFromFile(String outputDir, 
                                                           String basinId,
                                                           SacSmaParameters sacSmaParams, 
                                                           long varModelRunTimeLong,
                                                           int uhgOrdinateCount)
    {

        // check if "basinId_sacsmaparams" file exists, if so, get pxadj and peadj with the most recent time
        try
        {
            BufferedReader inputStream = new BufferedReader(new FileReader(outputDir + basinId + "_sacsmaparams"));

            Pattern pattern = Pattern.compile(" +"); //dealing with multiple spaces
            double pxadj = 0.0, peadj = 0.0;  //holds the values of the line above
            double pxadjTemp = 0.0, peadjTemp = 0.0; //holds the current line values
            //   for(int i=0; i < loadUnitHydrograph(basinId).getMeasurementList().size(); i++)
            for(int i=0; i < uhgOrdinateCount; i++)
            {
                String[] arrayStr = pattern.split(inputStream.readLine());
                long longTime = VarHelper.getLongTimeFromDateTimeString(arrayStr[0], "yyyyMMddHH");
                pxadjTemp = Double.parseDouble(arrayStr[1]);
                peadjTemp = Double.parseDouble(arrayStr[2]);

                if ((i == 0) && (varModelRunTimeLong < longTime)) //this current time is older than the 1st validtime in the file
                {
                    return sacSmaParams;  //no need to update pxadj & peadj
                }
                else if ((i != 0 ) && (varModelRunTimeLong < longTime)) //reading has passed, pick up last line's data
                {
                    //update peadj and pxadj and get out
                    sacSmaParams.setPxadj(pxadj);
                    sacSmaParams.setPeadj(peadj);
                    return sacSmaParams;
                }
                //longTime(validtime in text file) is still older than current time, save the data and read in next line
                pxadj = pxadjTemp;
                peadj = peadjTemp;
            } //close for loop

            //if program has reached here, the whole text file has been read through and all stored pxadj, peadj validtime is older than the Var current time
            //anyway, picks up the closest validtime pxadj, peadj
            sacSmaParams.setPxadj(pxadjTemp);
            sacSmaParams.setPeadj(peadjTemp);
        }
        catch(FileNotFoundException e)  //due to 1st time running Var program on this basin, then return original sacSmaParams as it is
        {
            _varControllerLogger.log("The file "+ outputDir + basinId + "_sacsmaparams\" was not found. Original values of peadj and pxadj were used.");
            return sacSmaParams;
        }
        catch(IOException e)
        {
            _varControllerLogger.log("File IO Exception during reading SacSma parameter from the text file");
            return sacSmaParams;
        }

        return sacSmaParams;
    }


}
