package ohd.hseb.sshp.var;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.measurement.RegularTimeSeries;
import ohd.hseb.model.sacsma.SacSmaState;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.TimeHelper;

import static ohd.hseb.db.DbTimeHelper.*;
import static ohd.hseb.util.TimeHelper.*;

/********************************************************************************************************
 * The class executes ensures the following:
 * prepararation of input files, 
 * running the actual VarAssimilationProgram, 
 * check and save results.
 * @author lix, cgobs
 *
 */
public class VarController
{   
    private static int FUTURE_HOURS = 0;
    private VarDataMgr _varDataMgr; 

    private static String _varInputDir;
    private static String _varOutputDir;
    private static String _runScriptForVarAssimilationPgm;
    private static FileLogger _varControllerLogger;
    private static String _segmentsFilePath;
    private static String _varAssimilationPgmLogFile;
    private static String _argTimeString;
    private static String _database;
    
    private VarAssimilationProgramExecuter _scriptExecuter = null;
    
    private CodeTimer _singleRunTimer = new CodeTimer();
    private CodeTimer _setUpTimer = new CodeTimer();
    
    private CodeTimer _generationCodeTimer = new CodeTimer();
   // private CodeTimer _executeCodeTimer = new CodeTimer();
    private CodeTimer _readAndSaveResultsCodeTimer = new CodeTimer();
    
    private Set<String> _successSet = new HashSet<String>();
    private Set<String> _failureSet = new HashSet<String>();
    
    
    // ---------------------------------------------------------------------------------------------------------------

    public VarController(String database, String argTimeString,
                         String varInputDir,
                         String varOutputDir, 
                         String runScriptForVarAssimilationPgm,
                         FileLogger varControllerLog,
                         String fileContainingSegments,
                         String varAssimilationPgmLogFile)
    {
        _database = database;
        _varInputDir = varInputDir;
        _varOutputDir = varOutputDir;
        _runScriptForVarAssimilationPgm = runScriptForVarAssimilationPgm;
        _varControllerLogger = varControllerLog;
        _segmentsFilePath = fileContainingSegments;
        _varAssimilationPgmLogFile = varAssimilationPgmLogFile;
        _argTimeString = argTimeString;
        
        logInputs();

        _varDataMgr = new VarDataMgr(_varControllerLogger, _database,
                                     _varInputDir, _varOutputDir,
                                     _segmentsFilePath);

        _scriptExecuter =  new VarAssimilationProgramExecuter(_varControllerLogger, 
                                                              _runScriptForVarAssimilationPgm,
                                                              _varAssimilationPgmLogFile);
    }

    public VarDataMgr getVarDataMgr()
    {
        return _varDataMgr;
    }
    public String getDatabaseName()
    {
        return _database;
    }

    public String getVarInputDir()
    {
        return _varInputDir;    
    }

    public String getVarOutputDir()
    {
        return _varOutputDir;   
    }

    public String getRunScriptForVarAssimilationPgm()
    {
        return _runScriptForVarAssimilationPgm;   
    }

    public FileLogger getVarControllerLogger()
    {
        return _varControllerLogger;
    }

    public String getSegmentsFile()
    {
        return _segmentsFilePath;   
    }

    public String getVarAssimilationPgmLogFile()
    {
        return _varAssimilationPgmLogFile;
    }
//  ----------------------------------------------------------------------------------------------------------------------------------
    public void logInputs()
    {
        
        _varControllerLogger.log("-------------------------------------------------------------------------------------------");
        _varControllerLogger.log("VAR Started with Inputs:");
        _varControllerLogger.log("Version: "+ "pre-OB9"+ "  Version Date:"+ "May 5, 2008");
        _varControllerLogger.log("Database: "+ _database);
        _varControllerLogger.log("User Input Time:"+ _argTimeString);
        _varControllerLogger.log("InputDir:"+ _varInputDir);
        _varControllerLogger.log("OutputDir:"+_varOutputDir);
        _varControllerLogger.log("Variational Assimilation Program:"+ _runScriptForVarAssimilationPgm);
        _varControllerLogger.log("Variational Assimilation Program's Log File:"+ _varAssimilationPgmLogFile);
    }

    //-------------------------------------------------------------------------------------------------------------

    /*************************************************************************************************************************
     * Gets the list of all possible basin ids and for each basin id
     * makes calls to 
     *      generate deck parameter file, 
     *      generate precip input file &  inflow input file
     *      start var Fortran program.
     *      read the output files(_adj_states_02_var, _states_02_var) created by the fortran var program  and save it in database 
     *      and delete the output files 
     * Possible outcomes: 
     * 1)If all the data for a basin Id is available for preparing the input files, then FortranScriptExecuter will run.
     *   If some data is not available, proceed to next basin id.
     * 2)Three possible outcomes when running FortranScriptExecuter:
     *      a)If Fortran Var script executes successfully and met the error tolerance
     *        range, then save the var vriables into database or textfile and delete the two output files 
     *          "basinId_states_02_var" & "basinId_adj_states_02_var", forcing Fortran Var script to rely on the deck 
     *          parameter file for next execution also, not just the first time
     *      b,c)If Fortran Var script failed or has finished but error was too large, a FortranScriptRunningException
     *          was thrown. The log file "log_var" was printed out to help user know the cause. No data saving. If output files has been
     *          generated(script was run through), delete the two output files too.
     */
    public void runVarController()
    {
        String header = "VarController.runVarController(): ";
    //    _varControllerLogger.log("In "+ header);
        long overallAssimilationTargetTimeLong = TimeHelper.truncateTimeInMillisToNearestHour(getLongTimeFromDateTimeString(_argTimeString), 1);

        List<String> allBasinIdsList = _varDataMgr.getAllBasinIdsList();

        
        // Look for any recent changes to the QPE grids. We want to rerun VAR for the affected hours. 
        // So, get the time of the earliest grid affected by these changes. Rerun from then until the present.
        // If no grids have been recently edited, then the overallAssimilationTargetTime is used
        // as the latest possible time for which an adjustedSacSmaState could be found 
        // (it will probably be at least an hour earlier , in that case)
        
        long possibleValidSacSmaStateTime = getEarliestRecentlyEditedGridTime();
        if (possibleValidSacSmaStateTime == -1)
        {
            possibleValidSacSmaStateTime = overallAssimilationTargetTimeLong;
        }
    
        String message = header + "possibleValidSacSmaStateTime = " + DbTimeHelper.getDateTimeStringFromLongTime(possibleValidSacSmaStateTime) + "    " +
                                  "overallAssimilationTargetTimeLong = " + DbTimeHelper.getDateTimeStringFromLongTime(overallAssimilationTargetTimeLong);
        
        _varControllerLogger.log(message);      
        System.out.println(message);
     
        for (String basinId: allBasinIdsList)
        {        
            _varControllerLogger.log("Beginning multi-run processing For basin Id = " + basinId);

            //get the top of hr of arg time string
          
            invokeVarForMultipleRuns( basinId, possibleValidSacSmaStateTime,  overallAssimilationTargetTimeLong );
            _varControllerLogger.log("Ended multi-run processing For basin Id = " + basinId + "\n");

          //  summarizeResults();

        }//end for
        
        System.out.println("Final Results:");
        
        summarizeResults();

        _varDataMgr.disconnectDatabase();
        return;

    } 

//  -------------------------------------------------------------------------------------------------------------
  

    /* 
     * Start from the available latest unadj sacsma state, which is the one with latest valid time for this basin 
     * and source is UNADJ_VAR  
     * and run the var in intervals of 1 hr
     * until the time of user's var model run time given in cmd line arg
     * Assume var was not running for some time and then restarted
     * If the user's run time input is 8Z, the uhg length is 5 hrs, the unadj state is available for say time 1Z,
     *     then
     *     1. var should be run for 6Z(var model run time), using unadj state available for 1Z, & precip & discharge timeseries from 1Z to 6Z
     *              store var's o/p for unadj state for 2Z and adj & unadj states for 6Z 
     *                                  run of time series from 2Z to 6Z 
     *                                  all of them having same current systime as basis time
     *     2. var should be run for 7Z(var model run time), using unadj state available for 2Z, & precip & discharge timeseries from 2Z to 7Z
     *              store var's o/p for unadj state for 3Z and adj & unadj states for 7Z 
     *                                  run of time series from 3Z to 7Z 
     *                                  all of them having same current systime as basis time
     *     3. var should be run for 8Z(var model run time), using unadj state available for 3Z, & precip & discharge timeseries from 3Z to 8Z
     *              store var's o/p for unadj state for 4Z and adj & unadj states for 8Z 
     *                                  run of time series from 4Z to 8Z 
     *                                  all of them having same current systime as basis time
     * In usual cases, VAR will be run continuously every hour, so the loop will be executed only once for each execution of VAR.
     */
    private void invokeVarForMultipleRuns(String basinId, long possibleValidSacSmaStateTime, long overallAssimilationTargetTimeLong )
    {
        RegularTimeSeries dischargeTimeSeries = null;
        RegularTimeSeries precipTimeSeries = null;
        String header = "VarController.invokeVarForMultipleRuns(); ";
        int modelRunCountForThisBasin = 1;
        long assimilationStartTimeLong = 0;
        long lastAppropriateUnadjSacSmaStateTimeLong = 0;
        int numOfFutureHours = FUTURE_HOURS; //5 days worth of future data, which is used as input by var

        long uhgLengthInMillis = 0;
        //initialize the location's data for a series of hourly runs
        try
        {
            
            String locationId = _varDataMgr.getLocationIdFromBasinId(basinId);
            
            uhgLengthInMillis =   _varDataMgr.getUhgLengthInMillis(locationId, basinId);
            
            String message = "Basin = " + basinId + " UnitHydrograph Length:"+ (uhgLengthInMillis / MILLIS_PER_HOUR);
            System.out.println(message);            
            _varControllerLogger.log(message);


            lastAppropriateUnadjSacSmaStateTimeLong =
                        determineAppropriateUnadjSacStateTime(basinId, 
                                                              uhgLengthInMillis, 
                                                              possibleValidSacSmaStateTime, 
                                                              overallAssimilationTargetTimeLong);
           
            boolean checkForMissingData = false;
            
         

            dischargeTimeSeries = _varDataMgr.getDischargeRegularTimeSeries(locationId,
                    lastAppropriateUnadjSacSmaStateTimeLong,
                    overallAssimilationTargetTimeLong,
                    checkForMissingData); //some data may be missing, so don't check for missing data
            
            precipTimeSeries = _varDataMgr.getPrecipRTSforPrecipFile(basinId, lastAppropriateUnadjSacSmaStateTimeLong,
                    overallAssimilationTargetTimeLong, checkForMissingData); //some data may be missing, so don't check for missing data

            _setUpTimer.stop(header + "setup has taken ");
        

            // for every possible hourly run, up to the overallAssimilationTargetTime, execute the VAR algorithm
            for( assimilationStartTimeLong = lastAppropriateUnadjSacSmaStateTimeLong  ;
                 assimilationStartTimeLong + uhgLengthInMillis <= overallAssimilationTargetTimeLong;
                 assimilationStartTimeLong += MILLIS_PER_HOUR)
            {
                long assimilationTargetTimeLong = assimilationStartTimeLong + uhgLengthInMillis;

                String assimilationStartTimeString = DbTimeHelper.getDateTimeStringFromLongTime(assimilationStartTimeLong);
                String assimilationTargetTimeString = DbTimeHelper.getDateTimeStringFromLongTime(assimilationTargetTimeLong);

                
                if (_varDataMgr.areDischargeAndPrecipDataAvailable(dischargeTimeSeries, precipTimeSeries, assimilationTargetTimeLong) )
                {
                    _varControllerLogger.log("Model RunCount For This Basin = " + modelRunCountForThisBasin);
                    _varControllerLogger.log("Assimilation Start Time = " + assimilationStartTimeString);
                    _varControllerLogger.log("Assimilation Target Time = " + assimilationTargetTimeString);

                   // System.out.println("Model RunCount For This Basin:" + modelRunCountForThisBasin);
                    System.out.print("Assimilation Start Time:[" + assimilationStartTimeString);
                    System.out.println("   Assimilation Target Time:[" + assimilationTargetTimeString);


                    _singleRunTimer.start();

                    executeVarForSingleRun( basinId, numOfFutureHours,  assimilationStartTimeLong,  assimilationTargetTimeLong);

                    _singleRunTimer.stop(header + " executeVarForSingleRun() took ");

                    System.out.println();
                    
                    modelRunCountForThisBasin++;

                }
                else
                {
                   // _varControllerLogger.log("Either Discharge or precip not available at assimilation Target Time:[" + assimilationTargetTimeString);
                   // System.out.println("Either Discharge or precip not available at assimilation Target Time:[" + assimilationTargetTimeString);
                    logFailure(basinId, assimilationStartTimeLong, assimilationTargetTimeLong);
                }

            }

        }
        catch(DataAbsentException e)
        {
            //String message = "Initial sac sma states are not available for [" +basinId+"]... proceeding to next basinId";
            //System.out.println(message);
            _varControllerLogger.log(e.getMessage());
            return;
        }



    } //end invokeVarForMultipleRuns
//  -------------------------------------------------------------------------------------------------------------
    /**
            This routine determines the Unadjusted SacState time that is needed to kick off 1 or more iterations of VAR runs for a particular basin.
    */
    private long determineAppropriateUnadjSacStateTime(String basinId,
                                                       long uhgLengthInMillis,
                                                       long possibleValidSacSmaStateTime, 
                                                       long overallAssimilationTargetTimeLong) throws DataAbsentException
    {
        String overallAssimilationTargetTimeString = DbTimeHelper.getDateTimeStringFromLongTime(overallAssimilationTargetTimeLong);
    
        long lastAppropriateUnadjSacSmaStateTimeLong = 0;
        boolean thereIsNoEarlyEnoughUnadjustedSacSmaState = false;
        String message = null;

        _setUpTimer.restart();
               
        //get the latest adjust sac sma state that we are allowed to use
        SacSmaState lastAdjustedSacSmaState = _varDataMgr.loadLastAdjustedSacSmaStateAtOrBeforeATime(basinId,
                                                                                                     possibleValidSacSmaStateTime);
        
        //If we have a lastAdjusted state, then find an unadjusted state than can be used to compete the next hours
        long lastAdjustedTime = -1;   
        if (lastAdjustedSacSmaState != null)
        {
            lastAdjustedTime = lastAdjustedSacSmaState.getValidTime();
            message = "Latest adjusted sac state's valid time : "+ DbTimeHelper.getDateTimeStringFromLongTime(lastAdjustedTime);
            System.out.println(message);
            _varControllerLogger.log(message);
            
            //This gets a SacSmaState that is appropriate to be the starting point of a VAR run for a time that ends 
            //UHG_length hours later - which is (lastAdjustedtime + MILLIS_PER_HOUR)
            SacSmaState lastAppropriateUnadjSacSmaState = 
                                    _varDataMgr.loadPrevUnadjSacSmaState(basinId, uhgLengthInMillis, lastAdjustedTime + MILLIS_PER_HOUR);
        
            
            if (lastAppropriateUnadjSacSmaState != null)
            {
                  lastAppropriateUnadjSacSmaStateTimeLong = lastAppropriateUnadjSacSmaState.getValidTime();
            }
            else 
            {
                thereIsNoEarlyEnoughUnadjustedSacSmaState = true;
                //There was not an unadjusted sac sma state early enough to create the next ADJUSTED sac-sma time
            }
                        
        }
        
        
        //if we can't find an adjusted state or we don't have an appropriate unadjusted state to use,
        // then do our best to determine which problem it is and then either bail or start with the oldest available unadjusted state
        if ((lastAdjustedSacSmaState == null) || (thereIsNoEarlyEnoughUnadjustedSacSmaState) ) //There are no available adjusted SAC-SMA times
        {
            message = "No previous adjusted state was found. "+ DbTimeHelper.getDateTimeStringFromLongTime(lastAdjustedTime) + "So, try starting with the oldest VAR unadjusted state.";
            System.out.println(message);
            _varControllerLogger.log(message);
                
            SacSmaState lastAppropriateUnadjSacSmaState = _varDataMgr.loadOldestUnadjSacSmaState(basinId);
            
            if (lastAppropriateUnadjSacSmaState != null)
            {
                lastAppropriateUnadjSacSmaStateTimeLong = lastAppropriateUnadjSacSmaState.getValidTime();
            }
            else //lastAppropriateUnadjSacSmaState == null
            {
                throw new DataAbsentException("There is no available unadjusted SAC-SMA state from which to begin a VAR run for basinId = " + basinId +"\n");
            }

        }
        
         
        String lastAvailUnadjSacSmaStateTimeString = 
            DbTimeHelper.getDateTimeStringFromLongTime(lastAppropriateUnadjSacSmaStateTimeLong);

        message = "Latest unadjusted sac state's valid time : "+ lastAvailUnadjSacSmaStateTimeString;
        System.out.println(message);
        _varControllerLogger.log(message);
        
        System.out.println("for " + basinId + " overall assimilation target time :"+ overallAssimilationTargetTimeString);
        System.out.println("Var start time for 1st run:"+ 
                DbTimeHelper.getDateTimeStringFromLongTime(overallAssimilationTargetTimeLong - uhgLengthInMillis));
       
        return lastAppropriateUnadjSacSmaStateTimeLong;
    }
    
//  -------------------------------------------------------------------------------------------------------------
   
    private long getEarliestRecentlyEditedGridTime()
    {
        //Purpose: find the earliest time of a file edit that occurred after the latest SACSMA state posting time
        
        long earliestGridTime = -1;
      
        long latestPostingTimeToSacSmaState = _varDataMgr.getLatestVarSacSmaStatePostingTime();
             
        //find the earliest locally-produced MPE file time after the latest posting time 
        List localGridTimeList = _varDataMgr.getNewerBestQpeXmrgFileTimeList(latestPostingTimeToSacSmaState);
        
        if (localGridTimeList.size() > 0)
        {
            earliestGridTime = (Long) localGridTimeList.get(0);
        } 
           
        //find the earliest RFC-produced MPE file time after the latest posting time 
        //if any, compare to the 
        List rfcGridTimeList = _varDataMgr.getNewerRfcXmrgFileTimeList(latestPostingTimeToSacSmaState);
       
        if (rfcGridTimeList.size() > 0)
        {
            long rfcEarliestGridTime = (Long) rfcGridTimeList.get(0);
            
            //if there were new local grids
            if  (earliestGridTime > -1) 
            {
              //if rfc grid time is earlier, use that earlier time
                if (rfcEarliestGridTime < earliestGridTime) 
                {
                    earliestGridTime = rfcEarliestGridTime;
                }
            }
            else // there were not any new local grids, so just use the rfc time
            {
                earliestGridTime = rfcEarliestGridTime;
            }
        }
        
        return earliestGridTime;
    }
    
//  -------------------------------------------------------------------------------------------------------------

    
    private void executeVarForSingleRun(String basinId, int numOfFutureHours, 
                                        long assimilationStartTimeLong, long assimilationTargetTimeLong)
    {    
        String header = "VarController.executeVarForSingleRun(): ";
        
        deleteFilesFromPreviousRun(); // delete output files created by fortran var during previous run
        
        boolean isVarAssimilationSuccessful = false;
        //generate the basinId.var_deck file, precipitation file "basinId.mapx_ts_var_dat" file & discharge file "basinId.qin01_ts_var_dat" file
        
        String assimilationStartTimeString =  DbTimeHelper.getDateTimeStringFromLongTime(assimilationStartTimeLong);
        String assimilationTargetTimeString =  DbTimeHelper.getDateTimeStringFromLongTime(assimilationTargetTimeLong);
    
        _generationCodeTimer.restart();
        
        boolean generationSuccessful = _varDataMgr.generateVarInputFiles(basinId,
                                                                         numOfFutureHours,
                                                                         assimilationStartTimeLong, 
                                                                         assimilationTargetTimeLong);
        _generationCodeTimer.stop(header + "File Generation has taken: ");
        
        if (generationSuccessful)
        {
            //System.out.println(header + "file generation succeeded for " + basinId + " from " +
            //                    assimilationStartTimeString + " to " +  assimilationTargetTimeString + ".");
          
            isVarAssimilationSuccessful = runVarAndSaveResults(basinId,
                                                               assimilationStartTimeLong,
                                                               assimilationTargetTimeLong,
                                                               assimilationStartTimeString,
                                                               assimilationTargetTimeString);  
       
        }
        else //file generation not successful
        {
            String message = "\nFile generation failed for basin " + basinId + 
                             " from " + assimilationStartTimeString + " to " + 
                             assimilationTargetTimeString +  ".";
            
            _varControllerLogger.log(message);
            
            System.out.println(message); 
        }
             
        //long success or failure of run
        logResults(basinId,  assimilationStartTimeLong, assimilationTargetTimeLong, isVarAssimilationSuccessful);

        return;
    }
//  -------------------------------------------------------------------------------------------------------------  
    private boolean runVarAndSaveResults(String basinId,
                                      long assimilationStartTimeLong,
                                      long assimilationTargetTimeLong,
                                      String assimilationStartTimeString,
                                      String assimilationTargetTimeString)
    {
        String header = "VarController.runVarAndSaveResults(): ";
//      run the Fortran var program           
        boolean isVarAssimilationSuccessful = _scriptExecuter.executeVarAssimilationProgram();
        boolean readAndSaveSuccessful = false;
        
        
        if(isVarAssimilationSuccessful)
        {
            //saving the generated results
            _readAndSaveResultsCodeTimer.restart();
            
            readAndSaveSuccessful = _varDataMgr.readAndSaveGeneratedResults(basinId,
                                                                                    assimilationStartTimeLong,
                                                                                    assimilationTargetTimeLong);
            
           _readAndSaveResultsCodeTimer.stop(header + "overall, readAndSaveGeneratedResults() has taken: ");
             
            if(readAndSaveSuccessful)
            {
                _varControllerLogger.log("Saved results for basin = " + basinId + 
                                         " from " + assimilationStartTimeString + " to " + assimilationTargetTimeString +  ".");
            }
            else
            {
                _varControllerLogger.log("Generated files, but unable to save results for basin = " + basinId + 
                        " from " + assimilationStartTimeString + " to " + assimilationTargetTimeString +  ".");
            }
        } //end if var successful

        
        else //data assimilation not successful
        {
            String message = "Data assimilation failed for basin = " + basinId + " for period from " 
                             + assimilationStartTimeString + " to " + assimilationTargetTimeString +  ".";
            
            _varControllerLogger.log(message);             
            System.out.println(message); 
        }
        
        return readAndSaveSuccessful;
        
    }
    
//  -------------------------------------------------------------------------------------------------------------  
    private void deleteFilesFromPreviousRun()
    {
        deletePreviousDeckFiles();
        deletePreviousOutputFiles();
        
    }
//  -------------------------------------------------------------------------------------------------------------  
    private void printAllFiles(String directoryPath)
    {
        File directory = new File(directoryPath);
        File[] fileArray2 = directory.listFiles();
        
        System.out.println("-----------------------------------:");
        System.out.println("Files in the input directory:");
        
        try
        {
            for (File file : fileArray2)
            {

                System.out.println(file.getCanonicalFile());
            }
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        
        System.out.println("-----------------------------------:");
    }
    
//  -------------------------------------------------------------------------------------------------------------  

    private void deletePreviousDeckFiles()
    {
       // String header = "VarController.deletePreviousDeckFiles(): ";

        //delete o/p files generated by fortran var program
        File directory = new File(_varInputDir + "/param/");

        File[] fileArray = directory.listFiles();

        if (fileArray == null)
        {
            return;
        }
        
        for (File file : fileArray)
        {          
            try
            {
                if (file.getName().endsWith("deck") && (file.isFile()))
                {

                    // System.out.println(header + " deleting " + file.getCanonicalPath());
                    boolean result = file.delete();
                    if (result)
                    {
                        //System.out.println(header + " I think I really deleted " + file.getCanonicalPath());
                    }
                    else
                    {
                        //System.out.println(header + " I don't think I really deleted " + file.getCanonicalPath());
                    }
                }
            }
            catch (Exception e)
            {
                e.printStackTrace();
            }
        }

    }

//  -------------------------------------------------------------------------------------------------------------  

    private void deletePreviousOutputFiles()
    {
        
       // String header = "VarController.deletePreviousOutputFiles(): ";
        
        //delete o/p files generated by fortran var program
        File directory = new File(_varOutputDir);
        
        File[] fileArray = directory.listFiles();
        
        if (fileArray == null)
        {
            return;
        }
        
        for (File file : fileArray)
        {
            try
            {
                if (file.isFile())
                {
                     file.delete();
                }
            }
            catch (Exception e)
            {
                e.printStackTrace();
            }
        }
        
       // printAllFiles(_varOutputDir);
        
    } 
//  -------------------------------------------------------------------------------------------------------------
    private void logSuccess(String basinId, long startTime, long endTime)
    {
        
        logResults(basinId,  startTime, endTime, true);
    }

    //  -------------------------------------------------------------------------------------------------------------

    private void logFailure(String basinId, long startTime, long endTime)
    {
        
        logResults(basinId,  startTime, endTime, false);
    }
    
//  -------------------------------------------------------------------------------------------------------------

    private void logResults(String basinId, long startTime, long endTime, boolean wasSuccess)
    {
        String startTimeString = DbTimeHelper.getDateTimeStringFromLongTime(startTime);
        String endTimeString = DbTimeHelper.getDateTimeStringFromLongTime(endTime);
        
        
        String runInfoString = basinId + '|' + startTimeString + '|' + endTimeString;
        
        if (wasSuccess)
        {
            _successSet.add(runInfoString);
        }
        else //was failure
        {
            _failureSet.add(runInfoString);
        }
        
        return;
    }

    //  -------------------------------------------------------------------------------------------------------------
 
    private void summarizeResults()
    {
        int successCount = _successSet.size();
        int failureCount = _failureSet.size();
        
        System.out.println("VAR execution summary --------------------------------");
        
        //print out success info
        System.out.println("The following are the VAR runs that succeeded:");
         
        
        for (String successString : getSortedListFromSet(_successSet))
        {
            System.out.println(successString);
        }
        
        
        //print out failure info
        System.out.println("The following are the VAR runs that failed:");        
        for (String failureString : getSortedListFromSet(_failureSet))
        {
            System.out.println(failureString);
        }
        
        System.out.println("There were " + successCount + " successful runs.");
        System.out.println("There were " + failureCount + " failed runs.");
        
        System.out.println("------------------------------------------------------");
        
        
        return;
    }
    
//  -------------------------------------------------------------------------------------------------------------
    List<String> getSortedListFromSet(Set originalSet)
    {
        List list = new ArrayList(originalSet);
        Collections.sort(list);
  
        return list;
    }
    
//  -------------------------------------------------------------------------------------------------------------

    
    /**************************************************************************************************************
     * This is the main method in the project. It takes input argumets from the run script
     * @param args
     */

    public static void main(String[] args)
    {
        String database = args[0];
        String argTimeString = args[1]; 
        if(argTimeString.equals("CURRENT_TIME"))
        {
            argTimeString = DbTimeHelper.getDateTimeStringFromLongTime(System.currentTimeMillis());
        }
        _varControllerLogger = new FileLogger(args[2] + "/" + args[3], true, true);

        System.out.println("Log file:"+ args[2]+"/"+args[3]);
        _varInputDir = args[4] +"/";
        _varOutputDir = args[5] + "/";
        _runScriptForVarAssimilationPgm = args[6];
        _segmentsFilePath = _varInputDir + "/" +args[7];
        _varAssimilationPgmLogFile = args[8];


        VarController varAssimilationController = new VarController(database, argTimeString, _varInputDir , _varOutputDir,
                _runScriptForVarAssimilationPgm, _varControllerLogger, 
                _segmentsFilePath, _varAssimilationPgmLogFile );

        _varControllerLogger.log("-------------------------------------------------------------------------------------------");
        _varControllerLogger.log("Application Started");

        varAssimilationController.runVarController();

        _varControllerLogger.log("Application Exiting...");
        _varControllerLogger.log("-------------------------------------------------------------------------------------------");
        System.exit(0);
    }

} //close class

