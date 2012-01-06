package ohd.hseb.sshp.var;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Map;
import java.util.regex.Pattern;

import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.FileLogger;

/**************************************************************************************************************
 * This class executes the Fortran program called Var. There are three possible outcomes: 
 * 1)The Fortran script successfully 
 * ran through AND the calculation error was within the tolerance range. -- This is the only case that Var results need
 * to be saved.
 * 2)The Fortran script was not  able to run, due to lack of data or some other reasons. The 
 * program throws a FortranScriptRunningException object.
 * 3)(intermediate situation between the previous 2 cases) The Fortran script
 * was successfully ran through, however, Var could not adjust state variables enough to meet the error tolerance range. A
 * FortranScriptRunningException object was thrown with message "var adjustment has failed.". 
 * @author lix
 *
 */
public class VarAssimilationProgramExecuter 
{
    private String _runScriptForFortranVarPgm; 
    private String _varAssimilationPgmLogFilePath;
    private FileLogger _varControllerLogger; 
    private CodeTimer _timer = new CodeTimer();
     

    public VarAssimilationProgramExecuter(FileLogger logger,
                                          String runScriptForFortranVarPgm, 
                                          String varAssimilationPgmLogFilePath)
    {
        _runScriptForFortranVarPgm = runScriptForFortranVarPgm;
        _varControllerLogger = logger;
        _varAssimilationPgmLogFilePath = varAssimilationPgmLogFilePath;
    }

//  ----------------------------------------------------------------------------------------------------------------------------------
    /*********************************************************************************************************
     * Generate the file "list_of_segments", then start Fortran var program, in the end, check if running is 
     * success or not.
     */
    public boolean executeVarAssimilationProgram()
    {
         String header = "VarAssimilationProgramExecuter.executeVarAssimilationProgram(): ";
        _varControllerLogger.log("In "+ header);

        _varControllerLogger.log("Starting the Fortran Var Program...");

        System.out.println(header + "monitoring the file: " + _varAssimilationPgmLogFilePath);
        boolean success = false;

        try
        {
            
            _timer.restart();
            
            //run Var Fortran program
            Runtime runtime = Runtime.getRuntime();           
          
            //run FORTRAN program
            Process process = runtime.exec(_runScriptForFortranVarPgm, getEnvironment() );
  
            determineExecutionResults(process, _varAssimilationPgmLogFilePath);

            //var has finished
            _timer.stop("The Fortran VAR program overall has taken ");
            _varControllerLogger.log("The Fortran VAR has taken " + _timer.getElapsedTime() + " millis.");

            success = true;
            
        }
        catch (FortranScriptRunningException fe)
        {
            fe.printStackTrace();
            fe.printStackTrace(_varControllerLogger.getPrintWriter());
            
            String message = "Var Assimilation is not completed... reason:\n "+ fe.getMessage();
            System.out.println(message);
            _varControllerLogger.log(message);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            e.printStackTrace(_varControllerLogger.getPrintWriter());
            
        }

        return success;
    } //close method
    // -----------------------------------------------------------------------
  
    private String getStandardError(Process p)
    {
        //String header = "VarAssimilationProgramExecuter.getStandardError():";
        
        StreamDrainer drainer = new StreamDrainer(p.getErrorStream());
        String drainedString = drainer.drain();
        
        //System.out.println(header + " drainedString = " + drainedString);
        
        return drainedString;
           
    }
    // -----------------------------------------------------------------------

    private String getStandardOutput(Process p)
    {
        //String header = "VarAssimilationProgramExecuter.getStandardOutput():";
        
        StreamDrainer drainer = new StreamDrainer(p.getInputStream());
        String drainedString = drainer.drain();
        
        //System.out.println(header + " drainedString = " + drainedString);
        
        return drainedString;
    } 
    // -----------------------------------------------------------------------
       
    private boolean determineExecutionResults(Process process,
                                              String logFileToExamine)
                                              throws FortranScriptRunningException
    { 
        
        boolean success = false;
        String header = "VarAssimilationProgramExecuter.determineExecutionResults(): ";

        BufferedReader bufferedReader = null;
        File fileHandler = null;
        //     boolean found = false;


        try  //3 possible outcomes
        {
            getStandardOutput(process);          
            getStandardError(process);
              
            process.waitFor();
   
            fileHandler = new File(logFileToExamine);

            bufferedReader = new BufferedReader(new FileReader(fileHandler)); 
            
            //check if the Var Fortran program has completed successfully or not
            Pattern successPattern = Pattern.compile(".*success.*");
            Pattern errorPattern   = Pattern.compile(".*error .* too large for ");
            Pattern skipVarPattern  = Pattern.compile("skip var");

            String line = null;
            while((line = bufferedReader.readLine()) != null )
            {
                System.out.println(header + "line =  " + line);

                if (errorPattern.matcher(line).find())
                {
                     throw new FortranScriptRunningException("fortran var failed with log file message:" + line ); 
                    //intermediate outcome: var was completed, but error was out of tolerance range,   
                    //mainly due to little precipitation in the period
                }
                else if ( skipVarPattern.matcher(line).find())
                {
                    throw new FortranScriptRunningException("var skipped because: " + line );
                }

                else if (successPattern.matcher(line).find())
                {
                    _varControllerLogger.log("Var Assimilation completed successfully!!!");
                    success = true;
                    break;  //best outcome: var was completed successfully and error was within tolerance
                }
            } //close  while 

            //if program reaches here, the worst outcome:  var was not able to be completed 
            //_varControllerLogger.log("var was not able to be completed due to some reasons.");
            if (!success)
            {
              //  process.destroy();
                throw new FortranScriptRunningException("Failure: Unable to determine success or failure based on fortran log file...");
            }
        }

    catch(Exception e)
    {
        String message = e.getMessage() +" exception was thrown when running Fortran var script.";
        //  _varControllerLogger.log(message);
        throw new FortranScriptRunningException(message);
    }

    finally  //always get executed, no matter Exception or not
    {
        if(fileHandler != null) 
        {
            try
            {
                if(bufferedReader != null)
                {
                    bufferedReader.close();
                }
            }
            catch(IOException e)
            {
                _varControllerLogger.log(e.getMessage());
            }
        }
    }
    
    return success;

}
    
//  -----------------------------------------------------------------------
  
    public String[] getEnvironment() 
    {
       
        Map<String, String> envMap = System.getenv();

        String[] envStringArray = new String[envMap.size()];
        int i =0;
        for (String key : envMap.keySet())
        {
            envStringArray[i] = key + "=" + envMap.get(key);
            i++;
        }

        return envStringArray;
    }
//  ----------------------------------------------------------------------------------------------------------------------------------
 
} //close class
