package ohd.hseb.monitor;

import ohd.hseb.util.StreamDrainer;

public class CommandRunner
{
    
    private String _standardOutput = null;
    private String _standardError = null;
    
    // -----------------------------------------------------------------------

    public boolean executeCommand(String commandString)
    {
         String header = "CommandRunner.executeProgram(): ";
      
     
        boolean success = false;

        try
        {
            Runtime runtime = Runtime.getRuntime();           
            Process process = runtime.exec(commandString);
  
            setStandardOutput(getStandardOutputFromProcess(process));          
            setStandardError(getStandardErrorFromProcess(process));
               
            success = true;        
        }
       
        catch (Exception e)
        {
            e.printStackTrace();        
        }

        return success;
    } //close method
    
    // -----------------------------------------------------------------------


    private String getStandardErrorFromProcess(Process p)
    {
        //String header = "VarAssimilationProgramExecuter.getStandardError():";
        
        StreamDrainer drainer = new StreamDrainer(p.getErrorStream());
        String drainedString = drainer.drain();
        
        //System.out.println(header + " drainedString = " + drainedString);
        
        return drainedString;
           
    }
    // -----------------------------------------------------------------------

    private String getStandardOutputFromProcess(Process p)
    {
        //String header = "VarAssimilationProgramExecuter.getStandardOutput():";
        
        StreamDrainer drainer = new StreamDrainer(p.getInputStream());
        String drainedString = drainer.drain();
        
        //System.out.println(header + " drainedString = " + drainedString);
        
        return drainedString;
    } 
    // -----------------------------------------------------------------------

    public void setStandardOutput(String standardOutput)
    {
        _standardOutput = standardOutput;
    }

    public String getStandardOutput()
    {
        return _standardOutput;
    }

    public void setStandardError(String standardError)
    {
        _standardError = standardError;
    }

    public String getStandardError()
    {
        return _standardError;
    }
    
    
}
