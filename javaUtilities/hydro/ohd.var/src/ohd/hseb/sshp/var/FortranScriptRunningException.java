package ohd.hseb.sshp.var;

/*******************************************************************************************************************
 * This class is used when FortranScriptExecuter running the Fortran script var needs to throw an Exception. It has 
 * a field containing the log_var file name with absolute path and a field containing the error message.
 * @author lix
 *
 */
public class FortranScriptRunningException extends Exception 
{
    public FortranScriptRunningException(String message)
    {
        super(message);
    }
} //close class
