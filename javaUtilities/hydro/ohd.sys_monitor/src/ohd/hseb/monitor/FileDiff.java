package ohd.hseb.monitor;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

public class FileDiff
{
    // -----------------------------------------------------------------------------------
    
    public FileDiff()
    {
        
    }
  
    // -----------------------------------------------------------------------------------
    private String diffToString(String file1Path, String file2Path)
    {
        String header = "FileDiff.diffToString(): ";
        
       
        StringBuffer buffer = new StringBuffer();
        Runtime runtime = Runtime.getRuntime();
        
        String commandString = "diff -Bbu " + file1Path + " " + file2Path;
        //String commandString = "/fs/home/cgobs/file_monitor/data/diff_files " + file1Path + " " + file2Path + " " + diffFilePath;
        
        System.out.println(header + "commandString = " + commandString);
          
        CommandRunner runner = new CommandRunner();
        runner.executeCommand(commandString);
        
        String rawDiffString = runner.getStandardOutput();
        
        System.out.println(header + "rawDiffString = " + rawDiffString);
        
        String newLinesOnly = filterByNonRepeatedLeadCharacter('+', rawDiffString);
         
        
        return newLinesOnly;
    } 
    // -----------------------------------------------------------------------------------        
    private String filterByNonRepeatedLeadCharacter(char leadCharacter, String originalString)
    {
        BufferedReader reader = new BufferedReader(new StringReader(originalString));
        StringBuffer buffer = new StringBuffer();
        
        boolean done = false;
        String line = null;
        
/*  Since this routine filters everything but the new lines or the old lines, we don't want to include the --- or the +++
 
+++ /fs/home/cgobs/file_monitor/data/test/file1 2009-02-18 09:05:46.000000000 -0500
@@ -2,3 +2,4 @@
 blah2
 blah3
 blah4
+blah5
        
*/

        try
        {

            while (! done)
            {
                line = reader.readLine();
                if  ( (line != null) && (line.length() > 1) )
                {
                    if (line.charAt(0) == leadCharacter)  //first is lead
                    {
                        if (line.charAt(1) != leadCharacter) //it does not repeat first character
                        {
                            buffer.append(line.substring(1));
                        }
                    }
                }
                else if (line == null)
                {
                    done = true;
                }
            }
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }

        return buffer.toString();
    }
     
    // -----------------------------------------------------------------------------------
    public String showNewLines( String file1Path, String file2Path)
    {
        return diffToString(file1Path, file2Path);

        //    return showNewLinesJavaOnly(file1Path, file2Path);
    }

    // -----------------------------------------------------------------------------------

    public String showNewLinesJavaOnly( String file1Path, String file2Path)
    {
        StringBuffer buffer = new StringBuffer();
        BufferedReader reader1 = null;
        BufferedReader reader2 = null;
        String line1 = null;
        String line2 = null;
        int line1Count = 0;
        int line2Count = 0;

        try
        {
            reader1 = new BufferedReader (new FileReader(file1Path));
            reader2 = new BufferedReader (new FileReader(file2Path));

            boolean done = false;      

            while (! done)
            {
                line1 = reader1.readLine();
                line2 = reader2.readLine();


                if (equalLines(line1, line2))
                {


                    if (bothNull(line1, line2))
                    {
                        done = true;
                    }
                    else
                    {
                        line1Count++;
                        line2Count++;

                    }
                }

                else //lines are not equal
                {           
                    if (line1 != null)
                    {
                        buffer.append("File1 :" + line1);
                        line1Count++;
                    }

                    if (line2 != null)
                    {
                        buffer.append("File2 :" + line2);
                        line2Count++;
                    }
                }

                System.out.println("File 1 line = " + line1Count + ": " + line1);
                System.out.println("File 2 line = " + line2Count + ": " + line2);
            }
        }

        catch (IOException e)
        {
            e.printStackTrace();
        }

        return buffer.toString();

    }
    // -----------------------------------------------------------------------------------

    private boolean bothNull(String line1, String line2)
    {
        return  ( (line1 == null) && (line2 == null) );
    }
    // -----------------------------------------------------------------------------------

    private boolean equalLines(String line1, String line2)
    {
        boolean equal = false;

        if ( (line1 == null) && (line2 == null) )  //both null
        {
            equal = true;
        }
        else if  ((line1 != null) && (line2 != null) ) //neither null
        {
            equal = line1.equals(line2);
        }



        return equal;
    }

    // -----------------------------------------------------------------------------------

    
} //end class FileDiff
