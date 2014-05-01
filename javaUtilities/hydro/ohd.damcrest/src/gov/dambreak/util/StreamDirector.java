package gov.dambreak.util;

import java.io.*;
/**
 * Class to redirect I/O streams to Process(es) invoked via Runtime.exec() 
 * Creation date: (10/31/02 3:42:35 PM)
 * @author: Daniel Urban 
 */
public class StreamDirector extends Thread 
{
    
    java.io.InputStream is;
    java.lang.String type;
    public java.io.OutputStream os;
    
    /**
     * Abbreviated constructor
     * Creation date: (10/31/02 3:54:16 PM)
     * @param inStream java.io.InputStream
     * @param inType java.lang.String
     */
    public StreamDirector(InputStream inStream, String inType) 
    {
        
        // Call Alternate constructor
        this ( inStream, inType, null );
        
    } 
    /**
     * Primary Constructor, currently left private
     *   until OutputStream logic is verified
     * Creation date: (10/31/02 3:57:54 PM)
     * @param inStream java.io.InputStream
     * @param inType java.lang.String
     * @param outStream java.io.OutputStream
     */
    StreamDirector(InputStream inStream, String inType, OutputStream outStream) 
    {
        this.is = inStream;
        this.type = inType;
        this.os = outStream;
    }
    /**
     * automatically invoked by overloaded run() method in
     */
    protected void doWork() 
    {
        try 
        {
            PrintWriter pw = null;
            if (os != null)
            {
                pw = new PrintWriter (os);
            }	
            InputStreamReader isr = new InputStreamReader(is);
            BufferedReader br = new BufferedReader(isr);
            String line=null;
            
            while ( (line=br.readLine()) != null)
            {
                if (pw != null)
                {
                    pw.println(line);
                }
                System.out.println(type + ">" + line);
            }
            if (pw != null)
            {
                pw.flush();
            }
        } 
        catch (IOException ioe)
        {
            ioe.printStackTrace();
        }
    }
    /**
     * Overloaded method from Thread
     * Creation date: (10/9/2003 2:10:47 PM)
     */
    public void run() 
    {
        this.doWork();
        
    }
}
