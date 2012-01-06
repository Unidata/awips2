package ohd.hseb.sshp.var;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringWriter;

public class StreamDrainer implements Runnable
{
    private InputStream  _inputStream;
    private StringBuffer _buffer =  null;
 //  ------------------------------------------------------------------------------------
 
    public StreamDrainer(InputStream inputStream)
    {
        _inputStream = inputStream;
        _buffer = new StringBuffer();
    }
//  ------------------------------------------------------------------------------------
    
    
    public void run()
    {
        try
        {
            drainInputStream(_inputStream);
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }

    }
//  ------------------------------------------------------------------------------------
    /*
    public String drain()
    {
        Thread thread = new Thread(this);
        thread.start();
        
        return getString();
    }
    */
    
    public String drain()
    { // last updated 1/17/08 
        Thread thread = new Thread(this);
        thread.start();
        
        while ( thread.isAlive() )
        {
            try
            {
                Thread.sleep( 10 );
            }
            catch ( InterruptedException e )
            {
                //do nothing
            }
        }
     //   System.out.println( "Right after start" );
        return getString();
    }

//  ------------------------------------------------------------------------------------
    
    public String getString()
    {
        return _buffer.toString();
    }
    
    // ------------------------------------------------------------------------------------
    
    private void drainInputStream(InputStream stream) throws IOException
    {
        String header = "StreamDrainer.drainInputStream(): ";
        StringWriter stringWriter = new StringWriter();
        BufferedReader reader = new BufferedReader(new InputStreamReader(stream));
        String s = null;
        
        while ((s = reader.readLine()) != null)
        {
           // stringWriter.write(s + "\n");
            _buffer.append(s + "\n");
          //  System.out.println(header + s + "n");
        }
        
        stringWriter.flush();
        
        return;
    }

    
}
