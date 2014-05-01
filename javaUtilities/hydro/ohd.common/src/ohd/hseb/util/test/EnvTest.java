package ohd.hseb.util.test;

import java.util.*;
import ohd.hseb.util.*;


public class EnvTest
{
   
    private long _envTotal = 0;
     

    public static void main(String[] args)
    {
        EnvTest test = new EnvTest();
        CodeTimer timer = new CodeTimer();
    
          
        int iterationCount = 10;
        long pureJavaTotalTime = 0;
    
      
        timer.start();
        test.envTest(iterationCount);
        timer.stop("intTest time for " + iterationCount + " iterations ");
   
        return;    

    }


    public void envTest(int iterationCount)
    {
    
        Properties props = System.getProperties();
        
        props.list(System.out);
    }
   
   
  

} //end class EnvTest
