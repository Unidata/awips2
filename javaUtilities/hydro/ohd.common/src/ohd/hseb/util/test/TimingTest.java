package ohd.hseb.util.test;

import ohd.hseb.util.*;

public class TimingTest
{
   
    private long _longTotal = 0;
    private double _doubleTotal = 0.0;
    

    public static void main(String[] args)
    {
        TimingTest test = new TimingTest();
        CodeTimer timer = new CodeTimer();
          
        int iterationCount = 1000000;
        long pureJavaTotalTime = 0;
    
        //timer.start();
        //test.outputTest("Chip", 10, iterationCount);
       // timer.stop("Pure Java output time for " + iterationCount + " iterations ");
        
        timer.start();
        test.intTest(iterationCount);
        timer.stop("intTest time for " + iterationCount + " iterations ");
        
        
        timer.start();
        test.floatTest(iterationCount);
        timer.stop("floatTest time for " + iterationCount + " iterations ");
         
         
        test.showResults(); 
         
        return;    


    }
   
     public void showResults()
     {
         System.out.println("longTotal = " + _longTotal);
         System.out.println("doubleTotal = " + _doubleTotal);
     }
   
   
    //--------test loop methods ---------------------------------------------------------

    //-------------------------------------------------------------------
  
    public void outputTest(String name, int age, int iterationCount)
    {
    
        for (int i = 0; i < iterationCount; i++)
        {
            
            outputMethod(name, age);
  
        }
          
        return;
        
    }
  
    //-------------------------------------------------------------------
    
    public void intTest(int iterationCount)
    {
    
        int j = 0;
        int k = 0;
        
      
        for (int i = 0; i < iterationCount; i++)
        {
            j = i + 2;
           
            k = intComputeMethod(i, j);
            _longTotal += k;
             
        }
        
       
       
        
        return;
        
    }
    
    //-------------------------------------------------------------------

    public void floatTest(int iterationCount)
    {
    
       
        long totalTime = 0;
        int j;
        float k = 0f;
        
      
        for (int i = 0; i < iterationCount; i++)
        {
            j = i + 2;
           
            k = floatComputeMethod( (float)i, (float)j );
             
            _doubleTotal += k;    
        }
       
        
        return;
        
    }
    
    
    //------work methods --------------------------------------------
    
    public void outputMethod(String name, int age)
    {
        System.out.println("My name = " + name + " and my age = " + age + ".");
    }
 
     
    public int intComputeMethod(int i, int j)
    {
        return i + j;
    }
 
    
    public float floatComputeMethod(float i, float j)
    {
         return i + j;
    }
       

} //end class TimingTest
