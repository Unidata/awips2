
package ohd.hseb.util.test;


public class JniTest
{
 
    static
    {
        try
        {
           System.loadLibrary("JniTest");
        }
        catch(Throwable t)
        {
           System.out.println("Unable to loadLibrary.");
        }
    }

    public static void main(String[] args)
    {
        JniTest test = new JniTest();
        System.out.println("I am calling jniMethod from java");
        test.jniMethod("Chip", 10);
    
        int iterationCount = 10000;
        long nativeTotalTime = 0;
        long pureJavaTotalTime = 0;
    
        pureJavaTotalTime = test.jTest("Chip", 10, iterationCount);
        
        nativeTotalTime = test.nativeTest("Chip", 10, iterationCount);
    
        System.out.println("Java calling C native time = " + nativeTotalTime + " millis. for " + iterationCount + " iterations");
        System.out.println("Pure Java time = " + pureJavaTotalTime + " millis. for " + iterationCount + " iterations");
        
        return;    


    }
    
    public long jTest(String name, int age, int iterationCount)
    {
    
       
        long totalTime = 0;
        long time1 = 0;
        long time2 = 0;
        
        time1 = System.currentTimeMillis();
        for (int i = 0; i < iterationCount; i++)
        {
            
            jMethod(name, age);
           
           
        }
        time2 = System.currentTimeMillis();
        totalTime = (time2 - time1);
        
        return totalTime;
        
    }
    
    public long nativeTest(String name, int age, int iterationCount)
    {
    
       
        long totalTime = 0;
        long time1 = 0;
        long time2 = 0;
        
        time1 = System.currentTimeMillis();
        for (int i = 0; i < iterationCount; i++)
        {
            
            jniMethod(name, age);
           
           
        }
        time2 = System.currentTimeMillis();
        totalTime = (time2 - time1);
        
        return totalTime;
       
    }
    
    public void jMethod(String name, int age)
    {
        System.out.println("My name = " + name + " and my age = " + age + ".");
    }
    

    public native void jniMethod(String name, int age);
    

} //end class Test
