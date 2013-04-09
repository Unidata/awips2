/**
 * 
 */
 package test.wrapper


import org.testng.annotations.*
import org.testng.TestNG
import org.testng.TestListenerAdapter
import static org.testng.AssertJUnit.*;

 import java.util.concurrent.*;
 import java.lang.Runnable;
 
 import org.rzo.yajsw.groovy.*;
 import org.rzo.yajsw.wrapper.*;
 
 import com.sun.jna.Platform;


/**
*
*/
public class TestMultiple {

	/**
	* Main entry point to run <code>TestMultiple</code> as
	* simple Groovy class
	*/
	public static void main(String[] args){
		def testng = new TestNG()
		testng.setTestClasses(TestMultiple)
		testng.addListener(new TestListenerAdapter())
		testng.run()
	}
	
	Object getJavaProcess()
	{
		def builder = new WrapperBuilder();
		builder.'wrapper.java.app.mainclass'='test.HelloWorld';
		builder.'wrapper.control'='LOOSE'
		return builder.process();		
	}
	
	Object getNativeProcess()
	{
		WrapperBuilder builder = new WrapperBuilder();
		builder.'wrapper.image' = "ping localhost" + (Platform.isWindows() ? " -t" : "");
		builder.'wrapper.control'='LOOSE'
		builder.'wrapper.jvm_exit.timeout'='0'
		builder.'wrapper.shutdown.timeout'='1'
		return builder.process();				
	}
	
	
	/**
	*
	*/
	@Test
	final void testMultiple(){
		List processes = []
		ExecutorService	pool	= Executors.newCachedThreadPool();
		(1..1).each {processes.add(getJavaProcess())}
		(1..1).each {processes.add(getNativeProcess())}
		(1..100).each
		{
			processes.each
			{ final process = it
				pool.execute(
						{
							process.start()
							assertTrue(process.isOSProcessRunning())
						} as Runnable)
			}
			Thread.sleep(5000)
			processes.each
			{ final process = it
				pool.execute(
						{
							process.stop()
							assertTrue(!process.isOSProcessRunning())
						} as Runnable)
			}
			Thread.sleep(2000)
		}
		pool.shutdown();

	}


}