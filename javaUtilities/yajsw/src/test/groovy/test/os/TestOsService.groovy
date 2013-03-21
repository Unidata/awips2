/**
 * 
 */
 package test.os


import org.testng.annotations.*
import org.testng.TestNG
import org.testng.TestListenerAdapter
import static org.testng.AssertJUnit.*;

 import org.rzo.yajsw.os.*;
 import com.sun.jna.Platform;
 
 import org.apache.commons.configuration.*;



/**
*
*/
public class TestOsService {

	/**
	* Main entry point to run <code>TestOsService</code> as
	* simple Groovy class
	*/
	public static void main(String[] args){
		def testng = new TestNG()
		testng.setTestClasses(TestOsService)
		testng.addListener(new TestListenerAdapter())
		testng.run()
	}
	
	Service createPing()
	{
		Service service = OperatingSystem.instance().serviceManagerInstance().createService()
		String strCmd = "ping localhost" + (Platform.isWindows() ? " -t" : "");
		String[] cmd = strCmd.split();
		service.setCommand(cmd)
		service.setName("ping")
		service.setDescription("ping")
		service.setDisplayName("ping")
		service.setConfig(new BaseConfiguration())
		return service
	}
	
	
	
	
	/**
	*
	*/
	@Test
	final void installUninstall(){
		Service service = createPing()
		service.init()
		// make sure it is not already installed
		service.uninstall()
		assertTrue(service.install())
		assertTrue(service.isInstalled(service.state()))
		assertTrue(service.uninstall())
		assertTrue(!service.isInstalled(service.state()))
	}
	
	@Test
	final void multiInstallUninstall()
	{
		Service service = createPing()
		service.init()
		service.uninstall()
		(1..10).each
		{
			assertTrue(service.install())
			assertTrue(service.isInstalled(service.state()))
			assertTrue(service.uninstall())
			assertTrue(!service.isInstalled(service.state()))
		}
	}
	
	//@Test
	final void startStop(){
		Service service = createPing()
		service.init()
		// make sure it is not already installed
		service.uninstall()
		assertTrue(service.install())
		assertTrue(service.isInstalled(service.state()))
		
		assertTrue(service.start())
		assertTrue(service.isRunning(service.state()))
		
		assertTrue(service.stop())
		assertTrue(!service.isRunning(service.state()))
		
		assertTrue(service.uninstall())
		assertTrue(!service.isInstalled(service.state()))
	}

	//@Test
	final void multiStartStop(){
		Service service = createPing()
		service.init()
		// make sure it is not already installed
		service.uninstall()
		assertTrue(service.install())
		assertTrue(service.isInstalled(service.state()))
		
		(1..10).each
		{
		assertTrue(service.start())
		assertTrue(service.isRunning(service.state()))
		
		assertTrue(service.stop())
		assertTrue(!service.isRunning(service.state()))
		}
		
		assertTrue(service.uninstall())
		assertTrue(!service.isInstalled(service.state()))
	}


}