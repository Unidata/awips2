/**
 * 
 */
 package test.controller


import org.testng.annotations.*
import org.testng.TestNG
import org.testng.TestListenerAdapter
import static org.testng.AssertJUnit.*;

 import org.rzo.yajsw.controller.*;
 import org.rzo.yajsw.controller.jvm.*;
import org.rzo.yajsw.wrapper.*;
import org.rzo.yajsw.Constants;


import java.util.concurrent.*;
import java.lang.Runnable;
import java.util.Random;

import org.jmock.*;
import org.jmock.internal.*;


/**
*
*/
public class JVMControllerTest {
	
	Random random = new Random(System.currentTimeMillis())
	//JUnit4GroovyMockery mockery = new JUnit4GroovyMockery();
	Mockery mockery = new Mockery();
	def String key = ""+0
	def message = new Message((byte)Constants.WRAPPER_MSG_KEY, key);

	@BeforeClass
	void initTest()
	{
	mockery.addExpectation(new InvocationExpectation()) // allow all invocations
	}
	

	/**
	* Main entry point to run <code>JVMControllerTest</code> as
	* simple Groovy class
	*/
	public static void main(String[] args){
		def testng = new TestNG()
		testng.setTestClasses(JVMControllerTest)
		testng.addListener(new TestListenerAdapter())
		testng.run()
	}
	
	JVMController createController()
	{
		WrappedJavaProcess process = new WrappedJavaProcess();
		JVMController controller = new JVMController(process);
		controller.setStartupTimeout(100000)
		controller.setPingTimeout(100000)
		controller.setKey(key)

		return controller;
	}
	
	void mockProcessLogon(JVMController controller)
	{
		controller._acceptor.getHandler().messageReceived(ioSession, message);
	}
	
	
	/**
	*
	*/
	final void testRepeatedStartStop()
	{
		JVMController controller = createController();
		controller.init()
		(1..100).each
		{
			assertTrue("could not start controller", controller.start());
			Thread.sleep((long)(random.nextInt(500) /10)*10) // sleep on windows < 10 has issues on windows
			assertEquals("wrong state after start", controller.stateAsStr(JVMController.STATE_WAITING), controller.stateAsStr(controller.getState()));
			mockProcessLogon(controller);
			controller.processStarted();
			Thread.sleep((long)(random.nextInt(500) /10)*10+100)
			assertEquals("wrong state after processStarted", controller.stateAsStr(JVMController.STATE_PROCESS_KILLED), controller.stateAsStr(controller.getState()));
			controller.stop(JVMController.STATE_USER_STOP);
			//Thread.sleep((long)(random.nextInt(500) /10)*10)
			assertEquals("wrong state after stop", controller.stateAsStr(JVMController.STATE_USER_STOP), controller.stateAsStr(controller.getState()));
			controller.reset();
			Thread.sleep((long)(random.nextInt(500) /10)*10)
			assertEquals("wrong state after reset", controller.stateAsStr(JVMController.STATE_UNKNOWN), controller.stateAsStr(controller.getState()));
		}
	}
	
	@Test
	final void testMultithreadRepeatedStartStop()
	{
		def futures = []
		ExecutorService	pool	= Executors.newCachedThreadPool();
		(1..10).each
		{
			futures.add( pool.execute( { testRepeatedStartStop() } as Runnable ))
		}
		pool.shutdown();
		assertTrue("start/stop test hangs", pool.awaitTermination(120, TimeUnit.SECONDS))
	}
	
	@Test
	final void testPortRange()
	{
		JVMController controller = createController();
		controller.setMaxPort(15003)
		controller.setMinPort(15003)
		controller.init();

		assertTrue("could not start controller", controller.start());
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_WAITING), controller.stateAsStr(controller.getState()));
		controller.stop(JVMController.STATE_USER_STOP);
		Thread.sleep((long)(random.nextInt(500) /10)*10)
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_USER_STOP), controller.stateAsStr(controller.getState()));
		controller.reset();
		Thread.sleep((long)(random.nextInt(500) /10)*10)
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_UNKNOWN), controller.stateAsStr(controller.getState()));

		ServerSocket socket = new ServerSocket()
		socket.bind(new InetSocketAddress(15003))
		assertFalse("started controller although port is not available", controller.start());
		assertNotSame("state after erronous start should not be WAITING", controller.getState(), JVMController.STATE_WAITING);

		
		controller.setMaxPort(15004)
		assertTrue("could not start controller", controller.start());
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_WAITING), controller.stateAsStr(controller.getState()));
		controller.stop(JVMController.STATE_USER_STOP);
		Thread.sleep((long)(random.nextInt(500) /10)*10)
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_USER_STOP), controller.stateAsStr(controller.getState()));
		controller.reset();
		Thread.sleep((long)(random.nextInt(500) /10)*10)
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_UNKNOWN), controller.stateAsStr(controller.getState()));
		
		socket.close()
	}
	
	@Test
	final void testStartupTimeout()
	{
		JVMController controller = createController();
		controller.setKey(key)
		controller.setStartupTimeout(1000)
		controller.setPingTimeout(100000)
		controller.init();

		
		// test timeout event
		assertTrue("could not start controller", controller.start());
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_WAITING), controller.stateAsStr(controller.getState()));
		Thread.sleep((long)(2000))
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_STARTUP_TIMEOUT), controller.stateAsStr(controller.getState()));
		controller.stop(JVMController.STATE_USER_STOP);
		controller.reset();
		Thread.sleep((long)(random.nextInt(500) /10)*10)
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_UNKNOWN), controller.stateAsStr(controller.getState()));

		// test stop before timeout
		assertTrue("could not start controller", controller.start());
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_WAITING), controller.stateAsStr(controller.getState()));
		Thread.sleep((long)(500))
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_WAITING), controller.stateAsStr(controller.getState()));
		controller.stop(JVMController.STATE_USER_STOP);
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_USER_STOP), controller.stateAsStr(controller.getState()));
		controller.reset();
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_UNKNOWN), controller.stateAsStr(controller.getState()));

		// test process started before timeout
		assertTrue("could not start controller", controller.start());
		Thread.sleep((long)500) 
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_WAITING), controller.stateAsStr(controller.getState()));
		mockProcessLogon(controller);
		// simulate process log on
		Thread.sleep((long)500) 
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_LOGGED_ON), controller.stateAsStr(controller.getState()));
		controller.stop(JVMController.STATE_USER_STOP);
		Thread.sleep((long)500) 
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_USER_STOP), controller.stateAsStr(controller.getState()));
		controller.reset();
		Thread.sleep((long)500) 
		assertEquals("wrong state", controller.stateAsStr(JVMController.STATE_UNKNOWN), controller.stateAsStr(controller.getState()));
	}

}