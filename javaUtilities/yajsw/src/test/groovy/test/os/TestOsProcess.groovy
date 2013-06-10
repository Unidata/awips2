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

import java.util.concurrent.*;
import java.lang.Runnable;


/**
*
*/
public class TestOsProcess {

	/**
	* Main entry point to run <code>TestOsProcess</code> as
	* simple Groovy class
	*/
	public static void main(String[] args){
		def testng = new TestNG()
		testng.setTestClasses(TestOsProcess)
		testng.addListener(new TestListenerAdapter())
		testng.run()
	}
	
	
	/**
	*
	*/
	final Process createPing(){
		String strCmd = "ping localhost" + (Platform.isWindows() ? " -t" : "");
		//String strCmd = "notepad"
		String[] cmd = strCmd.split();
		Process process = OperatingSystem.instance().processManagerInstance().createProcess();
		process.setCommand(cmd);
		process.setTitle('ping')
		return process;
	}
	
	@Test
	final void stopPing()
	{
		Process process = createPing();
		assertTrue(process.start());
		assertTrue(process.isRunning());
		assertTrue(!process.isTerminated());
		long time = System.currentTimeMillis();
		assertTrue(process.stop(5000, 9));
		long duration = System.currentTimeMillis() - time;
		assertTrue(process.isTerminated());
		assertTrue(!process.isRunning());
		assertTrue(duration < 7000)
		assertEquals(process.getExitCode(), 9);
	}

	@Test
	final void killPing()
	{
		Process process = createPing();
		assertTrue(process.start());
		assertTrue(process.isRunning());
		assertTrue(!process.isTerminated());
		long time = System.currentTimeMillis();
		assertTrue(process.kill(9));
		long duration = System.currentTimeMillis() - time;
		assertTrue(process.isTerminated());
		assertTrue(!process.isRunning());
		assertTrue(duration >= 0 && duration < 1000)
		assertEquals(process.getExitCode(), 9);
	}
	
	@Test
	final void multiKillPing()
	{
		Process process = createPing();
		(1..10).each
		{
		assertTrue(process.start());
		assertTrue(process.isRunning());
		assertTrue(!process.isTerminated());
		long time = System.currentTimeMillis();
		assertTrue(process.kill(9));
		long duration = System.currentTimeMillis() - time;
		assertTrue(process.isTerminated());
		assertTrue(!process.isRunning());
		assertTrue(duration >= 0 && duration < 5000)
		assertEquals(process.getExitCode(), 9);
		process.destroy();
		}		
	}
	
	@Test
	final void multiThreadMultiKillPing()
	{
		def futures = []
		ExecutorService	pool	= Executors.newCachedThreadPool();
		(1..50).each
		{
			futures.add( pool.execute( { multiKillPing() } as Runnable ))
		}
		pool.shutdown();
		assertTrue("start/stop test hangs", pool.awaitTermination(120, TimeUnit.SECONDS))

	}
	
	@Test
	final void performancePing()
	{
		Process process = createPing();
		assertTrue(process.start());
		assertTrue(process.getCurrentCpu() >= 0)
		assertTrue(process.getCurrentHandles() >= 1)
		assertTrue(process.getCurrentPageFaults() >= 0)
		assertTrue(process.getCurrentPhysicalMemory() >= 1)
		assertTrue(process.getCurrentThreads() >= 1)
		assertTrue(process.getCurrentVirtualMemory() >= 1)
		assertTrue(process.kill(9));
	}
	
	@Test
	final void priorityPing()
	{
		Process process = createPing();
		process.setPriority(Process.PRIORITY_BELOW_NORMAL);
		assertTrue(process.start());
		assertTrue(process.kill(9));		
	}

	@Test
	final void badUserPing()
	{
		Process process = createPing();
		process.setUser("no good user");
		assertTrue(!process.start());
		process.kill(9)
	}
	
	@Test
	final void currentUserPing()
	{
		//TODO
	}
	
	@Test
	final void pipeStreamsPing()
	{
		Process process = createPing();
		process.setPipeStreams(true, false)
		assertTrue(process.start());
		BufferedReader input = new BufferedReader(new InputStreamReader(process.getInputStream()))
		Thread.sleep(1000)
		String line = input.readLine()
		line = input.readLine()
		line = input.readLine()
		assertTrue("expecting ping in $line", line.toLowerCase().startsWith("ping"));
		assertTrue(process.kill(9));		
	}
	
	@Test
	final void getPingProcess()
	{
		Process process = createPing();
		assertTrue(process.start());
		int pid = process.getPid();
		Process ping = OperatingSystem.instance().processManagerInstance().getProcess(pid)
		assertEquals(ping.getCommand(), process.getCommand())
		assertTrue(ping.getUser() != null)
		assertTrue(ping.getWorkingDir() != null)
		assertEquals(ping.getTitle(), process.getTitle())
		assertTrue(ping.kill(9));		
		assertEquals(process.getExitCode(), 9);		
	}
	
	

	



}