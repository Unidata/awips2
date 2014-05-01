 package test.io


import org.testng.annotations.*
import org.testng.TestNG
import org.testng.TestListenerAdapter
import static org.testng.AssertJUnit.*;

import org.rzo.yajsw.io.*;

import java.util.concurrent.*;
import java.lang.Runnable;

import java.io.*;


/**
*
*/
public class TestCyclicBufferFile {

	/**
	* Main entry point to run <code>TestCyclicBufferFile</code> as
	* simple Groovy class
	*/
	public static void main(String[] args){
		def testng = new TestNG()
		testng.setTestClasses(TestCyclicBufferFile)
		testng.addListener(new TestListenerAdapter())
		testng.run()
	}
	
	
	void readx(CyclicBufferFileInputStream reader, int sizex)
	{
		InputStreamReader isr = new InputStreamReader(reader);
		BufferedReader br = new BufferedReader(isr);
		String line = ""
		int i=1;
		try
		{
		for (; (line = br.readLine()) != null && i<sizex; i++)
		{
			//println line
			assertEquals(line, ""+i)	
		}
		}
		catch (Exception ex)
		{
			println ex.getMessage()
		}
		assertEquals(i, sizex)
	}
	
	void writex(CyclicBufferFilePrintStream writer, int sizex)
	{
		(1..sizex).each
		{
			writer.println( ""+it)
			// give reader time to avoid buffer overwrite
			if (it % 10 == 0)
			Thread.sleep(10)
		}
	}
	
	
	/**
	*
	*/
	@Test
	final void testCyclicBuffer(){
		new File("test.dat").delete()
		CyclicBufferFileInputStream reader = new CyclicBufferFileInputStream(new File("test.dat"));
		CyclicBufferFilePrintStream writer = new CyclicBufferFilePrintStream(new File("test.dat"));
		writex(writer, 100)
		readx(reader, 100)
		writer.close()
		reader.close()
	}

	@Test
	final void testMultiThreadedCyclicBuffer(){
		new File("test.dat").delete()
		CyclicBufferFileInputStream reader = new CyclicBufferFileInputStream(new File("test.dat"));
		CyclicBufferFilePrintStream writer = new CyclicBufferFilePrintStream(new File("test.dat"));

		ExecutorService	pool	= Executors.newCachedThreadPool();
		int sizex = 52000
		//sizex = 100
		Future future = pool.submit({ readx(reader, sizex) } as Runnable);
						pool.submit({ writex(writer, sizex) } as Runnable);
		future.get(sizex*2+5000, TimeUnit.MILLISECONDS)
		pool.shutdown();
		writer.close()
		reader.close()
		

	}

}