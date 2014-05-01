/**
 * 
 */
 package test.io


import org.testng.annotations.*
import org.testng.TestNG
import org.testng.TestListenerAdapter
import static org.testng.AssertJUnit.*;

import org.rzo.yajsw.io.*;

/**
*
*/
public class TestTeeInputStream {

	/**
	* Main entry point to run <code>TeeInputStream</code> as
	* simple Groovy class
	*/
	public static void main(String[] args){
		def testng = new TestNG()
		testng.setTestClasses(TeeInputStream)
		testng.addListener(new TestListenerAdapter())
		testng.run()
	}
	
	InputStream createInput(String x)
	{
		File f = new File("in$x")
		//f.delete(); 
		f.createNewFile()
		f.write((1..100).join("\n"))
		
		return new FileInputStream("in$x")
	}
	
	
	/**
	*
	*/
	// TODO hangs @Test
	final void testTeeInputStream(){
		InputStream in1 = createInput("1")
		InputStream in2 = createInput("2")
		TeeInputStream inx = new TeeInputStream();
		inx.connect(in1);
		inx.connect(in2);
		while (true)
		{
			println inx.read()
		}
	}

}