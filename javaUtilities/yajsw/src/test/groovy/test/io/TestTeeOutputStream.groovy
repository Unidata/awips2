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
public class TestTeeOutputStream {

	/**
	* Main entry point to run <code>TestTeeOutputStream</code> as
	* simple Groovy class
	*/
	public static void main(String[] args){
		def testng = new TestNG()
		testng.setTestClasses(TestTeeOutputStream)
		testng.addListener(new TestListenerAdapter())
		testng.run()
	}
	
	
	/**
	*
	*/
	@Test
	final void testTeeOutputStream(){
		new File("out1.txt").delete()
		FileOutputStream out1 = new FileOutputStream("out1.txt");
		new File("out2.txt").delete()
		FileOutputStream out2 = new FileOutputStream("out2.txt");
		TeeOutputStream tee = new TeeOutputStream();
		tee.connect(out1);
		tee.connect(out2);
		
		(1..100).each
		{
			tee.write("$it\n" as byte[])
		}
		tee.close();
		out1.close();
		out2.close();
		File f1 = new File("out1.txt")
		int count = 1
		f1.eachLine
		{
			assertEquals(it, ""+count++)
		}
		File f2 = new File("out2.txt")
		count = 1
		f2.eachLine
		{
			assertEquals(it, ""+count++)
		}
		
		
		
		
	}

}