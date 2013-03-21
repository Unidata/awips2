/**
 * 
 */
 package test.wrapper


import org.testng.annotations.*
import org.testng.TestNG
import org.testng.TestListenerAdapter
import static org.testng.AssertJUnit.*;

/**
*
*/
public class TestWrappedJavaProcess {

	/**
	* Main entry point to run <code>TestWrappedJavaProcess</code> as
	* simple Groovy class
	*/
	public static void main(String[] args){
		def testng = new TestNG()
		testng.setTestClasses(TestWrappedJavaProcess)
		testng.addListener(new TestListenerAdapter())
		testng.run()
	}
	
	
	/**
	*
	*/
	@Test
	final void testSomething(){
		assertTrue(true)
	}

}