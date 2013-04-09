/**
 * 
 */
 package test.os


import org.testng.annotations.*
import org.testng.TestNG
import org.testng.TestListenerAdapter
import static org.testng.AssertJUnit.*;

 import org.apache.commons.configuration.*;
 import org.rzo.yajsw.os.*;


/**
*
*/
public class TestJavaHome {

	/**
	* Main entry point to run <code>TestJavaHome</code> as
	* simple Groovy class
	* 
	* Test requires jdk jre 1.5 + 1.6 installed.
	*/
	public static void main(String[] args){
		def testng = new TestNG()
		testng.setTestClasses(TestJavaHome)
		testng.addListener(new TestListenerAdapter())
		testng.run()
	}
	
	boolean expectedJVM(String cmd, String version)
	{
		return cmd.contains(version)
	}
	
	
	/**
	*
	*/
	@Test
	final void testJavaHomeMin(){
		BaseConfiguration conf = new BaseConfiguration();
		conf.setProperty("wrapper.java.command.minVersion", "1.6.0")
		JavaHome jvm = OperatingSystem.instance().getJavaHome(conf)
		assertTrue(expectedJVM("${jvm.findJava()}", "1.6"))
	}

	@Test
	final void testJavaHomeMax(){
		BaseConfiguration conf = new BaseConfiguration();
		conf.setProperty("wrapper.java.command.maxVersion", "1.5.0")
		JavaHome jvm = OperatingSystem.instance().getJavaHome(conf)
		assertTrue(expectedJVM("${jvm.findJava()}", "1.5"))
	}

	@Test
	final void testJavaHomeJre(){
		BaseConfiguration conf = new BaseConfiguration();
		conf.setProperty("wrapper.java.command.jreOnly", "true")
		JavaHome jvm = OperatingSystem.instance().getJavaHome(conf)
		assertTrue(expectedJVM("${jvm.findJava()}", "jre"))
	}

	@Test
	final void testJavaHomeJdk(){
		BaseConfiguration conf = new BaseConfiguration();
		conf.setProperty("wrapper.java.command.jdkOnly", "true")
		JavaHome jvm = OperatingSystem.instance().getJavaHome(conf)
		assertTrue(expectedJVM("${jvm.findJava()}", "jdk"))
	}

	@Test
	final void testJavaHomeJavaw(){
		BaseConfiguration conf = new BaseConfiguration();
		conf.setProperty("wrapper.java.command.javaw", "true")
		JavaHome jvm = OperatingSystem.instance().getJavaHome(conf)
		assertTrue(expectedJVM("${jvm.findJava()}", "javaw"))
	}

}