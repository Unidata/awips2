/**
 * 
 */
 package test.tools


import org.testng.annotations.*
import org.testng.TestNG
import org.testng.TestListenerAdapter
import static org.testng.AssertJUnit.*;

import org.rzo.yajsw.tools.*;

import org.rzo.yajsw.os.*;
import org.apache.commons.configuration.*;


/**
*
*/
public class TestGenConfig {

	/**
	* Main entry point to run <code>TestGenConfig</code> as
	* simple Groovy class
	*/
	public static void main(String[] args){
		def testng = new TestNG()
		testng.setTestClasses(TestGenConfig)
		testng.addListener(new TestListenerAdapter())
		testng.run()
	}
	
	
	/**
	*
	*/
	@Test
	final void testSplitArgs()
	{
		assertEquals(ConfigGenerator.splitArgs(''), []);
		assertEquals(ConfigGenerator.splitArgs('1'), ['1']);
		assertEquals(ConfigGenerator.splitArgs('1 2'), ['1', '2']);
		assertEquals(ConfigGenerator.splitArgs('-Xmx562m -Xms521m'), ['-Xmx562m', '-Xms521m']);
		assertEquals(ConfigGenerator.splitArgs('-Xmx562m -Xms521m'), ['-Xmx562m', '-Xms521m']);
		assertEquals(ConfigGenerator.splitArgs('-Xmx562m -Xms521m'), ['-Xmx562m', '-Xms521m']);
		assertEquals(ConfigGenerator.splitArgs('"1 "'), ['1']);
		assertEquals(ConfigGenerator.splitArgs('"1 " "2 "'), ['1', '2']);
		assertEquals(ConfigGenerator.splitArgs('" 1 " "2 "'), ['1', '2']);
		assertEquals(ConfigGenerator.splitArgs('"1 2" "3 4"'), ['1 2', '3 4']);
	}
	
	@Test
	final void testGenConfig()
	{
		Process process = OperatingSystem.instance().processManagerInstance().createProcess();
		process.setTitle('test')
		process.setCommand("java -Xmx25m -Xms25m -Dx=y \"-Dx=z z\" -cp wrapper.jar${File.pathSeparatorChar}test.jar test.HelloWorld c:/x.txt \"d d\"")
		process.start()
		Thread.sleep(1000)
		ConfigGenerator.generate(process.getPid(), null, new File("test.conf"));
		process.kill(0)
		org.apache.commons.configuration.Configuration conf = new PropertiesConfiguration("test.conf")
		new File("test.conf").delete()
		println conf.getString("wrapper.java.command")
		assertEquals(conf.getString("wrapper.java.command"), "java")
		assertEquals(conf.getString("wrapper.java.classpath.1"), "wrapper.jar")
		assertEquals(conf.getString("wrapper.java.classpath.2"), "test.jar")
		assertEquals(conf.getString("wrapper.java.app.mainclass"), "test.HelloWorld")
		assertEquals(conf.getString("wrapper.app.parameter.1"), "c:/x.txt")
		assertEquals(conf.getString("wrapper.app.parameter.2"), "d d")
		assertEquals(conf.getString("wrapper.working.dir"), new File(".").canonicalPath+File.separator)
		assertEquals(conf.getString("wrapper.java.additional.1"), '-Xmx25m')
		assertEquals(conf.getString("wrapper.java.additional.2"), '-Xms25m')
		assertEquals(conf.getString("wrapper.java.additional.3"), '-Dx=y')
		assertEquals(conf.getString("wrapper.java.additional.4"), '-Dx=z z')
	}

}