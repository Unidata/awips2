/**
 * 
 */
 package test.config


import org.testng.annotations.*
import org.testng.TestNG
import org.testng.TestListenerAdapter
import static org.testng.AssertJUnit.*;
import org.apache.commons.configuration.*;
import org.rzo.yajsw.config.*;
import org.apache.commons.configuration.Configuration;

/**
*
*/
public class YajswConfigurationImplTest {

	/**
	* Main entry point to run <code>YajswConfigurationImplTest</code> as
	* simple Groovy class
	*/
	public static void main(String[] args){
		def testng = new TestNG()
		testng.setTestClasses(YajswConfigurationImplTest)
		testng.addListener(new TestListenerAdapter())
		testng.run()
	}
	
	void createConfigurationFile(String file, Map co)
	{
		MapConfiguration mc = new MapConfiguration(co);
		PropertiesConfiguration prop = new PropertiesConfiguration();
		ConfigurationUtils.copy(mc, prop);
		prop.setFileName(file);
		prop.save();
	}
	
	Configuration createLocalConfiguration(Map conf)
	{
		return new MapConfiguration(conf);
	}
	
	
	/**
	*
	*/
	@Test
	final void testWithSystemEnv(){
		YajswConfigurationImpl conf = new YajswConfigurationImpl(null, true);
		assertTrue(conf.getString("java.home") != null)
	}
	/**
	*
	*/
	@Test
	final void testNoSystemEnv(){
		YajswConfigurationImpl conf = new YajswConfigurationImpl(null, false);
		assertTrue(conf.getString("java.home") == null)
	}

	/**
	*
	*/
	@Test
	final void testOverrideOrder(){
		// only conf file
		createConfigurationFile('test.conf', ["test" : "3"])
		Configuration local = createLocalConfiguration(['wrapper.config': 'test.conf'])
		YajswConfigurationImpl conf = new YajswConfigurationImpl(local, false);
		assertTrue(conf.getInt("test") == 3)

		// local overrides file
		local = createLocalConfiguration(['wrapper.config': 'test.conf', 'test': "2"])
		conf = new YajswConfigurationImpl(local, false);
		assertTrue(conf.getInt("test") == 2)

		// local overrides system and file
		System.setProperty("test", "1")
		conf = new YajswConfigurationImpl(local, true);
		assertTrue(conf.getInt("test") == 2)
		
		// system overrides file
		local = createLocalConfiguration(['wrapper.config': 'test.conf'])
		conf = new YajswConfigurationImpl(local, true);
		assertTrue(conf.getInt("test") == 1)
}
	
	@Test
	final void testInterpolation(){
		// simple interpolation
		Configuration local = createLocalConfiguration(['test': '${java.home}/test'])
		YajswConfigurationImpl conf = new YajswConfigurationImpl(local, true);
		assertTrue(conf.getString("test").equals(conf.getString("java.home")+"/test"))
		
		// groovy interpolation
		local = createLocalConfiguration(['test': '${"${java.home}".toLowerCase()}'])
		conf = new YajswConfigurationImpl(local, true);
		assertTrue(conf.getString("test").equals(conf.getString("java.home").toLowerCase()))
		assertTrue(conf.lookupSet.contains("test") && conf.lookupSet.size() == 1)
	}
	
	@Test
	final void testGetString()
	{
		YajswConfigurationImpl conf = new YajswConfigurationImpl(null, true);
		String result = conf.getString("java.home");
		println result
		assertTrue(!result.contains("\\"));		
	}

	@Test
	final void testIsLocalFile()
	{
		YajswConfigurationImpl conf = new YajswConfigurationImpl(null, true);
		assertTrue(conf.isLocalFile());		

		createConfigurationFile('test.conf', ["test" : "3"])
		Configuration local = createLocalConfiguration(['wrapper.config': 'test.conf']);
		conf = new YajswConfigurationImpl(local, true);
		assertTrue(conf.isLocalFile());		

	    local = createLocalConfiguration(['wrapper.config': 'http://eprognos.dhis.org/webdav/wrapper.tomcat.conf']);
		conf = new YajswConfigurationImpl(local, true);
		assertFalse(conf.isLocalFile());		

	    local = createLocalConfiguration(['wrapper.config': 'http://java.sun.com/javase/technologies/desktop/javawebstart/apps/draw.jnlp']);
		conf = new YajswConfigurationImpl(local, true);
		assertFalse(conf.isLocalFile());		
	}
	
	@Test
	final void testGetCachedPath()
	{
		// no config file
		YajswConfigurationImpl conf = new YajswConfigurationImpl(null, true);
		String result = conf.getCachedPath(true);
		assertNull(result);		

 		// local config file
		createConfigurationFile('test.conf', ["test" : "3"])
		Configuration local = createLocalConfiguration(['wrapper.config': 'test.conf']);
		conf = new YajswConfigurationImpl(local, true);
		result = conf.getCachedPath(true);
		assertNotNull(result)
		assertEquals(result, new File('test.conf').canonicalPath);		

		// remote config file
	    local = createLocalConfiguration(['wrapper.config': 'http://eprognos.dhis.org/webdav/wrapper.tomcat.conf']);
		conf = new YajswConfigurationImpl(local, true);
		result = conf.getCachedPath(true);
		assertNotNull(result)
		assertEquals(new PropertiesConfiguration(result).getString("wrapper.java.app.mainclass"), 'org.apache.catalina.startup.Bootstrap');		

		// remote jnlp file
	    local = createLocalConfiguration(['wrapper.config': 'http://java.sun.com/javase/technologies/desktop/javawebstart/apps/draw.jnlp']);
		conf = new YajswConfigurationImpl(local, true);
		result = conf.getCachedPath(true);
		assertNotNull(result)
		assertEquals(new PropertiesConfiguration(result).getString("wrapper.java.app.mainclass"), 'Draw');		
		
	}


}