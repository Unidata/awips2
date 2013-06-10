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
import org.rzo.yajsw.nettyutils.*;
import org.rzo.yajsw.app.WrapperManagerImpl;

import java.util.concurrent.*;

import org.jboss.netty.bootstrap.*;
import org.jboss.netty.channel.*;
import org.jboss.netty.channel.socket.nio.*;


/**
*
*/
public class TestJVMController2 {

	/**
	* Main entry point to run <code>TestJVMController2</code> as
	* simple Groovy class
	*/
	
	def String key = ""+1234
	def Executor executor = Executors.newCachedThreadPool();
	
	def int sleep = 10000


	
	public static void main(String[] args){
		def testng = new TestNG()
		testng.setTestClasses(TestJVMController2)
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
	
	
	void simConnect()
	{
		def ClientBootstrap connector;
		 connector = new ClientBootstrap(
                new NioClientSocketChannelFactory(
                		executor,
                		executor));
		connector.setOption("remoteAddress", new InetSocketAddress("127.0.0.1", 15003));
		connector.setOption("connectTimeoutMillis", 10 * 1000);
		connector.getPipeline().addLast("log", new LoggingFilter(null));
		connector.getPipeline().addLast("messageEncoder", new MessageEncoder());
		connector.getPipeline().addLast("messageDecoder", new MessageDecoder());

		ChannelFuture future1 = connector.connect();
		future1.await();
		println "connected ? "+ future1.isSuccess()
		future1.getChannel().write(new Message(Constants.WRAPPER_MSG_KEY, key));		
	}
	
	

	
	
	/**
	*
	*/
	//@Test
	final void testController(){
		JVMController controller = createController();
		controller.init();
		(1..10).each
		{
		controller.start();
		Thread.sleep(sleep);
		simConnect();
		simConnect();
		simConnect();
		simConnect();
		Thread.sleep(sleep);
		Thread.sleep(sleep);
		Thread.sleep(sleep);
		Thread.sleep(sleep);
		controller.reset();
		Thread.sleep(sleep);
		}		
	}
	
	@Test
	final void testJVMMain()
	{
		JVMController controller = createController();
		controller.init();
		controller.start();
		
		def WrapperManagerImpl wm = new WrapperManagerImpl();
		wm.setPort(15003)
		wm.setKey(key)
		wm.setPingInterval(5000);
		wm.start()
		
		Thread.sleep(60000)
		
		//controller.reset();
		
	}

}