package org.rzo.yajsw.tray.ahessian.server;

import java.util.logging.Logger;

import org.jboss.netty.logging.InternalLogger;
import org.jboss.netty.logging.InternalLoggerFactory;

public class AhessianLogging
{
	public static void setAhessianLogger(final Logger log)
	{
		InternalLoggerFactory.setDefaultFactory(new InternalLoggerFactory()
		{

			@Override
			public InternalLogger newInstance(String name)
			{
				return (InternalLogger) new JdkLogger(log, "ahessian-jmx" );
			}			
		});
	}

}
