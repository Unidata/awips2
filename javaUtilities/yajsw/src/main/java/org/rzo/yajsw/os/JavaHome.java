package org.rzo.yajsw.os;

import org.jboss.netty.logging.InternalLogger;

public interface JavaHome
{
	String findJava(String wrapperJava, String customProcessName);
	void setLogger(InternalLogger logger);

}
