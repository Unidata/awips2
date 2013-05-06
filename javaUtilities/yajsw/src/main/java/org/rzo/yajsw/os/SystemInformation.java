package org.rzo.yajsw.os;

import java.util.logging.Logger;

public interface SystemInformation
{
	public long totalRAM();

	public long freeRAM();

	public void setLogger(Logger logger);

}
