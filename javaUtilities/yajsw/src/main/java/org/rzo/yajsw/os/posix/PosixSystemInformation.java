package org.rzo.yajsw.os.posix;

import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.rzo.yajsw.os.SystemInformation;

public class PosixSystemInformation implements SystemInformation
{
	Utils	_utils	= new Utils();
	Logger	_logger;

	public void setLogger(Logger logger)
	{
		_logger = logger;
	}

	public long freeRAM()
	{
		String info = _utils.readFile("/proc/meminfo");
		if (info != null)
			try
			{
				String sp = ".*MemFree:\\s*(\\d+) kB.*";
				Pattern p = Pattern.compile(sp, Pattern.DOTALL);
				Matcher m = p.matcher(info);
				m.find();
				return Long.parseLong(m.group(1)) * 1024;
			}
			catch (Exception ex)
			{
				if (_logger != null)
					_logger.throwing(PosixSystemInformation.class.getName(), "freeRAM", ex);
			}
		return 0;
	}

	public long totalRAM()
	{
		String info = _utils.readFile("/proc/meminfo");
		if (info != null)
			try
			{
				String sp = ".*MemTotal:\\s*(\\d+) kB.*";
				Pattern p = Pattern.compile(sp, Pattern.DOTALL);
				Matcher m = p.matcher(info);
				m.find();
				return Long.parseLong(m.group(1)) * 1024;
			}
			catch (Exception ex)
			{
				if (_logger != null)
					_logger.throwing(PosixSystemInformation.class.getName(), "totalRAM", ex);
			}
		return 0;
	}

}
