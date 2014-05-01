package org.rzo.yajsw.os.ms.win.w32;

import org.rzo.yajsw.os.FileManager;
import org.rzo.yajsw.util.File;

public class WindowsXPFileManager implements FileManager
{

	/** The _instance. */
	static FileManager	_instance;

	/**
	 * Instance.
	 * 
	 * @return the process manager
	 */
	public static FileManager instance()
	{
		if (_instance == null)
			_instance = new WindowsXPFileManager();
		return _instance;
	}

	public long created(File file)
	{
		return FileUtils.created(file);
	}

	public long freeSpace(File file)
	{
		return FileUtils.freeSpace(file);
	}

	public long totalSpace(File file)
	{
		return FileUtils.totalSpace(file);
	}

}
