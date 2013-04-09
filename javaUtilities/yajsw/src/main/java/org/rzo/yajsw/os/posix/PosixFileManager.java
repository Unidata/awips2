package org.rzo.yajsw.os.posix;

import org.rzo.yajsw.os.FileManager;
import org.rzo.yajsw.util.File;

public class PosixFileManager implements FileManager
{
	private static FileManager	_instance;

	public static synchronized FileManager instance()
	{
		if (_instance == null)
			_instance = new PosixFileManager();
		return _instance;
	}

	public long created(File file)
	{
		// TODO Auto-generated method stub
		return -1;
	}

	public long freeSpace(File file)
	{
		// TODO Auto-generated method stub
		return -1;
	}

	public long totalSpace(File file)
	{
		// TODO Auto-generated method stub
		return -1;
	}

}
