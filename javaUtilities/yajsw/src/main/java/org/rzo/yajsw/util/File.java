package org.rzo.yajsw.util;

import java.util.Date;

import org.rzo.yajsw.os.OperatingSystem;

public class File extends java.io.File
{

	public File(String pathname)
	{
		super(pathname);
	}

	public long created()
	{
		return OperatingSystem.instance().fileManagerInstance().created(this);
	}

	public long getFreeSpace()
	{
		return OperatingSystem.instance().fileManagerInstance().freeSpace(this);
	}

	public long getTotalSpace()
	{
		return OperatingSystem.instance().fileManagerInstance().totalSpace(this);
	}

	static public void main(String[] args)
	{
		File f = new File("E:");
		System.out.println(new Date(f.created()));
		System.out.println(f.getFreeSpace());
		System.out.println(f.getTotalSpace());
	}

}
