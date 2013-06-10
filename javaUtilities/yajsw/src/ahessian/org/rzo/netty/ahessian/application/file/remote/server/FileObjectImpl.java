package org.rzo.netty.ahessian.application.file.remote.server;

import java.io.File;
import java.io.InputStream;

import org.rzo.netty.ahessian.application.file.remote.service.AsyncFileService;
import org.rzo.netty.ahessian.application.file.remote.service.FileObject;

public class FileObjectImpl implements FileObject
{
	long _created = -1;
	String _path = null;
	boolean _isDirectory = false;
	boolean _isFile = false;
	boolean _isHidden = false;
	long _lastModified = -1;
	long _length = -1;
	transient AsyncFileService _fileService = null;
	
	public FileObjectImpl(File file)
	{
		_path = file.getAbsolutePath();
		_isDirectory = file.isDirectory();
		_isFile = file.isFile();
		_isHidden = file.isHidden();
		_lastModified = file.lastModified();
		_length = file.length();
	}

	public FileObjectImpl()
	{
		
	}

	public long created()
	{
		return _created;
	}

	public InputStream getInputStream()
	{
		return _fileService.getInputStream(_path);
	}

	public String getPath()
	{
		return _path;
	}

	public boolean isDirectory()
	{
		return _isDirectory;
	}

	public boolean isFile()
	{
		return _isFile;
	}

	public boolean isHidden()
	{
		return _isHidden;
	}

	public long lastModified()
	{
		return _lastModified;
	}

	public long length()
	{
		return _length;
	}

}
