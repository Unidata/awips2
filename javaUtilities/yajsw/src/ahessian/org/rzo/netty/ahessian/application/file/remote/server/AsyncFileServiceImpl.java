package org.rzo.netty.ahessian.application.file.remote.server;

import java.io.File;
import java.io.InputStream;
import java.util.List;

import org.rzo.netty.ahessian.application.file.remote.service.AsyncFileService;
import org.rzo.netty.ahessian.application.file.remote.service.FileObject;

public class AsyncFileServiceImpl implements AsyncFileService
{

	public FileObject getFile(String file)
	{
		File f = new File(file);
		if (!f.exists())
			return null;
		return toFileObject(f);
	}

	private FileObject toFileObject(File f)
	{
		return new FileObjectImpl(f);
	}

	public InputStream getInputStream(String file)
	{
		return null;
	}

	public List<FileObject> list(String filter)
	{
		return null;
	}

}
