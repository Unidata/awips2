package org.rzo.netty.ahessian.application.file.remote.service;

import java.io.InputStream;

public interface FileObject
{
	public String     getPath();
	public boolean    isDirectory();
	public boolean    isFile();
	public boolean 	isHidden();
	public long 		lastModified();
	public long 		created();
	public long 		length();
	public InputStream getInputStream();
}
