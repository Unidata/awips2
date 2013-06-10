package org.rzo.yajsw.util;

import java.util.List;
import java.util.ListIterator;

import org.apache.commons.cli2.validation.FileValidator;
import org.apache.commons.cli2.validation.InvalidArgumentException;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileType;

public class VFSFileValidator extends FileValidator
{

	String	_base	= null;

	public VFSFileValidator setBase(String base)
	{
		_base = base;
		return this;
	}

	public static VFSFileValidator getExistingInstance()
	{
		VFSFileValidator validator = null;
		try
		{
			validator = new VFSFileValidator();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		validator.setExisting(true);
		return validator;
	}

	public static VFSFileValidator getExistingFileInstance()
	{
		VFSFileValidator validator = null;
		try
		{
			validator = new VFSFileValidator();
		}
		catch (Exception e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		validator.setExisting(true);
		validator.setFile(true);
		return validator;
	}

	public static VFSFileValidator getExistingDirectoryInstance()
	{
		VFSFileValidator validator = null;
		try
		{
			validator = new VFSFileValidator();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		validator.setExisting(true);
		validator.setDirectory(true);
		return validator;
	}

	public void validate(final List values) throws InvalidArgumentException
	{
		for (final ListIterator i = values.listIterator(); i.hasNext();)
		{
			final String name = (String) i.next();
			try
			{
				FileObject f = VFSUtils.resolveFile(_base, name);

				if ((isExisting() && !f.exists()) || (isFile() && !f.getType().equals(FileType.FILE))
						|| (isDirectory() && !f.getType().equals(FileType.FILE)) || (isHidden() && !f.isHidden())
						|| (isReadable() && !f.isReadable()) || (isWritable() && !f.isWriteable()))
				{

					throw new InvalidArgumentException(name);
				}

				i.set(name);
			}
			catch (Exception ex)
			{
				ex.printStackTrace();
				throw new InvalidArgumentException(name);
			}
		}
	}

}
