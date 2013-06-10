package org.rzo.yajsw.os;

import org.rzo.yajsw.util.File;

public interface FileManager
{

	long created(File file);

	long freeSpace(File file);

	long totalSpace(File file);

}
