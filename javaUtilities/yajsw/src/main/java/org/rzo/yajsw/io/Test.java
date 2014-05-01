/* This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * <p/>
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.  
 */
package org.rzo.yajsw.io;

import java.io.File;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

// TODO: Auto-generated Javadoc
/**
 * The Class Test.
 */
public class Test
{

	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 */
	public static void main(String[] args)
	{
		try
		{
			String name = "test" + System.currentTimeMillis();

			RandomAccessFile in = new RandomAccessFile(new File(name), "rw");
			ByteBuffer buf = in.getChannel().map(FileChannel.MapMode.READ_WRITE, 0, 1024 * 40);
			in.close();
			buf = null;
			// in = null;
			System.gc();
			Thread.yield();
			// Thread.sleep(5000);
			// System.out.println( in.getChannel().isOpen());
			System.out.println(new File(name).delete());
		}
		catch (Exception e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

}
