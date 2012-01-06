/****************************************************************************
 * NCSA HDF                                                                 *
 * National Comptational Science Alliance                                   *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * java-hdf5/COPYING file.                                                  *
 *                                                                          *
 ****************************************************************************/

package ncsa.hdf.hdf5lib.test;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class TestHDF5LibraryFrame  extends Frame
{
	public TextArea msgBox;

	public TestHDF5LibraryFrame()
	{
		super("Test Java HDF5 Interface");

		WindowListener adapter = new WindowAdapter()
		{
			public void windowClosing(WindowEvent e)
			{
				System.exit(0);
			}
		};
		addWindowListener(adapter);

		msgBox = new TextArea();
		msgBox.setEditable(false);
		add("Center", msgBox);
		setSize(600, 500);
		show();
	}

}
