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

public class TestShortRead implements TestModule
{
	private  String FILE_ROOT = null;


	String dbgInfo;
	boolean testResult = false;

	public TestShortRead()
	{

	}

	public boolean setUp( String workingDir, boolean cleanFilesAtStart) {
		FILE_ROOT = workingDir;
		return true;
	}

	public boolean cleanUp( boolean saveFiles )
        {
                try {
                H5.H5close();
                } catch (HDF5Exception ex) {
                        dbgInfo += "\nH5close(): FAILED "+ex;
			return false;
                }
		return true;
        }


	public String getTestName() {
		String desc = "Test Short/short short/byte write somewhere & read local";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test short and Short and short/byte data types, write somewhere and read local either way.";
		return desc;
	}

	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 4;
		int passed = 0;
		dbgInfo = "\n\n========== Test Write & read 2D Short/short flattened  ==========";
		if (testShortShortFlat()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write & read 2D Short/short Array  ==========";
		if (testShortShortArray()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write & read 2D Short/byte Flat  ==========";
		if (testShortByteFlat() ) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write & read 2D Short/byte Array  ==========";
		if (testShortByteArray() ) {
			passed++;
		}
		dbgInfo += "\n\n========== Short/short/byte tests complete: "+passed+" of "+ntests+" passed  ==========";
		testResult = (passed == ntests);
	}

	public boolean testPassed()
	{
		return testResult;
	}

	public String getVerboseResult()
	{
		return dbgInfo;
	}

	/**
	 * Tests
	 */
	private boolean testShortShortFlat()
	{
		String fileName = FILE_ROOT+"ShortShortFlat.h5";
		String dataset1Name = "shortArray";
		String dataset2Name = "ShortArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int errs = 0;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype2, dataspace2;   /* handles */
		int datatype1, dataspace1;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		Short[] data1 = new Short[nx*ny];			/* data to write */
		Short[] outData1 = new Short[nx*ny];
		Short[] outData3 = new Short[nx*ny];
		short[] data2 = new short[nx*ny];			/* data to write */
		short[] outData2 = new short[nx*ny];
		short[] outData4 = new short[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j*ny+i] = new Short((short)(j*nx+i));
				data2[j*ny+i] = (short)(j*nx+i);
			}
		}

 		tfile = -1;
 		try {
 		tfile = H5.H5Fopen(fileName,
                                 HDF5Constants.H5F_ACC_RDONLY,
                                 HDF5Constants.H5P_DEFAULT);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Fopen() second open Failed, exception: "+ex;
 			return false;
 		}
 
 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead Short data as Short";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData1[j*ny+i].equals(data1[j*ny+i])) {
					errs++;
				if (errs > 10 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i];
 		//			System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i]);System.out.flush();
				}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 after read 1 failed. "+ex;
			return false;
		}

 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 after read 1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead Short data as short";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData2[j*ny+i] != data1[j*ny+i].shortValue()) {
					errs++;
					if (errs > 20 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData2[j*ny+i];
 		//			//System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData2[j*ny+i]);System.out.flush();
					}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: tdataset1 read 2 failed. "+ex;
			return false;
		}

 		try {
 			tdataset2 = H5.H5Dopen(tfile, dataset2Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() Failed, dataset2 read 1 exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead short data as short";
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData4);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData4[j*ny+i] != data2[j*ny+i]) {
					errs++;
					if (errs > 30 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j*nx+i]+" read back as "+outData2[j*nx+i];
 		//			System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data2[j*nx+i]+" read back as "+outData4[j*nx+i]);System.out.flush();
					}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset 2 read 1 failed. "+ex;
			return false;
		}

 		try {
 			tdataset2 = H5.H5Dopen(tfile, dataset2Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Dopen() dataset2 read 2 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead short data as Short";
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData3);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: dataset 2 read 2 failed. "+ex; 
			return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData3[j*nx+i].shortValue() != data2[j*nx+i]) {
					errs++;
					if (errs > 40 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j*nx+i]+" read back as "+outData3[j*nx+i];
 		//			System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data2[j*nx+i]+" read back as "+outData3[j*nx+i]);System.out.flush();
					}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset 2 read 2 failed. "+ex;
			return false;
		}

 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
		if (errs > 0) {
			dbgInfo += "\nShortShortFlat(): at least "+errs+" errors";
			return false;
		} else {
			dbgInfo += "\nShortShortFlat(): OK ";
			return true;
		}
 	}

	private boolean testShortShortArray()
	{
		String fileName = FILE_ROOT+"ShortShortArray.h5";
		String dataset1Name = "shortArray";
		String dataset2Name = "ShortArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int errs = 0;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype2, dataspace2;   /* handles */
		int datatype1, dataspace1;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		Short[][] data1 = new Short[nx][ny];			/* data to write */
		Short[][] outData1 = new Short[nx][ny];
		Short[][] outData3 = new Short[nx][ny];
		short[][] data2 = new short[nx][ny];			/* data to write */
		short[][] outData2 = new short[nx][ny];
		short[][] outData4 = new short[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j][i] = new Short((short)(j*nx+i));
				data2[j][i] = (short)(j*nx+i);
			}
		}

 		tfile = -1;
 		try {
 		tfile = H5.H5Fopen(fileName,
                                 HDF5Constants.H5F_ACC_RDONLY,
                                 HDF5Constants.H5P_DEFAULT);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Fopen() second open Failed, exception: "+ex;
 			return false;
 		}
 
 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead Short data as Short";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData1[j][i].equals(data1[j][i])) {
					errs++;
					if (errs > 10 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
 					//  System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData1[j][i]);System.out.flush();
					}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 after read 1 failed. "+ex;
			return false;
		}

 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 after read 1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead Short data as short";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData2[j][i] != data1[j][i].shortValue()) {
					errs++;
					if (errs > 20 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData2[j][i];
 					//  System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData2[j][i]);System.out.flush();
					}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: tdataset1 read 2 failed. "+ex;
			return false;
		}

 		try {
 			tdataset2 = H5.H5Dopen(tfile, dataset2Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() Failed, dataset2 read 1 exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead short data as short";
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData4);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData4[j][i] != data2[j][i]) {
					errs++;
					if (errs > 30 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j][i]+" read back as "+outData2[j][i];
 					//  System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data2[j][i]+" read back as "+outData4[j][i]);System.out.flush();
					}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset 2 read 1 failed. "+ex;
			return false;
		}

 		try {
 			tdataset2 = H5.H5Dopen(tfile, dataset2Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Dopen() dataset2 read 2 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead short data as Short";
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData3);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: dataset 2 read 2 failed. "+ex; 
			return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData3[j][i].shortValue() != data2[j][i]) {
					errs++;
					if (errs > 40 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j][i]+" read back as "+outData3[j][i];
 					//  System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data2[j][i]+" read back as "+outData3[j][i]);System.out.flush();
					}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset 2 read 2 failed. "+ex;
			return false;
		}

 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
		if (errs > 0) {
			dbgInfo += "\nShortShortFlat(): at least "+errs+" errors";
			return false;
		} else {
			dbgInfo += "\nShortShortArray(): OK ";
			return true;
		}
 	}


	private boolean testShortByteFlat()
	{
		String fileName = FILE_ROOT+"shortByteFlat.h5";
		String dataset1Name = "byteArray";
		String dataset2Name = "shortArray";
		String dataset3Name = "ShortArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int errs = 0;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype1, dataspace1;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		byte[] data1 = new byte[nx*ny];			/* data to write */
		Short[] outData1 = new Short[nx*ny];
		short[] outData2 = new short[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j*ny+i] = (byte)(j+i);
			}
		}

 		tfile = -1;
 		try {
 		tfile = H5.H5Fopen(fileName,
                                 HDF5Constants.H5F_ACC_RDONLY,
                                 HDF5Constants.H5P_DEFAULT);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Fopen() second open Failed, exception: "+ex;
 			return false;
 		}
 
 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead byte data as Short";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData1[j*ny+i].byteValue() !=( data1[j*ny+i])) {
					errs++;
					if (errs > 10 ) {
 
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i];
 					//  System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i]);System.out.flush();
					}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 after read 1 failed. "+ex;
			return false;
		}

 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 after read 1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead byte data as short";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData2[j*ny+i] != data1[j*ny+i]) {
					errs++;
					if (errs > 20 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData2[j*ny+i];
 					//  System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData2[j*ny+i]);System.out.flush();
					}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: tdataset1 read 2 failed. "+ex;
			return false;
		}

 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
		if (errs > 0) {
			dbgInfo += "\nShortByteFlat(): at least "+errs+" errors";
			return false;
		} else {
			dbgInfo += "\nShortByteFlat(): OK ";
			return true;
		}
 	}

	private boolean testShortByteArray()
	{
		String fileName = FILE_ROOT+"ShortByteArray.h5";
		String dataset1Name = "byteArray";
		String dataset2Name = "shortArray";
		String dataset3Name = "ShortArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int errs = 0;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype1, dataspace1;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		byte[][] data1 = new byte[nx][ny];			/* data to write */
		Short[][] outData1 = new Short[nx][ny];
		short[][] outData2 = new short[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j][i] = (byte)(j+i);
			}
		}

 		tfile = -1;
 		try {
 		tfile = H5.H5Fopen(fileName,
                                 HDF5Constants.H5F_ACC_RDONLY,
                                 HDF5Constants.H5P_DEFAULT);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Fopen() second open Failed, exception: "+ex;
 			return false;
 		}
 
 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead byte data as Short";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData1[j][i].byteValue() !=( data1[j][i])) {
					errs++;
					if (errs > 10 ) {
 
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
 					//  System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData1[j][i]);System.out.flush();
					}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 after read 1 failed. "+ex;
			return false;
		}

 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 after read 1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead byte data as short";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData2[j][i] != data1[j][i]) {
					errs++;
					if (errs > 20 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData2[j][i];
 					//  System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData2[j][i]);System.out.flush();
					}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: tdataset1 read 2 failed. "+ex;
			return false;
		}

 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
		if (errs > 0) {
			dbgInfo += "\nShortByteArray(): at least "+errs+" errors";
			return false;
		} else {
			dbgInfo += "\nShortByteArray(): OK ";
			return true;
		}
 	}

}
