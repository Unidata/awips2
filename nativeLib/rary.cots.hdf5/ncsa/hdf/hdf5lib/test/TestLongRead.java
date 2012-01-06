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

public class TestLongRead implements TestModule
{
	private String FILE_ROOT = null;


	String dbgInfo;
	boolean testResult = false;

	public TestLongRead()
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
		String desc = "Test Long/long/int/short/byte write & read";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test long and Long data types, write and read all smaller types.";
		return desc;
	}

	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 8;
		int passed = 0;
		dbgInfo = "\n\n========== Test Write & read 2D Long/Long flattened  ==========";
		if(testLongLongFlat()) {
			passed++;
		}

		dbgInfo += "\n\n========== Test Write & read 2D Long/long Array  ==========";
		if(testLongLongArray()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write & read 2D Long/Int flattened  ==========";
		if(testLongIntFlat()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write & read 2D Long/Int Array  ==========";
		if(testLongIntArray()) {
			passed++;
		}

		dbgInfo += "\n\n========== Test Write & read 2D Long/byte Flat  ==========";
		if(testLongByteFlat()) {
			passed++;
		}

		dbgInfo += "\n\n========== Test Write & read 2D Long/byte Array  ==========";
		if(testLongByteArray()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write & read 2D Long/short Flat  ==========";
		if(testLongShortFlat()) {
			passed++;
		}

		dbgInfo += "\n\n========== Test Write & read 2D Long/short Array  ==========";
		if(testLongShortArray()) {
			passed++;
		}
		dbgInfo += "\n\n========== Long/long/int/short/byte tests complete: "+passed+" of "+ntests+" passed  ==========";
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
	 * Test 
	 */
	private boolean testLongLongFlat()
	{
		String fileName = FILE_ROOT+"LongLongFlat.h5";
		String dataset1Name = "longArray";
		String dataset2Name = "LongArray";
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
		Long[] data1 = new Long[nx*ny];			/* data to write */
		Long[] outData1 = new Long[nx*ny];
		Long[] outData3 = new Long[nx*ny];
		long[] data2 = new long[nx*ny];			/* data to write */
		long[] outData2 = new long[nx*ny];
		long[] outData4 = new long[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j*ny+i] = new Long(j*nx+i);
				data2[j*ny+i] = (long)(j*nx+i);
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
 
		dbgInfo += "\nRead Long data as Long";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 10); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 10); i++)  {
 				if (!outData1[j*ny+i].equals(data1[j*ny+i])) {
					errs++;
					if (errs > 10 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i];
					errs++;
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
 
		dbgInfo += "\nRead Long data as int";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex;
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 20); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 20); i++)  {
 				if (outData2[j*ny+i] != data1[j*ny+i].longValue()) {
				errs++;
				if (errs > 20 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData2[j*ny+i];
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
 
		dbgInfo += "\nRead long data as long";
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData4);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}
 		for (j = 0; (j < nx) && (errs < 30); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 30); i++)  {
 				if (outData4[j*ny+i] != data2[j*ny+i]) {
				errs++;
				if (errs > 30 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j*nx+i]+" read back as "+outData2[j*nx+i];
					errs++;
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
 
		dbgInfo += "\nRead long data as Long";
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData3);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: dataset 2 read 2 failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 40); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 40); i++)  {
 				if (outData3[j*nx+i].longValue() != data2[j*nx+i]) {
				errs++;
				if (errs > 40 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j*nx+i]+" read back as "+outData3[j*nx+i];
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

		if (errs == 0) {
			dbgInfo += "\nLongLongFlat(): OK ";
			return true;
		} else {
			dbgInfo += "\nLongLongFlat(): at least "+errs+" bad data errs ";
			return false;
		}
 	}

	private boolean testLongLongArray()
	{
		String fileName = FILE_ROOT+"LongLongArray.h5";
		String dataset1Name = "longArray";
		String dataset2Name = "LongArray";
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
		Long[][] data1 = new Long[nx][ny];			/* data to write */
		Long[][] outData1 = new Long[nx][ny];
		Long[][] outData3 = new Long[nx][ny];
		long[][] data2 = new long[nx][ny];			/* data to write */
		long[][] outData2 = new long[nx][ny];
		long[][] outData4 = new long[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j][i] = new Long(j*nx+i);
				data2[j][i] = (long)(j*nx+i);
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
 
		dbgInfo += "\nRead Long data as Long";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 10); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 10); i++)  {
 				if (!outData1[j][i].equals(data1[j][i])) {
				errs++;
				if (errs > 10 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
 				}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 after read 1 failed. "+ex;return false;}

 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 after read 1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead Long data as long";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

		
 		for (j = 0; (j < nx) && (errs < 20); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 20); i++)  {
 				if (outData2[j][i] != data1[j][i].longValue()) {
				errs++;
				if (errs > 20 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData2[j][i];
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
 
		dbgInfo += "\nRead long data as long";
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData4);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 30); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 30); i++)  {
 				if (outData4[j][i] != data2[j][i]) {
				errs++;
				if (errs > 30 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j][i]+" read back as "+outData2[j][i];
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
 
		dbgInfo += "\nRead long data as Long";
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData3);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: dataset 2 read 2 failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 30); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 30); i++)  {
 				if (outData3[j][i].longValue() != data2[j][i]) {
				errs++;
				if (errs > 30 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j][i]+" read back as "+outData3[j][i];
					errs++;
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
		if (errs == 0) {
			dbgInfo += "\nLongLongArray(): OK ";
			return true;
		} else {
			dbgInfo += "\nLongLongArray(): at least "+errs+" bad data errs ";
			return false;
		}
 	}

	private boolean testLongIntFlat()
	{
		String fileName = FILE_ROOT+"LongIntFlat.h5";
		String dataset1Name = "intArray";
		String dataset2Name = "longArray";
		String dataset3Name = "LongArray";
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
		int[] data1 = new int[nx*ny];			/* data to write */
		Long[] outData1 = new Long[nx*ny];
		long[] outData2 = new long[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j*ny+i] = (int)(j*nx+i);
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
 
		dbgInfo += "\nRead int data as Long";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 10); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 10); i++)  {
 				if (outData1[j*ny+i].intValue() !=( data1[j*ny+i])) {
				errs++;
				if (errs > 10 ) {
 
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i];
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
 
		dbgInfo += "\nRead int data as long";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 20); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 20); i++)  {
 				if (outData2[j*ny+i] != data1[j*ny+i]) {
				errs++;
				if (errs > 20 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData2[j*ny+i];
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
		if (errs == 0) {
			dbgInfo += "\nLongIntFlat(): OK ";
			return true;
		} else {
			dbgInfo += "\nLongIntFlat(): at least "+errs+" bad data errs ";
			return false;
		}
 	}

	private boolean testLongIntArray()
	{
		String fileName = FILE_ROOT+"LongIntArray.h5";
		String dataset1Name = "intArray";
		String dataset2Name = "longArray";
		String dataset3Name = "LongArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int errs = 0;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype2, dataspace2;   
		int datatype1, dataspace1;  
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		int[][] data1 = new int[nx][ny];			/* data to write */
		Long[][] outData1 = new Long[nx][ny];
		long[][] outData2 = new long[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j][i] = (int)(j*nx+i);
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
 
		dbgInfo += "\nRead int data as Long";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 10); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 10); i++)  {
 				if (outData1[j][i].intValue() !=( data1[j][i])) {
				errs++;
				if (errs > 10 ) {
 
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
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
 
		dbgInfo += "\nRead int data as long";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 10); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 10); i++)  {
 				if (outData2[j][i] != data1[j][i]) {
				errs++;
				if (errs > 10 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData2[j][i];
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
		if (errs == 0) {
			dbgInfo += "\nLongIntArray(): OK ";
			return true;
		} else {
			dbgInfo += "\nLongIntArray(): at least "+errs+" bad data errs ";
			return false;
		}
 	}

	private boolean testLongByteFlat()
	{
		String fileName = FILE_ROOT+"longByteFlat.h5";
		String dataset1Name = "byteArray";
		String dataset2Name = "longArray";
		String dataset3Name = "LongArray";
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
		byte[] data1 = new byte[nx*ny];			/* data to write */
		Long[] outData1 = new Long[nx*ny];
		long[] outData2 = new long[nx*ny];

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
 
		dbgInfo += "\nRead byte data as Long";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex;
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 10); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 10); i++)  {
 				if (outData1[j*ny+i].byteValue() !=( data1[j*ny+i])) {
				errs++;
				if (errs > 10 ) {
 
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i];
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
 
		dbgInfo += "\nRead byte data as long";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 20); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 20); i++)  {
 				if (outData2[j*ny+i] != data1[j*ny+i]) {
				errs++;
				if (errs > 20 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData2[j*ny+i];
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
		if (errs == 0) {
			dbgInfo += "\nLongByteFlat(): OK ";
			return true;
		} else {
			dbgInfo += "\nLongByteFlat(): at least "+errs+" bad data errs ";
			return false;
		}
 	}

	private boolean testLongByteArray()
	{
		String fileName = FILE_ROOT+"longByteArray.h5";
		String dataset1Name = "byteArray";
		String dataset2Name = "longArray";
		String dataset3Name = "LongArray";
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
		byte[][] data1 = new byte[nx][ny];			/* data to write */
		Long[][] outData1 = new Long[nx][ny];
		long[][] outData2 = new long[nx][ny];

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
 
		dbgInfo += "\nRead byte data as Long";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 10); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 10); i++)  {
 				if (outData1[j][i].byteValue() !=( data1[j][i])) {
				errs++;
				if (errs > 10 ) {
 
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
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
 
		dbgInfo += "\nRead byte data as long";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 20); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 20); i++)  {
 				if (outData2[j][i] != data1[j][i]) {
				errs++;
				if (errs > 20 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData2[j][i];
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
		if (errs == 0) {
			dbgInfo += "\nLongByteArray(): OK ";
			return true;
		} else {
			dbgInfo += "\nLongByteArray(): at least "+errs+" bad data errs ";
			return false;
		}
 	}


	private boolean testLongShortFlat()
	{
		String fileName = FILE_ROOT+"longShortFlat.h5";
		String dataset1Name = "shortArray";
		String dataset2Name = "longArray";
		String dataset3Name = "LongArray";
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
		short[] data1 = new short[nx*ny];			/* data to write */
		Long[] outData1 = new Long[nx*ny];
		long[] outData2 = new long[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j*ny+i] = (short)(j*ny+i);
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
 
		dbgInfo += "\nRead short data as Long";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 10); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 10); i++)  {
 				if (outData1[j*ny+i].shortValue() !=( data1[j*ny+i])) {
				errs++;
				if (errs > 10 ) {
 
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i];
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
 
		dbgInfo += "\nRead short data as long";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 20); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 20); i++)  {
 				if (outData2[j*ny+i] != data1[j*ny+i]) {
				errs++;
				if (errs > 20 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData2[j*ny+i];
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
		if (errs == 0) {
			dbgInfo += "\nLongShortFlat(): OK ";
			return true;
		} else {
			dbgInfo += "\nLongShortFlat(): at least "+errs+" bad data errs ";
			return false;
		}
 	}


	private boolean testLongShortArray()
	{
		String fileName = FILE_ROOT+"longShortArray.h5";
		String dataset1Name = "shortArray";
		String dataset2Name = "longArray";
		String dataset3Name = "LongArray";
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
		short[][] data1 = new short[nx][ny];			/* data to write */
		Long[][] outData1 = new Long[nx][ny];
		long[][] outData2 = new long[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j][i] = (short)(j*nx+i);
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
 
		dbgInfo += "\nRead short data as Long";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 10); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 10); i++)  {
 				if (outData1[j][i].shortValue() !=( data1[j][i])) {
				errs++;
				if (errs > 10 ) {
 
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
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
 
		dbgInfo += "\nRead short data as long";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; (j < nx) && (errs < 20); j++)
 		{
 			for (i = 0; (i < ny) && (errs < 20); i++)  {
 				if (outData2[j][i] != data1[j][i]) {
				errs++;
				if (errs > 20 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData2[j][i];
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
			dbgInfo += "\nLongShortArray(): at least "+errs+" errors";
			return false;
		} else {
			dbgInfo += "\nLongShortArray(): OK ";
			return true;
		}
 	}
  }
