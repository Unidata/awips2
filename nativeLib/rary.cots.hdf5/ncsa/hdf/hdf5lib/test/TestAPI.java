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

public class TestAPI implements TestModule
{
	private String FILE_ROOT = null;


	String dbgInfo;
	boolean testResult = false;

	public TestAPI()
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
		String desc = "Test HDF API link, calls";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Basic Test HDF API link and call.";
		return desc;
	}

	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 11;
		int passed = 0;
		dbgInfo = "\n\n========== Test HDF5 API linking ==========";
		if ( h5() ) {
			passed++;
		}
		if (h5d()) {
			passed++;
		}
		if (h5a()) {
			passed++;
		}
		if ( h5f() ) {
			passed++;
		}
		if (h5g()) {
			passed++;
		}
		if (h5i()) {
			passed++;
		}
		if ( h5p() ) {
			passed++;
		}
		if (h5r()) {
			passed++;
		}
		if (h5s()) {
			passed++;
		}
		if ( h5t() ) {
			passed++;
		}
		if (h5e()) {
			passed++;
		}
		dbgInfo += "\n\n========== General API link tests complete: "+passed+" of "+ntests+" passed  ==========";
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
	 * Test cases
	 */

	/**
	 * Test H5: General Library Functions
	 */
	private boolean h5()
	{
		int status = -1;
		int[] libversion = new int[3];

		dbgInfo += "\n\n========== Test H5: General Library Functions ==========";
		try {
			status = H5.H5open();
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5open() Failed, exception: "+ex;
			return false;
		}
		try {
			status = H5.H5get_libversion(libversion);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5get_libversion() Failed, exception: "+ex;
			return false;
		}
		H5.H5check_version(libversion[0], libversion[1], libversion[2]);
		dbgInfo += "\n\tlibversion: "+libversion[0] +" "+ libversion[1] +" "+ libversion[2];
		dbgInfo += "\nH5check_version("+libversion[0]+", "+libversion[1] +", "+libversion[2]+"): "+" OK";
		dbgInfo += "\n";

		return true;
	}

	/**
	 * Test H5A: Attribute Interface
	 */
	private boolean h5a()
	{
		String fileName = FILE_ROOT+"SDS.h5";
		String datasetName = "IntArray";
		String attrName = "attr";
		int file_id, dataset_id, attribute_id, dataspace_id;
		int[] attr_data = {100, 200};
		int status = -1;							 

		dbgInfo += "\n\n========== Test H5A: Attribute Interface ==========";

		// Open an existing file.
		file_id = -1;
		try {
			file_id = H5.H5Fopen(fileName,
			HDF5Constants.H5F_ACC_RDWR,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5open() Failed, exception: "+ex;
			return false;
		}

		// Open an existing dataset.
		try {
			dataset_id = H5.H5Dopen(file_id, datasetName);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dopen() Failed, exception: "+ex;
			return false;
		}

		// retrive dataspace id
		try {
			dataspace_id = H5.H5Dget_space(dataset_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dget_space() Failed, exception: "+ex;
			return false;
		}

		// Create a dataset attribute.
		try {
			attribute_id = H5.H5Acreate(dataset_id, attrName,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
			dataspace_id,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Acreate() Failed, exception: "+ex;
			return false;
		}

		dbgInfo += "\nH5Acreate(): "+attribute_id;

		// Write the attribute data.
		try {
			status = H5.H5Awrite(attribute_id,
				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
				attr_data);
		}
		catch (Exception ex) { 
			dbgInfo += "\nH5Awrite: failed: "+ex;
			return false;
		}

		try {
			H5.H5Aclose(attribute_id);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Aclose: failed: "+ex;
			return false;
		}

		try {
			attribute_id = H5.H5Aopen_name(dataset_id,attrName);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Aopen_name: failed: "+ex;
			return false;
		}
		dbgInfo += "\nH5Aopen_name(): "+attribute_id;

		// read the attribute data.
/*		int[] outData = new int[2];
		try {
			status = H5.H5Aread(attribute_id,
				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
				outData);
			dbgInfo += "\nH5read(): "+status;
			dbgInfo += "\n\tattribute data: "+outData[0]+"\t"+outData[1];
		}
		catch (Exception ex) {
			dbgInfo += "\nH5Aread: failed: "+ex;
			return false;
		}
*/
		String aName[] = {""};
		long aLength = 0;
		try {
			aLength = H5.H5Aget_name(attribute_id, 25, aName);
		} catch (Exception ex) {
			dbgInfo += "\nH5Aget_name: failed: "+ex;
			return false;
		}
		dbgInfo += "\nH5Aget_name(): "+aLength+", "+aName[0];

		try {
		dbgInfo += "\nH5Aget_space(): "+H5.H5Aget_space(attribute_id);
		} catch (Exception ex) {
			dbgInfo += "\nH5Aget_space: failed: "+ex;
			return false;
		}
		try {
		dbgInfo += "\nH5Aget_type(): "+H5.H5Aget_type(attribute_id);
		} catch (Exception ex) {
			dbgInfo += "\nH5Aget_type: failed: "+ex;
			return false;
		}
		try {
		dbgInfo += "\nH5Aget_num_attrs(): "+H5.H5Aget_num_attrs(dataset_id);
		} catch (Exception ex) {
			dbgInfo += "\nH5Aget_num_attrs: failed: "+ex;
			return false;
		}
		try {
		dbgInfo += "\nH5Aopen_idx(): "+H5.H5Aopen_idx(dataset_id, 0);
		} catch (Exception ex) {
			dbgInfo += "\nH5Aopen_idx: failed: "+ex;
			return false;
		}
		//dbgInfo += "\nH5Adelete(): "+H5.H5Adelete(dataset_id,attrName);

		try {
		H5.H5Aclose(attribute_id);
		} catch (Exception ex) {
			dbgInfo += "\nH5Aclose: failed: "+ex;
			return false;
		}
		try {
		H5.H5Sclose(dataspace_id);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset_id);
		} catch (Exception ex) {
			dbgInfo += "\nH5Aclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Fclose(file_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Aclose(): FAILED "+ex;
			return false;
		}

		// msgBox.append(dbgInfo);
		return true;
	}

	/**
	 * Test H5D: Dataset Interface Functions
	 */
	private boolean h5d()
	{
		String fileName = FILE_ROOT+"SDS.h5";
		String datasetName = "IntArray";
		int nx = 5;
		int ny = 6;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset;		 /* file and dataset handles */
		int datatype, dataspace;   /* handles */
		long[] dimsf = {5, 6};	 /* dataset dimensions */
		int[] data = new int[5*6];			/* data to write */

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j*ny+i] = j*10+i;
		}

		dbgInfo += "\n\n========== Test H5D: Dataset Interface Functions ==========";

		// Create a new file using H5F_ACC_TRUNC access,
		// default file creation properties, and default file
		// access properties.
		file = -1;
		try {
		file = H5.H5Fcreate(fileName,
			HDF5Constants.H5F_ACC_TRUNC,
			HDF5Constants.H5P_DEFAULT,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\nH5Fcreate(): "+file;

		// Describe the size of the array and create the data space
		// for fixed size dataset.
		dataspace = -1;
		try {
			dataspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) {
			dbgInfo += "\nH5Screate_simple: failed: "+ex;
			return false;
		}
		dbgInfo += "\nH5Screate_simple(): "+dataspace;

		// Define datatype for the data in the file.
		datatype = -1;
		try {
		datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
		} catch (Exception ex) {
			dbgInfo += "\nH5Tcopy: failed: "+ex;
			return false;
		}
		dbgInfo += "\nH5Tcopy(): "+datatype;

		try {
			status = H5.H5Tset_order(datatype, 0);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tset_order: failed: "+ex;
			return false;
		}

		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset = H5.H5Dcreate(file, datasetName, datatype, dataspace,
			  HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\nH5Dcreate(): "+dataset;

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
			dbgInfo += "\nH5Dwrite(): "+status;
		} catch (Exception ex) {
			dbgInfo += "\n\nH5Dwrite() Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Dget_space(dataset);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dget_space() Failed, exception: "+ex;
			return false;
		}
		try {
			status = H5.H5Dget_type(dataset);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dget_type() Failed, exception: "+ex;
			// msgBox.append(dbgInfo);
			return false;
		}
		try {
			status = H5.H5Dget_create_plist(dataset);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dget_create_plist() Failed, exception: "+ex;
			return false;
		}

		int[] outData = new int[5*6];
		try {
			status = H5.H5Dread(dataset,
				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5P_DEFAULT,
				outData);
				dbgInfo += "\nH5Dread(): "+status;
		} catch (Exception ex) {
			dbgInfo += "H5Dread: failed: "+ex;
			return false;
		}

		for (j = 0; j < nx; j++)
		{
			for (i = 0; i < ny; i++) {
				if (outData[j*ny+i] != data[j*ny+i]) {
					dbgInfo += "\nbad data at:"+outData[j*ny+i];
				}
			}
		}

 // when these are used, the resulting file crashes the dumper!
		dimsf[0] = 6;
		dimsf[1] = 7;
		try {
			status = H5.H5Dextend(dataset, dimsf);
		} catch (Exception ex) {
			dbgInfo += "H5Dextend: failed: "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace);
		} catch (Exception ex) {
			dbgInfo += "H5Sclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype);
		} catch (Exception ex) {
			dbgInfo += "H5Tclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset);
		} catch (Exception ex) {
			dbgInfo += "H5Dclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Fclose(file);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
		}

		return true;
	}

	/**
	 * Test H5F: File Interface
	 */
	private boolean h5f()
	{
		int status = -1;
		String fileName = FILE_ROOT+"SDS.h5";

		dbgInfo += "\n\n========== Test H5F: File Interface ==========";
		int file_id = -1;
		try {
		file_id = H5.H5Fopen(fileName,
			HDF5Constants.H5F_ACC_RDONLY,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Fopen() Failed, exception: "+ex;
			return false;
		}
         dbgInfo += "\nH5Fopen(): "+file_id;

		try {
			status = H5.H5Fflush(file_id, HDF5Constants.H5F_SCOPE_LOCAL);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Fflush() Failed, exception: "+ex;
			return false;
		}
		try {
         		status = H5.H5Fget_create_plist(file_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Fget_create_plist() Failed, exception: "+ex;
			return false;
		}
		try {
         		status = H5.H5Fget_access_plist(file_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Fget_access_plist() Failed, exception: "+ex;
			return false;
		}
		int new_file_id = -1;
		try {
			new_file_id = H5.H5Freopen(file_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Freopen() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\nH5Freopen(): "+ new_file_id;
		try {
			H5.H5Fclose(file_id);
			H5.H5Fclose(new_file_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
		}

		return true;
	}


	/**
	 * Test H5G: Group Interface
	 */
	private boolean h5g()
	{
		String fileName = FILE_ROOT+"group.h5";
		String groupName = "/MyGroup";
		int file_id, group_id, status;
		status = -1;

		dbgInfo += "\n\n========== Test H5G: Group Interface ==========";

		// Create a new file using default properties.
		file_id = -1;
		try {
		file_id = H5.H5Fcreate(fileName,
			HDF5Constants.H5F_ACC_TRUNC,
			HDF5Constants.H5P_DEFAULT,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}

		// Create a group named "/MyGroup" in the file.
		group_id = -1;
		try {
			group_id = H5.H5Gcreate(file_id, groupName, 0);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gcreate() Failed, exception: "+ex;
			return false;
		}

		dbgInfo += "\nH5Gcreate(): "+group_id;

		group_id = -1;
		try {
			group_id = H5.H5Gopen(file_id, groupName);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gopen() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\nH5Gopen(): "+group_id;

		try {
			status = H5.H5Gset_comment( file_id, groupName, "This is a test");
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gopen() Failed, exception: "+ex;
			return false;
		}

		String[] comment = {""};
		try {
			status = H5.H5Gget_comment(file_id, groupName, 80, comment);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gopen() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\n\tGroup comment: "+comment[0];

		long[] dimsf = {5};
		int[] data = {1,2,3,4,5};
		int dataspace_id = -1;
		try {
			dataspace_id = H5.H5Screate_simple(1, dimsf, null);
		} catch (Exception ex) {
			dbgInfo += "H5Screate: failed: "+ex;
			return false;
		}
		int dataset_id = -1;
		try {
			dataset_id = H5.H5Dcreate(file_id, "intArray",
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
			dataspace_id, HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}
		try {
			H5.H5Sclose(dataspace_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sclose() Failed, exception: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dclose() Failed, exception: "+ex;
			return false;
		}

		// Create a hard link
		status = -1;
		try {
			status = H5.H5Glink (file_id, HDF5Constants.H5G_LINK_HARD, "intArray", "/MyGroup/hard");
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Glink() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\nH5Glink(), Create a hard link: "+status;

		// Create a symbolic link
		try {
			status = H5.H5Glink (file_id, HDF5Constants.H5G_LINK_SOFT, "/intArray", "/MyGroup/soft");
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Glink() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\nH5Glink(), Create a symbolic link: "+status;

		// Create a symbolic link to something that doesn't exist
		try {
			status = H5.H5Glink (file_id, HDF5Constants.H5G_LINK_SOFT, "foobar", "/MyGroup/dangle");
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Glink() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\nH5Glink(), Create a symbolic link to something that doesn't exist: "+status;

		// Create a recursive symbolic link
		try {
		status = H5.H5Glink (file_id, HDF5Constants.H5G_LINK_SOFT, "/MyGroup/recursive", "/MyGroup/recursive");
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Glink() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\nH5Glink(), Create a recursive symbolic link: "+status;

		try {
			status = H5.H5Gmove(group_id, "/MyGroup/dangle", "/MyGroup/nonexist");
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gmove() Failed, exception: "+ex;
			return false;
		}

		try {
			status =  H5.H5Gunlink(group_id, "/MyGroup/nonexist");
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gunlink() Failed, exception: "+ex;
			return false;
		}

		String[] linkVal = {""};
		try {
			status = H5.H5Gget_linkval(file_id, "/MyGroup/soft", 80, linkVal);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gget_linkval() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\n\tLink value: "+linkVal[0];

		HDF5GroupInfo gInfo = new HDF5GroupInfo();
		try {
			status = H5.H5Gget_objinfo(file_id, "intArray", true, gInfo);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gget_objinfo() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\nHDF5GroupInfo: "+gInfo.toString();

	// Test the alternative interface to get_obj_info
		long[] fileI = new long[2];
		long[] objI = new long[2];
		long[] timeI = new long[1];
		int[] linkI = new int[3];
		try {
			status = H5.H5Gget_objinfo(file_id, "intArray", true, fileI, objI, linkI, timeI );
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gget_objinfo() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\nfileI: "+fileI[0]+" "+fileI[1];
		dbgInfo += "\nobjI: "+objI[0]+" "+objI[1];
		dbgInfo += "\ntimeI: "+timeI[0];

		int nelems = 0;
		try {
			nelems = H5.H5Gn_members(file_id, "/" );
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gn_members() Failed, exception: "+ex;
			return false;
		}
		if (nelems < 1) {
			dbgInfo += "\n\nH5Gn_members() returned: "+nelems+" ? ";
			return false;
		}
		int [] oType = new int[1];
		String [] oName = new String[1];
		oName[0] = new String(" ");
		dbgInfo += "Members of group '/'";
		for (int ii = 0; ii < nelems; ii++) {
			try {
				status = H5.H5Gget_obj_info_idx(file_id, "/",ii, oName, oType );
			} catch (HDF5Exception ex) {
				dbgInfo += "\n\nH5Gn_members() Failed, exception: "+ex;
				return false;
			}
			dbgInfo += "Member: "+ii+" "+oName+" is type "+oType;
		}

		try {
			H5.H5Gclose(group_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Gclose(): FAILED "+ex;
			return false;
		}
		try {
			H5.H5Fclose(file_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}
		return true;
	}

	/**
	 * Test H5I: Identifier Interface
	 */
	private boolean h5i()
	{
		String fileName = FILE_ROOT+"id.h5";
		String groupName = "/MyGroup";
		int file_id, group_id, status;
		status = -1;

		dbgInfo += "\n\n========== Test H5I: Identifier Interface ==========";

		// Create a new file using default properties.
		file_id = -1;
		try {
		file_id = H5.H5Fcreate(fileName,
			HDF5Constants.H5F_ACC_TRUNC,
			HDF5Constants.H5P_DEFAULT,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Fcreate() Failed, exception: "+ex;
			return false;
		}

		// Create a group named "/MyGroup" in the file.
		group_id = -1;
		try {
		group_id = H5.H5Gcreate(file_id, groupName, 0);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gcreate() Failed, exception: "+ex;
			return false;
		}
		try {
			status = H5.H5Iget_type(group_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Iget_type() Failed, exception: "+ex;
			return false;
		}

		long[] dimsf = {5};
		int[] data = {1,2,3,4,5};
		int dataspace_id = -1;
		try {
		dataspace_id = H5.H5Screate_simple(1, dimsf, null);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Screate() Failed, exception: "+ex;
			return false;
		}
		int dataset_id = -1;
		try {
		dataset_id = H5.H5Dcreate(file_id, "intArray",
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
			dataspace_id, HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}
		try {
			status = H5.H5Iget_type(dataset_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sclose() Failed, exception: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dclose() Failed, exception: "+ex;
			return false;
		}
		try {
			H5.H5Gclose(group_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gclose() Failed, exception: "+ex;
			return false;
		}
		try {
			H5.H5Fclose(file_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}
		return true;
	}

	/**
	 * Test H5P: Property List Interface
	 */
	private boolean h5p()
	{

		dbgInfo += "\n\n========== Test H5P: Property List Interface ==========";

		// Create a new file with a non-standard file-creation template
		int status = -1;
		int plist_id = -1;
		try {
		plist_id = H5.H5Pcreate(HDF5Constants.H5P_FILE_CREATE);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pcreate(): FAILED "+ex;
			return false;
			
		}
		dbgInfo += "\nH5Pcreate(): "+plist_id;

		int new_plist_id = -1;
		try {
		new_plist_id = H5.H5Pcopy(plist_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pcopy(): FAILED "+ex;
			return false;
		}
			
		dbgInfo += "\nH5Pcopy(): "+new_plist_id;

		try {
			status = H5.H5Pget_class(plist_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_class(): FAILED "+ex;
			return false;
		}

		// Set the new file-creation parameters
		try {
			status = H5.H5Pset_userblock(plist_id, (long)512);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pset_userblock(): FAILED "+ex;
			return false;
		}

		long[] block_size = {-1};
		try {
			status = H5.H5Pget_userblock(plist_id, block_size);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_userblock(): FAILED "+ex;
			return false;
		}
         dbgInfo += "\n\tUser block size: "+block_size[0];

		int[] version_info = {-1, -1, -1, -1};
		try {
			status = H5.H5Pget_version(plist_id, version_info);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_version(): FAILED "+ex;
			return false;
		}
         dbgInfo += "\n\tVersion: "+version_info[0]+", "+version_info[1]+", "+
			version_info[2]+", "+version_info[3];

		try {
			status = H5.H5Pset_sizes(plist_id, 8, 8);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pset_sizes(): FAILED "+ex;
			return false;
		}

		int[] sizes = {-1, -1};
		try {
			status = H5.H5Pget_sizes(plist_id, sizes);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_sizes(): FAILED "+ex;
			return false;
		}
         dbgInfo += "\n\tsizes: "+sizes[0]+", "+sizes[1];

		try {
			status = H5.H5Pset_sym_k(plist_id, 32, 8);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pset_sym_k(): FAILED "+ex;
			return false;
		}

		int[] sym_sizes = {-1, -1};
		try {
			status = H5.H5Pget_sym_k(plist_id, sym_sizes);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_sym_k (): FAILED "+ex;
			return false;
		}
         dbgInfo += "\n\tsymbol sizes: "+sym_sizes[0]+", "+sym_sizes[1];

		try {
			status = H5.H5Pset_istore_k(plist_id, 16);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pset_istore_k (): FAILED "+ex;
			return false;
		}

		int[] istore_size = {-1};
		try {
			status = H5.H5Pget_istore_k(plist_id, istore_size);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_istore_k (): FAILED "+ex;
			return false;
		}
         dbgInfo += "\n\tchunked storage size: "+istore_size[0];

		// File Access Properties
		int fapl = 0;
		try {
			fapl = H5.H5Pcreate(HDF5Constants.H5P_FILE_ACCESS);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pcreate (): FAILED "+ex;
			return false;
		}

		boolean bstatus = false;
/**** API changes in 1.4
		try {
			status = H5.H5Pget_driver(fapl);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_driver (): FAILED "+ex;
			return false;
		}
		try {

			status = H5.H5Pset_stdio(fapl);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pset_stdio (): FAILED "+ex;
			return false;
		}
		bstatus = H5.H5Pget_stdio(fapl);
		if (!bstatus) {
			dbgInfo += "\nH5Pget_stdio (): returned false?";
			return false;
		}

		try {
			status = H5.H5Pset_sec2(fapl);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pset_sec2 (): FAILED "+ex;
			return false;
		}
		try {
			bstatus = H5.H5Pget_sec2(fapl);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_sec2 (): FAILED "+ex;
			return false;
		}
		if (!bstatus) {
			dbgInfo += "\nH5Pget_sec2 (): returned false? ";
			return false;
		}
******/

		long[] alignment = {-1, -1};
		try {
			status = H5.H5Pset_alignment(fapl, 1024*1024, 512);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pset_alignment(): FAILED "+ex;
			return false;
		}
		try {
			status = H5.H5Pget_alignment(fapl, alignment);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_alignment (): FAILED "+ex;
			return false;
		}
		dbgInfo += "\n\talignment: "+alignment[0]+", "+alignment[1];

/****** changes in API in 1.4
		int[] core = {-1};
		try {
		dbgInfo += "\nH5Pset_core(): "+H5.H5Pset_core(fapl, 1024*1024);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_sym_k (): FAILED "+ex;
			return false;
		}
		try {
			bstatus = H5.H5Pget_core(fapl, core);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_core (): FAILED "+ex;
			return false;
		}
		dbgInfo += "\n\tcore: "+core[0];

		int[] memb_id = {-1};
		long[] memb_size = {-1};
		try {
			status = H5.H5Pset_family(fapl, 1024*1024*1024,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pset_family (): FAILED "+ex;
			return false;
		}
		try {
			status = H5.H5Pget_family(fapl, memb_size, memb_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_family (): FAILED "+ex;
			return false;
		}
		dbgInfo += "\n\tmemb_size: "+memb_size[0];
 		dbgInfo += "\n\tmemb_id: "+memb_id[0];
*****/

		int[] mdc_nelmts = {-1};
         int[] rdcc_nelmts = null;
         int[] rdcc_nbytes = null;
		double[] rdcc_w0 = null;
		try {
			status = H5.H5Pget_cache(fapl, mdc_nelmts,
			rdcc_nelmts, rdcc_nbytes, rdcc_w0);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_cache (): FAILED "+ex;
			return false;
		}
 		dbgInfo += "\n\tNumber of elements: "+mdc_nelmts[0];
		try {
			status = H5.H5Pset_cache(fapl, mdc_nelmts[0], 0, 0, 0.0);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pset_cache (): FAILED "+ex;
			return false;
		}

		int[] meta_properties = {-1};
		int[] raw_properties = {-1};
		String[] meta_ext = {""};
		String[] raw_ext = {""};

/*****  changes in API in 1.4
		try {
			status = H5.H5Pset_split(fapl,
			null,
			HDF5Constants.H5P_DEFAULT,
			null,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pset_split (): FAILED "+ex;
			return false;
		}
		try {
			bstatus = H5.H5Pget_split(fapl,
			80, meta_ext, meta_properties, 80, raw_ext, raw_properties);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_split (): FAILED "+ex;
			return false;
		}
*********/

		// Dataset Creation Properties
		int dcpl = -1;
		try {
			dcpl = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pcreate (): FAILED "+ex;
			return false;
		}

		try {
			status = H5.H5Pset_layout(dcpl,HDF5Constants.H5D_CONTIGUOUS);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pset_layout (): FAILED "+ex;
			return false;
		}
		try {
			status = H5.H5Pget_layout(dcpl);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_layout (): FAILED "+ex;
			return false;
		}

		long[] csize = {5, 100};
		long[] csize_out = {-1, -1};
		try {
			status = H5.H5Pset_chunk(dcpl, 2, csize);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pset_chunk (): FAILED "+ex;
			return false;
		}
		try {
			status = H5.H5Pget_chunk(dcpl, 2, csize_out);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_chunk (): FAILED "+ex;
			return false;
		}
		dbgInfo += "\n\tChunk size: "+csize_out[0]+", "+csize_out[1];
		try {

			bstatus = H5.H5Pset_deflate(dcpl, 5);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pset_deflate (): FAILED "+ex;
			return false;
		}
		try {

         		status = H5.H5Pset_filter(dcpl, 305, 0, 0, null);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pset_filter (): FAILED "+ex;
			return false;
		}
		int filter_number = 0;
		try {
			filter_number = H5.H5Pget_nfilters(dcpl);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_nfilters (): FAILED "+ex;
			return false;
		}
		int[] flags = {-1};
		int[] cd_nelmts = {1};
		int[] cd_values = {-1};
		String[] fname = {""};
		dbgInfo += "\nH5Pget_nfilters(): "+filter_number;

		int test_filter = 0; // choose between 0 and filter_number-1
		try {
         		status = H5.H5Pget_filter(dcpl, test_filter,
				flags, cd_nelmts, cd_values, 1, fname);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_filter (): FAILED "+ex;
			return false;
		}
		int size = cd_nelmts[test_filter];
		if (size > 0)
		{
			flags = new int[size];;
			cd_values = new int[size];
			dbgInfo += "\n\tfilter\telement\t\tflags\t\tcd_nelmts\tcd_values\tname";
			try {
				H5.H5Pget_filter(dcpl, test_filter, flags, cd_nelmts, cd_values, 80, fname);
			} catch (HDF5Exception ex) {
				dbgInfo += "\nH5Pget_filter (): FAILED "+ex;
				return false;
			}
		}
		for (int i=0; i<cd_nelmts[test_filter]; i++)
			dbgInfo += "\n\t"+test_filter+"\t"+i+"\t\t"+flags[i]+
			"\t\t"+cd_nelmts[i]+"\t\t"+cd_values[i]+"\t\t"+fname[i];

		try {
			status = H5.H5Pset_external(dcpl, 
				"Test", 0, 1024*1-24*1024);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_external (): FAILED "+ex;
			return false;
		}
		int external_count = -1;
		try {
			external_count = H5.H5Pget_external_count(dcpl);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_external_count (): FAILED "+ex;
			return false;
		}
		 dbgInfo += "\nH5Pget_external_count(): "+external_count;
		long[] ex_size={0,0};
		try {
			status = H5.H5Pget_external(dcpl, 
				0, 80,fname, ex_size);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Pget_external (): FAILED "+ex;
			return false;
		}

		return true;
	}

	/**
	 * Test H5R: Reference Interface
	 */
	private boolean h5r()
	{
		String fileName = FILE_ROOT+"reference.h5";
		int fid, status;
		status = -1;

		dbgInfo += "\n\n========== Test H5R: Reference Interface ==========";

		// Create a new file using default properties.
		fid = -1;
		try {
		fid = H5.H5Fcreate(fileName,
			HDF5Constants.H5F_ACC_TRUNC,
			HDF5Constants.H5P_DEFAULT,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Fcreate() Failed, exception: "+ex;
			return false;
		}

		long[] dims = {4};
		int sid = -1;
		try {
			sid = H5.H5Screate_simple(1, dims, null);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Screate() Failed, exception: "+ex;
			return false;
		}
		int group = -1;
		try {
			group=H5.H5Gcreate(fid,"Group",-1);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gcreate() Failed, exception: "+ex;
			return false;
		}
		int dataset=-1;
		try {
		dataset=H5.H5Dcreate(group,"Dataset",
			H5.J2C(HDF5CDataTypes.JH5T_STD_U32LE),  /*????? LE??*/
			sid, HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		//HDF5Reference ref = new HDF5Reference();
		byte[] ref = new byte[12];
		try {
		ref = H5.H5Rcreate(dataset,
			"/Group/Dataset", HDF5Constants.H5R_OBJECT, -1);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Rcreate (): FAILED "+ex;
			return false;
		}

		dbgInfo += "\nH5Rcreate(): OK";

		try {
		status = H5.H5Rdereference(dataset,
			HDF5Constants.H5R_OBJECT,ref);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Rdereference (): FAILED "+ex;
			return false;
		}
		dbgInfo += "\nH5Rdereference(): "+status;

		try {
			H5.H5Sclose(sid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Sclose(): FAILED "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Dclose(): FAILED "+ex;
			return false;
		}
		try {
			H5.H5Gclose(group);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Gclose(): FAILED "+ex;
			return false;
		}
		try {
			H5.H5Fclose(fid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}
		return true;
	}

	/**
	 * Test H5S: Dataspace Interface
	 */
	private boolean h5s()
	{
		int status = -1;
		dbgInfo += "\n\n========== Test H5S: Dataspace Interface ==========";

		int fid = -1;
		try {
		fid = H5.H5Fcreate(FILE_ROOT+"dataspace.h5",
			HDF5Constants.H5F_ACC_TRUNC,
			HDF5Constants.H5P_DEFAULT,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Fcreate() Failed, exception: "+ex;
			return false;
		}

		long[] dims = {3, 15, 13};
		int sid = -1;
		try {
			sid = H5.H5Screate_simple(3, dims, null);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Screate_simple() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\nH5Screate_simple(): "+sid;

		try {
			status = H5.H5Sget_simple_extent_type(sid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sget_create_simple_type() Failed, exception: "+ex;
			return false;
		}

		long[] start = {1, 0, 0};
		long[] stride = {1, 1, 1};
		long[] count = {2, 15, 1};
		long[] block = {1, 1, 1};
		try {
			status = H5.H5Sselect_hyperslab(sid,
			HDF5Constants.H5S_SELECT_SET,start,stride,count,block);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sselect_hyperslab() Failed, exception: "+ex;
			return false;
		}

		boolean bstatus = false;
		try {
			bstatus = H5.H5Sis_simple(sid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sis_simple() Failed, exception: "+ex;
			return false;
		}

		long[] offset = {-1, 0, 0};
		try {
			status = H5.H5Soffset_simple(sid,offset);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Soffset_simple() Failed, exception: "+ex;
			return false;
		}

		int sid_copy = -1;
		try {
			sid_copy = H5.H5Scopy(sid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Scopy() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\nH5Scopy(): "+sid_copy;

		long[] dims2x = {6, 30, 26};
		try {
			status = H5.H5Sset_extent_simple(sid, 3, dims, dims2x);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sset_extent_simple() Failed, exception: "+ex;
			return false;
		}
		try {

			status = H5.H5Sextent_copy(sid_copy, sid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sextent_copy() Failed, exception: "+ex;
			return false;
		}
		try {

			status = H5.H5Sset_extent_none(sid_copy);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sset_extent_none() Failed, exception: "+ex;
			return false;
		}
		long lstatus = 0;
		try {

			lstatus = H5.H5Sget_select_npoints(sid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sget_select_npoints() Failed, exception: "+ex;
			return false;
		}

		long[] dims_out = {-1, -1, -1};
		long[] maxdims_out = {-1, -1, -1};
		try {
			status = H5.H5Sget_simple_extent_dims(
			sid, dims_out, maxdims_out);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sget_simple_extent() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\n\tdims: \t"+dims_out[0]+", "+dims_out[1]+", "+dims_out[2];
		dbgInfo += "\n\tmaxdims: "+maxdims_out[0]+", "+maxdims_out[1]+", "+maxdims_out[2];

		try {
			status = H5.H5Sget_simple_extent_ndims(sid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sget_simple_extent_ndims() Failed, exception: "+ex;
			return false;
		}
		try {

			lstatus = H5.H5Sget_simple_extent_npoints(sid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sget_simple_extent_npoints() Failed, exception: "+ex;
			return false;
		}

		long[][] coord = new long[10][3];
		coord[0][0]=0; coord[0][1]=10; coord[0][2]= 5;
		coord[1][0]=1; coord[1][1]= 2; coord[1][2]= 7;
		coord[2][0]=2; coord[2][1]= 4; coord[2][2]= 9;
		coord[3][0]=0; coord[3][1]= 6; coord[3][2]=11;
		coord[4][0]=1; coord[4][1]= 8; coord[4][2]=13;
		coord[5][0]=2; coord[5][1]=12; coord[5][2]= 0;
		coord[6][0]=0; coord[6][1]=14; coord[6][2]= 2;
		coord[7][0]=1; coord[7][1]= 0; coord[7][2]= 4;
		coord[8][0]=2; coord[8][1]= 1; coord[8][2]= 6;
		coord[9][0]=0; coord[9][1]= 3; coord[9][2]= 8;
		try {
			status = H5.H5Sselect_elements(
			sid,HDF5Constants.H5S_SELECT_SET,10,coord);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sselect_elements() Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Sselect_all(sid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sselect_all() Failed, exception: "+ex;
			return false;
		}
		try {
			bstatus = H5.H5Sselect_valid(sid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sselect_valid() Failed, exception: "+ex;
			return false;
		}
		if (!bstatus) {
			dbgInfo += "\n\nH5Sselect_valid() returned false?";
			return false;
		}
		try {
			status = H5.H5Sselect_none(sid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sselect_none() Failed, exception: "+ex;
			return false;
		}
		try {
			H5.H5Sclose(sid_copy);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sclose() Failed, exception: "+ex;
			return false;
		}
		try {
			H5.H5Sclose(sid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sclose() Failed, exception: "+ex;
			return false;
		}
		try {
			H5.H5Fclose(fid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}
		return true;
	}

	/**
	 * Test H5T: Datatype Interface
	 */
	private boolean h5t()
	{
		int status = -1;
		int rank = 3;
		long[] dims = {3, 15, 13};

		dbgInfo += "\n\n========== Test H5T: Datatype Interface ==========";

		int fid = -1;
		try {
		fid = H5.H5Fcreate(FILE_ROOT+"datatype.h5",
			HDF5Constants.H5F_ACC_TRUNC,
			HDF5Constants.H5P_DEFAULT,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Fcreate() Failed, exception: "+ex;
			return false;
		}

		int sid = -1;
		try  {
			sid = H5.H5Screate_simple(rank, dims, null);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Screate() Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,10);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tcreate() Failed, exception: "+ex;
			return false;
		}

		int tid = -1;
		try {
			tid = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Fcreate() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\nH5Tcopy(): "+tid;

		try {
			status =  H5.H5Tset_order(tid, 0);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tset_order() Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Tcommit(fid, "native-int", tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tcommit() Failed, exception: "+ex;
			return false;
		}

		boolean bstatus = false;
		try {
			bstatus = H5.H5Tcommitted(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tcommitted() Failed, exception: "+ex;
			return false;
		}
		if (!bstatus) {
			dbgInfo += "\n\nH5Tcommitted() returned false?";
			return false;
		}
		int tid2 = -1;
		try {

			tid2 = H5.H5Topen (fid, "native-int");
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Topen() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\nH5Topen(): "+tid2;

		bstatus = false;
		try {
			bstatus = H5.H5Tequal(tid, tid2);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tequal() Failed, exception: "+ex;
			return false;
		}
		if (!bstatus) {
			dbgInfo += "\n\nH5Tequal() returned false?";
			return false;
		}

		try {
			H5.H5Tclose(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tclose() Failed, exception: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(tid2);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tclose() Failed, exception: "+ex;
			return false;
		}
		try {

			tid = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_LONG));
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tcopy() Failed, exception: "+ex;
			return false;
		}
		try {

			status = H5.H5Tget_class(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tget_class() Failed, exception: "+ex;
			return false;
		}
		try {

			status = H5.H5Tset_size(tid,10);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tset_size() Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Tget_size(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tget_size() Failed, exception: "+ex;
			return false;
		}
		try {
			status =  H5.H5Tget_order(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tget_order() Failed, exception: "+ex;
			return false;
		}
		try {

			status = H5.H5Tset_precision(tid,256);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5set_precision() Failed, exception: "+ex;
			return false;
		}
		try {
			status =  H5.H5Tget_precision(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tget_precision() Failed, exception: "+ex;
			return false;
		}
		try {

			status =  H5.H5Tset_offset(tid,6);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tset_offset() Failed, exception: "+ex;
			return false;
		}
		try {
			status =  H5.H5Tget_offset(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tget_offset() Failed, exception: "+ex;
			return false;
		}

		int[] pad = {-1, -1};
		try {
		dbgInfo += "\nH5Tset_pad(): "+ H5.H5Tset_pad(tid,
			HDF5Constants.H5T_PAD_ONE,HDF5Constants.H5T_PAD_ONE);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Fcreate() Failed, exception: "+ex;
			return false;
		}
		try {
			status = H5.H5Tget_pad(tid, pad);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tget_pad() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\n\tpad: \t"+pad[0]+"\t"+pad[1];

		try {
			status =  H5.H5Tset_sign(tid,HDF5Constants.H5T_SGN_2);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tset_sign() Failed, exception: "+ex;
			return false;
		}
		try {
			status =  H5.H5Tget_sign(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tget_sign() Failed, exception: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tclose() Failed, exception: "+ex;
			return false;
		}
		try {
			tid = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT));
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tcopy() Failed, exception: "+ex;
			return false;
		}

		int[] fields = {-1, -1, -1, -1, -1};
		try {
			status = H5.H5Tset_fields(tid,1,2,3,6,5);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tset_fields() Failed, exception: "+ex;
			return false;
		}
		try {
			status = H5.H5Tget_fields(tid,fields);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tget_fields() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\n\tfields: \t"+fields[0]+"\t"+fields[1]+"\t"+fields[2]+"\t"+fields[3]+"\t"+fields[4];

		try {
			status = H5.H5Tset_ebias(tid,5);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tset_ebias() Failed, exception: "+ex;
			return false;
		}
		try {
			status = H5.H5Tget_ebias(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tget_ebias() Failed, exception: "+ex;
			return false;
		}
		try {
			status = H5.H5Tset_ebias(tid,0);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5set_norm() Failed, exception: "+ex;
			return false;
		}
		try {
			status = H5.H5Tget_norm(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tget_norm() Failed, exception: "+ex;
			return false;
		}
		try {
			status = H5.H5Tset_inpad(tid,0);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tset_inpad() Failed, exception: "+ex;
			return false;
		}
		try {
		 	status = H5.H5Tget_inpad(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tget_inpad() Failed, exception: "+ex;
			return false;
		}
		try {

			H5.H5Tclose(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tclose() Failed, exception: "+ex;
			return false;
		}
		try {
			tid = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_C_S1));
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tcopy() Failed, exception: "+ex;
			return false;
		}
		try {
			H5.H5Tset_size(tid, 20);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tset_size() Failed, exception: "+ex;
			return false;
		}
		try {
			status = H5.H5Tset_cset(tid,0);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tset_cset() Failed, exception: "+ex;
			return false;
		}
		try {
			status =  H5.H5Tget_cset(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tget_cset() Failed, exception: "+ex;
			return false;
		}
		try {
			status = H5.H5Tset_strpad(tid,0);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tset_strpad() Failed, exception: "+ex;
			return false;
		}
		try {
			status =  H5.H5Tget_strpad(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5get_strpad() Failed, exception: "+ex;
			return false;
		}
		if ( status != 0) {
			dbgInfo += "\n\nH5get_strpad() returned: "+status+" should be 0?";
			return false;
		}
		try {
			H5.H5Tclose(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tclose() Failed, exception: "+ex;
			return false;
		}
		try {
			tid = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND, 20);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tcreate() Failed, exception: "+ex;
			return false;
		}
		dbgInfo += "\nH5Tcreate(HDF5Constants.H5T_COMPOUND): "+ tid;

		try {
			status = H5.H5Tget_class(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tget_class() Failed, exception: "+ex;
			return false;
		}
		try {
				status = H5.H5Tget_size(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tget_size() Failed, exception: "+ex;
			return false;
		}
		try {
			status = H5.H5Tget_nmembers(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tget_nmembers() Failed, exception: "+ex;
			return false;
		}
		try {

			H5.H5Tclose(tid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Tclose() Failed, exception: "+ex;
			return false;
		}
		try {
			H5.H5Sclose(sid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Sclose() Failed, exception: "+ex;
			return false;
		}
		try {
			H5.H5Fclose(fid);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}
		return true;
	}

	/**
	 * Test H5E: Error Interface
	 */
	private boolean h5e()
	{
		int status = -1;
		dbgInfo += "\n\n========== Test H5E: Errors ==========";
		int fid = -1;
		try {
			fid = H5.H5Fopen("nosuchfile.h5", 
				HDF5Constants.H5F_ACC_RDONLY, 
				HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			String s = ex.getMessage();
			if (s.equals("Unable to open file")) {
				dbgInfo += ("\n\nTest OK: Open failed as expected ");
				return true;
			} else {
				dbgInfo += ("\n\nTest FAILED: Open failed with wrong exception: ");
				return false;
			}
		}
		dbgInfo += ("\n\nTest FAILED: Open succeed");
		dbgInfo += ("\n   File: "+"nosuchfile.h5"+ " should not exist");
		return false;
	}

}

