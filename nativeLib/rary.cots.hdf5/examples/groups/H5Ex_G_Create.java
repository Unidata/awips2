/************************************************************

  This example shows how to create, open, and close a group.

  This file is intended for use with HDF5 Library verion 1.6

 ************************************************************/

package groups;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;

public class H5Ex_G_Create {
	private static String FILENAME = "h5ex_g_create.h5";
	private static String GROUPNAME = "G1";

	private static void CreateGroup() {
		int file_id = -1;
		int group_id = -1;

		// Create a new file using default properties.
		try {
			file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC,
					HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create a group in the file.
		try {
			if (file_id >= 0)
				group_id = H5.H5Gcreate(file_id, "/" + GROUPNAME,
						HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Close the group. The handle "group" can no longer be used.
		try {
			if (group_id >= 0)
				H5.H5Gclose(group_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Re-open the group, obtaining a new handle.
		try {
			if (file_id >= 0)
				group_id = H5.H5Gopen(file_id, "/" + GROUPNAME);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Close the group.
		try {
			if (group_id >= 0)
				H5.H5Gclose(group_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Close the file.
		try {
			if (file_id >= 0)
				H5.H5Fclose(file_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

	}

	public static void main(String[] args) {
		H5Ex_G_Create.CreateGroup();
	}

}
