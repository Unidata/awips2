//
//    Creating and closing a group.

package intro;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;

public class H5_CreateGroup {
	private static String FILENAME = "group.h5";
	private static String GROUPNAME = "MyGroup";

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
		H5_CreateGroup.CreateGroup();
	}

}
