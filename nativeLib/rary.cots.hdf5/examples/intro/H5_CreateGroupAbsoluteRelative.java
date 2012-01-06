//
//   Creating groups using absolute and relative names.

package intro;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;

public class H5_CreateGroupAbsoluteRelative {
	private static String FILENAME = "groups.h5";
	private static String GROUPNAME = "MyGroup";
	private static String GROUPNAME_A = "GroupA";
	private static String GROUPNAME_B = "GroupB";

	private static void CreateGroupAbsoluteAndRelative() {
		int file_id = -1;
		int group1_id = -1;
		int group2_id = -1;
		int group3_id = -1;

		// Create a new file using default properties.
		try {
			file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC,
					HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create a group named "/MyGroup" in the file.
		try {
			if (file_id >= 0)
				group1_id = H5.H5Gcreate(file_id, "/" + GROUPNAME, 0);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create group "Group_A" in group "MyGroup" using absolute name.
		try {
			if (file_id >= 0)
				group2_id = H5.H5Gcreate(file_id, "/" + GROUPNAME + "/" + GROUPNAME_A,
						0);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create group "Group_B" in group "MyGroup" using relative name.
		try {
			if (group1_id >= 0)
				group3_id = H5.H5Gcreate(group1_id, GROUPNAME_B, 0);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Close the group3.
		try {
			if (group3_id >= 0)
				H5.H5Gclose(group3_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Close the group2.
		try {
			if (group2_id >= 0)
				H5.H5Gclose(group2_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Close the group1.
		try {
			if (group1_id >= 0)
				H5.H5Gclose(group1_id);
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
		H5_CreateGroupAbsoluteRelative.CreateGroupAbsoluteAndRelative();
	}

}
