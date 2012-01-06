/************************************************************

  This example shows how to iterate over group members using
  H5Gget_obj_info_all.

  This file is intended for use with HDF5 Library verion 1.6

 ************************************************************/
package groups;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;

public class H5Ex_G_Iterate {
	private static String FILENAME = "h5ex_g_iterate.h5";
	private static String DATASETNAME = "/";

	enum H5G_object {
		H5G_UNKNOWN(-1), // Unknown object type
		H5G_GROUP(0), // Object is a group
		H5G_DATASET(1), // Object is a dataset
		H5G_TYPE(2), // Object is a named data type
		H5G_LINK(3), // Object is a symbolic link
		H5G_UDLINK(4), // Object is a user-defined link
		H5G_RESERVED_5(5), // Reserved for future use
		H5G_RESERVED_6(6), // Reserved for future use
		H5G_RESERVED_7(7); // Reserved for future use
		private static final Map<Integer, H5G_object> lookup = new HashMap<Integer, H5G_object>();

		static {
			for (H5G_object s : EnumSet.allOf(H5G_object.class))
				lookup.put(s.getCode(), s);
		}

		private int code;

		H5G_object(int layout_type) {
			this.code = layout_type;
		}

		public int getCode() {
			return this.code;
		}

		public static H5G_object get(int code) {
			return lookup.get(code);
		}
	}

	private static void do_iterate() {
		int file_id = -1;

		// Open a file using default properties.
		try {
			file_id = H5.H5Fopen(FILENAME, HDF5Constants.H5F_ACC_RDONLY,
					HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Begin iteration.
		System.out.println("Objects in root group:");
		try {
			if (file_id >= 0) {
				int count = H5.H5Gn_members(file_id, DATASETNAME);
				String[] oname = new String[count];
				int[] otype = new int[count];
        long[] orefs = new long[count];
				H5.H5Gget_obj_info_all(file_id, DATASETNAME, oname, otype, orefs);

				// Get type of the object and display its name and type.
				for (int indx = 0; indx < otype.length; indx++) {
					switch (H5G_object.get(otype[indx])) {
					case H5G_GROUP:
						System.out.println("  Group: " + oname[indx]);
						break;
					case H5G_DATASET:
						System.out.println("  Dataset: " + oname[indx]);
						break;
					case H5G_TYPE:
						System.out.println("  Datatype: " + oname[indx]);
						break;
					default:
						System.out.println("  Unknown: " + oname[indx]);
					}
				}
			}
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
		H5Ex_G_Iterate.do_iterate();
	}

}
