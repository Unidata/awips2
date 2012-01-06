//
//  Create two datasets within groups.

package intro;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;

public class H5_CreateGroupDataset {
	private static String FILENAME = "groups.h5";
	private static String GROUPNAME = "MyGroup";
	private static String GROUPNAME_A = "GroupA";
	private static String DATASETNAME1 = "dset1";
	private static String DATASETNAME2 = "dset2";
	private static final int DIM1_X = 3;
	private static final int DIM1_Y = 3;
	private static final int DIM2_X = 2;
	private static final int DIM2_Y = 10;

	private static void h5_crtgrpd() {
		int file_id = -1;
		int dataspace_id = -1;
		int dataset_id = -1;
		int group_id = -1;
		int[][] dset1_data = new int[DIM1_X][DIM1_Y];
		int[][] dset2_data = new int[DIM2_X][DIM2_Y];
		long[] dims1 = { DIM1_X, DIM1_Y };
		long[] dims2 = { DIM2_X, DIM2_Y };

		// Initialize the first dataset.
		for (int indx = 0; indx < DIM1_X; indx++)
			for (int jndx = 0; jndx < DIM1_Y; jndx++)
				dset1_data[indx][jndx] = jndx + 1;

		// Initialize the second dataset.
		for (int indx = 0; indx < DIM2_X; indx++)
			for (int jndx = 0; jndx < DIM2_Y; jndx++)
				dset2_data[indx][jndx] = jndx + 1;

		// Open an existing file.
		try {
			file_id = H5.H5Fopen(FILENAME, HDF5Constants.H5F_ACC_RDWR,
					HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create the data space for the first dataset.
		try {
			dataspace_id = H5.H5Screate_simple(2, dims1, null);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create the dataset in group "MyGroup".
		try {
			if ((file_id >= 0) && (dataspace_id >= 0))
				dataset_id = H5.H5Dcreate(file_id,
						"/" + GROUPNAME + "/" + DATASETNAME1, HDF5Constants.H5T_STD_I32BE,
						dataspace_id, HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Write the first dataset.
		try {
			if (dataset_id >= 0)
				H5.H5Dwrite(dataset_id, HDF5Constants.H5T_NATIVE_INT,
						HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
						HDF5Constants.H5P_DEFAULT, dset1_data);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Close the data space for the first dataset.
		try {
			if (dataspace_id >= 0)
				H5.H5Sclose(dataspace_id);
			dataspace_id = -1;
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Close the first dataset.
		try {
			if (dataset_id >= 0)
				H5.H5Dclose(dataset_id);
			dataset_id = -1;
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Open an existing group of the specified file.
		try {
			if (file_id >= 0)
				group_id = H5.H5Gopen(file_id, "/" + GROUPNAME + "/" + GROUPNAME_A);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create the data space for the second dataset.
		try {
			dataspace_id = H5.H5Screate_simple(2, dims2, null);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create the second dataset in group "Group_A".
		try {
			if ((group_id >= 0) && (dataspace_id >= 0))
				dataset_id = H5.H5Dcreate(group_id, DATASETNAME2,
						HDF5Constants.H5T_STD_I32BE, dataspace_id,
						HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Write the second dataset.
		try {
			if (dataset_id >= 0)
				H5.H5Dwrite(dataset_id, HDF5Constants.H5T_NATIVE_INT,
						HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
						HDF5Constants.H5P_DEFAULT, dset2_data);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Close the data space for the second dataset.
		try {
			if (dataspace_id >= 0)
				H5.H5Sclose(dataspace_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Close the second dataset.
		try {
			if (dataset_id >= 0)
				H5.H5Dclose(dataset_id);
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
		H5_CreateGroupDataset.h5_crtgrpd();
	}

}
