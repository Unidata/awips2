//
//   Creating and closing a dataset.

package intro;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;

public class H5_CreateDataset {
	private static String FILENAME = "dset.h5";
	private static String DATASETNAME = "dset";
	private static final int DIM_X = 4;
	private static final int DIM_Y = 6;

	private static void CreateDataset() {
		int file_id = -1;
		int dataspace_id = -1;
		int dataset_id = -1;
		long[] dims = { DIM_X, DIM_Y };

		// Create a new file using default properties.
		try {
			file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC,
					HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create the data space for the dataset.
		try {
			dataspace_id = H5.H5Screate_simple(2, dims, null);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create the dataset.
		try {
			if ((file_id >= 0) && (dataspace_id >= 0))
				dataset_id = H5.H5Dcreate(file_id, "/" + DATASETNAME,
						HDF5Constants.H5T_STD_I32BE, dataspace_id,
						HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// End access to the dataset and release resources used by it.
		try {
			if (dataset_id >= 0)
				H5.H5Dclose(dataset_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Terminate access to the data space.
		try {
			if (dataspace_id >= 0)
				H5.H5Sclose(dataspace_id);
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
		H5_CreateDataset.CreateDataset();
	}

}
