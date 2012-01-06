//
//   Creating a dataset attribute.

package intro;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;

public class H5_CreateAttribute {
	private static String FILENAME = "dset.h5";
	private static String DATASETNAME = "dset";
	private static String DATASETATTRIBUTE = "Units";

	private static void CreateDatasetAttribute() {
		int file_id = -1;
		int dataspace_id = -1;
		int dataset_id = -1;
		int attribute_id = -1;
		long[] dims = { 2 };
		int[] attr_data = { 100, 200 };

		// Open an existing file.
		try {
			file_id = H5.H5Fopen(FILENAME, HDF5Constants.H5F_ACC_RDWR,
					HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Open an existing dataset.
		try {
			if (file_id >= 0)
				dataset_id = H5.H5Dopen(file_id, "/" + DATASETNAME);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create the data space for the attribute.
		try {
			dataspace_id = H5.H5Screate_simple(1, dims, null);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create a dataset attribute.
		try {
			if ((dataset_id >= 0) && (dataspace_id >= 0))
				attribute_id = H5.H5Acreate(dataset_id, DATASETATTRIBUTE,
						HDF5Constants.H5T_STD_I32BE, dataspace_id,
						HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Write the attribute data.
		try {
			if (attribute_id >= 0)
				H5.H5Awrite(attribute_id, HDF5Constants.H5T_NATIVE_INT, attr_data);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Close the attribute.
		try {
			if (attribute_id >= 0)
				H5.H5Aclose(attribute_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Close the dataspace.
		try {
			if (dataspace_id >= 0)
				H5.H5Sclose(dataspace_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Close to the dataset.
		try {
			if (dataset_id >= 0)
				H5.H5Dclose(dataset_id);
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
		H5_CreateAttribute.CreateDatasetAttribute();
	}

}
