/************************************************************

  This example shows how to read and write compound
  datatypes to a dataset.  The program first writes
  compound structures to a dataset with a dataspace of DIM0,
  then closes the file.  Next, it reopens the file, reads
  back the data, and outputs it to the screen.

  This file is intended for use with HDF5 Library verion 1.6

 ************************************************************/

package datatypes;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.util.Arrays;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;

public class H5Ex_T_Compound {
	private static String FILENAME = "h5ex_t_cmpd.h5";
	private static String DATASETNAME = "DS1";
	private static final int DIM0 = 4;
	private static final int RANK = 1;

	// CompoundDatatype class is used to capture basic externalization information.
	// Strings need to have a Maximum Size specified.
	private static class CompoundDatatype {
		protected static final int OBJHEADERSIZE = 2;
		protected static final int[] MAGICNUMBERVALUE = { 0xac, 0xed, 0x00, 0x05 };
		protected static final int MAGICNUMBER = 4;
		protected static final int INTEGERSIZE = 4;
		protected static final int DOUBLESIZE = 8;
		protected final static int MAXSTRINGSIZE = 80;

		public void readExternal(ObjectInput in) throws IOException,
				ClassNotFoundException {
		}

		public void writeExternal(ObjectOutput out) throws IOException {
		}
	}

	// Compound type class includes supporting Sensor_Datatype class.
	// The Sensor_Datatype class could be external as well.
	private static class Sensor implements java.io.Externalizable {
		static Sensor_Datatype datatypes;

		int serial_no;
		String location;
		double temperature;
		double pressure;

		Sensor() {
			datatypes = new Sensor_Datatype();
		}

		// Each data member field must be shown how to be written and read.
		// Strings need to be handled by bytes.
		public void readExternal(ObjectInput in) throws IOException,
				ClassNotFoundException {
			serial_no = in.readInt();
			byte[] tempbuf = new byte[Sensor_Datatype.MAXSTRINGSIZE];
			for (int indx = 0; indx < Sensor_Datatype.MAXSTRINGSIZE; indx++) {
				tempbuf[indx] = in.readByte();
			}
			location = new String(tempbuf);
			temperature = in.readDouble();
			pressure = in.readDouble();
		}

		public void writeExternal(ObjectOutput out) throws IOException {
			out.writeInt(serial_no);
			for (int indx = 0; indx < Sensor_Datatype.MAXSTRINGSIZE; indx++) {
				if (indx < location.length())
					out.writeByte(location.charAt(indx));
				else
					out.writeByte(0);
			}
			out.writeDouble(temperature);
			out.writeDouble(pressure);
		}
	}

	// Using Java Externalization will add a two-byte object header in
	// the stream, which needs to be called out in the datatypes.
	private static class Sensor_Datatype extends CompoundDatatype {
		static int numberMembers = 5;
		static int[] memberDims = { 1, 1, 1, 1, 1 };

		String[] memberNames = { "ObjectHeader", "Serial number", "Location",
				"Temperature (F)", "Pressure (inHg)" };
		int[] memberMemTypes = { HDF5Constants.H5T_NATIVE_SHORT,
				HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5T_C_S1,
				HDF5Constants.H5T_NATIVE_DOUBLE, HDF5Constants.H5T_NATIVE_DOUBLE };
		int[] memberFileTypes = { HDF5Constants.H5T_STD_I16BE,
				HDF5Constants.H5T_STD_I32BE, HDF5Constants.H5T_C_S1,
				HDF5Constants.H5T_IEEE_F64BE, HDF5Constants.H5T_IEEE_F64BE };
		static int[] memberStorage = { OBJHEADERSIZE, INTEGERSIZE, MAXSTRINGSIZE,
				DOUBLESIZE, DOUBLESIZE };

		// Data size is the storage size for the members not the object.
		// Java Externalization also adds a 4-byte "Magic Number" to the beginning 
		// of the data stream
		static int getTotalDataSize() {
			int data_size = 0;
			for (int indx = 0; indx < numberMembers; indx++)
				data_size += memberStorage[indx] * memberDims[indx];
			return DIM0 * data_size + MAGICNUMBER;
		}

		static int getDataSize() {
			int data_size = 0;
			for (int indx = 0; indx < numberMembers; indx++)
				data_size += memberStorage[indx] * memberDims[indx];
			return data_size;
		}

		static int getOffset(int memberItem) {
			int data_offset = 0;
			for (int indx = 0; indx < memberItem; indx++)
				data_offset += memberStorage[indx];
			return data_offset;
		}
	}

	private static void CreateDataset() {
		int file_id = -1;
		int strtype_id = -1;
		int memtype_id = -1;
		int filetype_id = -1;
		int dataspace_id = -1;
		int dataset_id = -1;
		long[] dims = { DIM0 };
		Sensor[] object_data = new Sensor[DIM0];
		byte[] dset_data = null;

		// Initialize data.
		object_data[0] = new Sensor();
		object_data[0].serial_no = 1153;
		object_data[0].location = new String("Exterior (static)");
		object_data[0].temperature = 53.23;
		object_data[0].pressure = 24.57;
		object_data[1] = new Sensor();
		object_data[1].serial_no = 1184;
		object_data[1].location = new String("Intake");
		object_data[1].temperature = 55.12;
		object_data[1].pressure = 22.95;
		object_data[2] = new Sensor();
		object_data[2].serial_no = 1027;
		object_data[2].location = new String("Intake manifold");
		object_data[2].temperature = 103.55;
		object_data[2].pressure = 31.23;
		object_data[3] = new Sensor();
		object_data[3].serial_no = 1313;
		object_data[3].location = new String("Exhaust manifold");
		object_data[3].temperature = 1252.89;
		object_data[3].pressure = 84.11;

		// Create a new file using default properties.
		try {
			file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC,
					HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create string datatype.
		try {
			strtype_id = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
			if (strtype_id >= 0)
				H5.H5Tset_size(strtype_id, Sensor_Datatype.MAXSTRINGSIZE);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create the compound datatype for memory.
		try {
			memtype_id = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND, Sensor_Datatype
					.getDataSize());
			if (memtype_id >= 0) {
				for (int indx = 0; indx < Sensor_Datatype.numberMembers; indx++) {
					int type_id = Sensor.datatypes.memberMemTypes[indx];
					if (type_id == HDF5Constants.H5T_C_S1)
						type_id = strtype_id;
					H5.H5Tinsert(memtype_id, Sensor.datatypes.memberNames[indx],
							Sensor_Datatype.getOffset(indx), type_id);
				}
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create the compound datatype for the file. Because the standard
		// types we are using for the file may have different sizes than
		// the corresponding native types, we must manually calculate the
		// offset of each member.
		try {
			filetype_id = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND, Sensor_Datatype
					.getDataSize());
			if (filetype_id >= 0) {
				for (int indx = 0; indx < Sensor_Datatype.numberMembers; indx++) {
					int type_id = Sensor.datatypes.memberFileTypes[indx];
					if (type_id == HDF5Constants.H5T_C_S1)
						type_id = strtype_id;
					H5.H5Tinsert(filetype_id, Sensor.datatypes.memberNames[indx],
							Sensor_Datatype.getOffset(indx), type_id);
				}
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create dataspace. Setting maximum size to NULL sets the maximum
		// size to be the current size.
		try {
			dataspace_id = H5.H5Screate_simple(RANK, dims, null);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create the dataset.
		try {
			if ((file_id >= 0) && (dataspace_id >= 0) && (filetype_id >= 0))
				dataset_id = H5.H5Dcreate(file_id, DATASETNAME, filetype_id,
						dataspace_id, HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Write the compound data to the dataset.
		try {
			ByteArrayOutputStream baos = new ByteArrayOutputStream(Sensor_Datatype
					.getTotalDataSize());
			ObjectOutputStream oout = new ObjectOutputStream(baos);
			for (int indx = 0; indx < DIM0; indx++) {
				object_data[indx].writeExternal(oout);
				oout.flush();
			}
			oout.close();
			baos.close();
			dset_data = baos.toByteArray();
			byte[] write_data = new byte[dset_data.length-Sensor_Datatype.MAGICNUMBER];
			for(int indx=0; indx<dset_data.length-Sensor_Datatype.MAGICNUMBER;indx++)
				write_data[indx] = dset_data[indx+Sensor_Datatype.MAGICNUMBER];

			if ((dataset_id >= 0) && (memtype_id >= 0))
				H5.H5Dwrite(dataset_id, memtype_id, HDF5Constants.H5S_ALL,
						HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, write_data);
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

		// Terminate access to the file type.
		try {
			if (filetype_id >= 0)
				H5.H5Tclose(filetype_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Terminate access to the mem type.
		try {
			if (memtype_id >= 0)
				H5.H5Tclose(memtype_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		try {
			if (strtype_id >= 0)
				H5.H5Tclose(strtype_id);
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

	private static void ReadDataset() {
		int file_id = -1;
		int strtype_id = -1;
		int memtype_id = -1;
		int dataspace_id = -1;
		int dataset_id = -1;
		long[] dims = { DIM0 };
		Sensor[] object_data2;
		byte[] dset_data;

		// Open an existing file.
		try {
			file_id = H5.H5Fopen(FILENAME, HDF5Constants.H5F_ACC_RDONLY,
					HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Open an existing dataset.
		try {
			if (file_id >= 0)
				dataset_id = H5.H5Dopen(file_id, DATASETNAME);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Get dataspace and allocate memory for read buffer.
		try {
			if (dataset_id >= 0)
				dataspace_id = H5.H5Dget_space(dataset_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		try {
			if (dataspace_id >= 0)
				H5.H5Sget_simple_extent_dims(dataspace_id, dims, null);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create string datatype.
		try {
			strtype_id = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
			if (strtype_id >= 0)
				H5.H5Tset_size(strtype_id, Sensor_Datatype.MAXSTRINGSIZE);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Create the compound datatype for memory.
		try {
			memtype_id = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND, Sensor_Datatype
					.getDataSize());
			if (memtype_id >= 0) {
				for (int indx = 0; indx < Sensor_Datatype.numberMembers; indx++) {
					int type_id = Sensor.datatypes.memberMemTypes[indx];
					if (type_id == HDF5Constants.H5T_C_S1)
						type_id = strtype_id;
					H5.H5Tinsert(memtype_id, Sensor.datatypes.memberNames[indx],
							Sensor_Datatype.getOffset(indx), type_id);
				}
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// allocate memory for read buffer.
		byte[] read_data = new byte[(int) dims[0] * Sensor_Datatype.getDataSize()];

		object_data2 = new Sensor[(int) dims[0]];

		// Read data.
		try {
			if ((dataset_id >= 0) && (memtype_id >= 0))
				H5.H5Dread(dataset_id, memtype_id, HDF5Constants.H5S_ALL,
						HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, read_data);

			dset_data = new byte[read_data.length + Sensor_Datatype.MAGICNUMBER];
			for (int indx = 0; indx < Sensor_Datatype.MAGICNUMBER; indx++)
				dset_data[indx] = (byte) Sensor_Datatype.MAGICNUMBERVALUE[indx];
			for (int indx = 0; indx < read_data.length; indx++)
				dset_data[indx + Sensor_Datatype.MAGICNUMBER] = read_data[indx];
			ObjectInputStream objectIn = new ObjectInputStream(
					new ByteArrayInputStream(dset_data));

			for (int indx = 0; indx < (int) dims[0]; indx++) {
				object_data2[indx] = new Sensor();
				object_data2[indx].readExternal(objectIn);
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Output the data to the screen.
		for (int indx = 0; indx < dims[0]; indx++) {
			System.out.println(DATASETNAME + " [" + indx + "]:");
			System.out.println("Serial number   : " + object_data2[indx].serial_no);
			System.out.println("Location        : " + object_data2[indx].location);
			System.out.println("Temperature (F) : " + object_data2[indx].temperature);
			System.out.println("Pressure (inHg) : " + object_data2[indx].pressure);
			System.out.println();
		}
		System.out.println();

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

		// Terminate access to the mem type.
		try {
			if (memtype_id >= 0)
				H5.H5Tclose(memtype_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		try {
			if (strtype_id >= 0)
				H5.H5Tclose(strtype_id);
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
		H5Ex_T_Compound.CreateDataset();
		// Now we begin the read section of this example. Here we assume
		// the dataset and array have the same name and rank, but can have
		// any size. Therefore we must allocate a new array to read in
		// data using malloc().
		H5Ex_T_Compound.ReadDataset();
	}

}
