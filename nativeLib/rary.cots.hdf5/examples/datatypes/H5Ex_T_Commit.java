/************************************************************

  This example shows how to commit a named datatype to a
  file, and read back that datatype.  The program first
  defines a compound datatype, commits it to a file, then
  closes the file.  Next, it reopens the file, opens the
  datatype, and outputs the names of its fields to the
  screen.

  This file is intended for use with HDF5 Library verion 1.6

 ************************************************************/

package datatypes;

import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;

public class H5Ex_T_Commit {
	private static String FILENAME = "h5ex_t_commit.h5";
	private static String DATATYPENAME = "Sensor_Type";

	// Values for the various classes of datatypes
	enum H5T_class {
    H5T_NO_CLASS(-1),  // error
    H5T_INTEGER(0),    // integer types
    H5T_FLOAT(1),      // floating-point types
    H5T_TIME(2),       // date and time types
    H5T_STRING(3),     // character string types
    H5T_BITFIELD(4),   // bit field types
    H5T_OPAQUE(5),     // opaque types
    H5T_COMPOUND(6),   // compound types
    H5T_REFERENCE(7),  // reference types
    H5T_ENUM(8),	     // enumeration types
    H5T_VLEN(9),	     // Variable-Length types
    H5T_ARRAY(10),	   // Array types

    H5T_NCLASSES(11);  // this must be last
		private static final Map<Integer, H5T_class> lookup = new HashMap<Integer, H5T_class>();

		static {
			for (H5T_class s : EnumSet.allOf(H5T_class.class))
				lookup.put(s.getCode(), s);
		}

		private int code;

		H5T_class(int layout_type) {
			this.code = layout_type;
		}

		public int getCode() {
			return this.code;
		}

		public static H5T_class get(int code) {
			return lookup.get(code);
		}
	}

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

	// The supporting Sensor_Datatype class.
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

	private static void CreateDataType() {
		int file_id = -1;
		int strtype_id = -1;
		int filetype_id = -1;
		Sensor_Datatype datatypes = new Sensor_Datatype();
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

		// Create the compound datatype for the file. Because the standard
		// types we are using for the file may have different sizes than
		// the corresponding native types, we must manually calculate the
		// offset of each member.
		try {
			filetype_id = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND, Sensor_Datatype
					.getDataSize());
			if (filetype_id >= 0) {
				for (int indx = 0; indx < Sensor_Datatype.numberMembers; indx++) {
					int type_id = datatypes.memberFileTypes[indx];
					if (type_id == HDF5Constants.H5T_C_S1)
						type_id = strtype_id;
					H5.H5Tinsert(filetype_id, datatypes.memberNames[indx],
							Sensor_Datatype.getOffset(indx), type_id);
				}
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Commit the compound datatype to the file, creating a named
    // datatype.
		try {
			if ((file_id >= 0) && (filetype_id >= 0))
				H5.H5Tcommit(file_id, DATATYPENAME, filetype_id);
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

		// Terminate access to the str type.
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

	private static void ReadDataType() {
		int file_id = -1;
		int typeclass_id = -1;
		int filetype_id = -1;

		// Open an existing file.
		try {
			file_id = H5.H5Fopen(FILENAME, HDF5Constants.H5F_ACC_RDONLY,
					HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Open named datatype.
		try {
			if (file_id >= 0)
				filetype_id = H5.H5Topen(file_id, DATATYPENAME);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

    // Output the data to the screen.
		System.out.println("Named datatype:  " + DATATYPENAME+": ");

    // Get datatype class.  If it isn't compound, we won't print
    // anything.
		try {
			if (filetype_id >= 0)
				typeclass_id = H5.H5Tget_class(filetype_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		// Read data.
		try {
			if(H5T_class.get(typeclass_id)==H5T_class.H5T_COMPOUND) {
				System.out.println("   Class: H5T_COMPOUND");
        int nmembs = H5.H5Tget_nmembers (filetype_id);
        // Iterate over compound datatype members.
        for (int indx=0; indx<nmembs; indx++) {
          String member_name = H5.H5Tget_member_name (filetype_id, indx);
    			System.out.println("    " + member_name);
        }
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Terminate access to the mem type.
		try {
			if (filetype_id >= 0)
				H5.H5Tclose(filetype_id);
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
		H5Ex_T_Commit.CreateDataType();
		// Now we begin the read section of this example. Here we assume
		// the dataset and array have the same name and rank, but can have
		// any size. Therefore we must allocate a new array to read in
		// data using malloc().
		H5Ex_T_Commit.ReadDataType();
	}

}
