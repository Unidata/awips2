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

public class TestIter implements TestModule
{
	private String FILE_ROOT = null;


	String dbgInfo;
	String theTree;
	String theTree2;
	boolean testResult = false;

	public TestIter()
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
                        dbgInfo += "\nH5close(: FAILED "+ex;
			return false;
                }
		return true;
        }


	public String getTestName() {
		String desc = "Test iterator function";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test functions.";
		return desc;
	}

	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 1;
		int passed = 0;
		dbgInfo = "\n\n========== Test Basic iterator==========";
		if ( testIter1() ) {
			passed++;
		}
		dbgInfo += "\n\n========== Byte/byte tests complete: "+passed+" of "+ntests+" passed  ==========";
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

private boolean genfile( String filename )
{
	String  datasetName = "Space1";
	int  rank = 1;
	int  dims = 4;
	int		file;	
	int		dataset;
	int		group1; 
	int		pgroup;  
	int		group2; 
	int		group3; 
	int		group4; 
	int		group5; 
	int		group6; 
	int		group7; 
	int		link;  
	int		sid; 
	int		tid;  
	long		dimsf[] = {dims};
	int		ret;

	/* Compound datatype */
/*
	typedef struct s1_t {
		unsigned int a;
		unsigned int b;
		float c;
	} s1_t;
*/
/*
int [] oType1[0] = new int[1];
String [] oName1 = new String[1];
oName1[0] = new String(" ");          
int [] oType2 = new int[1];
String [] oName2 = new String[1];
oName2[0] = new String(" ");          
	int i;
	int i2;
	int ne2;
	int nelems = 0;
*/


	file = -1;
	try {
		file = H5.H5Fcreate(filename,
			HDF5Constants.H5F_ACC_TRUNC,
                        HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5create() Failed, exception: "+ex;
		return false;
	}

	/* Create dataspace for datasets */
	sid = -1;
	try {
		sid = H5.H5Screate_simple(rank, dimsf, null);
	} catch (Exception ex) { 
		dbgInfo += "\nH5Screate_simple: dataspace1 failed: "+ex; 
		return false;
	}


	/* Create a group */
	group1 = -1;
	try {
		group1 = H5.H5Gcreate(file, "Group1", -1);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Gcreate() Failed, group 1, exception: "+ex;
		return false;
	}

	group2 = -1;
	try {
		group2 = H5.H5Gcreate(group1, "Group2", -1);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Gcreate() Failed, group 2, exception: "+ex;
		return false;
	}

	group3 = -1;
	try {
		group3 = H5.H5Gcreate(group1, "Group3", -1);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Gcreate() Failed, group 3, exception: "+ex;
		return false;
	}

	group4 = -1;
	try {
		group4 = H5.H5Gcreate(group1, "Group4", -1);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Gcreate() Failed, group 4, exception: "+ex;
		return false;
	}

	// create a dangling link...
	try {
		ret = H5.H5Glink (file, HDF5Constants.H5G_LINK_SOFT, "think", "other");
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Glink() Failed, exception: "+ex;
		return false;
	}


	dataset = -1;
	try {
		dataset = H5.H5Dcreate(file, "Dataset1",
                        H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
                        sid, HDF5Constants.H5P_DEFAULT);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
		return false;
	}

	try {
		H5.H5Dclose(dataset);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Dclose() Failed, exception: "+ex;
		return false;
	}

	// put in a soft link
	try {
		ret = H5.H5Glink (file, HDF5Constants.H5G_LINK_SOFT, "Dataset1", "SD1");
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Glink() soft link failed, exception: "+ex;
		return false;
	}

	// put in a hard link
	try {
		ret = H5.H5Glink (file, HDF5Constants.H5G_LINK_HARD, "Dataset1", "HD1");
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Glink() hard link failed, exception: "+ex;
		return false;
	}


	dataset = -1;
	try {
		dataset = H5.H5Dcreate(group2, "Dataset2",
                        H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
                        sid, HDF5Constants.H5P_DEFAULT);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Dcreate() dataset 2 Failed, exception: "+ex;
		return false;
	}

	try {
		H5.H5Dclose(dataset);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Dclose() dataset 2 Failed, exception: "+ex;
		return false;
	}

	dataset = -1;
	try {
		dataset = H5.H5Dcreate(group2, "Dataset3",
                        H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
                        sid, HDF5Constants.H5P_DEFAULT);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Dcreate() dataset 3 Failed, exception: "+ex;
		return false;
	}

	try {
		H5.H5Dclose(dataset);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Dclose() dataset 3 Failed, exception: "+ex;
		return false;
	}

	dataset = -1;
	try {
		dataset = H5.H5Dcreate(group2, "Dataset4",
                        H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
                        sid, HDF5Constants.H5P_DEFAULT);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Dcreate() dataset 4 Failed, exception: "+ex;
		return false;
	}

	try {
		H5.H5Dclose(dataset);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Dclose() dataset 4 Failed, exception: "+ex;
		return false;
	}
	dataset = -1;
	try {
		dataset = H5.H5Dcreate(group3, "Dataset5",
                        H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
                        sid, HDF5Constants.H5P_DEFAULT);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Dcreate() dataset 5 Failed, exception: "+ex;
		return false;
	}

	try {
		H5.H5Dclose(dataset);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Dclose() dataset 5 Failed, exception: "+ex;
		return false;
	}

	dataset = -1;
	try {
		dataset = H5.H5Dcreate(group3, "Dataset6",
                        H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
                        sid, HDF5Constants.H5P_DEFAULT);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Dcreate() dataset 6 Failed, exception: "+ex;
		return false;
	}

	try {
		H5.H5Dclose(dataset);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Dclose() dataset 6 Failed, exception: "+ex;
		return false;
	}

	dataset = -1;
	try {
		dataset = H5.H5Dcreate(group4, "Dataset7",
                        H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
                        sid, HDF5Constants.H5P_DEFAULT);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Dcreate() dataset 7 Failed, exception: "+ex;
		return false;
	}

	try {
		H5.H5Dclose(dataset);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Dclose() dataset 7 Failed, exception: "+ex;
		return false;
	}

	try {
		H5.H5Gclose(group1);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Gclose() group 1 Failed, exception: "+ex;
		return false;
	}
	try {
		H5.H5Gclose(group2);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Gclose() group 2 Failed, exception: "+ex;
		return false;
	}
	try {
		H5.H5Gclose(group3);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Gclose() group 3 Failed, exception: "+ex;
		return false;
	}
	try {
		H5.H5Gclose(group4);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Gclose() group 4 Failed, exception: "+ex;
		return false;
	}


	tid = -1;
	try {
		tid = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,12);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Tcreate() compound Failed, exception: "+ex;
		return false;
	}

	try {
		H5.H5Tinsert(tid, "a", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
	} catch (Exception ex) { 
		dbgInfo += "\nH5Tinsert: 'a' datatype1 failed: "+ex; 
        //ex.printStackTrace();
		return false; 
	}
	try {
		H5.H5Tinsert(tid, "b", 4, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
	} catch (Exception ex) { 
		dbgInfo += "\nH5Tinsert: 'b' datatype1 failed: "+ex; 
        //ex.printStackTrace();
		return false; 
	}
	try {
		H5.H5Tinsert(tid, "c", 8, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT));
	} catch (Exception ex) { 
		dbgInfo += "\nH5Tinsert: 'c' datatype1 failed: "+ex; 
        //ex.printStackTrace();
		return false; 
	}


	try {
		ret = H5.H5Tcommit(file, "Datatype1", tid);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Tcommit() Failed, exception: "+ex;
		return false;
	}

	try {
		H5.H5Tclose(tid);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Tclose()  Failed, exception: "+ex;
		return false;
	}
	try {
		H5.H5Fclose(file);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Fclose() Failed, exception: "+ex;
		return false;
	}

	return true;
}


private boolean
breadth_first_2( int pid1, String gname1, int pid2, String gname2)
{
	int ret1;
	int ret2;
	int pgroup2;
	int pgroup1;
int i;
int nelems1 = 0;
int nelems2 = 0;
int [] oType1 = new int[1];
String [] oName1 = new String[1];
oName1[0] = new String(" ");          
int [] oType2 = new int[1];
String [] oName2 = new String[1];
oName2[0] = new String(" ");          


	/* Iterate through the file to see members of the root group */
	nelems1 = 0;
	try {
		nelems1 = H5.H5Gn_members(pid1, gname1 );
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Gn_members() pid1 Failed group "+gname1+", exception: "+ex;
		return false;
	}                     
	if (nelems1 < 0 ) {
		dbgInfo += "\nH5Gn_members: FAIL group: "+gname1;
		return false;
	}
	nelems2 = 0;
	try {
		nelems2 = H5.H5Gn_members(pid2, gname2 );
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Gn_members() pid2 Failed group "+gname2+", exception: "+ex;
		return false;
	}                     
	if (nelems2 < 0 ) {
		dbgInfo += "\nH5Gn_members: FAIL group: "+gname2;
		return false;
	}

	theTree += "((Group:"+gname1+")";
	//System.out.println("\n");
	//System.out.print("File 1: Members of  "+ gname1+": ");
	for ( i = 0; i < nelems1; i++) {
		try {
			ret1 = H5.H5Gget_obj_info_idx(pid1, gname1,i, oName1, oType1 );
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gn_members() Failed, exception: "+ex;
			return false;
		}  
		if (ret1 < 0) {
			dbgInfo += "\n error\n";
			continue;
		}
		switch (oType1[0]) {
		case HDF5Constants.H5G_GROUP: 
			theTree += "(Group:"+oName1[0]+")";
			//System.out.print(" Group "+oName1[0]+", ");
			break;
		case HDF5Constants.H5G_DATASET: 
			theTree += "(Dataset:"+oName1[0]+")";
			//System.out.print(" Dataset "+oName1[0]+", ");
			break;
		case HDF5Constants.H5G_TYPE: 
			theTree += "(Datatype:"+oName1[0]+")";
			//System.out.print(" Datatype "+oName1[0]+", ");
			break;
		default:
			theTree += "(Other:"+oName1[0]+")";
			//System.out.print(" Other "+oName1[0]+", ");
			break;
		}
		oName1[0] = null;
		oType1[0] = -1;
	}
	theTree += "))";
	theTree2 += "((Group:"+gname2+")(";
	//System.out.println("");
	//System.out.print("File 2: Members of "+ gname2+": ");
	for ( i = 0; i < nelems2; i++) {
		try {
			ret2 = H5.H5Gget_obj_info_idx(pid2, gname2,i, oName2, oType2 );
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gn_members() Failed, exception: "+ex;
			return false;
		}  
		if (ret2 < 0)  {
			dbgInfo += "\n error\n";
			continue;
		}
		switch (oType2[0]) {
		case HDF5Constants.H5G_GROUP: 
			theTree2 += "(Group:"+oName2[0]+")";
			//System.out.println(" Group  "+ oName2[0]+", ");
			break;
		case HDF5Constants.H5G_DATASET: 
			theTree2 += "(Dataset:"+oName2[0]+")";
			//System.out.println(" Dataset "+ oName2[0]+", ");
			break;
		case HDF5Constants.H5G_TYPE: 
			theTree2 += "(Datatype:"+oName2[0]+")";
			//System.out.println(" Datatype "+ oName2[0]+", ");
			break;
		default:
			theTree2 += "(Other:"+oName2[0]+")";
			//System.out.println(" Other  "+ oName2[0]+", ");
			break;
		}
		oName2[0] = null;
		oType2[0] = -1;
	}
	theTree2 += "))";
	//System.out.println("\n");

	for ( i = 0; i < nelems1; i++) {
		try {
			ret1 = H5.H5Gget_obj_info_idx(pid1, gname1,i, oName1, oType1 );
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gn_members() Failed, exception: "+ex;
			return false;
		}  
		if (ret1 < 0)  {
			dbgInfo += "\n error\n";
			continue;
		}
		try {
			ret2 = H5.H5Gget_obj_info_idx(pid2, gname2,i, oName2, oType2 );
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gn_members() Failed, exception: "+ex;
			return false;
		}  
		if (ret2 < 0)  {
			dbgInfo += "\n error\n";
			continue;
		}
		if (oType1[0] != oType2[0]) {
			dbgInfo += "\n\nTypes don't match "+oName1[0]+" " +oType1[0]+" != " +oName2[0]+" " +oType2[0];

			return false;
		}
		switch (oType1[0]) {
		case HDF5Constants.H5G_GROUP: 
			//System.out.println("[1]"+oName1[0]+" [2]"+oName2[0]+": -->  ");
			try {
				pgroup1 = H5.H5Gopen(pid1,gname1);
			} catch (HDF5Exception ex) {
				dbgInfo += "\n\nH5Gopen() gname1 Failed, exception: "+ex;
				return false;
			}
			try {
				pgroup2 = H5.H5Gopen(pid2,gname2);
			} catch (HDF5Exception ex) {
				dbgInfo += "\n\nH5Gopen() gname2 Failed, exception: "+ex;
				return false;
			}
			if (!breadth_first_2( pgroup1, oName1[0], pgroup2, oName2[0])) {
				return false;
			}
			break;
		default:
			break;
		}
		oName1[0] = null;
		oType1[0] = -1;
		oName2[0] = null;
		oType2[0] = -1;
	}
	return true;
}


private boolean 
breadth_first( int pid1, String gname1 )
{
	int ret1;
	int pgroup1;
	int i;
	int nelems1 = 0;
	int [] oType1 = new int[1];
	String [] oName1 = new String[1];
	oName1[0] = new String(" ");          


	/* Iterate through the file to see members of the root group */
	nelems1 = 0;
	try {
		nelems1 = H5.H5Gn_members(pid1, gname1 );
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Gn_members() pid1 Failed group "+gname1+", exception: "+ex;
		return false;
	}                     
	if (nelems1 < 0 ) {
		dbgInfo += ("\n\nH5Gn_members: FAIL group: "+gname1);
		return false;
	}

	//System.out.println("\n");
	//System.out.print("Members of  "+ gname1+": ");
	theTree += "((Group:"+gname1+")(";
	for ( i = 0; i < nelems1; i++) {
		try {
			ret1 = H5.H5Gget_obj_info_idx(pid1, gname1,i, oName1, oType1 );
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gn_members() Failed, exception: "+ex;
			return false;
		}  
		if (ret1 < 0) {
			dbgInfo += "\n error\n";
			continue;
		}
		switch (oType1[0]) {
		case HDF5Constants.H5G_GROUP: 
			theTree += "(Group:"+oName1[0]+")";
			//System.out.print(" Group "+oName1[0]+", ");
			break;
		case HDF5Constants.H5G_DATASET: 
			theTree += "(Dataset:"+oName1[0]+")";
			//System.out.print(" Dataset "+oName1[0]+", ");
			break;
		case HDF5Constants.H5G_TYPE: 
			theTree += "(Datatype:"+oName1[0]+")";
			//System.out.print(" Datatype "+oName1[0]+", ");
			break;
		default:
			theTree += "(Other:"+oName1[0]+")";
			//System.out.print(" Other "+oName1[0]+", ");
			break;
		}
		oName1[0] = null;
		oType1[0] = -1;
	}
	theTree +="))";
	//System.out.println("\n");

	for ( i = 0; i < nelems1; i++) {
		try {
			ret1 = H5.H5Gget_obj_info_idx(pid1, gname1,i, oName1, oType1 );
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gn_members() Failed, exception: "+ex;
			return false;
		}  
		if (ret1 < 0)  {
			dbgInfo += "\n error\n";
			continue;
		}
		switch (oType1[0]) {
		case HDF5Constants.H5G_GROUP: 
			//System.out.println(oName1[0]+": -->  "+gname1);
			try {
				pgroup1 = H5.H5Gopen(pid1,gname1);
			} catch (HDF5Exception ex) {
				dbgInfo += "\n\nH5Gopen() gname1 Failed, exception: "+ex;
				return false;
			}
			if (!breadth_first( pgroup1, oName1[0] ) ) {
				return false;
			}
			break;
		default:
			break;
		}
		oName1[0] = null;
		oType1[0] = -1;
	}
	return true;
}


private boolean depth_first( int pid, String gname)
{
	int nelems = 0;
	int [] oType = new int[1];
	String [] oName = new String[1];
	oName[0] = new String(" ");          
	int i;
	int ret;
	int pgroup;

	/* Iterate through the file to see members of the root group */

	nelems = 0;
	try {
		nelems = H5.H5Gn_members(pid, gname);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Gn_members() pid Failed group "+gname+", exception: "+ex;
		return false;
	}                     
	if (nelems < 0 ) {
		dbgInfo += "\n\nH5Gn_members() pid Failed group "+gname;
		return false;
	}
	dbgInfo += "\ntotal of "+nelems+" elements in "+gname;

	theTree +="(";
	for ( i = 0; i < nelems; i++) {
		try {
			ret = H5.H5Gget_obj_info_idx(pid, gname,i, oName, oType );
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Gn_members() Failed, exception: "+ex;
			return false;
		}  
		dbgInfo += "element idx: "+i+" ";
		if (ret < 0)  {
			dbgInfo += "\n error\n";
			continue;
		}
		switch (oType[0]) {
		case HDF5Constants.H5G_GROUP: 
			theTree += "((Group:"+oName[0]+")";
			dbgInfo += "\nObject with name "+oName[0]+" is a group";
			try {
				pgroup = H5.H5Gopen(pid,gname);
			} catch (HDF5Exception ex) {
				dbgInfo += "\n\nH5Gopen() Failed, exception: "+ex;
				return false;
			}  
			if (!depth_first( pgroup, oName[0])) {
				return false;
			}
			theTree += ")";
			break;
		case HDF5Constants.H5G_DATASET: 
			theTree += "(Dataset:"+oName[0]+")";
			dbgInfo += "\n Object with name "+oName[0]+" is a dataset";
			break;
		case HDF5Constants.H5G_TYPE: 
			theTree += "(Datatype:"+oName[0]+")";
			dbgInfo += "\n Object with name "+oName[0]+" is a named datatype";
			break;
		default:
			theTree += "(Other:"+oName[0]+")";
			dbgInfo += "\n Object with name "+oName[0]+" is an unknown type "+oType[0];
			break;
		}
		oName[0] = null;
		oType[0] = -1;
	}
	theTree +=")";
	return true;
}

private boolean testIter1()
{
	int file;
	int ret;

	if (!genfile("iterate1.h5") ) {
		return false;
	}

	try {
		file = H5.H5Fopen("iterate1.h5",
			HDF5Constants.H5F_ACC_RDWR, 
			HDF5Constants.H5P_DEFAULT);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Fopen failed "+ex;
		return false;
	}

	theTree = new String("");
	boolean df = depth_first(file,"/");
	if (!df) {
		dbgInfo += "\ndepth first faield";
		return false;
	}
String correct = new String("((Dataset:Dataset1)(Datatype:Datatype1)((Group:Group1)(((Group:Group2)((Dataset:Dataset2)(Dataset:Dataset3)(Dataset:Dataset4)))((Group:Group3)((Dataset:Dataset5)(Dataset:Dataset6)))((Group:Group4)((Dataset:Dataset7)))))(Dataset:HD1)(Other:SD1)(Other:other))");
	if (!theTree.equals(correct)) {
		dbgInfo += "\nDepth first tree is not correct as expected";
		dbgInfo += "\nShould be "+correct;
		dbgInfo += "\nis "+theTree;
		return false;
	}
	theTree = new String("");
	boolean bf = breadth_first(file,"/");
	if (!bf) {
		dbgInfo += "\nbreadth first faield";
		return false;
	}
correct = new String("((Group:/)((Dataset:Dataset1)(Datatype:Datatype1)(Group:Group1)(Dataset:HD1)(Other:SD1)(Other:other)))((Group:Group1)((Group:Group2)(Group:Group3)(Group:Group4)))((Group:Group2)((Dataset:Dataset2)(Dataset:Dataset3)(Dataset:Dataset4)))((Group:Group3)((Dataset:Dataset5)(Dataset:Dataset6)))((Group:Group4)((Dataset:Dataset7)))");
	if (!theTree.equals(correct)) {
		dbgInfo += "\nBreadth first tree is not correct as expected";
		dbgInfo += "\nShould be "+correct;
		dbgInfo += "\nis "+theTree;
		return false;
	}

	try {
		ret = H5.H5Fclose(file);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Fopen failed "+ex;
		return false;
	}
	return true;
}

}
