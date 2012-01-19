/**
 *
 */
package test.unittests;

import java.util.*;
import javax.swing.tree.DefaultMutableTreeNode;
import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.object.HObject;
import ncsa.hdf.object.Group;
import ncsa.hdf.object.Dataset;
import ncsa.hdf.object.Datatype;
import ncsa.hdf.object.FileFormat;
import ncsa.hdf.object.h5.H5Datatype;
import ncsa.hdf.object.h5.H5File;
import ncsa.hdf.object.h5.H5ScalarDS;
import junit.framework.TestCase;

/**
 * TestCase for H5File.
 * <p>
 * This class tests all the public methods in H5ScalarDS class.
 * <p>
 * The test file contains the following objects.
 * <pre>
 *
        /dataset_byte            Dataset {50, 10}
        /dataset_comp            Dataset {50, 10}
        /dataset_enum            Dataset {50, 10}
        /dataset_float           Dataset {50, 10}
        /dataset_image           Dataset {50, 10}
        /dataset_int             Dataset {50, 10}
        /dataset_str             Dataset {50, 10}
        /g0                      Group
        /g0/dataset_comp         Dataset {50, 10}
        /g0/dataset_int          Dataset {50, 10}
        /g0/datatype_float       Type
        /g0/datatype_int         Type
        /g0/datatype_str         Type
        /g0/g00                  Group
        /g0/g00/dataset_float    Dataset {50, 10}
        /g0_attr                 Group
 * </pre>
 * <p>
 * We use the following template to test all the methods:
 * <p>
     * What to test:
     * <ul>
     *   <li> Test for boundary conditions
     *   <ul>
     *     <li>
     *   </ul>
     *   <li> Test for failure
     *   <ul>
     *     <li>
     *   </ul>
     *   <li> Test for success on general functionality
     *   <ul>
     *     <li>
     *   </ul>
     * </ul>
 *
 * @author Peter Cao, The HDF Group
 */
public class H5FileTest extends TestCase {
    private static final H5File H5FILE = new H5File();
    private static final int NLOOPS = 10;
    private static final int TEST_VALUE_INT = Integer.MAX_VALUE;
    private static final float TEST_VALUE_FLOAT = Float.MAX_VALUE;
    private static final String TEST_VALUE_STR = "H5ScalarDSTest";
    private static final String DNAME = H5TestFile.NAME_DATASET_INT;
    private static final String DNAME_SUB = H5TestFile.NAME_DATASET_INT_SUB;

    private H5Datatype typeInt = null;
    private H5Datatype typeFloat = null;
    private H5Datatype typeStr = null;
    private H5File testFile = null;
    private H5ScalarDS testDataset = null;

    /**
     * @param arg0
     */
    public H5FileTest(final String arg0) {
        super(arg0);
    }

    /* (non-Javadoc)
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();

        typeInt = new H5Datatype(Datatype.CLASS_INTEGER, H5TestFile.DATATYPE_SIZE, -1, -1);
        typeFloat = new H5Datatype(Datatype.CLASS_FLOAT, H5TestFile.DATATYPE_SIZE, -1, -1);
        typeStr = new H5Datatype(Datatype.CLASS_STRING, H5TestFile.STR_LEN, -1, -1);

        testFile = (H5File)H5FILE.open(H5TestFile.NAME_FILE_H5, FileFormat.WRITE);
        assertNotNull(testFile);

        testFile.open();

        testDataset = (H5ScalarDS)testFile.get(DNAME);
        assertNotNull(testDataset);
    }

    /* (non-Javadoc)
     * @see junit.framework.TestCase#tearDown()
     */
    protected void tearDown() throws Exception {
        super.tearDown();

        // make sure all objects are closed
        final int fid = testFile.getFID();
        if (fid > 0) {
            int nObjs = 0;

            try { nObjs = H5.H5Fget_obj_count(fid, 31); }
            catch (final Exception ex) { fail("H5.H5Fget_obj_count() failed. "+ ex);   }
            assertEquals(1, nObjs); // file id should be the only one left open
         }

        if (testFile != null) {
            try { testFile.close(); } catch (final Exception ex) {}
            testFile = null;
        }
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#open()}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> open a file identifier
     *     <li> check the file content
     *     <li> close the file
     *   </ul>
     */
    public final void testOpen() {
        try { testFile.close(); } catch (final Exception ex) {}

        for (int i=0; i<NLOOPS; i++)
        {
            int nObjs = 0;
            int fid = -1;
            final H5File file = new H5File(H5TestFile.NAME_FILE_H5, FileFormat.WRITE);

            try {
                fid = file.open(); // opent the full tree
            } catch (final Exception ex) {
                fail("file.open() failed. "+ ex);
            }
            assertTrue(fid>0);

            // try to get all object in the file
            try {
                 for (int j=0; j<H5TestFile.OBJ_NAMES.length; j++) {
                    assertNotNull(file.get(H5TestFile.OBJ_NAMES[j]));
                }
            } catch (final Exception ex) {
                 fail("file.get() failed. "+ ex);
            }

            try {
                nObjs = H5.H5Fget_obj_count(file.getFID(), HDF5Constants.H5F_OBJ_ALL);
            } catch (final Exception ex) {
                 fail("H5.H5Fget_obj_count() failed. "+ ex);
            }
            assertTrue(nObjs <=1); // file id should be the only this left open. IS THIS BECAUSE THE ONLY THING WE HAVE DONE IS OPEN THE FILE?

            try {
                file.close();
            } catch (final Exception ex) {
                 fail("file.close() failed. "+ ex);
            }
        } //for (int i=0; i<NLOOPS; i++)

        try { testFile.open(); } catch (final Exception ex) {}
     }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#create(java.lang.String)}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> create a file
     *     <li> access the file
     *     <li> close/delete the file
     *   </ul>
     */
    public final void testCreateString() {
        final String nameNew = "testH5File.h5";
        H5File file = null;

        try {
            file = (H5File)H5FILE.create(nameNew);
        } catch (final Exception ex) {
            fail("file.create() failed. " +ex);
        }

        int fid = -1;
        try {
            fid = file.open();
        } catch (final Exception ex) {
            fail("file.open() failed. " +ex);
        }
        assertTrue(fid > 0);

        try { file.close(); } catch (final Exception ex) {}
        file.delete();
    }


    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#getRootNode()}.
      * <p>
     * What to test:
     *   <ul>
     *     <li> get the root node
     *     <li> check the content of the root node
     *   </ul>
    */
    public final void testGetRootNode() {
        final javax.swing.tree.TreeNode root = testFile.getRootNode();
        assertNotNull(root);
        assertTrue(root.getChildCount()>0);
    }


    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#isReadOnly()}.
     */
    public final void testIsReadOnly() {
        assertFalse(testFile.isReadOnly());
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#createGroup(java.lang.String, ncsa.hdf.object.Group)}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> create a file
     *     <li> create a group
     *     <li> access the group
     *     <li> close/delete the file
     *   </ul>
     */
    public final void testCreateGroup() {
        final String nameNew = "testH5File.h5";
        H5File file = null;

        try {
            file = (H5File)H5FILE.create(nameNew);
        } catch (final Exception ex) {
            fail("file.create() failed. " +ex);
        }

        int fid = -1;
        try {
            fid = file.open();
        } catch (final Exception ex) {
            fail("file.open() failed. " +ex);
        }
        assertTrue(fid > 0);

        Group grp = null;
        try {
            grp = file.createGroup("new group", null);
        } catch (final Exception ex) {
            fail("file.createGroup() failed. " +ex);
        }
        assertNotNull(grp);

        int gid = -1;
        try {
            gid = grp.open();
        } catch (final Exception ex) {
            fail("fgrp.open() failed. " +ex);
        }
        assertTrue(gid > 0);
        grp.close(gid);

        try { file.close(); } catch (final Exception ex) {}
        file.delete();
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#createScalarDS(java.lang.String, ncsa.hdf.object.Group, ncsa.hdf.object.Datatype, long[], long[], long[], int, java.lang.Object)},
     * <br> {@link ncsa.hdf.object.h5.H5File#createCompoundDS(java.lang.String, ncsa.hdf.object.Group, long[], java.lang.String[], ncsa.hdf.object.Datatype[], int[], java.lang.Object)},
     * <br> {@link ncsa.hdf.object.h5.H5File#createCompoundDS(java.lang.String, ncsa.hdf.object.Group, long[], long[], long[], int, java.lang.String[], ncsa.hdf.object.Datatype[], int[], java.lang.Object)},
     * <br> {@link ncsa.hdf.object.h5.H5File#createImage(java.lang.String, ncsa.hdf.object.Group, ncsa.hdf.object.Datatype, long[], long[], long[], int, int, int, java.lang.Object)},
     * <br> {@link ncsa.hdf.object.h5.H5File#createDatatype(int, int, int, int)},
     * <br> {@link ncsa.hdf.object.h5.H5File#createDatatype(int, int, int, int, java.lang.String)},
     * <br> {@link ncsa.hdf.object.h5.H5File#createLink(ncsa.hdf.object.Group, java.lang.String, ncsa.hdf.object.HObject)},
     * <br> {@link ncsa.hdf.object.h5.H5File#get(java.lang.String)},
     * <br> {@link ncsa.hdf.object.h5.H5File#getAttribute(int)},
     * <br> {@link ncsa.hdf.object.h5.H5File#writeAttribute(ncsa.hdf.object.HObject, ncsa.hdf.object.Attribute, boolean)}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> create a file
     *     <li> create a different types of objects
     *     <li> access the objects
     *     <li> close/delete the new file
     *   </ul>
     */
    public final void testCreateObjects() {
        final String nameNew = "testH5File.h5";
        H5File file = null;

        try {
            file = H5TestFile.createTestFile(nameNew);
            file.open();
        } catch (final Exception ex) {
            fail("H5TestFile.createTestFile() failed. " +ex);
        }
        assertNotNull(file);

        // try to get all object in the file
        try {
             for (int j=0; j<H5TestFile.OBJ_NAMES.length; j++) {
                assertNotNull(file.get(H5TestFile.OBJ_NAMES[j]));
             }
        } catch (final Exception ex) {
             fail("file.get() failed. "+ ex);
        }

        int nObjs = 0;
        try {
            nObjs = H5.H5Fget_obj_count(file.getFID(), HDF5Constants.H5F_OBJ_ALL);
        } catch (final Exception ex) {
             fail("H5.H5Fget_obj_count() failed. "+ ex);
        }
        assertTrue(nObjs <=1); // file id should be the only this left open

        try {
            file.close();
        } catch (final Exception ex) {
             fail("file.close() failed. "+ ex);
        }

        file.delete();
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#isThisType(java.lang.String)}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> Check an HDF5 file
     *     <li> Check a non HDF5 file
     *   </ul>
     */
    public final void testIsThisTypeString() {
        assertTrue(H5FILE.isThisType(H5TestFile.NAME_FILE_H5));
        assertFalse(H5FILE.isThisType("No such file"));
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#isThisType(ncsa.hdf.object.FileFormat)}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> Check an HDF5 file
     *     <li> Check a non HDF5 file
     *   </ul>
     */
    public final void testIsThisTypeFileFormat() {
        assertTrue(H5FILE.isThisType(testFile));
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#copy(ncsa.hdf.object.HObject, ncsa.hdf.object.Group)}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> create a new file
     *     <li> copy all the objects (datasts, groups and datatypes) from test file to the new file
     *     <li> check the content of the new file
     *     <li> close/delete the new file
     *   </ul>
     */
    public final void testCopyHObjectGroup() {
        Group root = null;
        HObject srcObj=null, dstObj=null;
        final String nameNewFile = "testH5File.h5";
        String dstName=null;
        H5File file = null;

        try {
            root = (Group)testFile.get("/");
        } catch (final Exception ex) {
            fail("file.get() failed. "+ex);
        }
        assertNotNull(root);

        final List members = root.getMemberList();
        final int n = members.size();
        assertTrue(n>0);

        try {
            file = (H5File)H5FILE.create(nameNewFile);
            file.open();
        } catch (final Exception ex) {
            fail("file.create() failed. " +ex);
        }
        assertNotNull(file);

        try {
            root = (Group)file.get("/");
        } catch (final Exception ex) {
            fail("file.get() failed. "+ex);
        }
        assertNotNull(root);

        // copy all the objects to the new file
        for (int i=0; i<n; i++) {
            dstName = null;
            dstObj = null;
            srcObj = (HObject)members.get(i);

            try {
                dstObj = (HObject)((DefaultMutableTreeNode)testFile.copy(srcObj, root)).getUserObject();
            } catch (final Exception ex) {
                // image palette probably is copied already
                if (H5TestFile.NAME_DATASET_IMAGE_PALETTE.equals(srcObj.getFullName())) {
                    continue;
                }

                fail("file.copy() failed on "+srcObj.getFullName() + ". " + ex);
            }
            assertNotNull(dstObj);
            dstName = dstObj.getFullName();

            // re-open the file to make sure the object is writen to file
            try {
                file.close();
                file.open();
            } catch (final Exception ex) {
                 fail("file.close() failed. "+ ex);
            }

            try {
                dstObj = file.get(dstName);
            } catch (final Exception ex) {
                fail("file.get() failed on "+dstObj.getFullName() + ". "+ex);
            }
            assertNotNull(dstObj);
        }

        try {
            file.close();
        } catch (final Exception ex) {
             fail("file.close() failed. "+ ex);
        }

        file.delete();
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#delete(ncsa.hdf.object.HObject)}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> create a new file with all types of objects (datasts, groups and datatypes)
     *     <li> check the content of the new file
     *     <li> delete all objects
     *     <li> close/re-open the file to check the content of the file
     *     <li> close/delete the new file
     *   </ul>
     */
    public final void testDeleteHObject() {
        Group root = null;
        HObject obj=null;
        final String nameNewFile = "testH5File.h5";
        H5File file = null;

        try {
            file = H5TestFile.createTestFile(nameNewFile);
            file.open();
        } catch (final Exception ex) {
            fail("H5TestFile.createTestFile() failed. " +ex);
        }
        assertNotNull(file);

        try {
            root = (Group)file.get("/");
        } catch (final Exception ex) {
            fail("file.get() failed. "+ex);
        }
        assertNotNull(root);

        final List members = root.getMemberList();
        final int n = members.size();
        assertTrue(n>0);

        final Object[] objs = members.toArray();
        for (int i=0; i<n; i++) {
            obj = (HObject)objs[i];

            try {
                file.delete(obj);
            } catch (final Exception ex) {
                fail("file.copy() failed on "+obj.getFullName() + ". " + ex);
            }

            // re-open the file to make sure the object is writen to file
            try {
                file.close();
                file.open();
            } catch (final Exception ex) {
                 fail("file.close() failed. "+ ex);
            }

            try {
                obj = file.get(obj.getFullName());
            } catch (final Exception ex) {
                obj = null;
                ; // Expected to fail, intentional;
            }
            assertNull(obj);
        }

        try {
            file.close();
        } catch (final Exception ex) {
             fail("file.close() failed. "+ ex);
        }

        file.delete();
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#get(java.lang.String)}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> ceate a test file
     *     <li> do not call file.open() (without the full tree in memory)
     *     <li> get all types of objects (datasts, groups and datatypes)
     *     <li> get object that does not exitst in file
     *     <li> close and delete the test file
     *   </ul>
     */
    public final void testGet() {
        int nObjs = 0; // number of object left open
        HObject obj = null;

        final String nameNewFile = "testH5File.h5";
        H5File file = null;

        try {
            H5TestFile.createTestFile(nameNewFile);
        } catch (final Exception ex) {
            fail("H5TestFile.createTestFile() failed. " +ex);
        }

        file = new H5File(nameNewFile);

        // get object that does not exist in file
        try {
            obj = file.get("/_INVALID_OBJECT_PATH_SHOULD_RETURN_NULL_");
        } catch (final Exception ex) {
            fail("file.get() failed on invalid path. "+ex);
        }
        assertNull(obj);

        // get all object in file
        for (int i=0; i<H5TestFile.OBJ_NAMES.length; i++) {
            try {
                obj = file.get(H5TestFile.OBJ_NAMES[i]);
            } catch (final Exception ex) {
                 fail("file.get(\""+H5TestFile.OBJ_NAMES[i]+"\" failed. " +ex);
            }
            assertNotNull(obj);
        }

        try { nObjs = H5.H5Fget_obj_count(file.getFID(), HDF5Constants.H5F_OBJ_ALL); }
        catch (final Exception ex) { fail("H5.H5Fget_obj_count() failed. "+ ex);   }
        assertEquals(1, nObjs); // file id should be the only one left open

        try {
            file.close();
        } catch (final Exception ex) {
             fail("file.close() failed. "+ ex);
        }

        file.delete();
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#get(java.lang.String)}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> ceate a test file
     *     <li> call file.open() (with the full tree in memory)
     *     <li> get all types of objects (datasts, groups and datatypes)
     *     <li> get object that does not exitst in file
     *     <li> close and delete the test file
     *   </ul>
     */
    public final void testGetFromOpen() {
        int nObjs = 0; // number of object left open
        HObject obj = null;

        final String nameNewFile = "testH5File.h5";
        H5File file = null;

        try {
            H5TestFile.createTestFile(nameNewFile);
        } catch (final Exception ex) {
            fail("H5TestFile.createTestFile() failed. " +ex);
        }

        file = new H5File(nameNewFile);

        try {
            file.open();
        } catch (final Exception ex) {
            fail("file.open failed. "+ex);
        }

        // get object that does not exist in file
        try {
            obj = file.get("/_INVALID_OBJECT_PATH_SHOULD_RETURN_NULL_");
        } catch (final Exception ex) {
            fail("file.get() failed on invalid path. "+ex);
        }
        assertNull(obj);

        // get all object in file
        for (int i=0; i<H5TestFile.OBJ_NAMES.length; i++) {
            try {
                obj = file.get(H5TestFile.OBJ_NAMES[i]);
            } catch (final Exception ex) {
                 fail("file.get(\""+H5TestFile.OBJ_NAMES[i]+"\" failed. " +ex);
            }
            assertNotNull(obj);
        }

        try { nObjs = H5.H5Fget_obj_count(file.getFID(), HDF5Constants.H5F_OBJ_ALL); }
        catch (final Exception ex) { fail("H5.H5Fget_obj_count() failed. "+ ex);   }
        assertEquals(1, nObjs); // file id should be the only one left open

        try {
            file.close();
        } catch (final Exception ex) {
             fail("file.close() failed. "+ ex);
        }

        file.delete();
    }


    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#H5File(java.lang.String, int)}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> create files with READ, WRITE and CREATE opttions
     *     <li> check access permision of the files
     *     <li> close/delete the new file
     *   </ul>
     */
    public final void testH5FileStringInt() {
        Dataset dset=null;
        final String nameNewFile = "testH5File.h5";
        H5File file = null;

        try {
            file = H5TestFile.createTestFile(nameNewFile);
            file.open();
        } catch (final Exception ex) {
            fail("H5TestFile.createTestFile() failed. " +ex);
        }
        assertNotNull(file);
        try {
            file.close();
        } catch (final Exception ex) {
             fail("file.close() failed. "+ ex);
        }

        // make sure the file is read only
        try {
            file = new H5File(nameNewFile, FileFormat.READ);
            file.open();
        } catch (final Exception ex) {
            fail("new H5File(nameNewFile, H5File.READ) failed. " +ex);
        }
        assertTrue(file.isReadOnly());

        try {
            dset = (Dataset)file.get(H5TestFile.NAME_DATASET_FLOAT);
            dset.getData();
        }  catch (final Exception ex) {
             fail("file.get() failed. "+ ex);
        }
        assertNotNull(dset);

        boolean isWrittenFailed = false;
        try {
            dset.write();
         } catch (final Exception ex) {
             isWrittenFailed = true; // Expected.
         }
         assertTrue(isWrittenFailed);

        try {
            file.close();
        } catch (final Exception ex) {
             fail("file.close() failed. "+ ex);
        }

        // make sure the file is read/write
        try {
            file = new H5File(nameNewFile, FileFormat.WRITE);
            file.open();
        } catch (final Exception ex) {
            fail("new H5File(nameNewFile, H5File.READ) failed. " +ex);
        }

        try {
            dset = (Dataset)file.get(H5TestFile.NAME_DATASET_FLOAT);
            dset.getData();
        }  catch (final Exception ex) {
             fail("file.get() failed. "+ ex);
        }
        assertNotNull(dset);

        try {
            dset.write();
         } catch (final Exception ex) {
             fail("file.write() failed. "+ ex);
         }

        try {
            file.close();
        } catch (final Exception ex) {
             fail("file.close() failed. "+ ex);
        }

        // create a new file
        try {
            file = new H5File(nameNewFile, FileFormat.CREATE);
            file.open();
        } catch (final Exception ex) {
            fail("new H5File(nameNewFile, H5File.READ) failed. " +ex);
        }

        try {
            dset = (Dataset)file.get(H5TestFile.NAME_DATASET_FLOAT);
            dset.getData();
        }  catch (final Exception ex) {
             ; // Expected. The file is empty.
        }

        try {
            file.close();
        } catch (final Exception ex) {
             fail("file.close() failed. "+ ex);
        }

        file.delete();
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#open(int)}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> open a file with H5F_CLOSE_STRONG file access
     *     <li> check the file content
     *     <li> close the file
     *   </ul>
     */
    public final void testOpenInt() {
        try { testFile.close(); } catch (final Exception ex) {}

        int nObjs = 0;
        int plist=-1;;

        final H5File file = new H5File(H5TestFile.NAME_FILE_H5, FileFormat.WRITE);

        try {
            plist = H5.H5Pcreate (HDF5Constants.H5P_FILE_ACCESS);
            H5.H5Pset_fclose_degree ( plist, HDF5Constants.H5F_CLOSE_STRONG);
        } catch (final Exception ex) {
            fail("H5.H5Pcreate() failed. "+ ex);
        }

        try {
            file.open(plist); // opent the full tree
        } catch (final Exception ex) {
            fail("file.open() failed. "+ ex);
        }
        try { H5.H5Pclose(plist); } catch (final Exception ex) {}

        // try to get all object in the file
        try {
             for (int j=0; j<H5TestFile.OBJ_NAMES.length; j++) {
                assertNotNull(file.get(H5TestFile.OBJ_NAMES[j]));
            }
        } catch (final Exception ex) {
             fail("file.get() failed. "+ ex);
        }

        try {
            nObjs = H5.H5Fget_obj_count(file.getFID(), HDF5Constants.H5F_OBJ_ALL);
        } catch (final Exception ex) {
             fail("H5.H5Fget_obj_count() failed. "+ ex);
        }

        try {
            file.close();
        } catch (final Exception ex) {
             fail("file.close() failed. "+ ex);
        }

        assertTrue(nObjs <=1); // file id should be the only this left open

        try { testFile.open(); } catch (final Exception ex) {}
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#updateReferenceDataset(ncsa.hdf.object.h5.H5File, ncsa.hdf.object.h5.H5File)}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> create a new file
     *     <li> copy a reference dataset from the test file to the new file
     *     <li> check the content of the dataset in the new file
     *     <li> close/delete the new file
     *   </ul>
     */
    public final void testUpdateReferenceDataset() {
        Group root = null;
        HObject srcObj=null, dstObj=null;
        final String nameNewFile = "testH5File.h5";
        String dstName=null;
        H5File file = null;

        try {
            root = (Group)testFile.get("/");
        } catch (final Exception ex) {
            fail("file.get() failed. "+ex);
        }
        assertNotNull(root);

        final List members = root.getMemberList();
        final int n = members.size();
        assertTrue(n>0);

        try {
            file = (H5File)H5FILE.create(nameNewFile);
            file.open();
        } catch (final Exception ex) {
            fail("file.create() failed. " +ex);
        }
        assertNotNull(file);

        try {
            root = (Group)file.get("/");
        } catch (final Exception ex) {
            fail("file.get() failed. "+ex);
        }
        assertNotNull(root);

        // copy all the objects to the new file
        for (int i=0; i<n; i++) {
            dstName = null;
            dstObj = null;
            srcObj = (HObject)members.get(i);

            try {
                dstObj = (HObject)((DefaultMutableTreeNode)testFile.copy(srcObj, root)).getUserObject();
            } catch (final Exception ex) {
                // image palette probably is copied already
                if (H5TestFile.NAME_DATASET_IMAGE_PALETTE.equals(srcObj.getFullName())) {
                    continue;
                }

                fail("file.copy() failed on "+srcObj.getFullName() + ". " + ex);
            }
            assertNotNull(dstObj);
            dstName = dstObj.getFullName();

            // re-open the file to make sure the object is writen to file
            try {
                file.close();
                file.open();
            } catch (final Exception ex) {
                 fail("file.close() failed. "+ ex);
            }

            try {
                dstObj = file.get(dstName);
            } catch (final Exception ex) {
                fail("file.get() failed on "+dstObj.getFullName() + ". "+ex);
            }
            assertNotNull(dstObj);
        }

        try {
            H5File.updateReferenceDataset(testFile, file);
        } catch (final Exception ex) {
            fail("H5File.updateReferenceDataset() failed. "+ ex);
        }

        long[] refs = null;
        try {
            refs = (long[]) ((Dataset)file.get(H5TestFile.NAME_DATASET_OBJ_REF)).getData();
        } catch (final Exception ex) {
            fail("file.get() failed. "+ ex);
        }
        assertNotNull(refs);

        // check the references are updated correctly
        long[] oid = null;
        HObject obj = null;
        for (int i=0; i<5; i++) {
            try {
                obj = file.get(H5TestFile.OBJ_NAMES[i]);
                oid = obj.getOID();
            } catch (final Exception ex) {
                fail("file.get() failed. "+ ex);
            }
            assertEquals(oid[0], refs[i]);
        }

        try {
            file.close();
        } catch (final Exception ex) {
             fail("file.close() failed. "+ ex);
        }

        file.delete();
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5File#createImageAttributes(ncsa.hdf.object.Dataset, int)}.
     */
    public final void testCreateImageAttributes() {
        H5ScalarDS img = null;

        try {
            img = (H5ScalarDS)testFile.get(H5TestFile.NAME_DATASET_IMAGE);
        } catch (final Exception ex) {
            fail("file.get() failed. "+ex);
        }
        assertNotNull(img);
        assertTrue(img.hasAttribute());
        assertTrue(img.isImage());
    }

}
