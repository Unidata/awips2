
/****************************************************************************
 * NCSA HDF                                                                 *
 * National Comptational Science Alliance                                   *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf-java/COPYING file.                                                   *
 *                                                                          *
 ****************************************************************************/

package test.object;

import javax.swing.tree.*;
import java.util.*;
import java.lang.reflect.*;
import ncsa.hdf.object.*;
import ncsa.hdf.object.h5.*;
import ncsa.hdf.hdf5lib.*;

/**
 * Test object at the ncsa.hdf.object package.
 * <p>
 *
 * @version 1.3.0 10/26/2001
 * @author Peter X. Cao
 *
 */
public class TestH5File
{
    /**
     * Test tree structure of the HDF5 file.
     * <p>
     * Tested with a large file (over 700MB, over 800 datasets) at
     * \\Eirene\sdt\mcgrath\EOS-Data\MODIS\L3\MOD08_E3.A2000337.002.2001037044240.h5
     * it takes about 5 seconds to retrieve the tree structure through the network.
     * Accessing local file can be a lot of faster.
     */
    private static void testTree(String fileName)
    {
        H5File h5file = new H5File(fileName, HDF5Constants.H5F_ACC_RDONLY);

        long t0 = System.currentTimeMillis();

        try {
            h5file.open();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }

        long t = System.currentTimeMillis()-t0;
        System.out.println("Time of retrieving the tree is "+t);

        TreeNode root = h5file.getRootNode();
        if (root != null)
        {
            printNode(root, "    ");
        }

	try {
        h5file.close();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }
    }

    private static void printNode(TreeNode node, String indent)
    {
        System.out.println(indent+node);

        int n = node.getChildCount();
        for (int i=0; i<n; i++)
        {
            printNode(node.getChildAt(i), indent+"    ");
        }
    }

    /**
     * Test H5CompoundDS.
     */
    private static void testH5CompoundDS(String fileName)
    {
        H5File h5file = new H5File(fileName, HDF5Constants.H5F_ACC_RDONLY);

        try {
            h5file.open();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }

        DefaultMutableTreeNode root = (DefaultMutableTreeNode)h5file.getRootNode();
        H5CompoundDS h5DS = null;
        DefaultMutableTreeNode node = null;
        if (root != null)
        {
            Enumeration nodes = root.depthFirstEnumeration();
            while (nodes.hasMoreElements())
            {
                node = (DefaultMutableTreeNode)nodes.nextElement();
                Object obj = node.getUserObject();
                if (obj instanceof H5CompoundDS)
                {
                    h5DS = (H5CompoundDS)obj;
                    System.out.println(h5DS);

                    // test H5CompoundDS attributes
                    Attribute attr = null;
                    List info = null;
                    try { info = h5DS.getMetadata(); }
                    catch (Exception ex) { System.out.println(ex); }

                    int n = 0;
                    if (info != null)
                    {
                        n = info.size();
                        for (int i=0; i<n; i++)
                        {
                            attr = (Attribute)info.get(i);
                            System.out.println(attr);
                        }
                    }

                    // compound members
                    int rank = h5DS.getRank();
                    if (rank <=0 ) {
                        h5DS.init();
                    }
                    n = h5DS.getMemberCount();
                    String[] names = h5DS.getMemberNames();
                    for (int i=0; i<n; i++)
                    {
                        System.out.println(names[i]);
                    }

                    // compound data
                    List list = null;

                    try {
                        list = (List)h5DS.read();
                    } catch (Exception ex) { System.out.println(ex);}

                    if (list != null)
                    {
                        n = list.size();
                        Object mdata = null;
                        for (int i=0; i<n; i++)
                        {
                            mdata = list.get(i);
                            if (mdata.getClass().isArray())
                            {
                                StringBuffer sb = new StringBuffer();
                                // print out the first 1000 data points
                                int mn = Math.min(Array.getLength(mdata), 1000);
                                for (int j=0; j<mn; j++)
                                {
                                    sb.append(Array.get(mdata, j));
                                    sb.append(" ");
                                }
                                System.out.println(sb.toString());
                            }
                        } // for (int i=0; i<n; i++)
                    } // if (list != null)
                } //if (obj instanceof H5CompoundDS
            } //while (nodes.hasMoreElements())
        } //if (root != null)

	try {
        h5file.close();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }
    }

    /**
     * Test H5ScalarDS.
     */
    private static void testH5ScalarDS(String fileName)
    {
        H5File h5file = new H5File(fileName, HDF5Constants.H5F_ACC_RDONLY);

        try {
            h5file.open();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }

        DefaultMutableTreeNode root = (DefaultMutableTreeNode)h5file.getRootNode();
        H5ScalarDS h5DS = null;
        DefaultMutableTreeNode node = null;
        if (root != null)
        {
            Enumeration nodes = root.depthFirstEnumeration();
            while (nodes.hasMoreElements())
            {
                node = (DefaultMutableTreeNode)nodes.nextElement();
                Object obj = node.getUserObject();
                if (obj instanceof H5ScalarDS)
                {
                    h5DS = (H5ScalarDS)obj;
                    System.out.println(h5DS);

                    // test H5CompoundDS attributes
                    Attribute attr = null;
                    List info = null;

                    try { info = h5DS.getMetadata(); }
                    catch (Exception ex) {System.out.println(ex);}

                    int n = 0;
                    if (info != null)
                    {
                        n = info.size();
                        for (int i=0; i<n; i++)
                        {
                            attr = (Attribute)info.get(i);
                            System.out.println(attr);
                        }
                    }

                    // data
                    Object data = null;
                    try
                    {
                        data = h5DS.read();
                    } catch (Exception ex) {}

                    if ((data != null) && data.getClass().isArray())
                    {
                        // print out the first 1000 data points
                        n = Math.min(Array.getLength(data), 1000);
                        StringBuffer sb = new StringBuffer();
                        for (int j=0; j<n; j++)
                        {
                            sb.append(Array.get(data, j));
                            sb.append(" ");
                        }
                        System.out.println(sb.toString());
                    }
                } //if (obj instanceof H5CompoundDS
            } //while (nodes.hasMoreElements())
        } //if (root != null)

	try {
        h5file.close();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }
    }

    /**
     * Test H5Group.
     */
    private static void testH5Group(String fileName)
    {
        H5File h5file = new H5File(fileName, HDF5Constants.H5F_ACC_RDONLY);

        try {
            h5file.open();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }

        DefaultMutableTreeNode root = (DefaultMutableTreeNode)h5file.getRootNode();
        H5Group g = null;
        DefaultMutableTreeNode node = null;
        if (root != null)
        {
            Enumeration nodes = root.depthFirstEnumeration();
            while (nodes.hasMoreElements())
            {
                node = (DefaultMutableTreeNode)nodes.nextElement();
                Object obj = node.getUserObject();
                if (obj instanceof H5Group)
                {
                    g = (H5Group)obj;
                    System.out.println(g);

                    // test H5CompoundDS attributes
                    Attribute attr = null;
                    List info = null;
                    try { g.getMetadata(); }
                    catch (Exception ex) { System.out.println(ex); }

                    if (info == null) {
                        continue;
                    }

                    int n = info.size();
                    for (int i=0; i<n; i++)
                    {
                        attr = (Attribute)info.get(i);
                        System.out.println(attr);
                    }
                } //if (obj instanceof H5Group
            } //while (nodes.hasMoreElements())
        } //if (root != null)

	try {
        h5file.close();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }
    }

    public static void main(String[] argv)
    {
        int argc = argv.length;

        if (argc <=0)
        {
            System.exit(1);
        }

	System.out.println("Tree: for: "+argv[0]);
        TestH5File.testTree(argv[0]);
    }

}
