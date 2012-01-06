
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
import ncsa.hdf.object.h4.*;
import ncsa.hdf.hdflib.*;

/**
 * Test object at the ncsa.hdf.object package.
 * <p>
 *
 * @version 1.3.0 10/26/2001
 * @author Peter X. Cao
 *
 */
public class TestH4File
{
    /**
     * Test tree structure of the HDF4 file.
     * <p>
     * Tested for regular file:
     * c:\winnt\profiles\xcao\desktop\hdf_files\amortest000171999.hdf
     * Tested with a large file (over 700MB, over 800 datasets) at
     * \\Eirene\sdt\mcgrath\EOS-Data\MODIS\L3\MOD08_E3.A2000337.002.2001037044240.hdf
     * it takes about 5 seconds to retrieve the tree structure through the network.
     * Accessing local file can be a lot of faster.
     */
    private static void testTree(String fileName)
    {
        H4File h4file = new H4File(fileName, HDFConstants.DFACC_WRITE);

        long t0 = System.currentTimeMillis();

        try {
            h4file.open();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }
        long t = System.currentTimeMillis()-t0;
        System.out.println("Time of retrieving the tree is "+t);

        TreeNode root = h4file.getRootNode();
        if (root != null)
        {
            printNode(root, "    ");
        }

        try {
        h4file.close();
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
     * Test H4Group.
     */
    private static void testH4Group(String fileName)
    {
        H4File h4file = new H4File(fileName, HDFConstants.DFACC_WRITE);

        try {
            h4file.open();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }

        DefaultMutableTreeNode root = (DefaultMutableTreeNode)h4file.getRootNode();
        H4Group g = null;
        DefaultMutableTreeNode node = null;
        if (root != null)
        {
            Enumeration nodes = root.depthFirstEnumeration();
            while (nodes.hasMoreElements())
            {
                node = (DefaultMutableTreeNode)nodes.nextElement();
                Object obj = node.getUserObject();
                if (obj instanceof H4Group)
                {
                    g = (H4Group)obj;
                    System.out.println(g);

                    // test H4CompoundDS attributes
                    Attribute attr = null;
                    List info = null;
                    try { info = g.getMetadata(); }
                    catch (Exception ex) {}

                    if (info == null) {
                        continue;
                    }

                    int n = info.size();
                    for (int i=0; i<n; i++)
                    {
                        attr = (Attribute)info.get(i);
                        System.out.println(attr);
                    }
                } //if (obj instanceof H4Group
            } //while (nodes.hasMoreElements())
        } //if (root != null)

	try {
        h4file.close();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }
    }

    /**
     * Test H4SDS.
     */
    private static void testH4SDS(String fileName)
    {
        H4File h4file = new H4File(fileName, HDFConstants.DFACC_READ);

        try {
            h4file.open();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }

        DefaultMutableTreeNode root = (DefaultMutableTreeNode)h4file.getRootNode();
        H4SDS sds = null;
        DefaultMutableTreeNode node = null;
        if (root != null)
        {
            Enumeration nodes = root.depthFirstEnumeration();
            while (nodes.hasMoreElements())
            {
                node = (DefaultMutableTreeNode)nodes.nextElement();
                Object obj = node.getUserObject();
                if (obj instanceof H4SDS)
                {
                    sds = (H4SDS)obj;
                    System.out.println(sds);

                    // test H4CompoundDS attributes
                    Attribute attr = null;
                    List info = null;
                    try {
                        info = sds.getMetadata();
                    } catch (Exception ex)
                    { System.out.println(ex); }

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
                        data = sds.read();
                    } catch (Exception ex) {System.out.println(ex);}

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
                } //if (obj instanceof H4Group
            } //while (nodes.hasMoreElements())
        } //if (root != null)

	try {
        h4file.close();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }
    }

    /**
     * Test H4Vdata.
     */
    private static void testH4Vdata(String fileName)
    {
        H4File h4file = new H4File(fileName, HDFConstants.DFACC_READ);

        try {
            h4file.open();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }

        DefaultMutableTreeNode root = (DefaultMutableTreeNode)h4file.getRootNode();
        H4Vdata  vdata = null;
        DefaultMutableTreeNode node = null;
        if (root != null)
        {
            Enumeration nodes = root.depthFirstEnumeration();
            while (nodes.hasMoreElements())
            {
                node = (DefaultMutableTreeNode)nodes.nextElement();
                Object obj = node.getUserObject();
                if (obj instanceof H4Vdata)
                {
                    vdata = (H4Vdata)obj;
                    System.out.println(vdata);

                    // test H4CompoundDS attributes
                    Attribute attr = null;
                    List info = null;
                    try {
                        info = vdata.getMetadata();
                    } catch (Exception ex)
                    { System.out.println(ex); }

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
                    int rank = vdata.getRank();
                    if (rank <=0 ) {
                        vdata.init();
                    }
                    n = vdata.getMemberCount();
                    String[] names = vdata.getMemberNames();
                    for (int i=0; i<n; i++)
                    {
                        System.out.println(names[i]);
                    }

                    // compound data
                    List list = null;

                    try {
                        list = (List)vdata.read();
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
               } //if (obj instanceof H4Vdata
            } //while (nodes.hasMoreElements())
        } //if (root != null)

	try {
        h4file.close();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }
    }

    /**
     * Test H4GRImage.
     */
    private static void testH4GRImage(String fileName)
    {
        H4File h4file = new H4File(fileName, HDFConstants.DFACC_READ);

        try {
            h4file.open();
        } catch (Exception ex)
        {
            System.out.println(ex);
        }

        DefaultMutableTreeNode root = (DefaultMutableTreeNode)h4file.getRootNode();
        H4GRImage sds = null;
        DefaultMutableTreeNode node = null;
        if (root != null)
        {
            Enumeration nodes = root.depthFirstEnumeration();
            while (nodes.hasMoreElements())
            {
                node = (DefaultMutableTreeNode)nodes.nextElement();
                Object obj = node.getUserObject();
                if (obj instanceof H4GRImage)
                {
                    sds = (H4GRImage)obj;
                    System.out.println(sds);

                    // test H4CompoundDS attributes
                    Attribute attr = null;
                    List info = null;
                    try {
                        info = sds.getMetadata();
                    } catch (Exception ex)
                    { System.out.println(ex); }

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
                        data = sds.read();
                    } catch (Exception ex) {System.out.println(ex);}

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
                } //if (obj instanceof H4Group
            } //while (nodes.hasMoreElements())
        } //if (root != null)

	try {
        h4file.close();
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

        TestH4File.testTree(argv[0]);
    }

}
