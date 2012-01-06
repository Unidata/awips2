/*****************************************************************************
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of the HDF Java Products distribution.                  *
 * The full copyright notice, including terms governing use, modification,   *
 * and redistribution, is contained in the files COPYING and Copyright.html. *
 * COPYING can be found at the root of the source code distribution tree.    *
 * Or, see http://hdfgroup.org/products/hdf-java/doc/Copyright.html.         *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 ****************************************************************************/

package ncsa.hdf.view;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.html.*;
import javax.swing.tree.DefaultMutableTreeNode;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.Dimension;
import java.awt.Dialog;
import java.util.*;
import java.awt.event.*;
import ncsa.hdf.object.*;

import java.net.URL;
import java.net.URLClassLoader;

/** NewAttributeDialog displays components for adding new attribute. 
 * 
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
*/
public class NewAttributeDialog extends JDialog
implements ActionListener, ItemListener, HyperlinkListener
{
	public static final long serialVersionUID = HObject.serialVersionUID;

    /** the default length of a string attribute */
    public static final int DEFAULT_STRING_ATTRIBUTE_LENGTH = 256;

    /** the object which the attribute to be attached to */
    private HObject hObject;

    private Attribute newAttribute;

    /** TextField for entering the name of the dataset */
    private JTextField nameField;

    /** The Choice of the datatypes */
    private JComboBox typeChoice;

    /** TextField for entering the attribute value. */
    private JTextField valueField;
    
    /** The Choice of the boject list */
    private JComboBox objChoice;

    private FileFormat fileFormat;

    /** TextField for entering the length of the data array or string. */
    private JTextField lengthField;

    private JLabel arrayLengthLabel;

    /** flag to indicate if the dataset is created */
    private boolean isAttributeCreated;

    private JScrollPane objListScroller;

    private final boolean isH5;

    private JDialog helpDialog;

    private JRadioButton  h4SdAttrRadioButton;
    private JRadioButton  h4GrAttrRadioButton;

    /** Constructs NewAttributeDialog with specified object (dataset, group, or
     *  image) which the new attribute to be attached to.
     *  @param owner the owner of the input
     *  @param obj the object which the attribute to be attached to.
     */
    public NewAttributeDialog(Dialog owner, HObject obj, Enumeration objList)
    {
        super (owner, "New Attribute...", true);

        hObject = obj;
        newAttribute = null;
        isH5 = obj.getFileFormat().isThisType(FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5));
        helpDialog = null;
        fileFormat = obj.getFileFormat();

        typeChoice = new JComboBox();
        typeChoice.addItem("string");
        typeChoice.addItem("byte (8-bit)");
        typeChoice.addItem("short (16-bit)");
        typeChoice.addItem("int (32-bit)");
        typeChoice.addItem("unsigned byte (8-bit)");
        typeChoice.addItem("unsigned short (16-bit)");
        typeChoice.addItem("unsigned int (32-bit)");
        typeChoice.addItem("long (64-bit)");
        typeChoice.addItem("float");
        typeChoice.addItem("double");
        if (isH5) {
            typeChoice.addItem("object reference");
        }

        JPanel contentPane = (JPanel)getContentPane();
        contentPane.setLayout(new BorderLayout(5,5));
        contentPane.setBorder(BorderFactory.createEmptyBorder(20,10,0,10));
        int w = 400 + (ViewProperties.getFontSize()-12)*15;
        int h = 180 + (ViewProperties.getFontSize()-12)*10;
        contentPane.setPreferredSize(new Dimension(w, h));

        JButton okButton = new JButton("   Ok   ");
        okButton.setActionCommand("Ok");
        okButton.setMnemonic(KeyEvent.VK_O);

        JButton cancelButton = new JButton("Cancel");
        cancelButton.setActionCommand("Cancel");
        cancelButton.setMnemonic(KeyEvent.VK_C);

        JButton helpButton = new JButton(" Help ");
        helpButton.setActionCommand("Show help");
        helpButton.setMnemonic(KeyEvent.VK_H);

        JPanel p = new JPanel();
        p.setLayout(new BorderLayout(5,5));
        JPanel p2 = new JPanel();
        p2.setLayout(new GridLayout(5,1,3,3));
        p2.add(new JLabel("Name: "));
        p2.add(new JLabel("Type: "));
        p2.add(arrayLengthLabel= new JLabel("Max String Length: "));
        p2.add(new JLabel("Value: "));
        p2.add(new JLabel("Object List: "));
        p.add("West", p2);

        JPanel typePane = new JPanel();
        typePane.setLayout(new BorderLayout());
        JPanel h4GattrPane = new JPanel();
        h4GattrPane.setLayout(new GridLayout(1,2,3,3));
        ButtonGroup bg = new ButtonGroup();
        JRadioButton grAttr = new JRadioButton("GR");
        JRadioButton sdAttr = new JRadioButton("SD");
        bg.add(sdAttr);
        bg.add(grAttr);
        sdAttr.setSelected(true);
        h4GattrPane.add(sdAttr);
        h4GattrPane.add(grAttr);
        typePane.add(typeChoice, BorderLayout.CENTER);
        typePane.add(h4GattrPane, BorderLayout.EAST);
        h4GrAttrRadioButton = grAttr;
        h4SdAttrRadioButton = sdAttr;

        p2 = new JPanel();
        p2.setLayout(new GridLayout(5,1,3,3));
        p2.add(nameField=new JTextField("",30));
        if (!isH5 && (obj instanceof Group) && ((Group)obj).isRoot()) {
            p2.add(typePane);
        } else {
            p2.add(typeChoice);
        }
        p2.add(lengthField=new JTextField("1"));
        p2.add(valueField=new JTextField("0"));
        p2.add(objChoice=new JComboBox());
        p.add("Center", p2);

        contentPane.add("Center", p);

        p = new JPanel();
        p.add(okButton);
        p.add(cancelButton);
        p.add(helpButton);
        contentPane.add("South", p);

        typeChoice.addItemListener(this);
        okButton.addActionListener(this);
        cancelButton.addActionListener(this);
        helpButton.addActionListener(this);
        objChoice.addItemListener(this);
        objChoice.setEnabled(false);
        
        String str;
        HObject hobj;
        DefaultMutableTreeNode theNode;
        while (objList.hasMoreElements()) {
            theNode = (DefaultMutableTreeNode)objList.nextElement();
            hobj = (HObject)theNode.getUserObject();
            if (hobj instanceof Group) {
                if ( ((Group)hobj).isRoot()) 
                    continue;
            }
            str = hobj.getFullName();
            objChoice.addItem(str);
        }

        Point l = owner.getLocation();
        l.x += 50;
        l.y += 80;
        setLocation(l);
        pack();
    }

    public void actionPerformed(ActionEvent e)
    {
        Object source = e.getSource();
        String cmd = e.getActionCommand();

        if (cmd.equals("Ok"))
        {
            if (createAttribute()) {
                dispose();
            }
        }
        else if (cmd.equals("Cancel"))
        {
            newAttribute= null;
            dispose();
        }
        else if (cmd.equals("Show help"))
        {
            if (helpDialog == null) {
                createHelpDialog();
            }
            helpDialog.setVisible(true);
        }
        else if (cmd.equals("Hide help"))
        {
            if (helpDialog != null) {
                helpDialog.setVisible(false);
            }
        }
    }

    public void itemStateChanged(ItemEvent e)
    {
        Object source = e.getSource();

        if (source.equals(typeChoice))
        {
            int idx = typeChoice.getSelectedIndex();
            objChoice.setEnabled(false);
            lengthField.setEnabled(true);

            if (idx == 0) {
                arrayLengthLabel.setText("Max String Length: ");
            } else if (typeChoice.getSelectedItem().equals("object reference")){
                lengthField.setText("1");
                lengthField.setEnabled(false);
                arrayLengthLabel.setText("Array Size: ");
                objChoice.setEnabled(true);
                valueField.setText((String)objChoice.getSelectedItem());
            } else {
                arrayLengthLabel.setText("Array Size: ");
            }
        } else if (source.equals(objChoice))
        {
            valueField.setText((String)objChoice.getSelectedItem());
        }
    }

    private boolean createAttribute()
    {
        int string_length = 0;
        int tclass=-1, tsize=-1, torder=-1, tsign=-1;

        Object value = null;
        String dt = (String)typeChoice.getSelectedItem();
        String strValue = valueField.getText();

        String attrName = nameField.getText();
        if (attrName != null) {
            attrName = attrName.trim();
        }

        if ((attrName == null) || (attrName.length()<1))
        {
            JOptionPane.showMessageDialog(this,
                "No attribute name.",
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return false;
        }

        String lengthStr = lengthField.getText();

        int arraySize = 0;
        if ((lengthStr == null) || (lengthStr.length() <=0))
        {
            arraySize = 1;
        }
        else
        {
            try { arraySize = Integer.parseInt(lengthStr); }
            catch (Exception e) { arraySize  = -1; }
        }

        if (arraySize <=0 )
        {
            JOptionPane.showMessageDialog(this,
                "Invalid attribute length.",
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return false;
        }

        StringTokenizer st = new StringTokenizer(strValue, ",");
        int count = Math.min(arraySize, st.countTokens());
        String theToken;

        if (dt.startsWith("byte"))
        {
            byte[] b = new byte[arraySize];
            for (int j=0; j<count; j++)
            {
                theToken = st.nextToken().trim();
                try { b[j] = Byte.parseByte(theToken); }
                catch (NumberFormatException ex)
                {
                    JOptionPane.showMessageDialog(this,
                        ex.getMessage(),
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
            value = b;
            tclass = Datatype.CLASS_INTEGER;
            tsize = 1;
            torder = Datatype.NATIVE;
        }
        else if (dt.startsWith("short"))
        {
            short[] s = new short[arraySize];

            for (int j=0; j<count; j++)
            {
                theToken = st.nextToken().trim();
                try { s[j] = Short.parseShort(theToken); }
                catch (NumberFormatException ex)
                {
                    JOptionPane.showMessageDialog(this,
                        ex.getMessage(),
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
            value = s;
            tclass = Datatype.CLASS_INTEGER;
            tsize = 2;
            torder = Datatype.NATIVE;
        }
        else if (dt.startsWith("int"))
        {
            int[] i = new int[arraySize];

            for (int j=0; j<count; j++)
            {
                theToken = st.nextToken().trim();
                try { i[j] = Integer.parseInt(theToken); }
                catch (NumberFormatException ex)
                {
                    JOptionPane.showMessageDialog(this,
                        ex.getMessage(),
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
            value = i;
            tclass = Datatype.CLASS_INTEGER;
            tsize = 4;
            torder = Datatype.NATIVE;
        }
        else if (dt.startsWith("unsigned byte"))
        {
            byte[] b = new byte[arraySize];
            short sv = 0;
            for (int j=0; j<count; j++)
            {
                theToken = st.nextToken().trim();
                try { sv = Short.parseShort(theToken); }
                catch (NumberFormatException ex)
                {
                    JOptionPane.showMessageDialog(this,
                        ex.getMessage(),
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return false;
                }
                if (sv <0 ) {
                    sv = 0;
                } else if ( sv > 255) {
                    sv = 255;
                }
                b[j] = (byte) sv;
            }
            value = b;

            tclass = Datatype.CLASS_INTEGER;
            tsize = 1;
            torder = Datatype.NATIVE;
            tsign = Datatype.SIGN_NONE;
        }
        else if (dt.startsWith("unsigned short"))
        {
            short[] s = new short[arraySize];
            int iv = 0;
            for (int j=0; j<count; j++)
            {
                theToken = st.nextToken().trim();
                try { iv = Integer.parseInt(theToken); }
                catch (NumberFormatException ex)
                {
                    JOptionPane.showMessageDialog(this,
                        ex.getMessage(),
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return false;
                }
                if (iv <0 ) {
                    iv = 0;
                } else if ( iv > 65535) {
                    iv = 65535;
                }
                s[j] = (short)iv;
            }
            value = s;
            tclass = Datatype.CLASS_INTEGER;
            tsize = 2;
            torder = Datatype.NATIVE;
            tsign = Datatype.SIGN_NONE;
        }
        else if (dt.startsWith("unsigned int"))
        {
            int[] i = new int[arraySize];
            long lv = 0;
            for (int j=0; j<count; j++)
            {
                theToken = st.nextToken().trim();
                try { lv = Long.parseLong(theToken); }
                catch (NumberFormatException ex)
                {
                    JOptionPane.showMessageDialog(this,
                        ex.getMessage(),
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return false;
                }
                if (lv <0 ) {
                    lv = 0;
                }
                if ( lv > 4294967295L) {
                    lv = 4294967295L;
                }
                i[j] = (int)lv;
            }
            value = i;
            tclass = Datatype.CLASS_INTEGER;
            tsize = 4;
            torder = Datatype.NATIVE;
            tsign = Datatype.SIGN_NONE;
        }
        else if (dt.startsWith("long"))
        {
            long[] l = new long[arraySize];
            for (int j=0; j<count; j++)
            {
                theToken = st.nextToken().trim();
                try { l[j] = Long.parseLong(theToken); }
                catch (NumberFormatException ex)
                {
                    JOptionPane.showMessageDialog(this,
                        ex.getMessage(),
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
            value = l;
            tclass = Datatype.CLASS_INTEGER;
            tsize = 8;
            torder = Datatype.NATIVE;
        }
        else if (dt.startsWith("float"))
        {
            float[] f = new float[arraySize];
            for (int j=0; j<count; j++)
            {
                theToken = st.nextToken().trim();
                try { f[j] = Float.parseFloat(theToken); }
                catch (NumberFormatException ex)
                {
                    JOptionPane.showMessageDialog(this,
                        ex.getMessage(),
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return false;
                }
                if (Float.isInfinite(f[j]) || Float.isNaN(f[j])) {
                    f[j] = 0;
                }
            }
            value = f;
            tclass = Datatype.CLASS_FLOAT;
            tsize = 4;
            torder = Datatype.NATIVE;
        }
        else if (dt.startsWith("double"))
        {
            double[] d = new double[arraySize];
            for (int j=0; j<count; j++)
            {
                theToken = st.nextToken().trim();
                try { d[j] = Double.parseDouble(theToken); }
                catch (NumberFormatException ex)
                {
                    JOptionPane.showMessageDialog(this,
                        ex.getMessage(),
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return false;
                }
                if (Double.isInfinite(d[j]) || Double.isNaN(d[j])) {
                    d[j] = 0;
                }
            }
            value = d;
            tclass = Datatype.CLASS_FLOAT;
            tsize = 8;
            torder = Datatype.NATIVE;
        }
        else if (dt.startsWith("object reference"))
        {
            /*
            long[] ref = new long[arraySize];
            for (int j=0; j<count; j++)
            {
                theToken = st.nextToken().trim();
                try { ref[j] = Long.parseLong(theToken); }
                catch (NumberFormatException ex)
                {
                    JOptionPane.showMessageDialog(this,
                        ex.getMessage(),
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
            */
            value = strValue;
            tclass = Datatype.CLASS_REFERENCE;
            tsize = 8;
            torder = Datatype.NATIVE;
        }
        else if (dt.equals("string"))
        {
            try {
                string_length = Integer.parseInt(lengthField.getText());
            } catch (Exception e) { string_length = 0; }

            //string_length = Math.max(string_length, strValue.length());
            if (string_length <=0) {
                string_length = DEFAULT_STRING_ATTRIBUTE_LENGTH;
            }

            if (strValue.length() > string_length) {
                strValue = strValue.substring(0, string_length);
            }

            tclass = Datatype.CLASS_STRING;
            tsize = string_length;

            String[] strArray = {strValue};
            value = strArray;

            if (isH5) {
                arraySize = 1; // support string type
            } else {
                arraySize = string_length; // array of characters
            }
        }

        Datatype datatype = null;

        try { datatype = fileFormat.createDatatype(tclass, tsize, torder, tsign); }
        catch (Exception ex)
        {
            JOptionPane.showMessageDialog(this,
                ex.getMessage(),
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return false;
        }

        long[] dims = {arraySize};
        Attribute attr = new Attribute(attrName, datatype, dims);
        attr.setValue(value);

        try {
            if (!isH5 &&
                (hObject instanceof Group) &&
                ((Group)hObject).isRoot() &&
                h4GrAttrRadioButton.isSelected())
            {
                // don't find a good way to write HDF4 global
                // attribute. Use the isExisted to separate the
                // global attribute is GR or SD
                hObject.getFileFormat().writeAttribute(hObject, attr, false);
                if (hObject.getMetadata() == null) {
                    hObject.getMetadata().add(attr);
                }
            } else {
                hObject.writeMetadata(attr);
            }
        }
        catch (Exception ex )
        {
            JOptionPane.showMessageDialog(this,
                ex.getMessage(),
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return false;
        }

        newAttribute = attr;

        return true;
    }

    /** Creates a dialog to show the help information. */
    private void createHelpDialog()
    {
        helpDialog = new JDialog(this, "Creation New Attribute");

        JPanel contentPane = (JPanel)helpDialog.getContentPane();
        contentPane.setLayout(new BorderLayout(5,5));
        contentPane.setBorder(BorderFactory.createEmptyBorder(15,5,5,5));
        int w = 500 + (ViewProperties.getFontSize()-12)*15;
        int h = 400 + (ViewProperties.getFontSize()-12)*10;
        contentPane.setPreferredSize(new Dimension(w, h));

        JButton b = new JButton("  Ok  ");
        b.addActionListener(this);
        b.setActionCommand("Hide help");
        JPanel tmpP = new JPanel();
        tmpP.add(b);
        contentPane.add (tmpP, BorderLayout.SOUTH);

        JEditorPane infoPane = new JEditorPane();
        infoPane.setEditable(false);
        JScrollPane editorScrollPane = new JScrollPane(infoPane);
        contentPane.add (editorScrollPane, BorderLayout.CENTER);

        try {
            URL url= null, url2=null, url3=null;
            String rootPath = ViewProperties.getViewRoot();

            try {
                url = new URL("file:"+rootPath+"/lib/jhdfview.jar");
            } catch (java.net.MalformedURLException mfu) {;}

            try {
                url2 = new URL("file:"+rootPath+"/");
            } catch (java.net.MalformedURLException mfu) {;}

            try {
                url3 = new URL("file:"+rootPath+"/src/");
            } catch (java.net.MalformedURLException mfu) {;}

            URL uu[] = {url, url2, url3};
            URLClassLoader cl = new URLClassLoader(uu);
            URL u = cl.findResource("ncsa/hdf/view/NewAttrHelp.html");

            infoPane.setPage(u);
            infoPane.addHyperlinkListener(this);
        } catch (Exception e) {
            infoPane.setContentType("text/html");
            StringBuffer buff = new StringBuffer();
            buff.append("<html>");
            buff.append("<body>");
            buff.append("ERROR: cannot load help information.");
            buff.append("</body>");
            buff.append("</html>");
            infoPane.setText(buff.toString());
       }

        Point l = helpDialog.getOwner().getLocation();
        l.x += 50;
        l.y += 80;
        helpDialog.setLocation(l);
        helpDialog.validate();
        helpDialog.pack();
    }

    public void hyperlinkUpdate(HyperlinkEvent e)
    {
        if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED)
        {
            JEditorPane pane = (JEditorPane) e.getSource();

            if (e instanceof HTMLFrameHyperlinkEvent)
            {
                HTMLFrameHyperlinkEvent  evt = (HTMLFrameHyperlinkEvent)e;
                HTMLDocument doc = (HTMLDocument)pane.getDocument();
                doc.processHTMLFrameHyperlinkEvent(evt);
            } else
            {
                try { pane.setPage(e.getURL()); }
                catch (Throwable t)  {}
            }
        }
    }

    /** return the new attribute created. */
    public Attribute getAttribute() { return newAttribute; }

}
